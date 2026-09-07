#include <urlicht/memory/arena.h>
#include <urlicht/memory/resource_view.h>
#include <urlicht/memory/concurrent_arena.h>
#include <urlicht/memory/pmr/arena_resource.h>

#include <algorithm>
#include <array>
#include <format>
#include <functional>
#include <iostream>
#include <numeric>
#include <span>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

struct telemetry_sample {
    int sensor_id;
    int second;
    double temperature;
    bool alarm;
};

struct sensor_summary {
    std::size_t sample_count{};
    double total_temperature{};
    double peak_temperature{std::numeric_limits<double>::lowest()};
};

struct batch_report {
    std::size_t accepted;
    std::size_t alarms;
    double average_temperature;
};

using request_arena = urlicht::memory::arena<{.use_upstream = false}>;
using sample_allocator =
    urlicht::memory::resource_view<telemetry_sample, request_arena>;
using summary_value = std::pair<const int, sensor_summary>;
using summary_allocator =
    urlicht::memory::resource_view<summary_value, request_arena>;
using report_allocator =
    urlicht::memory::resource_view<batch_report, request_arena>;

int main() {
    const std::array incoming{
        telemetry_sample{101, 4, 21.4, false},
        telemetry_sample{202, 2, 25.8, false},
        telemetry_sample{101, 1, 22.1, false},
        telemetry_sample{303, 6, 31.7, true},
        telemetry_sample{202, 5, 27.3, true},
        telemetry_sample{101, 3, 23.0, false},
        telemetry_sample{303, 7, 30.9, true},
        telemetry_sample{202, 8, 26.2, false},
    };

    request_arena request_memory(32 * 1024);
    {
        std::vector<telemetry_sample, sample_allocator> samples(request_memory);
        samples.assign(incoming.begin(), incoming.end());
        std::ranges::sort(samples, {}, &telemetry_sample::second);

        std::unordered_map<
            int,
            sensor_summary,
            std::hash<int>,
            std::equal_to<>,
            summary_allocator
        > summaries(request_memory);

        for (const auto& sample : samples) {
            auto& summary = summaries[sample.sensor_id];
            ++summary.sample_count;
            summary.total_temperature += sample.temperature;
            summary.peak_temperature =
                std::max(summary.peak_temperature, sample.temperature);
        }

        const auto alarm_count = std::ranges::count_if(
            samples,
            &telemetry_sample::alarm);
        const auto total_temperature = std::accumulate(
            samples.begin(),
            samples.end(),
            0.0,
            [](const double total, const telemetry_sample& sample) {
                return total + sample.temperature;
            });

        report_allocator report_alloc(request_memory);
        using report_traits = std::allocator_traits<report_allocator>;
        auto* report = report_traits::allocate(report_alloc, 1);
        report_traits::construct(
            report_alloc,
            report,
            batch_report{
                samples.size(),
                static_cast<std::size_t>(alarm_count),
                total_temperature / static_cast<double>(samples.size()),
            });

        std::cout << std::format(
            "Telemetry batch: {} samples, {} alarms, average {:.1f} C\n",
            report->accepted,
            report->alarms,
            report->average_temperature);
        std::cout << std::format("{:<8} {:>8} {:>12} {:>10}\n", "Sensor", "Samples", "Average C", "Peak C");
        std::cout << std::string(42, '-') << '\n';

        for (const int sensor_id : {101, 202, 303}) {
            const auto& summary = summaries.at(sensor_id);
            std::cout << std::format(
                "{:<8} {:>8} {:>12.1f} {:>10.1f}\n",
                sensor_id,
                summary.sample_count,
                summary.total_temperature
                    / static_cast<double>(summary.sample_count),
                summary.peak_temperature);
        }

        report_traits::destroy(report_alloc, report);
        report_traits::deallocate(report_alloc, report, 1);
    }

    request_memory.reset();
    const auto& initial_buffer = request_memory.get_initial_buffer();
    std::cout << std::format("\nRequest arena reset: {}\n",
        initial_buffer.curr == initial_buffer.end()
            ? "all scratch storage reusable"
            : "storage still in use");

    urlicht::memory::pmr::arena_resource<> response_memory(4 * 1024);
    {
        std::pmr::vector<std::pmr::string> response_lines(&response_memory);
        for (const auto [sensor_id, status] : std::array{
                 std::pair{101, "normal"},
                 std::pair{202, "watch"},
                 std::pair{303, "alarm"},
             }) {
            std::pmr::string line(&response_memory);
            std::format_to(
                std::back_inserter(line),
                "sensor={}; status={}",
                sensor_id,
                status);
            response_lines.emplace_back(std::move(line));
        }

        std::cout << "PMR response payload:\n";
        for (const auto& line : response_lines) {
            std::cout << std::format("  {}\n", line);
        }
    }
    response_memory.reset();

    using shared_arena = urlicht::memory::concurrent_arena<>;
    using worker_allocator =
        urlicht::memory::resource_view<int, shared_arena>;

    shared_arena shared_memory(4 * 1024);
    constexpr std::array west_readings{9, 3, 7, 5, 4};
    constexpr std::array east_readings{8, 6, 2, 1, 10};
    std::array<int, 2> shard_totals{};

    const auto aggregate_shard = [&](const std::size_t shard, const std::span<const int> readings) {
        std::vector<int, worker_allocator> scratch(shared_memory);
        scratch.assign(readings.begin(), readings.end());
        shard_totals[shard] =
            std::accumulate(scratch.begin(), scratch.end(), 0);
    };

    std::jthread west_worker(
        aggregate_shard,
        0,
        std::span<const int>{west_readings});
    std::jthread east_worker(
        aggregate_shard,
        1,
        std::span<const int>{east_readings});
    west_worker.join();
    east_worker.join();

    std::cout << std::format(
        "Concurrent arena shard totals: west={}, east={}, combined={}\n",
        shard_totals[0],
        shard_totals[1],
        shard_totals[0] + shard_totals[1]);
}
