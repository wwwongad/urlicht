#include <urlicht/concurrency/mpmc_queue.h>
#include <urlicht/config.h>
#include <benchmark/benchmark.h>
#include <algorithm>
#include <array>
#include <barrier>
#include <chrono>
#include <concepts>
#include <cstdint>
#include <ranges>
#include <thread>
#include <vector>
#include "third_party/rigtorp_mpmc.h"

#if URLICHT_BM_HAS_BOOST
#include <boost/lockfree/queue.hpp>
#endif

#if UL_PLATFORM_LINUX
#include <pthread.h>
#endif

inline void pin_current_thread(const std::size_t id) {
#if UL_PLATFORM_LINUX
    const std::size_t core_count = std::thread::hardware_concurrency();
    if (core_count == 0) {
        return;
    }
    const auto cpu = static_cast<unsigned>(id % core_count);

    cpu_set_t cpu_set;
    CPU_ZERO(&cpu_set);
    CPU_SET(cpu, &cpu_set);

    const int result = pthread_setaffinity_np(
        pthread_self(),
        sizeof(cpu_set_t),
        &cpu_set
    );
    (void)result;
#endif
}

#if defined(__x86_64__) || defined(_M_X64)
#include <x86intrin.h>
#endif

inline std::uint64_t now() noexcept {
#if defined(__x86_64__) || defined(_M_X64)
    return __rdtsc();
#elif defined(__aarch64__)
    std::uint64_t tsc;
    asm volatile("mrs %0, cntvct_el0" : "=r"(tsc));
    return tsc;
#else
    auto now = std::chrono::steady_clock::now();
    return std::chrono::duration_cast<std::chrono::nanoseconds>(now.time_since_epoch()).count();
#endif
}

constexpr std::size_t items_per_producer = 1 << 16;
constexpr std::size_t queue_size = 1 << 10;

constexpr std::array<std::pair<int, int>, 8> num_threads_config = {
       {{1, 1}, // Baseline
        {1, 2}, // Moderate consumer contention
        {2, 1}, // Moderate producer contention
        {2, 2}, // Balanced low contention
        {4, 4}, // Typical MPMC
        {1, 8}, // Saturated consumer contention
        {8, 1}, // Saturated producer contention
        {8, 8}} // Heavy workload
};

using value_type1 = std::uint64_t;

struct info_t {
    std::uint64_t info_id;
    std::uint64_t user_id;
    double value;
    std::uint64_t quantity;
    std::uint64_t timestamp;
    bool side;
    std::uint8_t type;
    std::uint8_t status;
};

using value_type2 = info_t;

template <typename T>
T make_value(const std::size_t id) {
    if constexpr (std::same_as<T, value_type1>) {
        return static_cast<value_type1>(id);
    } else {
        return info_t {
            .info_id = id
        };
    }
}

template <typename T>
T make_timed_value(const std::size_t id) {
    if constexpr (std::same_as<T, value_type1>) {
        return now();
    } else {
        return value_type2 {
            .info_id = id,
            .timestamp = now()
        };
    }
}

template <typename T>
std::size_t get_key(const T& val) {
    if constexpr (std::same_as<T, value_type1>) {
        return val;
    } else {
        return val.info_id;
    }
}

template <typename T>
std::size_t get_time_of(const T& val) {
    if constexpr (std::same_as<T, value_type1>) {
        return val;
    } else {
        return val.timestamp;
    }
}

template <typename T, std::size_t N>
class rigtorp_mpmc {
    rigtorp::MPMCQueue<T> queue_;
public:
    rigtorp_mpmc() : queue_{N} {}
    ~rigtorp_mpmc() = default;

    void emplace(T&& val) noexcept {
        queue_.emplace(std::move(val));
    }
    void dequeue(T& val) noexcept {
        queue_.pop(val);
    }

    void try_emplace_loop(T&& val) noexcept {
        while (!queue_.try_emplace(std::move(val))) { }
    }
    void try_dequeue_loop(T& val) noexcept {
        while (!queue_.try_pop(val)) { }
    }
};

#if URLICHT_BM_HAS_BOOST
template <typename T, std::size_t N>
class boost_mpmc {
    boost::lockfree::queue<T, boost::lockfree::capacity<N>> queue_;
public:
    boost_mpmc() = default;
    ~boost_mpmc() = default;

    void try_emplace_loop(T&& val) noexcept {
        while (!queue_.push(std::move(val))) { }
    }
    void try_dequeue_loop(T& val) noexcept {
        while (!queue_.pop(val)) { }
    }
};
#endif

template <typename T, std::size_t N>
class urlicht_mpmc {
    urlicht::concurrency::mpmc_queue<T, urlicht::concurrency::capacity<N>> queue_;
public:
    urlicht_mpmc() = default;
    ~urlicht_mpmc() = default;

    void emplace(T&& val) noexcept {
        queue_.emplace(std::move(val));
    }
    void dequeue(T& val) noexcept {
        queue_.dequeue(val);
    }

    void try_emplace_loop(T&& val) noexcept {
        while (!queue_.try_emplace(std::move(val))) { }
    }
    void try_dequeue_loop(T& val) noexcept {
        while (!queue_.try_dequeue(val)) { }
    }
};

enum class push_mode {
    emplace,
    try_loop
};

template <template <typename, std::size_t> typename Queue,
          typename T,
          std::size_t N,
          push_mode Mode = push_mode::try_loop>
static void BM_mpmc_throughput(benchmark::State& state) {
    const auto num_producer = state.range(0);
    const auto num_consumer = state.range(1);

    const auto num_threads = num_producer + num_consumer;
    const auto num_items = num_producer * items_per_producer;
    const auto items_per_consumer = num_items / num_consumer;

    for (auto _ : state) {
        state.PauseTiming();
        {
            Queue<T, N> queue;
            std::barrier barrier{num_threads + 1};
            std::size_t tid{0U};

            // Create producers
            std::vector<std::thread> producers;
            for (; tid < num_producer; ++tid) {
                producers.emplace_back([&queue, &barrier, id = tid] {
                    pin_current_thread(id);
                    barrier.arrive_and_wait();

                    for (std::size_t i{0U}; i < items_per_producer; ++i) {
                        if constexpr (Mode == push_mode::emplace) {
                            queue.emplace(make_value<T>(i));
                        } else {
                            queue.try_emplace_loop(make_value<T>(i));
                        }
                    }
                });
            }

            // Create consumers
            std::vector<std::thread> consumers;
            for (; tid < num_threads; ++tid) {
                consumers.emplace_back([&queue, &barrier, items_per_consumer, id = tid] {
                    pin_current_thread(id);
                    barrier.arrive_and_wait();

                    for (std::size_t i{0U}; i < items_per_consumer; ++i) {
                        T val;
                        if constexpr (Mode == push_mode::emplace) {
                            queue.dequeue(val);
                        } else {
                            queue.try_dequeue_loop(val);
                        }
                        benchmark::DoNotOptimize(get_key(val));
                    }
                });
            }
            state.ResumeTiming();
            barrier.arrive_and_wait();

            for (auto& thread : producers) {
                thread.join();
            }
            for (auto& thread : consumers) {
                thread.join();
            }
            state.PauseTiming();
            benchmark::ClobberMemory();
        }
        state.ResumeTiming();
    }

    state.SetItemsProcessed(state.iterations() * num_items);
    state.SetBytesProcessed(state.iterations() * num_items * sizeof(T));
}

template <template <typename, std::size_t> typename Queue,
          typename T,
          std::size_t N,
          push_mode Mode = push_mode::try_loop>
static void BM_mpmc_end_to_end_latency(benchmark::State& state) {
    const auto num_producer = state.range(0);
    const auto num_consumer = state.range(1);

    const auto num_threads = num_producer + num_consumer;
    const auto num_items = num_producer * items_per_producer;
    const auto items_per_consumer = num_items / num_consumer;

    for (auto _ : state) {
        state.PauseTiming();
        {
            Queue<T, N> queue;
            std::barrier barrier{num_threads + 1};
            std::size_t tid{0U};

            std::vector<std::vector<std::uint64_t>> consumer_latencies(num_consumer);
            for (auto& c : consumer_latencies) {
                c.reserve(items_per_consumer);
            }

            // Create consumers
            std::vector<std::thread> consumers;
            for (; tid < num_consumer; ++tid) {
                consumers.emplace_back([&queue, &barrier, &consumer_latencies, items_per_consumer, id = tid] {
                    pin_current_thread(id);
                    barrier.arrive_and_wait();

                    for (std::size_t i{0U}; i < items_per_consumer; ++i) {
                        T val;
                        if constexpr (Mode == push_mode::emplace) {
                            queue.dequeue(val);
                        } else {
                            queue.try_dequeue_loop(val);
                        }
                        benchmark::DoNotOptimize(get_key(val));

                        const auto latency = now() - get_time_of(val);
                        consumer_latencies[id].emplace_back(latency);
                    }
                });
            }

            // Create producers
            std::vector<std::thread> producers;
            for (; tid < num_threads; ++tid) {
                producers.emplace_back([&queue, &barrier, id = tid] {
                    pin_current_thread(id);
                    barrier.arrive_and_wait();

                    for (std::size_t i{0U}; i < items_per_producer; ++i) {
                        if constexpr (Mode == push_mode::emplace) {
                            queue.emplace(make_timed_value<T>(i));
                        } else {
                            queue.try_emplace_loop(make_timed_value<T>(i));
                        }
                    }
                });
            }

            state.ResumeTiming();
            barrier.arrive_and_wait();

            for (auto& thread : producers) {
                thread.join();
            }
            for (auto& thread : consumers) {
                thread.join();
            }

            state.PauseTiming();
            benchmark::ClobberMemory();

            std::vector<std::uint64_t> all_samples;
            all_samples.reserve(num_items);
            for (const auto& vec : consumer_latencies) {
                all_samples.insert(all_samples.end(), vec.begin(), vec.end());
            }
            std::ranges::sort(all_samples);

            state.counters["p50"] = all_samples[all_samples.size() * 0.50];
            state.counters["p90"] = all_samples[all_samples.size() * 0.90];
            state.counters["p99"] = all_samples[all_samples.size() * 0.99];
            state.counters["p99.9"] = all_samples[all_samples.size() * 0.999];
        }
        state.ResumeTiming();
    }
}

static void add_pc_args(benchmark::internal::Benchmark* b) {
    for (const auto& [producers, consumers] : num_threads_config) {
        b->Args({producers, consumers});
    }
}


BENCHMARK_TEMPLATE(BM_mpmc_throughput, rigtorp_mpmc, value_type1, queue_size)
    ->Name("rigtorp_mpmc/size_t/try_loop")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_throughput, rigtorp_mpmc, value_type2, queue_size)
    ->Name("rigtorp_mpmc/info_t/try_loop")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

#if URLICHT_BM_HAS_BOOST
BENCHMARK_TEMPLATE(BM_mpmc_throughput, boost_mpmc, value_type1, queue_size)
    ->Name("boost_mpmc/size_t/try_loop")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_throughput, boost_mpmc, value_type2, queue_size)
    ->Name("boost_mpmc/info_t/try_loop")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
#endif

BENCHMARK_TEMPLATE(BM_mpmc_throughput, urlicht_mpmc, value_type1, queue_size)
    ->Name("urlicht_mpmc/size_t/try_loop")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_throughput, urlicht_mpmc, value_type2, queue_size)
    ->Name("urlicht_mpmc/info_t/try_loop")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_mpmc_throughput, rigtorp_mpmc, value_type1, queue_size, push_mode::emplace)
    ->Name("rigtorp_mpmc/size_t/emplace")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_throughput, rigtorp_mpmc, value_type2, queue_size, push_mode::emplace)
    ->Name("rigtorp_mpmc/info_t/emplace")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_mpmc_throughput, urlicht_mpmc, value_type1, queue_size, push_mode::emplace)
    ->Name("urlicht_mpmc/size_t/emplace")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_throughput, urlicht_mpmc, value_type2, queue_size, push_mode::emplace)
    ->Name("urlicht_mpmc/info_t/emplace")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);


// End-to-end latency
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, rigtorp_mpmc, value_type1, queue_size)
    ->Name("rigtorp_mpmc/size_t/try_loop/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, rigtorp_mpmc, value_type2, queue_size)
    ->Name("rigtorp_mpmc/info_t/try_loop/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

#if URLICHT_BM_HAS_BOOST
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, boost_mpmc, value_type1, queue_size)
    ->Name("boost_mpmc/size_t/try_loop/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, boost_mpmc, value_type2, queue_size)
    ->Name("boost_mpmc/info_t/try_loop/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
#endif

BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, urlicht_mpmc, value_type1, queue_size)
    ->Name("urlicht_mpmc/size_t/try_loop/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, urlicht_mpmc, value_type2, queue_size)
    ->Name("urlicht_mpmc/info_t/try_loop/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, rigtorp_mpmc, value_type1, queue_size, push_mode::emplace)
    ->Name("rigtorp_mpmc/size_t/emplace/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, rigtorp_mpmc, value_type2, queue_size, push_mode::emplace)
    ->Name("rigtorp_mpmc/info_t/emplace/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, urlicht_mpmc, value_type1, queue_size, push_mode::emplace)
    ->Name("urlicht_mpmc/size_t/emplace/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_mpmc_end_to_end_latency, urlicht_mpmc, value_type2, queue_size, push_mode::emplace)
    ->Name("urlicht_mpmc/info_t/emplace/latency")
    ->Apply(add_pc_args)->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);