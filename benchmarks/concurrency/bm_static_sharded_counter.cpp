#include <urlicht/concurrency/static_sharded_counter.h>
#include <benchmark/benchmark.h>
#include <atomic>
#include <vector>
#include <thread>
#include <barrier>

class AtomicCounter {
    alignas(64) std::atomic<size_t> counter{};
public:
    void increment() noexcept {
        counter.fetch_add(1, std::memory_order_relaxed);
    }
    auto get() const noexcept {
        return counter.load(std::memory_order_relaxed);
    }
};

template <size_t Threshold = 1024, size_t NumLocal = 8>
class ExactShardedCounter {
    urlicht::concurrency::static_sharded_counter<size_t, Threshold, NumLocal> counter{};
public:
    void increment() noexcept {
        counter.increment();
    }
    auto get() noexcept {
        return counter.get_exact();
    }
};

template <size_t Threshold = 1024, size_t NumLocal = 8>
class ApproxShardedCounter {
    urlicht::concurrency::static_sharded_counter<size_t, Threshold, NumLocal> counter{};
public:
    void increment() noexcept {
        counter.increment();
    }
    auto get() const noexcept {
        return counter.get_approximate();
    }
};

// Increment only
template <typename Wrapper>
static void BM_Counter_Increment(benchmark::State& state) {
    const int num_threads = static_cast<int>(state.range(0));
    constexpr size_t num_ops = 10'000;

    Wrapper counter;
    for (auto _ : state) {
        state.PauseTiming();
        std::barrier sync_pt(num_threads + 1);
        std::vector<std::thread> threads;
        threads.reserve(num_threads);
        for (int t = 0; t < num_threads; ++t) {
            threads.emplace_back([&] {
                sync_pt.arrive_and_wait();
                for (size_t i = 0; i < num_ops; ++i) {
                    counter.increment();
                }
            });
        }
        state.ResumeTiming();
        sync_pt.arrive_and_wait();

        for (auto& th : threads) th.join();
        benchmark::DoNotOptimize(counter.get());
    }
    state.SetItemsProcessed(state.iterations() * num_threads * num_ops);
}

// 80% increments, 20% reads
template <typename Wrapper>
static void BM_Counter_Mixed_Workload(benchmark::State& state) {
    const int num_threads = static_cast<int>(state.range(0));
    constexpr size_t num_ops = 20'000;

    Wrapper counter;
    for (auto _ : state) {
        state.PauseTiming();
        std::barrier sync_pt(num_threads + 1);
        std::vector<std::thread> threads;
        threads.reserve(num_threads);
        for (int t = 0; t < num_threads; ++t) {
            threads.emplace_back([&] {
                sync_pt.arrive_and_wait();
                size_t x = 0;
                for (size_t i = 0; i < num_ops; ++i) {
                    if (i % 5 == 0) {
                        x += counter.get();
                    } else {
                        counter.increment();
                    }
                }
                benchmark::DoNotOptimize(x);
            });
        }
        state.ResumeTiming();
        sync_pt.arrive_and_wait();

        for (auto& th : threads) th.join();
    }
    state.SetItemsProcessed(state.iterations() * num_threads * num_ops);
}

BENCHMARK_TEMPLATE(BM_Counter_Increment, AtomicCounter)
    ->Range(8, 1024)->UseRealTime()->Name("atomic_size_t/increment")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Counter_Increment, ApproxShardedCounter<>)
    ->Range(8, 1024)->UseRealTime()->Name("static_sharded_counter/increment/8local")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Counter_Increment, ApproxShardedCounter<1024, 64>)
    ->Range(8, 1024)->UseRealTime()->Name("static_sharded_counter/increment/64local")
    ->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_Counter_Mixed_Workload, AtomicCounter)
    ->Range(8, 1024)->UseRealTime()->Name("atomic_size_t/readwrite")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Counter_Mixed_Workload, ExactShardedCounter<>)
    ->Range(8, 1024)->UseRealTime()->Name("static_sharded_counter/exact/readwrite/8local")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Counter_Mixed_Workload, ApproxShardedCounter<>)
    ->Range(8, 1024)->UseRealTime()->Name("static_sharded_counter/approx/readwrite/8local")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Counter_Mixed_Workload, ApproxShardedCounter<1024, 1024>)
    ->Range(8, 1024)->UseRealTime()->Name("static_sharded_counter/approx/readwrite/64local")
    ->Repetitions(10)->ReportAggregatesOnly(true);