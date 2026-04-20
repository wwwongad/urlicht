#include <benchmark/benchmark.h>
#include <functional>
#include <array>
#include <urlicht/functional/adaptive_function.h>

struct Stateless {
    size_t operator()(size_t val) const noexcept { return val; }
};

struct MidCallable {
    std::array<size_t, 32> capture{};
    size_t operator()(size_t val) const noexcept { return val * capture[0]; }
};

struct LargeCallable {
    std::array<size_t, 256> capture{};
    size_t operator()(size_t val) const noexcept { return val * capture[0]; }
};

using StdFunc   = std::function<size_t(size_t)>;
using AdaptiveSBO  = urlicht::adaptive_function<size_t(size_t) noexcept, 4096>;
using AdaptiveHeap = urlicht::adaptive_function<size_t(size_t) noexcept, 1>;
using NonTypeTag = decltype(urlicht::nontype<Stateless{}>);


template <class Wrapper, class Payload>
void BM_Invoke(benchmark::State& state) {
    Payload payload;
    Wrapper f(payload);
    size_t val = 1;
    for (auto _ : state) {
        val = f(val);
        benchmark::DoNotOptimize(val);
    }
}

template <class Wrapper, class Payload>
void BM_Create(benchmark::State& state) {
    Payload payload{};
    for (auto _ : state) {
        Wrapper f(payload);
        benchmark::DoNotOptimize(f);
        benchmark::ClobberMemory();
    }
}

static void BM_Invoke_Direct(benchmark::State& state) {
    size_t val = 1;
    for (auto _ : state) {
        benchmark::DoNotOptimize(val = Stateless{}(val));
    }
}

BENCHMARK_TEMPLATE(BM_Create, StdFunc, Stateless)->Name("create/std::function/zero_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, AdaptiveSBO, Stateless)->Name("create/adaptive_function/SBO/zero_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, AdaptiveSBO, NonTypeTag)->Name("create/adaptive_function/nontype/zero_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, StdFunc, MidCallable)->Name("create/std::function/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, AdaptiveSBO, MidCallable)->Name("create/adaptive_function/SBO/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, AdaptiveHeap, MidCallable)->Name("create/adaptive_function/heap/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, StdFunc, LargeCallable)->Name("create/std::function/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, AdaptiveSBO, LargeCallable)->Name("create/adaptive_function/SBO/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, AdaptiveHeap, LargeCallable)->Name("create/adaptive_function/heap/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);

//Invoke
BENCHMARK(BM_Invoke_Direct)->Name("invoke/direct")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Invoke, StdFunc, Stateless)->Name("invoke/std::function")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Invoke, AdaptiveSBO, Stateless)->Name("invoke/adaptive_function/SBO")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Invoke, AdaptiveSBO, NonTypeTag)->Name("invoke/adaptive_function/nontype")
    ->Repetitions(10)->ReportAggregatesOnly(true);
