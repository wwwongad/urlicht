#include <urlicht/algorithm/lower_bound.h>
#include <benchmark/benchmark.h>

#include <algorithm>
#include <cstdint>
#include <random>
#include <string>
#include <vector>
#include <ranges>

struct Aggregate {
    int id;
    std::string info;
    std::vector<int> data;
};

struct Proj {
    int operator()(const Aggregate &a) const noexcept {
        return a.id;
    }
};

static std::vector<int> make_even_sorted_data(std::size_t n) {
    std::vector<int> v;
    v.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        v.push_back(static_cast<int>(2 * i));
    }
    return v;
}

static std::vector<int> make_random_sorted_data(std::size_t n, std::uint32_t seed) {
    std::mt19937 rng(seed);
    std::uniform_int_distribution<int> dist(-1'000'000, 1'000'000);

    std::vector<int> v;
    v.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        v.push_back(dist(rng));
    }
    std::sort(v.begin(), v.end());
    return v;
}

static std::vector<Aggregate> make_aggregates(std::size_t n) {
    std::vector<Aggregate> v;
    v.reserve(n);
    for (int i = 0; i < n; ++i) {
        v.push_back(Aggregate{
            2 * i,
            "item",
            {i, i + 1, i + 2}
        });
    }
    return v;
}

class UrlichtLowerBound {
    public:
    template <typename ...Args>
    auto operator()(Args&& ...args) const {
        return urlicht::lower_bound(std::forward<Args>(args)...);
    }
};

class STDLowerBound {
    public:
    template <typename ...Args>
    auto operator()(Args&& ...args) const {
        return std::ranges::lower_bound(std::forward<Args>(args)...);
    }
};

template <typename Wrapper>
static void BM_LowerBound_Int_Present(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto data = make_even_sorted_data(n);

    std::size_t idx = 0;
    for (auto _ : state) {
        const int q = data[idx];
        idx = (idx + 1) % data.size();

        const auto it = Wrapper{}(data, q);
        benchmark::DoNotOptimize(&it);
        benchmark::ClobberMemory();
    }

    state.SetItemsProcessed(state.iterations());
}

template <typename Wrapper>
static void BM_LowerBound_Int_Absent(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto data = make_even_sorted_data(n);

    std::size_t idx = 0;
    for (auto _ : state) {
        const int q = static_cast<int>(2 * (idx % data.size()) + 1);
        ++idx;

        const auto it = Wrapper{}(data, q);
        benchmark::DoNotOptimize(&it);
    }

    state.SetItemsProcessed(state.iterations());
}

template <typename Wrapper>
static void BM_LowerBound_Int_RandomQueries(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto data = make_random_sorted_data(n, 0x12345678u);

    std::mt19937 rng(0xBADC0DEu);
    std::uniform_int_distribution qdist(-1'000'000, 1'000'000);

    for (auto _ : state) {
        const int q = qdist(rng);
        const auto it = Wrapper{}(data, q);
        benchmark::DoNotOptimize(&it);
    }

    state.SetItemsProcessed(state.iterations());
}

template <typename Wrapper>
static void BM_LowerBound_AggregateProjection(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto data = make_aggregates(n);

    std::size_t idx = 0;
    for (auto _ : state) {
        const int q = static_cast<int>(2 * (idx % data.size()));
        ++idx;

        const auto it = Wrapper{}(data, q, std::less{}, Proj{});
        benchmark::DoNotOptimize(&it);
    }

    state.SetItemsProcessed(state.iterations());
}

#define Rng DenseRange(1024, 35536, 4096)
BENCHMARK_TEMPLATE(BM_LowerBound_Int_Present, UrlichtLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_LowerBound_Int_Present, STDLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_LowerBound_Int_Absent, UrlichtLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_LowerBound_Int_Absent, STDLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_LowerBound_Int_RandomQueries, UrlichtLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_LowerBound_Int_RandomQueries, STDLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_LowerBound_AggregateProjection, UrlichtLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_LowerBound_Int_RandomQueries, STDLowerBound)
    ->Rng->Repetitions(10)->ReportAggregatesOnly(true);
