#include <benchmark/benchmark.h>
#include <vector>
#include <deque>
#include <algorithm>
#include <urlicht/container/inplace_vector.h>

constexpr size_t SMALL_CAP = 64;
constexpr size_t MEDIUM_CAP = 1024;
constexpr size_t LARGE_CAP = 8192;
constexpr size_t NUM_ELEMENTS = 10'000;

using str_vec = std::vector<std::string>;
using int_vec = std::vector<int>;

template <size_t N>
using str_ivec = urlicht::container::inplace_vector<std::string, N>;

template <size_t N>
using int_ivec = urlicht::container::inplace_vector<int, N>;

// ============================================================================
// BENCHMARK: Constructors
// ============================================================================

template<typename Vec>
static void BM_Vector_DefaultConstruct(benchmark::State& state) {
    for (auto _ : state) {
        Vec vec;
        benchmark::DoNotOptimize(vec.size());
    }
    state.SetItemsProcessed(state.iterations());
}

template<typename Vec>
static void BM_Vector_ConstructWithSizeAndValue(benchmark::State& state) {
    size_t size = state.range(0);

    for (auto _ : state) {
        Vec vec(size, "Small string");
        benchmark::DoNotOptimize(vec.data());
    }
    state.SetItemsProcessed(state.iterations() * size);
}

template<typename Vec>
static void BM_Vector_ConstructFromRange(benchmark::State& state) {
    size_t size = state.range(0);
    std::deque<std::string> deq;
    for (size_t i = 0; i < size; i++) {
        deq.push_back("Small string");
    }

    for (auto _ : state) {
        Vec vec(deq.begin(), deq.end());
        benchmark::DoNotOptimize(vec.data());
    }
    state.SetItemsProcessed(state.iterations() * size);
}

template<typename Vec>
static void BM_Vector_CopyConstruct(benchmark::State& state) {
    size_t size = state.range(0);

    Vec vec(size, "Small string");
    for (auto _ : state) {
        Vec copy(vec);
        benchmark::DoNotOptimize(copy.data());
    }
}

template <typename Vec>
static void BM_Vector_MoveConstruct(benchmark::State& state) {
    size_t size = state.range(0);

    Vec vec(size, "Medium sized string that should be able to bypass SBO optimization");
    for (auto _ : state) {
        Vec move(std::move(vec));
        benchmark::DoNotOptimize(move.data());
    }
}

BENCHMARK_TEMPLATE(BM_Vector_DefaultConstruct, str_vec)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/default_construct");

BENCHMARK_TEMPLATE(BM_Vector_DefaultConstruct, str_ivec<LARGE_CAP>)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/default_construct");

// Size and Value
BENCHMARK_TEMPLATE(BM_Vector_ConstructWithSizeAndValue, str_vec)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/construct_with_size_and_value");

BENCHMARK_TEMPLATE(BM_Vector_ConstructWithSizeAndValue, str_ivec<LARGE_CAP>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/construct_with_size_and_value");

// From range
BENCHMARK_TEMPLATE(BM_Vector_ConstructFromRange, str_vec)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/construct_from_range");

BENCHMARK_TEMPLATE(BM_Vector_ConstructFromRange, str_ivec<LARGE_CAP>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/construct_from_range");

// Copy construct
BENCHMARK_TEMPLATE(BM_Vector_CopyConstruct, str_vec)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/copy_construct");

BENCHMARK_TEMPLATE(BM_Vector_CopyConstruct, str_ivec<LARGE_CAP>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/copy_construct");

// Copy construct
BENCHMARK_TEMPLATE(BM_Vector_MoveConstruct, str_vec)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/move_construct");

BENCHMARK_TEMPLATE(BM_Vector_MoveConstruct, str_ivec<LARGE_CAP>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/move_construct");


template <bool Unsafe, typename Vec>
class InplaceVecWrapper {
    Vec vec;
    public:

    InplaceVecWrapper() = default;
    InplaceVecWrapper(size_t size) : vec(size) {}

    template <typename... Args>
    void emplace_back(Args&&... args) {
        if constexpr (Unsafe) {
            vec.unchecked_emplace_back(std::forward<Args>(args)...);
        } else {
            vec.emplace_back(std::forward<Args>(args)...);
        }
    }
    void push_back(typename Vec::value_type val) {
        if constexpr (Unsafe) {
            vec.unchecked_push_back(val);
        } else {
            vec.push_back(val);
        }
    }

    void pop_back() {
        if constexpr (Unsafe) {
            vec.unchecked_pop_back();
        } else {
            vec.pop_back();
        }
    }
    auto* data() noexcept{ return vec.data(); }
    auto size() const noexcept { return vec.size(); }
};

template<typename Vec>
static void BM_Vector_Trivial_PushBack(benchmark::State& state) {
    size_t size = state.range(0);

    for (auto _ : state) {
        Vec vec;
        for (size_t i = 0; i < size; i++) {
            vec.push_back(static_cast<int>(i));
            benchmark::DoNotOptimize(vec.size());
        }
        benchmark::DoNotOptimize(vec.data());
    }
    state.SetItemsProcessed(state.iterations() * size);
}

BENCHMARK_TEMPLATE(BM_Vector_Trivial_PushBack, int_vec)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/trivial_push_back");

BENCHMARK_TEMPLATE(BM_Vector_Trivial_PushBack, InplaceVecWrapper<false, int_ivec<LARGE_CAP>>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/trivial_push_back/safe");

BENCHMARK_TEMPLATE(BM_Vector_Trivial_PushBack, InplaceVecWrapper<true, int_ivec<LARGE_CAP>>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/trivial_push_back/unsafe");

template <typename Vec>
static void BM_Vector_EmplaceBack(benchmark::State& state) {
    size_t size = state.range(0);

    for (auto _ : state) {
        Vec vec;
        for (size_t i = 0; i < size; i++) {
            vec.emplace_back(10, 'k');
            benchmark::DoNotOptimize(vec.size());
        }
        benchmark::DoNotOptimize(vec.data());
    }
    state.SetItemsProcessed(state.iterations() * size);
}

BENCHMARK_TEMPLATE(BM_Vector_EmplaceBack, str_vec)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/emplace_back");

BENCHMARK_TEMPLATE(BM_Vector_EmplaceBack, InplaceVecWrapper<false, str_ivec<LARGE_CAP>>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/emplace_back/safe");

BENCHMARK_TEMPLATE(BM_Vector_EmplaceBack, InplaceVecWrapper<true, str_ivec<LARGE_CAP>>)
    ->Arg(SMALL_CAP)->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/emplace_back/unsafe");

template<typename Vec>
static void BM_Vector_PopBack(benchmark::State& state) {
    size_t size = state.range(0);
    Vec vec;
    vec.reserve(size);

    for (auto _ : state) {
        state.PauseTiming();
        vec.clear();
        for(size_t i = 0; i < size; ++i)
            vec.push_back("data");
        state.ResumeTiming();

        for(size_t i=0; i<size; ++i) {
            vec.pop_back();
            benchmark::DoNotOptimize(vec.size());
        }
        benchmark::DoNotOptimize(vec.data());
    }
    state.SetItemsProcessed(state.iterations() * size);
}

BENCHMARK_TEMPLATE(BM_Vector_PopBack, str_vec)
    ->Arg(MEDIUM_CAP)->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/std/pop_back");

BENCHMARK_TEMPLATE(BM_Vector_PopBack, str_ivec<MEDIUM_CAP>)
    ->Arg(MEDIUM_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/pop_back");

BENCHMARK_TEMPLATE(BM_Vector_PopBack, str_ivec<LARGE_CAP>)
    ->Arg(LARGE_CAP)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("container/inplace_vector/urlicht/pop_back");
