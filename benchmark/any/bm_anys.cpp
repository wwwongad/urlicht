#include <benchmark/benchmark.h>
#include <any>
#include <variant>
#include <array>
#include <string>
#include <memory>
#include <urlicht/any/adaptive_any.h>

enum class ObjType {
    SMALL, MEDIUM, LARGE
};

// Small objects - 4 to 8 bytes
using small_obj1 = int;
using small_obj2 = double;
using small_obj3 = void*;

// Mid-sized objects - ~48 bytes
using mid_obj1 = std::string;
using mid_obj2 = std::array<int, 12>;
using mid_obj3 = std::tuple<double, std::vector<double>, std::shared_ptr<int>>;

// Large objects - >500 bytes
using large_obj1 = std::array<int, 128>;
using large_obj2 = std::array<char, 580>;
using large_obj3 = std::array<std::array<std::array<char, 20>, 10>, 3>;

template <ObjType Type>
using obj1 =
    std::conditional_t<Type == ObjType::SMALL, small_obj1,
        std::conditional_t<Type == ObjType::MEDIUM, mid_obj1,
            std::conditional_t<Type == ObjType::LARGE, large_obj1,
                std::monostate
            >
        >
    >;

template <ObjType Type>
using obj2 =
    std::conditional_t<Type == ObjType::SMALL, small_obj2,
        std::conditional_t<Type == ObjType::MEDIUM, mid_obj2,
            std::conditional_t<Type == ObjType::LARGE, large_obj2,
                std::monostate
            >
        >
    >;

template <ObjType Type>
using obj3 =
    std::conditional_t<Type == ObjType::SMALL, small_obj3,
        std::conditional_t<Type == ObjType::MEDIUM, mid_obj3,
            std::conditional_t<Type == ObjType::LARGE, large_obj3,
                std::monostate
            >
        >
    >;

// Constants
constexpr size_t SMALL_SBO = 16u;
constexpr size_t MID_SBO = 64u;
constexpr size_t LARGE_SBO = 1024u;

constexpr size_t ITERATIONS = 5000;

template <typename AnyType, ObjType Type>
static void BM_CreateAnyFromObj(benchmark::State& state) {
    std::vector<AnyType> vec;
    vec.reserve(ITERATIONS * 3);
    for (auto _ : state) {
        for (size_t i = 0; i < ITERATIONS; i++) {
            vec.emplace_back(obj1<Type>{});
            vec.emplace_back(obj2<Type>{});
            vec.emplace_back(obj3<Type>{});
        }
        benchmark::DoNotOptimize(vec.size());
        vec.clear();
    }
}


template <typename AnyType, ObjType Type>
static void BM_AnyCopyConstruction(benchmark::State& state) {
    AnyType any1{obj1<Type>{}}, any2{obj2<Type>{}}, any3{obj3<Type>{}};
    std::vector<AnyType> vec;
    vec.reserve(ITERATIONS * 3);
    for (auto _ : state) {
        for (size_t i = 0; i < ITERATIONS; i++) {
            vec.emplace_back(any1);
            vec.emplace_back(any2);
            vec.emplace_back(any3);
        }
        benchmark::DoNotOptimize(vec.size());
        vec.clear();
    }
}


template <typename AnyType, ObjType Type>
static void BM_AnyAssignment(benchmark::State& state) {
    AnyType any1{obj1<Type>{}}, any2{obj2<Type>{}}, any3{obj3<Type>{}};
    std::vector<AnyType> vec(ITERATIONS * 3);
    for (auto _ : state) {
        for (size_t i = 0; i + 3 <= ITERATIONS; i++) {
            vec[i] = any1;
            vec[i + 1] = any2;
            vec[i + 2] = any3;
        }
        benchmark::DoNotOptimize(vec.data());
        benchmark::ClobberMemory();
    }
}


template <typename AnyType, ObjType Type, bool Unchecked = false>
static void BM_AnyCast(benchmark::State& state) {
    AnyType any1{obj1<Type>{}}, any2{obj2<Type>{}}, any3{obj3<Type>{}};
    for (auto _ : state) {
        obj1<Type>* p1;
        obj2<Type>* p2;
        obj3<Type>* p3;
        for (size_t i = 0; i < ITERATIONS; i++) {
            if constexpr (Unchecked) {
                p1 = unchecked_any_cast<obj1<Type>>(&any1);
                p2 = unchecked_any_cast<obj2<Type>>(&any2);
                p3 = unchecked_any_cast<obj3<Type>>(&any3);
            } else {
                p1 = any_cast<obj1<Type>>(&any1);
                p2 = any_cast<obj2<Type>>(&any2);
                p3 = any_cast<obj3<Type>>(&any3);
            }
            benchmark::DoNotOptimize(p1);
            benchmark::DoNotOptimize(p2);
            benchmark::DoNotOptimize(p3);
        }
        benchmark::ClobberMemory();
    }
}

// -----------------------------------------------------------------------------
// AnyConstruction
// -----------------------------------------------------------------------------
// Small objects
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, std::any, ObjType::SMALL)
    ->Name("CreateAnyFromObj/small/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, urlicht::adaptive_any<SMALL_SBO>, ObjType::SMALL)
    ->Name("CreateAnyFromObj/small/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);

// Mid-sized objects
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, std::any, ObjType::MEDIUM)
    ->Name("CreateAnyFromObj/mid/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, urlicht::adaptive_any<MID_SBO>, ObjType::MEDIUM)
    ->Name("CreateAnyFromObj/mid/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);

// Large objects
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, std::any, ObjType::LARGE)
    ->Name("CreateAnyFromObj/large/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, urlicht::adaptive_any<LARGE_SBO>, ObjType::LARGE)
    ->Name("CreateAnyFromObj/large/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_CreateAnyFromObj, urlicht::adaptive_any<MID_SBO>, ObjType::LARGE)
    ->Name("CreateAnyFromObj/large/adaptive_any/insufficient_storage")
    ->Repetitions(10)->ReportAggregatesOnly(true);

// -----------------------------------------------------------------------------
// AnyCopyConstruction
// -----------------------------------------------------------------------------
// Small objects
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, std::any, ObjType::SMALL)
    ->Name("AnyCopyConstruction/small/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, urlicht::adaptive_any<SMALL_SBO>, ObjType::SMALL)
    ->Name("AnyCopyConstruction/small/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);

// Medium objects
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, std::any, ObjType::MEDIUM)
    ->Name("AnyCopyConstruction/mid/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, urlicht::adaptive_any<MID_SBO>, ObjType::MEDIUM)
    ->Name("AnyCopyConstruction/mid/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);

// Large objects
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, std::any, ObjType::LARGE)
    ->Name("AnyCopyConstruction/large/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, urlicht::adaptive_any<LARGE_SBO>, ObjType::LARGE)
    ->Name("AnyCopyConstruction/large/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);
// Insufficient storage
BENCHMARK_TEMPLATE(BM_AnyCopyConstruction, urlicht::adaptive_any<MID_SBO>, ObjType::LARGE)
    ->Name("AnyCopyConstruction/large/adaptive_any/insufficient_storage")
    ->Repetitions(10)->ReportAggregatesOnly(true);

// -----------------------------------------------------------------------------
// AnyAssignment
// -----------------------------------------------------------------------------
// Small objects
BENCHMARK_TEMPLATE(BM_AnyAssignment, std::any, ObjType::SMALL)
    ->Name("AnyAssignment/small/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyAssignment, urlicht::adaptive_any<SMALL_SBO>, ObjType::SMALL)
    ->Name("AnyAssignment/small/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);

// Medium objects
BENCHMARK_TEMPLATE(BM_AnyAssignment, std::any, ObjType::MEDIUM)
    ->Name("AnyAssignment/mid/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyAssignment, urlicht::adaptive_any<MID_SBO>, ObjType::MEDIUM)
    ->Name("AnyAssignment/mid/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);

// Large objects
BENCHMARK_TEMPLATE(BM_AnyAssignment, std::any, ObjType::LARGE)
    ->Name("AnyAssignment/large/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyAssignment, urlicht::adaptive_any<LARGE_SBO>, ObjType::LARGE)
    ->Name("AnyAssignment/large/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyAssignment, urlicht::adaptive_any<MID_SBO>, ObjType::LARGE)
    ->Name("AnyAssignment/large/adaptive_any/insufficient_storage")
    ->Repetitions(10)->ReportAggregatesOnly(true);

// -----------------------------------------------------------------------------
// AnyCast
// -----------------------------------------------------------------------------

BENCHMARK_TEMPLATE(BM_AnyCast, std::any, ObjType::SMALL)
    ->Name("AnyCast/std::any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyCast, urlicht::adaptive_any<SMALL_SBO>, ObjType::SMALL)
    ->Name("AnyCast/adaptive_any")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_AnyCast, urlicht::adaptive_any<SMALL_SBO>, ObjType::SMALL, true)
    ->Name("AnyCast/adaptive_any/unchecked")->Repetitions(10)->ReportAggregatesOnly(true);

