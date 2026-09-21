#include <benchmark/benchmark.h>
#include <functional>
#include <array>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/resource_view.h>
#include <urlicht/functional/flexible_function.h>


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

using arena_t = urlicht::memory::arena<{.use_upstream = false}>;
using view_alloc = urlicht::memory::resource_view<std::byte, arena_t, {.unchecked_allocate = true}>;

using StdFunc   = std::function<size_t(size_t)>;
using FlexibleSBO  = urlicht::functional::flexible_function<size_t(size_t) noexcept, 4096>;
using FlexibleHeap = urlicht::functional::flexible_function<size_t(size_t) noexcept, 1>;
using FlexibleArena = urlicht::functional::flexible_function<size_t(size_t) noexcept, 1, 1, view_alloc>;

template <typename Func>
class no_alloc_func {
    Func func;
public:
    template <typename Payload, typename... Args>
    explicit no_alloc_func(Payload&& payload, [[maybe_unused]] Args&&... args)
    : func(payload) { }

    template <typename... Args>
    auto operator()(Args&&... x) const noexcept {
        return func(std::forward<Args>(x)...);
    }

    explicit operator bool() const noexcept {
        return func != nullptr;
    }
};

template <typename Func>
class with_alloc_func {
    Func func;
public:
    template <typename Payload, typename Alloc>
    explicit with_alloc_func(Payload&& payload, const Alloc& alloc)
    : func(std::allocator_arg, alloc, payload) { }

    template <typename... Args>
    auto operator()(Args&&... x) const noexcept {
        return func(std::forward<Args>(x)...);
    }

    explicit operator bool() const noexcept {
        return func != nullptr;
    }
};

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
    [[maybe_unused]] arena_t arena(1 << 12);
    Payload payload{};
    for (auto _ : state) {
        Wrapper f(payload, view_alloc(arena));
        benchmark::DoNotOptimize(static_cast<bool>(f));
        benchmark::ClobberMemory();
        if constexpr (std::same_as<Wrapper, with_alloc_func<FlexibleArena>>) {
            arena.reset();
        }
    }
}

static void BM_Invoke_Direct(benchmark::State& state) {
    size_t val = 1;
    for (auto _ : state) {
        benchmark::DoNotOptimize(val = Stateless{}(val));
    }
}

BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<StdFunc>, Stateless)
    ->Name("functional/flexible_function/create/std/zero_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<FlexibleSBO>, Stateless)
    ->Name("functional/flexible_function/create/urlicht/sbo/zero_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<FlexibleSBO>, NonTypeTag)
    ->Name("functional/flexible_function/create/urlicht/nontype/zero_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<StdFunc>, MidCallable)
    ->Name("functional/flexible_function/create/std/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<FlexibleSBO>, MidCallable)
    ->Name("functional/flexible_function/create/urlicht/sbo/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<FlexibleHeap>, MidCallable)
    ->Name("functional/flexible_function/create/urlicht/heap/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, with_alloc_func<FlexibleArena>, MidCallable)
    ->Name("functional/flexible_function/create/urlicht/arena/mid_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<StdFunc>, LargeCallable)
    ->Name("functional/flexible_function/create/std/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<FlexibleSBO>, LargeCallable)
    ->Name("functional/flexible_function/create/urlicht/sbo/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, no_alloc_func<FlexibleHeap>, LargeCallable)
    ->Name("functional/flexible_function/create/urlicht/heap/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Create, with_alloc_func<FlexibleArena>, LargeCallable)
    ->Name("functional/flexible_function/create/urlicht/arena/large_size")
    ->Repetitions(10)->ReportAggregatesOnly(true);

//Invoke
BENCHMARK(BM_Invoke_Direct)
    ->Name("functional/flexible_function/invoke/direct")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Invoke, StdFunc, Stateless)
    ->Name("functional/flexible_function/invoke/std")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Invoke, FlexibleSBO, Stateless)
    ->Name("functional/flexible_function/invoke/urlicht/sbo")
    ->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Invoke, FlexibleSBO, NonTypeTag)
    ->Name("functional/flexible_function/invoke/urlicht/nontype")
    ->Repetitions(10)->ReportAggregatesOnly(true);
