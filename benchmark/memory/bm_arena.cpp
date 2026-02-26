#include <benchmark/benchmark.h>
#include <vector>
#include <list>
#include <map>
#include <string>
#include <memory_resource>
#include <urlicht/memory/arena.h>


constexpr size_t KB = 1024;
constexpr size_t MB = 1024 * 1024;

// ============================================================================
// WRAPPERS
// ============================================================================

template <bool Unsafe = false>
class UrlichtArenaWrapper {
    urlicht::arena<> arena;
public:
    UrlichtArenaWrapper(size_t size) : arena(size) {}

    void* allocate(size_t bytes, size_t alignment) noexcept(Unsafe) {
        if constexpr (Unsafe) {
            return arena.unchecked_allocate_initial(bytes, alignment);
        } else {
            return arena.allocate(bytes, alignment);
        }
    }

    void reset() noexcept {
        arena.reset();
    }
};

class STDMonotonicWrapper {
    std::vector<char> buf;
    std::pmr::monotonic_buffer_resource mbr;
public:
    STDMonotonicWrapper(size_t size)
        : buf(size), mbr(size > 0 ? buf.data() : nullptr, buf.size())
    { }

    void* allocate(size_t bytes, size_t alignment) {
        return mbr.allocate(bytes, alignment);
    }

    void reset() noexcept {
        mbr.release();
    }
};

// ============================================================================
// BENCHMARK: HOT PATH
// ============================================================================

template <typename Wrapper>
static void BM_Arena_HotPath(benchmark::State& state) {
    constexpr size_t total_size = 256 * MB;
    const size_t alloc_size = state.range(0);
    const size_t max_alloc = total_size / (alloc_size + 8);

    Wrapper arena(total_size);
    size_t alloc_cnt = 0;

    for (auto _ : state) {
        if (alloc_cnt >= max_alloc) [[unlikely]] {
            state.PauseTiming();
            arena.reset();
            alloc_cnt = 0;
            state.ResumeTiming();
        }
        void* p = arena.allocate(alloc_size, 8u);
        benchmark::DoNotOptimize(p);
        ++alloc_cnt;
    }
    state.SetBytesProcessed(state.iterations() * alloc_size);
}

BENCHMARK_TEMPLATE(BM_Arena_HotPath, UrlichtArenaWrapper<>)->Arg(8)->Arg(32)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("HotPath/Urlicht");

BENCHMARK_TEMPLATE(BM_Arena_HotPath, UrlichtArenaWrapper<true>)->Arg(8)->Arg(32)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("HotPath/UrlichtUnsafe");

BENCHMARK_TEMPLATE(BM_Arena_HotPath, STDMonotonicWrapper)->Arg(8)->Arg(32)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("HotPath/StdMonotonic");

// ============================================================================
// BENCHMARK: FRAME LOOP
// ============================================================================

template <bool Unsafe = false>
static void BM_UrlichtArena_FrameLoop(benchmark::State& state) {
    const size_t vec_size = state.range(0);
    urlicht::arena<false> arena(16 * KB);
    using view_type = urlicht::arena_view<int, Unsafe, decltype(arena)>;

    for (auto _ : state) {
        std::vector<int, view_type> vec(arena);
        for (size_t i = 0; i < vec_size; ++i) {
            vec.push_back(i);
        }
        benchmark::DoNotOptimize(vec.data());
        arena.reset();
    }
    state.SetBytesProcessed(state.iterations() * vec_size * sizeof(int));
}

static void BM_STDMonotonic_FrameLoop(benchmark::State& state) {
    const size_t vec_size = state.range(0);
    std::vector<char> buf(16 * KB);
    std::pmr::monotonic_buffer_resource res(buf.data(), buf.size());

    for (auto _ : state) {
        std::pmr::vector<int> vec(&res);
        for (size_t i = 0; i < vec_size; ++i) {
            vec.push_back(i);
        }
        benchmark::DoNotOptimize(vec.data());
        res.release();
    }
    state.SetBytesProcessed(state.iterations() * vec_size * sizeof(int));
}

BENCHMARK_TEMPLATE(BM_UrlichtArena_FrameLoop, false)->Arg(16)->Arg(128)->Arg(1024)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("FrameLoop/Urlicht");

BENCHMARK_TEMPLATE(BM_UrlichtArena_FrameLoop, true)->Arg(16)->Arg(128)->Arg(1024)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("FrameLoop/UrlichtUnsafe");

BENCHMARK(BM_STDMonotonic_FrameLoop)->Arg(16)->Arg(128)->Arg(1024)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("FrameLoop/StdMonotonic");

// ============================================================================
// BENCHMARK: SPILLOVER
// ============================================================================

template <typename Wrapper>
static void BM_Arena_Spillover(benchmark::State& state) {
    const size_t alloc_size = state.range(0);
    const size_t block_size = 128u;

    for (auto _ : state) {
        Wrapper arena(0u);

        size_t allocated = 0;
        while (allocated < alloc_size) {
            void* p = arena.allocate(block_size, 8u);
            benchmark::DoNotOptimize(p);
            allocated += block_size;
        }
    }
    state.SetBytesProcessed(state.iterations() * state.range(0));
}

BENCHMARK_TEMPLATE(BM_Arena_Spillover, UrlichtArenaWrapper<>)->Range(MB, 128 * MB)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("Spillover/Urlicht");

BENCHMARK_TEMPLATE(BM_Arena_Spillover, STDMonotonicWrapper)->Range(MB, 128 * MB)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("Spillover/StdMonotonic");

// ============================================================================
// BENCHMARK: NODE BURST (std::map)
// ============================================================================

static void BM_UrlichtArena_NodeBurst(benchmark::State& state) {
    const size_t num_nodes = state.range(0);
    // ~1/2 of required memory
    urlicht::arena<> arena(num_nodes * 20);
    using view_type = urlicht::arena_view<std::pair<const int, int>>;

    for (auto _ : state) {
        std::map<int, int, std::less<>, view_type> map(arena);
        for (size_t i = 0; i < num_nodes; ++i) {
            map.try_emplace(i, i);
        }
        benchmark::DoNotOptimize(map.size());
        arena.reset();
    }
}

static void BM_STDMonotonic_NodeBurst(benchmark::State& state) {
    const size_t num_nodes = state.range(0);
    std::pmr::monotonic_buffer_resource mbr(num_nodes * 8);

    for (auto _ : state) {
        std::pmr::map<int, int> map(&mbr);
        for (size_t i = 0; i < num_nodes; ++i) {
            map.try_emplace(i, i);
        }
        benchmark::DoNotOptimize(map.size());
        mbr.release();
    }
}

BENCHMARK(BM_UrlichtArena_NodeBurst)->Range(1024, 100 * 1024)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("NodeBurst/Urlicht");

BENCHMARK(BM_STDMonotonic_NodeBurst)->Range(1024, 100 * 1024)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("NodeBurst/StdMonotonic");

// ============================================================================
// NEW BENCHMARK: SUDDEN LARGE ALLOCATION
// ============================================================================

template <typename Wrapper>
static void BM_SuddenLargeAlloc(benchmark::State& state) {
    const size_t large_size = state.range(0);

    for (auto _ : state) {
        state.PauseTiming();
        Wrapper arena(128 * KB);
        state.ResumeTiming();
        void* p{};
        for (size_t i = 0; i < 1000; ++i) {
            if (i % 300 == 0) { // three times
                p = arena.allocate(large_size, 8u);
            } else {
                p = arena.allocate(128, 8);
            }
            benchmark::DoNotOptimize(p);
        }
    }
}

BENCHMARK_TEMPLATE(BM_SuddenLargeAlloc, UrlichtArenaWrapper<>)->Range(MB, 64 * MB)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("SuddenLargeAlloc/Urlicht");

BENCHMARK_TEMPLATE(BM_SuddenLargeAlloc, STDMonotonicWrapper)->Range(MB, 64 * MB)
    ->Repetitions(10)->ReportAggregatesOnly(true)->Name("SuddenLargeAlloc/StdMonotonic");

// ============================================================================
// BENCHMARK: CACHE LOCALITY (Iteration Speed)
// ============================================================================

struct CacheLocality_STDAlloc {
    std::list<std::string> list;

    CacheLocality_STDAlloc(size_t count) {
        for (size_t i = 0; i < count; ++i) {
            list.push_back("Small string");
        }
    }
};

struct CacheLocality_PMR_Monotonic {
    std::pmr::monotonic_buffer_resource res;
    std::pmr::list<std::string> list;

    CacheLocality_PMR_Monotonic(size_t count)
        : res{}, list(&res) {
        for (size_t i = 0; i < count; ++i) {
            list.push_back("Small string");
        }
    }
};

struct CacheLocality_UrlichtArena {
    using ArenaType = urlicht::arena<true, {2 * MB, 2}>;

    ArenaType arena;
    // Note: arena must be initialized before list
    std::list<std::string, urlicht::arena_view<std::string, false, ArenaType>> list;

    CacheLocality_UrlichtArena(size_t count)
        : arena{}, list(arena) {
        for (size_t i = 0; i < count; ++i) {
            list.push_back("Small string");
        }
    }
};

template <typename Fixture>
static void BM_Scenario_CacheLocality(benchmark::State& st) {
    const size_t list_size = st.range(0);

    Fixture fixture(list_size);

    for (auto _ : st) {
        for (auto& s : fixture.list) {
            benchmark::DoNotOptimize(s.size());
        }
        benchmark::ClobberMemory();
    }

    st.SetItemsProcessed(st.iterations() * list_size);
}

BENCHMARK_TEMPLATE(BM_Scenario_CacheLocality, CacheLocality_STDAlloc)
    ->Name("Locality/StdAlloc")->ReportAggregatesOnly(true)
    ->Range(1024, 1024 * 256)->Repetitions(10);

BENCHMARK_TEMPLATE(BM_Scenario_CacheLocality, CacheLocality_PMR_Monotonic)
    ->Name("Locality/StdMonotonic")->ReportAggregatesOnly(true)
    ->Range(1024, 1024 * 256)->Repetitions(10);

BENCHMARK_TEMPLATE(BM_Scenario_CacheLocality, CacheLocality_UrlichtArena)
    ->Name("Locality/Urlicht")->ReportAggregatesOnly(true)
    ->Range(1024, 1024 * 256)->Repetitions(10);

BENCHMARK_MAIN();
