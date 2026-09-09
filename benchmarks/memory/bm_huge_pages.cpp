#include <benchmark/benchmark.h>
#include <urlicht/memory/huge_pages.h>

#include <system_error>
#include <vector>
#include <random>

constexpr size_t KB = 1024;
constexpr size_t MB = 1024 * 1024;

constexpr size_t kTotalBytes = 32 * MB;
constexpr size_t kHugePageBytes = 2 * MB;
constexpr size_t kElemSize = sizeof(uint64_t);
constexpr size_t kElems = kTotalBytes / kElemSize;     // 2^22
constexpr size_t kStrideElems = (4 * KB) / kElemSize;  // one word per 4 kB page

class MallocMemory {
    std::vector<uint64_t> buf_;
public:
    bool try_reserve() {
        buf_.assign(kElems, 0);
        return true;
    }
    uint64_t* data() noexcept {
        return buf_.data();
    }
};

// 32 MB backed by 2 MB huge pages.
class HugePagesMemory {
    urlicht::memory::huge_pages pages_;
public:
    bool try_reserve() {
        try {
            pages_ = urlicht::memory::huge_pages{
                urlicht::memory::huge_page_size::SIZE_2MB,
                kTotalBytes / kHugePageBytes
            };
        } catch (const std::system_error&) {
            return false; // No huge pages configured/free on this host.
        }
        if (pages_.data() == nullptr) {
            return false;
        }
        return true;
    }

    uint64_t* data() const noexcept {
        return static_cast<uint64_t*>(pages_.data());
    }
};

// ============================================================================
// BENCHMARK: SEQUENTIAL ACCESS
// ============================================================================

template <typename Region>
static void BM_SeqAccess(benchmark::State& state) {
    Region region;
    if (!region.try_reserve()) {
        state.SkipWithError("huge pages unavailable on this host");
        return;
    }
    const uint64_t* arr = region.data();

    for (auto _ : state) {
        uint64_t acc = 0;
        for (size_t i = 0; i < kElems; ++i) {
            acc += arr[i];
        }
        benchmark::DoNotOptimize(acc);
        benchmark::ClobberMemory();
    }
    state.SetItemsProcessed(state.iterations() * kElems);
}

BENCHMARK_TEMPLATE(BM_SeqAccess, MallocMemory)
    ->Name("SeqAccess/Malloc4K")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_SeqAccess, HugePagesMemory)
    ->Name("SeqAccess/HugePages2M")->Repetitions(10)->ReportAggregatesOnly(true);

// ============================================================================
// BENCHMARK: RANDOM ACCESS
// ============================================================================

// Fixed seed
std::mt19937 rng{0x111222333};
std::uniform_int_distribution<uint64_t> dist{0, kElems - 1};

template <typename Region>
static void BM_RandomAccess(benchmark::State& state) {
    Region region;
    if (!region.try_reserve()) {
        state.SkipWithError("huge pages unavailable on this host");
        return;
    }
    const uint64_t* arr = region.data();

    std::vector<std::size_t> indices(kElems);
    for (std::size_t i = 0; i < kElems; ++i) {
        indices[i] = dist(rng);
    }

    for (auto _ : state) {
        uint64_t acc = 0;
        for (const auto idx : indices) {
            acc += arr[idx];
        }
        benchmark::DoNotOptimize(acc);
        benchmark::ClobberMemory();
    }
    state.SetItemsProcessed(state.iterations() * kElems);
}

BENCHMARK_TEMPLATE(BM_RandomAccess, MallocMemory)
    ->Name("RandomAccess/Malloc4K")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_RandomAccess, HugePagesMemory)
    ->Name("RandomAccess/HugePages2M")->Repetitions(10)->ReportAggregatesOnly(true);
