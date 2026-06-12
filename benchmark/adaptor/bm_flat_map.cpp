#include <benchmark/benchmark.h>

#include <urlicht/adaptor/flat_map.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <numeric>
#include <random>
#include <utility>
#include <vector>

#ifndef URLICHT_BM_ADAPTOR_HAS_BOOST
#define URLICHT_BM_ADAPTOR_HAS_BOOST 0
#endif

#if URLICHT_BM_ADAPTOR_HAS_BOOST
#include <boost/container/flat_map.hpp>
#endif

using key_type = std::uint64_t;
using mapped_type = std::string;
using value_type = std::pair<key_type, mapped_type>;

constexpr std::uint32_t seed = 0xC0FFEEu;

std::vector<value_type> make_shuffled_pairs(std::size_t n, key_type base_key = 0) {
    std::vector<value_type> data;

    for (std::size_t i = 0; i < n; ++i) {
        const auto k = base_key + i * 2;
        const auto v = std::to_string(k);
        data.emplace_back(k, v);
    }
    std::mt19937 rng(seed);
    std::ranges::shuffle(data, rng);
    return data;
}

std::vector<key_type> make_shuffled_keys(std::size_t n, key_type base_key = 0) {
    std::vector<key_type> keys;

    for (std::size_t i = 0; i < n; ++i) {
        keys.push_back(static_cast<key_type>(base_key + i * 1.8));
    }
    std::mt19937 rng(seed);
    std::ranges::shuffle(keys, rng);
    return keys;
}

template <class Map>
static void BM_try_emplace(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto data = make_shuffled_pairs(n);

    for (auto _ : state) {
        Map m;
        for (auto&& [k, v] : data) {
            benchmark::DoNotOptimize(m.try_emplace(k, std::move(v)));
        }
        benchmark::DoNotOptimize(m.size());
        benchmark::ClobberMemory();
    }
    state.SetItemsProcessed(state.iterations() * n);
}

template <class Map>
static void BM_insert_range(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto original_data = make_shuffled_pairs(n);
    const auto data = make_shuffled_pairs(n);

    for (auto _ : state) {
        state.PauseTiming();
        Map m(original_data.rbegin(), original_data.rend());
        state.ResumeTiming();

        m.insert(data.begin(), data.end());

        benchmark::DoNotOptimize(m.size());
        benchmark::ClobberMemory();
    }

    state.SetItemsProcessed(state.iterations() * n);
}

template <class Map>
static void BM_find(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto init_data = make_shuffled_pairs(n);
    const auto queries = make_shuffled_keys(n);

    Map m(init_data.begin(), init_data.end());

    for (auto _ : state) {
        for (const auto key : queries) {
            auto it = m.find(key);
            benchmark::DoNotOptimize(it);
        }
        benchmark::ClobberMemory();
    }

    state.SetItemsProcessed(state.iterations() * queries.size());
}

template <class Map>
static void BM_traversal(benchmark::State& state) {
    const auto n = static_cast<std::size_t>(state.range(0));
    const auto init_data = make_shuffled_pairs(n);

    Map m(init_data.begin(), init_data.end());

    for (auto _ : state) {
        for (auto it = m.begin(); it != m.end(); ++it) {
            benchmark::DoNotOptimize(it);
        }
        benchmark::ClobberMemory();

        for (auto it = m.rbegin(); it != m.rend(); ++it) {
            benchmark::DoNotOptimize(it);
        }
        benchmark::ClobberMemory();
    }
    state.SetItemsProcessed(state.iterations() * n * 2);
}

template <class Map>
static void BM_erase(benchmark::State& state) {
    const std::size_t n = static_cast<std::size_t>(state.range(0));
    const auto initial_data = make_shuffled_pairs(n);
    const auto erase_keys = make_shuffled_keys(n);

    for (auto _ : state) {
        state.PauseTiming();
        Map m(initial_data.begin(), initial_data.end());
        state.ResumeTiming();

        for (const auto key : erase_keys) {
            benchmark::DoNotOptimize(m.erase(key));
        }
        benchmark::ClobberMemory();
        benchmark::DoNotOptimize(m);
    }

    state.SetItemsProcessed(state.iterations() * erase_keys.size());
}

#define BM_ARGS() \
    ->RangeMultiplier(4)->Range(1 << 8, 1 << 16)->Repetitions(10)->ReportAggregatesOnly(true);

#define URLICHT_REGISTER_MAP(MapType, Label) \
    BENCHMARK_TEMPLATE(BM_try_emplace, MapType)->Name("try_emplace/" Label) BM_ARGS(); \
    BENCHMARK_TEMPLATE(BM_insert_range, MapType)->Name("insert_range/" Label) BM_ARGS(); \
    BENCHMARK_TEMPLATE(BM_traversal, MapType)->Name("traversal/" Label) BM_ARGS(); \
    BENCHMARK_TEMPLATE(BM_find, MapType) ->Name("find/" Label) BM_ARGS(); \
    BENCHMARK_TEMPLATE(BM_erase, MapType) ->Name("erase/" Label) BM_ARGS();


using urlicht_flat_map_t = urlicht::flat_map<key_type, mapped_type>;
URLICHT_REGISTER_MAP(urlicht_flat_map_t, "urlicht::flat_map")

#if URLICHT_BM_ADAPTOR_HAS_BOOST
using boost_flat_map_t = boost::container::flat_map<key_type, mapped_type>;
URLICHT_REGISTER_MAP(boost_flat_map_t, "boost::flat_map")
#endif

using std_map_t = std::map<key_type, mapped_type>;
URLICHT_REGISTER_MAP(std_map_t, "std::map")


#undef URLICHT_REGISTER_MAP
#undef BM_ARGS
