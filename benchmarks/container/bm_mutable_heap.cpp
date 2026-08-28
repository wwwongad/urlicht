#include <urlicht/container/d_ary_heap.h>
#include <benchmark/benchmark.h>
#include <vector>
#include <limits>
#include <random>
#include <algorithm>

#if URLICHT_BM_HAS_BOOST
#include <boost/heap/d_ary_heap.hpp>
#endif

constexpr auto INF = std::numeric_limits<double>::max();
constexpr auto SRC_VERTEX = 0U;

struct edge_t {
    size_t to;
    double weight;
};

using graph_t = std::vector<std::vector<edge_t>>;

struct heap_info_t {
    size_t vertex;
    double dist;

    friend bool operator>(const heap_info_t& lhs, const heap_info_t& rhs) {
        return lhs.dist > rhs.dist;
    }
};


static graph_t make_random_graph(const size_t n, const double avg_degree = 8.0) {
    graph_t graph(n);

    std::mt19937_64 rng(0xDEADBEEFull ^ n);
    std::uniform_real_distribution weight_dist(1.0, 100.0);

    auto add_edge = [&](const size_t a, const size_t b) {
        const double w = weight_dist(rng);
        graph[a].push_back({b, w});
        graph[b].push_back({a, w});
    };

    std::vector<size_t> perm(n);
    std::iota(perm.begin(), perm.end(), size_t{0});
    std::ranges::shuffle(perm, rng);

    for (size_t i = 1; i < n; ++i) {
        std::uniform_int_distribution<size_t> pick(0, i - 1);
        add_edge(perm[i], perm[pick(rng)]);
    }

    const size_t tree_edges  = n - 1;
    const size_t target_edges =
        static_cast<size_t>(avg_degree * static_cast<double>(n) / 2.0);
    const size_t extra_edges =
        target_edges > tree_edges ? target_edges - tree_edges : 0;

    std::uniform_int_distribution<size_t> vertex_dist(0, n - 1);
    for (size_t e = 0; e < extra_edges; ++e) {
        const size_t a = vertex_dist(rng);
        const size_t b = vertex_dist(rng);
        if (a != b) {
            add_edge(a, b);
        }
    }
    return graph;
}

using urlicht_mutable_heap_no_reuse =
    urlicht::container::d_ary_heap<
        heap_info_t,
        std::vector,
        std::greater<>,
        {.mutable_ = true, .reuse_id = false, .track_generation = false}
    >;

using urlicht_mutable_heap_reuse =
    urlicht::container::d_ary_heap<
        heap_info_t,
        std::vector,
        std::greater<>,
        {.mutable_ = true, .reuse_id = true, .track_generation = false}
    >;

template <typename UHeap>
static void BM_urlicht_mutable_heap(benchmark::State& st) {
    const auto n = st.range(0); // Graph size
    const graph_t graph = make_random_graph(n);

    using handle_t = UHeap::handle_type;

    for (auto _ : st) {
        std::vector dist(n, INF);
        dist[SRC_VERTEX] = 0.0;

        std::vector<handle_t> handles(n);  // All handles invalid by default

        UHeap heap;
        heap.reserve(n);

        handles[SRC_VERTEX] = heap.push({SRC_VERTEX, 0.0});

        while (!heap.empty()) {
            const auto [v, d] = heap.top();
            heap.unchecked_pop();
            handles[v].invalidate();

            for (const auto [to, weight] : graph[v]) {
                if (const auto nd = d + weight; nd < dist[to]) {
                    dist[to] = nd;
                    if (handles[to].maybe_valid()) {
                        heap.unchecked_promote(handles[to], [&](auto& info) {
                            info.dist = nd;
                        });
                    } else {
                        handles[to] = heap.push({to, nd});
                    }
                }
            }
        }

        benchmark::DoNotOptimize(dist.data());
        benchmark::ClobberMemory();
    }
    st.SetItemsProcessed(st.iterations() * n);
}

BENCHMARK_TEMPLATE(BM_urlicht_mutable_heap, urlicht_mutable_heap_no_reuse)->Range(1 << 10, 1 << 20)
    ->Name("urlicht::d_ary_heap/no_reuse")
    ->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_urlicht_mutable_heap, urlicht_mutable_heap_reuse)->Range(1 << 10, 1 << 20)
    ->Name("urlicht::d_ary_heap/reuse_id")
    ->Repetitions(10)->ReportAggregatesOnly(true);

#if URLICHT_BM_HAS_BOOST
using boost_mutable_heap =
    boost::heap::d_ary_heap<
        heap_info_t,
        boost::heap::arity<4>,
        boost::heap::mutable_<true>,
        boost::heap::compare<std::greater<>>
    >;
using boost_handle = boost_mutable_heap::handle_type;

static void BM_boost_mutable_heap(benchmark::State& st) {
    const auto n = st.range(0);
    const graph_t graph = make_random_graph(n);

    for (auto _ : st) {
        std::vector dist(n, INF);
        dist[SRC_VERTEX] = 0.0;

        std::vector<char> in_heap(n, 0);
        std::vector<boost_handle> handles(n);

        boost_mutable_heap heap;
        heap.reserve(n);

        handles[SRC_VERTEX] = heap.push({SRC_VERTEX, 0.0});
        in_heap[SRC_VERTEX] = 1;

        while (!heap.empty()) {
            const auto [v, d] = heap.top();
            heap.pop();
            in_heap[v]  = 0;

            for (const auto [to, weight] : graph[v]) {
                if (const auto nd = d + weight; nd < dist[to]) {
                    dist[to] = nd;
                    if (in_heap[to]) {
                        heap.update(handles[to], {to, nd});
                    } else {
                        handles[to] = heap.push({to, nd});
                        in_heap[to] = 1;
                    }
                }
            }
        }
        benchmark::DoNotOptimize(dist.data());
        benchmark::ClobberMemory();
    }
    st.SetItemsProcessed(st.iterations() * n);
}

BENCHMARK(BM_boost_mutable_heap)->Range(1 << 10, 1 << 20)
    ->Name("boost::d_ary_heap")->Repetitions(10)->ReportAggregatesOnly(true);
#endif

