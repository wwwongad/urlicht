#include <iostream>
#include <benchmark/benchmark.h>
#include <urlicht/container/d_ary_heap.h>
#include <queue>
#include <random>
#include <unordered_map>
#include <limits>

template <typename PQueue>
class StdPQWrapper {
    PQueue pq;
public:
    template <typename Rng, typename Comp>
    StdPQWrapper(Rng&& rng, Comp comp) : pq(rng.begin(), rng.end(), std::move(comp)) {}

    void replace_top(const auto& x) {
        pq.pop();
        pq.emplace(x);
    }

    template <typename ...Args>
    void emplace(Args&& ...args) { pq.emplace(std::forward<Args>(args)...); }

    void pop() { pq.pop(); }
    const auto& top() const { return pq.top(); }
    bool empty() const { return pq.empty(); }
    size_t size() const { return pq.size(); }
    void reserve(size_t n) { }
    void output_sorted(std::vector<typename PQueue::value_type>& res) {
        res.clear();
        while (!pq.empty()) {
            res.push_back(pq.top());
            pq.pop();
        }
    }
};


template <typename PQueue>
class UrlichtPQWrapper {
    PQueue pq;
public:
    template <typename Rng, typename Comp>
    UrlichtPQWrapper(Rng&& rng, Comp comp) : pq(std::forward<Rng>(rng), std::move(comp)) {}

    void replace_top(const auto& x) {
        pq.unchecked_replace_top(x);
    }

    template <typename ...Args>
    void emplace(Args&& ...args) { pq.emplace(std::forward<Args>(args)...); }

    void pop() { pq.unchecked_pop(); }
    const auto& top() const { return pq.unchecked_top(); }
    bool empty() const { return pq.empty(); }
    size_t size() const { return pq.size(); }
    void reserve(size_t n) { pq.reserve(n); }
    void output_sorted(std::vector<typename PQueue::value_type>& res) {
        res.clear();
        pq.extract_sorted(std::back_inserter(res));
    }
};

template <typename T, typename Comp>
using std_priority_queue = StdPQWrapper<std::priority_queue<T, std::vector<T>, Comp>>;
template <typename T, typename Comp>
using urlicht_binary_heap = UrlichtPQWrapper<urlicht::container::priority_queue<T, std::vector<T>, Comp, 2>>;

// kNN benchmark
constexpr size_t dim = 10;
using point_t = std::array<double, dim>;

class Dist {
public:
    double operator()(const point_t& a, const point_t& b) const {
        double dist = 0.0;
        for (size_t i = 0; i < a.size(); ++i) {
            dist += (a[i] - b[i]) * (a[i] - b[i]);
        }
        return dist;
    }
};

class UncachedComp {
    point_t query_;
    std::vector<point_t>& points_;
public:
    UncachedComp(std::vector<point_t>& points, const point_t& query)
        : query_(query), points_(points){}

    UncachedComp(const UncachedComp&) = default;
    UncachedComp(UncachedComp&&) = default;

    bool operator()(const size_t x, const size_t y) const {
        return Dist{}(points_[x], query_) < Dist{}(points_[y], query_);  // yields a max heap
    }
};

class CachedComp {
    point_t query_;
    std::vector<point_t>& points_;
    mutable std::vector<double> dist_cache_;
public:
    CachedComp(std::vector<point_t>& points, const point_t& query)
        : query_(query), points_(points), dist_cache_(points_.size(), -1.0) {}

    CachedComp(const CachedComp&) = default;
    CachedComp(CachedComp&&) = default;

    bool operator()(const size_t x, const size_t y) const {
        if (dist_cache_[x] < 0.0) {
            dist_cache_[x] = Dist{}(points_[x], query_);
        }
        if (dist_cache_[y] < 0.0) {
            dist_cache_[y] = Dist{}(points_[y], query_);
        }
        return dist_cache_[x] < dist_cache_[y];  // yields a max heap
    }
};

auto make_rand_points(size_t n = 1000U) {
    std::mt19937 gen(42);
    std::uniform_real_distribution dist(0.0, 1.0);
    std::vector<point_t> points;
    points.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        point_t pt;
        for (size_t j = 0; j < dim; ++j) {
            pt[j] = dist(gen);
        }
        points.emplace_back(std::move(pt));
    }
    return points;
}

struct KNNData {
    int64_t n = 10000;
    int64_t k = static_cast<int64_t>(std::sqrt(n));
    std::vector<point_t> points = make_rand_points(n);
    point_t query = {0.5, 0.5};
};

template <typename Wrapper, typename Comp>
static void BM_kNN(benchmark::State& state) {
    KNNData data{.n = state.range(0)};
    std::vector<size_t> k(data.k);
    std::iota(k.begin(), k.end(), 0);
    for (auto _ : state) {
        state.PauseTiming();

        Comp comp(data.points, data.query);
        Wrapper pq(k, comp);

        state.ResumeTiming();

        for (size_t i = data.k; i < data.n; ++i) {
            if (comp(i, pq.top())) {
                pq.replace_top(i);
            }
            benchmark::DoNotOptimize(&pq.top());
        }
        benchmark::ClobberMemory();
    }
}


BENCHMARK_TEMPLATE(BM_kNN, std_priority_queue<size_t, UncachedComp>, UncachedComp)
    ->RangeMultiplier(10)->Range(1000, 100000)
    ->Name("priority_queue/std/kNN/uncached")->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_kNN, urlicht_binary_heap<size_t, UncachedComp>, UncachedComp)
    ->RangeMultiplier(10)->Range(1000, 100000)
    ->Name("priority_queue/urlicht/kNN/uncached")->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_kNN, std_priority_queue<size_t, CachedComp>, CachedComp)
    ->RangeMultiplier(10)->Range(1000, 100000)
    ->Name("priority_queue/std/kNN/cached")->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_kNN, urlicht_binary_heap<size_t, CachedComp>, CachedComp)
    ->RangeMultiplier(10)->Range(1000, 100000)
    ->Name("priority_queue/urlicht/kNN/cached")->Repetitions(10)->ReportAggregatesOnly(true);


template <bool IsTrivial = true>
class MakeSorted {
public:
    auto operator()(const size_t n = 1000) const
    requires (IsTrivial) {
        std::mt19937_64 gen(42);
        std::uniform_int_distribution<int64_t> dist(
            std::numeric_limits<int32_t>::min(), std::numeric_limits<int32_t>::max());

        std::vector<int64_t> data;
        data.reserve(n);
        for (size_t i = 0U; i < n; ++i) {
            data.push_back(dist(gen));
        }
        return data;
    }

    auto operator()(const size_t n = 1000) const
    requires (!IsTrivial) {
        std::vector<std::string> data;
        data.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            data.emplace_back(std::to_string(i + 100000) +
                            " char padding to bypass the small string optimization.");
        }
        return data;
    }

};

template <typename Wrapper, bool IsTrivial = true>
static void BM_sorting(benchmark::State& state) {
    const size_t n = state.range(0);
    auto data = MakeSorted<IsTrivial>{}(n);
    std::vector<typename decltype(data)::value_type> out; out.reserve(n);

    for (auto _ : state) {
        state.PauseTiming();
        auto copy = data;
        state.ResumeTiming();

        Wrapper pq(std::move(copy), std::greater{});
        pq.output_sorted(out);
        benchmark::DoNotOptimize(out.data());
        benchmark::DoNotOptimize(out.size());
        benchmark::ClobberMemory();
    }
}


BENCHMARK_TEMPLATE(BM_sorting, std_priority_queue<int64_t, std::greater<>>)
    ->RangeMultiplier(10)->Range(100, 20000)
    ->Name("priority_queue/std/sorting/trivial")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_sorting, urlicht_binary_heap<int64_t, std::greater<>>)
    ->RangeMultiplier(10)->Range(100, 20000)
    ->Name("priority_queue/urlicht/sorting/trivial")->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_sorting, std_priority_queue<std::string, std::greater<>>, false)
    ->RangeMultiplier(10)->Range(100, 20000)
    ->Name("priority_queue/std/sorting/non-trivial")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_sorting, urlicht_binary_heap<std::string, std::greater<>>, false)
    ->RangeMultiplier(10)->Range(100, 20000)
    ->Name("priority_queue/urlicht/sorting/non-trivial")->Repetitions(10)->ReportAggregatesOnly(true);

struct Graph {
    int n;
    std::vector<std::vector<std::pair<int, double>>> adj;
    Graph(int n, int degree) : n(n), adj(n) {
        std::mt19937_64 gen(123);
        std::uniform_int_distribution vdist(0, n - 1);
        std::uniform_real_distribution wdist(1.0, 10.0);
        for (int u = 0; u < n; ++u) {
            for (int i = 0; i < degree; ++i) {
                int v = vdist(gen);
                while (v == u) v = vdist(gen);  // no self‑loops
                double w = wdist(gen);
                adj[u].emplace_back(v, w);
            }
        }
    }
};

const Graph& get_cached_graph(const int n, const int degree) {
    static std::unordered_map<int, Graph> stable_graphs;
    auto it = stable_graphs.find(n);
    if (it == stable_graphs.end()) {
        it = stable_graphs.try_emplace(n, Graph(n, degree)).first;
    }
    return it->second;
}

template <typename Wrapper>
static void BM_Dijkstra(benchmark::State& state) {
    const int n = state.range(0);
    const int degree = std::max(50, n / 10);

    const Graph& graph = get_cached_graph(n, degree);

    std::vector<double> dist(n);
    for (auto _ : state) {
        constexpr int src = 0;
        state.PauseTiming();
        std::ranges::fill(dist, std::numeric_limits<double>::infinity());
        dist[src] = 0.0;

        Wrapper pq(std::vector<std::pair<double, int>>{}, std::greater<>{});
        pq.emplace(0.0, src);

        state.ResumeTiming();

        pq.reserve(n * 2);

        while (!pq.empty()) {
            auto [d, u] = pq.top();
            pq.pop();
            if (d != dist[u]) continue;

            for (const auto& [v, w] : graph.adj[u]) {
                double nd = d + w;
                if (nd < dist[v]) {
                    dist[v] = nd;
                    pq.emplace(nd, v);
                }
            }
            benchmark::DoNotOptimize(dist.data());
        }
        benchmark::ClobberMemory();
    }
}


BENCHMARK_TEMPLATE(BM_Dijkstra, std_priority_queue<std::pair<double, int>, std::greater<>>)
    ->RangeMultiplier(10)->Range(100, 10000)
    ->Name("priority_queue/std/dijkstra")->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_Dijkstra, urlicht_binary_heap<std::pair<double, int>, std::greater<>>)
    ->RangeMultiplier(10)->Range(100, 10000)
    ->Name("priority_queue/urlicht/dijkstra")->Repetitions(10)->ReportAggregatesOnly(true);
