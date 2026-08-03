#include <urlicht/container/d_ary_heap.h>
#include <iomanip>
#include <iostream>
#include <vector>
#include <random>

constexpr size_t dim = 10;
using point_t = std::array<double, dim>;

double dist(const point_t& a, const point_t& b) noexcept {
    double dist = 0.0;
    for (size_t i = 0; i < a.size(); ++i) {
        dist += (a[i] - b[i]) * (a[i] - b[i]);
    }
    return dist;
}

class Comp {
    point_t query_;
    std::vector<point_t>& points_;
    mutable std::vector<double> dist_cache_;
public:
    Comp(std::vector<point_t>& points, const point_t& query)
        : query_(query), points_(points), dist_cache_(points_.size(), -1.0) {}

    Comp(const Comp&) = default;
    Comp(Comp&&) = default;

    bool operator()(const size_t x, const size_t y) const {
        if (dist_cache_[x] < 0.0) {
            dist_cache_[x] = dist(points_[x], query_);
        }
        if (dist_cache_[y] < 0.0) {
            dist_cache_[y] = dist(points_[y], query_);
        }
        return dist_cache_[x] < dist_cache_[y];  // yields a max heap
    }
};

std::mt19937 gen(42);
std::uniform_real_distribution udist(0.0, 1.0);

auto make_rand_point() {
    point_t pt{};
    for (size_t i = 0; i < dim; ++i) {
        pt[i] = udist(gen);
    }
    return pt;
}

auto make_rand_points(const size_t n = 1000U) {
    std::vector<point_t> points;
    for (size_t i = 0; i < n; ++i) {
        points.emplace_back(make_rand_point());
    }
    return points;
}

struct KNNData {
    int64_t n = 100;
    int64_t k = static_cast<int64_t>(std::sqrt(n));
    std::vector<point_t> points = make_rand_points(n);
    point_t query = make_rand_point();
};

std::ostream& operator<<(std::ostream& os, const point_t& pt) {
    os << std::setprecision(3) << "[";
    for (size_t i = 0; i < dim - 1; ++i) {
        os << pt[i] << ", ";
    }
    os << pt[dim - 1] << "]";
    return os;
}

int main() {
    KNNData data{};
    std::vector<size_t> k(data.k);
    std::iota(k.begin(), k.end(), 0);

    Comp comp(data.points, data.query);
    urlicht::container::d_ary_heap<size_t, std::vector, Comp> heap(k, comp);

    for (size_t i = data.k; i < data.n; ++i) {
        if (comp(i, heap.top())) {
            heap.unchecked_replace_top(i);
        }
    }

    std::cout << "The top " << data.k << " nearest neighbors of point " << data.query << " are: \n";
    for (const size_t idx : heap) {
        std::cout << data.points[idx] << ", \n";
    }
}
