#include <urlicht/container/dense_disjoint_sets.h>

#include <format>
#include <iostream>
#include <vector>

struct edge {
    int from;
    int to;
    size_t weight;
};

std::ostream& operator<<(std::ostream& os, const edge& e) {
    os << std::format("{} -> {}, weight={}", e.from, e.to, e.weight);
    return os;
}

int main() {
    // A dense graph of vertices 0-4
    std::vector<edge> edges {{
        {0, 1, 12}, {0, 2, 6}, {0, 3, 34},
        {0, 4, 25}, {1, 2, 9}, {1, 3, 18},
        {1, 4, 22}, {2, 3, 13}, {2, 4, 40},
        {3, 4, 17}}
    };
    std::cout << "The edges of graph A are: \n";
    for (auto edge : edges) {
        std::cout << edge << std::endl;
    }

    std::ranges::sort(edges, std::less{}, &edge::weight);

    constexpr urlicht::container::dense_disjoint_sets_policy ds_policy = {
        .union_by = urlicht::container::dense_disjoint_sets_union_policy::by_rank,
        .path = urlicht::container::dense_disjoint_sets_path_policy::full_compression
    };

    urlicht::container::dense_disjoint_sets<std::uint8_t, ds_policy> ds(5);
    std::vector<edge> mst{};
    for (const auto [from, to, weight] : edges) {
        if (!ds.unchecked_same_set(from, to)) { // We are certain that {from} and {to} are within [0, 5).
            ds.unchecked_unite(from, to);
            mst.emplace_back(from, to, weight);
        }
    }
    std::cout << "The minimum spanning tree of A contains edges: \n";
    for (auto edge : mst) {
        std::cout << edge << std::endl;
    }
}
