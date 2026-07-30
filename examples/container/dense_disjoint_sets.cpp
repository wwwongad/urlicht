#include <urlicht/container/dense_disjoint_sets.h>

#include <iostream>
#include <vector>

int main() {
    // Index each computer in a small office network.
    urlicht::container::dense_disjoint_sets<> network(6);
    for (const auto [left, right] : std::vector<std::pair<int, int>>{
             {0, 1}, {1, 2}, {3, 4}}) {
        network.try_unite(left, right);
    }

    std::cout << "Computer 0 and 2 connected: " << std::boolalpha
              << network.same_set(0, 2) << '\n';
    std::cout << "Connected network groups: " << network.set_count() << '\n';
}
