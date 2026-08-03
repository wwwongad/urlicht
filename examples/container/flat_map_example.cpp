#include <urlicht/container/flat_map.h>

#include <iostream>
#include <string>
#include <format>

// Specialized O(1) lower_bound algorithm for a range of arithmetic sequence
template <typename IntT = int>
class uniform_lower_bound_fn {
    IntT low_, high_, diff_;
public:
    uniform_lower_bound_fn(IntT low, IntT high, IntT diff)
        : low_(low), high_(high), diff_(diff) {
        if (diff_ <= 0)
            throw std::invalid_argument("diff must be positive");
    }

    uniform_lower_bound_fn(const uniform_lower_bound_fn&) = default;

    template <typename Rng, typename Comp>
    auto operator()(Rng&& rng, const IntT val, [[maybe_unused]] const Comp& comp) const {
        if (val < low_) {
            return std::ranges::begin(rng);
        }
        if (val >= high_) {
            throw std::out_of_range("lower_bound_fn");
        }
        auto off = static_cast<std::size_t>((val - low_ + diff_ - 1) / diff_);
        return std::ranges::begin(rng) + off;
    }
};

template <typename Map>
void print_map(const Map& map, const char* msg = "Current content of the flat_map:\n") {
    std::cout << msg;
    if (map.empty()) {
        std::cout << std::format("Empty map\n");
        return;
    }
    for (const auto&[k, v] : map) {
        std::cout << std::format("[{}, {}] ", k, v);
    }
    std::cout << std::endl;
}

int main() {
    std::vector keys{0, 2, 4, 6, 8, 10};
    std::vector<std::string> values{"zero", "two", "four", "six", "eight", "ten"};

    urlicht::container::flat_map /* <
        int, std::string, std::less<>, std::vector<int>, std::vector<std::string>, uniform_lower_bound_fn<>
    > */ map(keys, values, std::less{}, uniform_lower_bound_fn{0, 16, 2});
    print_map(map, "Initial content of flat_map: \n");

    std::cout << std::endl;

    std::cout << "Inserting new elements: \n";
    for (const auto& [k, v] : std::vector<std::pair<int, std::string>>{{12, "twelve"}, {14, "fourteen"}}) {
        map.try_emplace(k, v);
        std::cout << std::format("Inserted element [key={}, mapped=\"{}\"]\n", k, v);
    }
    print_map(map);

    std::cout << std::endl;

    std::cout << "Looking for keys in query: \n";
    for (int k : {1, 2, 5, 8, 9}) {
        if (auto it = map.find(k); it != map.end()) {
            std::cout << std::format("Key={} found, mapped = {}\n", k, it->second);
        } else {
            std::cout << std::format("Key={} not found\n", k);
        }
    }

    std::cout << std::endl;

    std::cout << "Returning the content of flat_map to the vectors: \n";
    auto [key_cont, mapped_cont] = std::move(map).extract();
    keys = std::move(key_cont);
    values = std::move(mapped_cont);

    print_map(map);

    std::cout << "Keys (std::vector<int>): [";
    for (auto k : keys) std::cout << k << ", ";
    std::cout << "]\n";

    std::cout << "Mapped (std::vector<std::string>): [";
    for (auto v : values) std::cout << std::format("\"{}\", ", v);
    std::cout << "]\n";
}

