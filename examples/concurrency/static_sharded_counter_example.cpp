#include <urlicht/concurrency/static_sharded_counter.h>

#include <iostream>
#include <thread>
#include <vector>

int main() {
    urlicht::concurrency::static_sharded_counter<std::size_t, 128, 4> requests;
    std::vector<std::jthread> workers;

    for (int worker = 0; worker < 4; ++worker) {
        workers.emplace_back([&] {
            for (int request = 0; request < 1'000; ++request) {
                requests.increment();
            }
        });
    }

    std::cout << "Approximate requires: " << requests.get_approximate() << '\n';
    std::cout << "Exact requests: " << requests.get_exact() << '\n';
}
