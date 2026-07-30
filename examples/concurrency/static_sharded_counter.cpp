#include <urlicht/concurrency/static_sharded_counter.h>

#include <iostream>
#include <thread>
#include <vector>

int main() {
    urlicht::concurrency::static_sharded_counter<std::size_t, 128, 4> requests;
    std::vector<std::thread> workers;

    for (int worker = 0; worker < 4; ++worker) {
        workers.emplace_back([&] {
            for (int request = 0; request < 1'000; ++request) {
                requests.increment();
            }
        });
    }
    for (auto& worker : workers) {
        worker.join();
    }

    std::cout << "Completed requests: " << requests.get_exact() << '\n';
}
