#include <urlicht/concurrency/mpmc_queue.h>

#include <atomic>
#include <iostream>
#include <thread>

int main() {
    urlicht::concurrency::mpmc_queue<int, urlicht::concurrency::capacity<64>> jobs;
    std::atomic<int> processed{};

    auto producer = [&] {
        for (int job = 1; job <= 25; ++job) {
            while (!jobs.try_emplace(job)) {
            }
        }
    };
    auto consumer = [&] {
        for (int count = 0; count < 25; ++count) {
            while (!jobs.try_consume_front([&](int job) noexcept { processed += job; })) {
            }
        }
    };

    std::thread first_producer(producer);
    std::thread second_producer(producer);
    std::thread first_consumer(consumer);
    std::thread second_consumer(consumer);
    first_producer.join();
    second_producer.join();
    first_consumer.join();
    second_consumer.join();

    std::cout << "Processed job weight: " << processed << '\n';
}
