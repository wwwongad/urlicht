#include <urlicht/concurrency/spsc_queue.h>

#include <atomic>
#include <iostream>
#include <thread>

int main() {
    urlicht::concurrency::spsc_queue<int, urlicht::concurrency::capacity<64>> readings;
    std::atomic<int> total{};

    std::thread producer([&] {
        for (int reading = 1; reading <= 100; ++reading) {
            while (!readings.try_emplace(reading)) {
            }
        }
    });
    std::thread consumer([&] {
        for (int count = 0; count < 100; ++count) {
            while (!readings.try_consume_front([&](const int value) noexcept { total += value; })) {
            }
        }
    });

    producer.join();
    consumer.join();
    std::cout << "Sum of sensor readings: " << total << '\n';
}
