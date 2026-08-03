#include <urlicht/concurrency/spsc_queue.h>
#include <memory>
#include <memory_resource>
#include <string>
#include <chrono>
#include <iostream>
#include <syncstream>
#include <thread>
#include <format>

auto& get_pool() noexcept {
    static std::pmr::synchronized_pool_resource pool{};
    return pool;
}

class Deleter {
    std::pmr::polymorphic_allocator<> alloc_;
public:
    template <typename T>
    Deleter(T& res) : alloc_{&res} {}

    void operator() (auto* const ptr) noexcept {
        std::destroy_at(ptr);
        alloc_.delete_object<std::string>(ptr);
    }
};

using allocator_t = std::pmr::polymorphic_allocator<>;
using ptr_t = std::unique_ptr<std::string, Deleter>;

int main() {
    constexpr size_t num_messages = 20;

    urlicht::concurrency::spsc_queue<ptr_t, urlicht::concurrency::capacity<16>> message_queue;
    std::jthread producer([&] {
        for (size_t i = 0; i < num_messages; ++i) {
            auto& pool = get_pool();
            allocator_t alloc{&pool};
            ptr_t ptr {alloc.new_object<std::string>(std::format("Message no.{}", i)), pool};

            std::osyncstream{std::cout} << "Sending message: " << *ptr << '\n';
            message_queue.emplace(std::move(ptr));

            std::this_thread::sleep_for(std::chrono::microseconds{100});
        }
    });
    std::jthread consumer([&] {
        for (size_t i = 0; i < num_messages; ++i) {
            message_queue.consume_front([](auto&& ptr) noexcept {
                std::osyncstream{std::cout} << "Received message: " << *ptr << '\n';
            });
        }
    });
}
