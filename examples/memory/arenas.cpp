#include <urlicht/memory/arena.h>
#include <urlicht/memory/arena_view.h>
#include <urlicht/memory/concurrent_arena.h>

#include <iostream>
#include <string>
#include <thread>
#include <vector>

int main() {
    using fixed_arena = urlicht::memory::arena<false>;
    fixed_arena frame_memory(4 * 1024);
    std::vector<std::string, urlicht::memory::arena_view<std::string, false, fixed_arena>> labels(
        frame_memory);
    labels.emplace_back("player");
    labels.emplace_back("enemy");
    std::cout << "Frame entities: " << labels.size() << '\n';
    labels.clear();
    frame_memory.reset();

    urlicht::memory::concurrent_arena<> shared_memory(4 * 1024);
    std::thread worker_a([&] { (void)shared_memory.allocate(128, alignof(int)); });
    std::thread worker_b([&] { (void)shared_memory.allocate(128, alignof(double)); });
    worker_a.join();
    worker_b.join();
    std::cout << "Concurrent scratch allocations completed\n";
}
