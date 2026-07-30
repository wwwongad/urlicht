#include <urlicht/memory/pmr/arena_resource.h>

#include <iostream>
#include <memory_resource>
#include <string>
#include <vector>

int main() {
    urlicht::memory::pmr::arena_resource<> request_memory(8 * 1024);
    std::pmr::vector<std::pmr::string> response(&request_memory);

    response.emplace_back("HTTP/1.1 200 OK");
    response.emplace_back("Content-Type: application/json");
    std::cout << "Response headers: " << response.size() << '\n';

    response.clear();
    request_memory.reset();
}
