#include <urlicht/memory/tagged_ptr.h>

#include <iostream>
#include <unordered_map>

struct alignas(8) connection {
    int id;
};

int main() {
    connection primary{42};
    urlicht::memory::tagged_ptr current(&primary);
    current.set_tag(1);  // Mark the connection as authenticated.

    std::unordered_map<urlicht::memory::tagged_ptr<connection>, const char*> states;
    states.emplace(current, "authenticated");
    std::cout << "Connection " << current->id << ": " << states.at(current) << '\n';

    auto owned = urlicht::memory::make_tagged<connection, true>(connection{7});
    std::cout << "Owned fallback connection: " << owned->id << '\n';
}
