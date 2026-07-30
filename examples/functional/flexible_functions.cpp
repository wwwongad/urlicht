#include <urlicht/functional/flexible_function.h>
#include <urlicht/functional/flexible_move_only_function.h>

#include <iostream>
#include <memory>
#include <string>

int main() {
    urlicht::functional::flexible_function<void(const std::string&)> audit =
        [](const std::string& event) { std::cout << "audit: " << event << '\n'; };
    audit("account-created");

    auto retry_count = std::make_unique<int>(0);
    urlicht::functional::flexible_move_only_function<bool() noexcept> retry =
        [count = std::move(retry_count)]() mutable noexcept { return ++*count <= 3; };

    std::cout << "Retry allowed: " << std::boolalpha << retry() << '\n';
}
