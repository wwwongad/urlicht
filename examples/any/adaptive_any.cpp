#include <urlicht/any/adaptive_any.h>

#include <iostream>
#include <string>
#include <unordered_map>

int main() {
    using setting = urlicht::any::adaptive_any<32>;
    std::unordered_map<std::string, setting> settings;

    settings.emplace("retries", 3);
    settings.emplace("endpoint", std::string{"https://api.example.test"});

    if (const auto* retries = urlicht::any::any_cast<int>(&settings.at("retries"))) {
        std::cout << "Retry budget: " << *retries << '\n';
    }
    std::cout << "Endpoint: "
              << urlicht::any::any_cast<std::string>(settings.at("endpoint")) << '\n';
}
