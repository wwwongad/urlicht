#include <urlicht/any/adaptive_any.h>

#include <vector>
#include <iostream>
#include <string>
#include <unordered_map>

enum class type_t {
    string,
    integer,
    boolean,
    floating_point,
    vector,
    unordered_map
};

constexpr std::string to_string(type_t t) {
    switch (t) {
        case type_t::string:
            return "string";
        case type_t::integer:
            return "integer";
        case type_t::boolean:
            return "boolean";
        case type_t::floating_point:
            return "floating_point";
        case type_t::vector:
            return "vector";
        case type_t::unordered_map:
            return "unordered_map";
        default:
            return "unknown";
    }
}

struct setting_t {
    type_t type;
    urlicht::any::adaptive_any<64> obj;
};

int main() {
    std::unordered_map<std::string, setting_t> settings;

    settings["username"] = {type_t::string, std::string("admin")};
    settings["max_connections"] = {type_t::integer, 100};
    settings["enable_logging"] = {type_t::boolean, true};
    settings["cost_per_token"] = {type_t::floating_point, 3.14159};
    settings["supported_formats"] =
        {type_t::vector, std::vector<std::string>{"json", "xml", "csv"}};
    settings["allowed use time"] =
        {type_t::unordered_map, std::unordered_map<std::string, int>{{"cmd", 120}, {"notepad", 600}}};

    for (const auto& [key, setting] : settings) {
        std::cout << key << " (" << to_string(setting.type) << "): ";
        switch (setting.type) {
            case type_t::string:
                std::cout << urlicht::any::any_cast<std::string>(setting.obj);
                break;
            case type_t::integer:
                std::cout << urlicht::any::any_cast<int>(setting.obj);
                break;
            case type_t::boolean:
                std::cout << (urlicht::any::any_cast<bool>(setting.obj) ? "true" : "false");
                break;
            case type_t::floating_point:
                std::cout << urlicht::any::any_cast<double>(setting.obj);
                break;
            case type_t::vector: {
                for (const auto& item : urlicht::any::any_cast<std::vector<std::string>>(setting.obj)) {
                    std::cout << item << " ";
                }
                break;
            }
            case type_t::unordered_map: {
                const auto& map = urlicht::any::any_cast<std::unordered_map<std::string, int>>(setting.obj);
                for (const auto& [key2, value] : map) {
                    std::cout << '[' << key2 << ", " << value << "] ";
                }
                break;
            }
        }
        std::cout << '\n';
    }

    return 0;
}
