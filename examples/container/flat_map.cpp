#include <urlicht/container/flat_map.h>

#include <iostream>
#include <string>

int main() {
    urlicht::container::flat_map<std::string, int> inventory{
        {"tea", 8}, {"coffee", 12}, {"cocoa", 5}
    };

    // flat_map stores its sorted keys and values in parallel contiguous containers.
    for (std::size_t index = 0; index < inventory.size(); ++index) {
        std::cout << inventory.keys()[index] << ": "
                  << inventory.values()[index] << '\n';
    }

    const auto first_c = inventory.lower_bound("cocoa");
    if (first_c != inventory.end()) {
        std::cout << "First product at or after cocoa: " << first_c->first << '\n';
    }
}
