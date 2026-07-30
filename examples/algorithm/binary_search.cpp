#include <urlicht/algorithm/binary_search.h>

#include <iostream>
#include <string>
#include <vector>

struct product {
    int sku;
    std::string name;
};

int main() {
    const std::vector<product> catalog{
        {1001, "notebook"}, {1003, "pen"}, {1007, "stapler"}
    };

    constexpr int requested_sku = 1003;
    const bool available = urlicht::algorithm::binary_search(catalog, requested_sku, std::less{}, &product::sku);

    std::cout << "SKU " << requested_sku
              << (available ? " is in stock\n" : " is unavailable\n");
}
