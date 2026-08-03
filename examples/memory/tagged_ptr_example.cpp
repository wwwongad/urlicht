#include <urlicht/memory/tagged_ptr.h>

#include <algorithm>
#include <array>
#include <format>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

struct alignas(16) work_order {
    int id;
    std::string_view destination;
    int service_minutes;
};

using work_handle = urlicht::memory::tagged_ptr<work_order>;

constexpr work_handle::tag_size_type cold_chain_bit = 2;
constexpr work_handle::tag_size_type inspected_bit = 3;
constexpr unsigned priority_mask = 0b0011U;

template <typename Handle>
constexpr unsigned priority(const Handle& handle) {
    return handle.template tag<unsigned>() & priority_mask;
}

template <typename Handle>
constexpr bool is_cold_chain(const Handle& handle) {
    return handle.tag_bit(cold_chain_bit);
}

template <typename Handle>
constexpr auto make_tag(const unsigned priority_value, const bool cold_chain, const bool inspected) {
    using tag_type = typename Handle::tag_type;
    auto tag = static_cast<tag_type>(priority_value & priority_mask);
    if (cold_chain) {
        tag = static_cast<tag_type>(tag | (1U << cold_chain_bit));
    }
    if (inspected) {
        tag = static_cast<tag_type>(tag | (1U << inspected_bit));
    }
    return tag;
}

constexpr std::string_view priority_name(const unsigned value) {
    constexpr std::array names{"routine", "standard", "express", "critical"};
    return names[value & priority_mask];
}

template <typename Handle>
void print_dispatch_queue(const std::vector<Handle>& queue) {
    std::cout << std::format(
        "{:<6} {:<18} {:<10} {:<8} {:>8}\n",
        "Order", "Destination", "Priority", "Cold", "Service");
    std::cout << std::string(56, '-') << '\n';

    for (const auto& handle : queue) {
        std::cout << std::format(
            "{:<6} {:<18} {:<10} {:<8} {:>5} min\n",
            handle->id,
            handle->destination,
            priority_name(priority(handle)),
            is_cold_chain(handle) ? "yes" : "no",
            handle->service_minutes);
    }
}

int main() {
    static_assert(work_handle::num_free_bits() == 4);

    std::array orders{
        work_order{4101, "Central Clinic", 18},
        work_order{4102, "Harbor Market", 12},
        work_order{4103, "North Laboratory", 25},
        work_order{4104, "Union Hotel", 15},
        work_order{4105, "West Pharmacy", 20},
    };

    std::vector<work_handle> dispatch_queue;
    dispatch_queue.reserve(orders.size());

    const auto enqueue = [&](
        work_order& order,
        const unsigned priority_value,
        const bool cold_chain,
        const bool inspected) {
        work_handle handle(&order);
        handle.set_tag(
            make_tag<work_handle>(priority_value, cold_chain, inspected));
        dispatch_queue.push_back(handle);
    };

    enqueue(orders[0], 3, true, true);
    enqueue(orders[1], 0, false, true);
    enqueue(orders[2], 2, true, false);
    enqueue(orders[3], 1, false, true);
    enqueue(orders[4], 2, true, true);

    std::ranges::sort(
        dispatch_queue,
        [](const auto& lhs, const auto& rhs) {
            if (priority(lhs) != priority(rhs)) {
                return priority(lhs) > priority(rhs);
            }
            return lhs->service_minutes < rhs->service_minutes;
        });

    std::cout << "Warehouse dispatch queue (metadata packed into pointer tags):\n";
    print_dispatch_queue(dispatch_queue);

    std::unordered_map<work_handle, std::string_view> inspection_state;
    for (const auto& handle : dispatch_queue) {
        inspection_state.emplace(
            handle,
            handle.tag_bit(inspected_bit) ? "cleared" : "inspection required");
    }

    std::cout << "\nHash-based inspection lookup:\n";
    for (const auto& handle : dispatch_queue) {
        std::cout << std::format(
            "  order {} -> {}\n",
            handle->id,
            inspection_state.at(handle));
    }

    const auto [raw_order, packed_tag] = dispatch_queue.front();
    const auto* addressed_order = std::to_address(dispatch_queue.front());
    std::cout << std::format(
        "\nStructured view: order {}, tag=0b{:04b}, pointer_traits={}\n",
        raw_order->id,
        static_cast<unsigned>(packed_tag),
        addressed_order == raw_order ? "same address" : "mismatch");
    std::cout << std::format(
        "Standard formatter exposes packed integer value: {}\n",
        dispatch_queue.front());

    auto untagged = dispatch_queue.front();
    untagged.clear_tag();
    std::cout << std::format(
        "Same pointer without its tag is {} hash key.\n",
        inspection_state.contains(untagged) ? "the same" : "a distinct");

    using owning_handle = urlicht::memory::tagged_ptr<work_order, true>;
    std::vector<owning_handle> overflow_queue;
    overflow_queue.emplace_back(
        urlicht::memory::make_tagged<work_order, true>(
            work_order{4202, "South Hospice", 11}));
    overflow_queue.emplace_back(
        urlicht::memory::make_tagged<work_order, true>(
            work_order{4201, "Airport Depot", 14}));

    overflow_queue[0].set_tag(make_tag<owning_handle>(3, true, false));
    overflow_queue[1].set_tag(make_tag<owning_handle>(1, false, true));

    std::cout << "\nOwning spillover queue:\n";
    print_dispatch_queue(overflow_queue);
}
