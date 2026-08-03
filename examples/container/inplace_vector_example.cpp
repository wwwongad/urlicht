#include <urlicht/container/inplace_vector.h>

#include <algorithm>
#include <array>
#include <format>
#include <iostream>
#include <numeric>
#include <ranges>
#include <stack>
#include <string>
#include <string_view>
#include <tuple>

enum class service_level {
    medical,
    express,
    standard
};

struct delivery {
    int stop;
    std::string_view recipient;
    service_level service;
    double weight_kg;
    int eta_minutes;
    bool cancelled;
};

constexpr std::string_view service_name(const service_level service) {
    switch (service) {
        case service_level::medical:
            return "medical";
        case service_level::express:
            return "express";
        case service_level::standard:
            return "standard";
    }
    return "unknown";
}

constexpr int service_rank(const service_level service) {
    switch (service) {
        case service_level::medical:
            return 0;
        case service_level::express:
            return 1;
        case service_level::standard:
            return 2;
    }
    return 3;
}

template <typename Manifest>
void print_manifest(const Manifest& manifest) {
    std::cout << std::format(
        "{:<5} {:<16} {:<10} {:>9} {:>8}\n",
        "Stop", "Recipient", "Service", "Weight", "ETA");
    std::cout << std::string(54, '-') << '\n';

    for (const auto& item : manifest) {
        std::cout << std::format(
            "{:<5} {:<16} {:<10} {:>7.1f} kg {:>5} min\n",
            item.stop,
            item.recipient,
            service_name(item.service),
            item.weight_kg,
            item.eta_minutes);
    }
}

int main() {
    using manifest_type = urlicht::container::inplace_vector<delivery, 8>;

    const std::array delivery_feed{
        delivery{101, "Central Clinic", service_level::medical, 2.4, 18, false},
        delivery{102, "North Bakery", service_level::standard, 7.8, 34, true},
        delivery{103, "Cedar Pharmacy", service_level::medical, 1.1, 22, false},
        delivery{104, "Orchid Hotel", service_level::express, 4.6, 28, false},
        delivery{105, "Union Offices", service_level::standard, 9.3, 41, false},
        delivery{106, "West Lab", service_level::express, 3.2, 25, false},
        delivery{107, "City Library", service_level::standard, 5.0, 37, false},
        delivery{108, "Harbor Cafe", service_level::express, 6.4, 31, false},
        delivery{109, "South Hospice", service_level::medical, 1.8, 20, false},
    };

    manifest_type manifest;
    auto pending = manifest.try_append_range(delivery_feed);

    std::cout << std::format(
        "Loaded {} of {} dispatches into fixed capacity {}.\n",
        manifest.ssize(),
        delivery_feed.size(),
        static_cast<std::size_t>(manifest.capacity()));

    std::cout << std::format(
        "{} dispatch(es) deferred while the manifest was full.\n",
        std::ranges::distance(pending, delivery_feed.end()));

    const auto size_before_cancellation = manifest.ssize();
    manifest.erase_if([](const delivery& item) {
        return item.cancelled;
    });
    const auto cancelled = size_before_cancellation - manifest.ssize();

    pending = manifest.try_append_range(pending, delivery_feed.end());
    std::cout << std::format(
        "Removed {} cancellation(s); deferred work now remaining: {}.\n\n",
        cancelled,
        std::ranges::distance(pending, delivery_feed.end()));

    std::ranges::sort(manifest, [](const delivery& lhs, const delivery& rhs) {
        return std::tuple{service_rank(lhs.service), lhs.eta_minutes}
             < std::tuple{service_rank(rhs.service), rhs.eta_minutes};
    });

    print_manifest(manifest);

    const auto total_weight = std::accumulate(
        manifest.begin(),
        manifest.end(),
        0.0,
        [](const double total, const delivery& item) {
            return total + item.weight_kg;
        });
    const auto priority_stops = std::ranges::count_if(
        manifest,
        [](const delivery& item) {
            return item.service != service_level::standard;
        });

    std::cout << std::format(
        "\nRoute summary: {} stops, {} priority, {:.1f} kg total.\n",
        manifest.ssize(),
        priority_stops,
        total_weight);
}
