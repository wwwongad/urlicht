#ifndef URLICHT_MEMORY_DETAIL_RESOURCE_TRAITS_H
#define URLICHT_MEMORY_DETAIL_RESOURCE_TRAITS_H

#include <cstddef>
#include <type_traits>

// Concepts enabling optimization dispatch in resource_view and pmr::arena_resource.
namespace urlicht::memory::detail {

    template <typename Res>
    concept memory_resource = requires(Res& resource, void* ptr, std::size_t n, std::size_t align) {
        { resource.allocate(n, align) };
        { resource.deallocate(ptr, n, align) } -> std::same_as<void>;
    }
    && !requires { typename Res::value_type; } // memory resource is typeless
    && std::equality_comparable<Res>;

    // Opt-in trait: a resource whose deallocate is a static no-op (memory reclaimed only via
    // reset()/release(), e.g. arena/concurrent_arena) opts in by specializing this to
    // std::true_type. Defaults to std::false_type. resource_view consults it to decide
    // whether per-object deallocations can be elided entirely.
    template <typename Resource>
    struct has_noop_deallocate : std::false_type {};

    // True when the resource exposes an unchecked_allocate member: an unchecked, no-throw
    // allocation fast path (e.g. arena::unchecked_allocate).
    template <typename Resource>
    concept has_unchecked_allocate = requires(Resource& resource) {
        resource.unchecked_allocate(std::size_t{}, std::size_t{});
    };

} // namespace urlicht::memory::detail

#endif //URLICHT_MEMORY_DETAIL_RESOURCE_TRAITS_H
