#ifndef URLICHT_MEMORY_DETAIL_RESOURCE_TRAITS_H
#define URLICHT_MEMORY_DETAIL_RESOURCE_TRAITS_H

#include <cstddef>
#include <type_traits>

// Concepts enabling optimization dispatch in resource_view and pmr::arena_resource.
namespace urlicht::memory::detail {

    // True when &Resource::deallocate denotes a static member (a function pointer) rather
    // than a non-static member (a pointer-to-member). A static no-op deallocate means
    // resource_view can skip forwarding deallocations entirely.
    template <typename Resource>
    concept has_noop_deallocate = requires {
        &Resource::deallocate;
    } && !std::is_member_pointer_v<decltype(&Resource::deallocate)>;

    // True when the resource exposes an unchecked_allocate member: an unchecked, no-throw
    // allocation fast path (e.g. arena::unchecked_allocate).
    template <typename Resource>
    concept has_unchecked_allocate = requires(Resource& resource) {
        resource.unchecked_allocate(std::size_t{}, std::size_t{});
    };

} // namespace urlicht::memory::detail

#endif //URLICHT_MEMORY_DETAIL_RESOURCE_TRAITS_H
