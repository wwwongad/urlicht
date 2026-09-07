#ifndef URLICHT_MEMORY_DETAIL_ARENA_FWD_H
#define URLICHT_MEMORY_DETAIL_ARENA_FWD_H
#include <memory>
#include <urlicht/memory/allocation_options.h>
#include <urlicht/concepts/concepts.h>
#include <urlicht/internal/config.h>
#include <cstddef>
#include <bit>
#include <type_traits>

namespace urlicht::memory {
    namespace detail {

        // Chunk footer (metadata) is placed at the end of the buffer, as follows:
        //  | start               <----  curr              | &chunk_footer
        ////////////////////////////////////////////////////////////////////////
        ///                   user data                |     chunk_footer    ///
        ////////////////////////////////////////////////////////////////////////

        struct initial_buffer {
            std::byte* start;
            std::byte* curr;
            std::size_t size;
            bool external;

            constexpr std::byte* end() const noexcept {
                return start + size;
            }
        };

        struct chunk_footer {
            chunk_footer* next;
            std::byte* start;
            std::byte* curr;
            std::size_t allocation_size;

            constexpr auto actual_buffer_size() const noexcept {
                return std::bit_cast<const std::byte*>(this) - start;
            }
        };

#if UL_HAS_CPP23
        template <typename Pointer, typename SizeType = std::size_t>
        using allocation_result_impl = std::allocation_result<Pointer, SizeType>;
#else
        template <typename Pointer, typename SizeType = std::size_t>
        struct allocation_result_impl {
            Pointer ptr;
            SizeType count;
        };
#endif
    } // namespace detail

    struct arena_growth_policy {
        size_t initial_size = 1024;
        double growth_rate = 1.2;
    };

    /************************ URLICHT ARENA **************************/

    template <resource_options Opt = resource_options{},
              arena_growth_policy GrowthPolicy = arena_growth_policy{},
              urlicht::concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class arena;

    namespace detail {
        template <typename>
        struct is_arena : std::false_type {};

        template <resource_options Opt, arena_growth_policy GP, typename Up>
        struct is_arena<arena<Opt, GP, Up>> : std::true_type {};
    }

    /************************ URLICHT CONCURRENT ARENA **************************/

    template <resource_options Opt = resource_options{},
              arena_growth_policy GrowthPolicy = arena_growth_policy{},
              urlicht::concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class concurrent_arena;

    namespace detail {
        template <typename>
        struct is_concurrent_arena : std::false_type {};

        template <resource_options Opt, arena_growth_policy GP, typename Up>
        struct is_concurrent_arena<concurrent_arena<Opt, GP, Up>> : std::true_type {};
    }

    /************************ URLICHT RESOURCE VIEW **************************/

    template <urlicht::concepts::object T, typename Resource = arena<>, allocator_options Opt = allocator_options{}>
    class resource_view;

    namespace detail {
        template <typename>
        struct is_resource_view : std::false_type {};

        template <typename T, typename Resource, allocator_options Opt>
        struct is_resource_view<resource_view<T, Resource, Opt>> : std::true_type {};
    }

    /************************ URLICHT PMR ARENA RESOURCE **************************/

    namespace pmr {
        /**
         * @brief Owning wrapper class for urlicht::memory::arena that conforms to the std::pmr::memory_resource interface.
         * @tparam Arena The underlying urlicht::memory::arena for raw memory allocation. Defaults to
         *               urlicht::memory::arena<resource_options{}, std::allocator<std::byte>, ArenaGrowthPolicy{}>.
         */
        template <typename Arena = arena<>, allocator_options Opt = allocator_options{}>
        class arena_resource;
    }

    namespace detail {
        template <typename>
        struct is_arena_resource : std::false_type {};

        template <typename Arena, allocator_options Opt>
        struct is_arena_resource<pmr::arena_resource<Arena, Opt>> : std::true_type {};
    }

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_arena_v = memory::detail::is_arena<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_concurrent_arena_v = memory::detail::is_concurrent_arena<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_resource_view_v = memory::detail::is_resource_view<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_pmr_arena_resource_v = memory::detail::is_arena_resource<T>::value;
}

namespace urlicht::memory::detail {
    // Defines whether a memory resource class is compatible with resource_view
    template <typename T>
    inline constexpr bool is_urlicht_memory_resource_v =
        urlicht::is_urlicht_arena_v<T> || urlicht::is_urlicht_concurrent_arena_v<T>;
}

#endif //URLICHT_MEMORY_DETAIL_ARENA_FWD_H
