
#ifndef URLICHT_ARENA_FWD_H
#define URLICHT_ARENA_FWD_H
#include <memory>
#include <urlicht/concepts_utility.h>
#include <urlicht/config.h>
#include <cstddef>
#include <bit>
#include <type_traits>

namespace urlicht {
    namespace memory_detail {

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

    }

    struct ArenaGrowthPolicy {
        size_t initial_size = 1024;
        double growth_rate = 1.2;
    };

    /************************ URLICHT ARENA **************************/

    template <bool UseUpstream = true,
              ArenaGrowthPolicy GrowthPolicy = ArenaGrowthPolicy{},
              concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class arena;

    namespace memory_detail {
        template <typename T>
        struct is_arena : std::false_type {};

        template <bool U, ArenaGrowthPolicy P, typename A>
        struct is_arena<arena<U, P, A>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_arena_v = memory_detail::is_arena<T>::value;

    /************************ URLICHT CONCURRENT ARENA **************************/

    template <bool UseUpstream = true,
              ArenaGrowthPolicy GrowthPolicy = ArenaGrowthPolicy{},
              concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class concurrent_arena;

    namespace memory_detail {
        template <typename T>
        struct is_concurrent_arena : std::false_type {};

        template <bool U, ArenaGrowthPolicy P, concepts::allocator A>
        struct is_concurrent_arena<concurrent_arena<U, P, A>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_concurrent_arena_v = memory_detail::is_concurrent_arena<T>::value;

    /************************ URLICHT ARENA VIEW **************************/

    template <concepts::object T, bool UnsafeAllocInit = false, typename Arena = arena<>>
    class arena_view;

    template <typename A>
    inline constexpr bool is_urlicht_arena_view_compatible_v =
                is_urlicht_arena_v<A> || is_urlicht_concurrent_arena_v<A>;

    namespace memory_detail {
        template <typename T>
        struct is_arena_view : std::false_type {};

        template <typename T, bool I, typename A>
        struct is_arena_view<arena_view<T, I, A>> : std::true_type {};

        template <typename T>
        struct no_upstream_arena_impl {};

        template <bool U, ArenaGrowthPolicy P, typename A>
        struct no_upstream_arena_impl<arena<U, P, A>> {
            using type = arena<false, P, A>;
        };

        template <bool U, ArenaGrowthPolicy P, typename A>
        struct no_upstream_arena_impl<concurrent_arena<U, P, A>> {
            using type = concurrent_arena<false, P, A>;
        };

        template <typename Arena, bool UnsafeAllocInit>
        using zero_overhead_arena =
            std::conditional_t<UnsafeAllocInit, typename no_upstream_arena_impl<Arena>::type, Arena>;
    }

    template <typename T>
    inline constexpr bool is_urlicht_arena_view_v = memory_detail::is_arena_view<T>::value;

    /************************ URLICHT PMR ARENA RESOURCE **************************/

    namespace pmr {
        /**
         * @brief Owning wrapper class for urlicht::arena that conforms to the std::pmr::memory_resource interface.
         * @tparam Arena The underlying urlicht::arena for raw memory allocation. Defaults to
         *               urlicht::arena<true, std::allocator<std::byte>, ArenaGrowthPolicy{}>.
         */
        template <typename Arena = arena<>, bool UnsafeAllocInit = false>
        class arena_resource;
    }

    namespace memory_detail {
        template <typename T>
        struct is_arena_resource : std::false_type {};

        template <typename A, bool U>
        struct is_arena_resource<pmr::arena_resource<A, U>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_pmr_arena_resource_v = memory_detail::is_arena_resource<T>::value;
}

#endif //URLICHT_ARENA_FWD_H
