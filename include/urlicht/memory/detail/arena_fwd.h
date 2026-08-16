#ifndef URLICHT_ARENA_FWD_H
#define URLICHT_ARENA_FWD_H
#include <memory>
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
    }
#endif

    struct arena_growth_policy {
        size_t initial_size = 1024;
        double growth_rate = 1.2;
    };

    /************************ URLICHT ARENA **************************/

    template <bool UseUpstream = true,
              arena_growth_policy GrowthPolicy = arena_growth_policy{},
              urlicht::concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class arena;

    namespace detail {
        template <typename T>
        struct is_arena : std::false_type {};

        template <bool U, arena_growth_policy P, typename A>
        struct is_arena<arena<U, P, A>> : std::true_type {};
    }

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_arena_v = memory::detail::is_arena<T>::value;
}

namespace urlicht::memory {

    /************************ URLICHT CONCURRENT ARENA **************************/

    template <bool UseUpstream = true,
              arena_growth_policy GrowthPolicy = arena_growth_policy{},
              urlicht::concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class concurrent_arena;

    namespace detail {
        template <typename T>
        struct is_concurrent_arena : std::false_type {};

        template <bool U, arena_growth_policy P, urlicht::concepts::allocator A>
        struct is_concurrent_arena<concurrent_arena<U, P, A>> : std::true_type {};
    }

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_concurrent_arena_v =
        memory::detail::is_concurrent_arena<T>::value;
}

namespace urlicht::memory {

    /************************ URLICHT ARENA VIEW **************************/

    template <urlicht::concepts::object T, bool UnsafeAllocInit = false, typename Arena = arena<>>
    class arena_view;

}

namespace urlicht {
    template <typename A>
    inline constexpr bool is_urlicht_arena_view_compatible_v =
        is_urlicht_arena_v<A> || is_urlicht_concurrent_arena_v<A>;
}

namespace urlicht::memory {

    namespace detail {
        template <typename T>
        struct is_arena_view : std::false_type {};

        template <typename T, bool I, typename A>
        struct is_arena_view<arena_view<T, I, A>> : std::true_type {};

        template <typename T>
        struct no_upstream_arena_impl {};

        template <bool U, arena_growth_policy P, typename A>
        struct no_upstream_arena_impl<arena<U, P, A>> {
            using type = arena<false, P, A>;
        };

        template <bool U, arena_growth_policy P, typename A>
        struct no_upstream_arena_impl<concurrent_arena<U, P, A>> {
            using type = concurrent_arena<false, P, A>;
        };

        template <typename Arena, bool UnsafeAllocInit>
        using zero_overhead_arena =
            std::conditional_t<UnsafeAllocInit, typename no_upstream_arena_impl<Arena>::type, Arena>;
    }

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_arena_view_v = memory::detail::is_arena_view<T>::value;
}

namespace urlicht::memory {

    /************************ URLICHT PMR ARENA RESOURCE **************************/

    namespace pmr {
        /**
         * @brief Owning wrapper class for urlicht::memory::arena that conforms to the std::pmr::memory_resource interface.
         * @tparam Arena The underlying urlicht::memory::arena for raw memory allocation. Defaults to
         *               urlicht::memory::arena<true, std::allocator<std::byte>, ArenaGrowthPolicy{}>.
         */
        template <typename Arena = arena<>, bool UnsafeAllocInit = false>
        class arena_resource;
    }

    namespace detail {
        template <typename T>
        struct is_arena_resource : std::false_type {};

        template <typename A, bool U>
        struct is_arena_resource<pmr::arena_resource<A, U>> : std::true_type {};
    }

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_pmr_arena_resource_v =
        memory::detail::is_arena_resource<T>::value;
}

#endif //URLICHT_ARENA_FWD_H
