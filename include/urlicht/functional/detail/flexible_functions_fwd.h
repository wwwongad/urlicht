#ifndef URLICHT_FLEXIBLE_FUNCTIONS_FWD_H
#define URLICHT_FLEXIBLE_FUNCTIONS_FWD_H
#include <cstddef>
#include <type_traits>
#include <memory>

namespace urlicht::functional {
    /**
     * @brief A type-erased function wrapper with threefold advantages over std::function: customizable SBO
     *        and alignment size, specifier awareness, and typed allocator/PMR support. Functional objects
     *        are stored inline when they meet the size and alignment requirements. Otherwise, they are allocated
     *        on the heap using the provided allocator as a fallback.
     *
     * @tparam T Function signature. Supports all valid combinations of noexcept, &/&&, and const.
     * @tparam OptimizeForSize SBO size. Defaults to 64.
     * @tparam OptimizeForAlign Alignment size. Defaults to {alignof(std::max_align_t)}.
     * @tparam Alloc Allocator for heap storage. Its value_type must be std::byte.
     */
    template <typename T,
              size_t OptimizeForSize = 64u,
              size_t OptimizeForAlign = alignof(std::max_align_t),
              typename Alloc = std::allocator<std::byte>>
    class flexible_function;

    /**
    * @brief A type-erased function wrapper with threefold advantages over std::function: customizable SBO
     *       and alignment size, specifier awareness, and typed allocator/PMR support. Functional objects
     *       are stored inline when they meet the size and alignment requirements. Otherwise, they are allocated
     *       on the heap using the provided allocator as a fallback. This
     *       class is move-only and cannot be copied.
     *
     * @tparam T Function signature. Supports all valid combinations of noexcept, &/&&, and const.
     * @tparam OptimizeForSize SBO size. Defaults to 64.
     * @tparam OptimizeForAlign Alignment size. Defaults to alignof(std::max_align_t).
     * @tparam Alloc Allocator for heap storage. Its value_type must be std::byte.
     */
    template <typename T,
              size_t OptimizeForSize = 64u,
              size_t OptimizeForAlign = alignof(std::max_align_t),
              typename Alloc = std::allocator<std::byte>>
    class flexible_move_only_function;

    namespace detail {
        template <typename>
        struct is_urlicht_flexible_function : std::false_type {};

        template <typename Sig, size_t S, size_t A, typename Alloc>
        struct is_urlicht_flexible_function<flexible_function<Sig, S, A, Alloc>>
            : std::true_type {};

        template <typename>
        struct is_urlicht_flexible_move_only_function : std::false_type {};

        template <typename Sig, size_t S, size_t A, typename Alloc>
        struct is_urlicht_flexible_move_only_function<flexible_move_only_function<Sig, S, A, Alloc>>
            : std::true_type {};
    }

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_flexible_function_v =
        functional::detail::is_urlicht_flexible_function<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_flexible_move_only_function_v =
        functional::detail::is_urlicht_flexible_move_only_function<T>::value;
}

#endif //URLICHT_FLEXIBLE_FUNCTIONS_FWD_H
