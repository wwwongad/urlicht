#ifndef URLICHT_ADAPTIVE_FUNCTIONS_FWD_H
#define URLICHT_ADAPTIVE_FUNCTIONS_FWD_H
#include <cstddef>

namespace urlicht {
    /**
     * @brief A specifier-aware polymorphic function wrapper with customizable small-object optimization
     *        (SBO) size and alignment. Functional objects are stored inline when they meet the size and alignment
     *        requirements. Otherwise, they are allocated on the heap as a fallback.
     *
     * @tparam T Function signature. Supports all valid combinations of noexcept, &/&&, and const.
     * @tparam OptimizeForSize SBO size. Defaults to 64.
     * @tparam OptimizeForAlign Alignment size. Defaults to {alignof(std::max_align_t)}.
     */
    template <typename T, size_t OptimizeForSize = 64u, size_t OptimizeForAlign = alignof(std::max_align_t)>
    class adaptive_function;

    /**
     * @brief A specifier-aware polymorphic function wrapper with customizable small-object optimization
     *        (SBO) size and alignment. Functional objects are stored inline when they
     *        meet the size and alignment requirements; otherwise they fall back to heap allocation. This
     *        class is move-only and cannot be copied.
     *
     * @tparam T Function signature. Supports all valid combinations of noexcept, &/&&, and const.
     * @tparam OptimizeForSize SBO size. Defaults to 64.
     * @tparam OptimizeForAlign Alignment size. Defaults to alignof(std::max_align_t).
     */
    template <typename T, size_t OptimizeForSize = 64u, size_t OptimizeForAlign = alignof(std::max_align_t)>
    class adaptive_move_only_function;

    namespace functional_detail {
        template <typename>
        struct is_urlicht_adaptive_function : std::false_type {};

        template <typename Sig, size_t S, size_t A>
        struct is_urlicht_adaptive_function<adaptive_function<Sig, S, A>> : std::true_type {};

        template <typename>
        struct is_urlicht_adaptive_move_only_function : std::false_type {};

        template <typename Sig, size_t S, size_t A>
        struct is_urlicht_adaptive_move_only_function<adaptive_move_only_function<Sig, S, A>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_adaptive_function_v =
        functional_detail::is_urlicht_adaptive_function<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_adaptive_move_only_function_v =
        functional_detail::is_urlicht_adaptive_move_only_function<T>::value;
}

#endif //URLICHT_ADAPTIVE_FUNCTIONS_FWD_H
