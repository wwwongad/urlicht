#ifndef COMPARE_H
#define COMPARE_H
#include <concepts>
#include <functional>
#include <cstdint>
#include <urlicht/concepts_utility.h>

namespace urlicht::compare {

    // urlicht::compare::less
    namespace detail {
        template <typename T, typename U = T>
        requires concepts::less_comparable<T, U>
        struct total_order_less {
            using PTy = std::remove_cvref_t<T>;
            using PUy = std::remove_cvref_t<U>;

            [[nodiscard]]
            constexpr bool operator()(const PTy& lhs, const PUy& rhs) const
            noexcept(concepts::nothrow_less_comparable<PTy, PUy>) {
                if constexpr (concepts::decays_to_ptr<T> && concepts::decays_to_ptr<U>)
                    return std::bit_cast<std::uintptr_t>(lhs) < std::bit_cast<std::uintptr_t>(rhs);
                else
                    return lhs < rhs;
            }
        };
    }

    template <typename T = void, typename U = T>
    struct less {
        using PTy = std::remove_reference_t<T>;
        using PUy = std::remove_reference_t<U>;

        [[nodiscard]]
        constexpr bool operator()(const PTy& lhs, const PUy& rhs) const
        noexcept(noexcept(detail::total_order_less<T, U>{}(lhs, rhs)))
        requires concepts::less_comparable<T, U> {
            return detail::total_order_less<T, U>{}(lhs, rhs);
        }

        [[nodiscard]]
        constexpr bool operator()(const PUy& lhs, const PTy& rhs) const
        noexcept(noexcept(detail::total_order_less<U, T>{}(lhs, rhs)))
        requires (!std::same_as<PTy, PUy>) && concepts::less_comparable<U, T> {
            return detail::total_order_less<U, T>{}(lhs, rhs);
        }
    };

    template <>
    struct less<void> {
        using is_transparent = void;

        template <typename T, typename U>
        requires concepts::less_comparable<T, U>
        [[nodiscard]]
        constexpr bool operator()(const T& lhs, const U& rhs) const
        noexcept(noexcept(detail::total_order_less<T, U>{}(lhs, rhs))) {
            return detail::total_order_less<T, U>{}(lhs, rhs);
        }
    };

    // urlicht::compare::greater
    namespace detail {
        template <typename T, typename U = T>
        requires concepts::greater_comparable<T, U>
        struct total_order_greater {
            using PTy = std::remove_reference_t<T>;
            using PUy = std::remove_reference_t<U>;

            [[nodiscard]]
            constexpr bool operator()(const PTy& lhs, const PUy& rhs) const
            noexcept(concepts::nothrow_greater_comparable<PTy, PUy>) {
                if constexpr (concepts::decays_to_ptr<T> && concepts::decays_to_ptr<U>){
                    return std::bit_cast<std::uintptr_t>(lhs) > std::bit_cast<std::uintptr_t>(rhs);
                } else {
                    return lhs > rhs;
                }
            }
        };
    }

    template <typename T = void, typename U = T>
    struct greater {
        using PTy = std::remove_reference_t<T>;
        using PUy = std::remove_reference_t<U>;

        [[nodiscard]]
        constexpr bool operator()(const PTy& lhs, const PUy& rhs) const
        noexcept(noexcept(detail::total_order_greater<T, U>{}(lhs, rhs)))
        requires concepts::greater_comparable<T, U> {
            return detail::total_order_greater<T, U>{}(lhs, rhs);
        }

        [[nodiscard]]
        constexpr bool operator()(const PUy& lhs, const PTy& rhs) const
        noexcept(noexcept(detail::total_order_greater<U, T>{}(lhs, rhs)))
        requires (!std::same_as<PTy, PUy>) && concepts::greater_comparable<U, T> {
            return detail::total_order_greater<U, T>{}(lhs, rhs);
        }
    };

    template <>
    struct greater<void> {
        using is_transparent = void;

        template <typename T, typename U>
        requires concepts::greater_comparable<T, U>
        [[nodiscard]]
        constexpr bool operator()(const T& lhs, const U& rhs) const
        noexcept(noexcept(detail::total_order_greater<T, U>{}(lhs, rhs))) {
            return detail::total_order_greater<T, U>{}(lhs, rhs);
        }
    };

}

#endif //COMPARE_H
