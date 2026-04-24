#ifndef URLICHT_TAG_H
#define URLICHT_TAG_H

#include <type_traits>

namespace urlicht {

    namespace detail {
        template <typename>
        struct inplace_t {};

        template <typename>
        struct inplace_cond_t {};

        template <auto V>
        struct nontype_t {
            static constexpr const auto& value() noexcept {
                return V;
            }
        };

        struct sorted_t {};

        struct sorted_unique_t {};

        struct heapified_t {};

        template <typename>
        struct is_urlicht_inplace_t : std::false_type {};

        template <typename T>
        struct is_urlicht_inplace_t<inplace_t<T>> : std::true_type {};

        template <typename>
        struct is_urlicht_inplace_cond_t : std::false_type {};

        template <typename T>
        struct is_urlicht_inplace_cond_t<inplace_cond_t<T>> : std::true_type {};

        template <typename>
        struct is_urlicht_nontype_t : std::false_type {};

        template <auto V>
        struct is_urlicht_nontype_t<nontype_t<V>> : std::true_type {};

        template <typename>
        struct is_urlicht_sorted_t : std::false_type {};

        template <>
        struct is_urlicht_sorted_t<sorted_t> : std::true_type {};

        template <typename>
        struct is_urlicht_sorted_unique_t : std::false_type {};

        template <>
        struct is_urlicht_sorted_unique_t<sorted_unique_t> : std::true_type {};

        template <typename>
        struct is_urlicht_heapified_t : std::false_type {};

        template <>
        struct is_urlicht_heapified_t<heapified_t> : std::true_type {};
    }

    template <typename T>
    inline constexpr detail::inplace_t<T> inplace{};

    template <typename T>
    inline constexpr bool is_urlicht_inplace_v = detail::is_urlicht_inplace_t<T>::value;

    template <typename T>
    inline constexpr detail::inplace_cond_t<T> inplace_cond{};

    template <typename T>
    inline constexpr bool is_urlicht_inplace_cond_v = detail::is_urlicht_inplace_cond_t<T>::value;

    template <auto V>
    inline constexpr detail::nontype_t<V> nontype{};

    template <typename T>
    inline constexpr bool is_urlicht_nontype_v = detail::is_urlicht_nontype_t<T>::value;

    inline constexpr detail::sorted_t sorted{};

    template <typename T>
    inline constexpr bool is_urlicht_sorted_v = detail::is_urlicht_sorted_t<T>::value;

    inline constexpr detail::sorted_unique_t sorted_unique{};

    template <typename T>
    inline constexpr bool is_urlicht_sorted_unique_v = detail::is_urlicht_sorted_unique_t<T>::value;

    inline constexpr detail::heapified_t heapified{};

    template <typename T>
    inline constexpr bool is_urlicht_heapified_v = detail::is_urlicht_heapified_t<T>::value;

}
#endif //URLICHT_TAG_H
