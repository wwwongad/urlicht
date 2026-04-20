#ifndef URLICHT_TAG_H
#define URLICHT_TAG_H

#include <type_traits>

namespace urlicht {

    namespace detail {
        template <typename T>
        struct inplace_t {};

        template <typename T>
        struct inplace_cond_t {};

        template <auto V>
        struct nontype_t {
            static constexpr const auto& value() noexcept {
                return V;
            }
        };

        template <typename T>
        struct is_urlicht_inplace_t : std::false_type {};

        template <typename T>
        struct is_urlicht_inplace_t<inplace_t<T>> : std::true_type {};

        template <typename T>
        struct is_urlicht_inplace_cond_t : std::false_type {};

        template <typename T>
        struct is_urlicht_inplace_cond_t<inplace_cond_t<T>> : std::true_type {};

        template <typename T>
        struct is_urlicht_nontype_t : std::false_type {};

        template <auto V>
        struct is_urlicht_nontype_t<nontype_t<V>> : std::true_type {};
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

}
#endif //URLICHT_TAG_H
