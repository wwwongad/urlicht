#ifndef URLICHT_TAG_H
#define URLICHT_TAG_H

#include <type_traits>
#include <utility>
#include <memory>

namespace urlicht::internal {
    template <typename T>
    using inplace_t = std::in_place_type_t<T>;

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

    using allocator_arg_t = std::allocator_arg_t;

    template <typename>
    struct is_urlicht_inplace_t : std::false_type {};
    template <typename T>
    struct is_urlicht_inplace_t<std::in_place_type_t<T>> : std::true_type {};

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

    template <typename>
    struct is_urlicht_allocator_arg_t_t : std::false_type {};
    template <>
    struct is_urlicht_allocator_arg_t_t<std::allocator_arg_t> : std::true_type {};

}

namespace urlicht {
    template <typename T>
    inline constexpr internal::inplace_t<T> inplace{};

    template <typename T>
    inline constexpr internal::inplace_cond_t<T> inplace_cond{};

    template <auto V>
    inline constexpr internal::nontype_t<V> nontype{};

    inline constexpr internal::sorted_t sorted{};
    inline constexpr internal::sorted_unique_t sorted_unique{};
    inline constexpr internal::heapified_t heapified{};
    inline constexpr internal::allocator_arg_t allocator_arg{};

    template <typename T>
    inline constexpr bool is_urlicht_inplace_v = internal::is_urlicht_inplace_t<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_inplace_cond_v = internal::is_urlicht_inplace_cond_t<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_nontype_v = internal::is_urlicht_nontype_t<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_sorted_v = internal::is_urlicht_sorted_t<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_sorted_unique_v =
        internal::is_urlicht_sorted_unique_t<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_heapified_v = internal::is_urlicht_heapified_t<T>::value;

    template <typename T>
    inline constexpr bool is_urlicht_allocator_arg_v =
        internal::is_urlicht_allocator_arg_t_t<T>::value;
}
#endif //URLICHT_TAG_H