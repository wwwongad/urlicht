#ifndef URLICHT_ANY_UTILS_H
#define URLICHT_ANY_UTILS_H

#include <urlicht/concepts_utility.h>

namespace urlicht {

    namespace any_detail {
        template <typename T, std::size_t Size, std::size_t Align>
        inline constexpr bool is_small_obj_v =
                sizeof(T) <= Size && alignof(T) <= Align && std::is_nothrow_move_constructible_v<T>;

        struct vtable {
            void (*destroy) (void* src) noexcept = nullptr;
            void (*move) (void* src, void* dest) noexcept = nullptr;
            void (*clone) (const void* src, void* dest) = nullptr;
            const std::type_info* (*type_info) () noexcept = nullptr;
            bool (*in_sbo) () noexcept = nullptr;
        };

        template <typename T>
        struct inplace_t {};

        template <typename T>
        struct inplace_cond_t {};

    }

    template <typename T>
    inline constexpr any_detail::inplace_t<T> inplace{};

    template <typename T>
    inline constexpr any_detail::inplace_cond_t<T> inplace_cond{};

}

#endif //URLICHT_ANY_UTILS_H
