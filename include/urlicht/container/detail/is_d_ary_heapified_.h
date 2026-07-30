#ifndef URLICHT_IS_D_ARY_HEAPIFIED__H
#define URLICHT_IS_D_ARY_HEAPIFIED__H

#include <ranges>
#include <functional>

namespace urlicht::container::detail {

    template <std::size_t Arity, typename Rng, typename Cmp>
    [[nodiscard]] constexpr bool is_d_ary_heapified_(Rng&& rng, const Cmp& cmp) {
        auto child_it = std::ranges::begin(rng);
        const auto end_it = std::ranges::end(rng);

        if (child_it == end_it) [[unlikely]] {
            return true;
        }

        auto parent_it = child_it;
        ++child_it;

        std::size_t child_count = 0;
        while (child_it != end_it) {
            if (std::invoke(cmp, *parent_it, *child_it)) {
                return false;
            }
            ++child_it;
            ++child_count;
            if (child_count == Arity) {
                ++parent_it;
                child_count = 0;
            }
        }
        return true;
    }
}

#endif //URLICHT_IS_D_ARY_HEAPIFIED__H
