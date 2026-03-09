#ifndef URLICHT_UPPER_BOUND_H
#define URLICHT_UPPER_BOUND_H
#include <urlicht/procedural/search/lower_bound.h>

namespace urlicht {
    class upper_bound_fn {
    public:
        template<std::random_access_iterator Iter,
            std::sentinel_for<Iter> Sentinel,
            typename Proj = std::identity,
            typename T = std::indirect_result_t<Proj&, Iter>,
            std::indirect_strict_weak_order<const T*, std::projected<Iter, Proj>> Comp = compare::less<>>
        [[nodiscard]] UL_CONSTEXPR23 bool
        operator() (Iter begin, Sentinel end, const T& value, Comp comp = Comp{}, Proj proj = Proj{}) const {
            UL_ASSERT(std::ranges::is_sorted(begin, end, comp, proj), "The given range is not sorted.");
            auto cmp_lhs_rhs_swapped = [&comp](const auto& lhs, const auto& rhs) {
                return !std::invoke(comp, rhs, lhs);
            };

            return lower_bound(begin, end, value, cmp_lhs_rhs_swapped, proj);
        }

        template<std::ranges::random_access_range Rng,
            typename Proj = std::identity,
            typename T = std::indirect_result_t<Proj&, std::ranges::iterator_t<Rng>>,
            std::indirect_strict_weak_order <
                const T*, std::projected<std::ranges::iterator_t<Rng>, Proj>
            > Comp = compare::less<>>
        [[nodiscard]]
        UL_CONSTEXPR23 auto operator()(Rng&& r, const T& value, Comp comp = Comp{}, Proj proj = Proj{}) const {
            auto begin = std::ranges::begin(r);
            auto end   = std::ranges::end(r);
            return (*this)(begin, end, value, comp, proj);
        }
    };

    inline constexpr upper_bound_fn upper_bound{};
}
#endif //URLICHT_UPPER_BOUND_H
