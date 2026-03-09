
#ifndef URLICHT_BINARY_SEARCH_H
#define URLICHT_BINARY_SEARCH_H
#include <urlicht/procedural/search/lower_bound.h>

namespace urlicht {
    class binary_search_fn {
    public:
        template<std::random_access_iterator Iter, std::sentinel_for<Iter> Sentinel,
            typename Proj = std::identity,
            typename T = std::indirect_result_t<Proj&, Iter>,
            std::indirect_strict_weak_order<const T*, std::projected<Iter, Proj>> Comp = compare::less<>>
        [[nodiscard]]
        constexpr bool
        operator() (Iter begin, Sentinel end, const T& value, Comp comp = Comp{}, Proj proj = Proj{}) const {
            UL_ASSERT(std::ranges::is_sorted(begin, end, comp, proj), "The given range is not sorted.");

            const auto pos = lower_bound(begin, end, value, comp, proj);
            return pos != end && !std::invoke(comp, value, std::invoke(proj, *pos));
        }

        template<std::ranges::random_access_range Rng,
            typename Proj = std::identity,
            typename T = std::indirect_result_t<Proj&, std::ranges::iterator_t<Rng>>,
            std::indirect_strict_weak_order <
                const T*, std::projected<std::ranges::iterator_t<Rng>, Proj>
            > Comp = compare::less<>>
        [[nodiscard]]
        constexpr auto operator()(Rng&& r, const T& value, Comp comp = Comp{}, Proj proj = Proj{}) const {
            auto begin = std::ranges::begin(r);
            auto end   = std::ranges::end(r);
            return (*this)(begin, end, value, comp, proj);
        }
    };

    inline constexpr auto binary_search = binary_search_fn{};
}


#endif //URLICHT_BINARY_SEARCH_H
