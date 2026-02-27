#ifndef URLICHT_LOWER_BOUND_H
#define URLICHT_LOWER_BOUND_H
#include <urlicht/compare.h>
#include <bit>
#include <functional>
#include <urlicht/config.h>
#include <algorithm>

namespace urlicht {

    template<std::random_access_iterator Iter, std::sentinel_for<Iter> Sentinel,
             typename Proj = std::identity,
             typename T = std::indirect_result_t<Proj&, Iter>,
             std::indirect_strict_weak_order<const T*, std::projected<Iter, Proj>> Comp = compare::less<>>
    Iter lower_bound(Iter begin, Sentinel end, const T& value, Comp comp = Comp{}, Proj proj = Proj{}) {
        UL_ASSERT(std::ranges::is_sorted(begin, end, comp, proj), "The given range is not sorted.");

        auto length = std::ranges::distance(begin, end);
        if (length == 0U) [[unlikely]] {
            return end;
        }
        using diff_t = decltype(length);
        using udiff_t = std::make_unsigned_t<diff_t>;
        auto step = std::bit_floor(static_cast<udiff_t>(length));

        if (static_cast<diff_t>(step) != length && std::invoke(comp, std::invoke(proj, begin[step]), value)) {
            length -= step + 1;
            if (length == 0) [[unlikely]] {
                return end;
            }
            step = std::bit_ceil(static_cast<udiff_t>(length));
            begin = std::ranges::next(end, -static_cast<diff_t>(step));
        }
        for (step /= 2; step != 0; step /= 2) {
            std::ranges::advance(
                begin,
                step & -std::invoke(comp, std::invoke(proj, begin[step]), value)
            );
        }
        return std::ranges::next(begin, std::invoke(comp, std::invoke(proj, *begin), value));
    }

    template<std::ranges::random_access_range Rng,
             typename Proj = std::identity,
             typename T = std::indirect_result_t<Proj&, std::ranges::iterator_t<Rng>>,
             std::indirect_strict_weak_order <
                 const T*, std::projected<std::ranges::iterator_t<Rng>, Proj>
             > Comp = compare::less<>>
    auto lower_bound(Rng&& r, const T& value, Comp comp = Comp{}, Proj proj = Proj{}) {
        auto begin = std::ranges::begin(r);
        auto end   = std::ranges::end(r);
        return lower_bound(begin, end, value, comp, proj);
    }
}

#endif //URLICHT_LOWER_BOUND_H
