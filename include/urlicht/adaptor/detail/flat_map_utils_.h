#ifndef _URLICHT_FLAT_MAP_UTILS_H
#define _URLICHT_FLAT_MAP_UTILS_H

#include <concepts>
#include <algorithm>
#include <urlicht/config.h>

namespace urlicht::adaptor_detail {
    // Range-based binary search
    template <typename Fn, typename T, typename Cont, typename Comp>
    concept lower_bound_for =
        requires (const Fn& fn, const Cont& cont, const T& value, Comp comp) {
            { fn(cont, value, comp) } -> std::same_as<typename Cont::const_iterator>;
        };

    template <typename T, typename ...Args>
    static constexpr bool emplace_should_have_strong_exception_safety_for =
        std::is_nothrow_constructible_v<T, Args&&...> && std::is_nothrow_move_assignable_v<T>;

    // Checks if the given range is strictly increasing
    template <typename Iter, typename Sent, typename Cmp = std::ranges::less>
    constexpr bool is_sorted_and_unique(Iter begin, Sent end, Cmp comp = Cmp{}) {
        auto reversed_cmp = [&](const auto& x, const auto& y) {
            return !comp(x, y);
        };
        return std::ranges::adjacent_find(begin, end, reversed_cmp) == end;
    }

#if UL_HAS_CPP23
    template <typename R1, typename R2>
    using zip_view = std::ranges::zip_view<R1, R2>;
#else
    template <class R1, class R2>
class zip_view {
    R1* r1_;
    R2* r2_;

    using I1 = std::ranges::iterator_t<R1>;
    using I2 = std::ranges::iterator_t<R2>;
public:
    constexpr zip_view(R1& r1, R2& r2) noexcept : r1_(&r1), r2_(&r2) {}

    class reference {
    public:
        std::ranges::range_reference_t<R1> first;
        std::ranges::range_reference_t<R2> second;

        using value_type = std::pair<std::ranges::range_value_t<R1>,std::ranges::range_value_t<R2>>;

        constexpr reference(std::ranges::range_reference_t<R1> a,
                            std::ranges::range_reference_t<R2> b) noexcept
        : first(a), second(b) {}

        constexpr operator value_type() const noexcept {
            return {first, second};
        }

        constexpr reference& operator=(const reference& rhs) noexcept {
            first = rhs.first;
            second = rhs.second;
            return *this;
        }

        constexpr const reference& operator=(const reference& rhs) const noexcept {
            first = rhs.first;
            second = rhs.second;
            return *this;
        }

        template <class A, class B>
        constexpr reference& operator=(const std::pair<A, B>& rhs) noexcept {
            first = rhs.first;
            second = rhs.second;
            return *this;
        }

        template <class A, class B>
        constexpr const reference& operator=(const std::pair<A, B>& rhs) const noexcept {
            first = rhs.first;
            second = rhs.second;
            return *this;
        }

        friend constexpr bool operator==(const reference& a, const reference& b) noexcept {
            return a.first == b.first && a.second == b.second;
        }

        friend constexpr bool operator==(const reference& a, const value_type& b) noexcept {
            return a.first == b.first && a.second == b.second;
        }

        friend constexpr bool operator==(const value_type& a, const reference& b) noexcept {
            return a.first == b.first && a.second == b.second;
        }

        friend constexpr auto operator<=>(const reference& a, const reference& b) noexcept {
            return value_type(a) <=> value_type(b);
        }

        friend constexpr auto operator<=>(const reference& a, const value_type& b) noexcept {
            return value_type(a) <=> b;
        }

        friend constexpr auto operator<=>(const value_type& a, const reference& b) noexcept {
            return a <=> value_type(b);
        }
    };

    class iterator {
        I1 i1_{};
        I2 i2_{};

    public:
        using value_type = std::pair<std::ranges::range_value_t<R1>,std::ranges::range_value_t<R2>>;
        using reference = zip_view::reference;
        using difference_type = std::common_type_t<std::iter_difference_t<I1>,std::iter_difference_t<I2>>;
        using iterator_concept  = std::random_access_iterator_tag;
        using iterator_category = std::random_access_iterator_tag;

        constexpr iterator() noexcept = default;
        constexpr iterator(I1 a, I2 b) noexcept : i1_(a), i2_(b) {}

        constexpr reference operator*() const noexcept {
            return reference(*i1_, *i2_);
        }

        constexpr iterator& operator++() noexcept {
            ++i1_;
            ++i2_;
            return *this;
        }

        constexpr iterator operator++(int) noexcept {
            auto t = *this;
            ++*this;
            return t;
        }

        constexpr iterator& operator--() noexcept {
            --i1_;
            --i2_;
            return *this;
        }

        constexpr iterator operator--(int) noexcept {
            auto t = *this;
            --*this;
            return t;
        }

        constexpr iterator& operator+=(difference_type n) noexcept {
            i1_ += n;
            i2_ += n;
            return *this;
        }

        constexpr iterator& operator-=(difference_type n) noexcept {
            return *this += -n;
        }

        constexpr reference operator[](difference_type n) const noexcept {
            return *(*this + n);
        }

        friend constexpr iterator operator+(iterator it, difference_type n) noexcept {
            it += n;
            return it;
        }

        friend constexpr iterator operator+(difference_type n, iterator it) noexcept {
            it += n;
            return it;
        }

        friend constexpr iterator operator-(iterator it, difference_type n) noexcept {
            it -= n;
            return it;
        }

        friend constexpr difference_type operator-(const iterator& a, const iterator& b) noexcept {
            return static_cast<difference_type>(a.i1_ - b.i1_);
        }

        friend constexpr bool operator==(const iterator& a, const iterator& b) noexcept = default;

        friend constexpr auto operator<=>(const iterator& a, const iterator& b) noexcept {
            return a.i1_ <=> b.i1_;
        }

        friend void iter_swap(const iterator& a, const iterator& b)
        noexcept(noexcept(std::ranges::iter_swap(a.i1_, b.i1_)) &&
                 noexcept(std::ranges::iter_swap(a.i2_, b.i2_))) {
            std::ranges::iter_swap(a.i1_, b.i1_);
            std::ranges::iter_swap(a.i2_, b.i2_);
        }

        friend value_type iter_move(const iterator& it)
        noexcept(noexcept(std::ranges::iter_move(it.i1_)) &&
                 noexcept(std::ranges::iter_move(it.i2_))) {
            return value_type(
                std::ranges::iter_move(it.i1_),
                std::ranges::iter_move(it.i2_)
            );
        }
    };

    friend void swap(reference a, reference b)
    noexcept(noexcept(std::ranges::swap(a.first, b.first)) &&
             noexcept(std::ranges::swap(a.second, b.second))) {
        std::ranges::swap(a.first, b.first);
        std::ranges::swap(a.second, b.second);
    }

    constexpr iterator begin()
    noexcept(noexcept(std::ranges::begin(*r1_)) &&
             noexcept(std::ranges::begin(*r2_))) {
        return iterator(std::ranges::begin(*r1_), std::ranges::begin(*r2_));
    }

    constexpr iterator end() noexcept(noexcept(begin()) && noexcept(size())) {
        auto it = begin();
        it += static_cast<typename iterator::difference_type>(size());
        return it;
    }

    constexpr auto size() const noexcept(noexcept(std::ranges::size(*r1_)) &&
                                         noexcept(std::ranges::size(*r2_))) {
        using S = std::common_type_t<std::ranges::range_size_t<R1>, std::ranges::range_size_t<R2>>;
        S s1 = static_cast<S>(std::ranges::size(*r1_));
        S s2 = static_cast<S>(std::ranges::size(*r2_));
        return std::min(s1, s2);
    }
};
#endif

}
#endif //_URLICHT_FLAT_MAP_UTILS_H
