
#ifndef URLICHT_SCOPE_GUARD_H
#define URLICHT_SCOPE_GUARD_H

#include <functional>
#include <type_traits>
#include <utility>

namespace urlicht::detail {

    template <class F>
    class scope_guard {
    public:
        scope_guard() = delete;

        constexpr explicit scope_guard(F&& f) noexcept(std::is_nothrow_move_constructible_v<F>)
            : fn_(std::forward<F>(f)), active_(true) {}

        constexpr explicit scope_guard(const F& f) noexcept(std::is_nothrow_copy_constructible_v<F>)
            : fn_(f), active_(true) {}

        scope_guard(const scope_guard&) = delete;
        scope_guard& operator=(const scope_guard&) = delete;

        constexpr scope_guard(scope_guard&& other) noexcept(std::is_nothrow_move_constructible_v<F>)
            : fn_(std::move(other.fn_)), active_(other.active_) {
            other.active_ = false;
        }

        constexpr scope_guard& operator=(scope_guard&&) = delete;

        constexpr ~scope_guard() noexcept(std::is_nothrow_invocable_v<F>) {
            if (active_) [[likely]] {
                std::invoke(fn_);
            }
        }

        constexpr void release() noexcept {
            active_ = false;
        }

        [[nodiscard]] constexpr bool active() const noexcept { return active_; }

    private:
        F fn_;
        bool active_;
    };

    template <class F>
    [[nodiscard]] constexpr auto make_scope_guard(F&& f)
    noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>) {
        return scope_guard<std::remove_cvref_t<F>>(std::forward<F>(f));
    }

}

#endif //URLICHT_SCOPE_GUARD_H
