#ifndef URLICHT_SCOPE_SCOPE_ACTION_H
#define URLICHT_SCOPE_SCOPE_ACTION_H

#include <urlicht/internal/config.h>
#include <concepts>
#include <exception>
#include <functional>
#include <type_traits>
#include <utility>

namespace urlicht::scope {

    /**
     * @brief A nothrow nullary callable returning void.
     */
    template <typename F>
    concept scope_function = std::destructible<F> && std::is_nothrow_invocable_v<F&>
                          && std::same_as<std::invoke_result_t<F&>, void>;

    /**
     * @brief A nothrow nullary predicate.
     */
    template <typename F>
    concept scope_predicate = std::destructible<F> && std::is_nothrow_invocable_v<F&>
                           && std::same_as<std::invoke_result_t<F&>, bool>;

    namespace detail {
        // Role-specific empty types let [[no_unique_address]] co-locate default members.
        struct entry_noop_fn {
            constexpr void operator()() const noexcept { }
        };

        struct entry_always_true {
            [[nodiscard]] constexpr bool operator()() const noexcept {
                return true;
            }
        };

        struct entry_always_false {
            [[nodiscard]] constexpr bool operator()() const noexcept {
                return false;
            }
        };

        struct exit_always_true {
            [[nodiscard]] constexpr bool operator()() const noexcept {
                return true;
            }
        };

        [[nodiscard]] constexpr int uncaught_exceptions_() noexcept {
            return std::is_constant_evaluated() ? 0 : std::uncaught_exceptions();
        }

        struct scope_success_cond {
            int init_{uncaught_exceptions_()};

            [[nodiscard]] constexpr bool operator()() const noexcept {
                return uncaught_exceptions_() <= init_;
            }
        };

        struct scope_fail_cond {
            int init_{uncaught_exceptions_()};

            [[nodiscard]] constexpr bool operator()() const noexcept {
                return uncaught_exceptions_() > init_;
            }
        };

        template <typename T>
        inline constexpr bool is_safe_default_v_ =
            std::default_initializable<T> && (!std::is_pointer_v<T>) &&
            std::is_nothrow_default_constructible_v<T>;

        template <typename Bool>
        concept active_flag = std::same_as<std::decay_t<Bool>, bool>;

        /**
         * @brief A scoped entry/exit action pair: the entry action runs when the object is constructed,
         *        the exit action runs when it is destroyed, and each is gated by its own condition.
         *        The active flag controls only the exit action, not the entry action.
         *
         * @tparam EntryFunc Nothrow nullary callable returning void, invoked on construction.
         * @tparam ExitFunc  Nothrow nullary callable returning void, invoked on destruction.
         * @tparam EntryCond Nothrow nullary predicate; the entry action runs only if it returns true.
         * @tparam ExitCond  Nothrow nullary predicate; the exit action runs only if it returns true.
         * @note Internal type; construct guards through the make_scope_* factories.
         *
         */
        template <
            typename EntryFunc,
            typename ExitFunc,
            typename EntryCond = entry_always_true,
            typename ExitCond = exit_always_true
        >
        class scope_action {
            static_assert(scope_function<EntryFunc>,
                "EntryFunc must satisfy scope_function: a nothrow nullary callable returning void");
            static_assert(scope_function<ExitFunc>,
                "ExitFunc must satisfy scope_function: a nothrow nullary callable returning void");
            static_assert(scope_predicate<EntryCond>,
                "EntryCond must satisfy scope_predicate: a nothrow nullary predicate");
            static_assert(scope_predicate<ExitCond>,
                "ExitCond must satisfy scope_predicate: a nothrow nullary predicate");

        public:
            using entry_function_type = EntryFunc;
            using exit_function_type = ExitFunc;
            using entry_condition_type = EntryCond;
            using exit_condition_type = ExitCond;

            constexpr scope_action() noexcept
            requires is_safe_default_v_<EntryFunc> && is_safe_default_v_<ExitFunc>
                  && is_safe_default_v_<EntryCond> && is_safe_default_v_<ExitCond>
            : entry_func_{}, exit_func_{}, entry_cond_{}, exit_cond_{} {
                run_entry_();
            }

            // Direct initialization matches the nothrow traits without selecting initializer-list overloads.
            template <typename EntryFunc_ = EntryFunc, typename Bool = bool>
            requires std::is_nothrow_constructible_v<EntryFunc, EntryFunc_&&>
                  && active_flag<Bool>
                  && is_safe_default_v_<ExitFunc>
                  && is_safe_default_v_<EntryCond> && is_safe_default_v_<ExitCond>
            constexpr explicit scope_action(EntryFunc_&& entry_func,
                                            const Bool active = true) noexcept
            : entry_func_(std::forward<EntryFunc_>(entry_func)),
              exit_func_{}, entry_cond_{}, exit_cond_{}, active_{active} {
                run_entry_();
            }

            template <typename EntryFunc_ = EntryFunc, typename ExitFunc_ = ExitFunc,
                      typename Bool = bool>
            requires std::is_nothrow_constructible_v<EntryFunc, EntryFunc_&&>
                  && std::is_nothrow_constructible_v<ExitFunc, ExitFunc_&&>
                  && active_flag<Bool>
                  && is_safe_default_v_<EntryCond> && is_safe_default_v_<ExitCond>
            constexpr scope_action(EntryFunc_&& entry_func,
                                   ExitFunc_&& exit_func,
                                   const Bool active = true) noexcept
            : entry_func_(std::forward<EntryFunc_>(entry_func)),
              exit_func_(std::forward<ExitFunc_>(exit_func)),
              entry_cond_{}, exit_cond_{}, active_{active} {
                run_entry_();
            }

            template <typename EntryFunc_ = EntryFunc, typename ExitFunc_ = ExitFunc,
                      typename EntryCond_ = EntryCond, typename Bool = bool>
            requires std::is_nothrow_constructible_v<EntryFunc, EntryFunc_&&>
                  && std::is_nothrow_constructible_v<ExitFunc, ExitFunc_&&>
                  && std::is_nothrow_constructible_v<EntryCond, EntryCond_&&>
                  && active_flag<Bool>
                  && is_safe_default_v_<ExitCond>
            constexpr scope_action(EntryFunc_&& entry_func,
                                   ExitFunc_&& exit_func,
                                   EntryCond_&& entry_cond,
                                   const Bool active = true) noexcept
            : entry_func_(std::forward<EntryFunc_>(entry_func)),
              exit_func_(std::forward<ExitFunc_>(exit_func)),
              entry_cond_(std::forward<EntryCond_>(entry_cond)),
              exit_cond_{}, active_{active} {
                run_entry_();
            }

            template <typename EntryFunc_ = EntryFunc, typename ExitFunc_ = ExitFunc,
                      typename EntryCond_ = EntryCond, typename ExitCond_ = ExitCond,
                      typename Bool = bool>
            requires std::is_nothrow_constructible_v<EntryFunc, EntryFunc_&&>
                  && std::is_nothrow_constructible_v<ExitFunc, ExitFunc_&&>
                  && std::is_nothrow_constructible_v<EntryCond, EntryCond_&&>
                  && std::is_nothrow_constructible_v<ExitCond, ExitCond_&&>
                  && active_flag<Bool>
            constexpr scope_action(EntryFunc_&& entry_func,
                                   ExitFunc_&& exit_func,
                                   EntryCond_&& entry_cond,
                                   ExitCond_&& exit_cond,
                                   const Bool active = true) noexcept
            : entry_func_(std::forward<EntryFunc_>(entry_func)),
              exit_func_(std::forward<ExitFunc_>(exit_func)),
              entry_cond_(std::forward<EntryCond_>(entry_cond)),
              exit_cond_(std::forward<ExitCond_>(exit_cond)),
              active_{active} {
                run_entry_();
            }

            scope_action(const scope_action&) = delete;
            scope_action& operator=(const scope_action&) = delete;
            scope_action& operator=(scope_action&&) = delete;

            /**
             * @brief Transfers the exit action's active state without running the entry action again.
             */
            constexpr scope_action(scope_action&& other) noexcept
            requires std::is_nothrow_move_constructible_v<EntryFunc>
                  && std::is_nothrow_move_constructible_v<ExitFunc>
                  && std::is_nothrow_move_constructible_v<EntryCond>
                  && std::is_nothrow_move_constructible_v<ExitCond>
            : entry_func_(std::forward<EntryFunc>(other.entry_func_)),
              exit_func_(std::forward<ExitFunc>(other.exit_func_)),
              entry_cond_(std::forward<EntryCond>(other.entry_cond_)),
              exit_cond_(std::forward<ExitCond>(other.exit_cond_)),
              active_{std::exchange(other.active_, false)} { }

            /**
             * @brief Runs the exit action if {active} is true and the exit condition holds.
             */
            constexpr ~scope_action() noexcept {
                run_exit_();
            }

            constexpr void set_active(const bool active) noexcept {
                active_ = active;
            }

            [[nodiscard]] constexpr bool active() const noexcept {
                return active_;
            }

        private:
            constexpr void run_entry_() noexcept {
                if (std::invoke(entry_cond_)) [[likely]] {
                    std::invoke(entry_func_);
                }
            }

            constexpr void run_exit_() noexcept {
                if (active_ && std::invoke(exit_cond_)) [[likely]] {
                    std::invoke(exit_func_);
                }
            }

            UL_NO_UNIQUE_ADDRESS EntryFunc entry_func_;
            UL_NO_UNIQUE_ADDRESS ExitFunc exit_func_;
            UL_NO_UNIQUE_ADDRESS EntryCond entry_cond_;
            UL_NO_UNIQUE_ADDRESS ExitCond exit_cond_;
            bool active_{true};
        };

        // Type aliases
        template <typename ExitFunc, typename ExitCond = exit_always_true>
        using scope_exit = scope_action<entry_noop_fn, ExitFunc, entry_always_false, ExitCond>;

        template <typename ExitFunc>
        using scope_success = scope_action<entry_noop_fn, ExitFunc, entry_always_false, scope_success_cond>;

        template <typename ExitFunc>
        using scope_fail = scope_action<entry_noop_fn, ExitFunc, entry_always_false, scope_fail_cond>;

        // CTAD guides: store decayed callable types. The optional trailing flag is constrained to
        // exactly bool (active_flag), so a bool-convertible non-predicate argument is rejected by
        // deduction rather than being silently interpreted as the flag.
        template <scope_function EntryFunc, scope_function ExitFunc, active_flag Bool = bool>
        scope_action(EntryFunc, ExitFunc, Bool = true) -> scope_action<EntryFunc, ExitFunc>;

        template <scope_function EntryFunc, scope_function ExitFunc, scope_predicate EntryCond,
                  active_flag Bool = bool>
        scope_action(EntryFunc, ExitFunc, EntryCond, Bool = true)
            -> scope_action<EntryFunc, ExitFunc, EntryCond>;

        template <scope_function EntryFunc, scope_function ExitFunc,
                  scope_predicate EntryCond, scope_predicate ExitCond, active_flag Bool = bool>
        scope_action(EntryFunc, ExitFunc, EntryCond, ExitCond, Bool = true)
            -> scope_action<EntryFunc, ExitFunc, EntryCond, ExitCond>;

        template <typename>
        struct is_scope_action : std::false_type {};
        template <typename EnF, typename ExF, typename EnC, typename ExC>
        struct is_scope_action<scope_action<EnF, ExF, EnC, ExC>> : std::true_type {};
    }

    // Factory methods

    /**
     * @brief Creates a guard from an entry/exit action pair, with optional conditions and active flag.
     * @param args Arguments in exact order:
     *        1. entry_func: Nothrow nullary callable object invoked when the guard is constructed.
     *        2. exit_func: Nothrow nullary callable object invoked when the guard goes out of scope.
     *        3. entry_cond (optional): Nothrow nullary predicate, controls whether entry_func is executed.
     *                                  Defaults to detail::entry_always_true{}.
     *        4. exit_cond (optional): Nothrow nullary predicate, controls whether exit_func is executed.
     *                                 Defaults to detail::exit_always_true{}.
     *        5. active (optional): Boolean flag to enable/disable exit_func. Defaults to true.
     */
    template <typename... Args>
    requires requires (Args&&... args) { detail::scope_action(std::forward<Args>(args)...); }
    [[nodiscard]] constexpr auto make_scope_action(Args&&... args) noexcept {
        return detail::scope_action(std::forward<Args>(args)...);
    }

    /**
     * @brief Creates a guard that runs @p{exit_func} when out of scope, with optional conditions and active flag.
     */
    template <typename ExitFunc, typename ExitCond = detail::exit_always_true, typename Bool = bool>
    requires scope_function<std::decay_t<ExitFunc>> && scope_predicate<std::decay_t<ExitCond>>
          && detail::active_flag<Bool>
          && std::is_nothrow_constructible_v<std::decay_t<ExitFunc>, ExitFunc&&>
          && std::is_nothrow_constructible_v<std::decay_t<ExitCond>, ExitCond&&>
    [[nodiscard]] constexpr auto make_scope_exit(ExitFunc&& exit_func,
                                                 ExitCond&& exit_cond = {},
                                                 const Bool active = true) noexcept {
        return detail::scope_exit<std::decay_t<ExitFunc>, std::decay_t<ExitCond>>(
            detail::entry_noop_fn{},
            std::forward<ExitFunc>(exit_func),
            detail::entry_always_false{},
            std::forward<ExitCond>(exit_cond),
            active);
    }

    /**
     * @brief Creates a guard that runs @p{exit_func} when out of scope, with an active flag.
     */
    template <typename ExitFunc, typename Bool>
    requires scope_function<std::decay_t<ExitFunc>> && detail::active_flag<Bool>
          && std::is_nothrow_constructible_v<std::decay_t<ExitFunc>, ExitFunc&&>
    [[nodiscard]] constexpr auto make_scope_exit(ExitFunc&& exit_func, const Bool active) noexcept {
        return make_scope_exit(std::forward<ExitFunc>(exit_func), detail::exit_always_true{}, active);
    }

    /**
     * @brief Creates a guard that runs @p{exit_func} if there is no uncaught exception when out of scope.
     */
    template <typename ExitFunc, typename Bool = bool>
    requires scope_function<std::decay_t<ExitFunc>> && detail::active_flag<Bool>
          && std::is_nothrow_constructible_v<std::decay_t<ExitFunc>, ExitFunc&&>
    [[nodiscard]] constexpr auto make_scope_success(ExitFunc&& exit_func,
                                                    const Bool active = true) noexcept {
        return detail::scope_success<std::decay_t<ExitFunc>>(
            detail::entry_noop_fn{}, std::forward<ExitFunc>(exit_func),
            detail::entry_always_false{}, detail::scope_success_cond{}, active);
    }

    /**
     * @brief Creates a guard that runs @p{exit_func} if there are uncaught exception(s) when out of scope.
     */
    template <typename ExitFunc, typename Bool = bool>
    requires scope_function<std::decay_t<ExitFunc>> && detail::active_flag<Bool>
          && std::is_nothrow_constructible_v<std::decay_t<ExitFunc>, ExitFunc&&>
    [[nodiscard]] constexpr auto make_scope_fail(ExitFunc&& exit_func,
                                                 const Bool active = true) noexcept {
        return detail::scope_fail<std::decay_t<ExitFunc>>(
            detail::entry_noop_fn{}, std::forward<ExitFunc>(exit_func),
            detail::entry_always_false{}, detail::scope_fail_cond{}, active);
    }
}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_scope_action_v = urlicht::scope::detail::is_scope_action<T>::value;
}

#endif //URLICHT_SCOPE_SCOPE_ACTION_H
