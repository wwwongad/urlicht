#include <gtest/gtest.h>
#include <urlicht/scope/scope_action.h>
#include <concepts>
#include <initializer_list>
#include <memory>
#include <type_traits>
#include <utility>

using namespace urlicht::scope;

namespace {
    template <typename R,
              bool DefaultNoexcept = true,
              bool CopyNoexcept = true,
              bool MoveNoexcept = true>
    struct callable {
        constexpr callable() noexcept(DefaultNoexcept) { }
        constexpr callable(const callable&) noexcept(CopyNoexcept) { }
        constexpr callable(callable&&) noexcept(MoveNoexcept) { }
        constexpr R operator()() & noexcept { return R(); }
    };

    template <typename R>
    struct rvalue_callable {
        R operator()() && noexcept { return R(); }
    };

    template <typename R>
    struct throwing_lvalue_callable {
        R operator()() & { return R(); }
        R operator()() && noexcept { return R(); }
    };

    template <typename R>
    struct throwing_rvalue_callable {
        R operator()() & noexcept { return R(); }
        R operator()() && { return R(); }
    };

    struct wrong_arg_function {
        void operator()(int) noexcept;
    };

    struct wrong_arg_predicate {
        bool operator()(int) noexcept;
    };

    struct bool_like {
        operator bool() const noexcept;
    };

    template <typename R>
    struct immovable_callable {
        immovable_callable() noexcept = default;
        immovable_callable(const immovable_callable&) = delete;
        immovable_callable(immovable_callable&&) = delete;
        R operator()() noexcept { return R(); }
    };

    struct non_default_function {
        non_default_function() = delete;
        explicit non_default_function(int) noexcept { }
        void operator()() const noexcept { }
    };

    struct move_only_function {
        move_only_function() noexcept = default;
        move_only_function(const move_only_function&) = delete;
        move_only_function(move_only_function&&) noexcept = default;
        void operator()() const noexcept { }
    };

    struct empty_entry_function {
        void operator()() const noexcept { }
    };

    struct empty_exit_function {
        void operator()() const noexcept { }
    };

    template <typename... Args>
    concept can_make_action = requires (Args&&... args) {
        make_scope_action(std::forward<Args>(args)...);
    };

    template <typename... Args>
    concept can_make_exit = requires (Args&&... args) {
        make_scope_exit(std::forward<Args>(args)...);
    };

    template <typename... Args>
    concept can_make_success = requires (Args&&... args) {
        make_scope_success(std::forward<Args>(args)...);
    };

    template <typename... Args>
    concept can_make_fail = requires (Args&&... args) {
        make_scope_fail(std::forward<Args>(args)...);
    };

    using function = callable<void>;
    using predicate = callable<bool>;
    using throwing_default_function = callable<void, false>;
    using throwing_default_predicate = callable<bool, false>;
    using throwing_copy_function = callable<void, true, false>;
    using throwing_copy_predicate = callable<bool, true, false>;
    using throwing_move_function = callable<void, true, true, false>;
    using throwing_move_predicate = callable<bool, true, true, false>;
    using function_ptr = void (*)() noexcept;
    using predicate_ptr = bool (*)() noexcept;
    using action = decltype(make_scope_action(function{}, function{}));
    using conditional_action = decltype(make_scope_action(function{}, function{}, predicate{}, predicate{}));
    using exit_action = decltype(make_scope_exit(function{}));
    using success_action = decltype(make_scope_success(function{}));
    using fail_action = decltype(make_scope_fail(function{}));
    using empty_action = decltype(make_scope_action(empty_entry_function{}, empty_exit_function{}));
    using empty_exit_action = decltype(make_scope_exit(empty_exit_function{}));

    /************** Function/predicate eligibility ***************/
    static_assert(scope_function<function>);
    static_assert(scope_function<function&>);
    static_assert(!scope_function<const function>);
    static_assert(scope_function<function_ptr>);
    static_assert(!scope_function<void (*)()>);
    static_assert(!scope_function<predicate>);
    static_assert(!scope_function<callable<int>>);
    static_assert(!scope_function<wrong_arg_function>);
    static_assert(!scope_function<rvalue_callable<void>>);
    static_assert(!scope_function<throwing_lvalue_callable<void>>);
    static_assert(scope_function<throwing_rvalue_callable<void>>);
    static_assert(!scope_function<int>);
    static_assert(!scope_function<void>);

    static_assert(scope_predicate<predicate>);
    static_assert(scope_predicate<predicate&>);
    static_assert(!scope_predicate<const predicate>);
    static_assert(scope_predicate<predicate_ptr>);
    static_assert(!scope_predicate<bool (*)()>);
    static_assert(!scope_predicate<function>);
    static_assert(!scope_predicate<callable<int>>);
    static_assert(!scope_predicate<callable<bool_like>>);
    static_assert(!scope_predicate<callable<bool&>>);
    static_assert(!scope_predicate<wrong_arg_predicate>);
    static_assert(!scope_predicate<rvalue_callable<bool>>);
    static_assert(!scope_predicate<throwing_lvalue_callable<bool>>);
    static_assert(scope_predicate<throwing_rvalue_callable<bool>>);
    static_assert(!scope_predicate<int>);
    static_assert(!scope_predicate<void>);

    /************** Internal constructibility and noexcept ***************/
    static_assert(std::is_nothrow_default_constructible_v<action>);
    static_assert(std::is_nothrow_constructible_v<action, function>);
    static_assert(std::is_nothrow_constructible_v<action, function, bool>);
    static_assert(std::is_nothrow_constructible_v<action, function, function>);
    static_assert(std::is_nothrow_constructible_v<action, function, function, bool>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<function, function, predicate>,
                                                 function, function, predicate>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<function, function, predicate>,
                                                 function, function, predicate, bool>);
    static_assert(std::is_nothrow_constructible_v<conditional_action,
                                                 function, function, predicate, predicate>);
    static_assert(std::is_nothrow_constructible_v<conditional_action,
                                                 function, function, predicate, predicate, bool>);
    static_assert(std::is_nothrow_constructible_v<conditional_action,
                                                 const function&, function&, const predicate&, predicate&>);
    static_assert(!std::constructible_from<conditional_action, function, function, wrong_arg_predicate>);
    static_assert(!std::constructible_from<conditional_action, function, function, predicate, wrong_arg_predicate>);
    static_assert(std::is_nothrow_move_constructible_v<conditional_action>);
    static_assert(std::is_nothrow_destructible_v<conditional_action>);
    static_assert(!std::is_copy_constructible_v<action>);
    static_assert(!std::is_copy_assignable_v<action>);
    static_assert(!std::is_move_assignable_v<action>);
    static_assert(noexcept(std::declval<action&>().active()));
    static_assert(noexcept(std::declval<action&>().set_active(false)));

    static_assert(!std::default_initializable<detail::scope_action<function_ptr, function>>);
    static_assert(!std::constructible_from<detail::scope_action<function, function_ptr>, function>);
    static_assert(!std::constructible_from<detail::scope_action<function, function, predicate_ptr>,
                                           function, function>);
    static_assert(!std::constructible_from<detail::scope_action<function, function, predicate, predicate_ptr>,
                                           function, function, predicate>);
    static_assert(std::is_nothrow_constructible_v<
        detail::scope_action<function_ptr, function_ptr, predicate_ptr, predicate_ptr>,
        function_ptr, function_ptr, predicate_ptr, predicate_ptr>);

    static_assert(!std::default_initializable<detail::scope_action<non_default_function, function>>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<non_default_function, function>,
                                                 non_default_function>);
    static_assert(!std::default_initializable<detail::scope_action<throwing_default_function, function>>);
    static_assert(!std::constructible_from<detail::scope_action<function, throwing_default_function>, function>);
    static_assert(!std::constructible_from<detail::scope_action<function, function, throwing_default_predicate>,
                                           function, function>);
    static_assert(!std::constructible_from<detail::scope_action<function, function, predicate, throwing_default_predicate>,
                                           function, function, predicate>);
    static_assert(std::is_nothrow_default_constructible_v<
        detail::scope_action<immovable_callable<void>, immovable_callable<void>,
                             immovable_callable<bool>, immovable_callable<bool>>>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<function, immovable_callable<void>>, function>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<function, function, immovable_callable<bool>>,
                                                 function, function>);
    static_assert(std::is_nothrow_constructible_v<
        detail::scope_action<function, function, predicate, immovable_callable<bool>>, function, function, predicate>);

    static_assert(!std::constructible_from<detail::scope_action<throwing_copy_function, function>, throwing_copy_function&>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<throwing_copy_function, function>,
                                                 throwing_copy_function>);
    static_assert(!std::constructible_from<detail::scope_action<function, throwing_copy_function>,
                                           function, throwing_copy_function&>);
    static_assert(!std::constructible_from<detail::scope_action<function, function, throwing_copy_predicate>,
                                           function, function, throwing_copy_predicate&>);
    static_assert(!std::constructible_from<detail::scope_action<function, function, predicate, throwing_copy_predicate>,
                                           function, function, predicate, throwing_copy_predicate&>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<throwing_move_function, function>,
                                                 const throwing_move_function&>);
    static_assert(!std::constructible_from<detail::scope_action<throwing_move_function, function>, throwing_move_function>);
    static_assert(!std::is_move_constructible_v<detail::scope_action<throwing_move_function, function>>);
    static_assert(!std::is_move_constructible_v<detail::scope_action<function, throwing_move_function>>);
    static_assert(!std::is_move_constructible_v<detail::scope_action<function, function, throwing_move_predicate>>);
    static_assert(!std::is_move_constructible_v<detail::scope_action<function, function, predicate, throwing_move_predicate>>);
    static_assert(!std::is_move_constructible_v<detail::scope_action<immovable_callable<void>, function>>);
    static_assert(std::is_nothrow_constructible_v<detail::scope_action<move_only_function, function>, move_only_function>);
    static_assert(!std::constructible_from<detail::scope_action<move_only_function, function>, move_only_function&>);
    static_assert(std::is_nothrow_move_constructible_v<detail::scope_action<move_only_function, function>>);
    static_assert(std::is_nothrow_move_constructible_v<detail::scope_action<function&, function&, predicate&, predicate&>>);

    /************** Factory deduction and eligibility ***************/
    static_assert(!std::same_as<detail::entry_always_true, detail::exit_always_true>);
    static_assert(sizeof(empty_action) == sizeof(bool));
    static_assert(sizeof(empty_exit_action) == sizeof(bool));
    static_assert(std::same_as<action, detail::scope_action<function, function>>);
    static_assert(std::same_as<conditional_action, detail::scope_action<function, function, predicate, predicate>>);
    static_assert(std::same_as<decltype(make_scope_action(function{}, function{}, false)), action>);
    static_assert(std::same_as<decltype(make_scope_action(function{}, function{}, predicate{})),
                               detail::scope_action<function, function, predicate>>);
    static_assert(std::same_as<decltype(make_scope_action(function{}, function{}, predicate{}, false)),
                               detail::scope_action<function, function, predicate>>);
    static_assert(std::same_as<decltype(make_scope_action(function{}, function{}, predicate{}, predicate{}, false)),
                               conditional_action>);
    static_assert(std::same_as<decltype(make_scope_action(std::declval<const function&>(), std::declval<function&>())),
                               action>);
    static_assert(std::same_as<decltype(make_scope_exit(std::declval<void (&)() noexcept>())),
                               detail::scope_exit<function_ptr>>);
    static_assert(std::same_as<exit_action, detail::scope_exit<function>>);
    static_assert(std::same_as<decltype(make_scope_exit(function{}, predicate{})), detail::scope_exit<function, predicate>>);
    static_assert(std::same_as<success_action, detail::scope_success<function>>);
    static_assert(std::same_as<fail_action, detail::scope_fail<function>>);
    static_assert(std::is_nothrow_move_constructible_v<exit_action>);
    static_assert(std::is_nothrow_move_constructible_v<success_action>);
    static_assert(std::is_nothrow_move_constructible_v<fail_action>);
    static_assert(std::is_nothrow_destructible_v<exit_action>);
    static_assert(std::is_nothrow_destructible_v<success_action>);
    static_assert(std::is_nothrow_destructible_v<fail_action>);
    static_assert(noexcept(make_scope_action(function{}, function{})));
    static_assert(noexcept(make_scope_action(function{}, function{}, false)));
    static_assert(noexcept(make_scope_action(function{}, function{}, predicate{})));
    static_assert(noexcept(make_scope_action(function{}, function{}, predicate{}, false)));
    static_assert(noexcept(make_scope_action(function{}, function{}, predicate{}, predicate{})));
    static_assert(noexcept(make_scope_action(function{}, function{}, predicate{}, predicate{}, false)));
    static_assert(noexcept(make_scope_exit(function{})));
    static_assert(noexcept(make_scope_exit(function{}, false)));
    static_assert(noexcept(make_scope_exit(function{}, predicate{}, false)));
    static_assert(noexcept(make_scope_success(function{})));
    static_assert(noexcept(make_scope_fail(function{})));
    static_assert(!can_make_action<>);
    static_assert(!can_make_action<function>);
    static_assert(!can_make_action<function, bool>);
    static_assert(!can_make_action<int>);
    static_assert(!can_make_action<throwing_lvalue_callable<void>, function>);
    static_assert(!can_make_action<function, function, throwing_lvalue_callable<bool>>);
    static_assert(!can_make_action<function, function, predicate, throwing_lvalue_callable<bool>>);
    static_assert(!can_make_action<throwing_copy_function&, function>);
    static_assert(!can_make_action<function, throwing_move_function>);
    static_assert(!can_make_exit<throwing_lvalue_callable<void>>);
    static_assert(!can_make_exit<function, throwing_lvalue_callable<bool>>);
    static_assert(!can_make_exit<function, throwing_copy_predicate&>);
    static_assert(!can_make_exit<function, throwing_move_predicate>);
    static_assert(!can_make_success<throwing_lvalue_callable<void>>);
    static_assert(!can_make_fail<throwing_lvalue_callable<void>>);
    static_assert(!can_make_exit<throwing_copy_function&, bool>);
    static_assert(!can_make_success<throwing_copy_function&>);
    static_assert(!can_make_fail<throwing_move_function>);
    static_assert(can_make_action<move_only_function, move_only_function>);
    static_assert(can_make_exit<move_only_function>);
    static_assert(can_make_success<move_only_function>);
    static_assert(can_make_fail<move_only_function>);
    static_assert(urlicht::is_urlicht_scope_action_v<action>);
    static_assert(urlicht::is_urlicht_scope_action_v<conditional_action>);
    static_assert(urlicht::is_urlicht_scope_action_v<exit_action>);
    static_assert(urlicht::is_urlicht_scope_action_v<success_action>);
    static_assert(urlicht::is_urlicht_scope_action_v<fail_action>);
    static_assert(!urlicht::is_urlicht_scope_action_v<function>);

    using plain_lambda = decltype([] { return true; });      // forgot noexcept

    // A callable that satisfies scope_predicate AND converts to bool: the predicate wins.
    struct predicate_conv_bool {
        bool operator()() const noexcept { return true; }
        operator bool() const noexcept { return false; }
    };
    static_assert(scope_predicate<predicate_conv_bool>);
    static_assert(can_make_exit<function, predicate_conv_bool>);
    static_assert(std::same_as<decltype(make_scope_exit(function{}, predicate_conv_bool{})),
                               detail::scope_exit<function, predicate_conv_bool>>);

    static_assert(!can_make_exit<function, plain_lambda>);
    static_assert(!can_make_action<function, function, plain_lambda>);
    static_assert(!can_make_action<function, function, predicate, plain_lambda>);

    static_assert(!can_make_exit<function, int>);
    static_assert(!can_make_success<function, int>);
    static_assert(!can_make_fail<function, int>);
    static_assert(!can_make_action<function, function, int>);

    static_assert(can_make_exit<function, bool>);
    static_assert(can_make_success<function, bool>);
    static_assert(can_make_fail<function, bool>);
    static_assert(can_make_action<function, function, bool>);
    static_assert(can_make_action<function, function, predicate, bool>);
    static_assert(can_make_action<function, function, predicate, predicate, bool>);

    static_assert(std::constructible_from<detail::scope_action<function, function>, function, function, bool>);
    static_assert(!std::constructible_from<detail::scope_action<function, function>, function, function, int>);
    static_assert(!std::constructible_from<detail::scope_action<function, function>, function, function, plain_lambda>);

    constexpr int constant_evaluation() {
        int calls = 0;
        {
            auto guard = make_scope_action(
                [&]() noexcept { ++calls; },
                [&]() noexcept { calls += 10; });
            auto moved = std::move(guard);
            if (guard.active() || !moved.active()) {
                return -1;
            }
            moved.set_active(false);
            moved.set_active(true);
        }
        return calls;
    }

    static_assert(constant_evaluation() == 11);

    constexpr int constant_exit_conditions() {
        int calls = 0;
        {
            auto success = make_scope_success([&]() noexcept { ++calls; });
            auto fail = make_scope_fail([&]() noexcept { calls += 100; });
            auto exit = make_scope_exit([&]() noexcept { calls += 10; });
            auto moved_success = std::move(success);
            auto moved_fail = std::move(fail);
        }
        return calls;
    }

    static_assert(constant_exit_conditions() == 11);

    struct tracking_state {
        int copies = 0;
        int moves = 0;
        int calls = 0;
    };

    template <typename R>
    struct tracked_callable {
        tracking_state* state;

        explicit tracked_callable(tracking_state& s) noexcept : state{&s} { }
        tracked_callable(const tracked_callable& other) noexcept : state{other.state} { ++state->copies; }
        tracked_callable(tracked_callable&& other) noexcept : state{std::exchange(other.state, nullptr)} {
            ++state->moves;
        }

        R operator()() & noexcept {
            ++state->calls;
            if constexpr (std::same_as<R, bool>) {
                return true;
            } else {
                return R();
            }
        }
    };

    template <typename R>
    struct list_initializable_callable {
        inline static int list_constructions = 0;

        list_initializable_callable() noexcept = default;
        list_initializable_callable(const list_initializable_callable&) noexcept = default;
        list_initializable_callable(list_initializable_callable&&) noexcept = default;
        list_initializable_callable(std::initializer_list<int>) noexcept(false) { ++list_constructions; }
        operator int() const noexcept { return 0; }
        R operator()() noexcept {
            if constexpr (std::same_as<R, bool>) {
                return true;
            } else {
                return R();
            }
        }
    };

    struct default_function {
        inline static int observed = -1;
        int value;
        void operator()() const noexcept { observed = value; }
    };

    struct default_predicate {
        bool value;
        bool operator()() const noexcept { return value; }
    };

    int pointer_calls = 0;
    void pointer_function() noexcept { ++pointer_calls; }
    bool pointer_predicate() noexcept { return true; }
}

TEST(ScopeAction, EntryExitOrdering) {
    int order = 0;
    {
        auto outer = make_scope_action([&]() noexcept { order = order * 10 + 1; },
                                       [&]() noexcept { order = order * 10 + 5; });
        EXPECT_EQ(order, 1);
        {
            auto inner = make_scope_action([&]() noexcept { order = order * 10 + 2; },
                                           [&]() noexcept { order = order * 10 + 4; });
            auto exit = make_scope_exit([&]() noexcept { order = order * 10 + 3; });
            EXPECT_EQ(order, 12);
        }
        EXPECT_EQ(order, 1234);
    }
    EXPECT_EQ(order, 12345);
}

TEST(ScopeAction, ConditionsIndependency) {
    for (const bool do_enter : {false, true}) {
        for (const bool do_exit : {false, true}) {
            int exits = 0;
            int exit_checks = 0;
            {
                int entries = 0;
                int entry_checks = 0;
                bool exit_enabled = !do_exit;
                auto guard = make_scope_action([&]() noexcept { ++entries; },
                                               [&]() noexcept { ++exits; },
                                               [&]() noexcept { ++entry_checks; return do_enter; },
                                               [&]() noexcept { ++exit_checks; return exit_enabled; });
                EXPECT_EQ(entries, do_enter ? 1 : 0);
                EXPECT_EQ(entry_checks, 1);
                EXPECT_EQ(exit_checks, 0);
                exit_enabled = do_exit;
            }
            EXPECT_EQ(exits, do_exit ? 1 : 0);
            EXPECT_EQ(exit_checks, 1);
        }
    }
    int entries = 0;
    int exits = 0;
    {
        auto entry_condition = []() noexcept { return false; };
        auto guard = make_scope_action(
            [&]() noexcept { ++entries; },
            [&]() noexcept { ++exits; },
            entry_condition);
    }
    EXPECT_EQ(entries, 0);
    EXPECT_EQ(exits, 1);
}

TEST(ScopeAction, ActiveControlsOnlyExit) {
    for (const bool initially_active : {false, true}) {
        for (const bool finally_active : {false, true}) {
            int entries = 0;
            int exits = 0;
            int exit_checks = 0;
            {
                auto guard = make_scope_action([&]() noexcept { ++entries; },
                                               [&]() noexcept { ++exits; },
                                               []() noexcept { return true; },
                                               [&]() noexcept { ++exit_checks; return true; },
                                               initially_active);
                EXPECT_EQ(entries, 1);
                EXPECT_EQ(guard.active(), initially_active);
                guard.set_active(!finally_active);
                guard.set_active(finally_active);
                EXPECT_EQ(guard.active(), finally_active);
            }
            EXPECT_EQ(entries, 1);
            EXPECT_EQ(exits, finally_active ? 1 : 0);
            EXPECT_EQ(exit_checks, finally_active ? 1 : 0);
        }
    }
}

TEST(ScopeAction, PerfectForwarding) {
    tracking_state state;
    tracked_callable<void> entry(state);
    const tracked_callable<void> exit(state);
    tracked_callable<bool> entry_cond(state);
    const tracked_callable<bool> exit_cond(state);
    {
        auto guard = make_scope_action(entry, exit, entry_cond, exit_cond);
        EXPECT_EQ(state.copies, 4);
        EXPECT_EQ(state.moves, 0);
        EXPECT_EQ(state.calls, 2);
        EXPECT_EQ(entry.state, &state);
        EXPECT_EQ(entry_cond.state, &state);
    }
    EXPECT_EQ(state.calls, 4);
    state = {};
    {
        auto guard = make_scope_action(std::move(entry), tracked_callable<void>(state),
                                       std::move(entry_cond), tracked_callable<bool>(state));
        EXPECT_EQ(state.copies, 0);
        EXPECT_EQ(state.moves, 4);
        EXPECT_EQ(entry.state, nullptr);
        EXPECT_EQ(entry_cond.state, nullptr);
    }
    EXPECT_EQ(state.calls, 4);

    list_initializable_callable<void>::list_constructions = 0;
    list_initializable_callable<bool>::list_constructions = 0;
    {
        list_initializable_callable<bool> list_predicate;
        list_initializable_callable<void> list_function;
        // The throwing initializer-list constructor shouldn't be invoked.
        auto two = make_scope_action(list_function, list_function);
        auto three = make_scope_action(list_function, list_function, list_predicate);
        auto four = make_scope_action(list_function, list_function, list_predicate, list_predicate);
        auto moved = std::move(four);
        EXPECT_FALSE(four.active());
        EXPECT_TRUE(moved.active());
    }
    EXPECT_EQ(list_initializable_callable<void>::list_constructions, 0);
    EXPECT_EQ(list_initializable_callable<bool>::list_constructions, 0);
}

TEST(ScopeAction, MoveConstruction) {
    int exits = 0;
    int exit_checks = 0;
    {
        int entries = 0;
        int entry_checks = 0;
        auto source = make_scope_action(
            [value = std::make_unique<int>(2), &entries]() noexcept { entries += *value; },
            [value = std::make_unique<int>(3), &exits]() noexcept { exits += *value; },
            [value = std::make_unique<bool>(true), &entry_checks]() noexcept { ++entry_checks; return *value; },
            [value = std::make_unique<bool>(true), &exit_checks]() noexcept { ++exit_checks; return *value; });
        static_assert(!std::is_copy_constructible_v<decltype(source)>);
        static_assert(std::is_nothrow_move_constructible_v<decltype(source)>);
        // Entry func and entry cond called and only called here
        EXPECT_EQ(entries, 2);
        EXPECT_EQ(entry_checks, 1);
        {
            auto first = std::move(source);
            auto second = std::move(first);
            EXPECT_FALSE(source.active());
            EXPECT_FALSE(first.active());
            EXPECT_TRUE(second.active());
            EXPECT_EQ(entries, 2);
            EXPECT_EQ(entry_checks, 1);
        }
        EXPECT_EQ(exits, 3);
        EXPECT_EQ(exit_checks, 1);
    }
    EXPECT_EQ(exits, 3);
    EXPECT_EQ(exit_checks, 1);
    {
        auto source = make_scope_exit([&]() noexcept { ++exits; }, false);
        auto moved = std::move(source);
        EXPECT_FALSE(source.active());
        EXPECT_FALSE(moved.active());
    }
    EXPECT_EQ(exits, 3);

    tracking_state state;
    tracked_callable<void> function_ref(state);
    tracked_callable<bool> predicate_ref(state);
    {
        auto source = make_scope_action([&]() noexcept { function_ref(); },
                                        [&]() noexcept { function_ref(); },
                                        [&]() noexcept { return predicate_ref(); },
                                        [&]() noexcept { return predicate_ref(); });
        auto moved = std::move(source);
        EXPECT_FALSE(source.active());
        EXPECT_TRUE(moved.active());
    }
    EXPECT_EQ(state.copies, 0);
    EXPECT_EQ(state.moves, 0);
    EXPECT_EQ(state.calls, 4);
}

TEST(ScopeAction, DefaultsAndFunctionPointers) {
    default_function::observed = -1;
    {
        detail::scope_action<default_function, default_function> guard;
        EXPECT_EQ(default_function::observed, 0);
        default_function::observed = -1;
    }
    EXPECT_EQ(default_function::observed, 0);
    default_function::observed = -1;
    {
        detail::scope_action<default_function, default_function, default_predicate, default_predicate> guard;
        EXPECT_EQ(default_function::observed, -1);
    }
    EXPECT_EQ(default_function::observed, -1);
    {
        detail::scope_action<function, immovable_callable<void>> guard(function{});
    }
    {
        detail::scope_action<function, function, immovable_callable<bool>> guard(function{}, function{});
    }
    {
        detail::scope_action<function, function, predicate, immovable_callable<bool>> guard(
            function{}, function{}, predicate{});
    }
    pointer_calls = 0;
    {
        [[maybe_unused]] auto guard =
            make_scope_action(pointer_function, pointer_function, pointer_predicate, pointer_predicate);
        static_assert(std::same_as<decltype(guard),
                                   detail::scope_action<function_ptr, function_ptr, predicate_ptr, predicate_ptr>>);
        EXPECT_EQ(pointer_calls, 1);
        [[maybe_unused]] auto exit = make_scope_exit(pointer_function);
        static_assert(std::same_as<decltype(exit), detail::scope_exit<function_ptr>>);
    }
    EXPECT_EQ(pointer_calls, 3);
}

TEST(ScopeAction, ExitCondAndActivation) {
    int exits = 0;
    int checks = 0;
    {
        auto exit = make_scope_exit([&]() noexcept { ++exits; });
        auto inactive_exit = make_scope_exit([&]() noexcept { ++exits; }, false);
        auto skipped_exit = make_scope_exit([&]() noexcept { ++exits; },
                                           [&]() noexcept { ++checks; return false; });
        auto inactive_conditional_exit = make_scope_exit([&]() noexcept { ++exits; },
                                                        [&]() noexcept { ++checks; return true; }, false);
        auto reactivated_exit = make_scope_exit([&]() noexcept { ++exits; },
                                               [&]() noexcept { ++checks; return true; }, false);
        EXPECT_EQ(exits, 0);
        EXPECT_EQ(checks, 0);
        EXPECT_FALSE(inactive_exit.active());
        EXPECT_FALSE(inactive_conditional_exit.active());
        EXPECT_FALSE(reactivated_exit.active());
        reactivated_exit.set_active(true);
        EXPECT_TRUE(reactivated_exit.active());
    }
    EXPECT_EQ(exits, 2);
    EXPECT_EQ(checks, 2);
}

TEST(ScopeAction, CaughtExceptions) {
    int successes = 0;
    int failures = 0;
    auto run = [&](const bool catch_exception) {
        auto success = make_scope_success([&]() noexcept { ++successes; });
        auto fail = make_scope_fail([&]() noexcept { ++failures; });
        auto inactive_success = make_scope_success([&]() noexcept { ++successes; }, false);
        if (catch_exception) {
            try {
                throw 1;
            } catch (int) { }
        }
        return 42;
    };
    EXPECT_EQ(run(false), 42);
    EXPECT_EQ(successes, 1);
    EXPECT_EQ(failures, 0);
    EXPECT_EQ(run(true), 42);
    EXPECT_EQ(successes, 2);
    EXPECT_EQ(failures, 0);
}

TEST(ScopeAction, MovingDuringUnwinding) {
    int successes = 0;
    int failures = 0;
    int exits = 0;
    auto run = [&] {
        auto success = make_scope_success([&]() noexcept { ++successes; });
        auto fail = make_scope_fail([&]() noexcept { ++failures; });
        auto exit = make_scope_exit([&]() noexcept { ++exits; });
        auto inactive_fail = make_scope_fail([&]() noexcept { ++failures; }, false);
        struct move_on_unwind {
            decltype(success)& success_guard;
            decltype(fail)& fail_guard;
            ~move_on_unwind() noexcept {
                auto moved_success = std::move(success_guard);
                auto moved_fail = std::move(fail_guard);
            }
        } mover{success, fail};
        throw 1;
    };
    EXPECT_THROW(run(), int);
    EXPECT_EQ(successes, 0);
    EXPECT_EQ(failures, 1);
    EXPECT_EQ(exits, 1);
}

TEST(ScopeAction, NestedExceptions) {
    int successes = 0;
    int failures = 0;
    int nested_successes = 0;
    int nested_failures = 0;
    struct on_unwind {
        int& successes;
        int& failures;
        int& nested_successes;
        int& nested_failures;
        ~on_unwind() noexcept {
            auto success = make_scope_success([this]() noexcept { ++successes; });
            auto fail = make_scope_fail([this]() noexcept { ++failures; });
            try {
                auto nested_success = make_scope_success([this]() noexcept { ++nested_successes; });
                auto nested_fail = make_scope_fail([this]() noexcept { ++nested_failures; });
                throw 2;
            } catch (int) { }
        }
    };
    auto run = [&] {
        on_unwind guard{successes, failures, nested_successes, nested_failures};
        throw 1;
    };
    EXPECT_THROW(run(), int);
    EXPECT_EQ(successes, 1);
    EXPECT_EQ(failures, 0);
    EXPECT_EQ(nested_successes, 0);
    EXPECT_EQ(nested_failures, 1);
}