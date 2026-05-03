#include <gtest/gtest.h>
#include <urlicht/functional/adaptive_function.h>
#include <memory>
#include <type_traits>

using namespace urlicht;

struct nospec_f {
    int operator()() { return -1; }
};

struct const_f {
    int operator()() const { return 0; }
};

struct lvalue_f {
    int operator ()() & { return 1; }
};

struct const_lvalue_f {
    int operator ()() const & { return 2; }
};

struct rvalue_f {
    int operator()() && { return 3; }
};

struct const_rvalue_f {
    int operator()() const && { return 4; }
};

struct noexcept_f {
    int operator()() noexcept { return 5; }
};

struct both_ref_f {
    int operator()() & { return 10; }
    int operator()() && { return 11; }
};

struct ret_long_f {
    long operator()() { return 20.109; }
};

struct ret_void_f {
    void operator()() {}
};

struct wrong_arg_f {
    int operator()(int) { return 12; }
};

/************** Constructibility ***************/
// No specifiers
static_assert(std::constructible_from<adaptive_function<int()>, nospec_f>);
static_assert(std::constructible_from<adaptive_function<int()>, const_f>);
static_assert(!std::constructible_from<adaptive_function<int()>, lvalue_f>);
static_assert(std::constructible_from<adaptive_function<int()>, const_lvalue_f>); // can bind to const rvalue
static_assert(!std::constructible_from<adaptive_function<int()>, rvalue_f>);
static_assert(!std::constructible_from<adaptive_function<int()>, const_rvalue_f>);
static_assert(std::constructible_from<adaptive_function<int()>, noexcept_f>);
static_assert(std::constructible_from<adaptive_function<int()>, both_ref_f>);
static_assert(std::constructible_from<adaptive_function<int()>, ret_long_f>);
static_assert(!std::constructible_from<adaptive_function<int()>, ret_void_f>);
static_assert(!std::constructible_from<adaptive_function<int()>, wrong_arg_f>);

// noexcept
static_assert(std::constructible_from<adaptive_function<int() noexcept>, noexcept_f>);
static_assert(!std::constructible_from<adaptive_function<int() noexcept>, nospec_f>);
static_assert(!std::constructible_from<adaptive_function<int() noexcept>, const_f>);

// lvalue
static_assert(std::constructible_from<adaptive_function<int() &>, lvalue_f>);
static_assert(std::constructible_from<adaptive_function<int() &>, nospec_f>);
static_assert(!std::constructible_from<adaptive_function<int() &>, rvalue_f>);
static_assert(std::constructible_from<adaptive_function<int() &>, both_ref_f>);

// rvalue
static_assert(!std::constructible_from<adaptive_function<int() &&>, lvalue_f>);
static_assert(std::constructible_from<adaptive_function<int() &&>, nospec_f>);
static_assert(std::constructible_from<adaptive_function<int() &&>, rvalue_f>);
static_assert(std::constructible_from<adaptive_function<int() &&>, both_ref_f>);

// const
static_assert(!std::constructible_from<adaptive_function<int() const>, nospec_f>);
static_assert(std::constructible_from<adaptive_function<int() const>, const_f>);
static_assert(std::constructible_from<adaptive_function<int() const>, const_lvalue_f>);
static_assert(!std::constructible_from<adaptive_function<int() const>, const_rvalue_f>);
static_assert(!std::constructible_from<adaptive_function<int() const>, both_ref_f>);


/************** Invocability ***************/
// No specifiers
static_assert(std::is_invocable_v<adaptive_function<int()>&>);
static_assert(std::is_invocable_v<adaptive_function<int()>&&>);
static_assert(!std::is_invocable_v<const adaptive_function<int()>&>);
static_assert(!std::is_invocable_v<const adaptive_function<int()>&&>);

// &
static_assert(std::is_invocable_v<adaptive_function<int()&>&>);
static_assert(!std::is_invocable_v<adaptive_function<int()&>&&>);
static_assert(!std::is_invocable_v<const adaptive_function<int()&>&>);
static_assert(!std::is_invocable_v<const adaptive_function<int()&>&&>);

// &&
static_assert(std::is_invocable_v<adaptive_function<int()&&>&&>);
static_assert(!std::is_invocable_v<adaptive_function<int()&&>&>);
static_assert(!std::is_invocable_v<const adaptive_function<int()&&>&&>);
static_assert(!std::is_invocable_v<const adaptive_function<int()&&>&>);

// const
static_assert(std::is_invocable_v<adaptive_function<int() const>&>);
static_assert(std::is_invocable_v<adaptive_function<int() const>&&>);
static_assert(std::is_invocable_v<const adaptive_function<int() const>&>);
static_assert(std::is_invocable_v<const adaptive_function<int() const>&&>);

// const &
static_assert(std::is_invocable_v<adaptive_function<int() const &>&>);
static_assert(std::is_invocable_v<adaptive_function<int() const &>&&>);
static_assert(std::is_invocable_v<const adaptive_function<int() const &>&>);
static_assert(std::is_invocable_v<const adaptive_function<int() const &>&&>);

// const &&
static_assert(!std::is_invocable_v<adaptive_function<int() const &&>&>);
static_assert(std::is_invocable_v<adaptive_function<int() const &&>&&>);
static_assert(!std::is_invocable_v<const adaptive_function<int() const &&>&>);
static_assert(std::is_invocable_v<const adaptive_function<int() const &&>&&>);

struct tracker_func {
    static int constructed;
    static int destructed;
    static void reset() { constructed = 0; destructed = 0; }
private:
    int num = 0;
public:
    tracker_func() { ++constructed; }
    tracker_func(int x) : num{x} { ++constructed; }
    tracker_func(const tracker_func& other) : num(other.num) { ++constructed; }
    tracker_func(tracker_func&& other) noexcept : num(other.num) { other.num = 0; ++constructed; }
    ~tracker_func() { ++destructed; }
    int operator() () const noexcept { return num; }
};

int tracker_func::constructed = 0;
int tracker_func::destructed = 0;

bool check_tracker_record(int expect_constructed, int expect_destructed) {
    EXPECT_EQ(tracker_func::constructed, expect_constructed);
    EXPECT_EQ(tracker_func::destructed, expect_destructed);
    return tracker_func::constructed == expect_constructed && tracker_func::destructed == expect_destructed;
}

TEST(AdaptiveFunction, DefaultInit) {
    adaptive_function<int()> f;
    EXPECT_FALSE(f);
    EXPECT_EQ(f, nullptr);
    EXPECT_EQ(nullptr, f);
}

TEST(AdaptiveFunction, LambdaBasics) {
    int x = 10;
    auto l = [&x](int i) {
        return i * x;
    };
    adaptive_function<int(int)> f = l;
    EXPECT_TRUE(f);
    EXPECT_NE(f, nullptr);
    EXPECT_TRUE(f.is<decltype(l)>());
    EXPECT_TRUE(f.in_sbo());
    EXPECT_EQ(f(10), 100);
    x = 20;
    EXPECT_EQ(f(10), 200);
}

TEST(AdaptiveFunction, NullDetection) {
    adaptive_function<int()> f = nullptr;
    EXPECT_FALSE(f);
    EXPECT_EQ(f, nullptr);

    f = nospec_f{};
    EXPECT_TRUE(f);

    std::function<int()> null_f;
    f = null_f;
    EXPECT_FALSE(f);
}

int free_func() { return -123; }
int free_func2() { return -222; }

TEST(AdaptiveFunction, FunctionPointers) {
    int (*fp)() = &free_func;
    adaptive_function<int()> f = fp;
    EXPECT_TRUE(f);
    EXPECT_EQ(f(), -123);

    fp = nullptr;
    f = fp;
    EXPECT_FALSE(f);

    fp = &free_func2;
    f = fp;
    EXPECT_TRUE(f);
    EXPECT_EQ(f(), -222);
}

TEST(AdaptiveFunction, Reset) {
    tracker_func f1;
    tracker_func::reset();
    adaptive_function<int()> f = f1;
    EXPECT_TRUE(f);
    EXPECT_TRUE(check_tracker_record(1, 0));

    f.reset();
    EXPECT_FALSE(f);
    EXPECT_TRUE(check_tracker_record(1, 1));
}

struct large_callable {
    int arr[100];
    int operator()() const { return arr[0]; }
};

TEST(AdaptiveFunction, FallBackOnHeap) {
    adaptive_function<int()> f = nospec_f{};
    EXPECT_TRUE(f);
    EXPECT_TRUE(f.in_sbo());

    f = large_callable{};
    EXPECT_TRUE(f);
    EXPECT_FALSE(f.in_sbo());  // On heap

    f = nospec_f{};
    EXPECT_TRUE(f);
    EXPECT_TRUE(f.in_sbo());   // Back to stack
}

TEST(AdaptiveFunction, CustomSBO) {
    adaptive_function<int(), 1> f = tracker_func{};
    EXPECT_TRUE(f);
    EXPECT_FALSE(f.in_sbo());

    adaptive_function<int(), 1024> f2 = large_callable{};
    EXPECT_TRUE(f2);
    EXPECT_TRUE(f2.in_sbo());
}


TEST(AdaptiveFunction, Alignment) {
    struct alignas(1024) over_aligned_f {
        int operator()() { return 100; }
    };


    adaptive_function<int()> f1 = over_aligned_f{};
    EXPECT_TRUE(f1);
    EXPECT_FALSE(f1.in_sbo());
    EXPECT_EQ(f1(), 100);

    adaptive_function<int(), 1024, 1024> f2 = over_aligned_f{};
    EXPECT_TRUE(f2);
    EXPECT_TRUE(f2.in_sbo());
    EXPECT_EQ(f2(), 100);
}

TEST(AdaptiveFunction, Lifecycle) {
    auto test_f = []<auto sbo_size, auto in_sbo>(detail::nontype_t<sbo_size>, detail::nontype_t<in_sbo>) {
        tracker_func f1(10), f2(20);
        tracker_func::reset();
        {
            adaptive_function<int(), sbo_size> f = f1;
            EXPECT_EQ(f.in_sbo(), in_sbo);
            EXPECT_TRUE(check_tracker_record(1, 0));
            EXPECT_EQ(f(), 10);

            f = f2;
            EXPECT_EQ(f.in_sbo(), in_sbo);
            EXPECT_TRUE(check_tracker_record(2, 1));
            EXPECT_EQ(f(), 20);
        }
        EXPECT_TRUE(check_tracker_record(2, 2));
    };
    test_f(nontype<64>, nontype<true>); // On stack
    test_f(nontype<1>, nontype<false>); // On heap
}

TEST(AdaptiveFunction, CopySemantics) {
    tracker_func f(123);
    tracker_func::reset();
    {
        adaptive_function<int()> f1 = f;
        EXPECT_TRUE(check_tracker_record(1, 0));

        adaptive_function<int()> f2 = f1;
        EXPECT_TRUE(check_tracker_record(2, 0));
        EXPECT_EQ(f2(), 123);

        adaptive_function<int()> f3{};
        f3 = f2;
        EXPECT_TRUE(check_tracker_record(3, 0));
        EXPECT_EQ(f3(), 123);
    }
    EXPECT_TRUE(check_tracker_record(3, 3));
}

TEST(AdaptiveFunction, MoveSemantics) {
    tracker_func f(123);
    tracker_func::reset();
    {
        adaptive_function<int()> f1 = f;
        EXPECT_TRUE(check_tracker_record(1, 0));

        adaptive_function<int()> f2 = std::move(f1);
        EXPECT_TRUE(check_tracker_record(2, 1));
        EXPECT_FALSE(f1);
        EXPECT_EQ(f2(), 123);

        adaptive_function<int()> f3{};
        f3 = std::move(f2);
        EXPECT_TRUE(check_tracker_record(3, 2));
        EXPECT_FALSE(f2);
        EXPECT_EQ(f3(), 123);
    }
    EXPECT_TRUE(check_tracker_record(3, 3));
}

TEST(AdaptiveFunction, InplaceConstruction) {
    tracker_func::reset();
    adaptive_function<int()> f1(inplace<tracker_func>, 42);
    EXPECT_TRUE(check_tracker_record(1, 0));
    EXPECT_TRUE(f1);
    EXPECT_EQ(f1(), 42);

    struct il_func {
        std::size_t x{};
        il_func(std::initializer_list<int> il, size_t h) : x{il.size() * h} {}
        auto operator()() { return x; }
    };

    adaptive_function<size_t()> f2(inplace<il_func>, {1, 2, 3}, 100u);
    EXPECT_TRUE(f2);
    EXPECT_EQ(f2(), 300);
}

TEST(AdaptiveFunction, NontypeConstruction) {
    adaptive_function<int()> f(nontype<free_func>);
    EXPECT_TRUE(f);
    EXPECT_FALSE(f.in_sbo());
    EXPECT_EQ(f(),-123);
}

TEST(AdaptiveFunction, Emplace) {
    tracker_func::reset();
    adaptive_function<int()> f;
    auto& emplaced_ref = f.emplace<tracker_func>(42);

    EXPECT_TRUE(f);
    EXPECT_EQ(f(), 42);
    EXPECT_EQ(emplaced_ref(), 42);
    EXPECT_TRUE(check_tracker_record(1, 0));

    f.emplace<const tracker_func&>(99); // Discard redundant qualifiers
    EXPECT_TRUE(f.is<tracker_func>());
    EXPECT_EQ(f(), 99);
    EXPECT_TRUE(check_tracker_record(2, 1));
}

TEST(AdaptiveFunction, Swap) {
    adaptive_function<int()> f1, f2;
    swap(f1, f2);
    EXPECT_FALSE(f1);
    EXPECT_FALSE(f2);

    f1 = tracker_func{10};
    swap(f1, f2);
    EXPECT_FALSE(f1);
    EXPECT_TRUE(f2);
    EXPECT_EQ(f2(), 10);

    f1 = tracker_func{20};
    swap(f1, f2);
    EXPECT_TRUE(f1);
    EXPECT_TRUE(f2);
    EXPECT_EQ(f1(), 10);
    EXPECT_EQ(f2(), 20);
}

struct consumer_f {
    void operator()(const std::vector<int>& vec) const {
        std::vector v(vec);
    }
    void operator()(std::vector<int>&& vec) const {
        std::vector v(std::move(vec));
    }
};

TEST(AdaptiveFunction, CorrectRefOverload) {
    adaptive_function<int()&> lval(both_ref_f{});
    static_assert(!std::invocable<decltype(lval)&&>);
    EXPECT_EQ(lval(), 10);

    adaptive_function<int()&&> rval(both_ref_f{});
    static_assert(!std::invocable<decltype(rval)&>);
    EXPECT_EQ(std::move(rval)(), 11);
}

TEST(AdaptiveFunction, CorrectArgForward) {
    std::vector<int> v1(100), v2(100);
    adaptive_function<void(const std::vector<int>&)> f1 = consumer_f{};
    f1(v1);
    EXPECT_FALSE(v1.empty());
    EXPECT_EQ(v1.size(), 100);

    f1(std::move(v1)); // Bind to lvalue internally
    EXPECT_FALSE(v1.empty());
    EXPECT_EQ(v1.size(), 100);

    adaptive_function<void(std::vector<int>&&)> f2 = consumer_f{};
    f2(std::move(v2));
    EXPECT_TRUE(v2.empty());
    EXPECT_EQ(v2.size(), 0);
}

////////////////////////////////////////////
///     adaptive_move_only_function     ///
///////////////////////////////////////////


struct move_only_f {
    std::unique_ptr<int> p;

    explicit move_only_f(int v) : p(std::make_unique<int>(v)) {}
    move_only_f(move_only_f&&) noexcept = default;
    move_only_f& operator=(move_only_f&&) noexcept = default;

    move_only_f(const move_only_f&) = delete;
    move_only_f& operator=(const move_only_f&) = delete;

    int operator()() noexcept { return *p; }
};

using move_only_func = urlicht::adaptive_move_only_function<int(), 64, alignof(std::max_align_t)>;

static_assert(!std::copy_constructible<move_only_func>);
static_assert(!std::is_copy_assignable_v<move_only_func>);
static_assert(std::move_constructible<move_only_func>);
static_assert(std::is_move_assignable_v<move_only_func>);
static_assert(urlicht::is_urlicht_adaptive_function_v<move_only_func>);

TEST(AdaptiveMoveOnlyFunction, ConstructAndInvoke) {
    move_only_func f = move_only_f{42};
    EXPECT_TRUE(f);
    EXPECT_TRUE(f.in_sbo());
    EXPECT_EQ(f(), 42);
}

TEST(AdaptiveMoveOnlyFunction, MoveConstructor) {
    move_only_func f1 = move_only_f{7};
    EXPECT_TRUE(f1);

    move_only_func f2 = std::move(f1);
    EXPECT_FALSE(f1);
    EXPECT_TRUE(f2);
    EXPECT_EQ(f2(), 7);
}

TEST(AdaptiveMoveOnlyFunction, MoveAssignment) {
    move_only_func src = move_only_f{111};
    move_only_func dst = move_only_f{222};

    EXPECT_TRUE(src);
    EXPECT_TRUE(dst);

    dst = std::move(src);
    EXPECT_FALSE(src);
    EXPECT_TRUE(dst);
    EXPECT_EQ(dst(), 111);
}

TEST(AdaptiveMoveOnlyFunction, Emplace) {
    move_only_func f;
    auto& ref = f.emplace<move_only_f>(99);

    EXPECT_TRUE(f);
    EXPECT_EQ(ref(), 99);
    EXPECT_EQ(f(), 99);
}

TEST(AdaptiveMoveOnlyFunction, MoveOnlyLambda) {
    auto lam = [p = std::make_unique<int>(5)]() mutable noexcept {
        return (*p)++;
    };

    move_only_func f(std::move(lam));
    EXPECT_TRUE(f);
    EXPECT_EQ(f(), 5);
    EXPECT_EQ(f(), 6);
}

TEST(AdaptiveMoveOnlyFunction, Swap) {
    move_only_func a;
    move_only_func b;

    swap(a, b);
    EXPECT_FALSE(a);
    EXPECT_FALSE(b);

    a = move_only_f{42};
    swap(a, b);
    EXPECT_FALSE(a);
    EXPECT_TRUE(b);
    EXPECT_EQ(b(), 42);

    a = move_only_f{7};
    swap(a, b);
    EXPECT_TRUE(a);
    EXPECT_TRUE(b);
    EXPECT_EQ(a(), 42);
    EXPECT_EQ(b(), 7);
}
