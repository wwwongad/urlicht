#include <gtest/gtest.h>
#include <urlicht/memory/observer_ptr.h>

#include <concepts>
#include <cstdint>
#include <format>
#include <functional>
#include <memory>
#include <sstream>
#include <type_traits>
#include <unordered_set>
#include <utility>

namespace um = urlicht::memory;

namespace {

    // Distinct sentinel values used across the tests.
    constexpr int kBaseValue = 7;
    constexpr int kInitialValue = 42;
    constexpr int kUpdatedValue = 8;

    struct Base {
        int value{kBaseValue};
    };

    struct Derived : Base {
        int extra{9};
    };

    constexpr int kConstValue = 7;

    // -----------------------------------------------------------------------------
    // 1. Compile-time contracts
    // -----------------------------------------------------------------------------

    // Layout / special members: an observer is a raw pointer in disguise.
    static_assert(std::is_object_v<int>);
    static_assert(std::is_trivially_copyable_v<um::observer_ptr<int>>);
    static_assert(std::is_trivially_destructible_v<um::observer_ptr<int>>);
    static_assert(sizeof(um::observer_ptr<int>) == sizeof(int*));
    static_assert(alignof(um::observer_ptr<int>) == alignof(int*));
    static_assert(std::is_nothrow_default_constructible_v<um::observer_ptr<int>>);
    static_assert(std::is_nothrow_copy_constructible_v<um::observer_ptr<int>>);
    static_assert(std::is_nothrow_move_assignable_v<um::observer_ptr<int>>);

    // No accidental raw-pointer escape: only an explicit conversion exists.
    static_assert(!std::is_convertible_v<um::observer_ptr<int>, int*>);
    static_assert(!std::is_convertible_v<um::observer_ptr<int>, const int*>);
    static_assert(!std::is_convertible_v<um::observer_ptr<int>, void*>);
    static_assert(requires(um::observer_ptr<int> p) { static_cast<int*>(p); });

    // Observing an rvalue would dangle: construction from rvalues must be rejected.
    static_assert(std::is_constructible_v<um::observer_ptr<int>, int&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<int>, int&&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<int>, int>);
    static_assert(std::is_constructible_v<um::observer_ptr<const int>, const int&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<const int>, int&&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<const int>, int>);

    // Smart-pointer constructors bind lvalues only (a temporary owner would dangle).
    static_assert(std::is_constructible_v<um::observer_ptr<Base>, const std::unique_ptr<Base>&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<Base>, std::unique_ptr<Base>&&>);
    static_assert(std::is_constructible_v<um::observer_ptr<Base>, const std::shared_ptr<Base>&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<Base>, std::shared_ptr<Base>&&>);

    // Covariant owners are accepted (matching raw-pointer convertibility), but not downcasts.
    static_assert(std::is_constructible_v<um::observer_ptr<Base>, const std::unique_ptr<Derived>&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<Base>, std::unique_ptr<Derived>&&>);
    static_assert(std::is_constructible_v<um::observer_ptr<Base>, const std::shared_ptr<Derived>&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<Base>, std::shared_ptr<Derived>&&>);
    static_assert(!std::is_constructible_v<um::observer_ptr<Derived>, const std::shared_ptr<Base>&>);
    static_assert(std::is_constructible_v<um::observer_ptr<const Base>, const std::shared_ptr<Base>&>);

    // Conversion between observers mirrors raw-pointer convertibility.
    static_assert(std::is_convertible_v<um::observer_ptr<Derived>, um::observer_ptr<Base>>);
    static_assert(!std::is_convertible_v<um::observer_ptr<Base>, um::observer_ptr<Derived>>);

    // Traits.
    static_assert(urlicht::is_urlicht_observer_ptr_v<um::observer_ptr<int>>);
    static_assert(urlicht::is_urlicht_observer_ptr_v<um::observer_ptr<Base>>);
    static_assert(!urlicht::is_urlicht_observer_ptr_v<int*>);
    static_assert(!urlicht::is_urlicht_observer_ptr_v<std::unique_ptr<int>>);

    // CTAD.
    static_assert(std::same_as<decltype(um::observer_ptr{std::declval<int*>()}), um::observer_ptr<int>>);
    static_assert(std::same_as<decltype(um::observer_ptr{std::declval<int&>()}), um::observer_ptr<int>>);
    static_assert(std::same_as<decltype(um::observer_ptr{std::declval<Base*>()}), um::observer_ptr<Base>>);

} // namespace

// -----------------------------------------------------------------------------
// 2. Construction
// -----------------------------------------------------------------------------

TEST(ObserverPtr, DefaultConstruction) {
    const um::observer_ptr<int> def;
    EXPECT_FALSE(def);
    EXPECT_EQ(def.get(), nullptr);
    EXPECT_EQ(def.raw(), 0U);

    const um::observer_ptr<int> nul{nullptr};
    EXPECT_FALSE(nul);
    EXPECT_EQ(nul.get(), nullptr);

    const um::observer_ptr<int> nul2 = nullptr;
    EXPECT_FALSE(nul2);
}

TEST(ObserverPtr, FromPointer) {
    int x{kInitialValue};

    const um::observer_ptr<int> explicit_{&x};
    EXPECT_TRUE(explicit_);
    EXPECT_EQ(explicit_.get(), &x);

    // The pointer constructor is implicit.
    const um::observer_ptr<int> implicit_ = &x;
    EXPECT_EQ(implicit_.get(), &x);

    um::observer_ptr<int> assigned;
    assigned = &x;
    EXPECT_EQ(assigned.get(), &x);
}

TEST(ObserverPtr, FromReference) {
    int x{kInitialValue};
    const int cx{kConstValue};

    // Only lvalues bind.
    const um::observer_ptr<int> from_lvalue{x};
    EXPECT_EQ(from_lvalue.get(), &x);

    const um::observer_ptr<const int> from_const_lvalue{cx};
    EXPECT_EQ(from_const_lvalue.get(), &cx);
}

TEST(ObserverPtr, FromSmartPointers) {
    const auto up = std::make_unique<Base>();
    const um::observer_ptr<Base> from_unique{up};
    EXPECT_EQ(from_unique.get(), up.get());

    const auto sp = std::make_shared<Derived>();
    const um::observer_ptr<Base> from_shared{sp};
    EXPECT_EQ(from_shared.get(), sp.get());
}

TEST(ObserverPtr, ConvertingConstruction) {
    Derived d;

    const um::observer_ptr<Derived> od{&d};
    const um::observer_ptr<Base> ob{od};
    EXPECT_EQ(ob.get(), static_cast<Base*>(&d));

    const um::observer_ptr<Base> implicit_ = od;
    EXPECT_EQ(implicit_.get(), static_cast<Base*>(&d));
}

// -----------------------------------------------------------------------------
// 3. Modifiers
// -----------------------------------------------------------------------------

TEST(ObserverPtr, AssignAndReset) {
    int x{1};
    int y{2};

    um::observer_ptr<int> p{&x};
    p = &y;
    EXPECT_EQ(p.get(), &y);

    p = nullptr;
    EXPECT_FALSE(p);

    p.reset();
    EXPECT_EQ(p.get(), nullptr);
    EXPECT_FALSE(p);

    p.reset(&x);
    EXPECT_EQ(p.get(), &x);

    p.clear();
    EXPECT_EQ(p.get(), nullptr);
}

TEST(ObserverPtr, Swap) {
    int x{1};
    int y{2};
    um::observer_ptr<int> a{&x};
    um::observer_ptr<int> b{&y};

    a.swap(b);
    EXPECT_EQ(a.get(), &y);
    EXPECT_EQ(b.get(), &x);

    using std::swap;
    swap(a, b);
    EXPECT_EQ(a.get(), &x);
    EXPECT_EQ(b.get(), &y);

    // ADL swap on one empty observer.
    um::observer_ptr<int> empty;
    swap(a, empty);
    EXPECT_EQ(a.get(), nullptr);
    EXPECT_EQ(empty.get(), &x);
}

// -----------------------------------------------------------------------------
// 4. Observers
// -----------------------------------------------------------------------------

TEST(ObserverPtr, Dereference) {
    struct Item { int v{5}; };
    Item item;

    const um::observer_ptr<Item> p{&item};
    EXPECT_EQ((*p).v, 5);
    EXPECT_EQ(p->v, 5);

    p->v = kUpdatedValue;
    EXPECT_EQ(item.v, kUpdatedValue);
}

TEST(ObserverPtr, BooleanAndRawAccess) {
    int x{1};
    const um::observer_ptr<int> p{&x};

    EXPECT_TRUE(static_cast<bool>(p));
    EXPECT_TRUE(p);
    EXPECT_EQ(p.get(), &x);
    EXPECT_EQ(p.raw(), reinterpret_cast<std::uintptr_t>(&x));

    // Explicit conversion to a raw pointer.
    int* const raw = static_cast<int*>(p);
    EXPECT_EQ(raw, &x);
}

// -----------------------------------------------------------------------------
// 5. Comparisons
// -----------------------------------------------------------------------------

TEST(ObserverPtr, ComparisonWithObserver) {
    int x{1};
    int y{2};
    const um::observer_ptr<int> px{&x};
    const um::observer_ptr<int> px2{&x};
    const um::observer_ptr<int> py{&y};

    EXPECT_TRUE(px == px2);
    EXPECT_FALSE(px == py);
    EXPECT_TRUE(px != py);
    EXPECT_FALSE(px != px2);

    // Ordering follows address order.
    const um::observer_ptr<int> lower{&x};
    const um::observer_ptr<int> higher{&y};
    if (&x < &y) {
        EXPECT_TRUE(lower < higher);
        EXPECT_TRUE(lower <= higher);
        EXPECT_TRUE(higher > lower);
        EXPECT_TRUE(higher >= lower);
    } else {
        EXPECT_TRUE(higher < lower);
        EXPECT_TRUE(higher <= lower);
        EXPECT_TRUE(lower > higher);
        EXPECT_TRUE(lower >= higher);
    }
}

TEST(ObserverPtr, ComparisonWithRawPointer) {
    int x{1};
    const um::observer_ptr<int> p{&x};

    EXPECT_TRUE(p == &x);
    EXPECT_TRUE(&x == p);
    EXPECT_FALSE(p == nullptr);
    EXPECT_FALSE(nullptr == p);

    const um::observer_ptr<int> empty;
    EXPECT_TRUE(empty == nullptr);
    EXPECT_TRUE(nullptr == empty);
    EXPECT_FALSE(empty == &x);

    // Explicit bool plus comparisons keep `p != nullptr` unambiguous.
    EXPECT_TRUE(p != nullptr);
    EXPECT_FALSE(empty != nullptr);
}

// -----------------------------------------------------------------------------
// 6. Standard-library integration
// -----------------------------------------------------------------------------

TEST(ObserverPtr, Hashing) {
    int a{1};
    int b{2};

    const um::observer_ptr<int> pa{&a};
    const um::observer_ptr<int> pa2{&a};
    const um::observer_ptr<int> pb{&b};

    constexpr std::hash<um::observer_ptr<int>> hasher;
    EXPECT_EQ(hasher(pa), hasher(pa2));
    EXPECT_EQ(hasher(pa), std::hash<std::uintptr_t>{}(pa.raw()));

    std::unordered_set<um::observer_ptr<int>> set;
    set.insert(pa);
    set.insert(pa2); // duplicate address
    set.insert(pb);
    EXPECT_EQ(set.size(), 2U);
    EXPECT_EQ(set.count(pa), 1U);
}

TEST(ObserverPtr, Formatting) {
    int x{1};
    const um::observer_ptr<int> p{&x};

    EXPECT_EQ(std::format("{}", p), std::format("{}", p.raw()));

    std::ostringstream os;
    os << p;
    EXPECT_EQ(os.str(), std::to_string(p.raw()));
}

TEST(ObserverPtr, PointerTraits) {
    using traits = std::pointer_traits<um::observer_ptr<int>>;
    static_assert(std::same_as<traits::element_type, int>);
    static_assert(std::same_as<traits::pointer, um::observer_ptr<int>>);
    static_assert(std::same_as<traits::rebind<double>, um::observer_ptr<double>>);
    static_assert(std::same_as<traits::difference_type, std::ptrdiff_t>);

    int x{3};
    const um::observer_ptr<int> p = traits::pointer_to(x);
    EXPECT_EQ(p.get(), &x);

    int* const addr = traits::to_address(p);
    EXPECT_EQ(addr, &x);
}

// -----------------------------------------------------------------------------
// 7. Lifetime
// -----------------------------------------------------------------------------

TEST(ObserverPtr, NoOwnership) {
    const auto owner = std::make_unique<int>(11);
    {
        const um::observer_ptr<int> p{owner.get()};
        EXPECT_EQ(*p, 11);
    }
    // observer destruction must not delete the pointee
    EXPECT_EQ(*owner, 11);
}