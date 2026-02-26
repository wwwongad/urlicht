#include <gtest/gtest.h>
#include <urlicht/any/adaptive_any.h>

struct TrackerAA {
    static int constructed;
    int id;

    TrackerAA(int i) : id(i) { ++constructed; }
    TrackerAA(const TrackerAA& other) : id(other.id) { ++constructed; }
    ~TrackerAA() { --constructed; }
};

int TrackerAA::constructed = 0;

using namespace urlicht;

struct alignas(32) Aligned32 {
    float x, y, z, w;
    friend bool operator== (const Aligned32& lhs, const Aligned32& rhs) {
        return lhs.x == rhs.x && lhs.y == rhs.y &&
               lhs.z == rhs.z && lhs.w == rhs.w;
    }
};

struct LargeType {
    char data[1024];
};

TEST(AdaptiveAnyTest, BasicOp) {
    adaptive_any<16, 8> sbo_any(12345);
    EXPECT_TRUE(sbo_any.is<int>());
    EXPECT_TRUE(sbo_any.has_value());
    EXPECT_TRUE(sbo_any.in_sbo());
    EXPECT_EQ(any_cast<int>(sbo_any), 12345);

    adaptive_any<16, 8> heap_any(LargeType{});
    EXPECT_TRUE(heap_any.has_value());
    EXPECT_FALSE(heap_any.in_sbo());
    EXPECT_TRUE(heap_any.is<LargeType>());
}

TEST(AdaptiveAnyTest, InplaceConstruction) {
    // Note that all extra qualifiers are ignored in construction and cast
    adaptive_any<16, 8> any(inplace<Aligned32>, Aligned32{1, 2, 3, 4});
    EXPECT_TRUE(any.has_value());
    EXPECT_EQ(any_cast<const Aligned32&>(any), Aligned32(1, 2, 3, 4));

    adaptive_any<16, 8> any2(inplace<Aligned32&&>, Aligned32{1, 2, 3, 4});
    EXPECT_TRUE(any.has_value());
    EXPECT_EQ(any_cast<Aligned32>(any), Aligned32(1, 2, 3, 4));

    adaptive_any<16, 8> any3(inplace<const Aligned32&>, Aligned32{1, 2, 3, 4});
    EXPECT_TRUE(any.has_value());
    EXPECT_EQ(any_cast<Aligned32&&>(any), Aligned32(1, 2, 3, 4));
}

TEST(AdaptiveAnyTest, CopySemantics) {
    adaptive_any<32, 8> a1(std::string("Hello World"));

    adaptive_any<32, 8> a2 = a1;

    EXPECT_EQ(any_cast<std::string>(a1), "Hello World");
    EXPECT_EQ(any_cast<std::string>(a2), "Hello World");

    any_cast<std::string>(a2) = "Modified";
    EXPECT_EQ(any_cast<std::string>(a1), "Hello World");
    EXPECT_EQ(any_cast<std::string>(a2), "Modified");

    adaptive_any<32, 8> a3;
    a3 = a1;
    EXPECT_EQ(any_cast<std::string>(a3), "Hello World");
}

TEST(AdaptiveAnyTest, MoveSemantics) {
    adaptive_any<32, 8> a1(std::string("Will be moved from"));
    EXPECT_TRUE(a1.has_value());

    adaptive_any<32, 8> a2 = std::move(a1);

    EXPECT_FALSE(a1.has_value());
    EXPECT_TRUE(a2.has_value());
    EXPECT_EQ(any_cast<std::string>(a2), "Will be moved from");

    adaptive_any<32, 8> a3;
    a3 = std::move(a2);

    EXPECT_FALSE(a2.has_value());
    EXPECT_EQ(any_cast<std::string>(a3), "Will be moved from");
}

TEST(AdaptiveAnyTest, SelfAssignment) {
    adaptive_any<32, 8> a(100);

    a = a;
    EXPECT_TRUE(a.has_value());
    EXPECT_EQ(any_cast<int>(a), 100);

    a = std::move(a);
    EXPECT_TRUE(a.has_value());
    EXPECT_EQ(any_cast<int>(a), 100);
}

TEST(AdaptiveAnyTest, BadCasting) {
    adaptive_any<32, 8> a(3.14159);

    EXPECT_TRUE(a.is<double>());
    EXPECT_FALSE(a.is<int>());
    EXPECT_DOUBLE_EQ(any_cast<double>(a), 3.14159);

    EXPECT_EQ(any_cast<int>(&a), nullptr);
    EXPECT_THROW(any_cast<int>(a), std::bad_any_cast);

    EXPECT_DOUBLE_EQ(unchecked_any_cast<double>(a), 3.14159);
}


TEST(AdaptiveAnyTest, LifetimeManagement) {
    TrackerAA::constructed = 0;
    auto* ptr = new adaptive_any<64, 8>(TrackerAA(1));

    int current = TrackerAA::constructed;
    EXPECT_GT(current, 0);

    delete ptr;
    EXPECT_EQ(TrackerAA::constructed, current - 1);
}

TEST(AdaptiveAnyTest, EmplaceNew) {
    TrackerAA::constructed = 0;
    adaptive_any<64, 8> any(TrackerAA(1));
    int current = TrackerAA::constructed;

    any.emplace<int>(123);
    EXPECT_EQ(TrackerAA::constructed, current - 1);
}

TEST(AdaptiveAnyTest, Alignment) {
    adaptive_any<64, 16> a(Aligned32{1, 2, 3, 4});
    EXPECT_FALSE(a.in_sbo());

    adaptive_any<64, 32> b(Aligned32{1, 2, 3, 4});
    EXPECT_TRUE(b.in_sbo());

    auto* ptr = any_cast<Aligned32>(&b);
    std::uintptr_t addr = reinterpret_cast<std::uintptr_t>(ptr);
    EXPECT_EQ(addr % 32, 0);
}

TEST(AdaptiveAnyTest, Swap) {
    adaptive_any<32, 8> a(10);
    adaptive_any<32, 8> b(20);

    a.swap(b);

    EXPECT_EQ(any_cast<int>(a), 20);
    EXPECT_EQ(any_cast<int>(b), 10);

    adaptive_any<32, 8> c;
    c.swap(a);

    EXPECT_EQ(any_cast<int>(c), 20);
    EXPECT_FALSE(a.has_value());
}

TEST(AdaptiveAnyTest, MakeAdaptiveAny) {
    auto a = make_adaptive_any<int>(42);

    EXPECT_TRUE(a.has_value());
    EXPECT_EQ(any_cast<int>(a), 42);
    EXPECT_GE(a.buffer_size(), sizeof(int));
}

TEST(AdaptiveAnyTest, CVRefHandling) {
    int x = 10;
    adaptive_any<32, 8> a(x);

    int& ref = any_cast<int&>(a);
    ref = 20;
    EXPECT_EQ(any_cast<int>(a), 20);

    int cref = any_cast<const int&>(a);
    EXPECT_EQ(cref, 20);

    int val = any_cast<int&&>(a);
    EXPECT_EQ(val, 20);
}
