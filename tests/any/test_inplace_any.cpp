#include <gtest/gtest.h>
#include <urlicht/any/inplace_any.h>

struct TrackerIA {
    static int constructed;
    static int destroyed;
    int value;

    TrackerIA(int v) : value(v) { constructed++; }
    TrackerIA(const TrackerIA& other) : value(other.value) { constructed++; }
    TrackerIA(TrackerIA&& other) noexcept : value(other.value) {
        other.value = 0; 
        constructed++; 
    }
    ~TrackerIA() { destroyed++; }

    static void reset() { constructed = 0; destroyed = 0; }
};

int TrackerIA::constructed = 0;
int TrackerIA::destroyed = 0;

TEST(InplaceAnyTest, Basics) {
    urlicht::inplace_any<32> a(42);
    EXPECT_TRUE(a.has_value());
    EXPECT_EQ(a.type_info(), typeid(int));
    EXPECT_TRUE(a.is<int>());
    
    // Valid cast
    EXPECT_EQ(urlicht::any_cast<int>(a), 42);
    
    // Invalid cast
    EXPECT_THROW(urlicht::any_cast<float>(a), std::bad_any_cast);
    EXPECT_EQ(urlicht::any_cast<float>(&a), nullptr);
}

TEST(InplaceAnyTest, Emplace) {
    urlicht::inplace_any<32> a;
    EXPECT_FALSE(a.has_value());

    a.emplace<int>(100);
    EXPECT_TRUE(a.has_value());
    EXPECT_EQ(urlicht::any_cast<int>(a), 100);

    a.emplace<std::string>("Hello");
    EXPECT_EQ(a.type_info(), typeid(std::string));
    EXPECT_EQ(urlicht::any_cast<std::string>(a), "Hello");
}

TEST(InplaceAnyTest, Lifecycle) {
    TrackerIA::reset();
    {
        urlicht::inplace_any<64> a;
        a.emplace<TrackerIA>(10);
        EXPECT_EQ(TrackerIA::constructed, 1);
        EXPECT_EQ(TrackerIA::destroyed, 0);
        EXPECT_EQ(urlicht::any_cast<TrackerIA>(a).value, 10);

        a.emplace<TrackerIA>(20);
        EXPECT_EQ(TrackerIA::constructed, 2);
        EXPECT_EQ(TrackerIA::destroyed, 1);  // first one destroyed
    }
    EXPECT_EQ(TrackerIA::destroyed, 2);
}

TEST(InplaceAnyTest, Copy) {
    TrackerIA::reset();
    {
        urlicht::inplace_any<64> a(TrackerIA(10));
        EXPECT_EQ(TrackerIA::constructed, 2); // 1 temp, 1 in any
        EXPECT_EQ(TrackerIA::destroyed, 1);

        urlicht::inplace_any<64> b = a;
        EXPECT_EQ(TrackerIA::constructed, 3);
        EXPECT_EQ(urlicht::any_cast<TrackerIA>(b).value, 10);
        
        urlicht::inplace_any<64> c;
        c = a;
        EXPECT_EQ(TrackerIA::constructed, 4);
    }
    EXPECT_EQ(TrackerIA::destroyed, 4); // all destroyed
}

TEST(InplaceAnyTest, MoveSemantics) {
    TrackerIA::reset();
    {
        urlicht::inplace_any<64> a(TrackerIA(10));
        
        urlicht::inplace_any<64> b = std::move(a);
        EXPECT_TRUE(b.has_value());
        EXPECT_FALSE(a.has_value());
        EXPECT_EQ(urlicht::any_cast<TrackerIA>(b).value, 10);

        urlicht::inplace_any<64> c;
        c = std::move(b);
        EXPECT_TRUE(c.has_value());
        EXPECT_FALSE(b.has_value());
    }
    EXPECT_EQ(TrackerIA::constructed, 4);
    EXPECT_EQ(TrackerIA::destroyed, 4);
}

TEST(InplaceAnyTest, Swap) {
    urlicht::inplace_any<32> a(10);
    urlicht::inplace_any<32> b(20);

    a.swap(b);
    EXPECT_EQ(urlicht::any_cast<int>(a), 20);
    EXPECT_EQ(urlicht::any_cast<int>(b), 10);

    urlicht::inplace_any<32> c;
    c.swap(a);
    EXPECT_TRUE(c.has_value());
    EXPECT_EQ(urlicht::any_cast<int>(c), 20);
    EXPECT_FALSE(a.has_value());
}

TEST(InplaceAnyTest, Reset) {
    TrackerIA::reset();
    urlicht::inplace_any<64> a(TrackerIA(5));
    EXPECT_TRUE(a.has_value());
    
    a.reset();
    EXPECT_FALSE(a.has_value());
    EXPECT_EQ(TrackerIA::destroyed, 2); // 1 temp, 1 inside any
}

TEST(InplaceAnyTest, Alignment) {
    struct alignas(32) AlignedStruct {
        char data;
    };

    urlicht::inplace_any<64, 32> a;
    a.emplace<AlignedStruct>();
    
    const void* ptr = &urlicht::any_cast<AlignedStruct>(a);
    std::uintptr_t addr = reinterpret_cast<std::uintptr_t>(ptr);
    EXPECT_EQ(addr % 32, 0);
}

TEST(InplaceAnyTest, StoringArray) {
    constexpr std::array<int, 10> arr{};
    urlicht::inplace_any<64> a(arr);

    EXPECT_TRUE(a.has_value());
    EXPECT_TRUE((a.is<std::array<int, 10>>()));
    EXPECT_EQ((urlicht::any_cast<std::array<int, 10>>(a)), arr);
}

TEST(InplaceAnyTest, SizeConstraints) {
    struct Large {
        char data[100];
    };
    
    urlicht::inplace_any<100> a((Large()));
    EXPECT_TRUE(a.has_value());
}

TEST(InplaceAnyTest, InplaceConstructorTag) {
    struct Complex {
        int x, y;
        Complex(int a, int b) : x(a), y(b) {}
    };

    urlicht::inplace_any<32> a(urlicht::inplace<Complex>, 1, 2);
    EXPECT_EQ(urlicht::any_cast<Complex>(a).x, 1);
    EXPECT_EQ(urlicht::any_cast<Complex>(a).y, 2);

    urlicht::inplace_any<32> b(urlicht::inplace<Complex>, Complex{3, 4});
    EXPECT_EQ(urlicht::any_cast<Complex>(b).x, 3);
    EXPECT_EQ(urlicht::any_cast<Complex>(b).y, 4);

    // Redundant qualifiers (all ignored)
    urlicht::inplace_any<32> r(urlicht::inplace<const Complex&>, Complex{3, 4});
    EXPECT_EQ(urlicht::any_cast<Complex&&>(r).x, 3);
    EXPECT_EQ(urlicht::any_cast<const Complex&>(r).y, 4);

    urlicht::inplace_any<32> c(urlicht::inplace_cond<Complex>, false, Complex{3, 4});
    EXPECT_TRUE(!c.has_value());

}

TEST(InplaceAnyTest, MakeInplaceAny) {
    auto a = urlicht::make_inplace_any<int>(999);
    EXPECT_TRUE(a.is<int>());
    EXPECT_EQ(urlicht::any_cast<int>(a), 999);

    // Heuristics: max size = sizeof(T) * 2
    EXPECT_GE(decltype(a)::max_size(), sizeof(int));
    EXPECT_EQ(decltype(a)::max_align(), alignof(int));
}