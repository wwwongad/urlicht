#include <gtest/gtest.h>
#include <urlicht/memory/tagged_ptr.h>
#include <string>
#include <vector>

struct alignas(16) AlignedItem {
    int value;
    static int construction_count;
    static int destruction_count;

    AlignedItem(int v = 0) : value(v) { construction_count++; }
    ~AlignedItem() { destruction_count++; }

    static void reset_counts() {
        construction_count = 0;
        destruction_count = 0;
    }
};

int AlignedItem::construction_count = 0;
int AlignedItem::destruction_count = 0;

// -----------------------------------------------------------------------------
// 1. Static Assertions and Traits
// -----------------------------------------------------------------------------

TEST(TaggedPtrTest, TraitsAndAlignment) {
    using Ptr = urlicht::tagged_ptr<AlignedItem>;

    // alignof(AlignedItem) == 16, log2(16) = 4 bits
    EXPECT_EQ(Ptr::num_free_bits(), 4);

    EXPECT_EQ(sizeof(Ptr), sizeof(uintptr_t));

}

// -----------------------------------------------------------------------------
// 2. Non-Owning Tests (Observer Semantics)
// -----------------------------------------------------------------------------

TEST(TaggedPtrTest, NonOwning_ConstructionAndAccess) {
    AlignedItem item(42);
    urlicht::tagged_ptr ptr(&item);

    EXPECT_FALSE(ptr.is_owning());
    EXPECT_EQ(ptr.get(), &item);
    EXPECT_EQ(ptr->value, 42);
    EXPECT_EQ((*ptr).value, 42);
    EXPECT_TRUE(static_cast<bool>(ptr));
}

TEST(TaggedPtrTest, NonOwning_TagManipulation) {
    AlignedItem item(100);
    urlicht::tagged_ptr ptr(&item);

    // Initial tag should be 0
    EXPECT_EQ(ptr.tag(), 0);

    // Set tag
    ptr.set_tag(7); // 0111
    EXPECT_EQ(ptr.tag(), 7);
    EXPECT_EQ(ptr.get(), &item); // Pointer should still be valid

    // Bit manipulation
    ptr.set_tag_bit(3); // Now 1111 (15)
    EXPECT_EQ(ptr.tag(), 15);
    EXPECT_TRUE(ptr.tag_bit(3));
    EXPECT_TRUE(ptr.tag_bit(0));

    ptr.clear_tag_bit(0); // Now 1110 (14)
    EXPECT_EQ(ptr.tag(), 14);
    EXPECT_FALSE(ptr.tag_bit(0));

    // Clear all tags
    ptr.clear_tag();
    EXPECT_EQ(ptr.tag(), 0);
    EXPECT_EQ(ptr.get(), &item);
}

TEST(TaggedPtrTest, NonOwning_Modifiers) {
    AlignedItem item1(1);
    AlignedItem item2(2);
    urlicht::tagged_ptr ptr(&item1);
    ptr.set_tag(5);

    // 1. Exchange Ptr (keep tag)
    auto* old = ptr.exchange_ptr(&item2);
    EXPECT_EQ(old, &item1);
    EXPECT_EQ(ptr.get(), &item2);
    EXPECT_EQ(ptr.tag(), 5);

    // 2. Reset (change ptr, clears tag)
    ptr.reset(&item1);
    EXPECT_EQ(ptr.get(), &item1);
    EXPECT_EQ(ptr.tag(), 0);

    // 3. Reset with tag
    ptr.reset(&item2, 3);
    EXPECT_EQ(ptr.get(), &item2);
    EXPECT_EQ(ptr.tag(), 3);

    auto* released = ptr.release_ptr();
    EXPECT_EQ(ptr.get(), nullptr);
    EXPECT_EQ(released, &item2);
    EXPECT_EQ(ptr.tag(), 3);    // unchanged
}

// -----------------------------------------------------------------------------
// 3. Owning Tests
// -----------------------------------------------------------------------------

TEST(TaggedPtrTest, Owning_Lifecycle) {
    AlignedItem::reset_counts();
    {
        auto ptr = urlicht::make_tagged<AlignedItem, true>(10);
        EXPECT_TRUE(ptr.is_owning());
        EXPECT_EQ(AlignedItem::construction_count, 1);
        EXPECT_EQ(AlignedItem::destruction_count, 0);

        ptr.set_tag(12);
        EXPECT_EQ(ptr->value, 10);
    }

    EXPECT_EQ(AlignedItem::destruction_count, 1);
}

TEST(TaggedPtrTest, Owning_MoveSemantics) {
    AlignedItem::reset_counts();

    // Move Constructor
    {
        auto ptr1 = urlicht::make_tagged<AlignedItem, true>(50);
        ptr1.set_tag(5);

        auto ptr2 = std::move(ptr1);

        EXPECT_EQ(ptr2->value, 50);
        EXPECT_EQ(ptr2.tag(), 5);
        EXPECT_EQ(ptr1.get(), nullptr); // Source should be null
    }
    EXPECT_EQ(AlignedItem::destruction_count, 1);

    AlignedItem::reset_counts();

    // Move Assignment
    {
        auto ptr1 = urlicht::make_tagged<AlignedItem, true>(100);
        auto ptr2 = urlicht::make_tagged<AlignedItem, true>(200);

        ptr1 = std::move(ptr2); // should destroy 100 here

        EXPECT_EQ(ptr1->value, 200);
        EXPECT_EQ(ptr2.get(), nullptr);
        EXPECT_EQ(AlignedItem::destruction_count, 1);
    }
    EXPECT_EQ(AlignedItem::destruction_count, 2); // ptr1(200) also died
}

TEST(TaggedPtrTest, Owning_ResetAndRelease) {
    AlignedItem::reset_counts();

    // Release
    {
        auto ptr = urlicht::make_tagged<AlignedItem, true>(1);
        ptr.set_tag(2);

        AlignedItem* raw = ptr.release_ptr();
        EXPECT_EQ(ptr.get(), nullptr);
        EXPECT_EQ(ptr.tag(), 2); // Tag remains after release_ptr

        delete raw; // Manual delete
    }
    EXPECT_EQ(AlignedItem::destruction_count, 1);

    // Reset
    AlignedItem::reset_counts();
    {
        auto ptr = urlicht::make_tagged<AlignedItem, true>(1);
        // Reset with new pointer
        ptr.reset(new AlignedItem(2));
        EXPECT_EQ(AlignedItem::destruction_count, 1); // First item deleted
        EXPECT_EQ(ptr->value, 2);
    }
    EXPECT_EQ(AlignedItem::destruction_count, 2);
}

// -----------------------------------------------------------------------------
// 4. Array Support
// -----------------------------------------------------------------------------

TEST(TaggedPtrTest, ArraySupport) {
    AlignedItem::reset_counts();
    {
        // make_tagged for arrays
        auto ptr = urlicht::make_tagged<AlignedItem[], true>(3); // size 3

        EXPECT_EQ(AlignedItem::construction_count, 3);
        for (int i = 0; i < 3; ++i) {
            EXPECT_EQ(ptr[i].value, 0);
        }

        ptr[0].value = 10;
        ptr[1].value = 20;

        EXPECT_EQ(ptr[0].value, 10);
        EXPECT_EQ(ptr[1].value, 20);

        ptr.set_tag(7);
        EXPECT_EQ(ptr.tag(), 7);
    }
    EXPECT_EQ(AlignedItem::destruction_count, 3);
}

// -----------------------------------------------------------------------------
// 5. Comparison and Utilities
// -----------------------------------------------------------------------------

TEST(TaggedPtrTest, Comparisons) {
    AlignedItem a, b;
    urlicht::tagged_ptr p1(&a);
    urlicht::tagged_ptr p2(&a);
    urlicht::tagged_ptr p3(&b);

    // Ignores tag when comparing to raw pointer
    p1.set_tag(5);
    EXPECT_TRUE(p1 == &a);
    EXPECT_FALSE(p1 == &b);

    // Equality between tagged_ptrs includes the tag
    EXPECT_FALSE(p1 == p2);
    p2.set_tag(5);
    EXPECT_TRUE(p1 == p2);

    // Ordering
    EXPECT_GT(p1, p3);
    EXPECT_LT(p3, p1);
}

TEST(TaggedPtrTest, Swap) {
    AlignedItem a(1), b(2);
    urlicht::tagged_ptr p1(&a);
    p1.set_tag(1);
    urlicht::tagged_ptr p2(&b);
    p2.set_tag(2);

    using std::swap;
    swap(p1, p2);

    EXPECT_EQ(p1.get(), &b);
    EXPECT_EQ(p1.tag(), 2);
    EXPECT_EQ(p2.get(), &a);
    EXPECT_EQ(p2.tag(), 1);
}

// -----------------------------------------------------------------------------
// 6. STL Integration
// -----------------------------------------------------------------------------

TEST(TaggedPtrTest, StructuralBindings) {
    AlignedItem item;
    urlicht::tagged_ptr ptr(&item);
    ptr.set_tag(9);

    auto [p, t] = ptr;
    EXPECT_EQ(p, &item);
    EXPECT_EQ(t, 9);
}

TEST(TaggedPtrTest, Hashing) {
    AlignedItem item;
    urlicht::tagged_ptr ptr(&item);
    ptr.set_tag(3);

    std::hash<urlicht::tagged_ptr<AlignedItem, false>> hasher;
    size_t h1 = hasher(ptr);

    ptr.set_tag(4);
    size_t h2 = hasher(ptr);

    EXPECT_NE(h1, h2);
}

TEST(TaggedPtrTest, Formatting) {
    AlignedItem item;
    urlicht::tagged_ptr<AlignedItem, false> ptr(&item);
    ptr.set_tag(1);

    // The formatter outputs the raw uintptr_t value
    std::string s = std::format("{}", ptr);
    EXPECT_EQ(s, std::to_string(ptr.raw()));
}

TEST(TaggedPtrTest, PointerTraits) {
    using Ptr = urlicht::tagged_ptr<AlignedItem>;
    AlignedItem item;

    auto* raw = std::pointer_traits<Ptr>::to_address(Ptr(&item));
    EXPECT_EQ(raw, &item);

    auto smart = std::pointer_traits<Ptr>::pointer_to(item);
    EXPECT_EQ(smart.get(), &item);
}