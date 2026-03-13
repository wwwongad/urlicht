#include <gtest/gtest.h>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/pmr/arena_resource.h>
#include <algorithm>
#include <deque>
#include <limits>
#include <list>
#include <cmath>
#include <scoped_allocator>
#include <memory_resource>
#include <unordered_set>

static bool is_aligned(void* p, const size_t align) {
    return reinterpret_cast<std::uintptr_t>(p) % align == 0;
}

TEST(ArenaResource, AllocAndAlign) {
    urlicht::pmr::arena_resource<urlicht::arena<false>> arena(65536);
    std::pmr::memory_resource* res = &arena;
    auto test_alloc = [&](const size_t size, const size_t align = 8u) {
        auto* ptr = static_cast<char*>(res->allocate(size, align));
        EXPECT_NE(ptr, nullptr);
        EXPECT_TRUE(is_aligned(ptr, align));
        arena.reset();
    };

    test_alloc(10, 8);
    test_alloc(20, 32);
    test_alloc(65535, 1);
    test_alloc(1, 65536);

    // Heap chunk fall-back
    urlicht::pmr::arena_resource<> arena2;
    std::pmr::memory_resource* res2 = &arena2;
    auto* ptr2 = static_cast<char*>(res2->allocate(10000, 1 << 20));
    EXPECT_NE(ptr2, nullptr);
    EXPECT_TRUE(is_aligned(ptr2, 1 << 20));
}

TEST(ArenaResource, SharingArena) {
    urlicht::pmr::arena_resource<> arena;
    std::vector<std::pmr::memory_resource*> vec(10, &arena);

    for (int i = 0; i < 1024; ++i) {
        for (int j = 0; j < 10; ++j) {
            auto* ptr = static_cast<char*>(vec[j]->allocate(8 * j, 1 << (j % 5)));
            EXPECT_NE(ptr, nullptr);
            EXPECT_TRUE(is_aligned(ptr, 1 << (j % 5)));
        }
    }
}


TEST(ArenaResource, ExtremeSizesAndBadAlloc) {
    constexpr size_t LARGE_SIZE = 281'474'976'710'656;
    constexpr size_t MAX_SIZE = std::numeric_limits<size_t>::max();
    urlicht::pmr::arena_resource<> arena;
    std::pmr::polymorphic_allocator<char> alloc(&arena);
    void* ptr{};
    EXPECT_THROW(ptr = alloc.allocate(LARGE_SIZE), std::bad_alloc); // From upstream
    EXPECT_THROW(ptr = alloc.allocate(MAX_SIZE), std::bad_array_new_length);
    EXPECT_EQ(ptr, nullptr);

    urlicht::pmr::arena_resource<urlicht::arena<false>> exhaustive_arena(10000);
    std::pmr::polymorphic_allocator<char> alloc2(&exhaustive_arena);
    EXPECT_THROW(ptr = alloc2.allocate(10001), std::bad_alloc); // From arena
    EXPECT_EQ(ptr, nullptr);
}

TEST(ArenaResource, PMRVector) {
    urlicht::pmr::arena_resource<> arena(65536);
    std::pmr::vector<int> vec(&arena);

    for (int i = 0; i < 1024; ++i) {
        vec.push_back(i);
    }
    EXPECT_EQ(vec.size(), 1024);

    EXPECT_NO_THROW(
        vec.reserve(1 << 20);  // Fall-back to heap chunks
    );
    EXPECT_EQ(vec.capacity(), 1 << 20);
}

TEST(ArenaResource, NestedPMR) {
    using str_type = std::pmr::string;
    urlicht::pmr::arena_resource<> arena(1 << 20);
    std::pmr::vector<str_type> vec(&arena);
    auto& init = arena.arena().get_initial_buffer();

    vec.reserve(1024);
    EXPECT_GE(init.end() - init.curr, 1024 * sizeof(str_type));
    EXPECT_LT(init.end() - init.curr, 1024 * sizeof(str_type) + 50 * 100);

    for (int i = 0; i < 50; ++i) {
        vec.emplace_back(100, 'x');
    }
    EXPECT_GE(init.end() - init.curr, 1024 * sizeof(str_type) + 50 * 100);
}

TEST(ArenaResource, PMRNodeContainers) {
    urlicht::pmr::arena_resource<> arena(1 << 20);

    std::pmr::list<int> list(&arena);
    for (int i = 0; i < 1024; ++i) {
        list.push_back(i);
    }
    EXPECT_EQ(list.size(), 1024);

    std::pmr::unordered_set<int> set(&arena);
    for (int i = 0; i < 2048; ++i) {
        set.insert(i);
    }
    EXPECT_EQ(set.size(), 2048);

    std::pmr::map<const int, std::string> map(&arena);
    for (int i = 0; i < 1024; ++i) {
        map.try_emplace(i, std::to_string(i));
    }
    EXPECT_EQ(map.size(), 1024);
}

TEST(ArenaResource, UnsafeMode) {
    urlicht::pmr::arena_resource<urlicht::arena<false>, true> unsafe_arena((1 << 18) * 4 + 3);
    std::pmr::vector<int> vec(&unsafe_arena);

    for (int i = 0; i < 1024; ++i) {
        vec.push_back(i);
    }
    EXPECT_EQ(vec.size(), 1024);

    EXPECT_NO_THROW(vec.reserve(1 << 16));
    EXPECT_GE(vec.capacity(), 1 << 16);
    unsafe_arena.reset();
    EXPECT_NO_THROW(vec.resize(1 << 18));
    EXPECT_EQ(vec.size(), 1 << 18);
}

TEST(ArenaResource, FrameLoopWithReset) {
    urlicht::pmr::arena_resource<> arena(1 << 16);

    for (int i = 0; i < 1 << 16; ++i) {
        std::pmr::vector<int> vec(&arena);
        for (int j = 0; j < 1024; ++j) {
            vec.push_back(j);
        }
        EXPECT_EQ(vec.size(), 1024);
        arena.reset();
    }
}

TEST(ArenaResource, EqualityComparison) {
    urlicht::pmr::arena_resource<> a1, a2;
    std::pmr::polymorphic_allocator<char> alloc(&a1), alloc2(&a2);
    EXPECT_NE(alloc, alloc2);

    std::pmr::polymorphic_allocator<char> alloc3(&a1);
    EXPECT_EQ(alloc, alloc3);
}