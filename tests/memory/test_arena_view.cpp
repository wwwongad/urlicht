#include <gtest/gtest.h>
#include <urlicht/memory/arena_view.h>
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

TEST(ArenaView, BasicAllocation) {

    auto test_alloc = [&](size_t size) {
        urlicht::arena arena(65536);
        urlicht::arena_view<int> view(arena);
        auto& init = arena.get_initial_buffer();
        auto* ptr = view.allocate(size);
        EXPECT_NE(ptr, nullptr);
        EXPECT_GE(init.end() - init.curr, size * sizeof(int));
    };

    test_alloc(1);
    test_alloc(16);
    test_alloc(333);
    test_alloc(16383);

    // Heap chunk fall-back
    urlicht::arena arena(1024 * sizeof(int));
    urlicht::arena_view<int> view(arena);
    auto* ptr = view.allocate(2048);
    EXPECT_NE(ptr, nullptr);
    auto& init = arena.get_initial_buffer();
    EXPECT_EQ(init.end(), init.curr);  // Unaltered
    auto* chunk = arena.get_chunk_footer();
    EXPECT_NE(chunk, nullptr);
    EXPECT_GE(chunk->actual_buffer_size(), 2048 * sizeof(int));
}

struct large_align {
    alignas(128) int d;
};

TEST(ArenaView, MultiAlignment) {
    urlicht::arena arena(65536);

    auto test_align = [&] <typename T> ([[maybe_unused]] T placeholder) {
        urlicht::arena_view<T> view(arena);
        auto* ptr = view.allocate(16);
        EXPECT_NE(ptr, nullptr);
        EXPECT_TRUE(is_aligned(ptr, alignof(T)));
    };

    test_align(1);
    test_align(1.23);
    test_align(large_align{1});
}

TEST(ArenaView, WithoutUpstream) {
    urlicht::arena<false> arena(2048);
    urlicht::arena_view<std::byte, false, decltype(arena)> view(arena);

    auto* p1 = view.allocate(512);
    EXPECT_NE(p1, nullptr);
    p1 = view.allocate(1024);
    EXPECT_NE(p1, nullptr);

    std::byte* p2{};
    EXPECT_THROW(p2 = view.allocate(4096), std::bad_alloc);
    EXPECT_EQ(p2, nullptr);
}

TEST(ArenaView, ExtremeSizes) {
    constexpr size_t large_size = 281'474'976'710'656;
    constexpr size_t max_size = std::numeric_limits<size_t>::max();

    urlicht::arena<> arena;
    urlicht::arena_view<int> view(arena);
    void* p{};
    EXPECT_THROW(p = view.allocate(large_size), std::bad_alloc);
    EXPECT_THROW(p = view.allocate(max_size), std::bad_array_new_length);
    EXPECT_EQ(p, nullptr);
}

TEST(ArenaView, SharingArena) {
    urlicht::arena arena(1 << 20);
    urlicht::arena_view<int> vi(arena);
    urlicht::arena_view<double> vd(arena);
    auto& init = arena.get_initial_buffer();

    for (int i = 1; i <= 10; ++i) {
        auto* p1 = vi.allocate(10);
        auto* p2 = vd.allocate(20);
        EXPECT_NE(p1, nullptr);
        EXPECT_NE(p2, nullptr);
        EXPECT_TRUE(is_aligned(p1, alignof(int)));
        EXPECT_TRUE(is_aligned(p2, alignof(double)));
        EXPECT_GE(init.end() - init.curr, (10 * sizeof(int) + 20 * sizeof(double)) * i);
    }
}

TEST(ArenaView, STDVectorUsage) {
    urlicht::arena arena(65536);
    std::vector<std::string, urlicht::arena_view<std::string>> vec(arena);

    for (int i = 0; i < 100; ++i) {
        vec.push_back(std::to_string(i));
    }
    EXPECT_EQ(vec.size(), 100);
    auto& init = arena.get_initial_buffer();
    EXPECT_GE(init.end() - init.curr, 100 * sizeof(std::string));

    EXPECT_NO_THROW(
        vec.reserve(100000);
    );
    EXPECT_EQ(vec.capacity(), 100000);
    EXPECT_NE(arena.get_chunk_footer(), nullptr);
}

TEST(ArenaView, NestedView) {
    urlicht::arena<false> arena(1 << 20);
    using char_arena_view = urlicht::arena_view<char, false, decltype(arena)>;
    using string_type = std::basic_string<char, std::char_traits<char>, char_arena_view>;
    using string_arena_view = urlicht::arena_view<string_type, false, decltype(arena)>;

    using vector_type = std::vector<
        string_type,
        std::scoped_allocator_adaptor<string_arena_view>
    >;

    auto& init = arena.get_initial_buffer();

    vector_type vec(10, arena);
    EXPECT_GE(init.end() - init.curr, 10 * sizeof(string_type));

    EXPECT_LT(init.end() - init.curr, 10 * sizeof(string_type) + 100);
    vec.emplace_back(100, 'a');
    EXPECT_GE(init.end() - init.curr, 10 * sizeof(string_type) + 100);

    EXPECT_LT(init.end() - init.curr, 10 * sizeof(string_type) + 1000);
    vec.emplace_back(1000, 'b');
    EXPECT_GE(init.end() - init.curr, 10 * sizeof(std::string) + 1000);
}

TEST(ArenaView, NodeBasedContainers) {
    urlicht::arena arena;

    std::list<int, urlicht::arena_view<int>> list(arena);
    for (int i = 0; i < 16; ++i) {
        list.push_back(i);
    }
    EXPECT_EQ(list.size(), 16);

    std::unordered_set<int, std::hash<int>, std::equal_to<>, urlicht::arena_view<int>> set(arena);
    for (int i = 0; i < 48; ++i) {
        set.insert(i * 777 % 331);
    }
    EXPECT_EQ(set.size(), 48);

    using map_arena_view = urlicht::arena_view<std::pair<const int, std::string>>;
    std::map<int, std::string, std::less<>, map_arena_view> map(arena);
    for (int i = 0; i < 1024; ++i) {
        map.emplace(i, std::to_string(i));
    }
    EXPECT_EQ(map.size(), 1024);
}

TEST(ArenaView, UnsafeMode) {
    urlicht::arena<false> arena(1 << 28);  // 256 MB
    using view_type = urlicht::arena_view<int, true, decltype(arena)>;
    std::vector<int, view_type> vec(arena);

    for (int i = 0; i < 1024; ++i) {
        vec.push_back(i);
    }
    EXPECT_EQ(vec.size(), 1024);

    vec.reserve(1 << 25);
    EXPECT_EQ(vec.capacity(), 1 << 25);
}

TEST(ArenaView, FrameLoopWithReset) {
    urlicht::arena<false> arena(1 << 14);
    using view_int = urlicht::arena_view<int, true, decltype(arena)>;
    for (int i = 0; i < 1 << 20; ++i) {
        {
            std::vector<int, view_int> vec(arena);
            for (int j = 0; j < 1024; ++j) {
                vec.push_back(i * j);
            }
            EXPECT_EQ(vec.size(), 1024);
        }
        arena.reset();
    }
    auto& init = arena.get_initial_buffer();
    EXPECT_EQ(init.end(), init.curr);
}

TEST(ArenaView, EqualityByArenaPointer) {
    urlicht::arena a(4096), b(4096);
    const urlicht::arena_view<int> va(a), va2(a), vb(b);
    EXPECT_TRUE(va == va2);
    EXPECT_FALSE(va == vb);
}
