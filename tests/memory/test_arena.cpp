#include <gtest/gtest.h>
#include <urlicht/memory/arena.h>
#include <urlicht/adaptor/detail/flat_tree_.h>
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

TEST(Arena, InitialBuffer) {
    urlicht::arena<false> arena(1048);

    auto* ptr1 = static_cast<char*>(arena.allocate(16, 1));
    EXPECT_NE(ptr1, nullptr);
    auto* ptr2 = static_cast<char*>(arena.allocate(32, 1));
    EXPECT_NE(ptr2, nullptr);
    EXPECT_EQ(ptr1 - ptr2, 32u); // Downward allocation

    auto* too_big = static_cast<char*>(arena.allocate(1001, 1));
    EXPECT_EQ(too_big, nullptr);

    auto* just_fit = static_cast<char*>(arena.allocate(1000, 1));
    EXPECT_NE(just_fit, nullptr);
    EXPECT_EQ(ptr2 - just_fit, 1000u);
}

TEST(Arena, ExternalBuffer) {
    std::vector<char> buffer(1024);
    std::ranges::fill(buffer, 'x');
    {
        urlicht::arena<false> arena(buffer.data(), 1024u);
        auto* ptr = static_cast<char*>(arena.allocate(16, 1));
        EXPECT_NE(ptr, nullptr);
        EXPECT_EQ(ptr - buffer.data(), 1008u);
    }
    // External buffer is not freed
    for (char c : buffer) {
        EXPECT_EQ(c, 'x');
    }
}

TEST(Arena, ArbitraryAlignment) {
    urlicht::arena arena(512);

    auto* ptr1 = static_cast<char*>(arena.allocate(111, 1));
    EXPECT_NE(ptr1, nullptr);
    EXPECT_TRUE(is_aligned(ptr1, 1));

    auto* ptr2 = static_cast<char*>(arena.allocate(133, 8));
    EXPECT_NE(ptr2, nullptr);
    EXPECT_TRUE(is_aligned(ptr2, 8));

    auto* ptr3 = static_cast<char*>(arena.allocate(1007, 2048));
    EXPECT_NE(ptr3, nullptr);
    EXPECT_TRUE(is_aligned(ptr3, 2048));
}

TEST(Arena, UncheckedAlloc) {
    urlicht::arena arena(512);
    auto& init = arena.get_initial_buffer();

    auto* ptr = static_cast<char*>(arena.unchecked_allocate_initial(12, 1));
    EXPECT_NE(ptr, nullptr);
    EXPECT_TRUE(is_aligned(ptr, 1));
    EXPECT_EQ(init.end() - init.curr, 12);

    ptr = static_cast<char*>(arena.unchecked_allocate_initial(256, 8));
    EXPECT_NE(ptr, nullptr);
    EXPECT_TRUE(is_aligned(ptr, 8));
    EXPECT_GE(init.end() - init.curr, 256);
}

TEST(Arena, MoveOperations) {
    std::vector<std::byte> buffer(1024);
    urlicht::arena arena1(buffer.data(), 1024u);
    auto* pt = arena1.allocate(16);
    pt = arena1.allocate(2048);

    auto arena2(std::move(arena1));
    const auto& init = arena2.get_initial_buffer();
    EXPECT_EQ(init.start, buffer.data());
    EXPECT_EQ(init.curr - init.start, 1008u);

    const auto* chunk = arena2.get_chunk_footer();
    EXPECT_NE(chunk, nullptr);
    EXPECT_GE(chunk->actual_buffer_size(), 2048u);

    const auto& moved_init = arena1.get_initial_buffer();
    EXPECT_EQ(moved_init.start, nullptr);
    EXPECT_EQ(moved_init.curr, nullptr);
    EXPECT_EQ(moved_init.size, 0u);

    const auto& moved_chunk = arena1.get_chunk_footer();
    EXPECT_EQ(moved_chunk, nullptr);
}

TEST(Arena, HeapChunkGrowth) {
    constexpr urlicht::ArenaGrowthPolicy policy{2048, 1.5};
    urlicht::arena<true, policy> arena;

    const char* ptr = static_cast<char*>(arena.allocate(1, 1));
    EXPECT_NE(ptr, nullptr);
    const char* ptr2 = static_cast<char*>(arena.allocate(2043, 1));
    EXPECT_NE(ptr2, nullptr);
    EXPECT_EQ(ptr - ptr2, 2043u);  // From the same chunk

    // Required size < default next size
    const char* ptr3 = static_cast<char*>(arena.allocate(2048, 1));
    EXPECT_NE(ptr3, nullptr);
    EXPECT_NE(ptr2 - ptr3, 2048u);  // From a new chunk
    auto* footer = arena.get_chunk_footer();
    EXPECT_NE(footer, nullptr);
    EXPECT_GE(footer->actual_buffer_size(), 2048u * 1.5);

    // Required size > default next size
    const char* ptr4 = static_cast<char*>(arena.allocate(1 << 28, 1));
    EXPECT_NE(ptr4, nullptr);
    footer = arena.get_chunk_footer();
    EXPECT_NE(footer, nullptr);
    EXPECT_GE(footer->actual_buffer_size(), 1 << 28);
}

TEST(Arena, ExtremeSizes) {
    constexpr size_t large_size = 281'474'976'710'656; // 2^48
    constexpr size_t max_size = std::numeric_limits<size_t>::max();

    urlicht::arena<false> arena1(1024);
    void* ptr{};
    EXPECT_NO_THROW(ptr = arena1.allocate(large_size, 1));
    EXPECT_EQ(ptr, nullptr);
    EXPECT_NO_THROW(ptr = arena1.allocate(max_size, 1));
    EXPECT_EQ(ptr, nullptr);

    urlicht::arena<true> arena2;
    EXPECT_THROW(ptr = arena2.allocate(large_size), std::bad_alloc);
    EXPECT_THROW(ptr = arena2.allocate(max_size), std::bad_array_new_length);
    EXPECT_EQ(ptr, nullptr);
}

TEST(Arena, TriesInitialFirst) {
    std::vector<char> buffer(1024);
    urlicht::arena arena(buffer.data(), 1024u);
    auto* small_alloc1 = static_cast<char*>(arena.allocate(24, 1));
    EXPECT_EQ(small_alloc1, buffer.data() + 1000);

    auto* big_alloc = static_cast<char*>(arena.allocate(2048, 1));
    EXPECT_NE(small_alloc1 - big_alloc, 2048);  // Not from initial
    auto* small_alloc2 = static_cast<char*>(arena.allocate(128, 1));
    EXPECT_EQ(small_alloc1 - small_alloc2, 128); // From initial

    for (size_t i = 0; i < 10; ++i) {
        [[maybe_unused]] auto* alloc = static_cast<char*>(arena.allocate(2048 * i, 2));
    }
    auto* small_alloc3 = static_cast<char*>(arena.allocate(256, 1));
    EXPECT_EQ(small_alloc2 - small_alloc3, 256); // Still from initial
}


TEST(Arena, ResetInitialAndHead) {
    urlicht::arena arena(1024u);
    auto* ptr1 = static_cast<char*>(arena.allocate(512, 1));
    auto& init = arena.get_initial_buffer();
    EXPECT_EQ(init.start + 512, init.curr); // modified

    ptr1 = static_cast<char*>(arena.allocate(2048, 1));
    auto* chunk1 = arena.get_chunk_footer();
    EXPECT_NE(chunk1, nullptr);
    EXPECT_NE(chunk1->curr, chunk1->start + chunk1->actual_buffer_size()); // modified

    ptr1 = static_cast<char*>(arena.allocate(65536, 1));
    auto* chunk2 = arena.get_chunk_footer();
    EXPECT_NE(chunk2, nullptr);
    EXPECT_NE(chunk2, chunk1);
    EXPECT_NE(chunk2->curr, chunk2->start + chunk2->actual_buffer_size());

    arena.reset();
    EXPECT_EQ(init.curr, init.start + 1024);
    EXPECT_EQ(chunk2->curr, chunk2->start + chunk2->actual_buffer_size());

    EXPECT_NE(chunk1->curr, chunk1->start + chunk1->actual_buffer_size()); // intermediate chunks not changed
}

TEST(Arena, ReleaseHeapChunksOnly) {
    urlicht::arena arena(1024);
    [[maybe_unused]] auto* ptr1 = static_cast<char*>(arena.allocate(512, 1));
    auto& init = arena.get_initial_buffer();
    for (int i = 1; i < 10; ++i) {
        [[maybe_unused]] auto* ptr = static_cast<char*>(arena.allocate(2048 * i, 1));
    }
    EXPECT_NE(arena.get_chunk_footer(), nullptr);

    arena.release();
    EXPECT_EQ(arena.get_chunk_footer(), nullptr);  // released
    EXPECT_EQ(init.curr, nullptr);

    // External buffer
    std::vector<char> buffer(1024);
    urlicht::arena arena2(buffer.data(), 1024u);
    ptr1 = static_cast<char*>(arena.allocate(512, 1));
    auto& init2 = arena2.get_initial_buffer();
    EXPECT_NE(init2.curr, init2.start + 512);
    arena2.release();
    EXPECT_EQ(init2.curr, init2.start + 1024); // Back to original position
}

TEST(Arena, Comparison) {
    urlicht::arena arena, arena2;
    EXPECT_TRUE(arena == arena);
    EXPECT_FALSE(arena == arena2);

    const auto& clref = arena;
    EXPECT_TRUE(clref == arena);
    auto&& rref = arena;
    EXPECT_TRUE(rref == arena);

    auto move = std::move(arena);
    EXPECT_FALSE(move == arena);
}

TEST(Arena, TypeSafety) {
    using default_ = urlicht::arena<>;
    using exhaustive = urlicht::arena<false>;
    using radical = urlicht::arena<true, {65536, 2.0}>;
    using redundant = urlicht::arena<false, {2048, 1.5}>;
    using recursive =
        urlicht::arena<true,
                    {2048, 1.5},
                    urlicht::arena_view<std::byte>>;
    using arena_view_default = urlicht::arena_view<int>::arena_type;
    static_assert(urlicht::is_urlicht_arena_v<default_>);
    static_assert(urlicht::is_urlicht_arena_v<exhaustive>);
    static_assert(urlicht::is_urlicht_arena_v<radical>);
    static_assert(urlicht::is_urlicht_arena_v<redundant>);
    static_assert(urlicht::is_urlicht_arena_v<recursive>);
    static_assert(urlicht::is_urlicht_arena_v<arena_view_default>);
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

TEST(ArenaView, MultiAlignment) {
    alignas(128) struct large_align {
        int d;
    };
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

TEST(ArenaResource, AllocAndAlign) {
    urlicht::pmr::arena_resource<urlicht::arena<false>> arena(65536);
    std::pmr::memory_resource* res = &arena;
    auto test_alloc = [&](size_t size, size_t align = 8u) {
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
