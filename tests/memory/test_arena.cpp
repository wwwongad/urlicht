#include <gtest/gtest.h>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/arena_view.h>
#include <algorithm>
#include <deque>
#include <limits>
#include <list>
#include <cmath>
#include <scoped_allocator>
#include <memory_resource>

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
    [[maybe_unused]] auto* pt = arena1.allocate(16);
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
    [[maybe_unused]] auto* ptr1 = static_cast<char*>(arena.allocate(512, 1));
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

