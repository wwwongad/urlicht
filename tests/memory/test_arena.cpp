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
#include <cstdint>
#include <vector>

static bool is_aligned(void* p, const size_t align) {
    return reinterpret_cast<std::uintptr_t>(p) % align == 0;
}

TEST(Arena, InitialBuffer) {
    urlicht::arena<false> arena(1048);

    auto [p1, c1] = arena.allocate(16, 1);
    auto* ptr1 = static_cast<char*>(p1);
    EXPECT_NE(ptr1, nullptr);
    EXPECT_GE(c1, 16u);

    auto [p2, c2] = arena.allocate(32, 1);
    auto* ptr2 = static_cast<char*>(p2);
    EXPECT_NE(ptr2, nullptr);
    EXPECT_GE(c2, 32u);
    EXPECT_EQ(ptr1 - ptr2, 32u); // Downward allocation

    auto [p3, c3] = arena.allocate(1001, 1);
    auto* too_big = static_cast<char*>(p3);
    EXPECT_EQ(too_big, nullptr);
    EXPECT_EQ(c3, 0u);

    auto [p4, c4] = arena.allocate(1000, 1);
    auto* just_fit = static_cast<char*>(p4);
    EXPECT_NE(just_fit, nullptr);
    EXPECT_GE(c4, 1000u);
    EXPECT_EQ(ptr2 - just_fit, 1000u);
}

TEST(Arena, ExternalBuffer) {
    std::vector<char> buffer(1024);
    std::ranges::fill(buffer, 'x');
    {
        urlicht::arena<false> arena(buffer.data(), 1024u);
        auto [p, c] = arena.allocate(16, 1);
        auto* ptr = static_cast<char*>(p);
        EXPECT_NE(ptr, nullptr);
        EXPECT_GE(c, 16u);
        EXPECT_EQ(ptr - buffer.data(), 1008u);
    }
    // External buffer is not freed
    for (char ch : buffer) {
        EXPECT_EQ(ch, 'x');
    }
}

TEST(Arena, ArbitraryAlignment) {
    urlicht::arena arena(512);

    auto [p1, c1] = arena.allocate(111, 1);
    auto* ptr1 = static_cast<char*>(p1);
    EXPECT_NE(ptr1, nullptr);
    EXPECT_GE(c1, 111u);
    EXPECT_TRUE(is_aligned(ptr1, 1));

    auto [p2, c2] = arena.allocate(133, 8);
    auto* ptr2 = static_cast<char*>(p2);
    EXPECT_NE(ptr2, nullptr);
    EXPECT_GE(c2, 133u);
    EXPECT_TRUE(is_aligned(ptr2, 8));

    auto [p3, c3] = arena.allocate(1007, 2048);
    auto* ptr3 = static_cast<char*>(p3);
    EXPECT_NE(ptr3, nullptr);
    EXPECT_GE(c3, 1007u);
    EXPECT_TRUE(is_aligned(ptr3, 2048));
}

TEST(Arena, UncheckedAlloc) {
    urlicht::arena arena(512);
    auto& init = arena.get_initial_buffer();

    auto [p1, c1] = arena.unchecked_allocate_initial(12, 1);
    auto* ptr = static_cast<char*>(p1);
    EXPECT_NE(ptr, nullptr);
    EXPECT_GE(c1, 12u);
    EXPECT_TRUE(is_aligned(ptr, 1));
    EXPECT_EQ(init.end() - init.curr, 12);

    auto [p2, c2] = arena.unchecked_allocate_initial(256, 8);
    ptr = static_cast<char*>(p2);
    EXPECT_NE(ptr, nullptr);
    EXPECT_GE(c2, 256u);
    EXPECT_TRUE(is_aligned(ptr, 8));
    EXPECT_GE(init.end() - init.curr, 256);
}

TEST(Arena, MoveOperations) {
    std::vector<std::byte> buffer(1024);
    urlicht::arena arena1(buffer.data(), 1024u);
    [[maybe_unused]] auto r1 = arena1.allocate(16);
    [[maybe_unused]] auto r2 = arena1.allocate(2048);

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

    auto [p1, c1] = arena.allocate(1, 1);
    const char* ptr = static_cast<char*>(p1);
    EXPECT_NE(ptr, nullptr);
    EXPECT_GE(c1, 1u);

    auto [p2, c2] = arena.allocate(2043, 1);
    const char* ptr2 = static_cast<char*>(p2);
    EXPECT_NE(ptr2, nullptr);
    EXPECT_GE(c2, 2043u);
    EXPECT_EQ(ptr - ptr2, 2043u);  // From the same chunk

    // Required size < default next size
    auto [p3, c3] = arena.allocate(2048, 1);
    const char* ptr3 = static_cast<char*>(p3);
    EXPECT_NE(ptr3, nullptr);
    EXPECT_GE(c3, 2048u);
    EXPECT_NE(ptr2 - ptr3, 2048u);  // From a new chunk
    auto* footer = arena.get_chunk_footer();
    EXPECT_NE(footer, nullptr);
    EXPECT_GE(footer->actual_buffer_size(), static_cast<size_t>(2048u * 1.5));

    // Required size > default next size
    auto [p4, c4] = arena.allocate(1 << 28, 1);
    const char* ptr4 = static_cast<char*>(p4);
    EXPECT_NE(ptr4, nullptr);
    EXPECT_GE(c4, static_cast<size_t>(1 << 28));
    footer = arena.get_chunk_footer();
    EXPECT_NE(footer, nullptr);
    EXPECT_GE(footer->actual_buffer_size(), static_cast<size_t>(1 << 28));
}

TEST(Arena, AllocationResultCount) {
    std::vector<std::byte> buffer(128);
    urlicht::arena<false> arena(buffer.data(), buffer.size());

    const auto& init = arena.get_initial_buffer();
    auto* before_curr = init.curr;

    constexpr size_t requested = 13;
    constexpr size_t align = 16;
    auto [ptr, count] = arena.allocate(requested, align);

    EXPECT_NE(ptr, nullptr);
    EXPECT_GT(count, requested);
    EXPECT_LT(count, requested + align);

    EXPECT_EQ(static_cast<std::byte*>(ptr), init.curr);
    EXPECT_EQ(before_curr - init.curr, static_cast<std::ptrdiff_t>(count));
}

TEST(Arena, ExtremeSizes) {
    constexpr size_t large_size = 281'474'976'710'656; // 2^48
    constexpr size_t max_size = std::numeric_limits<size_t>::max();

    urlicht::arena<false>::allocation_result res;

    urlicht::arena<false> arena1(1024);
    EXPECT_NO_THROW(res = arena1.allocate(large_size));
    EXPECT_EQ(res.ptr, nullptr);
    EXPECT_EQ(res.count, 0U);

    urlicht::arena arena2;
    EXPECT_THROW((void)arena2.allocate(large_size), std::bad_alloc);
    EXPECT_THROW((void)arena2.allocate(max_size), std::bad_array_new_length);
}

TEST(Arena, TriesInitialFirst) {
    std::vector<char> buffer(1024);
    urlicht::arena arena(buffer.data(), 1024u);

    auto [p1, c1] = arena.allocate(24, 1);
    auto* small_alloc1 = static_cast<char*>(p1);
    EXPECT_GE(c1, 24u);
    EXPECT_EQ(small_alloc1, buffer.data() + 1000);

    auto [p2, c2] = arena.allocate(2048, 1);
    auto* big_alloc = static_cast<char*>(p2);
    EXPECT_NE(big_alloc, nullptr);
    EXPECT_GE(c2, 2048u);
    EXPECT_NE(small_alloc1 - big_alloc, 2048);  // Not from initial

    auto [p3, c3] = arena.allocate(128, 1);
    auto* small_alloc2 = static_cast<char*>(p3);
    EXPECT_NE(small_alloc2, nullptr);
    EXPECT_GE(c3, 128u);
    EXPECT_EQ(small_alloc1 - small_alloc2, 128); // From initial

    for (size_t i = 0; i < 10; ++i) {
        [[maybe_unused]] auto alloc_res = arena.allocate(2048 * i, 2);
    }
    auto [p4, c4] = arena.allocate(256, 1);
    auto* small_alloc3 = static_cast<char*>(p4);
    EXPECT_NE(small_alloc3, nullptr);
    EXPECT_GE(c4, 256u);
    EXPECT_EQ(small_alloc2 - small_alloc3, 256); // Still from initial
}

TEST(Arena, ResetInitialAndHead) {
    urlicht::arena arena(1024u);
    [[maybe_unused]] auto r1 = arena.allocate(512, 1);
    auto& init = arena.get_initial_buffer();
    EXPECT_EQ(init.start + 512, init.curr); // modified

    [[maybe_unused]] auto r2 = arena.allocate(2048, 1);
    auto* chunk1 = arena.get_chunk_footer();
    EXPECT_NE(chunk1, nullptr);
    EXPECT_NE(chunk1->curr, chunk1->start + chunk1->actual_buffer_size()); // modified

    [[maybe_unused]] auto r3 = arena.allocate(65536, 1);
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
    [[maybe_unused]] auto r1 = arena.allocate(512, 1);
    auto& init = arena.get_initial_buffer();
    for (int i = 1; i < 10; ++i) {
        [[maybe_unused]] auto rr = arena.allocate(2048 * i, 1);
    }
    EXPECT_NE(arena.get_chunk_footer(), nullptr);

    arena.release();
    EXPECT_EQ(arena.get_chunk_footer(), nullptr);  // released
    EXPECT_EQ(init.curr, nullptr);

    // External buffer
    std::vector<char> buffer(1024);
    urlicht::arena arena2(buffer.data(), 1024u);
    [[maybe_unused]] auto r2 = arena2.allocate(512, 1);
    auto& init2 = arena2.get_initial_buffer();
    EXPECT_EQ(init2.curr, init2.start + 512);
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

    auto moved = std::move(arena);
    EXPECT_FALSE(moved == arena);
}
