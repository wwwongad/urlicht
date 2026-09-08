#include <gtest/gtest.h>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/resource_view.h>
#include <urlicht/concepts/concepts.h>
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

static_assert(urlicht::concepts::allocator<urlicht::memory::resource_view<int>>);

TEST(ResourceView, BasicAllocation) {

    auto test_alloc = [&](size_t size) {
        urlicht::memory::arena arena(65536);
        urlicht::memory::resource_view<int> view(arena);
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
    urlicht::memory::arena arena(1024 * sizeof(int));
    urlicht::memory::resource_view<int> view(arena);

    auto* ptr = view.allocate(2048);
    EXPECT_NE(ptr, nullptr);
    auto& init = arena.get_initial_buffer();
    EXPECT_EQ(init.end(), init.curr);  // Unaltered

    auto* chunk = arena.get_chunk_footer();
    EXPECT_NE(chunk, nullptr);
    EXPECT_GE(chunk->actual_buffer_size(), 2048 * sizeof(int));
}

TEST(ResourceView, AllocateAtLeast) {
    urlicht::memory::arena<{.use_upstream = false}> arena(256);
    urlicht::memory::resource_view<int, decltype(arena)> view(arena);

    auto& init = arena.get_initial_buffer();
    auto* before_curr = init.curr;

    constexpr size_t n = 7;
    auto [ptr, count] = view.allocate_at_least(n);

    EXPECT_NE(ptr, nullptr);
    EXPECT_TRUE(is_aligned(ptr, alignof(int)));
    EXPECT_GE(count, n);

    const auto bytes_consumed = static_cast<size_t>(before_curr - init.curr);

    EXPECT_GE(bytes_consumed, count * sizeof(int));
    EXPECT_LT(bytes_consumed, (count + 1) * sizeof(int));
}

struct large_align {
    alignas(128) int d;
};

TEST(ResourceView, MultiAlignment) {
    urlicht::memory::arena arena(65536);

    auto test_align = [&] <typename T> ([[maybe_unused]] T placeholder) {
        urlicht::memory::resource_view<T> view(arena);
        auto* ptr = view.allocate(16);
        EXPECT_NE(ptr, nullptr);
        EXPECT_TRUE(is_aligned(ptr, alignof(T)));
    };

    test_align(1);
    test_align(1.23);
    test_align(large_align{1});
}


TEST(ResourceView, WithoutUpstream) {
    urlicht::memory::arena<{.use_upstream = false}> arena(2048);
    urlicht::memory::resource_view<std::byte, decltype(arena)> view(arena);

    auto* p1 = view.allocate(512);
    EXPECT_NE(p1, nullptr);
    p1 = view.allocate(1024);
    EXPECT_NE(p1, nullptr);

    std::byte* p2{};
    EXPECT_THROW(p2 = view.allocate(4096), std::bad_alloc);
    EXPECT_EQ(p2, nullptr);
}

TEST(ResourceView, ExtremeSizes) {
    constexpr size_t large_size = 281'474'976'710'656;
    constexpr size_t max_size = std::numeric_limits<size_t>::max();

    urlicht::memory::arena<> arena;
    urlicht::memory::resource_view<int> view(arena);
    void* p{};
    EXPECT_THROW(p = view.allocate(large_size), std::bad_alloc);
    EXPECT_THROW(p = view.allocate(max_size), std::bad_array_new_length);
    EXPECT_EQ(p, nullptr);
}

TEST(ResourceView, AllocateBytes) {
    urlicht::memory::arena arena(1024);
    urlicht::memory::resource_view<std::byte> view(arena);

    void* p1 = view.allocate_bytes(48, 16);
    ASSERT_NE(p1, nullptr);
    EXPECT_TRUE(is_aligned(p1, 16));

    void* p2 = view.allocate_bytes(4096, 64);
    ASSERT_NE(p2, nullptr);
    EXPECT_TRUE(is_aligned(p2, 64));

    auto* chunk = arena.get_chunk_footer();
    EXPECT_NE(chunk, nullptr);
    EXPECT_GE(chunk->actual_buffer_size(), 4096u);
}

TEST(ResourceView, AllocateBytesOnExhaustion) {
    // Safe mode
    urlicht::memory::arena<{.use_upstream = false}> arena(128);
    urlicht::memory::resource_view<std::byte, decltype(arena)> view(arena);
    ASSERT_NE(view.allocate_bytes(128, 1), nullptr);
    EXPECT_THROW((void)view.allocate_bytes(1, 1), std::bad_alloc);

    // Unsafe mode
    urlicht::memory::arena<{.use_upstream = false}> arena2(64);
    urlicht::memory::resource_view<std::byte, decltype(arena), {.unchecked_allocate = true}> view2(arena2);
    void* p = view2.allocate_bytes(64, 1);
    EXPECT_NE(p, nullptr);
}

TEST(ResourceView, SharingResource) {
    urlicht::memory::arena arena(1 << 20);
    urlicht::memory::resource_view<int> vi(arena);
    urlicht::memory::resource_view<double> vd(arena);
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

TEST(ResourceView, STDVectorUsage) {
    urlicht::memory::arena arena(65536);
    std::vector<std::string, urlicht::memory::resource_view<std::string>> vec(arena);

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

TEST(ResourceView, NestedView) {
    urlicht::memory::arena<{.use_upstream = false}> arena(1 << 20);
    using char_resource_view = urlicht::memory::resource_view<char, decltype(arena)>;
    using string_type = std::basic_string<char, std::char_traits<char>, char_resource_view>;
    using string_resource_view = urlicht::memory::resource_view<string_type, decltype(arena)>;

    using vector_type = std::vector<
        string_type,
        std::scoped_allocator_adaptor<string_resource_view>
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

TEST(ResourceView, NodeBasedContainers) {
    urlicht::memory::arena arena;

    std::list<int, urlicht::memory::resource_view<int>> list(arena);
    for (int i = 0; i < 16; ++i) {
        list.push_back(i);
    }
    EXPECT_EQ(list.size(), 16);

    std::unordered_set<int, std::hash<int>, std::equal_to<>, urlicht::memory::resource_view<int>> set(arena);
    for (int i = 0; i < 48; ++i) {
        set.insert(i * 777 % 331);
    }
    EXPECT_EQ(set.size(), 48);

    using map_resource_view = urlicht::memory::resource_view<std::pair<const int, std::string>>;
    std::map<int, std::string, std::less<>, map_resource_view> map(arena);
    for (int i = 0; i < 1024; ++i) {
        map.emplace(i, std::to_string(i));
    }
    EXPECT_EQ(map.size(), 1024);
}

TEST(ResourceView, UnsafeMode) {
    urlicht::memory::arena<{.use_upstream = false}> arena(1 << 28);  // 256 MB
    using view_type = urlicht::memory::resource_view<int, decltype(arena),{.unchecked_allocate = true}>;
    std::vector<int, view_type> vec(arena);

    for (int i = 0; i < 1024; ++i) {
        vec.push_back(i);
    }
    EXPECT_EQ(vec.size(), 1024);

    vec.reserve(1 << 25);
    EXPECT_EQ(vec.capacity(), 1 << 25);
}

TEST(ResourceView, FrameLoopWithReset) {
    urlicht::memory::arena<{.use_upstream = false}> arena(1 << 14);
    using view_int = urlicht::memory::resource_view<int, decltype(arena), {.unchecked_allocate = true}>;
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

TEST(ResourceView, EqualityByResourcePointer) {
    urlicht::memory::arena a(4096), b(4096);
    const urlicht::memory::resource_view<int> va(a), va2(a), vb(b);
    EXPECT_TRUE(va == va2);
    EXPECT_FALSE(va == vb);
}
