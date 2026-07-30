#include <urlicht/container/d_ary_heap.h>
#include <gtest/gtest.h>
#include <urlicht/container/inplace_vector.h>
#include <urlicht/memory/arena_view.h>
#include <algorithm>
#include <array>
#include <random>
#include <forward_list>
#include <string>
#include <memory>

#include "utils/heap_checks.h"

using namespace urlicht::container;

// DAryHeapCommon test is for methods shared across heaps of all policies (i.e. the methods defined
// in d_ary_heap_base_). We repeat every test for all four possible node representations to see if the projections/
// adaptors work properly.

// Stateful comparator comparing strings by their represented numeric value
class Comp {
public:
    mutable int secret;
    Comp() = delete;
    Comp(const int n) : secret(n) {}
    Comp(const Comp&) = default;

    bool operator()(const std::string& s1, const std::string& s2) const {
        secret += s1.size() % s2.size();
        return std::stoi(s1) < std::stoi(s2);
    }

    friend bool operator==(const Comp& lhs, const Comp& rhs) {
        return lhs.secret == rhs.secret;
    }
};

std::random_device rd;
std::mt19937 gen(rd());
std::uniform_int_distribution value_dist(0, 9999);
std::uniform_int_distribution size_dist(100, 222);

std::vector<std::string> make_unique_data(const size_t n = 100) {
    std::vector<std::string> res;
    for (size_t i = 0; i < n; ++i) {
        res.push_back(std::to_string(value_dist(gen)));
    }
    std::ranges::sort(res, Comp{0});

    auto dup_rng = std::ranges::unique(res);
    res.erase(dup_rng.begin(), dup_rng.end());

    std::ranges::shuffle(res, gen);
    return res;
}

template <typename HeapType>
class DAryHeapCommon : public testing::Test {
protected:
    std::vector<std::string> str_vec;
    std::size_t size;
public:
    DAryHeapCommon() : str_vec{make_unique_data(size_dist(gen))}, size{str_vec.size()} {}
};

using heap1_t = urlicht::container::d_ary_heap<std::string, std::vector, Comp>; // plain T
using heap2_t = urlicht::container::d_ary_heap<std::string, std::vector, Comp, {.stable = true}>; // pair<T, Counter>
using heap3_t = urlicht::container::d_ary_heap<std::string, std::vector, Comp, {.mutable_ = true}>; // pair<T, Id>
using heap4_t = // pair<pair<T, Counter>, Id>
    urlicht::container::d_ary_heap<std::string, std::vector, Comp, {.mutable_ = true, .stable = true}>;

using heap_types = testing::Types<heap1_t, heap2_t, heap3_t, heap4_t>;

TYPED_TEST_SUITE(DAryHeapCommon, heap_types);

TYPED_TEST(DAryHeapCommon, DefaultConstruction) {
    TypeParam heap{10};
    EXPECT_TRUE(heap.empty());
    EXPECT_EQ(heap.size(), 0U);
    EXPECT_TRUE(heap.begin() == heap.end());
    EXPECT_TRUE(heap.rbegin() == heap.rend());
    EXPECT_THROW(void(heap.top()), std::out_of_range);

    using container_t = typename TypeParam::container_type;
    EXPECT_EQ(heap.container(), container_t{});
}

TYPED_TEST(DAryHeapCommon, ConstructFromRange) {
    TypeParam heap{this->str_vec, 10};
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, this->str_vec));

    // A different range
    std::forward_list lst(this->str_vec.begin(), this->str_vec.end());
    TypeParam heap2{lst, 0};
    EXPECT_TRUE(is_d_ary_heapified(heap2));
    EXPECT_TRUE(is_heap_of(heap2, this->str_vec));

    // Initializer list
    TypeParam heap3{{"100", "66", "299", "345", "-10"}, 18};
    EXPECT_TRUE(is_d_ary_heapified(heap3));
    EXPECT_TRUE(is_heap_of(heap3, {"100", "66", "299", "345", "-10"}));
}

TYPED_TEST(DAryHeapCommon, ConstructFromHeapifiedRange) {
    // Heapify str_vec
    std::ranges::sort(this->str_vec, Comp{0});
    std::ranges::reverse(this->str_vec);

    TypeParam heap{urlicht::heapified, this->str_vec, 0};
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, this->str_vec));

    // Initializer list
    TypeParam heap2{urlicht::heapified, {"999", "123", "456", "654", "321"}, 1};
    EXPECT_TRUE(is_d_ary_heapified(heap2));
    EXPECT_TRUE(is_heap_of(heap2, {"999", "123", "456", "654", "321"}));
}

template <typename Heap>
void all_data_member_equal(const Heap& h1, const Heap& h2) {
    EXPECT_EQ(h1.container(), h2.container());
    EXPECT_EQ(h1.value_comp(), h2.value_comp());
    if constexpr (Heap::is_stable()) {
        EXPECT_EQ(h1.counter(), h2.counter());
    } if constexpr (Heap::is_mutable()) {
        EXPECT_EQ(h1.id_to_pos_map(), h2.id_to_pos_map());
    } if constexpr (Heap::reuse_id()) {
        EXPECT_EQ(h1.free_id_pool(), h2.free_id_pool());
    } if constexpr (Heap::track_gen()) {
        EXPECT_EQ(h1.id_to_gen_map(), h2.id_to_gen_map());
    }
}

TYPED_TEST(DAryHeapCommon, CopySemantics) {
    TypeParam heap{this->str_vec, 10};

    TypeParam cpy{heap};
    EXPECT_TRUE(is_heap_of(cpy, this->str_vec));
    all_data_member_equal(heap, cpy);

    TypeParam asg{20};
    asg = heap;
    EXPECT_TRUE(is_heap_of(asg, this->str_vec));
    all_data_member_equal(heap, asg);

    // Self-assignment
    asg = asg;
    EXPECT_TRUE(is_heap_of(asg, this->str_vec));
    all_data_member_equal(heap, asg);
}

TYPED_TEST(DAryHeapCommon, MoveSemantics) {
    TypeParam orig{this->str_vec, 10};
    auto snapshot{orig};

    TypeParam heap1{std::move(orig)};
    EXPECT_TRUE(is_heap_of(heap1, this->str_vec));
    all_data_member_equal(heap1, snapshot);
    EXPECT_TRUE(orig.empty());

    TypeParam heap2{21};
    heap2 = std::move(heap1);
    EXPECT_TRUE(is_heap_of(heap2, this->str_vec));
    all_data_member_equal(heap2, snapshot);
    EXPECT_TRUE(heap1.empty());

    heap2 = std::move(heap2);
    EXPECT_TRUE(is_heap_of(heap2, this->str_vec));
    all_data_member_equal(heap2, snapshot);
}

TYPED_TEST(DAryHeapCommon, Swap) {
    TypeParam heap1{this->str_vec, 10};
    TypeParam heap2{make_unique_data(), 22};
    auto snapshot1 = heap1, snapshot2 = heap2;

    heap1.swap(heap2);
    all_data_member_equal(heap1, snapshot2);
    all_data_member_equal(heap2, snapshot1);

    swap(heap1, heap2);
    all_data_member_equal(heap1, snapshot1);
    all_data_member_equal(heap2, snapshot2);

    TypeParam{1}.swap(heap1);
    EXPECT_TRUE(heap1.empty());
    EXPECT_EQ(heap1.container().capacity(), 0U);
}

TYPED_TEST(DAryHeapCommon, Emplace) {
    TypeParam heap{17};
    for (auto& str : this->str_vec) {
        heap.emplace(str);
    }
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, this->str_vec));

    // Passing rvalue
    auto snapshot = this->str_vec;
    TypeParam heap2{11};
    for (auto& str : this->str_vec) {
        heap2.emplace(std::move(str));
    }
    EXPECT_TRUE(is_d_ary_heapified(heap2));
    EXPECT_TRUE(is_heap_of(heap2, snapshot));
    for (auto& str : this->str_vec)
        EXPECT_TRUE(str.empty());

    heap2.emplace(5, '9');  // in-place construction
    EXPECT_EQ(heap2.top(), "99999");
}

TYPED_TEST(DAryHeapCommon, PopVariants) {
    TypeParam heap{Comp{0}};
    EXPECT_FALSE(heap.try_pop());
    EXPECT_THROW(heap.pop(), std::out_of_range);

    heap.emplace("100");
    heap.emplace(5, '2');

    EXPECT_TRUE(heap.try_pop());
    EXPECT_EQ(heap.size(), 1U);
    heap.unchecked_pop();
    EXPECT_TRUE(heap.empty());
}


TYPED_TEST(DAryHeapCommon, Iterator) {
    using iter = typename TypeParam::const_iterator;
    // Projection type safety
    static_assert(std::same_as<decltype(std::declval<iter>().operator*()), typename TypeParam::const_reference>);
    static_assert(std::same_as<decltype(std::declval<iter>().operator->()), typename TypeParam::const_pointer>);
    static_assert(std::same_as<decltype(std::declval<iter>().operator[](0U)), typename TypeParam::const_reference>);

    TypeParam heap{this->str_vec, 100};
    EXPECT_EQ(heap.end() - heap.begin(), this->size);

    // STL concepts compatibility
    for (size_t idx = 0; idx < this->size; ++idx) {
        EXPECT_NE(std::ranges::find(heap.begin(), heap.end(), this->str_vec[idx]), heap.end());
        EXPECT_NE(std::ranges::find(heap.rbegin(), heap.rend(), this->str_vec[idx]), heap.rend());
    }

    EXPECT_EQ(std::ranges::find(heap.begin(), heap.end(), std::to_string(19999)), heap.end());
    EXPECT_EQ(std::ranges::find(heap.rbegin(), heap.rend(), std::to_string(29999)), heap.rend());

    std::vector<std::string> data_from_heap{heap.begin(), heap.end()};
    std::ranges::sort(this->str_vec, Comp{0});
    std::ranges::sort(data_from_heap, Comp{0});
    EXPECT_EQ(this->str_vec, data_from_heap);

    // Random access
    std::ranges::reverse(this->str_vec);  // In descending order
    TypeParam heap2{urlicht::heapified, this->str_vec, 13};

    auto it = heap2.begin();
    auto rit = heap2.rbegin();
    for (size_t idx = 0; idx < this->size; ++idx) {
        EXPECT_EQ(it[idx], this->str_vec[idx]);
        EXPECT_EQ(rit[idx], this->str_vec[this->size - idx - 1]);
    }

    // Comparison
    auto it2 = heap2.begin();
    EXPECT_EQ(it, it2); it2 += 10;
    EXPECT_LT(it, it2); it += 12;
    EXPECT_GT(it, it2);

    for (size_t idx = 0; idx < this->size; ++idx) {
        auto nth_it = heap2.nth(idx);
        EXPECT_EQ(heap2.iter_index_of(nth_it), idx);
    }
}

TYPED_TEST(DAryHeapCommon, PushRange) {
    TypeParam heap{this->str_vec, 99};

    auto additional = make_unique_data();
    this->str_vec.insert(this->str_vec.begin(), additional.begin(), additional.end());

    heap.push_range(additional, true);
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, this->str_vec));

    this->str_vec.insert(this->str_vec.begin(), {"12", "21", "38", "9"});
    heap.push_range({"12", "21", "38", "9"});
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, this->str_vec));
}

TYPED_TEST(DAryHeapCommon, ClearAndReuse) {
    TypeParam heap{this->str_vec, 8};

    heap.clear();
    EXPECT_EQ(heap.size(), 0U);
    EXPECT_TRUE(heap.empty());

    auto data = make_unique_data();
    heap.push_range(data);
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, data));
}

TYPED_TEST(DAryHeapCommon, ModifyTopVariants) {
    // k-smallest
    const size_t k = this->size / 5;
    TypeParam heap{111};

    EXPECT_FALSE(heap.try_modify_top([](auto&){}));
    EXPECT_THROW(heap.modify_top([](auto&){}), std::out_of_range);

    heap.push_range(std::ranges::subrange{this->str_vec.begin(), this->str_vec.begin() + k});

    for (size_t idx = k; idx < this->size; ++idx) {
        if (Comp{0}(this->str_vec[idx], heap.unchecked_top())) {
            heap.unchecked_modify_top(
                [&](auto& top) { top = this->str_vec[idx]; }
            );
        }
        EXPECT_TRUE(is_d_ary_heapified(heap));
    }
    std::ranges::sort(this->str_vec, Comp{0});
    std::vector<std::string> k_smallest(this->str_vec.begin(), this->str_vec.begin() + k);

    EXPECT_TRUE(is_heap_of(heap, k_smallest));
}

TYPED_TEST(DAryHeapCommon, ModifyAtVariants) {
    TypeParam heap{this->str_vec, 12};

    EXPECT_FALSE(heap.try_modify_at(this->size, [](auto&){}));
    EXPECT_THROW(heap.modify_at(this->size, [](auto&){}), std::out_of_range);

    // All elements time 2
    for (size_t idx = 0; idx < this->size; ++idx) {
        heap.unchecked_modify_at(
            idx,
            [](auto& val) { val = std::to_string(std::stoi(val) * 2); }
        );
        EXPECT_TRUE(is_d_ary_heapified(heap));
    }

    for (auto& str : this->str_vec) {
        str = std::to_string(std::stoi(str) * 2);
    }
    EXPECT_TRUE(is_heap_of(heap, this->str_vec));
}

TYPED_TEST(DAryHeapCommon, EraseAtVariants) {
    TypeParam heap{this->str_vec, 12};

    EXPECT_FALSE(heap.try_erase_at(this->size));
    EXPECT_THROW(heap.erase_at(this->size), std::out_of_range);

    for (size_t idx = 0; idx < this->size / 4; ++idx) {
        std::string elem = heap[idx];

        heap.unchecked_erase_at(idx);
        erase(this->str_vec, elem);

        EXPECT_TRUE(is_d_ary_heapified(heap));
        EXPECT_TRUE(is_heap_of(heap, this->str_vec));
    }
}

TYPED_TEST(DAryHeapCommon, ExtractAtVariants) {
    TypeParam heap{this->str_vec, 12};

    EXPECT_FALSE(heap.try_extract_at(this->size).has_value());
    EXPECT_THROW((void)heap.extract_at(this->size), std::out_of_range);

    std::vector<std::string> to_extract, res;
    std::ranges::sample(this->str_vec, std::back_inserter(to_extract), this->size / 5, std::mt19937{0xDEAD});

    for (auto& match : to_extract) {
        erase(this->str_vec, match);
        for (size_t idx = 0U; idx < heap.size(); ++idx) {
            if (heap[idx] == match) {
                res.push_back(heap.unchecked_extract_at(idx));
                break;
            }
        }
        EXPECT_TRUE(is_d_ary_heapified(heap));
        EXPECT_TRUE(is_heap_of(heap, this->str_vec));
    }

    std::ranges::sort(to_extract);
    std::ranges::sort(res);
    EXPECT_EQ(to_extract, res);

    // Extract all
    std::vector<std::string> remaining;
    heap.extract_all(std::back_inserter(remaining));

    std::ranges::sort(remaining);
    std::ranges::sort(this->str_vec);
    EXPECT_EQ(remaining, this->str_vec);
}

TYPED_TEST(DAryHeapCommon, ExtractSorted) {
    TypeParam heap{this->str_vec, 1};

    std::vector<std::string> res;
    heap.extract_sorted(std::back_inserter(res));

    EXPECT_TRUE(heap.empty());
    std::ranges::reverse(res);
    EXPECT_TRUE(std::ranges::is_sorted(res, Comp{0}));
}

TYPED_TEST(DAryHeapCommon, EqualityComparison) {
    TypeParam heap1{this->str_vec, 99};
    TypeParam heap2{this->str_vec, 0};

    EXPECT_TRUE(heap1 == heap2); // Comparator ignored
    EXPECT_TRUE(heap1 <= heap2);
    EXPECT_TRUE(heap1 >= heap2);
    EXPECT_FALSE(heap1 != heap2);
    EXPECT_FALSE(heap1 < heap2);
    EXPECT_FALSE(heap1 > heap2);

    heap1.unchecked_pop();
    EXPECT_FALSE(heap1 == heap2);
    EXPECT_TRUE(heap1 != heap2);

    heap1.clear(); heap2.clear();
    EXPECT_TRUE(heap1 == heap2);
    EXPECT_FALSE(heap1 != heap2);
}

TYPED_TEST(DAryHeapCommon, OrderComparison) {
    TypeParam greater{this->str_vec, 0};
    TypeParam lesser{this->str_vec, 0};

    lesser.unchecked_pop(); // pop max elem

    EXPECT_TRUE(greater > lesser);
    EXPECT_TRUE(lesser < greater);
    EXPECT_TRUE(greater >= lesser);
    EXPECT_TRUE(lesser <= greater);

    EXPECT_FALSE(greater < lesser);
    EXPECT_FALSE(lesser > greater);
    EXPECT_FALSE(greater <= lesser);
    EXPECT_FALSE(lesser >= greater);

    greater.clear(); lesser.clear();
    EXPECT_FALSE(greater < lesser);
    EXPECT_FALSE(greater > lesser);
}

// Tests for edge cases

TEST(DAryHeapEdgeCase, Capacity) {
    urlicht::container::priority_queue<double> pq;
    pq.reserve(10);

    EXPECT_TRUE(pq.empty());
    EXPECT_EQ(pq.size(), 0);
    EXPECT_EQ(pq.container().capacity(), 10);

    pq.emplace(12.5);
    EXPECT_FALSE(pq.empty());
    EXPECT_EQ(pq.size(), 1);
    EXPECT_EQ(pq.container().capacity(), 10);

    pq.shrink_to_fit();
    EXPECT_FALSE(pq.empty());
    EXPECT_EQ(pq.size(), 1);
    EXPECT_EQ(pq.container().capacity(), 1);
}

TEST(DAryHeapEdgeCase, MoveOnlyTypes) {
    class ptr_comp {
    public:
        bool operator()(const std::unique_ptr<int>& lhs,
                        const std::unique_ptr<int>& rhs) const {
            return *lhs < *rhs;
        }
    };
    using pq_t =
        urlicht::container::priority_queue<std::unique_ptr<int>, std::vector<std::unique_ptr<int>>, ptr_comp>;

    pq_t pq;
    for (int i = 0; i < 100; ++i) {
        pq.emplace(std::make_unique<int>(i));
        EXPECT_EQ(*(pq.unchecked_top()), i);
    }

    const auto top = pq.unchecked_extract_at(0U);
    EXPECT_EQ(*top, 99);
    EXPECT_EQ(*(pq.unchecked_top()), 98);
}

TEST(DAryHeapEdgeCase, UnstableDuplicates) {
    urlicht::container::priority_queue<int> pq;
    std::vector<int> nums;

    for (int i = 0; i < 10; ++i) {
        for (int j = i; j < 100 + i; ++j) {
            pq.emplace(j);
            nums.emplace_back(j);
        }
        EXPECT_TRUE(is_d_ary_heapified(pq));
        EXPECT_TRUE(is_heap_of(pq, nums));
    }
}

template <typename Heap, typename Arena>
void expect_allocator_propagated(const Heap& heap, const Arena& expected_arena) {
    EXPECT_EQ(heap.container().get_allocator().get_arena(), expected_arena);
    if constexpr (Heap::is_mutable()) {
        EXPECT_EQ(heap.id_to_pos_map().get_allocator().get_arena(), expected_arena);
    }
    if constexpr (Heap::reuse_id()) {
        EXPECT_EQ(heap.free_id_pool().get_allocator().get_arena(), expected_arena);
    }
    if constexpr (Heap::track_gen()) {
        EXPECT_EQ(heap.id_to_gen_map().get_allocator().get_arena(), expected_arena);
    }
}

template <typename HeapType>
class DAryHeapAlloc : public testing::Test {};

template <typename T>
using arena_vec = std::vector<T, urlicht::memory::arena_view<T>>;

using alloc_heap1_t = urlicht::container::d_ary_heap<int, arena_vec>;
using alloc_heap2_t = urlicht::container::d_ary_heap<int, arena_vec, std::less<>, {.stable = true}>;
using alloc_heap3_t = urlicht::container::d_ary_heap<int, arena_vec, std::less<>, {.mutable_ = true}>;
using alloc_heap4_t = urlicht::container::d_ary_heap<int, arena_vec, std::less<>, {.mutable_ = true, .stable = true}>;

using alloc_heap_types = testing::Types<alloc_heap1_t, alloc_heap2_t, alloc_heap3_t, alloc_heap4_t>;

TYPED_TEST_SUITE(DAryHeapAlloc, alloc_heap_types);

TYPED_TEST(DAryHeapAlloc, ConstructsFromAlloc) {
    static_assert(std::uses_allocator_v<TypeParam, urlicht::memory::arena_view<int>>);

    urlicht::memory::arena<> arena{1 << 12};
    urlicht::memory::arena_view<int> alloc{arena};

    TypeParam heap{alloc};
    EXPECT_TRUE(heap.empty());
    expect_allocator_propagated(heap, arena);

    TypeParam with_range{std::array{3, 1, 4, 1, 5}, alloc};
    EXPECT_TRUE(is_d_ary_heapified(with_range));
    EXPECT_TRUE(is_heap_of(with_range, {3, 1, 4, 1, 5}));
    expect_allocator_propagated(with_range, arena);

    TypeParam with_comp{std::array{19, 2, 7, 42, 333}, std::less<>{}, alloc};
    EXPECT_TRUE(is_d_ary_heapified(with_comp));
    EXPECT_TRUE(is_heap_of(with_comp, {19, 2, 7, 42, 333}));
    expect_allocator_propagated(with_comp, arena);
}

TYPED_TEST(DAryHeapAlloc, ConstructsFromAllocHeapified) {
    urlicht::memory::arena<> arena{1 << 12};
    urlicht::memory::arena_view<int> alloc{arena};

    TypeParam heap{urlicht::heapified, std::array{9, 7, 8, 1, 3}, alloc};
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, {9, 7, 8, 1, 3}));
    expect_allocator_propagated(heap, arena);
}
