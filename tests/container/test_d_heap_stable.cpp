#include <gtest/gtest.h>
#include <urlicht/container/d_ary_heap.h>
#include "utils/heap_checks.h"
#include <algorithm>
#include <limits>
#include <map>
#include <random>
#include <vector>

using namespace urlicht::container;

struct stable_item {
    int val{};
    int tag{};  // For verifying stability

    friend bool operator<(const stable_item& a, const stable_item& b) {
        return a.val < b.val;
    }

    friend bool operator==(const stable_item& a, const stable_item& b) = default;
};

using stable_test_data = std::vector<stable_item>;

auto make_stable_test_data(const int size = 100, const int same_count = 3,
                           const int offset = 0, const int stride = 1) {
    stable_test_data data;
    for (int value_idx = 0; value_idx < size; ++value_idx) {
        for (int tag = 0; tag < same_count; ++tag) {
            data.emplace_back(value_idx * stride + offset, tag);
        }
    }
    return data;  // {0, 0}, {0, 1},...,{1, 0},...,{size - 1, same_count - 1}
}

auto make_expected(const int size = 100, const int same_count = 3,
                    const int offset = 0, const int stride = 1) {
    stable_test_data expected;
    for (int value_idx = size; value_idx-- > 0;) {
        for (int tag = 0; tag < same_count; ++tag) {
            expected.emplace_back(value_idx * stride + offset, tag);
        }
    }
    return expected; // {size - 1, 0}, {size - 1, 1},...,{size - 2, 0},...,{0, 0}
}

template <typename Heap>
void expect_pop_order(Heap heap, const stable_test_data& expected) {
    ASSERT_EQ(heap.size(), expected.size());

    for (const auto& [val, tag] : expected) {
        ASSERT_FALSE(heap.empty());
        auto top = heap.top();
        EXPECT_EQ(top.val, val);
        EXPECT_EQ(top.tag, tag);
        heap.pop();
    }
}

template <typename HeapType>
class DAryHeapStable : public testing::Test {};

using sheap1_t = d_ary_heap<stable_item, std::vector, std::less<>, {.stable = true}>;
using sheap2_t = urlicht::container::d_ary_heap<stable_item, std::vector, std::less<>,{.mutable_ = true, .stable = true}>;

using stable_heap_types = testing::Types<sheap1_t, sheap2_t>;

TYPED_TEST_SUITE(DAryHeapStable, stable_heap_types);

TYPED_TEST(DAryHeapStable, SequentialPush) {
    const auto data = make_stable_test_data();
    const auto expected = make_expected();

    TypeParam heap; heap.push_range(data);

    EXPECT_TRUE(is_d_ary_heapified(heap));
    expect_pop_order(heap, expected);
}

TYPED_TEST(DAryHeapStable, ReversePush) {
    const auto expected = make_expected();
    auto data = make_stable_test_data();

    std::ranges::stable_sort(
        data,
        [](const stable_item& lhs, const stable_item& rhs) {
            return lhs.val > rhs.val;
        }
    );

    TypeParam heap; heap.push_range(data);

    EXPECT_TRUE(is_d_ary_heapified(heap));
    expect_pop_order(heap, expected);
}

TYPED_TEST(DAryHeapStable, RandomPushes) {  // From Boost.Heap test
    auto push_data = make_stable_test_data();
    std::mt19937 rng{0xC0FFEE};
    std::ranges::shuffle(push_data, rng);

    TypeParam heap;
    std::unordered_map<int, std::vector<stable_item>> insertion_order;
    for (const auto& item : push_data) {
        heap.push(item);
        insertion_order[item.val].push_back(item);
    }

    EXPECT_TRUE(is_d_ary_heapified(heap));

    std::unordered_map<int, std::size_t> next_index;
    int last_value = std::numeric_limits<int>::max();
    while (!heap.empty()) {
        const auto top = heap.top();
        EXPECT_LE(top.val, last_value);

        const auto& expected_bucket = insertion_order.at(top.val);
        const auto idx = next_index[top.val]++;
        EXPECT_LT(idx, expected_bucket.size());
        EXPECT_EQ(top, expected_bucket[idx]);

        last_value = top.val;
        heap.pop();
    }
}
