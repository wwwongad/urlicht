#include "utils/heap_checks.h"
#include <urlicht/container/d_ary_heap.h>

template <typename Heap>
void expect_handle_index_consistent(const Heap& heap) {
    EXPECT_GE(heap.id_to_pos_map().size(), heap.size());

    for (typename Heap::size_type pos = 0; pos < heap.size(); ++pos) {
        const auto h = heap.handle_at(pos);

        EXPECT_TRUE(h.maybe_valid());
        EXPECT_TRUE(heap.is_valid_handle(h));
        EXPECT_EQ(heap.index_of(h), pos);
        EXPECT_EQ(heap.id_to_pos_map()[h.id()], pos);
        EXPECT_EQ(heap[h], heap[pos]);
        EXPECT_EQ(heap.at(h), heap[pos]);

        if constexpr (Heap::track_gen()) {
            EXPECT_EQ(heap.id_to_gen_map()[h.id()], h.gen());
        }
    }
}

// From idx to num -> num = idx * stride
std::vector<int> make_seq_data(const int base = 0, const int stride = 5) {
    std::vector<int> data;
    int num = base;
    for (int i = 0; i < 100; ++i) {
        data.push_back(num);
        num += stride;
    }
    return data;
}

template <typename HeapType>
class DAryHeapMutable : public testing::Test {
protected:
    std::vector<int> data;
    std::vector<typename HeapType::handle_type> handles;
    const size_t size = 100;
public:
    DAryHeapMutable() : data(make_seq_data()) {}
};

using mheap1_t =
    urlicht::container::d_ary_heap<
        int, std::vector, std::less<>,{.mutable_ = true, .reuse_id = false, .track_generation = false}>;

using mheap2_t =
    urlicht::container::d_ary_heap<
        int, std::vector, std::less<>,{.mutable_ = true, .reuse_id = true, .track_generation = false}>;

using mheap3_t =
    urlicht::container::d_ary_heap<int, std::vector, std::less<>,{.mutable_ = true, .reuse_id = true}>;

using mheap4_t =
    urlicht::container::d_ary_heap<int, std::vector, std::less<>,{.mutable_ = true, .stable = true}>;

using mheap_types = testing::Types<mheap1_t, mheap2_t, mheap3_t, mheap4_t>;

TYPED_TEST_SUITE(DAryHeapMutable, mheap_types);

TYPED_TEST(DAryHeapMutable, InvalidHandle) {
    TypeParam heap{};
    typename TypeParam::handle_type invalid{};

    EXPECT_FALSE(invalid.maybe_valid());
    EXPECT_FALSE(heap.is_valid_handle(invalid));
    EXPECT_FALSE(heap.try_modify(invalid, [](int&) {}));
    EXPECT_FALSE(heap.try_promote(invalid, [](int&) {}));
    EXPECT_FALSE(heap.try_demote(invalid, [](int&) {}));
    EXPECT_FALSE(heap.try_erase(invalid));
    EXPECT_FALSE(heap.try_extract(invalid).has_value());

    EXPECT_THROW((void)heap.at(invalid), std::out_of_range);
    EXPECT_THROW(heap.modify(invalid, [](int&) {}), std::out_of_range);
    EXPECT_THROW(heap.promote(invalid, [](int&) {}), std::out_of_range);
    EXPECT_THROW(heap.demote(invalid, [](int&) {}), std::out_of_range);
    EXPECT_THROW(heap.erase(invalid), std::out_of_range);
    EXPECT_THROW((void)heap.extract(invalid), std::out_of_range);
}

TYPED_TEST(DAryHeapMutable, EmplaceWithHandles) {
    TypeParam heap;

    for (int value : this->data) {
        this->handles.push_back(heap.emplace(value));
    }

    expect_handle_index_consistent(heap);
    EXPECT_TRUE(is_d_ary_heapified(heap));
    EXPECT_TRUE(is_heap_of(heap, this->data));
    EXPECT_EQ(heap.id_to_pos_map().size(), heap.size());

    for (size_t idx = 0; auto h : this->handles) {
        EXPECT_TRUE(h.maybe_valid());
        EXPECT_TRUE(heap.is_valid_handle(h));
        EXPECT_EQ(heap[h], this->data[idx++]);
    }
}

TYPED_TEST(DAryHeapMutable, PushRangeWithHandles) {
    TypeParam heap{this->data};

    auto extra = make_seq_data();
    heap.push_range_with_handles(extra, std::back_inserter(this->handles));

    expect_handle_index_consistent(heap);
    EXPECT_TRUE(is_d_ary_heapified(heap));

    for (size_t idx = 0; auto& h : this->handles) {
        EXPECT_TRUE(h.maybe_valid());
        EXPECT_TRUE(heap.is_valid_handle(h));
        EXPECT_EQ(heap[h], extra[idx++]);
    }

    this->data.insert(this->data.end(), extra.begin(), extra.end());
    EXPECT_TRUE(is_heap_of(heap, this->data));
}

TYPED_TEST(DAryHeapMutable, Modify) {
    TypeParam heap;

    auto h10 = heap.emplace(10);
    heap.emplace(40);
    auto h20 = heap.emplace(20);
    heap.emplace(30);

    heap.unchecked_modify(h10, [](int& x) { x = 50; });
    EXPECT_TRUE(heap.is_valid_handle(h10));
    EXPECT_EQ(heap[h10], 50);
    EXPECT_EQ(heap.top(), 50);
    EXPECT_TRUE(is_d_ary_heapified(heap));

    EXPECT_TRUE(heap.try_modify(h20, [](int& x) { x = 5; }));
    EXPECT_TRUE(heap.is_valid_handle(h20));
    EXPECT_EQ(heap[h20], 5);
    EXPECT_TRUE(is_d_ary_heapified(heap));

    expect_handle_index_consistent(heap);
    EXPECT_TRUE(is_heap_of(heap, {50, 40, 5, 30}));
}

TYPED_TEST(DAryHeapMutable, PromoteAndDemote) {
    TypeParam heap;

    heap.emplace(10);
    heap.emplace(40);
    auto h20 = heap.emplace(20);
    heap.emplace(30);

    heap.unchecked_promote(h20, [](int& x) { x = 60; });
    EXPECT_TRUE(heap.is_valid_handle(h20));
    EXPECT_EQ(heap[h20], 60);
    EXPECT_EQ(heap.top(), 60);
    EXPECT_TRUE(is_d_ary_heapified(heap));

    EXPECT_TRUE(heap.try_demote(h20, [](int& x) { x = 6; }));
    EXPECT_TRUE(heap.is_valid_handle(h20));
    EXPECT_EQ(heap[h20], 6);
    EXPECT_EQ(heap.top(), 40);
    EXPECT_TRUE(is_d_ary_heapified(heap));

    expect_handle_index_consistent(heap);
    EXPECT_TRUE(is_heap_of(heap, {10, 40, 6, 30}));
}

TYPED_TEST(DAryHeapMutable, Erase) {
    TypeParam heap;
    heap.push_range_with_handles(this->data, std::back_inserter(this->handles));

    for (size_t idx = 0U; auto& h : this->handles) {
        if (idx++ % 5 == 0) {
            EXPECT_TRUE(heap.try_erase(h));
            EXPECT_TRUE(is_d_ary_heapified(heap));
            EXPECT_FALSE(heap.is_valid_handle(h));

            EXPECT_TRUE(h.maybe_valid());
            h.invalidate();
            EXPECT_FALSE(h.maybe_valid());
        }
    }

    for (size_t idx = 0; auto& h : this->handles) {
        if (h.maybe_valid()) { // Here, maybe valid must be valid
            EXPECT_EQ(heap[h], this->data[idx]);
        }
        ++idx;
    }
    expect_handle_index_consistent(heap);
}

TYPED_TEST(DAryHeapMutable, Extract) {
    TypeParam heap;
    heap.push_range_with_handles(this->data, std::back_inserter(this->handles));

    for (size_t idx = 0U; auto& h : this->handles) {
        if (idx++ % 5 == 0) {
            auto extracted = heap.try_extract(h); // std::optional
            EXPECT_TRUE(extracted.has_value());
            EXPECT_EQ(extracted.value(), this->data[idx - 1]);
            EXPECT_TRUE(is_d_ary_heapified(heap));

            h.invalidate();
        }
    }

    for (size_t idx = 0; auto& h : this->handles) {
        if (h.maybe_valid()) {
            EXPECT_EQ(heap[h], this->data[idx]);
        }
        ++idx;
    }
    expect_handle_index_consistent(heap);
}

TYPED_TEST(DAryHeapMutable, Clear) {
    TypeParam heap;
    heap.push_range_with_handles(this->data, std::back_inserter(this->handles));

    heap.clear();

    EXPECT_TRUE(heap.empty());
    EXPECT_EQ(heap.size(), 0U);
    EXPECT_TRUE(heap.id_to_pos_map().empty());

    for (auto& h : this->handles) {
        EXPECT_TRUE(h.maybe_valid());
        EXPECT_FALSE(heap.is_valid_handle(h));
    }

    if constexpr (TypeParam::reuse_id()) {
        EXPECT_TRUE(heap.free_id_pool().empty());
    } if constexpr (TypeParam::track_gen()) {
        EXPECT_TRUE(heap.id_to_gen_map().empty());
    } if constexpr (TypeParam::is_stable()) {
        EXPECT_EQ(heap.counter(), 0U);
    }
}

TYPED_TEST(DAryHeapMutable, IdReuseAndGeneration) {
    TypeParam heap;

    heap.emplace(10);
    auto h2 = heap.emplace(20);
    heap.emplace(30);

    heap.erase(h2);
    EXPECT_FALSE(heap.is_valid_handle(h2));

    if constexpr (TypeParam::reuse_id()) {
        ASSERT_EQ(heap.free_id_pool().size(), 1U);
    }

    auto h4 = heap.emplace(99);
    EXPECT_TRUE(heap.is_valid_handle(h4));
    EXPECT_EQ(heap[h4], 99);

    if constexpr (TypeParam::reuse_id()) {
        EXPECT_EQ(h4.id(), h2.id());
    } else {
        EXPECT_NE(h4.id(), h2.id());
    }

    if constexpr (TypeParam::track_gen()) {
        EXPECT_FALSE(heap.is_valid_handle(h2));
        EXPECT_NE(h4.gen(), h2.gen());
        EXPECT_EQ(heap.id_to_gen_map()[h4.id()], h4.gen());
    } else if constexpr (TypeParam::reuse_id()) {
        // Without generation tracking, a stale handle may become valid again once its id is reused.
        EXPECT_TRUE(heap.is_valid_handle(h2));
        EXPECT_EQ(heap[h2], 99);
    } else {
        EXPECT_FALSE(heap.is_valid_handle(h2));
    }

    EXPECT_TRUE(is_d_ary_heapified(heap));
    expect_handle_index_consistent(heap);
    EXPECT_TRUE(is_heap_of(heap, {10, 30, 99}));
}