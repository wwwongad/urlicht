#include <gtest/gtest.h>
#include <urlicht/container/dense_disjoint_sets.h>
#include <urlicht/memory/arena_view.h>

using namespace urlicht::container;

TEST(DenseDisjointSets, DefaultConstruction) {
    const dense_disjoint_sets<> ds;

    EXPECT_TRUE(ds.empty());
    EXPECT_EQ(ds.size(), 0U);
    EXPECT_EQ(ds.set_count(), 0U);
    EXPECT_EQ(ds.begin(), ds.end());
    EXPECT_EQ(ds.rbegin(), ds.rend());

    using cont = dense_disjoint_sets<>::parent_container_type;
    EXPECT_EQ(ds.parents(), cont{});
}

TEST(DenseDisjointSets, InitializesWithSingletons) {
    dense_disjoint_sets<> ds(5);

    EXPECT_FALSE(ds.empty());
    EXPECT_EQ(ds.size(), 5U);
    EXPECT_EQ(ds.set_count(), 5U);

    for (uint32_t i = 0U; i < 5U; ++i) {
        EXPECT_EQ(ds.unchecked_find(i), i); // each elem is its own root
        EXPECT_TRUE(ds.is_root(i));
        for (uint32_t j = 0U; j < 5U; ++j) {
            if (i == j) {
                EXPECT_TRUE(ds.unchecked_same_set(i, j));
            } else {
                EXPECT_FALSE(ds.unchecked_same_set(i, j));
            }
        }
    }
}

template <typename T>
using arena_vec = std::vector<T, urlicht::memory::arena_view<T>>;

TEST(DenseDisjointSets, StatefulAlloc) {
    urlicht::memory::arena<> arena{1 << 16};
    urlicht::memory::arena_view<uint32_t> alloc{arena};

    dense_disjoint_sets<uint32_t, {}, arena_vec> ds(5, alloc);
    EXPECT_FALSE(ds.empty());
    EXPECT_EQ(ds.size(), 5U);
    EXPECT_EQ(ds.parents().get_allocator().get_arena(), arena);
    EXPECT_EQ(ds.metrics().get_allocator().get_arena(), arena);

    static_assert(std::uses_allocator_v<decltype(ds), urlicht::memory::arena_view<uint32_t>>);

    using small_ds_t = // No metrics container
        dense_disjoint_sets<uint16_t, {.union_by = dense_disjoint_sets_union_policy::none}, arena_vec>;

    small_ds_t ds2(100, alloc);
    EXPECT_FALSE(ds2.empty());
    EXPECT_EQ(ds2.size(), 100U);
    EXPECT_EQ(ds2.parents().get_allocator().get_arena(), arena);
}

auto make_sample_sets() {
    dense_disjoint_sets<> ds(100);
    for (size_t i = 0; i < 100; i++) {
        ds.try_unite(i, i % 7 + i % 93);
    }
    return ds;
}

TEST(DenseDisjointSets, CopySemantics) {
    const auto ds = make_sample_sets();
    const auto par = ds.parents();
    const auto met = ds.metrics();

    dense_disjoint_sets<> copy{ds};
    EXPECT_EQ(copy.set_count(), ds.set_count());
    EXPECT_EQ(copy.parents(), ds.parents());
    EXPECT_EQ(copy.metrics(), ds.metrics());

    copy.make_set();
    EXPECT_NE(copy.size(), ds.size());
    EXPECT_NE(copy.set_count(), ds.set_count());

    dense_disjoint_sets<> assigned(2);
    assigned = ds;
    EXPECT_EQ(assigned.set_count(), ds.set_count());
    EXPECT_EQ(assigned.parents(), par);
    EXPECT_EQ(assigned.metrics(), met);

    assigned = assigned;  // unchanged
    EXPECT_EQ(assigned.set_count(), ds.set_count());
    EXPECT_EQ(assigned.parents(), par);
    EXPECT_EQ(assigned.metrics(), met);
}

TEST(DenseDisjointSets, MoveSemantics) {
    auto original = make_sample_sets();
    const auto snapshot = original;

    dense_disjoint_sets<> moved{std::move(original)};
    EXPECT_EQ(moved.parents(), snapshot.parents());
    EXPECT_EQ(moved.metrics(), snapshot.metrics());
    EXPECT_TRUE(original.empty());
    EXPECT_EQ(original.set_count(), 0U);

    dense_disjoint_sets<> assigned(2);
    assigned = std::move(moved);
    EXPECT_EQ(assigned.parents(), snapshot.parents());
    EXPECT_EQ(assigned.metrics(), snapshot.metrics());
    EXPECT_TRUE(moved.empty());
    EXPECT_EQ(moved.set_count(), 0U);

    assigned = std::move(assigned);
    EXPECT_EQ(assigned.parents(), snapshot.parents());
    EXPECT_EQ(assigned.metrics(), snapshot.metrics());
}

TEST(DenseDisjointSets, UniteBasics) {
    dense_disjoint_sets<> ds(5);
    EXPECT_TRUE(ds.try_unite(1, 2));
    EXPECT_TRUE(ds.try_unite(3, 4));

    EXPECT_TRUE(ds.same_set(1, 2));
    EXPECT_TRUE(ds.same_set(3, 4));
    EXPECT_FALSE(ds.same_set(1, 3));

    EXPECT_TRUE(ds.try_unite(2, 3));
    EXPECT_TRUE(ds.same_set(1, 4));
}

TEST(DenseDisjointSets, MakeSet) {
    dense_disjoint_sets<> ds1(5);  // by size (by default)
    const auto id = ds1.make_set();
    EXPECT_EQ(id, 5);
    EXPECT_EQ(ds1.unchecked_find(id), id);
    EXPECT_EQ(ds1.metrics()[id], 1);

    dense_disjoint_sets<uint32_t, {.union_by = dense_disjoint_sets_union_policy::by_rank}> ds2;
    const auto id2 = ds2.make_set();
    EXPECT_EQ(id2, 0);
    EXPECT_EQ(ds2.unchecked_find(id2), id2);
    EXPECT_EQ(ds2.metrics()[id2], 0);
}

TEST(DenseDisjointSets, UniteBySize) {
    dense_disjoint_sets<> ds(5);

    // set 1: {0, 1, 2}
    ds.unchecked_unite(0, 1);
    ds.unchecked_unite(1, 2);

    const auto root = ds.unchecked_find(0);
    EXPECT_EQ(ds.metrics()[root], 3);

    // set 2: {3, 4}
    ds.unchecked_unite(3, 4);
    EXPECT_EQ(ds.metrics()[ds.unchecked_find(3)], 2);

    ds.unchecked_unite(2, 3); // unite the two sets
    EXPECT_EQ(ds.unchecked_find(3), root);
    EXPECT_EQ(ds.unchecked_find(4), root);
}

TEST(DenseDisjointSets, UniteByRank) {
    dense_disjoint_sets<uint32_t, {.union_by = dense_disjoint_sets_union_policy::by_rank}> ds(6);

    ds.unchecked_unite(0, 1); // rank == 1
    ds.unchecked_unite(2, 3);
    ds.unchecked_unite(1, 2);

    const auto root = ds.unchecked_find(0);
    EXPECT_EQ(ds.metrics()[root], 2);

    ds.unchecked_unite(4, 5);
    EXPECT_EQ(ds.metrics()[ds.unchecked_find(4)], 1);

    ds.unchecked_unite(3, 4);
    EXPECT_EQ(ds.unchecked_find(4), root);
    EXPECT_EQ(ds.unchecked_find(5), root);
}

TEST(DenseDisjointSets, AppendSets) {
    auto ds = make_sample_sets();
    const auto par = ds.parents();
    const auto met = ds.metrics();

    ds.append_sets(50);

    for (size_t i = 0; i < 100; ++i) {
        EXPECT_EQ(ds.parents()[i], par[i]);
        EXPECT_EQ(ds.metrics()[i], met[i]);
    }
    for (size_t i = 100; i < 150; ++i) {
        EXPECT_EQ(ds.parents()[i], i);
        EXPECT_EQ(ds.metrics()[i], 1); // by size
    }
}

TEST(DenseDisjointSets, Capacity) {
    dense_disjoint_sets<uint8_t> ds{};
    EXPECT_EQ(ds.max_size(), 256U); // uint8_t is in [0, 255]
    EXPECT_EQ(ds.max_id(), 255U);

    ds.reserve(200);
    EXPECT_TRUE(ds.empty());
    EXPECT_EQ(ds.size(), 0u);
    EXPECT_EQ(ds.capacity(), 200);

    ds.shrink_to_fit();
    EXPECT_EQ(ds.capacity(), 0U);

    EXPECT_THROW(ds.reserve(257U), std::length_error);
}

TEST(DenseDisjointSets, Swap) {
    auto lhs = make_sample_sets();
    dense_disjoint_sets<> rhs(3);
    rhs.try_unite(0, 2);

    const auto lhs_snapshot = lhs;
    const auto rhs_snapshot = rhs;

    lhs.swap(rhs);
    EXPECT_EQ(lhs, rhs_snapshot);
    EXPECT_EQ(rhs, lhs_snapshot);

    swap(lhs, rhs);
    EXPECT_EQ(lhs, lhs_snapshot);
    EXPECT_EQ(rhs, rhs_snapshot);
}

TEST(DenseDisjointSets, Clear) {
    auto ds = make_sample_sets();
    ds.reserve(16);
    const auto old_capacity = ds.capacity();

    ds.clear();

    EXPECT_TRUE(ds.empty());
    EXPECT_EQ(ds.size(), 0U);
    EXPECT_EQ(ds.set_count(), 0U);
    EXPECT_EQ(ds.parents(), dense_disjoint_sets<>::parent_container_type{});
    EXPECT_EQ(ds.metrics(), dense_disjoint_sets<>::metric_container_type{});
    EXPECT_EQ(ds.capacity(), old_capacity);
}