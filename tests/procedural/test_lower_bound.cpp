#include <urlicht/procedural/search/lower_bound.h>
#include <gtest/gtest.h>
#include <vector>
#include <deque>
#include <urlicht/container/inplace_vector.h>
#include <algorithm>
#include <random>

struct Aggregate {
    int id;
    std::string info;
    std::vector<int> data;
};

struct Proj {
    int operator()(const Aggregate &a) const noexcept {
        return a.id;
    }
};

class LowerBound : public ::testing::Test {
protected:
    std::vector<int> ints;
    std::vector<Aggregate> aggregates;

    LowerBound() {}
};

TEST_F(LowerBound, EmptyRange) {
    auto it = urlicht::lower_bound(ints.cbegin(), ints.cend(), 0);
    EXPECT_EQ(it, ints.end());
    it = urlicht::lower_bound(ints, 9);
    EXPECT_EQ(it, ints.end());
}

TEST_F(LowerBound, SingleSized) {
    ints.emplace_back(1);
    // val < elem
    auto it = urlicht::lower_bound(ints, -1);
    EXPECT_EQ(*it, 1);
    EXPECT_EQ(it, ints.begin());
    // val == elem
    it = urlicht::lower_bound(ints, 1);
    EXPECT_EQ(*it, 1);
    EXPECT_EQ(it, ints.begin());
    // val > elem
    it = urlicht::lower_bound(ints, 9);
    EXPECT_EQ(it, ints.end());
}

TEST_F(LowerBound, AllElemLessOrGreater) {
    auto il = {5, 6, 7, 8, 9};
    ints.assign(il);

    auto it = urlicht::lower_bound(ints, -1);
    EXPECT_EQ(*it, 5);
    EXPECT_EQ(it, ints.begin());

    it = urlicht::lower_bound(ints, 10);
    EXPECT_EQ(it, ints.end());
}

TEST_F(LowerBound, ValuePresent) {
    auto il = {5, 6, 9, 10, 21};
    ints.assign(il);

    auto it = urlicht::lower_bound(ints, 5);
    EXPECT_EQ(*it, 5);
    EXPECT_EQ(it, ints.begin());

    it = urlicht::lower_bound(ints, 9);
    EXPECT_EQ(*it, 9);
    EXPECT_EQ(it, ints.begin() + 2);

    it = urlicht::lower_bound(ints, 21);
    EXPECT_EQ(*it, 21);
    EXPECT_EQ(it, ints.end() - 1);
}

TEST_F(LowerBound, FirstOccurrence) {
    auto il = {1, 1, 2, 2, 2, 2, 2, 4};
    ints.assign(il);

    auto it = urlicht::lower_bound(ints, 2);
    EXPECT_EQ(*it, 2);
    EXPECT_EQ(it, ints.begin() + 2);
}

TEST_F(LowerBound, AbsentBetweenElem) {
    ints = {5, 5, 7, 10, 15, 15, 15};

    auto it = urlicht::lower_bound(ints, 6);
    EXPECT_EQ(*it, 7);
    EXPECT_EQ(it, ints.begin() + 2);

    for (int i = 11; i < 15; ++i) {
        it = urlicht::lower_bound(ints, i);
        EXPECT_EQ(*it, 15);
        EXPECT_EQ(it, ints.begin() + 4);
    }
}

TEST_F(LowerBound, OnSubrange) {
    ints = {0, 2, 4, 6, 8, 10, 12};
    auto begin = ints.begin() + 2; // 4
    auto end = ints.end() - 1;     // up to 10
    std::ranges::subrange sr(begin, end);

    auto it = urlicht::lower_bound(sr, 2);
    EXPECT_EQ(*it, 4);
    EXPECT_EQ(it, ints.begin() + 2);

    it = urlicht::lower_bound(sr, 5);
    EXPECT_EQ(*it, 6);
    EXPECT_EQ(it, ints.begin() + 3);

    it = urlicht::lower_bound(sr, 11);
    EXPECT_EQ(it, end);
}

TEST_F(LowerBound, CustomComparator) {
    ints = {20, 17, 17, 15, 14, 10};

    auto it = urlicht::lower_bound(ints, 17, std::greater{});
    EXPECT_EQ(*it, 17);
    EXPECT_EQ(it, ints.begin() + 1);

    it = urlicht::lower_bound(ints, 16, std::greater{});
    EXPECT_EQ(*it, 15);
    EXPECT_EQ(it, ints.begin() + 3);

    it = urlicht::lower_bound(ints, 9, std::greater{});
    EXPECT_EQ(it, ints.end());
}

TEST_F(LowerBound, CustomProjection) {
    aggregates = {
        {1, "b", {1, 2, 3}},
        {3, "a", {2, 3, 4}},
        {3, "c", {-10, -3}},
        {8, "a", {4, 2, 5}},
        {15, "f", {3, 0, 8}}
    };

    auto it = urlicht::lower_bound(aggregates, 3, std::less{}, Proj{});
    EXPECT_EQ(it->id, 3);
    EXPECT_EQ(it, aggregates.begin() + 1);

    it = urlicht::lower_bound(aggregates, 4, std::less{}, Proj{});
    EXPECT_EQ(it->id, 8);
    EXPECT_EQ(it, aggregates.begin() + 3);

    it = urlicht::lower_bound(aggregates, 16, std::less{}, Proj{});
    EXPECT_EQ(it, aggregates.end());
}

TEST_F(LowerBound, AlternativeContainers) {
    auto test_exist = [](auto& cont, auto val, size_t pos) {
        auto it = urlicht::lower_bound(cont, val);
        EXPECT_GE(*it, val);  // Only test for greater or equal here
        EXPECT_EQ(it, cont.begin() + pos);
    };

    auto test_end = [](auto& cont, auto val) {
        auto it = urlicht::lower_bound(cont, val);
        EXPECT_EQ(it, cont.end());
    };
    std::array arr = {4, 5, 5, 6, 8, 10};
    std::deque deq{3.88, 3.92, 3.92, 3.92, 12.1, 13.5};
    urlicht::inplace_vector<std::string, 10> iv{
        "a", "b", "b", "c", "e", "f"
    };
    // array
    test_exist(arr, 5, 1);
    test_exist(arr, 7, 4);
    test_end(arr, 11);
    // deque
    test_exist(deq, 3.92, 1);
    test_exist(deq, 8.1, 4);
    test_end(deq, 13.6);
    // inplace_vector
    test_exist(iv, "b", 1);
    test_exist(iv, "d", 4);
    test_end(iv, "z");
}

TEST_F(LowerBound, MatchesSTDOverRandomInputs) {
    std::mt19937 rng(0xC0FFEEu);
    std::uniform_int_distribution size_dist(0, 1024);
    std::uniform_int_distribution value_dist(-2048, 2048);

    for (int trial = 0; trial < 500; ++trial) {
        const int n = size_dist(rng);
        std::vector<int> v;
        v.reserve(static_cast<std::size_t>(n));
        for (int i = 0; i < n; ++i) {
            v.push_back(value_dist(rng));
        }
        std::ranges::sort(v);

        for (int q = -2050; q <= 2050; ++q) {
            const auto it_u = urlicht::lower_bound(v, q);
            const auto it_s = std::ranges::lower_bound(v, q);

            const auto du = std::distance(v.begin(), it_u);
            const auto ds = std::distance(v.begin(), it_s);
            EXPECT_EQ(du, ds);
        }
    }
}
