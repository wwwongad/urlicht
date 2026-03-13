#include <gtest/gtest.h>

#include <urlicht/procedural/search/upper_bound.h>
#include <urlicht/compare.h>

#include <algorithm>
#include <functional>
#include <random>
#include <type_traits>
#include <utility>
#include <vector>

TEST(UpperBound, EmptyRange) {
    std::vector<int> v;
    auto it = urlicht::upper_bound(v.begin(), v.end(), 42);
    EXPECT_EQ(it, v.end());
}

TEST(UpperBound, BasicAscending) {
    std::vector<int> v{1, 3, 5, 7, 9};

    EXPECT_EQ(urlicht::upper_bound(v.begin(), v.end(), 0), v.begin());
    EXPECT_EQ(urlicht::upper_bound(v.begin(), v.end(), 1), v.begin() + 1);
    EXPECT_EQ(urlicht::upper_bound(v.begin(), v.end(), 4), v.begin() + 2);
    EXPECT_EQ(urlicht::upper_bound(v.begin(), v.end(), 9), v.end());
    EXPECT_EQ(urlicht::upper_bound(v.begin(), v.end(), 10), v.end());
}

TEST(UpperBound, WithDuplicates) {
    std::vector<int> v{1, 2, 2, 2, 3, 5};

    auto it = urlicht::upper_bound(v.begin(), v.end(), 2);
    EXPECT_EQ(it, v.begin() + 4); // first element > 2
}

TEST(UpperBound, RangeOverload) {
    std::vector<int> v{1, 2, 2, 2, 3, 5};

    auto it = urlicht::upper_bound(v, 2);
    EXPECT_EQ(it, v.begin() + 4);
}

struct Record {
    int key;
    int payload;
};

TEST(UpperBound, Projection) {
    std::vector<Record> v{
        {1, 10}, {2, 20}, {2, 21},
        {2, 22}, {4, 40}
    };

    auto it =
        urlicht::upper_bound(v.begin(), v.end(), 2, urlicht::compare::less<>{}, &Record::key);
    EXPECT_EQ(std::distance(v.begin(), it), 4);
}

TEST(UpperBound, CustomComparator) {
    std::vector<int> v{9, 7, 7, 7, 5, 1};

    auto it = urlicht::upper_bound(v.begin(), v.end(), 7, std::greater<>{});
    EXPECT_EQ(std::distance(v.begin(), it), 4);
}

TEST(UpperBound, MatchesSTDOverRandInputs) {
    std::mt19937 rng(123456);
    std::uniform_int_distribution size_dist(0, 1024);
    std::uniform_int_distribution value_dist(-2048, 2048);

    for (int trial = 0; trial < 500; ++trial) {
        const int n = size_dist(rng);

        std::vector<int> v;
        v.reserve(static_cast<std::size_t>(n));
        for (int i = 0; i < n; ++i) v.push_back(value_dist(rng));
        std::ranges::sort(v);

        for (int q = -2050; q <= 2050; ++q) {
            auto it_u = urlicht::upper_bound(v.begin(), v.end(), q);
            auto it_s = std::ranges::upper_bound(v, q);
            EXPECT_EQ(std::distance(v.begin(), it_u), std::distance(v.begin(), it_s));
        }
    }
}
