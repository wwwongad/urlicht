#include <gtest/gtest.h>

#include <urlicht/procedural/search/binary_search.h>
#include <urlicht/compare.h>

#include <algorithm>
#include <functional>
#include <random>
#include <ranges>
#include <vector>


TEST(BinarySearch, EmptyRange) {
    std::vector<int> v;
    EXPECT_FALSE(urlicht::binary_search(v.begin(), v.end(), 1));
    EXPECT_FALSE(urlicht::binary_search(v, 1));
}

TEST(BinarySearch, BasicAscending) {
    std::vector<int> v{1, 3, 5, 7, 9};

    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 1));
    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 5));
    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 9));

    EXPECT_FALSE(urlicht::binary_search(v.begin(), v.end(), 0));
    EXPECT_FALSE(urlicht::binary_search(v.begin(), v.end(), 2));
    EXPECT_FALSE(urlicht::binary_search(v.begin(), v.end(), 10));
}

TEST(BinarySearch, WithDuplicates) {
    std::vector<int> v{1, 2, 2, 2, 3, 5};
    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 2));
    EXPECT_FALSE(urlicht::binary_search(v.begin(), v.end(), 4));
}

TEST(BinarySearch, RangeOverload) {
    std::vector<int> v{1, 2, 4, 8, 16};
    EXPECT_TRUE(urlicht::binary_search(v, 8));
    EXPECT_FALSE(urlicht::binary_search(v, 9));
}

struct Record {
    int key;
    int payload;
};

TEST(BinarySearch, Projection) {
    std::vector<Record> v{
        {1, 10}, {2, 20}, {2, 21},
        {4, 40}, {8, 80}
    };

    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 2, urlicht::compare::less<>{}, &Record::key));
    EXPECT_TRUE(urlicht::binary_search(v, 8, urlicht::compare::less<>{}, &Record::key));
    EXPECT_FALSE(urlicht::binary_search(v, 3, urlicht::compare::less<>{}, &Record::key));
}

TEST(BinarySearch, CustomComparator) {
    std::vector<int> v{9, 7, 5, 3, 1}; // sorted with std::greater<>

    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 7, std::greater<>{}));
    EXPECT_TRUE(urlicht::binary_search(v.begin(), v.end(), 1, std::greater<>{}));
    EXPECT_FALSE(urlicht::binary_search(v.begin(), v.end(), 8, std::greater<>{}));
}

TEST(BinarySearch, MatchesSTDOverRandInputs) {
    std::mt19937 rng(987654);
    std::uniform_int_distribution size_dist(0, 1024);
    std::uniform_int_distribution value_dist(-2048, 2048);

    for (int trial = 0; trial < 500; ++trial) {
        const int n = size_dist(rng);

        std::vector<int> v;
        v.reserve(static_cast<std::size_t>(n));
        for (int i = 0; i < n; ++i) v.push_back(value_dist(rng));
        std::ranges::sort(v);

        for (int q = -2050; q <= 2050; ++q) {
            bool got = urlicht::binary_search(v.begin(), v.end(), q);
            bool exp = std::ranges::binary_search(v, q);
            EXPECT_EQ(got, exp);
        }
    }
}


