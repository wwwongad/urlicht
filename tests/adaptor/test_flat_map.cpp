#include <gtest/gtest.h>
#include <urlicht/adaptor/flat_map.h>
#include <deque>
#include <list>
#include <map>
#include <random>
#include <urlicht/container/inplace_vector.h>
#include <urlicht/memory/arena_view.h>

using namespace urlicht;

template <typename Map>
void MapEquals(const Map& map,
               std::initializer_list<std::pair<typename Map::key_type, typename Map::mapped_type>> expected) {
    EXPECT_EQ(map.size(), expected.size());

    auto it = map.begin();
    auto exp = expected.begin();

    for (; exp != expected.end(); ++exp, ++it) {
        EXPECT_EQ(it->first, exp->first);
        EXPECT_EQ(it->second, exp->second);
    }
    EXPECT_EQ(it, map.end());
}

template <typename FlatMap, typename StdMap>
void CheckSameAsStdMap(const FlatMap& map, const StdMap& std_map) {
    EXPECT_EQ(map.size(), std_map.size());
    EXPECT_EQ(map.keys().size(), std_map.size());
    EXPECT_EQ(map.values().size(), std_map.size());

    auto it1 = map.begin();
    auto it2 = std_map.begin();

    for (; it2 != std_map.end(); ++it1, ++it2) {
        ASSERT_NE(it1, map.end());
        EXPECT_EQ(it1->first, it2->first);
        EXPECT_EQ(it1->second, it2->second);
    }
    EXPECT_EQ(it1, map.end());
}

class Compare {
    int key = 10;
public:
    using is_transparent = void;

    Compare(const int key_) : key(key_) {}

    int get_key() const { return key; }
    bool operator()(const auto& lhs, const auto& rhs) const {
        return lhs % key < rhs % key;
    }
};

class LinearLowerBound {
    mutable std::ptrdiff_t count;
public:
    LinearLowerBound() = default;
    LinearLowerBound(std::ptrdiff_t count_) {
        count = count_;
    }
    template <typename Cont, typename Val, typename Comp>
    auto operator()(Cont&& cont, const Val& value, Comp comp) const {
        for (auto it = cont.begin(); it != cont.end(); ++it) {
            if (!comp(*it, value)) {
                count = std::max(count, it - cont.begin());
                return it;
            }
        }
        count = std::max(count, cont.end() - cont.begin());
        return cont.end();
    }

    int get_count() const noexcept { return count; }
};

TEST(FlatMap, TypeAlias) {
    // All tparams non-default
    using map = flat_map<
        int, int, std::greater<>, inplace_vector<int, 1024>,
        inplace_vector<int, 1024>, decltype(std::ranges::lower_bound)
    >;
    static_assert(std::same_as<map::key_type, int>);
    static_assert(std::same_as<map::mapped_type, int>);
    static_assert(std::same_as<map::value_type, std::pair<const int, int>>);
    static_assert(std::same_as<map::reference, std::pair<const int&, int&>>);
    static_assert(std::same_as<map::const_reference, std::pair<const int&, const int&>>);
    static_assert(std::same_as<map::key_container_type, inplace_vector<int, 1024>>);
    static_assert(std::same_as<map::mapped_container_type, inplace_vector<int, 1024>>);
    static_assert(std::same_as<map::key_compare, std::greater<>>);
#if UL_HAS_CPP23
    static_assert(std::random_access_iterator<map::iterator>);
    static_assert(std::random_access_iterator<map::const_iterator>);
#endif
    static_assert(std::same_as<map::lower_bound_function_type, decltype(std::ranges::lower_bound)>);
}

TEST(FlatMap, DefaultConstructor) {
    flat_map<int, int> map;
    EXPECT_EQ(map.size(), 0U);
    EXPECT_TRUE(map.empty());
    EXPECT_EQ(map.begin(), map.end());
    EXPECT_EQ(map.cbegin(), map.cend());
    EXPECT_EQ(map.rbegin(), map.rend());
    EXPECT_EQ(map.crbegin(), map.crend());
}

TEST(FlatMap, ConstructWithComparator) {
    flat_map<int, int, Compare> map(100);
    EXPECT_EQ(map.size(), 0U);
    EXPECT_TRUE(map.empty());
    EXPECT_EQ(map.begin(), map.end());

    auto& cmp = map.key_comp();
    EXPECT_EQ(cmp.get_key(), 100);
    EXPECT_TRUE(cmp(1000, 5));
}

TEST(FlatMap, ConstructWithContainers) {
    std::vector<std::string> str_vec = {"3", "2", "0", "1", "4", "3"};
    std::vector int_vec = {3, 2, 0, 1, 4, 99};

    flat_map<std::string, int> map1(str_vec, int_vec);
    MapEquals(map1, {{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}});
    EXPECT_EQ(str_vec.size(), 6);  // Unchanged
    EXPECT_EQ(int_vec.size(), 6);

    flat_map<std::string, int> map2(std::move(str_vec), std::move(int_vec));
    MapEquals(map2, {{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}});
    EXPECT_TRUE(str_vec.empty());  // Moved from
    EXPECT_TRUE(int_vec.empty());
}

TEST(FlatMap, ConstructWithSortedContainers) {
    std::vector keys = {0, 1, 1, 2, 3, 3, 4};
    std::vector vals = {10, 20, 21, 30, 40, 41, 50};

    flat_map<int, int> map1(sorted, keys, vals);
    MapEquals(map1, {{0, 10}, {1, 20}, {2, 30}, {3, 40}, {4, 50}});
    EXPECT_EQ(keys.size(), 7);
    EXPECT_EQ(vals.size(), 7);

    std::vector keys2 = {0, 1, 2, 3};
    std::vector vals2 = {7, 8, 9, 10};
    flat_map<int, int> map2(sorted_unique, std::move(keys2), std::move(vals2));
    MapEquals(map2, {{0, 7}, {1, 8}, {2, 9}, {3, 10}});
    EXPECT_TRUE(keys2.empty());
    EXPECT_TRUE(vals2.empty());
}

TEST(FlatMap, ConstructWithIterators) {
    std::list<std::pair<std::string, int>> list = {
        {"3", 4}, {"1", 2}, {"4", 5}, {"2", 3}, {"5", 6}, {"0", 1}
    };
    auto snapshot = list;
    flat_map<std::string, int> map(list.begin(), list.end());
    MapEquals(map, {{"0", 1},{"1", 2},{"2", 3},{"3", 4},{"4", 5},{"5", 6}});
    EXPECT_EQ(list, snapshot);  // Unchanged

    flat_map<std::string, int> map2(std::make_move_iterator(list.begin()),
                                    std::make_move_iterator(list.end()));
    MapEquals(map2, {{"0", 1},{"1", 2},{"2", 3},{"3", 4},{"4", 5},{"5", 6}});
    for (const auto& p : list) {
        EXPECT_TRUE(p.first.empty()); // Moved from
    }
}

TEST(FlatMap, ConstructWithRange) {
    std::deque<std::pair<std::string, int>> deq = {
        {"3", 4}, {"2", 3}, {"1", 2}, {"3", 4}, {"4", 5}, {"5", 6}, {"0", 1}, {"1", 2}
    };
    auto snapshot = deq;
    flat_map<std::string, int> map(deq);
    MapEquals(map, {{"0", 1},{"1", 2},{"2", 3},{"3", 4},{"4", 5},{"5", 6}});
    EXPECT_EQ(deq, snapshot);

    std::ranges::sort(deq);
    flat_map<std::string, int> map2(sorted, deq);
    MapEquals(map, {{"0", 1},{"1", 2},{"2", 3},{"3", 4},{"4", 5},{"5", 6}});

#if UL_HAS_CPP23
    flat_map<std::string, int> map2(deq | std::ranges::views::as_rvalue);
    MapEquals(map, {{"0", 1},{"1", 2},{"2", 3},{"3", 4},{"4", 5},{"5", 6}});
    for (const auto& p : deq) {
        EXPECT_TRUE(p.first.empty()); // Moved from
    }
#endif
}

TEST(FlatMap, ConstructWithInitializerList) {
    std::array<std::pair<int, std::string>, 8> p = {
        {{3, "2"}, {4, "3"}, {5, "4"}, {1, "0"},
            {2, "1"}, {4, "3"}, {6, "5"}, {5, "4"}}
    };

    flat_map<int, std::string> map {
        p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7]
    };
    MapEquals(map, {{1, "0"}, {2, "1"}, {3, "2"}, {4, "3"}, {5, "4"}, {6, "5"}});

    std::ranges::sort(p);
    flat_map<int, std::string> map2 {
        sorted,
        {p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7]}
    };
    MapEquals(map, {{1, "0"}, {2, "1"}, {3, "2"}, {4, "3"}, {5, "4"}, {6, "5"}});
}

TEST(FlatMap, ConstructWithCustomCompAndLowerBound) {
    std::deque<std::pair<int, std::string>> deq = {
        {3, "3"}, {11, "11"}, {0, "0"}, {2, "2"}, {3, "3"}, {5, "5"}, {14, "14"}, {0, "0"}
    };
    flat_map<int, std::string, Compare, std::vector<int>, std::vector<std::string>>
        map{deq, Compare{10}};
    MapEquals(map, {{0, "0"}, {11, "11"}, {2, "2"}, {3, "3"}, {14, "14"}, {5, "5"}});
    EXPECT_EQ(map.key_comp().get_key(), 10);

    deq = {
        {8, "8"}, {9, "9"}, {9, "9"}, {4, "4"}, {5, "5"}, {13, "13"}, {13, "13"}
    };
    flat_map<int, std::string, Compare, std::vector<int>, std::vector<std::string>, LinearLowerBound>
        map2{sorted, deq, Compare{7}, LinearLowerBound{-10}};
    MapEquals(map2, {{8, "8"}, {9, "9"}, {4, "4"}, {5, "5"}, {13, "13"}});
    EXPECT_EQ(map2.key_comp().get_key(), 7);
    EXPECT_EQ(map2.lower_bound_fn().get_count(), -10);  // unused
}

TEST(FlatMap, CopyAndMoveAssignment) {
    using map_t = flat_map<int, std::string, Compare>;

    map_t src(7);
    src.emplace(8, "8");
    src.emplace(2, "2");
    map_t dst(11);
    dst.emplace(9, "9");

    dst = src;
    MapEquals(dst, {{8, "8"}, {2, "2"}});
    MapEquals(src, {{8, "8"}, {2, "2"}});
    EXPECT_EQ(dst.key_comp().get_key(), 7);

    dst = dst;
    MapEquals(dst, {{8, "8"}, {2, "2"}});

    map_t moved_to(13);
    moved_to.emplace(100, "hundred");

    moved_to = std::move(dst);
    MapEquals(moved_to, {{8, "8"}, {2, "2"}});
    EXPECT_EQ(moved_to.key_comp().get_key(), 7);
    EXPECT_TRUE(dst.empty());

    moved_to = std::move(moved_to);
    MapEquals(moved_to, {{8, "8"}, {2, "2"}});
}

TEST(FlatMap, Iterator) {
    flat_map<int, std::string> map = {{1, "2"}, {2, "3"}, {3, "4"}};
    int k = 1, v = 2;
    // Iteration
    EXPECT_EQ(map.end() - map.begin(), 3);
    for (auto it = map.begin(); it != map.end(); ++it) {
        EXPECT_EQ(it->first, k++);
        EXPECT_EQ(it->second, std::to_string(v++));
        EXPECT_EQ(it->second.size(), 1);
    }
    // Random access
    auto it = map.begin();
    EXPECT_EQ(it[0].first, 1);
    EXPECT_EQ(it[1].first, 2);
    EXPECT_EQ(it[2].first, 3);
    it += 2;
    EXPECT_EQ(*it, std::make_pair(3, "4"));
    it -= 1;
    EXPECT_EQ(*it, std::make_pair(2, "3"));

    // Modifying underlying element
    auto it2 = it;
    EXPECT_EQ(it, it2);
    EXPECT_EQ(*it, *it2);
    it->second = "100";
    EXPECT_EQ(*it, *it2);
}

TEST(FlatMap, ReverseIterator) {
    std::initializer_list<std::pair<int, int>> list = {{1, 2}, {2, 3}, {3, 4}};
    flat_map<int, int> map{list};
    int k = 3, v = 4;
    for (auto it = map.rbegin(); it != map.rend(); ++it) {
        EXPECT_TRUE(it->first == k--);
        EXPECT_EQ(it->second, v--);
    }

    auto it = map.rbegin();
    EXPECT_EQ(it[0].first, 3);
    EXPECT_EQ(it[1].first, 2);
    EXPECT_EQ(it[2].first, 1);
    it += 2;
    EXPECT_EQ(*it, std::make_pair(1, 2));
    --it;
    EXPECT_EQ(*it, std::make_pair(2, 3));
}

TEST(FlatMap, NthAndIndexOf) {
    flat_map<int, std::string> map = {{10, "10"}, {20, "20"}, {30, "30"}};

    EXPECT_EQ(map.nth(0)->first, 10);
    EXPECT_EQ(map.nth(1)->first, 20);
    EXPECT_EQ(map.nth(2)->first, 30);

    EXPECT_EQ(map.index_of(map.begin()), 0);
    EXPECT_EQ(map.index_of(map.begin() + 2), 2);

    // Const iterator
    const auto& cmap = map;
    EXPECT_EQ(cmap.nth(1)->second, "20");
    EXPECT_EQ(cmap.nth(1)->second.size(), 2);
    EXPECT_EQ(cmap.index_of(cmap.cbegin() + 2), 2);
}

TEST(FlatMap, LowerUpperBoundAndFind) {
    // The correctness of urlicht::lower_bound is tested separately. This test is for
    // map-specific functionalities only.
    flat_map<int, std::string> map = {
        {10, "ten"},
        {20, "twenty"},
        {30, "thirty"},
        {40, "forty"}
    };

    EXPECT_EQ(map.lower_bound(5)->first, 10);
    EXPECT_EQ(map.lower_bound(10)->first, 10);
    EXPECT_EQ(map.lower_bound(25)->first, 30);
    EXPECT_EQ(map.lower_bound(40)->first, 40);
    EXPECT_EQ(map.lower_bound(50), map.end());

    EXPECT_EQ(map.upper_bound(5)->first, 10);
    EXPECT_EQ(map.upper_bound(10)->first, 20);
    EXPECT_EQ(map.upper_bound(25)->first, 30);
    EXPECT_EQ(map.upper_bound(40), map.end());
    EXPECT_EQ(map.upper_bound(50), map.end());

    EXPECT_EQ(map.find(0), map.end());

    auto it = map.find(30);
    EXPECT_NE(it, map.end());
    EXPECT_EQ(it->second, "thirty");

    EXPECT_EQ(map.find(35), map.end());
    EXPECT_EQ(map.find(50), map.end());
}

TEST(FlatMap, HeterogeneousLookup) {
    flat_map<std::string, int> map = {
        {"B", 10}, {"C", 20}, {"D", 30}, {"FFF", 40}
    };

    EXPECT_EQ(map.lower_bound("B")->second, 10);
    EXPECT_EQ(map.find(std::string{"B"})->second, 10);
    EXPECT_EQ(map.find(std::string_view{"C"})->second, 20);
    EXPECT_EQ(map.find("D")->second, 30);
    EXPECT_EQ(map.find("FFF")->second, 40);

    EXPECT_EQ(map.lower_bound("A")->second, 10);
    EXPECT_EQ(map.find(std::string_view{"A"}), map.end());
    EXPECT_EQ(map.upper_bound("EE")->second, 40);
    EXPECT_EQ(map.find("F"), map.end());
    EXPECT_EQ(map.find("ABC"), map.end());
}

TEST(FlatMap, Emplace) {
    flat_map<int, int> map;
    auto [it, ok] = map.emplace(1, 2);
    EXPECT_TRUE(ok);
    EXPECT_EQ(it->first, 1);
    EXPECT_EQ(it->second, 2);
    EXPECT_EQ(map.size(), 1);

    auto [it2, ok2] = map.emplace(1, 3);
    EXPECT_FALSE(ok2);
    EXPECT_EQ(it2->first, 1);
    EXPECT_EQ(it2->second, 2);
    EXPECT_EQ(map.size(), 1);

    using rng = std::vector<std::pair<int, int>>;
    rng to_emplace = {{3, 1}, {4, 2}, {0, 3}, {-4, 5}, {10, 1}, {-4, 3}, {3, 10}};

    for (auto [k, v] : to_emplace) {
        auto [it, _] = map.emplace(k, v);
        EXPECT_EQ(it->first, k);
    }
    MapEquals(map, {{-4, 5}, {0, 3}, {1, 2}, {3, 1}, {4, 2}, {10, 1}});
}

TEST(FlatMap, TryEmplace) {
    flat_map<std::string, std::string> map;
    std::string k1 = "Key1", v1 = "Value1";
    auto [it, ok] = map.try_emplace(k1, std::move(v1));
    EXPECT_TRUE(ok);
    EXPECT_FALSE(k1.empty());
    EXPECT_TRUE(v1.empty());  // Moved from

    std::string k2 = "Key1", v2 = "Value2";
    auto [it2, ok2] = map.try_emplace(std::move(k2), std::move(v2));
    EXPECT_FALSE(ok2);
    EXPECT_EQ(k2, "Key1");
    EXPECT_EQ(v2, "Value2");

    auto hint = map.begin();
    auto it3 = map.try_emplace(hint, "Key0", "Value0");
    EXPECT_EQ(it3->first, "Key0");

    auto it4 = map.try_emplace(map.end(), "Key2", "Value2");
    EXPECT_EQ(it4->first, "Key2");
    MapEquals(map, {{"Key0", "Value0"}, {"Key1", "Value1"}, {"Key2", "Value2"}});
}

TEST(FlatMap, SubscriptOperatorAndAt) {
    flat_map<std::string, int> map;
    map["a"] = 1;
    EXPECT_EQ(map["a"], 1);

    int& b = map["b"];
    EXPECT_EQ(b, 0);
    b = 2;

    std::string_view c = "c";
    map[c] = 3;
    MapEquals(map, {{"a", 1}, {"b", 2}, {"c", 3}});

    EXPECT_EQ(map.at("a"), 1);
    map.at("a") = 10;
    EXPECT_EQ(map.at(std::string_view{"c"}), 3);
    map.at(std::string_view{"c"}) = 30;

    const auto& cmap = map;
    EXPECT_EQ(cmap.at("b"), 2);
    EXPECT_EQ(cmap.at(std::string_view{"c"}), 30);

    EXPECT_THROW((void)map.at("f"), std::out_of_range);
    EXPECT_THROW((void)cmap.at("g"), std::out_of_range);
    MapEquals(map, {{"a", 10}, {"b", 2}, {"c", 30}});
}

TEST(FlatMap, InsertOrAssign) {
    flat_map<std::string, int> map;

    auto [it1, inserted1] = map.insert_or_assign("b", 2);
    EXPECT_TRUE(inserted1);
    EXPECT_EQ(it1->first, "b");
    EXPECT_EQ(it1->second, 2);

    auto it3 = map.insert_or_assign(map.begin(), "a", 1);
    EXPECT_EQ(it3->first, "a");
    EXPECT_EQ(it3->second, 1);

    auto it4 = map.insert_or_assign(map.end(), "c", 3);
    EXPECT_EQ(it4->first, "c");
    EXPECT_EQ(it4->second, 3);

    std::string_view key = "b";
    auto [it5, inserted5] = map.insert_or_assign(key, 200);
    EXPECT_FALSE(inserted5);
    EXPECT_EQ(it5->first, "b");
    EXPECT_EQ(it5->second, 200);

    MapEquals(map, {{"a", 1}, {"b", 200}, {"c", 3}});
}

TEST(FlatMap, InsertRange) {
    flat_map<int, int> map = {{2, 20}, {4, 40}};

    std::vector<std::pair<int, int>> unsorted = {
        {3, 30}, {1, 10}, {5, 50}, {4, 400}};
    map.insert(unsorted.begin(), unsorted.end());
    MapEquals(map, {{1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}});

    std::vector<std::pair<int, int>> sorted_input = {
        {0, 0}, {2, 200}, {6, 60}};
    map.insert(sorted, sorted_input.begin(), sorted_input.end());
    MapEquals(map, {{0, 0}, {1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}, {6, 60}});

    map.insert({{7, 70}, {6, 600}});
    MapEquals(map, {{0, 0}, {1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}, {6, 60}, {7, 70}});

    std::array<std::pair<int, int>, 3> more = {{{8, 80}, {9, 90}, {10, 100}}};
    map.insert_range(sorted, more);
    MapEquals(map, {
        {0, 0}, {1, 10}, {2, 20}, {3, 30}, {4, 40},
        {5, 50}, {6, 60}, {7, 70}, {8, 80}, {9, 90}, {10, 100}
    });
}

TEST(FlatMap, FindWithCustomCompAndLowerBound) {
    using map_t =
        flat_map<int, std::string, Compare, std::vector<int>, std::vector<std::string>, LinearLowerBound>;
    map_t map {
        {{10, "10"}, {11, "11"}, {3, "3"}, {14, "14"}, {24, "24"}, {16, "16"}},
        Compare{9},
        LinearLowerBound{0}
    };

    map_t::iterator it;
    EXPECT_EQ(map.find(9), map.end());
    EXPECT_EQ(map.lower_bound_fn().get_count(), 0);

    EXPECT_NE(it = map.find(12), map.end());
    EXPECT_EQ(it->second, "3");
    EXPECT_EQ(map.lower_bound_fn().get_count(), 2);

    EXPECT_EQ(map.find(4), map.end());
    EXPECT_EQ(map.lower_bound_fn().get_count(), 3);

    EXPECT_NE(it = map.find(16), map.end());
    EXPECT_EQ(it->second, "16");
    EXPECT_EQ(map.lower_bound_fn().get_count(), 5);

    EXPECT_EQ(map.find(8), map.end());
    EXPECT_EQ(map.lower_bound_fn().get_count(), 6);
}

TEST(FlatMap, Erase) {
    flat_map<int, std::string> map = {
        {1, "one"}, {2, "two"}, {3, "three"}, {4, "four"}, {5, "five"}};

    const auto it = map.erase(map.begin() + 1);
    EXPECT_NE(it, map.end());
    EXPECT_EQ(it->first, 3);
    MapEquals(map, {{1, "one"}, {3, "three"}, {4, "four"}, {5, "five"}});

    const auto it2 = map.erase(map.cbegin() + 2);
    EXPECT_NE(it2, map.end());
    EXPECT_EQ(it2->first, 5);
    MapEquals(map, {{1, "one"}, {3, "three"}, {5, "five"}});

    const auto it3 = map.erase(map.begin(), map.begin() + 2);
    EXPECT_NE(it3, map.end());
    EXPECT_EQ(it3->first, 5);
    MapEquals(map, {{5, "five"}});

    EXPECT_EQ(map.erase(5), 1U);
    EXPECT_TRUE(map.empty());

    EXPECT_EQ(map.erase(42), 0U);
}

TEST(FlatMap, EraseIf) {
    flat_map<int, std::string> map = {
        {1, "one"}, {2, "two"}, {3, "three"}, {4, "four"}, {5, "five"}
    };

    auto removed = map.erase_if([](const auto& elem) {
        return elem.first % 2 == 0;
    });
    EXPECT_EQ(removed, 2U);
    MapEquals(map, {{1, "one"}, {3, "three"}, {5, "five"}});

    removed = map.erase_if([](const auto&) { return false; });
    EXPECT_EQ(removed, 0U);
    MapEquals(map, {{1, "one"}, {3, "three"}, {5, "five"}});

    removed = map.erase_if([](const auto&) { return true; });
    EXPECT_EQ(removed, 3U);
    EXPECT_TRUE(map.empty());
}

TEST(FlatMap, Extract) {
    flat_map<int, int> map;
    for (int i = 0; i < 10; ++i)
        map.emplace(i, i + 1);

    auto [keys, vals] = std::move(map).extract();
    EXPECT_TRUE(map.keys().empty());
    EXPECT_TRUE(map.values().empty());
    for (int i = 0; i < 10; ++i) {
        EXPECT_EQ(keys[i], i);
        EXPECT_EQ(vals[i], i + 1);
    }
}

TEST(FlatMap, Replace) {
    flat_map<std::string, std::string> map {
        {"a", "b"}, {"c", "d"}, {"e", "f"}
    };

    std::vector<std::string> keys, values;
    for (int i = 0; i < 9; ++i) {
        keys.emplace_back(std::to_string(i));
        values.emplace_back(std::to_string(i + 1));
    }
    const auto key_temp = keys, val_temp = values;

    map.replace(std::move(keys), std::move(values));
    EXPECT_EQ(map.keys(), key_temp);
    EXPECT_EQ(map.values(), val_temp);
    EXPECT_TRUE(keys.empty());
    EXPECT_TRUE(values.empty());
}

TEST(FlatMap, Swap) {
    using map_t = flat_map<int, std::string, Compare, std::vector<int>, std::vector<std::string>, LinearLowerBound>;
    map_t map{sorted_unique, {{6, "6"}, {7, "7"}, {4, "4"}}, 5, -13};
    map_t map2{sorted_unique, {{16, "16"}, {12, "12"}}, 13, -15};

    map.swap(map2);
    MapEquals(map, {{16, "16"}, {12, "12"}});
    EXPECT_EQ(map.key_comp().get_key(), 13);
    EXPECT_EQ(map.lower_bound_fn().get_count(), -15);
    MapEquals(map2, {{6, "6"}, {7, "7"}, {4, "4"}});
    EXPECT_EQ(map2.key_comp().get_key(), 5);
    EXPECT_EQ(map2.lower_bound_fn().get_count(), -13);

    swap(map, map2);
    MapEquals(map, {{6, "6"}, {7, "7"}, {4, "4"}});
    EXPECT_EQ(map.key_comp().get_key(), 5);
    EXPECT_EQ(map.lower_bound_fn().get_count(), -13);
    MapEquals(map2, {{16, "16"}, {12, "12"}});
    EXPECT_EQ(map2.key_comp().get_key(), 13);
    EXPECT_EQ(map2.lower_bound_fn().get_count(), -15);


    swap(map, map); // Self-swap
    MapEquals(map, {{6, "6"}, {7, "7"}, {4, "4"}});
    EXPECT_EQ(map.key_comp().get_key(), 5);
    EXPECT_EQ(map.lower_bound_fn().get_count(), -13);
}

TEST(FlatMap, Clear) {
    flat_map<int, std::string> map = {{1, "one"}, {2, "two"}};
    map.clear();

    EXPECT_TRUE(map.empty());
    EXPECT_EQ(map.begin(), map.end());
    EXPECT_TRUE(map.keys().empty());
    EXPECT_TRUE(map.values().empty());
}

TEST(FlatMap, Comparison) {
    flat_map<int, int> map1 {
        {1, 3}, {2, 2}, {3, 1}, {4, 4}
    };
    auto map2 = map1;
    EXPECT_EQ(map1, map2);
    EXPECT_LE(map1, map2);
    EXPECT_GE(map1, map2);

    map1.insert_or_assign(2, 10);
    EXPECT_GT(map1, map2);

    map2 = map1;
    map1.try_emplace(0, 8);
    EXPECT_LT(map1, map2);
    map2.try_emplace(0, 6);
    EXPECT_LT(map2, map1);

    map2.clear();
    EXPECT_LT(map2, map1);
    map1.clear();
    EXPECT_EQ(map1, map2);
}

TEST(FlatMap, RandomizedStressTest) {
    std::mt19937 rng{0xC0FFEEu};
    std::uniform_int_distribution key_dist(-250, 250);
    std::uniform_int_distribution value_dist(-100000, 100000);
    std::uniform_int_distribution op_dist(0, 4);

    flat_map<int, int> map;
    std::map<int, int> ref;

    for (int step = 0; step < 20000; ++step) {
        SCOPED_TRACE(step);

        const int key = key_dist(rng);
        const int value = value_dist(rng);

        switch (op_dist(rng)) {
            case 0: {
                auto [f_it, fm_inserted] = map.insert_or_assign(key, value);
                auto [r_it, std_inserted] = ref.insert_or_assign(key, value);
                EXPECT_EQ(fm_inserted, std_inserted);
                EXPECT_EQ(f_it->first, r_it->first);
                EXPECT_EQ(f_it->second, r_it->second);
                break;
            }

            case 1: {
                auto [f_it, fm_inserted] = map.try_emplace(key, value);
                auto [r_it, std_inserted] = ref.try_emplace(key, value);
                EXPECT_EQ(fm_inserted, std_inserted);
                EXPECT_EQ(f_it->first, r_it->first);
                EXPECT_EQ(f_it->second, r_it->second);
                break;
            }

            case 2: {
                EXPECT_EQ(map.erase(key), ref.erase(key));
                break;
            }

            case 3: {
                int& mv = map[key];
                int& rv = ref[key];
                EXPECT_EQ(mv, rv);
                mv = value;
                rv = value;
                break;
            }

            case 4: {
                bool contains;
                EXPECT_EQ(contains = map.contains(key), ref.contains(key));
                auto f_it = map.find(key);
                auto r_it = ref.find(key);
                EXPECT_EQ(f_it == map.end(), r_it == ref.end());
                if (contains) {
                    EXPECT_EQ(f_it->first, r_it->first);
                    EXPECT_EQ(f_it->second, r_it->second);
                }
                break;
            }
            default: break;
        }

        if (step % 100 == 0) {
            CheckSameAsStdMap(map, ref);
        }
    }
}

