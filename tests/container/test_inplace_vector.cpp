#include <urlicht/container/inplace_vector.h>
#include <urlicht/compare.h>
#include <iostream>
#include <list>
#include <gtest/gtest.h>

using namespace urlicht;

class NonDef {
    public:
    int i;
    NonDef() = delete;
    NonDef(int j) : i{j} {}
};

bool operator==(const NonDef &lhs, const NonDef &rhs) {
    return lhs.i == rhs.i;
}

template <typename Cont>
constexpr void check_size(const Cont& cont, typename Cont::size_type size) {
    EXPECT_EQ(cont.size(), size);
    EXPECT_EQ(cont.ssize(), size);
    if (size)
        EXPECT_FALSE(cont.empty());
    else
        EXPECT_TRUE(cont.empty());
    typename Cont::size_type i = 0, j = 0;
    for ( [[maybe_unused]] auto& x : cont) ++i;
    for (auto rit = cont.rbegin(); rit != cont.rend(); ++rit) ++j;
    EXPECT_EQ(i, size);
    EXPECT_EQ(j, size);
}

template <typename Cont>
constexpr void check_all_equal(const Cont& vec, typename Cont::const_reference val) {
    EXPECT_EQ(vec.front(), val);
    EXPECT_EQ(vec.back(), val);
    EXPECT_EQ(*vec.data(), val);

    for (int i = 0; auto& x : vec) {
        EXPECT_EQ(x, val);
        EXPECT_EQ(vec[i], val);
        EXPECT_EQ(vec.at(i), val);
        ++i;
    }
}

template <typename Cont>
constexpr void check_moved_from(const Cont& cont) {
    for (const auto& x : cont) {
        EXPECT_TRUE(x.empty());
    }
}

class InplaceVector : public testing::Test {
protected:
    // Use only after the corresponding constructors are tested
    inplace_vector<int, 10> _int_vec{};
    inplace_vector<std::string, 10> _str_vec{};
public:
    InplaceVector() : _int_vec(5), _str_vec(5) {}
};

TEST_F(InplaceVector, TypeAlias) {
    using IV = inplace_vector<int, 10>;

    EXPECT_TRUE((std::same_as<IV::value_type, int>));
    EXPECT_TRUE((std::same_as<IV::size_type, uint8_t>));
    EXPECT_TRUE((std::same_as<IV::difference_type, std::ptrdiff_t>));
    EXPECT_TRUE((std::same_as<IV::reference, int&>));
    EXPECT_TRUE((std::same_as<IV::const_reference, const int&>));
    EXPECT_TRUE((std::same_as<IV::pointer, int*>));
    EXPECT_TRUE((std::same_as<IV::const_pointer, const int*>));
    EXPECT_TRUE((std::same_as<IV::iterator, int*>));
    EXPECT_TRUE((std::same_as<IV::const_iterator, const int*>));
    EXPECT_TRUE((std::same_as<IV::reverse_iterator, std::reverse_iterator<int*>>));
    EXPECT_TRUE((std::same_as<IV::const_reverse_iterator, std::reverse_iterator<const int*>>));

    // Adaptive size_type
    EXPECT_TRUE((std::same_as<inplace_vector<int, 256>::size_type, uint16_t>));
    EXPECT_TRUE((std::same_as<inplace_vector<int, 65536>::size_type, uint32_t>));
    EXPECT_TRUE((std::same_as<inplace_vector<int, 4'294'967'296>::size_type, uint64_t>));
}

TEST_F(InplaceVector, StaticData) {
    using IV = inplace_vector<int, 10>;

    EXPECT_EQ(IV::max_size(), 10u);
    EXPECT_EQ(IV::capacity(), 10u);
    EXPECT_NO_THROW(IV::reserve(10));
    EXPECT_THROW(IV::reserve(11), std::bad_alloc);
    EXPECT_NO_THROW(IV::shrink_to_fit());

}

TEST_F(InplaceVector, DefaultConstruction) {
    inplace_vector<int, 10> vec;
    check_size(vec, 0);
    EXPECT_THROW([[maybe_unused]] auto x = vec.at(0), std::out_of_range);
}

TEST_F(InplaceVector, SizeConstruction) {
    inplace_vector<int, 10> vec(5);
    check_size(vec, 5);
    check_all_equal(vec, 0);
    EXPECT_THROW([[maybe_unused]] auto x = vec.at(5), std::out_of_range);

    // Move-only type
    inplace_vector<std::unique_ptr<int>, 10> uvec(10);
    check_size(uvec, 10);
    check_all_equal(uvec, nullptr);

    // Bad alloc
    auto throwing = [] { return inplace_vector<int, 10>(12); };
    EXPECT_THROW(throwing(), std::bad_alloc);

    // Non default-constructible value type
    static_assert(!std::constructible_from<inplace_vector<NonDef, 10>, int>);
}

TEST_F(InplaceVector, SizeValueConstruction) {
    inplace_vector<int, 10> vec(5, 12);
    check_size(vec, 5);
    check_all_equal(vec, 12);

    // Bad alloc
    auto throwing = [] { return inplace_vector<int, 10>(12, 9); };
    EXPECT_THROW(throwing(), std::bad_alloc);

    // Non copy-constructible value type
    static_assert(!std::constructible_from<inplace_vector<std::unique_ptr<int>, 10>, int, std::unique_ptr<int>>);
}

TEST_F(InplaceVector, InitializerListConstruction) {
    const auto il = {1, 2, 3, 4, 5, 6, 7, 8};
    inplace_vector<int, 10> vec(il);
    check_size(vec, 8);
    EXPECT_TRUE(std::ranges::equal(vec, il));

    // Constructible value type
    const auto str_il = {"abc", "def", "ghi", "jhk"};
    inplace_vector<std::string, 10> str_vec(str_il);
    check_size(str_vec, 4);
    EXPECT_TRUE(std::ranges::equal(str_vec, str_il));

    // Bad alloc
    const auto large_il = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    auto throwing = [&large_il] { return inplace_vector<int, 10>(large_il); };
    EXPECT_THROW(throwing(), std::bad_alloc);
}

TEST_F(InplaceVector, IteratorConstruction) {
    // Empty
    {
        std::list<int> empty;
        inplace_vector<int, 10> vec(empty.begin(), empty.end());
        check_size(vec, 0);
    }

    // Iterators pair
    {
        std::list list(8, 1);
        inplace_vector<int, 10> v(list.begin(), list.end());
        check_size(v, 8);
        check_all_equal(v, 1);
    }

    // Bad alloc (forward iterator)
    {
        std::list list2(12, 1);
        auto throwing = [&list2] {
            return inplace_vector<int, 10>(list2.begin(), list2.end());
        };
        EXPECT_THROW(throwing(), std::bad_alloc);
    }

    // Move iterators
    {
        std::list<std::string> str_list{"abcdefg", "hijklmn", "opurstuvw"};
        auto copy = str_list;
        inplace_vector<std::string, 10> str_v(std::make_move_iterator(str_list.begin()),
                                              std::make_move_iterator(str_list.end()));
        check_size(str_v, 3);
        check_moved_from(str_list);
        EXPECT_TRUE(std::ranges::equal(str_v, copy));
    }

    // Iterator-sentinel pair
    {
        auto rng = {1, 2, 3, 4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([] (auto x) { return x % 2 == 0; });
        inplace_vector<int, 10> v2(std::ranges::begin(view), std::ranges::end(view));
        check_size(v2, 4);
        EXPECT_TRUE(std::ranges::equal(v2, view));
    }

    // Bad alloc (sentinel)
    {
        auto large_view = std::ranges::iota_view(1, 100)
                        | std::views::filter([] (auto x) { return x % 2 == 0; });
        auto throwing2 = [&large_view] {
            return inplace_vector<int, 10>(std::ranges::begin(large_view),
                                           std::ranges::end(large_view));
        };
        EXPECT_THROW(throwing2(), std::bad_alloc);
    }

    // Input iterator
    {
        std::istringstream iss("1 2 3 4 5");
        auto input_view = std::ranges::istream_view<int>(iss);
        inplace_vector<int, 10> v3(std::ranges::begin(input_view),
                                    std::ranges::end(input_view));
        check_size(v3, 5);
        EXPECT_TRUE(std::ranges::equal(v3, std::array{1, 2, 3, 4, 5}));
    }

    // Bad alloc (input iterator)
    {
        std::istringstream large_iss("1 2 3 4 5 6 7 8 9 10");
        auto large_input_view = std::ranges::istream_view<int>(large_iss);

        auto throwing3 = [&large_input_view] {
            return inplace_vector<int, 5>(std::ranges::begin(large_input_view),
                                           std::ranges::end(large_input_view));
        };
        EXPECT_THROW(throwing3(), std::bad_alloc);
    }
}

TEST_F(InplaceVector, RangeConstruction) {
    // Empty range
    {
        std::vector<int> empty;
        inplace_vector<int, 10> v5(empty);
        check_size(v5, 0);
    }

    // Range (forward range)
    {
        std::list list(8, 1);
        inplace_vector<int, 10> v(list);
        check_size(v, 8);
        check_all_equal(v, 1);
    }

    // Bad alloc (forward range)
    {
        std::list list2(12, 1);
        auto throwing = [&list2] {
            return inplace_vector<int, 10>(list2);
        };
        EXPECT_THROW(throwing(), std::bad_alloc);
    }

    // Move range
    {
        std::list<std::string> str_list{"abcdefg", "hijklmn", "opurstuvw"};
        auto copy = str_list;
        inplace_vector<std::string, 10> str_v(std::move(str_list));
        check_size(str_v, 3);
        check_moved_from(str_list);
        EXPECT_TRUE(std::ranges::equal(str_v, copy));
    }

    // Range with sentinel (view)
    {
        auto rng = {1, 2, 3, 4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](auto x) { return x % 2 == 0; });
        inplace_vector<int, 10> v2(view);
        check_size(v2, 4);
        EXPECT_TRUE(std::ranges::equal(v2, view));
    }

    // Bad alloc (view with sentinel)
    {
        auto large_view = std::ranges::iota_view(1, 100)
                       | std::views::filter([](auto x) { return x % 2 == 0; });
        auto throwing2 = [&large_view] {
            return inplace_vector<int, 10>(large_view);
        };
        EXPECT_THROW(throwing2(), std::bad_alloc);
    }

    // Input range
    {
        std::istringstream iss("1 2 3 4 5");
        auto input_view = std::ranges::istream_view<int>(iss);
        inplace_vector<int, 10> v3(input_view);
        check_size(v3, 5);
        EXPECT_TRUE(std::ranges::equal(v3, std::array{1, 2, 3, 4, 5}));
    }

    // Bad alloc (input range)
    {
        std::istringstream large_iss("1 2 3 4 5 6 7 8 9 10");
        auto large_input_view = std::ranges::istream_view<int>(large_iss);

        auto throwing3 = [&large_input_view] {
            return inplace_vector<int, 5>(large_input_view);
        };
        EXPECT_THROW(throwing3(), std::bad_alloc);
    }

}

TEST_F(InplaceVector, CopyConstruction) {
    auto copy{_int_vec};
    check_size(copy, 5);
    check_all_equal(copy, 0);
    // Deep copy
    _int_vec[0] = 100;
    EXPECT_EQ(copy[0], 0);

    // Prevents "copy construction" from vectors of incompatible sizes
    static_assert(!std::constructible_from<inplace_vector<int, 10>, const inplace_vector<int, 11>&>);
    static_assert(!std::constructible_from<inplace_vector<int, 10>, const inplace_vector<int, 9>&>);
}

TEST_F(InplaceVector, MoveConstruction) {
    inplace_vector<std::string, 10> v1 {
        "ABCD", "EFGH", "IJKL", "MNO", "PQRST", "SUVW"
    };
    auto snapshot = v1;

    auto v2{std::move(v1)};
    check_size(v2, 6);
    check_moved_from(v1);
    EXPECT_TRUE(std::ranges::equal(snapshot, v2));

    static_assert(!std::constructible_from<inplace_vector<int, 10>, inplace_vector<int, 11>&&>);
    static_assert(!std::constructible_from<inplace_vector<int, 10>, inplace_vector<int, 9>&&>);
}

TEST_F(InplaceVector, CopyAssignment) {
    inplace_vector<int, 10> v2(8);
    v2 = _int_vec;

    check_size(v2, 5);
    EXPECT_TRUE(std::ranges::equal(v2, _int_vec));
    // Deep copy
    _int_vec[0] = 100;
    EXPECT_EQ(v2[0], 0); // Unchanged

    static_assert(!std::assignable_from<inplace_vector<int, 10>&, const inplace_vector<int, 11>&>);
    static_assert(!std::assignable_from<inplace_vector<int, 10>&, const inplace_vector<int, 9>&>);
}

TEST_F(InplaceVector, MoveAssignment) {
    inplace_vector<std::string, 10> v1 {
        "ABCD", "EFGH", "IJKL", "MNO", "PQRST", "SUVW"
    };
    auto snapshot = v1;

    inplace_vector<std::string, 10> v2(4, "ghost");
    v2 = std::move(v1);
    check_size(v2, 6);
    check_moved_from(v1);
    EXPECT_TRUE(std::ranges::equal(snapshot, v2));

    static_assert(!std::assignable_from<inplace_vector<int, 10>&, inplace_vector<int, 11>&&>);
    static_assert(!std::assignable_from<inplace_vector<int, 10>&, inplace_vector<int, 9>&&>);
}

TEST_F(InplaceVector, AssignWithSize) {

    _int_vec.assign(3);
    check_size(_int_vec, 3);
    check_all_equal(_int_vec, 0);

    _int_vec.assign(8, -11);
    check_size(_int_vec, 8);
    check_all_equal(_int_vec, -11);

    _int_vec.assign(0);
    check_size(_int_vec, 0);

    // Bad alloc
    EXPECT_THROW(_int_vec.assign(100), std::bad_alloc);
}

TEST_F(InplaceVector, AssignWithIterator) {

    // 1. Forward iterator
    {
        std::list new_data{6, 7, 8, 9, 10};
        _int_vec.assign(new_data.begin(), new_data.end());
        check_size(_int_vec, 5);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{6, 7, 8, 9, 10}));
    }

    // 2. Bad alloc with forward iterators
    {
        std::list large_data(12, 42);
        EXPECT_THROW(_int_vec.assign(large_data.begin(), large_data.end());, std::bad_alloc);
    }

    // 3. Move iterator
    {
        std::list<std::string> str_list{"hello", "world", "test"};

        _str_vec.assign(std::make_move_iterator(str_list.begin()),
                       std::make_move_iterator(str_list.end()));

        check_size(_str_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array<std::string_view, 3>{"hello", "world", "test"}));
        check_moved_from(str_list);
    }

    // 4. Iterator-sentinel
    {
        auto rng = {1, 2, 3, 4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](auto x) { return x % 2 == 0; });

        _int_vec.assign(std::ranges::begin(view), std::ranges::end(view));
        check_size(_int_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{2, 4, 6, 8}));
    }

    // 5. Bad alloc with sentinel
    {
        auto large_view = std::ranges::iota_view(1, 100)
                        | std::views::filter([](auto x) { return x % 3 == 0; });

        auto throwing = [&] { _int_vec.assign(std::ranges::begin(large_view),
                                             std::ranges::end(large_view)); };
        EXPECT_THROW(throwing(), std::bad_alloc);
    }

    // 6. Input iterator
    {
        std::istringstream iss("100 200 300");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.assign(std::ranges::begin(input_view), std::ranges::end(input_view));
        check_size(_int_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{100, 200, 300}));
    }

    // 7. Bad alloc with input iterator
    {
        std::istringstream large_iss("1 2 3 4 5 6 7 8 9 10 11 12");
        auto large_input_view = std::ranges::istream_view<int>(large_iss);

        auto throwing = [&] { _int_vec.assign(std::ranges::begin(large_input_view),
                                              std::ranges::end(large_input_view)); };
        EXPECT_THROW(throwing(), std::bad_alloc);
    }

    // 8. Empty range
    {
        std::vector<int> empty;
        _int_vec.assign(empty.begin(), empty.end());
        check_size(_int_vec, 0);
    }

    // 9. Different value type
    {
        std::list chars{'a', 'b', 'c'};
        _int_vec.assign(chars.begin(), chars.end());
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{97, 98, 99}));
    }

    // 10. Self assignment
    {
        _int_vec = {1, 2, 3, 4};
        _int_vec.assign(_int_vec.begin(), _int_vec.end());
        check_size(_int_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 2, 3, 4}));
    }

}

TEST_F(InplaceVector, AssignWithRange) {
    // 1. Forward range
    {
        std::list new_data{4, 5, 6, 7};
        _int_vec.assign_range(new_data);
        check_size(_int_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{4, 5, 6, 7}));
    }

    // 2. Bad alloc with forward range
    {
        std::list large_data(15, 42);
        EXPECT_THROW( _int_vec.assign_range(large_data); , std::bad_alloc);
    }

    // 3. Move range assignment
    {
        std::list<std::string> str_list{"move", "these", "strings"};

        _str_vec.assign_range(std::move(str_list));

        check_size(_str_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"move", "these", "strings"}));
        check_moved_from(str_list);
    }

    // 4. Range with sentinel
    {
        auto rng = {10, 20, 30, 40, 50};
        auto view = rng | std::views::take(3);

        _int_vec.assign_range(view);
        check_size(_int_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{10, 20, 30}));
    }

    // 5. Bad alloc with view
    {
        auto large_view = std::ranges::iota_view(1, 20);
        EXPECT_THROW(_int_vec.assign_range(large_view);, std::bad_alloc);
    }

    // 6. Input range
    {
        std::istringstream iss("999 888 777");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.assign_range(input_view);
        check_size(_int_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{999, 888, 777}));
    }

    // 7. Bad alloc with input range
    {
        std::istringstream large_iss("1 2 3 4 5 6 7 8 9 10 11");
        auto large_input_view = std::ranges::istream_view<int>(large_iss);

        EXPECT_THROW(_int_vec.assign_range(large_input_view), std::bad_alloc);
    }

    // 8. Different value type
    {
        std::list chars{'a', 'b', 'c'};
        _int_vec.assign_range(chars);
        check_size(_int_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{97, 98, 99}));
    }

    // 9. Empty range
    {
        std::set<int> empty_set;
        _int_vec.assign_range(empty_set);
        check_size(_int_vec, 0);
    }

    // 10. Self-assignment
    {
        _int_vec = {1, 2, 3, 4};
        _int_vec.assign_range(_int_vec);
        check_size(_int_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 2, 3, 4}));
    }
}

TEST_F(InplaceVector, EmplaceBack) {
    // Basic usage
    _int_vec.unchecked_emplace_back(100);
    check_size(_int_vec, 6);
    EXPECT_EQ(_int_vec.back(), 100);

    // Inplace construction
    _str_vec.unchecked_emplace_back(10, 'a');
    EXPECT_EQ(_str_vec.back(), "aaaaaaaaaa");

    // Move construction
    std::string moved = "Going to be moved";
    _str_vec.unchecked_emplace_back(std::move(moved));
    EXPECT_TRUE(moved.empty());

    // returning reference/pointer
    _int_vec.assign(0);
    EXPECT_EQ(_int_vec.unchecked_emplace_back(80), 80);
    EXPECT_EQ(*_int_vec.try_emplace_back(99), 99);
    _int_vec.unchecked_emplace_back(120) = 222;
    EXPECT_EQ(_int_vec.back(), 222);

    // out-of-range emplacement
    _int_vec.assign(_int_vec.max_size());
    EXPECT_NO_THROW(_int_vec.try_emplace_back(100));
    EXPECT_EQ(_int_vec.try_emplace_back(100), nullptr);
    EXPECT_THROW(_int_vec.emplace_back(100), std::bad_alloc);
    check_all_equal(_int_vec, 0); // remains unchanged
}

class IVTrackDestruction {
public:
    static int destroy_count;
    IVTrackDestruction() = default;
    ~IVTrackDestruction() {
        ++destroy_count;
    }
    static void clear() { destroy_count = 0; }
};

int IVTrackDestruction::destroy_count = 0;

TEST_F(InplaceVector, PopBack) {
    // Basic usage
    inplace_vector<int, 10> vec{1, 2, 3, 4, 5};
    vec.unchecked_pop_back();
    EXPECT_TRUE(std::ranges::equal(vec, std::array{1, 2, 3, 4}));

    // Ensure destruction
    inplace_vector<IVTrackDestruction, 10> tracked_vec(5);
    IVTrackDestruction::clear();
    while (!tracked_vec.empty()) {
        tracked_vec.unchecked_pop_back();
    }
    EXPECT_EQ(IVTrackDestruction::destroy_count, 5);

    // No effects when empty
    check_size(tracked_vec, 0);
    EXPECT_NO_THROW(tracked_vec.pop_back());
    check_size(tracked_vec, 0);
}

TEST_F(InplaceVector, AppendRange) {
    /************* Iterator Overload ************/
    // 1. Forward iterator
    {
        _int_vec.assign(3, 1);
        std::list new_data{4, 5, 6};

        _int_vec.append_range(new_data.begin(), new_data.end());
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 2. Bad alloc with forward iterator
    {
        _int_vec.assign(8, 1);
        std::list large_data{4, 5, 6, 7};

        auto throwing = [&] { _int_vec.append_range(large_data.begin(), large_data.end()); };
        EXPECT_THROW(throwing(), std::bad_alloc);
        // remains unchanged
        check_size(_int_vec, 8);
        check_all_equal(_int_vec, 1);
    }

    // 3. Move iterator
    {
        _str_vec.assign(3, "a");
        std::list<std::string> new_data{"c", "d", "e"};

        _str_vec.append_range(std::make_move_iterator(new_data.begin()),
                       std::make_move_iterator(new_data.end()));

        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array {"a", "a", "a", "c", "d", "e"}));
        check_moved_from(new_data);
    }

    // 4. Iterator-sentinel
    {
        _int_vec.assign(3, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](const auto x) { return x % 2 == 0; });

        _int_vec.append_range(std::ranges::begin(view), std::ranges::end(view));
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 6, 8}));
    }

    // 5. Bad alloc with iterator-sentinel
    {
        _int_vec.assign(8, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](const auto x) { return x % 2 == 0; });

        auto throwing = [&] {
            _int_vec.append_range(std::ranges::begin(view), std::ranges::end(view));
        };

        EXPECT_THROW(throwing(), std::bad_alloc);
        // remains unchanged
        check_size(_int_vec, 8);
        check_all_equal(_int_vec, 1);
    }

    // 6. Input iterator
    {
        _int_vec.assign(3, 1);
        std::istringstream iss("4 5 6");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.append_range(std::ranges::begin(input_view), std::ranges::end(input_view));
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 7. Bad alloc with input iterator
    {
        _int_vec.assign(8, 1);
        std::istringstream iss("4 5 6 7");

        auto input_view = std::ranges::istream_view<int>(iss);
        auto throwing = [&] {
            _int_vec.append_range(std::ranges::begin(input_view), std::ranges::end(input_view));
        };

        EXPECT_THROW(throwing(), std::bad_alloc);
        // unchanged
        check_size(_int_vec, 8);
        check_all_equal(_int_vec, 1);
    }

    /************* Range Overload ************/
    // 8. Sized range
    {
        _int_vec.assign(3, 1);
        std::list new_data{4, 5, 6};

        _int_vec.append_range(new_data);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 9. Bad alloc with sized range
    {
        _int_vec.assign(8, 1);
        std::list large_data{4, 5, 6, 7};

        auto throwing = [&] { _int_vec.append_range(large_data); };
        EXPECT_THROW(throwing(), std::bad_alloc);
        // remains unchanged
        check_size(_int_vec, 8);
        check_all_equal(_int_vec, 1);
    }

    // 10. Rvalue range
    {
        _str_vec.assign(3, "a");
        std::list<std::string> new_data{"c", "d", "e"};

        _str_vec.append_range(std::move(new_data));

        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "a", "a", "c", "d", "e"}));
        check_moved_from(new_data);
    }

    // 11. Range with sentinel (view)
    {
        _int_vec.assign(3, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](int x) { return x % 2 == 0; });

        _int_vec.append_range(view);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 6, 8}));
    }

    // 12. Bad alloc with sentinel range
    {
        _int_vec.assign(8, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](int x) { return x % 2 == 0; });

        auto throwing = [&] { _int_vec.append_range(view); };
        EXPECT_THROW(throwing(), std::bad_alloc);
        // remains unchanged
        check_size(_int_vec, 8);
        check_all_equal(_int_vec, 1);
    }

    // 13. Input range
    {
        _int_vec.assign(3, 1);
        std::istringstream iss("4 5 6");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.append_range(input_view);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 14. Bad alloc with input range
    {
        _str_vec.assign(8, "a");
        std::istringstream iss("4 5 6 7");
        auto input_view = std::ranges::istream_view<std::string>(iss);

        auto throwing = [&] { _str_vec.append_range(input_view); };
        EXPECT_THROW(throwing(), std::bad_alloc);
        // unchanged
        check_size(_str_vec, 8);
        check_all_equal(_str_vec, "a");
    }
}

TEST_F(InplaceVector, TryAppendRange) {
    /************* Iterator Overload ************/
    // 1. Forward iterator
    {
        _int_vec.assign(3, 1);
        std::list new_data{4, 5, 6};

        auto next = _int_vec.try_append_range(new_data.begin(), new_data.end());
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
        EXPECT_TRUE(next == new_data.end()); // All elements appended
    }

    // 2. Forward iterator - partial append
    {
        _int_vec.assign(8, 1);
        std::list large_data{9, 10, 11, 12};

        auto next = _int_vec.try_append_range(large_data.begin(), large_data.end());
        check_size(_int_vec, 10); // Filled to capacity
        EXPECT_TRUE(_int_vec[8] == 9);
        EXPECT_TRUE(_int_vec[9] == 10);
        EXPECT_TRUE(*next == 11);
    }

    // 3. Move iterator
    {
        _str_vec.assign(3, "a");
        std::list<std::string> new_data{"c", "d", "e"};

        auto next = _str_vec.try_append_range(
            std::make_move_iterator(new_data.begin()),
            std::make_move_iterator(new_data.end()));

        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array {"a", "a", "a", "c", "d", "e"}));
        check_moved_from(new_data);
        EXPECT_TRUE(next == std::make_move_iterator(new_data.end()));
    }

    // 4. Iterator-sentinel
    {
        _int_vec.assign(3, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](int x) { return x % 2 == 0; });

        auto next = _int_vec.try_append_range(
                  std::ranges::begin(view), std::ranges::end(view));
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 6, 8}));
        EXPECT_TRUE(next == std::ranges::end(view)); // All appended
    }

    // 5. Iterator-sentinel - partial append
    {
        _int_vec.assign(8, 1);
        auto rng = {4, 5, 6, 7, 8, 9, 10};
        auto view = rng | std::views::filter([](int x) { return x % 2 == 0; }); // 4,6,8,10

        auto next = _int_vec.try_append_range(
            std::ranges::begin(view), std::ranges::end(view));

        check_size(_int_vec, 10);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,1,1,1,1,1,1,1,4,6}));
        EXPECT_TRUE(*next == 8);
    }

    // 6. Input iterator
    {
        _int_vec.assign(3, 1);
        std::istringstream iss("4 5 6");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.try_append_range(
            std::ranges::begin(input_view), std::ranges::end(input_view));
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
        EXPECT_TRUE(iss.eof());  // stream exhausted
    }

    // 7. Input iterator - partial append
    {
        _int_vec.assign(8, 1);
        std::istringstream iss("9 10 11 12 13");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.try_append_range(
            std::ranges::begin(input_view), std::ranges::end(input_view));

        check_size(_int_vec, 10);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,1,1,1,1,1,1,1,9,10}));
        EXPECT_FALSE(iss.eof());
    }

    /************* Range Overload ************/
    // 8. Sized range
    {
        _int_vec.assign(3, 1);
        std::list new_data{4, 5, 6};

        auto result = _int_vec.try_append_range(new_data);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
        EXPECT_TRUE(result == new_data.end());
    }

    // 9. Sized range - partial append
    {
        _int_vec.assign(8, 1);
        std::list large_data{9, 10, 11, 12};

        auto result = _int_vec.try_append_range(large_data);
        check_size(_int_vec, 10);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{
            1,1,1,1,1,1,1,1,9,10
        }));
        EXPECT_TRUE(*result == 11); // First unappended element
    }

    // 10. Rvalue range
    {
        _str_vec.assign(3, "a");
        std::list<std::string> new_data{"c", "d", "e"};

        auto result = _str_vec.try_append_range(std::move(new_data));
        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "a", "a", "c", "d", "e"}));
        check_moved_from(new_data);
        EXPECT_TRUE(result == std::make_move_iterator(new_data.end()));
    }

    // 13. Range with sentinel
    {
        _int_vec.assign(3, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](int x) { return x % 2 == 0; });

        auto result = _int_vec.try_append_range(view);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 6, 8}));
        EXPECT_TRUE(result == std::ranges::end(view));
    }

    // 14. Range with sentinel - partial append
    {
        _int_vec.assign(8, 1);
        auto rng = {4, 5, 6, 7, 8, 9, 10};
        auto view = rng | std::views::take_while([](int x) { return x <= 10; });

        auto result = _int_vec.try_append_range(view);
        check_size(_int_vec, 10);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,1,1,1,1,1,1,1,4,5}));
        EXPECT_TRUE(*result == 6);
    }

    // 15. Input range
    {
        _int_vec.assign(3, 1);
        std::istringstream iss("4 5 6");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.try_append_range(input_view);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
        EXPECT_TRUE(iss.eof());
    }

    // 16. Input range - stops at capacity
    {
        _int_vec.assign(8, 1);
        std::istringstream iss("9 10 11 12 13");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.try_append_range(input_view);
        check_size(_int_vec, 10);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,1,1,1,1,1,1,1,9,10}));
        EXPECT_FALSE(iss.eof());
    }
}

TEST_F(InplaceVector, UncheckedAppendRange) {
    // User may want to check whether the size fits before using this method.
    // Otherwise, it is UB.

    /************* Iterator Overload ************/
    // 1. Forward iterator
    {
        _int_vec.assign(3, 1);
        std::list new_data{4, 5, 6};

        ASSERT_TRUE(
            _int_vec.size() + std::ranges::distance(new_data.begin(), new_data.end())
            <= _int_vec.capacity());

        _int_vec.unchecked_append_range(new_data.begin(), new_data.end());
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 2. Forward iterator - UB if exceeds capacity (not tested)

    // 3. Move iterator
    {
        _str_vec.assign(3, "a");
        std::list<std::string> new_data{"c", "d", "e"};

        ASSERT_TRUE(_str_vec.size() + 3 <= _str_vec.capacity());

        _str_vec.unchecked_append_range(
            std::make_move_iterator(new_data.begin()),
            std::make_move_iterator(new_data.end()));

        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array {"a", "a", "a", "c", "d", "e"}));
        check_moved_from(new_data);
    }

    // 4. Iterator-sentinel
    {
        _int_vec.assign(3, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::filter([](int x) { return x % 2 == 0; }); // 4,6,8

        auto begin = std::ranges::begin(view);
        auto end = std::ranges::end(view);
        ASSERT_TRUE(_int_vec.size() + std::ranges::distance(begin, end) <= _int_vec.capacity());

        _int_vec.unchecked_append_range(begin, end);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 6, 8}));
    }

    // 5. Iterator-sentinel - UB if exceeds capacity

    // 6. Input iterator
    {
        _int_vec.assign(3, 1);
        std::istringstream iss("4 5 6");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.unchecked_append_range(std::ranges::begin(input_view),
                                       std::ranges::end(input_view));
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 7. Input iterator - UB if exceeds capacity

    /************* Range Overload ************/
    // 8. Sized range
    {
        _int_vec.assign(3, 1);
        std::list new_data{4, 5, 6};

        ASSERT_TRUE(_int_vec.size() + new_data.size() <= _int_vec.capacity());
        _int_vec.unchecked_append_range(new_data);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 9. Rvalue range
    {
        _str_vec.assign(3, "a");
        std::list<std::string> new_data{"c", "d", "e"};

        ASSERT_TRUE(_str_vec.size() + new_data.size() <= _str_vec.capacity());
        _str_vec.unchecked_append_range(std::move(new_data));
        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "a", "a", "c", "d", "e"}));
        check_moved_from(new_data);
    }

    // 10. Range with sentinel (view)
    {
        _int_vec.assign(3, 1);
        auto rng = {4, 5, 6, 7, 8};
        auto view = rng | std::views::take(3); // 4,5,6

        // We know it's 3 elements
        ASSERT_TRUE(_int_vec.size() + 3 <= _int_vec.capacity());
        _int_vec.unchecked_append_range(view);
        check_size(_int_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1, 1, 1, 4, 5, 6}));
    }

    // 11. Input range
    {
        _int_vec.assign(8, 1);
        std::istringstream iss("9 10 11 12");
        auto input_view = std::ranges::istream_view<int>(iss);

        _int_vec.unchecked_append_range(input_view);
        check_size(_int_vec, 10);
        EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,1,1,1,1,1,1,1,9,10}));
        EXPECT_FALSE(iss.eof());
    }

}

TEST_F(InplaceVector, UncheckedEmplace) {
    // emplace at beginning
    auto it = _int_vec.unchecked_emplace(_int_vec.begin(), 1);
    check_size(_int_vec, 6);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,0,0,0,0,0}));
    EXPECT_EQ(it, _int_vec.begin());

    // middle
    it = _int_vec.unchecked_emplace(_int_vec.nth(2), 2);
    check_size(_int_vec, 7);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,0,2,0,0,0,0}));
    EXPECT_EQ(it, _int_vec.begin() + 2);

    // end
    it = _int_vec.unchecked_emplace(_int_vec.end(), 3);
    check_size(_int_vec, 8);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array{1,0,2,0,0,0,0,3}));
    EXPECT_EQ(it, _int_vec.end() - 1);

    // In-place construction
    auto s_it = _str_vec.unchecked_emplace(_str_vec.begin() + 1, 5, 's');
    check_size(_str_vec, 6);
    EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"", "sssss", "", "", "", ""}));
    EXPECT_EQ(s_it, _str_vec.begin() + 1);

    // Move-only type
    {
        inplace_vector<std::unique_ptr<int>, 10> vec;
        vec.emplace_back(std::make_unique<int>(1));
        vec.emplace_back(std::make_unique<int>(3));

        auto u_it = vec.unchecked_emplace(vec.nth(1), std::make_unique<int>(2));
        check_size(vec, 3);
        EXPECT_EQ(*vec[0], 1);
        EXPECT_EQ(*vec[1], 2);
        EXPECT_EQ(*vec[2], 3);
        EXPECT_EQ(u_it, vec.nth(1));
    }
}

TEST_F(InplaceVector, Emplace) {
    // just test exception correctness here
    _int_vec.assign(10);
    EXPECT_THROW((_int_vec.emplace(_int_vec.begin(), 0)), std::bad_alloc);

    _int_vec.assign(5);
    EXPECT_THROW((_int_vec.emplace(_int_vec.end() + 1, 0)), std::out_of_range);

    EXPECT_THROW((_int_vec.emplace(_int_vec.begin() - 1, 0)), std::out_of_range);
}


TEST_F(InplaceVector, InsertWithValue) {
    // beginning
    auto it = _str_vec.insert(_str_vec.begin(), 2, "-1");
    check_size(_str_vec, 7);
    EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"-1", "-1", "", "", "", "", ""}));
    EXPECT_EQ(it, _str_vec.begin());

    // end
    it = _str_vec.insert(_str_vec.end(), 3, "-3");
    check_size(_str_vec, 10);
    EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"-1", "-1", "", "", "", "", "", "-3", "-3", "-3"}));
    EXPECT_EQ(it, _str_vec.begin() + 7);  // original end

    // middle
    _str_vec.assign(5, "");
    it = _str_vec.insert(_str_vec.begin() + 2, 4, "9");
    check_size(_str_vec, 9);
    EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"", "", "9", "9", "9", "9", "", "", ""}));
    EXPECT_EQ(it, _str_vec.begin() + 2);

    // exceptions
    EXPECT_THROW((_str_vec.insert(_str_vec.end(), 4, "-3")), std::bad_alloc);
    EXPECT_THROW((_str_vec.insert(_str_vec.begin() - 1, "1")), std::out_of_range);
    EXPECT_THROW((_str_vec.insert(_str_vec.end() + 1, "1")), std::out_of_range);
}

TEST_F(InplaceVector, InsertWithIterators) {
    // 1. Forward iterator, at beginning
    {
        _str_vec = {"c", "d", "e"};
        std::list<std::string> new_data{"a", "b"};

        auto it = _str_vec.insert(_str_vec.begin(), new_data.begin(), new_data.end());
        check_size(_str_vec, 5);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e"}));
        EXPECT_TRUE(it == _str_vec.begin());
    }

    // 2. Forward iterator, at middle
    {
        _str_vec = {"a", "b", "e", "f"};
        std::list<std::string> new_data{"c", "d"};

        auto it = _str_vec.insert(_str_vec.begin() + 2, new_data.begin(), new_data.end());
        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e", "f"}));
        EXPECT_TRUE(it == _str_vec.begin() + 2);
    }

    // 3. Forward iterator, at end
    {
        _str_vec = {"a", "b", "c"};
        std::list<std::string> new_data{"d", "e", "f"};

        auto it = _str_vec.insert(_str_vec.end(), new_data.begin(), new_data.end());
        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e", "f"}));
        EXPECT_TRUE(it == _str_vec.begin() + 3);
    }

    // 4. Move iterator insert
    {
        _str_vec = {"a", "e", "f"};
        std::list<std::string> new_data{"b", "c", "d"};

        auto it = _str_vec.insert(_str_vec.begin() + 1,
                                  std::make_move_iterator(new_data.begin()),
                                  std::make_move_iterator(new_data.end()));
        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e", "f"}));
        check_moved_from(new_data);
        EXPECT_TRUE(it == _str_vec.begin() + 1);
    }

    // 5. Iterator-sentinel insert
    {
        _str_vec = {"a", "e"};
        auto rng = {"b", "c", "d"};
        auto view = rng | std::views::filter([](const std::string& x) { return x != "c"; }); // ["b","d"]

        auto it = _str_vec.insert(_str_vec.begin() + 1,
                                  std::ranges::begin(view),
                                  std::ranges::end(view));
        check_size(_str_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "d", "e"}));
        EXPECT_TRUE(it == _str_vec.begin() + 1);
    }

    // 6. std::out_of_range on invalid position
    {
        _str_vec = {"a", "b", "c"};
        std::list<std::string> new_data{"z"};

        inplace_vector<std::string, 10> other_vec;
        EXPECT_THROW(_str_vec.insert(other_vec.begin(), new_data.begin(), new_data.end()),
                     std::out_of_range);
        // unchanged
        check_size(_str_vec, 3);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c"}));
    }

    // 7. std::bad_alloc (input iterator)
    {
        _int_vec.assign(8, 1);
        std::istringstream iss("1 2 3 4 5 6");
        auto input_view = std::ranges::istream_view<int>(iss);
        auto throwing = [&] {
            _int_vec.insert(_int_vec.begin(),
                std::ranges::begin(input_view), std::ranges::end(input_view));
        };
        EXPECT_THROW(throwing(), std::bad_alloc);
        // unchanged
        check_size(_int_vec, 8);
        check_all_equal(_int_vec, 1);
    }

    // 8. Move-only type insertion
    {
        inplace_vector<std::unique_ptr<int>, 10> vec;
        vec.emplace_back(std::make_unique<int>(1));
        vec.emplace_back(std::make_unique<int>(4));

        std::list<std::unique_ptr<int>> new_data;
        new_data.push_back(std::make_unique<int>(2));
        new_data.push_back(std::make_unique<int>(3));

        auto it = vec.insert(vec.begin() + 1,
                            std::make_move_iterator(new_data.begin()),
                            std::make_move_iterator(new_data.end()));
        check_size(vec, 4);
        EXPECT_EQ(*vec[0], 1);
        EXPECT_EQ(*vec[1], 2);
        EXPECT_EQ(*vec[2], 3);
        EXPECT_EQ(*vec[3], 4);
        EXPECT_TRUE(*new_data.begin() == nullptr);
        EXPECT_TRUE(*(++new_data.begin()) == nullptr);
        EXPECT_TRUE(it == vec.begin() + 1);
    }
}

TEST_F(InplaceVector, InsertWithRange) {
    // 1. Sized range insert at beginning
    {
        _str_vec = {"c", "d", "e"}; // ["c","d","e"]
        std::vector<std::string> new_data{"a", "b"};

        auto it = _str_vec.insert_range(_str_vec.begin(), new_data);
        check_size(_str_vec, 5);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e"}));
        EXPECT_TRUE(it == _str_vec.begin());
    }

    // 2. Sized range insert at middle
    {
        _str_vec = {"a", "b", "e", "f"}; // ["a","b","e","f"]
        std::list<std::string> new_data{"c", "d"};

        auto it = _str_vec.insert_range(_str_vec.begin() + 2, new_data);
        check_size(_str_vec, 6);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e", "f"}));
        EXPECT_TRUE(it == _str_vec.begin() + 2);
    }

    // 3. Rvalue range
    {
        _str_vec = {"a", "d", "e"};
        std::list<std::string> new_data{"b", "c"};
        auto copy = new_data;

        auto it = _str_vec.insert_range(_str_vec.begin() + 1, std::move(new_data));
        check_size(_str_vec, 5);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d", "e"}));
        check_moved_from(new_data);
        EXPECT_TRUE(it == _str_vec.begin() + 1);
    }
    // 4. Range with sentinel
    {
        _str_vec = {"a", "e"}; // ["a","e"]
        auto rng = {"b", "c", "d"};
        auto view = rng | std::views::take(2); // ["b","c"]

        auto it = _str_vec.insert_range(_str_vec.begin() + 1, view);
        check_size(_str_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "e"}));
        EXPECT_TRUE(it == _str_vec.begin() + 1);
    }
    // 5. Input range
    {
        _str_vec = {"a", "d"}; // ["a","d"]
        std::istringstream iss("b c");
        auto input_view = std::ranges::istream_view<std::string>(iss);

        auto it = _str_vec.insert_range(_str_vec.begin() + 1, input_view);
        check_size(_str_vec, 4);
        EXPECT_TRUE(std::ranges::equal(_str_vec, std::array{"a", "b", "c", "d"}));
        EXPECT_TRUE(it == _str_vec.begin() + 1);
    }
    // 6. std::bad_alloc (input_range)
    {
        _str_vec.assign(5); // fills with empty strings
        std::istringstream iss("a b c d e f");
        auto input_view = std::ranges::istream_view<std::string>(iss);
        EXPECT_THROW((_str_vec.insert_range(_str_vec.begin(), input_view)), std::bad_alloc);
        // unchanged
        check_size(_str_vec, 5);
        check_all_equal(_str_vec, ""); // still empty strings
    }
}


TEST_F(InplaceVector, Resize) {
    // Resizes to a larger size
    _int_vec.resize(8, -3);
    check_size(_int_vec, 8);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array {0, 0, 0, 0, 0, -3, -3, -3}));

    // Resizes to a smaller size
    _int_vec.resize(3, -1);
    check_size(_int_vec, 3);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array {0, 0, 0}));

    // Resize with default value
    _str_vec.resize(7);
    check_size(_str_vec, 7);
    EXPECT_TRUE(std::ranges::equal(_str_vec, std::array {"", "", "", "", "", "", ""}));

    // Move only type
    inplace_vector<std::unique_ptr<int>, 10> uvec;
    uvec.resize(10);
    check_size(uvec, 10);
    check_all_equal(uvec, nullptr);

    // Ensures proper destruction
    inplace_vector<IVTrackDestruction, 10> vec(10);
    IVTrackDestruction item;
    IVTrackDestruction::clear();
    vec.resize(5, item);
    EXPECT_EQ(IVTrackDestruction::destroy_count, 5);

    // std::bad_alloc
    EXPECT_THROW(_int_vec.resize(11), std::bad_alloc);
}

TEST_F(InplaceVector, EraseWithIterator) {
    inplace_vector<int, 10> vec{1, 2, 3, 4, 5, 6};
    // middle
    vec.erase(vec.nth(2), vec.nth(5));
    check_size(vec, 3);
    EXPECT_TRUE(std::ranges::equal(vec, std::array {1, 2, 6}));
    // front
    vec.erase(vec.begin());
    check_size(vec, 2);
    EXPECT_TRUE(std::ranges::equal(vec, std::array {2, 6}));
    // end
    vec.erase(vec.end() - 1);
    check_size(vec, 1);
    EXPECT_TRUE(std::ranges::equal(vec, std::array {2}));

    // Proper destruction
    inplace_vector<IVTrackDestruction, 10> vec2(10);
    IVTrackDestruction::clear();
    vec2.erase(vec2.nth(2), vec2.nth(5));
    EXPECT_EQ(IVTrackDestruction::destroy_count, 3);
    check_size(vec2, 7);
}

TEST_F(InplaceVector, EraseWithValue) {
    inplace_vector vec{1, 2, 3, 4, 1, 1, 8, 9, 1, 1};
    vec.erase(1);
    check_size(vec, 5);
    EXPECT_TRUE(std::ranges::equal(vec, std::array {2, 3, 4, 8, 9}));

    // No target value
    inplace_vector<int, 10> nvec{4, 3, 2, 5, 6, 7};
    nvec.erase(1);
    check_size(nvec, 6);  //unchnanged
    EXPECT_TRUE(std::ranges::equal(nvec, std::array {4, 3, 2, 5, 6, 7}));
}

TEST_F(InplaceVector, EraseIf) {
    auto pred = [](int x) { return x % 2 == 0; };
    _int_vec = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    _int_vec.erase_if(pred);

    check_size(_int_vec, 5);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array {1, 3, 5, 7, 9}));
}

TEST_F(InplaceVector, Clear) {
    inplace_vector<IVTrackDestruction, 10> vec2(10);
    IVTrackDestruction::clear();
    vec2.clear();
    EXPECT_EQ(IVTrackDestruction::destroy_count, 10);
    check_size(vec2, 0);
}

TEST_F(InplaceVector, Swap) {
    inplace_vector<int, 10> vec{1, 2, 3, 4, 5, 6, 7, 8};
    _int_vec.swap(vec);
    check_size(_int_vec, 8);
    check_size(vec, 5);
    EXPECT_TRUE(std::ranges::equal(_int_vec, std::array {1, 2, 3, 4, 5, 6, 7, 8}));
    EXPECT_TRUE(std::ranges::equal(vec, std::array {0, 0, 0, 0, 0}));

    // Move only type
    inplace_vector<std::unique_ptr<int>, 10> uvec1(10);
    inplace_vector<std::unique_ptr<int>, 10> uvec2(5);
    swap(uvec1, uvec2);

    check_size(uvec1, 5);
    check_all_equal(uvec1, nullptr);
    check_size(uvec2, 10);
    check_all_equal(uvec2, nullptr);
}

TEST_F(InplaceVector, Comparisons) {
    inplace_vector<int, 10> vec1(5), vec2(5);
    EXPECT_TRUE(vec1 == vec2);
    EXPECT_FALSE(vec1 != vec2);
    EXPECT_FALSE(vec1 < vec2);
    EXPECT_TRUE(vec1 <= vec2);
    EXPECT_FALSE(vec1 > vec2);
    EXPECT_TRUE(vec1 >= vec2);

    vec1 = {1, 2, 3};
    vec2 = {1, 3, 4};
    EXPECT_FALSE(vec1 == vec2);
    EXPECT_TRUE(vec1 != vec2);
    EXPECT_TRUE(vec1 < vec2);
    EXPECT_TRUE(vec1 <= vec2);
    EXPECT_FALSE(vec1 > vec2);
    EXPECT_FALSE(vec1 >= vec2);

    vec1 = {1, 2, 3, 4};
    vec2 = {1, 2, 3};
    EXPECT_FALSE(vec1 == vec2);
    EXPECT_TRUE(vec1 != vec2);
    EXPECT_FALSE(vec1 < vec2);
    EXPECT_FALSE(vec1 <= vec2);
    EXPECT_TRUE(vec1 > vec2);
    EXPECT_TRUE(vec1 >= vec2);
}
