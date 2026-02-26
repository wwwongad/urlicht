#include <urlicht/any/any_view.h>
#include <gtest/gtest.h>
#include <string>

class AnyViewTest : public ::testing::Test {};

TEST_F(AnyViewTest, Basics) {
    int i = 42;
    urlicht::any_view v(i);

    EXPECT_TRUE(v.has_value());
    EXPECT_EQ(v.type_info(), typeid(int));
    EXPECT_TRUE(v.is<int>());
    EXPECT_FALSE(v.is<double>());

    // Check value
    EXPECT_EQ(urlicht::any_cast<int>(v), 42);
}

TEST_F(AnyViewTest, PointerConstruction) {
    double d = 3.14159;
    urlicht::any_view v(&d);

    EXPECT_TRUE(v.is<double>());
    EXPECT_DOUBLE_EQ(urlicht::any_cast<double>(v), 3.14159);

    int* p = nullptr;
    urlicht::any_view null_int(p);
    EXPECT_FALSE(null_int.has_value());
    EXPECT_EQ(null_int.type_info(), typeid(void));

    urlicht::any_view null_ptr(nullptr);
    EXPECT_FALSE(null_ptr.has_value());
    EXPECT_EQ(null_ptr.type_info(), typeid(void));
}

TEST_F(AnyViewTest, ModifyOriginal) {
    int i = 10;
    urlicht::any_view v(i);

    EXPECT_EQ(urlicht::any_cast<int>(v), 10);

    i = 20;
    EXPECT_EQ(urlicht::any_cast<int>(v), 20);
}

TEST_F(AnyViewTest, RebindingView) {
    int i = 100;
    float f = 5.5f;

    urlicht::any_view v(i);
    EXPECT_TRUE(v.is<int>());

    v.view(f);
    EXPECT_TRUE(v.is<float>());
    EXPECT_FLOAT_EQ(urlicht::any_cast<float>(v), 5.5f);
}

TEST_F(AnyViewTest, CopyMoveReset) {
    double d = 1.5;
    urlicht::any_view a{d};

    urlicht::any_view b = a; // copy
    EXPECT_TRUE(b.is<double>());
    EXPECT_TRUE(a.same_type(b));

    urlicht::any_view c = std::move(a); // move
    EXPECT_TRUE(c.is<double>());
    EXPECT_TRUE(a);   // Not emptied
    EXPECT_TRUE(a.same_type(c));

    urlicht::any_view e;
    e = b; // copy assign
    EXPECT_TRUE(e.is<double>());
    EXPECT_TRUE(e.same_type(b));

    urlicht::any_view f;
    f = std::move(e); // move assign
    EXPECT_TRUE(f.is<double>());
    EXPECT_TRUE(f.same_type(e));

    f.reset();
    EXPECT_FALSE(f);
}

TEST_F(AnyViewTest, BadAnyCast) {
    int i = 42;
    urlicht::any_view v(i);

    EXPECT_THROW(urlicht::any_cast<float>(v), std::bad_any_cast);
    EXPECT_EQ(urlicht::any_cast<float>(&v), nullptr);

    // Redundant qualifiers (ignored)
    EXPECT_EQ(urlicht::any_cast<const int&>(v), 42);
    EXPECT_EQ(urlicht::any_cast<int&&>(v), 42);
}

TEST_F(AnyViewTest, Swap) {
    int a = 1;
    double b = 2.0;

    urlicht::any_view v1(a);
    urlicht::any_view v2(b);

    swap(v1, v2);

    EXPECT_TRUE(v1.is<double>());
    EXPECT_TRUE(v2.is<int>());
}

TEST_F(AnyViewTest, MakeAnyViewHelper) {
    std::string s = "Hello";
    auto v = urlicht::make_any_view(s);

    EXPECT_TRUE(v.is<std::string>());
    EXPECT_EQ(urlicht::any_cast<std::string>(v), "Hello");
}

TEST_F(AnyViewTest, ContainerUsage) {
    std::vector<urlicht::any_view> views;

    int a = 1;
    double b = 2.0;
    std::string c = "three";

    views.push_back(a);
    views.emplace_back(b);
    views.emplace_back(c);

    EXPECT_EQ(views.size(), 3);
    EXPECT_TRUE(views[0].is<int>());
    EXPECT_TRUE(views[1].is<double>());
    EXPECT_TRUE(views[2].is<std::string>());
}
