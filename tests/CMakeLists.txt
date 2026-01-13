include(FetchContent)

FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.tar.gz
        URL_HASH SHA256=8ad598c73ad796e0d8280b082cebd82a630d73e73cd3c70057938a6501bba5d7
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(googletest)

add_executable(urlicht_test
        memory/test_tagged_ptr.cpp
        any/test_inplace_any.cpp
        any/test_any_view.cpp
        any/test_adaptive_any.cpp
        container/test_union_find.cpp
        procedural/graph/test_kruskal_mst.cpp
        tracker.h
)

target_link_libraries(urlicht_test
        PRIVATE
        GTest::gtest_main
        urlicht
)

include(GoogleTest)
gtest_discover_tests(urlicht_test
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
)
