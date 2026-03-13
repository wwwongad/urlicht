
find_package(GTest QUIET)

if(NOT GTest_FOUND)
    include(FetchContent)
    FetchContent_Declare(
            googletest
            URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip
    )
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
endif()

file(GLOB_RECURSE URLICHT_TEST_SOURCES
        CONFIGURE_DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/any/test_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/container/test_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/memory/test_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/procedural/test_*.cpp"
)

if(URLICHT_TEST_SOURCES STREQUAL "")
    message(FATAL_ERROR "No test sources found in ${CMAKE_CURRENT_SOURCE_DIR}")
endif()

add_executable(urlicht_test ${URLICHT_TEST_SOURCES})

target_compile_features(urlicht_test PRIVATE cxx_std_20)

target_link_libraries(urlicht_test
        PRIVATE
        urlicht
        GTest::gtest
        GTest::gtest_main
)
