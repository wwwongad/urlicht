find_package(GTest QUIET)

if(NOT GTest_FOUND)
    include(FetchContent)

    FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip
    )
    if(MSVC)
        set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    endif()
    FetchContent_MakeAvailable(googletest)
endif()

include(GoogleTest)

file(GLOB_RECURSE URLICHT_TEST_SOURCES
    CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/algorithm/test_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/any/test_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/concurrency/test_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/container/test_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/functional/test_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/memory/test_*.cpp"
)

if(NOT URLICHT_TEST_SOURCES)
    message(FATAL_ERROR "No test sources found in ${CMAKE_CURRENT_SOURCE_DIR}")
endif()

add_executable(urlicht_test ${URLICHT_TEST_SOURCES})

if(MSVC)
    target_compile_options(urlicht_test PRIVATE /W4)
else()
    target_compile_options(urlicht_test PRIVATE -Wall -Wextra -Wpedantic)
endif()

target_link_libraries(urlicht_test
    PRIVATE
    Urlicht::urlicht
    GTest::gtest_main
)

gtest_discover_tests(urlicht_test)
