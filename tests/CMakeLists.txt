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
    "${CMAKE_CURRENT_SOURCE_DIR}/scope/test_*.cpp"
)

if(NOT URLICHT_TEST_SOURCES)
    message(FATAL_ERROR "No test sources found in ${CMAKE_CURRENT_SOURCE_DIR}")
endif()

add_executable(urlicht_test ${URLICHT_TEST_SOURCES})

if(MSVC)
    target_compile_options(urlicht_test PRIVATE /W4 /Zi)
else()
    target_compile_options(urlicht_test PRIVATE -g3 -Wall -Wextra -Wpedantic)
endif()

target_link_libraries(urlicht_test
    PRIVATE
    Urlicht::urlicht
    GTest::gtest_main
)


if(URLICHT_TEST_SANITIZERS)
    if(URLICHT_TEST_SANITIZERS MATCHES "^(ON|TRUE|YES|Y|1)$")
        set(URLICHT_TEST_SANITIZERS "address,undefined")
    endif()

    string(REPLACE "," ";" _urlicht_sanitizers "${URLICHT_TEST_SANITIZERS}")
    list(REMOVE_ITEM _urlicht_sanitizers "")
    list(JOIN _urlicht_sanitizers "," _urlicht_sanitize_flag)

    if(_urlicht_sanitize_flag MATCHES "(^|,)address(,|$)"
       AND _urlicht_sanitize_flag MATCHES "(^|,)thread(,|$)")
        message(FATAL_ERROR
            "URLICHT_TEST_SANITIZERS: 'address' and 'thread' cannot be enabled together; "
            "configure two separate build trees to run both.")
    endif()

    if(MSVC)
        if(NOT _urlicht_sanitize_flag STREQUAL "address")
            message(WARNING
                "MSVC only supports the 'address' sanitizer; ignoring "
                "'${_urlicht_sanitize_flag}' and using /fsanitize=address.")
        endif()
        message(STATUS "Urlicht tests: sanitizers enabled (/fsanitize=address)")
        target_compile_options(urlicht_test PRIVATE /fsanitize=address)
        target_link_options(urlicht_test PRIVATE /INCREMENTAL:NO)
    else()
        message(STATUS "Urlicht tests: sanitizers enabled (-fsanitize=${_urlicht_sanitize_flag})")
        target_compile_options(urlicht_test PRIVATE
            -fsanitize=${_urlicht_sanitize_flag}
            -fno-omit-frame-pointer
        )
        target_link_options(urlicht_test PRIVATE
            -fsanitize=${_urlicht_sanitize_flag}
        )
    endif()
endif()

if(URLICHT_TEST_SANITIZERS)
    gtest_discover_tests(urlicht_test DISCOVERY_MODE PRE_TEST DISCOVERY_TIMEOUT 120)
else()
    gtest_discover_tests(urlicht_test)
endif()
