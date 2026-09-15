cmake_minimum_required(VERSION 3.30)
project(
    Urlicht
    VERSION 0.1.0
    DESCRIPTION "Modern C++20/23 template library"
    LANGUAGES CXX
)

include(CTest)

add_library(urlicht INTERFACE)
add_library(Urlicht::urlicht ALIAS urlicht)

target_include_directories(urlicht INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
target_compile_features(urlicht INTERFACE cxx_std_20)

option(URLICHT_BUILD_TESTS "Build the unit tests" OFF)
option(URLICHT_BUILD_BENCHMARKS "Build benchmarks" OFF)
option(URLICHT_BUILD_EXAMPLES "Build examples" OFF)

# Enable with:
#   cmake -S . -B build -DURLICHT_TEST_SANITIZERS=address,undefined
#   cmake -S . -B build -DURLICHT_TEST_SANITIZERS=thread
#   cmake -S . -B build -DURLICHT_TEST_SANITIZERS=ON     # shorthand for address,undefined
# An empty value (the default) disables sanitizing.

# NOTE: the cache docstring must NOT contain a semicolon.
set(URLICHT_TEST_SANITIZERS "" CACHE STRING
    "Sanitizers for the test target (address,undefined,thread,leak). Empty disables. ON means address,undefined")

if(BUILD_TESTING AND URLICHT_BUILD_TESTS)
    add_subdirectory(tests)
elseif(URLICHT_TEST_SANITIZERS)
    message(WARNING
        "URLICHT_TEST_SANITIZERS is set ('${URLICHT_TEST_SANITIZERS}') but the tests are disabled;
         it will have no effect.")
endif()

if(URLICHT_BUILD_BENCHMARKS)
    add_subdirectory(benchmarks)
endif()

if(URLICHT_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

install(DIRECTORY include/urlicht DESTINATION include)
install(TARGETS urlicht EXPORT urlicht-targets)