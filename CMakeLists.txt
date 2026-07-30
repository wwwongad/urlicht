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

option(URLICHT_BUILD_TESTS "Build the unit tests" ON)
option(URLICHT_BUILD_BENCHMARKS "Build benchmarks" OFF)
option(URLICHT_BUILD_EXAMPLES "Build examples" OFF)

if(BUILD_TESTING AND URLICHT_BUILD_TESTS)
    add_subdirectory(tests)
endif()

if(URLICHT_BUILD_BENCHMARKS)
    add_subdirectory(benchmarks)
endif()

if(URLICHT_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

install(DIRECTORY include/urlicht DESTINATION include)
install(TARGETS urlicht EXPORT urlicht-targets)