cmake_minimum_required(VERSION 3.30)
project(AlgorithmTemplates
        VERSION 0.1.0
        DESCRIPTION "Modern C++20/23 template library"
        LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(MSVC)
    add_compile_options(/W4 /WX /permissive-)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()


add_library(urlicht INTERFACE)
target_include_directories(urlicht INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

target_compile_features(urlicht INTERFACE cxx_std_23)


option(URLICHT_BUILD_TESTS "Build tests" OFF)
option(URLICHT_BUILD_BENCHMARKS "Build benchmarks" ON)

if(URLICHT_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

if(URLICHT_BUILD_BENCHMARKS)
    add_subdirectory(benchmarks)
endif()


install(DIRECTORY include/urlicht DESTINATION include)
install(TARGETS urlicht EXPORT urlicht-targets)