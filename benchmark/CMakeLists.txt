include(FetchContent)

find_package(benchmark QUIET)

if(NOT benchmark_FOUND)
    message(STATUS "Google Benchmark not found locally, fetching from GitHub...")
    FetchContent_Declare(
            google_benchmark
            GIT_REPOSITORY https://github.com/google/benchmark.git
            GIT_TAG        v1.8.3 
    )
    FetchContent_MakeAvailable(google_benchmark)
endif()

file(GLOB_RECURSE URLICHT_BENCHMARK_SOURCES
        CONFIGURE_DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/any/bm_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/concurrency/bm_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/container/bm_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/functional/bm_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/memory/bm_*.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/algorithm/bm_*.cpp"
)

add_executable(urlicht_benchmark
        ${URLICHT_BENCHMARK_SOURCES}
)

target_link_libraries(urlicht_benchmark
        PRIVATE
        benchmark::benchmark
        benchmark::benchmark_main
        urlicht
)

if(MSVC)
    target_compile_options(urlicht_benchmark PRIVATE /O2 /DNDEBUG /arch:AVX2)
else()
    target_compile_options(urlicht_benchmark PRIVATE -O3 -DNDEBUG -march=native)
endif()

set_target_properties(urlicht_benchmark PROPERTIES
        ENVIRONMENT "LD_PRELOAD=/usr/lib64/libmimalloc.so"
)
