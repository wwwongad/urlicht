include(FetchContent)

find_package(benchmark QUIET)
find_package(Boost QUIET)

if(NOT benchmark_FOUND)
    message(STATUS "Google Benchmark not found locally; fetching it from GitHub")

    FetchContent_Declare(
        google_benchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG v1.8.3
    )
    FetchContent_MakeAvailable(google_benchmark)
endif()

file(GLOB_RECURSE URLICHT_BENCHMARK_SOURCES
    CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/algorithm/bm_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/any/bm_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/concurrency/bm_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/container/bm_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/functional/bm_*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/memory/bm_*.cpp"
)

if(NOT URLICHT_BENCHMARK_SOURCES)
    message(FATAL_ERROR "No benchmark sources found in ${CMAKE_CURRENT_SOURCE_DIR}")
endif()


function(urlicht_configure_benchmark target)
    target_link_libraries(${target} PRIVATE Urlicht::urlicht)

    if(Boost_FOUND)
        target_compile_definitions(${target} PRIVATE URLICHT_BM_HAS_BOOST=1)
    else()
        target_compile_definitions(${target} PRIVATE URLICHT_BM_HAS_BOOST=0)
    endif()

    if(MSVC)
        target_compile_options(${target} PRIVATE /O2 /arch:AVX2 /GL)
        target_link_options(${target} PRIVATE /LTCG)
    else()
        target_compile_options(${target} PRIVATE
            -O3
            -march=native
            -mtune=native
            -flto=auto
            -fno-semantic-interposition
        )
    endif()
endfunction()

add_executable(urlicht_benchmark ${URLICHT_BENCHMARK_SOURCES})

target_link_libraries(urlicht_benchmark PRIVATE benchmark::benchmark_main)

urlicht_configure_benchmark(urlicht_benchmark)
