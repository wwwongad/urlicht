find_package(benchmark REQUIRED)
find_package(Boost REQUIRED)

add_executable(urlicht_benchmark
        memory/bm_arena.cpp
)


target_link_libraries(urlicht_benchmark
        PRIVATE
        benchmark::benchmark
        benchmark::benchmark_main
        urlicht
)

set_target_properties(urlicht_benchmark PROPERTIES
        VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
)