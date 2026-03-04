find_package(benchmark REQUIRED)
find_package(Boost REQUIRED)

add_executable(urlicht_benchmark
        procedural/bm_lower_bound.cpp
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
        VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
)
