find_package(Boost QUIET)

add_library(urlicht_adaptor_bm INTERFACE)

file(GLOB URLICHT_BM_ADAPTOR_FILE
        "${CMAKE_CURRENT_SOURCE_DIR}/bm_*.cpp"
)

if(EXISTS "${URLICHT_BM_ADAPTOR_FILE}")
    if(Boost_FOUND)
        message(STATUS "Boost found for urlicht/benchmark/adaptor")

        target_compile_definitions(urlicht_adaptor_bm INTERFACE
                URLICHT_BM_ADAPTOR_HAS_BOOST=1
        )
        target_include_directories(urlicht_adaptor_bm INTERFACE
                ${Boost_INCLUDE_DIRS}
        )
    else()
        message(STATUS "Boost not found for urlicht/benchmark/adaptor")

        target_compile_definitions(urlicht_adaptor_bm INTERFACE
                URLICHT_BM_ADAPTOR_HAS_BOOST=0
        )
    endif()
endif()