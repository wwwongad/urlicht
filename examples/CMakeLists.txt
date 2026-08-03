file(GLOB_RECURSE URLICHT_EXAMPLE_SOURCES CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/*/*.cpp"
)

if(NOT URLICHT_EXAMPLE_SOURCES)
    message(FATAL_ERROR "No example sources found in ${CMAKE_CURRENT_SOURCE_DIR}")
endif()

foreach(source IN LISTS URLICHT_EXAMPLE_SOURCES)
    get_filename_component(name "${source}" NAME_WE)
    add_executable("urlicht_${name}" "${source}")
    target_link_libraries("urlicht_${name}" PRIVATE Urlicht::urlicht)
    target_compile_features("urlicht_${name}" PRIVATE cxx_std_20)
endforeach()
