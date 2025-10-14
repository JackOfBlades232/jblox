cmake_minimum_required(VERSION 3.10)
project(cpplox VERSION 0.1 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if(UNIX)
    add_compile_options(-stdlib=libc++)
    add_link_options(-stdlib=libc++)
endif(UNIX)

add_compile_options(-Wno-binding-in-condition)

configure_file(templates/version.hpp include/version.hpp)

add_executable(metaprogram metaprogram/main.cpp)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/ast.hpp
    COMMAND metaprogram ${CMAKE_CURRENT_BINARY_DIR}/include
    DEPENDS metaprogram
    VERBATIM)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/cpplox
    ${CMAKE_CURRENT_BINARY_DIR}/include)

add_executable(
    cpplox src/main.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/include/ast.hpp
    ${CMAKE_CURRENT_BINARY_DIR}/include/version.hpp)

