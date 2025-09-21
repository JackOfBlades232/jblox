cmake_minimum_required(VERSION 3.10)
project(cpplox VERSION 0.1 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# @NOTE: non-portable, only works on clang, and possibly just linux
add_compile_options(-stdlib=libc++)
add_link_options(-stdlib=libc++)

configure_file(templates/version.hpp include/version.hpp)
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/cpplox
    ${CMAKE_CURRENT_BINARY_DIR}/include)

add_executable(cpplox src/main.cpp)

