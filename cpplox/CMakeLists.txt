cmake_minimum_required(VERSION 3.10)
project(cpplox VERSION 0.0.1 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# @NOTE: non-portable, only works on clang, and possibly just linux
add_compile_options(-stdlib=libc++)
add_link_options(-stdlib=libc++)

add_executable(cpplox main.cpp)

