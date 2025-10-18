cmake_minimum_required(VERSION 3.10)
project(clox VERSION 0.1 LANGUAGES C)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/clox
    ${CMAKE_CURRENT_BINARY_DIR}/include)

add_executable(clox src/main.c)

