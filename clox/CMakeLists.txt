cmake_minimum_required(VERSION 3.10)
project(clox VERSION 0.1 LANGUAGES C)
set(CMAKE_EXPORT_COMPILE_COMMANDS True)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/clox
    ${CMAKE_CURRENT_BINARY_DIR}/include)

if (MSVC)
    add_compile_options(/W4)
    add_compile_options(/GS- /Zl)
    add_link_options(/NODEFAULTLIB /ENTRY:_start /SUBSYSTEM:CONSOLE)
else()
    add_compile_options(-g)
    add_compile_options(-masm=intel)
    add_compile_options(
        -Wall -Wextra -pedantic -Wno-language-extension-token)
    add_compile_options(
        -nostdlib -ffreestanding
        -fno-stack-protector -fno-builtin)
    add_link_options(-nostdlib -nodefaultlibs -ffreestanding)
endif()

add_executable(clox src/start.c src/main.c src/os.c src/io.c)

