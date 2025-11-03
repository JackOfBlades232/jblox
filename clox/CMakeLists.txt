cmake_minimum_required(VERSION 3.10)
project(clox VERSION 0.1 LANGUAGES C)
set(CMAKE_EXPORT_COMPILE_COMMANDS True)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/clox
    ${CMAKE_CURRENT_BINARY_DIR}/include)

if (MSVC)
    # @TODO: bring up to speed
    add_compile_options(/W4)
    add_compile_options(/GS- /Zl)
    add_link_options(/NODEFAULTLIB /ENTRY:_start /SUBSYSTEM:CONSOLE)
else()
    add_compile_options(
        -masm=intel
        -Wall -Wextra -pedantic -Wno-language-extension-token
        -nostdinc -ffreestanding
        -fno-stack-protector -fno-builtin
        -fno-builtin-memcpy -fno-builtin-memset
        -fno-unwind-tables -fno-asynchronous-unwind-tables
        -ffunction-sections -fdata-sections
        -fno-ident)
    add_link_options(
        -static -nostdlib -nodefaultlibs
        -Wl,--build-id=none -Wl,--gc-sections -Wl,--undefined=main)

    # Debug
    add_compile_options(-g)

    # Opt
    #add_compile_options(-Os -O2 -flto)
    #add_link_options(-flto)

    # Strip
    #add_link_options(-s)
endif()

add_executable(clox src/main.c)

