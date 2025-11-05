cmake_minimum_required(VERSION 3.10)
project(clox VERSION 0.1 LANGUAGES C)
set(CMAKE_EXPORT_COMPILE_COMMANDS True)
SET(CMAKE_C_STANDARD_LIBRARIES "")

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/clox
    ${CMAKE_CURRENT_BINARY_DIR}/include)

if (MSVC)
    # @TODO: bring up to speed
    add_compile_options(/GS- /Zl /INCREMENTAL:NO)
    add_compile_options(
        -Wall -Wextra -Wpedantic -Werror
        -Wno-incompatible-library-redeclaration -Wno-undef
        -Wno-unsafe-buffer-usage -Wno-unused-macros -Wno-missing-prototypes
        -Wno-declaration-after-statement -Wno-pre-c11-compat -Wno-padded
        -Wno-cast-align -Wno-cast-qual -Wno-bad-function-cast
        -Wno-reserved-identifier -Wno-missing-variable-declarations
        -nostdinc -fno-builtin -fno-builtin-memcpy -fno-builtin-memset
        -fno-ident)
    add_link_options(
        /INCREMENTAL:NO /IMPLIB:NO
        /NODEFAULTLIB /ENTRY:start /SUBSYSTEM:CONSOLE)

    # Debug
    add_compile_options(/Zi)

    # Opt
    #add_compile_options(/Oi /O2 /Os)
else()
    add_compile_options(
        -masm=intel
        -Wall -Wextra -Wpedantic -Werror
        -Wno-language-extension-token -Wno-main
        -pedantic -nostdinc -ffreestanding
        -fno-stack-protector -fno-builtin
        -fno-builtin-memcpy -fno-builtin-memset
        -fno-unwind-tables -fno-asynchronous-unwind-tables
        -ffunction-sections -fdata-sections
        -fno-ident)
    add_link_options(
        -static -nostdlib -nodefaultlibs
        -Wl,--build-id=none -Wl,--gc-sections -Wl,--undefined=sys_main)

    # Debug
    add_compile_options(-g)

    # Opt
    #add_compile_options(-Os -O3 -flto)
    #add_link_options(-flto)

    # Strip
    #add_link_options(-s)
endif()

add_executable(clox src/main.c)

