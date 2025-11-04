#pragma once

#include "io_types.h"
#include "os.h"
#include "defs.h"
#include "fmt.h"

#if _WIN32

static inline b32 io_is_valid(io_handle_t const *hnd)
{
    return hnd->hnd != NULL;
}

static inline isize io_write(
    io_handle_t *hnd, u8 const *bytes, usize byte_count)
{
    u32 written_byte_count = 0;
    g_os.sys.write_file(
        hnd->hnd, bytes, (u32)byte_count, &written_byte_count, NULL);
    return written_byte_count;
}

static inline isize io_read(
    io_handle_t *hnd, u8 *out_bytes, usize out_byte_count)
{
    u32 read_byte_count = 0;
    g_os.sys.read_file(
        hnd->hnd, out_bytes, (u32)out_byte_count, &read_byte_count, NULL);
    return read_byte_count;
}

#else

#include "syscalls.h"

static inline b32 io_is_valid(io_handle_t const *hnd)
{
    return hnd->fid >= 0;
}

static inline isize io_write(
    io_handle_t *hnd, u8 const *bytes, usize byte_count)
{
    return sys_write(hnd->fid - 1, bytes, byte_count);
}

static inline isize io_read(
    io_handle_t *hnd, u8 *out_bytes, usize out_byte_count)
{
    return sys_read(hnd->fid - 1, out_bytes, out_byte_count);
}

#endif

static inline isize fmt_print(io_handle_t *hnd, char const *fmt, ...)
{
    char buf[256]; // Lazy...
    VA_LIST args;
    VA_START(args, fmt);
    usize chars = (usize)fmt_vsprint(buf, sizeof(buf), fmt, args);
    isize written_chars = io_write(hnd, (u8 const *)buf, chars);
    VA_END(args);
    return written_chars;
}

#define OUTPUT(s_) \
    io_write(&g_os.hstdout, (u8 *)s_, STRLITLEN(s_) + 1)
#define OUTPUTF(s_, ...) \
    fmt_print(&g_os.hstdout, s_ __VA_OPT__(,) __VA_ARGS__)
