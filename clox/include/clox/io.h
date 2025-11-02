#pragma once

#include "defs.h"
#include "fmt.h"
#include "syscalls.h"

#define DEFAULT_IO_BUFSIZE 4096

#if _WIN32

// @TODO

#else

typedef struct {
    int fid;
} io_handle_t;

static inline b32 io_is_valid(io_handle_t const *hnd)
{
    return hnd->fid > 0;
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
    usize chars = fmt_vsprint(buf, sizeof(buf), fmt, args);
    isize written_chars = io_write(hnd, (u8 const *)buf, chars);
    VA_END(args);
    return written_chars;
}

#define OUTPUT(s_) \
    io_write(&g_os_proc_state.hstdout, (u8 *)s_, STRLITLEN(s_) + 1)
#define OUTPUTF(s_, ...) \
    fmt_print(&g_os_proc_state.hstdout, s_ __VA_OPT__(,) __VA_ARGS__)
