#pragma once

#include "io_types.h"
#include "context.h"
#include "defs.h"
#include "fmt.h"
#include "string.h"

#if _WIN32

static inline b32 io_is_valid(ctx_t const *ctx, io_handle_t const *hnd)
{
    (void)ctx;
    return hnd->hnd != NULL;
}

static inline isize io_write(ctx_t const *ctx,
    io_handle_t *hnd, u8 const *bytes, usize byte_count)
{
    u32 written_byte_count = 0;
    ctx->os->sys.write_file(
        hnd->hnd, bytes, (u32)byte_count, &written_byte_count, NULL);
    return written_byte_count;
}

static inline isize io_read(ctx_t const *ctx,
    io_handle_t *hnd, u8 *out_bytes, usize out_byte_count)
{
    u32 read_byte_count = 0;
    ctx->os->sys.read_file(
        hnd->hnd, out_bytes, (u32)out_byte_count, &read_byte_count, NULL);
    return read_byte_count;
}

#else

#include "syscalls.h"

static inline b32 io_is_valid(ctx_t const *ctx, io_handle_t const *hnd)
{
    (void)ctx;
    return hnd->fid >= 0;
}

static inline isize io_write(ctx_t const *ctx,
    io_handle_t *hnd, u8 const *bytes, usize byte_count)
{
    (void)ctx;
    return sys_write(hnd->fid - 1, bytes, byte_count);
}

static inline isize io_read(ctx_t const *ctx,
    io_handle_t *hnd, u8 *out_bytes, usize out_byte_count)
{
    (void)ctx;
    return sys_read(hnd->fid - 1, out_bytes, out_byte_count);
}

#endif

static inline b32 io_file_is_valid(ctx_t const *ctx, io_file_t const *f)
{
    return io_is_valid(ctx, &f->ioh);
}

#if _WIN32

static inline io_file_t io_read_open_file(ctx_t const *ctx, char const *fn)
{
    io_file_t f = {0};

    f.ioh.hnd = ctx->os->sys.create_file_a(
        fn, WIN32_GENERIC_READ, 0, NULL,
        WIN32_OPEN_EXISTING, WIN32_FILE_ATTRIBUTE_NORMAL, NULL);
    if (!io_is_valid(ctx, &f.ioh))
        return f;

    u32 len_lo = 0, len_hi = 0;
    len_lo = ctx->os->sys.get_file_size(f.ioh.hnd, &len_hi);
    f.len = ((usize)len_hi << 32) | (usize)len_lo;

    return f;
}

static inline void io_close_file(ctx_t const *ctx, io_file_t *f)
{
    if (io_file_is_valid(ctx, f)) {
        ctx->os->sys.close_handle(f->ioh.hnd);
        *f = (io_file_t){0};
    }
}

#else

static inline io_file_t io_read_open_file(ctx_t const *ctx, char const *fn)
{
    io_file_t f = {0};

    f.ioh.fid = sys_open(fn, SYS_O_RDONLY, 0) + 1;
    if (!io_is_valid(ctx, &f.ioh))
        return f;

    f.len = sys_lseek(f.ioh.fid - 1, 0, SYS_SEEK_END);
    sys_lseek(f.ioh.fid - 1, 0, SYS_SEEK_SET);

    return f;
}

static inline void io_close_file(ctx_t const *ctx, io_file_t *f)
{
    if (io_file_is_valid(ctx, f)) {
        sys_close(f->ioh.fid - 1);
        *f = (io_file_t){0};
    }
}

#endif

static inline isize fmt_print(ctx_t const *ctx,
    io_handle_t *hnd, char const *fmt, ...)
{
    char buf[256]; // Lazy...
    VA_LIST args;
    VA_START(args, fmt);
    usize chars = (usize)fmt_vsprint(ctx, buf, sizeof(buf), fmt, args);
    isize written_chars = io_write(ctx, hnd, (u8 const *)buf, chars);
    VA_END(args);
    return written_chars;
}

#define OUTPUT(s_) \
    io_write(ctx, &ctx->os->hstdout, (u8 *)s_, STRLITLEN(s_))
#define OUTPUTF(s_, ...) \
    fmt_print(ctx, &ctx->os->hstdout, s_ __VA_OPT__(,) __VA_ARGS__)
#define OUTPUTS(s_) \
    io_write(ctx, &ctx->os->hstdout, (u8 *)(s_).p, (s_).len)
