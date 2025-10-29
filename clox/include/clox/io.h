#pragma once

#include "defs.h"

#define DEFAULT_IO_BUFSIZE 4096

#if _WIN32

// @TODO

#else

#include <unistd.h>

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
    return write(hnd->fid - 1, bytes, byte_count);
}

static inline isize io_read(
    io_handle_t *hnd, u8 *out_bytes, usize out_byte_count)
{
    return read(hnd->fid - 1, out_bytes, out_byte_count);
}

#endif
