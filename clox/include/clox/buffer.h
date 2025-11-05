#pragma once

#include "defs.h"
#include "memory.h"
#include "context.h"

typedef struct {
    u8 *data;
    u64 len : 63;
    b64 is_large_pages : 1;
} buffer_t;

static inline b32 buf_is_valid(ctx_t const *ctx, buffer_t const *b)
{
    (void)ctx;
    return b->data != NULL;
}

static inline buffer_t buf_allocate(ctx_t const *ctx, u64 bytes)
{
    void *data = os_allocate_pages_memory(ctx, bytes);
    return (buffer_t){(u8 *)data, bytes, false};
}

static inline buffer_t buf_allocate_lp(ctx_t const *ctx, u64 bytes)
{
    void *data = os_allocate_large_pages_memory(ctx, bytes);
    return (buffer_t){(u8 *)data, bytes, true};
}

static inline buffer_t buf_allocate_best(ctx_t const *ctx, u64 bytes)
{
    buffer_t b = buf_allocate_lp(ctx, bytes);
    if (!buf_is_valid(ctx, &b))
        b = buf_allocate(ctx, bytes);
    return b;
}

static inline void buf_deallocate(ctx_t const *ctx, buffer_t *buf)
{
    if (buf->data) {
        (buf->is_large_pages ?
            &os_free_large_pages_memory :
            &os_free_pages_memory)(ctx, buf->data, buf->len);
        buf->data = NULL;
        buf->len = 0;
        buf->is_large_pages = false;
    }
}

