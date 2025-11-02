#pragma once

#include "defs.h"
#include "memory.h"

typedef struct {
    u8 *data;
    u64 len : 63;
    b32 is_large_pages : 1;
} buffer_t;

static inline b32 buf_is_valid(buffer_t const *b)
{
    return b->data != NULL;
}

static inline buffer_t buf_allocate(u64 bytes)
{
    void *data = os_allocate_pages_memory(bytes);
    return (buffer_t){(u8 *)data, bytes, false};
}

static inline buffer_t buf_allocate_lp(u64 bytes)
{
    void *data = os_allocate_large_pages_memory(bytes);
    return (buffer_t){(u8 *)data, bytes, true};
}

static inline buffer_t buf_allocate_best(u64 bytes)
{
    buffer_t b = buf_allocate_lp(bytes);
    if (buf_is_valid(&b))
        return b;
    else
        return buf_allocate(bytes);
}

static inline void buf_deallocate(buffer_t *buf)
{
    if (buf->data) {
        (buf->is_large_pages ?
            &os_free_large_pages_memory :
            &os_free_pages_memory)(buf->data, buf->len);
        buf->data = NULL;
        buf->len = 0;
        buf->is_large_pages = false;
    }
}

