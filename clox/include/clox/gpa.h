#pragma once

#include "buffer.h"
#include "debug.h"

typedef struct gpa_free_header {
    usize size;
    struct gpa_free_header *next;
} gpa_free_header_t;

typedef struct {
    usize size;
} gpa_allocated_header_t;

typedef struct gpa {
    buffer_t memory;
    gpa_free_header_t *head;
} gpa_t;

#define GPA_ALIGNMENT 16
_Static_assert(
    GPA_ALIGNMENT % _Alignof(gpa_free_header_t) == 0,
    "GPA min alignment must be valid for free header");
_Static_assert(
    GPA_ALIGNMENT % _Alignof(gpa_allocated_header_t) == 0,
    "GPA min alignment must be valid for allocated header");
_Static_assert(
    sizeof(gpa_allocated_header_t) <= GPA_ALIGNMENT,
    "GPA min alignment must be less then allocated header size");
_Static_assert(sizeof(gpa_free_header_t) <= GPA_ALIGNMENT,
    "GPA min alignment must be less then free header size");

static inline void *gpa_allocate(gpa_t *gpa, usize bytes)
{
    gpa_free_header_t *free = gpa->head, *prev = NULL;
    usize const real_byte_size =
        ROUND_UP(GPA_ALIGNMENT + bytes, GPA_ALIGNMENT);
    usize const min_blocks = real_byte_size / GPA_ALIGNMENT;

    while (free) {
        usize free_size = free->size;
        gpa_free_header_t *next = free->next;
        if (free_size >= min_blocks) {

            gpa_allocated_header_t *alloc = (gpa_allocated_header_t *)free;
            alloc->size = min_blocks;

            if (free_size > min_blocks) {
                gpa_free_header_t *new_header =
                    (gpa_free_header_t *)((u8 *)free + real_byte_size);
                new_header->size = free_size - min_blocks;
                new_header->next = next;
                next = new_header;
            }

            if (prev)
                prev->next = next;
            else
                gpa->head = next;

            return (u8 *)alloc + GPA_ALIGNMENT;
        }

        free = next;
    }

    return NULL;
}

static inline void gpa_deallocate(gpa_t *gpa, void const *p)
{
    gpa_allocated_header_t *alloc =
        (gpa_allocated_header_t *)((u8 *)p - GPA_ALIGNMENT);
    usize size = alloc->size;

    gpa_free_header_t *free = (gpa_free_header_t *)alloc;

    // If next to each other, coalesce
    while (gpa->head ==
        (gpa_free_header_t *)((u8 *)free + size * GPA_ALIGNMENT))
    {
        size += gpa->head->size;
        gpa_free_header_t *next = gpa->head->next;
        gpa->head = next;
    }

    free->size = size;
    free->next = gpa->head;
    gpa->head = free;
}

static inline gpa_t gpa_make(ctx_t const *ctx, buffer_t mem)
{
    ASSERT(mem.len >= GPA_ALIGNMENT);
    ASSERT(mem.len % GPA_ALIGNMENT == 0);
    gpa_free_header_t *free = (gpa_free_header_t *)mem.data;
    free->size = mem.len / GPA_ALIGNMENT;
    free->next = NULL;
    return (gpa_t){mem, free};
}
