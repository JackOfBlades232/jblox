#pragma once

struct os_process_state;
struct gpa;

typedef struct {
    struct os_process_state *os;
    struct gpa *managed_heap;
    struct gpa *unmanaged_heap;
} ctx_t;
