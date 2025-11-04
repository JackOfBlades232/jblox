#pragma once

struct os_process_state;
struct gpa;

typedef struct {
    struct os_process_state *os;
    struct gpa *gpa;
} ctx_t;
