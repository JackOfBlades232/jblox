#pragma once

struct os_process_state;
struct gpa;

typedef struct {
    struct os_process_state *os;
    struct gpa *gpa;
} ctx_t;

#define PUSH_CONTEXT(name_, ...) ctx_t name_ = (ctx_t){__VA_ARGS__}
#define SETUP_CONTEXT(name_) ctx_t *ctx = &(name_)
