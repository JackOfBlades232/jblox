#pragma once

#include "syscalls.h"

#if _WIN32

typedef struct {
    win32_handle_t hnd;
} io_handle_t;

#else

typedef struct {
    int fid;
} io_handle_t;

#endif
