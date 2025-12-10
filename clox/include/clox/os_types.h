#pragma once

#include "defs.h"
#include "syscalls.h"
#include "io_types.h"

#if _WIN32

typedef struct os_process_state {
    win32_syscalls_t sys;

    win32_handle_t process_hnd;
    usize regular_page_size;
    usize large_page_size; // 0 means disabled

    io_handle_t hstdin;
    io_handle_t hstdout;
    io_handle_t hstderr;

    win32_large_integer_t hr_start;
    win32_large_integer_t hr_freq;

    char **argv;
    int argc;
} os_process_state_t;

#else

typedef struct os_process_state {
    sys_pid_t pid;
    usize regular_page_size;
    char stat_file_name_buf[128];

    sys_timespec_t hr_start;

    io_handle_t hstdin;
    io_handle_t hstdout;
    io_handle_t hstderr;

    char **argv;
    int argc;
} os_process_state_t;

#endif
