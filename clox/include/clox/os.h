#pragma once

#include "defs.h"
#include "syscalls.h"
#include "io_types.h"

#if _WIN32

typedef struct {
    win32_syscalls_t sys;

    win32_handle_t process_hnd;
    usize regular_page_size;
    usize large_page_size; // 0 means disabled

    io_handle_t hstdin;
    io_handle_t hstdout;
    io_handle_t hstderr;

    char **argv;
    int argc;
} os_process_state_t;

#else

typedef struct {
    sys_pid_t pid;
    usize regular_page_size;
    char stat_file_name_buf[128];

    io_handle_t hstdin;
    io_handle_t hstdout;
    io_handle_t hstderr;

    char **argv;
    int argc;
} os_process_state_t;

#endif

static os_process_state_t g_os = {0};

#if _WIN32

static inline void os_exit(int code)
{
    g_os.sys.exit_process((uint)code);
}

#else

static inline void os_exit(int code)
{
    sys_exit(code);
}

#endif
