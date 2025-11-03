#pragma once

#include "defs.h"
#include "syscalls.h"
#include "io.h"
#include "fmt.h"

#if _WIN32

// @TODO: dynamic load
#include <windows.h>

// @TODO: dynamic load
#pragma comment(lib, "Advapi32.lib")

typedef struct {
    HANDLE process_hnd;
    usize regular_page_size;
    usize large_page_size; // 0 means disabled
    io_handle_t hstdin;
    io_handle_t hstdout;
    io_handle_t hstderr;
} os_process_state_t;

static inline void os_init_process_state(os_process_state_t *st)
{
    st->process_hnd = GetCurrentProcess();
    st->regular_page_size = 4096;
    // @TODO: init io
}

static inline b32 os_try_enable_large_pages(os_process_state_t *st)
{
    HANDLE token_hnd;

    if (OpenProcessToken(
        st->process_hnd, TOKEN_ADJUST_PRIVILEGES, &token_hnd))
    {
        TOKEN_PRIVILEGES privs = {0};
        privs.PrivilegeCount = 1;
        privs.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        if (LookupPrivilegeValue(
            NULL, SE_LOCK_MEMORY_NAME, &privs.Privileges[0].Luid)) 
        {
            AdjustTokenPrivileges(token_hnd, FALSE, &privs, 0, 0, 0);
            if (GetLastError() == ERROR_SUCCESS)
                st->large_page_size = GetLargePageMinimum();
        }
        
        CloseHandle(token_hnd);
    }

    return st->large_page_size > 0;
}

#else

typedef struct {
    sys_pid_t pid;
    usize regular_page_size;
    char stat_file_name_buf[128];
    io_handle_t hstdin;
    io_handle_t hstdout;
    io_handle_t hstderr;
} os_process_state_t;

static inline void os_init_process_state(os_process_state_t *st)
{
    st->pid = sys_getpid();
    st->regular_page_size = 4096; // @TODO: support properly?
    fmt_sprint(
        st->stat_file_name_buf, sizeof(st->stat_file_name_buf),
        "/proc/%d/stat", st->pid);
    st->hstdin = (io_handle_t){SYS_STDIN_FILENO + 1};
    st->hstdout = (io_handle_t){SYS_STDOUT_FILENO + 1};
    st->hstderr = (io_handle_t){SYS_STDERR_FILENO + 1};
}

static inline b32 os_try_enable_large_pages(os_process_state_t *st)
{
    (void)st;
    return true;
}

static inline void os_exit(int code)
{
    sys_exit(code);
}

#endif

static os_process_state_t g_os_proc_state = {0};

