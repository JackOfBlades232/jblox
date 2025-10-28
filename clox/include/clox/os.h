#pragma once

#include "defs.h"

#if _WIN32

#include <windows.h>

#pragma comment(lib, "Advapi32.lib")

typedef struct {
    HANDLE process_hnd;
    usize regular_page_size;
    usize large_page_size; // 0 means disabled
} os_process_state_t;

static inline void init_os_process_state(os_process_state_t *st)
{
    st->process_hnd = GetCurrentProcess();
    st->regular_page_size = 4096;
}

static inline b32 try_enable_large_pages(os_process_state_t *st)
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

static inline i64 get_last_os_error()
{
    return (i64)GetLastError();
}

#else

#include <unistd.h>

typedef struct {
    pid_t pid;
    usize regular_page_size;
    char stat_file_name_buf[128];
} os_process_state_t;

static inline void init_os_process_state(os_process_state_t *st)
{
    st->pid = getpid();
    st->regular_page_size = (usize)getpagesize();
    snprintf(
        st->stat_file_name_buf, sizeof(st->stat_file_name_buf),
        "/proc/%d/stat", st->pid);
}

static inline b32 try_enable_large_pages(os_process_state_t *st)
{
    (void)st;
    return true;
}

static inline i64 get_last_os_error()
{
    return (i64)errno;
}

#endif

extern os_process_state_t g_os_proc_state;

