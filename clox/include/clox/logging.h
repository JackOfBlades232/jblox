#pragma once

#include "io.h"
#include "os.h"

#define LOG(s_) \
    io_write(&g_os_proc_state.hstderr, (u8 *)(s_ "\n"), STRLITLEN(s_) + 1)
#define LOGF(s_, ...) \
    fmt_print(&g_os_proc_state.hstderr, s_ "\n" __VA_OPT__(,) __VA_ARGS__)
