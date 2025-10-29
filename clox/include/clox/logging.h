#pragma once

#include "os.h"

#define LOG(s_) \
    io_write(&g_os_proc_state.hstdin, (u8 *)(s_ "\n"), STRLITLEN(s_) + 1)
