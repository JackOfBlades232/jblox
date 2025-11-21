#pragma once

#include "logging.h"

#if NDEBUG

#define ASSERT(e_) ((void)e_)
#define DEBUG_BREAK

#else

#if _WIN32

#define DEBUG_BREAK __debugbreak()

#else

#define DEBUG_BREAK asm("int3")

#endif

#define ASSERT(e_)                              \
    do {                                        \
        if (!(e_)) {                            \
            LOG("Assertion (" #e_ ") failed."); \
            DEBUG_BREAK;                        \
        }                                       \
    } while (0)

#endif

#define PANIC(msg_) do { LOG("PANIC: " msg_); os_exit(ctx, 13); } while (0)
#define VERIFY(e_, msg_) do { if (!(e_)) PANIC(msg_); } while (0)
