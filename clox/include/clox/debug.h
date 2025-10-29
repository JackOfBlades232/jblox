#pragma once

#include "logging.h"

#if NDEBUG

#define ASSERT(...)

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

#define STATIC_ASSERT(e_, s_) _Static_assert((e_), s_)
