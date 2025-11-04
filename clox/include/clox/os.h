#pragma once

#include "context.h"
#include "os_types.h"

#if _WIN32

static inline void os_exit(ctx_t const *ctx, int code)
{
    ctx->os->sys.exit_process((uint)code);
}

#else

static inline void os_exit(ctx_t const *ctx, int code)
{
    (void)ctx;
    sys_exit(code);
}

#endif
