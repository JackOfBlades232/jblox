#pragma once

#include "context.h"
#include "os_types.h"

#if _WIN32

static inline void os_exit(ctx_t const *ctx, int code)
{
    ctx->os->sys.exit_process((uint)code);
}

static inline f64 os_time(ctx_t const *ctx)
{
    win32_large_integer_t now;
    ctx->os->sys.query_performance_counter(&now);
    return
        (f64)(now.q - ctx->os->hr_start.q) * 1000.0 /
        (f64)(ctx->os->hr_freq.q);
}

#else

static inline void os_exit(ctx_t const *ctx, int code)
{
    (void)ctx;
    sys_exit(code);
}

static inline f64 os_time(ctx_t const *ctx)
{
    sys_timespec_t now;
    sys_clock_gettime(SYS_CLOCK_MONOTONIC, &now);
    return
        (f64)(now.tv_sec - ctx->os->hr_start.tv_sec) * 1000.0 +
        (f64)(now.tv_nsec - ctx->os->hr_start.tv_nsec) / 1.0e6;
}

#endif
