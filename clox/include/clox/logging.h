#pragma once

#include "io.h"
#include "os.h"

#define LOG(s_) \
    io_write(ctx, &ctx->os->hstderr, (u8 *)(s_ "\n"), STRLITLEN(s_) + 1)
#define LOGF(s_, ...) \
    fmt_print(ctx, &ctx->os->hstderr, s_ "\n" __VA_OPT__(,) __VA_ARGS__)

#define LOG_NONL(s_) \
    io_write(ctx, &ctx->os->hstderr, (u8 *)(s_), STRLITLEN(s_) + 1)
#define LOGF_NONL(s_, ...) \
    fmt_print(ctx, &ctx->os->hstderr, s_ __VA_OPT__(,) __VA_ARGS__)
