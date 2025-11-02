#pragma once

#include "defs.h"

// fwd
static inline isize fmt_sprint(char *buf, usize bufsize, char const *fmt, ...);

#define DECLARE_FMT_SPRINT_INT(type_)                                        \
static inline usize type_ ## _fmt_sprint (char *buf, usize bufsize, type_ n) \
{                                                                            \
    char *p = buf;                                                           \
    usize bs = bufsize;                                                      \
    b32 neg = n < (type_)0;                                                  \
    if (bs && neg) {                                                         \
        *p++ = '-';                                                          \
        --bs;                                                                \
        n = -n;                                                              \
    }                                                                        \
    while (bs && n) {                                                        \
        *p++ = n % (type_)10 + '0';                                          \
        n /= (type_)10;                                                      \
        --bs;                                                                \
    }                                                                        \
    usize len = bufsize - bs;                                                \
    usize off = neg ? 1 : 0;                                                 \
    for (usize i = off; i < off + (len - off) / 2; ++i)                      \
        SWAP(buf[i], buf[len + off - i - 1], char);                          \
    return len;                                                              \
}
#define X(t_) DECLARE_FMT_SPRINT_INT(t_)
XITYPES
#undef X
#define FMT_SPRINT_INT(buf_, bufsize_, n_) \
    GENF_ITYPES(n_, fmt_sprint)((buf_), (bufsize_), (n_))

// Doesn't do special numbers
static inline usize fmt_sprint_double(char *buf, usize bufsize, f64 n)
{
    u64 bits = *(u64 *)&n;
    b32 s = bits >> 63;
    i64 e = (bits >> 52) & ~(1 << 11) - 1023;
    u64 m = bits & ((1ll << 52) - 1);

    u64 mantissa_with_one = m | (1ull << 52);
    i64 shift = e - 52;

    u64 w, f;
    if (shift >= 0) {
        w = mantissa_with_one << shift;
        f = 0;
    } else {
        w = mantissa_with_one >> -shift;
        f = mantissa_with_one & ((1ull << -shift) - 1);
    }

    usize len = FMT_SPRINT_INT(buf, bufsize, (i64)w * (s ? -1 : 1));
    if (f && len < bufsize) {
        buf[len++] = '.';
        len += FMT_SPRINT_INT(buf + len, bufsize - len, f);
    }
    
    return len;
}

static inline usize fmt_sprint_ptr(char *buf, usize bufsize, void const *ptr)
{
    usize n = (usize)ptr;
    char *p = buf;
    usize bs = bufsize;
    while (bs && n) {
        int d = n % 16;
        *p++ = d >= 10 ? (d - 10 + 'a') : (d + '0');
        n /= 16;
        --bs;
    }
    usize len = bufsize - bs;
    for (usize i = 0; i < len / 2; ++i)
        SWAP(buf[i], buf[len - i - 1], char);
    return len;
}

// Non-standard
static inline isize fmt_vsprint(
    char *buf, usize bufsize, char const *fmt, VA_LIST args)
{
    if (!bufsize)
        return 0;

    char *dst = buf;
    for (char *end = buf + bufsize - 1; dst < end && *fmt; ++fmt) {
        switch (*fmt) {
        case '%': {
            ++fmt;
            switch (*fmt) {
            case '%':
                *dst++ = '%';
                break;
            case 'c': {
                int c = VA_ARG(args, int);
                *dst++ = c;
            } break;
            case 'd': {
                i32 n = VA_ARG(args, i32);
                dst += FMT_SPRINT_INT(dst, end - dst, n);
            } break;
            case 'u': {
                u32 n = VA_ARG(args, u32);
                dst += FMT_SPRINT_INT(dst, end - dst, n);
            } break;
            case 'D': {
                i64 n = VA_ARG(args, i64);
                dst += FMT_SPRINT_INT(dst, end - dst, n);
            } break;
            case 'U': {
                u64 n = VA_ARG(args, u64);
                dst += FMT_SPRINT_INT(dst, end - dst, n);
            } break;
            case 'f': {
                f64 n = VA_ARG(args, f64);
                dst += fmt_sprint_double(dst, end - dst, n);
            } break;
            case 'p': {
                void const *p = VA_ARG(args, void const *);
                dst += fmt_sprint_ptr(dst, end - dst, p);
            } break;
            case 's': {
                char const *p = VA_ARG(args, char const *);
                while (dst < end && *p)
                    *dst++ = *p++;
            } break;
            // @TODO: length-ed string
            }
        } break;

        default:
            *dst++ = *fmt;
            break;
        }
    }

    *dst = '\0';
    return dst - buf;
}

static inline isize fmt_sprint(char *buf, usize bufsize, char const *fmt, ...)
{
    VA_LIST args;
    VA_START(args, fmt);
    usize ret = fmt_vsprint(buf, bufsize, fmt, args);
    VA_END(args);
    return ret;
}

