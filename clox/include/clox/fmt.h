#pragma once

#include "defs.h"

// fwd
static inline isize fmt_sprint(char *buf, usize bufsize, char const *fmt, ...);

#define X(type_)                                                             \
static inline usize type_ ## _fmt_sprint (char *buf, usize bufsize, type_ n) \
{                                                                            \
    char *p = buf;                                                           \
    usize bs = bufsize;                                                      \
    b32 neg = n < (type_)0;                                                  \
    if (bs && neg) {                                                         \
        *p++ = '-';                                                          \
        --bs;                                                                \
        n = ((type_)-n);                                                     \
    }                                                                        \
    while (bs && n) {                                                        \
        *p++ = n % (type_)10 + '0';                                          \
        n /= (type_)10;                                                      \
        --bs;                                                                \
    }                                                                        \
    usize off = neg ? 1 : 0;                                                 \
    usize len = bufsize - bs;                                                \
    if (len == off) {                                                        \
        *p++ = '0';                                                          \
        --bs;                                                                \
        ++len;                                                               \
    }                                                                        \
    for (usize i = off; i < off + (len - off) / 2; ++i)                      \
        SWAP(buf[i], buf[len + off - i - 1], char);                          \
    return len;                                                              \
}
XSITYPES
#undef X
#define X(type_)                                                             \
static inline usize type_ ## _fmt_sprint (char *buf, usize bufsize, type_ n) \
{                                                                            \
    char *p = buf;                                                           \
    usize bs = bufsize;                                                      \
    while (bs && n) {                                                        \
        *p++ = n % (type_)10 + '0';                                          \
        n /= (type_)10;                                                      \
        --bs;                                                                \
    }                                                                        \
    usize len = bufsize - bs;                                                \
    if (len == 0) {                                                          \
        *p++ = '0';                                                          \
        --bs;                                                                \
        ++len;                                                               \
    }                                                                        \
    for (usize i = 0; i < len / 2; ++i)                                      \
        SWAP(buf[i], buf[len - i - 1], char);                                \
    return len;                                                              \
}
XUITYPES
#undef X
#define FMT_SPRINT_INT(buf_, bufsize_, n_) \
    GENF_ITYPES(n_, fmt_sprint)((buf_), (bufsize_), (n_))

// Doesn't do special numbers
static inline usize fmt_sprint_double(char *buf, usize bufsize, f64 n)
{
    u64 bits = *(u64 *)&n;
    b32 s = bits >> 63;
    i64 e = ((bits >> 52) & ((1 << 11) - 1)) - 1023;
    u64 m = bits & ((1ll << 52) - 1);

    u64 mantissa_with_one = m | (1ull << 52);
    i64 shift = e - 52;

    u64 w, f;
    if (shift >= 0) {
        w = mantissa_with_one << shift;
        f = 0;
    } else {
        w = mantissa_with_one >> -shift;
        f = (mantissa_with_one & ((1ull << -shift) - 1)) << e;
    }

    usize len = FMT_SPRINT_INT(buf, bufsize, (i64)w * (s ? -1 : 1));
    if (f && len < bufsize) {
        buf[len++] = '.';
        uint significant_digits = 15;
        while (f && len < bufsize && significant_digits--) {
            f *= 10;
            u64 digit = f >> 52;
            buf[len++] = (char)digit + '0';
            f &= ((1ull << 52) - 1);
        }
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
        *p++ = d >= 10 ? ((char)d - 10 + 'a') : ((char)d + '0');
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
                *dst++ = (char)c;
            } break;
            case 'd': {
                i32 n = VA_ARG(args, i32);
                dst += FMT_SPRINT_INT(dst, (usize)(end - dst), n);
            } break;
            case 'u': {
                u32 n = VA_ARG(args, u32);
                dst += FMT_SPRINT_INT(dst, (usize)(end - dst), n);
            } break;
            case 'D': {
                i64 n = VA_ARG(args, i64);
                dst += FMT_SPRINT_INT(dst, (usize)(end - dst), n);
            } break;
            case 'U': {
                u64 n = VA_ARG(args, u64);
                dst += FMT_SPRINT_INT(dst, (usize)(end - dst), n);
            } break;
            case 'f': {
                f64 n = VA_ARG(args, f64);
                dst += fmt_sprint_double(dst, (usize)(end - dst), n);
            } break;
            case 'p': {
                void const *p = VA_ARG(args, void const *);
                dst += fmt_sprint_ptr(dst, (usize)(end - dst), p);
            } break;
            case 's': {
                char const *p = VA_ARG(args, char const *);
                while (dst < end && *p)
                    *dst++ = *p++;
            } break;
            // @TODO: length-ed string
            default:
                *dst++ = *fmt;
                break;
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
    isize ret = fmt_vsprint(buf, bufsize, fmt, args);
    VA_END(args);
    return ret;
}

