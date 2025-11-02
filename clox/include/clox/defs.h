#pragma once

#define _CRT_SECURE_NO_WARNINGS

#include <stddef.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef u32 b32;
typedef unsigned int uint;
typedef unsigned char uchar;
typedef unsigned long ulong;
typedef long long llong;
typedef unsigned long long ullong;
typedef size_t usize;

#if _WIN32
#include <BaseTsd.h>
typedef SSIZE_T isize;
#else
#include <sys/types.h>
typedef ssize_t isize;
#endif

#define XITYPES \
    X(u8)       \
    X(u16)      \
    X(u32)      \
    X(u64)      \
    X(i8)       \
    X(i16)      \
    X(i32)      \
    X(i64)      \
    X(b32)      \
    X(uint)     \
    X(uchar)    \
    X(ulong)    \
    X(llong)    \
    X(ullong)   \
    X(usize)    \
    X(isize)

#define XFTYPES \
    X(f32)      \
    X(f64)      \

#define XNTYPES XITYPES XFTYPES

#define GENF_NTYPES(X_, name_)        \
    _Generic((X_),                    \
        u8: u8 ## _ ## name_,         \
        u16: u16 ## _ ## name_,       \
        u32: u32 ## _ ## name_,       \
        u64: u64 ## _ ## name_,       \
        i8: i8 ## _ ## name_,         \
        i16: i16 ## _ ## name_,       \
        i32: i32 ## _ ## name_,       \
        i64: i64 ## _ ## name_,       \
        f32: f32 ## _ ## name_,       \
        f64: f64 ## _ ## name_        \
    )

#define GENF_ITYPES(X_, name_)        \
    _Generic((X_),                    \
        u8: u8 ## _ ## name_,         \
        u16: u16 ## _ ## name_,       \
        u32: u32 ## _ ## name_,       \
        u64: u64 ## _ ## name_,       \
        i8: i8 ## _ ## name_,         \
        i16: i16 ## _ ## name_,       \
        i32: i32 ## _ ## name_,       \
        i64: i64 ## _ ## name_        \
    )

#define GENF_FTYPES(X_, name_)        \
    _Generic((X_),                    \
        f32: f32 ## _ ## name_,       \
        f64: f64 ## _ ## name_        \
    )

#define true ((b32)1)
#define false ((b32)0)

#define CAT_(a_, b_) a_ ## b_
#define CAT(a_, b_) CAT_(a_, b_)

#define ARRCNT(a_) (sizeof(a_) / sizeof(*(a_)))
#define STRLITLEN(s_) (sizeof(s_) / sizeof(char) - 1)

#define SWAP(a_, b_, type_) \
    do { type_ tmp_ = (a_); (a_) = (b_); (b_) = tmp_; } while (0)

#define ABS(a_) ((a_) < 0 ? -(a_) : (a_))
#define MIN(a_, b_) ((a_) < (b_) ? (a_) : (b_))
#define MAX(a_, b_) ((a_) > (b_) ? (a_) : (b_))

#define ROUND_UP(v_, m_) ((m_) * (((v_) - 1) / (m_) + 1))

#define DECLARE_GCD(type_)                             \
    static inline type_ type_ ## _gcd(type_ a, type_ b) \
    {                                                  \
        while (b != 0) {                               \
            type_ t = b;                               \
            b = a % b;                                 \
            a = t;                                     \
        }                                              \
        return ABS(a);                                 \
    }
#define X(t_) DECLARE_GCD(t_)
XITYPES
#undef X
#define GCD(a_, b_) GENF_ITYPES(a_, gcd)((a_), (b_))

#define LCM(a_, b_) ((ABS(a_) * ABS(b_)) / GCD(a_, b_))

#define VA_LIST __builtin_va_list
#define VA_START(ap_, param_) __builtin_va_start(ap_, param_)
#define VA_END(ap_) __builtin_va_end(ap_)
#define VA_ARG(ap_, type_) __builtin_va_arg(ap_, type_)
