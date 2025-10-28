#pragma once

#define _CRT_SECURE_NO_WARNINGS

#include <stddef.h>
#include <stdint.h>
#include <float.h>

#include <assert.h>
#include <stdio.h>

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

#define true ((b32)1)
#define false ((b32)0)

#define CAT_(a_, b_) a_ ## b_
#define CAT(a_, b_) CAT_(a_, b_)

#define ROUND_UP(v_, m_) ((m_) * (((v_) - 1) / (m_) + 1))
