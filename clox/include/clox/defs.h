#pragma once

#define _CRT_SECURE_NO_WARNINGS

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <float.h>

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

#define CAT_(a_, b_) a_ ## b_
#define CAT(a_, b_) CAT_(a_, b_)
