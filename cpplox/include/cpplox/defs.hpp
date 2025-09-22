#pragma once

#pragma clang diagnostic ignored "-Wunqualified-std-cast-call"

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <sys/types.h>

#include <utility>
#include <iostream>
#include <print>
#include <span>

#include <version.hpp>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using f32 = float;
using f64 = double;
using b32 = u32;
using uint = unsigned int;
using uchar = unsigned char;
using usize = size_t;
using isize = ssize_t;

// Don't need all the colonoscopy, and my type naming conventions are already
// different from the std ones.
using namespace std;

namespace detail
{

template <class TF>
struct Defer {
    TF f;

    Defer(TF &&f) : f{move(f)} {}
    ~Defer() { f(); }
};

}

#define DEFER(stmt_) detail::Defer defer__ ## __COUNTER__ {[&] { stmt_; }}
