#pragma once

#pragma clang diagnostic ignored "-Wunqualified-std-cast-call"
#define _CRT_SECURE_NO_WARNINGS

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cassert>

#include <utility>
#include <iostream>
#include <print>
#include <span>
#include <variant>
#include <optional>
#include <vector>
#include <string>
#include <unordered_map>
#include <memory>
#include <algorithm>

#include <version.hpp>

// Don't need all the colonoscopy, and my type naming conventions are already
// different from the std ones.
using namespace std;

#ifdef _WIN32
using ssize_t = make_signed_t<size_t>;
#else
#include <sys/types.h>
#endif

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
