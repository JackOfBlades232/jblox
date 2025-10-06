#pragma once

#pragma clang diagnostic ignored "-Wunqualified-std-cast-call"
#define _CRT_SECURE_NO_WARNINGS

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cassert>
#include <cfloat>

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
#include <concepts>
#include <chrono>

#include <version.hpp>

// Don't need all the colonoscopy, and my type naming conventions are already
// different from the std ones.
using namespace std;
using namespace std::chrono;

#ifdef _WIN32
using ssize_t = make_signed_t<size_t>;
extern "C"
{
union LARGE_INTEGER {
    struct {
        unsigned long LowPart;
        long HighPart;
    };
    struct {
        unsigned long LowPart;
        long HighPart;
    } u;
    long long QuadPart;
};
__declspec(dllimport) int __stdcall QueryPerformanceCounter(LARGE_INTEGER *);
__declspec(dllimport) int __stdcall QueryPerformanceFrequency(LARGE_INTEGER *);
}
#else
#include <sys/types.h>
#include <time.h>
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

#define CAT_(a_, b_) a_ ## b_
#define CAT(a_, b_) CAT_(a_, b_)

namespace detail
{

template <class TF>
struct Defer {
    TF f;

    Defer(TF &&f) : f{move(f)} {}
    ~Defer() { f(); }
};

}

#define DEFER(stmt_) \
    detail::Defer CAT(defer__, __LINE__) {[&] { stmt_; }}
#define DEFERM(stmt_) \
    detail::Defer CAT(defer__, __LINE__) {[&, this] { stmt_; }}

inline class GlobalHiresTimer {
#ifdef _WIN32
    LARGE_INTEGER m_hr_start, m_hr_freq;
#else
    struct timespec m_hr_start;
#endif

public:
    GlobalHiresTimer() {
#ifdef _WIN32
        QueryPerformanceFrequency(&m_hr_freq);
        QueryPerformanceCounter(&m_hr_start);
#else
        clock_gettime(CLOCK_MONOTONIC, &m_hr_start);
#endif
    }

    ~GlobalHiresTimer() {}

    f64 MsFromProgramStart() const {
#ifdef _WIN32
        LARGE_INTEGER now;
        QueryPerformanceCounter(&now);
        return
            f64(now.QuadPart - m_hr_start.QuadPart) * 1000.0 /
            f64(m_hr_freq.QuadPart);
#else
        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC, &now);
        return
            f64(now.tv_sec - m_hr_start.tv_sec) * 1000.0 +
            f64(now.tv_nsec - m_hr_start.tv_nsec) / 1.0e6;
#endif
    }
} g_hires_timer{};
