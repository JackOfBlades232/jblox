#pragma once

#include "defs.h"

#if _WIN32

// @TODO

#else

#include <unistd.h>
#include <sys/syscall.h>

// @TODO: sort out if I need volatiles/clobbers

static inline isize sys_call0(usize id)
{
    isize ret;
    asm(
        "syscall"
        : "=a"(ret)
        : "a"(id)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call1(usize id, void *arg)
{
    isize ret;
    asm(
        "syscall"
        : "=a"(ret)
        : "a"(id), "D"(arg)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call2(usize id, void *arg1, void *arg2)
{
    isize ret;
    asm(
        "syscall"
        : "=a"(ret)
        : "a"(id), "D"(arg1), "S"(arg2)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call3(usize id, void *arg1, void *arg2, void *arg3)
{
    isize ret;
    asm(
        "syscall"
        : "=a"(ret)
        : "a"(id), "D"(arg1), "S"(arg2), "d"(arg3)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call6(
    usize id,
    void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6)
{
    isize ret;
    register i64 r10 asm("r10") = (i64)arg4;
    register i64 r8 asm("r8") = (i64)arg5;
    register i64 r9 asm("r9") = (i64)arg6;
    asm(
        "syscall"
        : "=a"(ret)
        : "a"(id), "D"(arg1), "S"(arg2), "d"(arg3), "r"(r10), "r"(r8), "r"(r9)
        : "rcx", "r11");
    return ret;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wint-to-void-pointer-cast"

static inline void sys_exit(int code)
{
    sys_call1(SYS_exit, (void *)code);
}

static inline isize sys_write(int fd, u8 const *buf, usize bytes)
{
    return sys_call3(SYS_write, (void *)fd, (void *)buf, (void *)bytes);
}

static inline isize sys_read(int fd, u8 *buf, usize bytes)
{
    return sys_call3(SYS_read, (void *)fd, (void *)buf, (void *)bytes);
}

static inline pid_t sys_getpid(void)
{
    return (pid_t)sys_call0(SYS_getpid);
}

static inline void *sys_mmap(
    void *addr, usize length, int prot, int flags, int fd, off_t offset)
{
    return (void *)sys_call6(
        SYS_mmap,
        addr, (void *)length,
        (void *)prot, (void *)flags, (void *)fd, (void *)offset);
}

static inline void *sys_munmap(void *addr, usize length)
{
    return (void *)sys_call2(SYS_munmap, addr, (void *)length);
}

#pragma GCC diagnostic pop

#endif
