#pragma once

#include "defs.h"

#if _WIN32

// @TODO

#else

#define SYS_EXIT 60
#define SYS_WRITE 1
#define SYS_READ 0
#define SYS_GETPID 39
#define SYS_MMAP 9
#define SYS_MUNMAP 11

#define SYS_PROT_READ 0x1
#define SYS_PROT_WRITE 0x2
#define SYS_MAP_PRIVATE	0x02
#define SYS_MAP_ANON 0x20
#define SYS_MAP_HUGETLB	0x040000
#define SYS_MAP_HUGE_2MB (21U << 26)

#define SYS_STDIN_FILENO 0
#define SYS_STDOUT_FILENO 1
#define SYS_STDERR_FILENO 2

typedef int sys_pid_t;

// @TODO: sort out if I need more clobbers

static inline isize sys_call0(usize id)
{
    isize ret;
    asm volatile(
        "syscall"
        : "=a"(ret)
        : "a"(id)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call1(usize id, void *arg)
{
    isize ret;
    asm volatile(
        "syscall"
        : "=a"(ret)
        : "a"(id), "D"(arg)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call2(usize id, void *arg1, void *arg2)
{
    isize ret;
    asm volatile(
        "syscall"
        : "=a"(ret)
        : "a"(id), "D"(arg1), "S"(arg2)
        : "rcx", "r11");
    return ret;
}

static inline isize sys_call3(usize id, void *arg1, void *arg2, void *arg3)
{
    isize ret;
    asm volatile(
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
    asm volatile(
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
    sys_call1(SYS_EXIT, (void *)code);
}

static inline isize sys_write(int fd, u8 const *buf, usize bytes)
{
    return sys_call3(SYS_WRITE, (void *)fd, (void *)buf, (void *)bytes);
}

static inline isize sys_read(int fd, u8 *buf, usize bytes)
{
    return sys_call3(SYS_READ, (void *)fd, (void *)buf, (void *)bytes);
}

static inline sys_pid_t sys_getpid(void)
{
    return (sys_pid_t)sys_call0(SYS_GETPID);
}

static inline void *sys_mmap(
    void *addr, usize length, int prot, int flags, int fd, usize offset)
{
    return (void *)sys_call6(
        SYS_MMAP,
        addr, (void *)length,
        (void *)prot, (void *)flags, (void *)fd, (void *)offset);
}

static inline void *sys_munmap(void *addr, usize length)
{
    return (void *)sys_call2(SYS_MUNMAP, addr, (void *)length);
}

#pragma GCC diagnostic pop

#endif
