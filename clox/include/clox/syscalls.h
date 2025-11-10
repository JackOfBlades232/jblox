#pragma once

#include "defs.h"

#if _WIN32

#define WIN32_ERROR_SUCCESS 0l
#define WIN32_INVALID_HANDLE_VALUE ((win32_handle_t)((u64)-1))
#define WIN32_STD_INPUT_HANDLE ((u32)-10)
#define WIN32_STD_OUTPUT_HANDLE ((u32)-11)
#define WIN32_STD_ERROR_HANDLE ((u32)-12)
#define WIN32_MEM_COMMIT 0x1000
#define WIN32_MEM_RESERVE 0x2000
#define WIN32_MEM_RELEASE 0x8000
#define WIN32_MEM_LARGE_PAGES 0x20000000
#define WIN32_PAGE_READWRITE 0x4
#define WIN32_TOKEN_ADJUST_PRIVILEGES 0x20
#define WIN32_SE_PRIVILEGE_ENABLED 0x2L
#define WIN32_GENERIC_READ 0x80000000
#define WIN32_OPEN_EXISTING 3
#define WIN32_FILE_ATTRIBUTE_NORMAL 0x80

typedef void *win32_handle_t;

typedef struct {
    u32 privilege_count;
    struct {
        struct {
            u32 low_part;
            long high_part;
        } luid;
        u32 attributes;
    } privileges[1];
} win32_token_privileges_t;

typedef void *(__stdcall *win32_get_proc_addr_t)(win32_handle_t, char const *);
typedef b32 (__stdcall *win32_close_handle_t)(win32_handle_t);
typedef win32_handle_t (__stdcall *win32_load_library_a_t)(char const *);
typedef b32 (__stdcall *win32_free_library_t)(win32_handle_t);
typedef void (__stdcall *win32_exit_process_t)(uint);
typedef u32 (__stdcall *win32_get_last_error_t)(void);
typedef void *(__stdcall *win32_virtual_alloc_t)(void *, usize, u32, u32);
typedef b32 (__stdcall *win32_virtual_free_t)(void *, usize, u32);
typedef win32_handle_t (__stdcall *win32_create_file_a_t)(
    char const *, u32, u32, void *, u32, u32, win32_handle_t);
typedef b32 (__stdcall *win32_write_file_t)(
    win32_handle_t, void const *, u32, u32 *, void *);
typedef b32 (__stdcall *win32_read_file_t)(
    win32_handle_t, void *, u32, u32 *, void *);
typedef u32 (__stdcall *win32_get_file_size_t)(win32_handle_t, u32 *);
typedef win32_handle_t (__stdcall *win32_get_current_process_t)(void);
typedef win32_handle_t (__stdcall *win32_get_std_handle_t)(u32);
typedef char const *(__stdcall *win32_get_command_line_a_t)(void);
typedef usize (__stdcall *win32_get_large_page_minimum_t)(void);
typedef b32 (__stdcall *win32_open_process_token_t)(
    win32_handle_t, u32, win32_handle_t *);
typedef b32 (__stdcall *win32_lookup_privilege_value_a_t)(
    char const *, char const *, void *);
typedef b32 (__stdcall *win32_adjust_token_privileges_t)(
    win32_handle_t, b32, win32_token_privileges_t const *,
    u32, win32_token_privileges_t *, u32 *);

typedef struct {
    win32_get_proc_addr_t get_proc_addr;
    win32_close_handle_t close_handle;
    win32_load_library_a_t load_library_a;
    win32_free_library_t free_library;
    win32_exit_process_t exit_process;
    win32_get_last_error_t get_last_error;
    win32_virtual_alloc_t virtual_alloc;
    win32_virtual_free_t virtual_free;
    win32_create_file_a_t create_file_a;
    win32_write_file_t write_file;
    win32_read_file_t read_file;
    win32_get_file_size_t get_file_size;
} win32_syscalls_t;

u64 __readgsqword(ulong offset);

#else

#define SYS_EXIT 60
#define SYS_WRITE 1
#define SYS_READ 0
#define SYS_OPEN 2
#define SYS_CLOSE 3
#define SYS_LSEEK 8
#define SYS_GETPID 39
#define SYS_MMAP 9
#define SYS_MUNMAP 11

#define SYS_PROT_READ 0x1
#define SYS_PROT_WRITE 0x2
#define SYS_MAP_PRIVATE 0x02
#define SYS_MAP_ANON 0x20
#define SYS_MAP_HUGETLB 0x040000
#define SYS_MAP_HUGE_2MB (21U << 26)
#define SYS_O_RDONLY 0
#define SYS_SEEK_SET 0
#define SYS_SEEK_END 2

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

static inline int sys_open(char const *path, int flags, uint mode)
{
    return sys_call3(SYS_OPEN, (void *)path, (void *)flags, (void *)mode);
}

static inline void sys_close(int fd)
{
    sys_call1(SYS_CLOSE, (void *)fd);
}

static inline usize sys_lseek(int fd, usize off, uint whence)
{
    return sys_call3(SYS_LSEEK, (void *)fd, (void *)off, (void *)whence);
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
