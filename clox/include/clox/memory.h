#pragma once

#include "defs.h" 

#if _WIN32

#include <windows.h>

static inline void *allocate_os_pages_memory(usize bytes)
{
    return VirtualAlloc(
        NULL, bytes, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

static inline void free_os_pages_memory(void *mem, usize size)
{
    (void)size;
    VirtualFree(mem, 0, MEM_RELEASE);
}

static inline void *allocate_os_large_pages_memory(usize bytes)
{
    if (g_os_proc_state.large_page_size == 0)
        return NULL;
    usize const bytes_for_pages =
        ROUND_UP(bytes, g_os_proc_state.large_page_size);
    return VirtualAlloc(
        NULL, bytes_for_pages,
        MEM_RESERVE | MEM_COMMIT | MEM_LARGE_PAGES, PAGE_READWRITE);
}

static inline void free_os_large_pages_memory(void *mem, usize size)
{
    (void)size;
    free_os_pages_memory(mem, 0);
}

#else

#include <unistd.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <linux/mman.h>

static inline void *allocate_os_pages_memory(usize bytes)
{
    usize const bytes_for_pages =
        ROUND_UP(bytes, g_os_proc_state.regular_page_size);
    return mmap(
        NULL, bytes_for_pages, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
}

static inline void free_os_pages_memory(void *mem, usize bytes)
{
    munmap(mem, ROUND_UP(bytes, g_os_proc_state.regular_page_size));
}

static inline void *allocate_os_large_pages_memory(usize bytes)
{
    void *ptr = mmap(
        NULL, bytes, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB | MAP_HUGE_2MB, 0, 0);
    return ptr == MAP_FAILED ? NULL : ptr;
}

static inline void free_os_large_pages_memory(void *mem, usize bytes)
{
    usize const c_huge_page_alignment = 2 << 20;
    munmap(mem, ROUND_UP(bytes, c_huge_page_alignment));
}

#endif

