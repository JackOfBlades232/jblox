#pragma once

#include "defs.h" 
#include "os.h" 

#if _WIN32

static inline void *os_allocate_pages_memory(usize bytes)
{
    return g_os.sys.virtual_alloc(
        NULL, bytes, WIN32_MEM_RESERVE | WIN32_MEM_COMMIT,
        WIN32_PAGE_READWRITE);
}

static inline void os_free_pages_memory(void *mem, usize size)
{
    (void)size;
    g_os.sys.virtual_free(mem, 0, WIN32_MEM_RELEASE);
}

static inline void *os_allocate_large_pages_memory(usize bytes)
{
    if (g_os.large_page_size == 0)
        return NULL;
    usize const bytes_for_pages =
        ROUND_UP(bytes, g_os.large_page_size);
    return g_os.sys.virtual_alloc(
        NULL, bytes_for_pages,
        WIN32_MEM_RESERVE | WIN32_MEM_COMMIT | WIN32_MEM_LARGE_PAGES, 
        WIN32_PAGE_READWRITE);
}

static inline void os_free_large_pages_memory(void *mem, usize size)
{
    (void)size;
    os_free_pages_memory(mem, 0);
}

#else

static inline void *os_allocate_pages_memory(usize bytes)
{
    usize const bytes_for_pages =
        ROUND_UP(bytes, g_os.regular_page_size);
    return sys_mmap(
        NULL, bytes_for_pages, SYS_PROT_READ | SYS_PROT_WRITE,
        SYS_MAP_PRIVATE | SYS_MAP_ANON, 0, 0);
}

static inline void os_free_pages_memory(void *mem, usize bytes)
{
    sys_munmap(mem, ROUND_UP(bytes, g_os.regular_page_size));
}

static inline void *os_allocate_large_pages_memory(usize bytes)
{
    usize const c_huge_page_alignment = 2 << 20; // @TODO: settable?
    usize const bytes_for_pages = ROUND_UP(bytes, c_huge_page_alignment);
    void *ptr = sys_mmap(
        NULL, bytes_for_pages, SYS_PROT_READ | SYS_PROT_WRITE,
        SYS_MAP_PRIVATE | SYS_MAP_ANON | SYS_MAP_HUGETLB | SYS_MAP_HUGE_2MB,
        -1, 0);
    return (isize)ptr < 0 ? NULL : ptr;
}

static inline void os_free_large_pages_memory(void *mem, usize bytes)
{
    usize const c_huge_page_alignment = 2 << 20; // @TODO: settable?
    sys_munmap(mem, ROUND_UP(bytes, c_huge_page_alignment));
}

#endif

