#include <os.h>
#include <memory.h>
#include <buffer.h>
#include <fmt.h>
#include <io.h>
#include <logging.h>
#include <defs.h>

#include <gpa.h>

int main(int argc, char **argv)
{
    os_init_process_state(&g_os_proc_state);
    os_try_enable_large_pages(&g_os_proc_state);

    buffer_t program_memory = buf_allocate_best(1ull << 30);

    // @TEST s
    LOGF("Hello, world! Argc=%d, Argv=%p", argc, argv);
    if (buf_is_valid(&program_memory)) {
        LOGF(
            "Allocated %U bytes for program memory at %p (%s)",
            program_memory.len, program_memory.data,
            program_memory.is_large_pages ? "large pages" : "regular pages");
    } else {
        LOGF("Failed to allocate %U bytes for program memory!", 4ull << 30);
        return 2;
    }

    gpa_t gpa = make_gpa(program_memory);

    // @TEST s
    int *number = GPA_ALLOC(gpa, int);
    f64 *doubles = GPA_ALLOC_N(gpa, f64, 1024);
    u8 *bytes = gpa_allocate(&gpa, 1 << 20);
    if (!number) {
        LOG("GPA alloc failed for number!");
        return 2;
    } else {
        LOGF("Allocated number at %p", number);
    }
    if (!doubles) {
        LOG("GPA alloc failed for double array!");
        return 2;
    } else {
        LOGF("Allocated double array at %p", doubles);
    }
    if (!bytes) {
        LOG("GPA alloc failed for byte blob!");
        return 2;
    } else {
        LOGF("Allocated byte blob at %p", bytes);
    }
    GPA_DEALLOC(gpa, doubles);
    GPA_DEALLOC(gpa, number);
    gpa_deallocate(&gpa, bytes);

    return 0;
}


#if _WIN32

// @TODO

#else

__attribute__((naked)) void _start(void)
{
    asm(
        "xor rbp, rbp\n"
        "pop rdi\n"
        "mov rsi, rsp\n"
        "and rsp, -16\n"
        "call main\n"
        "mov rdi, rax\n"
        "mov rax, 60\n"
        "syscall\n"
        "ret");
}

#endif
