#include <os.h>
#include <memory.h>
#include <buffer.h>
#include <defs.h>

#include <gpa.h>

int main(int argc, char **argv)
{
    init_os_process_state(&g_os_proc_state);
    try_enable_large_pages(&g_os_proc_state);

    buffer_t program_memory = buf_allocate_best(4ull << 30);

    // @TEST s
    // @TODO: reimplement, add logging system
    printf("Hello, world! Argc=%d, Argv=%p\n", argc, argv);
    if (buf_is_valid(&program_memory)) {
        printf("Allocated %llu bytes for program memory at %p\n",
            program_memory.len, program_memory.data);
    } else {
        printf("Failed to allocate %llu bytes for program memory!\n", 4ull << 30);
        return 2;
    }

    gpa_t gpa = make_gpa(program_memory);

    // @TEST s
    int *number = GPA_ALLOC(gpa, int);
    f64 *doubles = GPA_ALLOC_N(gpa, f64, 1024);
    u8 *bytes = gpa_allocate(&gpa, 1 << 20);
    if (!number) {
        printf("GPA alloc failed for number!\n");
        return 2;
    } else {
        printf("Allocated number at %p\n", number);
    }
    if (!doubles) {
        printf("GPA alloc failed for double array!\n");
        return 2;
    } else {
        printf("Allocated double array at %p\n", doubles);
    }
    if (!bytes) {
        printf("GPA alloc failed for byte blob!\n");
        return 2;
    } else {
        printf("Allocated byte blob at %p\n", bytes);
    }
    GPA_DEALLOC(gpa, doubles);
    GPA_DEALLOC(gpa, number);
    gpa_deallocate(&gpa, bytes);

    return 0;
}
