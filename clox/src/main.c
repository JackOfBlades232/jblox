#include <os.h>
#include <memory.h>
#include <buffer.h>
#include <defs.h>

int main(int argc, char **argv)
{
    init_os_process_state(&g_os_proc_state);
    try_enable_large_pages(&g_os_proc_state);

    buffer_t program_memory = buf_allocate_best(4ull << 30);

    // @TEST
    // @TODO: reimplement, add logging system
    printf("Hello, world! Argc=%d, Argv=%p\n", argc, argv);

    if (buf_is_valid(&program_memory)) {
        printf("Allocated %llu bytes for program memory at %p",
            program_memory.len, program_memory.data);
    } else {
        printf("Failed to allocate %llu bytes for program memory!", 4ull << 30);
        return 2;
    }

    return 0;
}
