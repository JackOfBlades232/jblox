#include <defs.h>
#include <syscalls.h>

int main(int argc, char **argv);

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
