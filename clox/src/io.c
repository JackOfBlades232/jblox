#include "io.h"

io_handle_t g_stdin = {STDIN_FILENO + 1};
io_handle_t g_stdout = {STDOUT_FILENO + 1};
io_handle_t g_stderr = {STDERR_FILENO + 1};
