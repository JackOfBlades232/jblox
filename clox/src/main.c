#include <os.h>
#include <memory.h>
#include <buffer.h>
#include <fmt.h>
#include <io.h>
#include <logging.h>
#include <defs.h>
#include <gpa.h>

static inline void *reallocate(void const *ptr, usize old_size, usize new_size)
{
    if (new_size == 0) {
        gpa_deallocate(&g_gpa, ptr);
        return NULL;
    }

    void *res = gpa_allocate(&g_gpa, new_size);
    VERIFY(res, "Out of memory.");

    if (old_size) {
        memcpy(res, ptr, MIN(old_size, new_size));
        gpa_deallocate(&g_gpa, ptr);
    }

    return res;
}

#define GROW_CAP(cap_) ((cap_) < 8 ? 8 : 2 * (cap_))
#define GROW_ARRAY(type_, ptr_, oldcap_, newcap_) \
    (type_ *)reallocate(                          \
        (ptr_), (oldcap_) * sizeof(type_), (newcap_) * sizeof(type_))
#define FREE_ARRAY(type_, ptr_, cap_) \
    reallocate((ptr_), sizeof(type_) * (cap_), 0)

typedef enum {
    e_op_constant,
    e_op_constant_long,
    e_op_return,
} opcode_t;

typedef f64 value_t;

typedef struct {
    uint cnt, cap;
    value_t *values;
} value_array_t;

static inline value_array_t make_value_array(void)
{
    return (value_array_t){0};
}

static inline void write_value_array(value_array_t *arr, value_t val)
{
    if (arr->cap < arr->cnt + 1) {
        uint old_cap = arr->cap;
        arr->cap = GROW_CAP(old_cap);
        arr->values = GROW_ARRAY(value_t, arr->values, old_cap, arr->cap);
    }

    arr->values[arr->cnt++] = val;
}

static inline void free_value_array(value_array_t *arr)
{
    FREE_ARRAY(value_t, arr->values, arr->cap);
    *arr = make_value_array();
}

typedef struct {
    int line;
    uint instr_range_end;
} line_info_entry_t;

typedef struct {
    uint cnt, cap;
    line_info_entry_t *entries;
} line_info_t;

static inline line_info_t make_line_info(void)
{
    return (line_info_t){0};
}

static inline void write_line_info(line_info_t *info, uint line, uint instr_end)
{
    if (info->cap < info->cnt + 1) {
        uint old_cap = info->cap;
        info->cap = GROW_CAP(old_cap);
        info->entries =
            GROW_ARRAY(line_info_entry_t, info->entries, old_cap, info->cap);
    }

    info->entries[info->cnt++] = (line_info_entry_t){line, instr_end};
}

static inline void free_line_info(line_info_t *info)
{
    FREE_ARRAY(value_t, info->entries, info->cap);
    *info = make_line_info();
}

typedef struct {
    uint cnt, cap;
    u8 *code;
    value_array_t constants;
    line_info_t lines;
    uint current_line;
} chunk_t;

static inline chunk_t make_chunk(void)
{
    chunk_t c = {0};
    c.current_line = -1;
    return c;
}

static inline void write_chunk(chunk_t *chunk, u8 byte, uint line)
{
    if (chunk->cap < chunk->cnt + 1) {
        uint old_cap = chunk->cap;
        chunk->cap = GROW_CAP(old_cap);
        chunk->code = GROW_ARRAY(u8, chunk->code, old_cap, chunk->cap);
    }

    chunk->code[chunk->cnt++] = byte;

    if (line != chunk->current_line) {
        write_line_info(&chunk->lines, line, chunk->cnt);
        chunk->current_line = line;
    } else {
        chunk->lines.entries[chunk->lines.cnt - 1].instr_range_end = chunk->cnt;
    }
}

static inline uint add_constant(chunk_t *chunk, value_t val)
{
    write_value_array(&chunk->constants, val);
    return chunk->constants.cnt - 1;
}

static inline void write_constant(chunk_t *chunk, value_t val, uint line)
{
    uint constant = add_constant(chunk, val);
    if (constant <= 255) {
        write_chunk(chunk, e_op_constant, line);
        write_chunk(chunk, constant, line);
    } else if (constant <= 16777215) {
        write_chunk(chunk, e_op_constant_long, line);
        write_chunk(chunk, constant & 0xFF, line);
        write_chunk(chunk, (constant >> 8) & 0xFF, line);
        write_chunk(chunk, (constant >> 16) & 0xFF, line);
    } else {
        // @TODO: a more gracious error?
        PANIC(
            "Constants overflow: "
            "one chunk can't contain more than 16777215 constants.");
    }
}

static inline void free_chunk(chunk_t *chunk)
{
    FREE_ARRAY(u8, chunk->code, chunk->cap);
    free_value_array(&chunk->constants);
    free_line_info(&chunk->lines);
    *chunk = make_chunk();
}

#if NDEBUG

#define DISASSEMBLE_CHUNK(...)

#else

static void disasm_value(value_t val)
{
    LOGF_NONL("%f", val);
}

static uint disasm_simple_instruction(char const *name, uint at)
{
    LOGF("%s", name);
    return at + 1;
}

static uint disasm_constant_instruction(
    char const *name, chunk_t const *chunk, uint at)
{
    u8 constant = chunk->code[at + 1];
    LOGF_NONL("%s %u (", name, constant);
    disasm_value(chunk->constants.values[constant]);
    LOG(")");
    return at + 2;
}

static uint disasm_long_constant_instruction(
    char const *name, chunk_t const *chunk, uint at)
{
    uint constant =
        chunk->code[at + 1]
        | ((uint)chunk->code[at + 2] << 8)
        | ((uint)chunk->code[at + 3] << 16);
    LOGF_NONL("%s %u (", name, constant);
    disasm_value(chunk->constants.values[constant]);
    LOG(")");
    return at + 4;
}

static uint disasm_instruction(chunk_t const *chunk, uint at)
{
    usize chars = LOGF_NONL("%u:", at);
    for (usize i = chars; i < 8; ++i)
        LOG_NONL(" ");

    uint line_info_id = 0;
    uint line_range_start = 0;
    while (
        line_info_id < chunk->lines.cnt &&
        at >= chunk->lines.entries[line_info_id].instr_range_end)
    {
        line_range_start = chunk->lines.entries[line_info_id].instr_range_end;
        ++line_info_id;
    }

    if (at == line_range_start) {
        usize chars = LOGF_NONL("%u ", chunk->lines.entries[line_info_id].line);
        for (usize i = chars; i < 6; ++i)
            LOG_NONL(" ");
    } else {
        LOG_NONL("|");
        for (usize i = 0; i < 5; ++i)
            LOG_NONL(" ");
    }

    u8 instr = chunk->code[at];
    switch (instr) {
    case e_op_constant:
        return disasm_constant_instruction("OP_CONSTANT", chunk, at);
    case e_op_constant_long:
        return disasm_long_constant_instruction("OP_CONSTANT_LONG", chunk, at);
    case e_op_return:
        return disasm_simple_instruction("OP_RETURN", at);
    default:
        LOGF("Unknown opcode %d", (int)instr);
        return at + 1;
    }
}

static void disasm_chunk(chunk_t const *chunk, char const *name)
{
    LOGF("== %s ==", name);
    for (uint off = 0; off < chunk->cnt;)
        off = disasm_instruction(chunk, off); 
}

#define DISASSEMBLE_CHUNK(chunk_, name_) disasm_chunk((chunk_), (name_))

#endif

static int lox_main(int argc, char **argv)
{
    (void)argc;
    (void)argv;

    {
        chunk_t chunk = make_chunk();

        f64 base = 1.2;
        uint i = 0;
        for (; i < 512; ++i)
            write_constant(&chunk, base + i * 0.2, 123 + i / 100);

        write_chunk(&chunk, e_op_return, 123 + i / 100);

        DISASSEMBLE_CHUNK(&chunk, "test chunk");

        free_chunk(&chunk);
    }

    return 0;
}

int main(int argc, char **argv)
{
    os_init_process_state(&g_os_proc_state);
    os_try_enable_large_pages(&g_os_proc_state);

    buffer_t program_memory = buf_allocate_best(1ull << 30);
    VERIFY(buf_is_valid(&program_memory), "Failed to allocate program memory.");

    g_gpa = gpa_make(program_memory);

    return lox_main(argc, argv);
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
