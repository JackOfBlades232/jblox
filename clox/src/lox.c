#include <context.h>
#include <os.h>
#include <memory.h>
#include <buffer.h>
#include <fmt.h>
#include <io.h>
#include <logging.h>
#include <defs.h>
#include <gpa.h>
#include <debug.h>
#include <string.h>

static inline void *reallocate(
    ctx_t const *ctx, void const *ptr, usize old_size, usize new_size)
{
    if (new_size == 0) {
        gpa_deallocate(ctx, ctx->gpa, ptr);
        return NULL;
    }

    void *res = gpa_allocate(ctx, ctx->gpa, new_size);
    VERIFY(res, "Out of memory.");

    if (old_size) {
        memcpy(res, ptr, MIN(old_size, new_size));
        gpa_deallocate(ctx, ctx->gpa, ptr);
    }

    return res;
}

#define GROW_CAP(cap_) ((cap_) < 8 ? 8 : 2 * (cap_))
#define GROW_ARRAY(type_, ptr_, oldcap_, newcap_) \
    (type_ *)reallocate(                          \
        ctx, (ptr_), (oldcap_) * sizeof(type_), (newcap_) * sizeof(type_))
#define FREE_ARRAY(type_, ptr_, cap_) \
    reallocate(ctx, (ptr_), sizeof(type_) * (cap_), 0)

typedef enum {
    e_op_constant,
    e_op_constant_long,
    e_op_add,
    e_op_subtract,
    e_op_multiply,
    e_op_divide,
    e_op_negate,
    e_op_return,
} opcode_t;

typedef f64 value_t;

typedef struct {
    uint cnt, cap;
    value_t *values;
} value_array_t;

static inline value_array_t make_value_array(ctx_t const *ctx)
{
    (void)ctx;
    return (value_array_t){0};
}

static inline void write_value_array(
    ctx_t const *ctx, value_array_t *arr, value_t val)
{
    if (arr->cap < arr->cnt + 1) {
        uint old_cap = arr->cap;
        arr->cap = GROW_CAP(old_cap);
        arr->values = GROW_ARRAY(value_t, arr->values, old_cap, arr->cap);
    }

    arr->values[arr->cnt++] = val;
}

static inline void free_value_array(ctx_t const *ctx, value_array_t *arr)
{
    FREE_ARRAY(value_t, arr->values, arr->cap);
    *arr = make_value_array(ctx);
}

typedef struct {
    uint line;
    uint instr_range_end;
} line_info_entry_t;

typedef struct {
    uint cnt, cap;
    line_info_entry_t *entries;
} line_info_t;

static inline line_info_t make_line_info(ctx_t const *ctx)
{
    (void)ctx;
    return (line_info_t){0};
}

static inline void write_line_info(
    ctx_t const *ctx, line_info_t *info, uint line, uint instr_end)
{
    if (info->cap < info->cnt + 1) {
        uint old_cap = info->cap;
        info->cap = GROW_CAP(old_cap);
        info->entries =
            GROW_ARRAY(line_info_entry_t, info->entries, old_cap, info->cap);
    }

    info->entries[info->cnt++] = (line_info_entry_t){line, instr_end};
}

static inline void free_line_info(ctx_t const *ctx, line_info_t *info)
{
    FREE_ARRAY(value_t, info->entries, info->cap);
    *info = make_line_info(ctx);
}

typedef struct {
    uint cnt, cap;
    u8 *code;
    value_array_t constants;
    line_info_t lines;
    uint current_line;
} chunk_t;

static inline chunk_t make_chunk(ctx_t const *ctx)
{
    (void)ctx;
    chunk_t c = {0};
    c.current_line = ((uint)-1);
    return c;
}

static inline void write_chunk(ctx_t const *ctx, chunk_t *chunk, u8 byte, uint line)
{
    if (chunk->cap < chunk->cnt + 1) {
        uint old_cap = chunk->cap;
        chunk->cap = GROW_CAP(old_cap);
        chunk->code = GROW_ARRAY(u8, chunk->code, old_cap, chunk->cap);
    }

    chunk->code[chunk->cnt++] = byte;

    if (line != chunk->current_line) {
        write_line_info(ctx, &chunk->lines, line, chunk->cnt);
        chunk->current_line = line;
    } else {
        chunk->lines.entries[chunk->lines.cnt - 1].instr_range_end = chunk->cnt;
    }
}

static inline uint add_constant(ctx_t const *ctx, chunk_t *chunk, value_t val)
{
    write_value_array(ctx, &chunk->constants, val);
    return chunk->constants.cnt - 1;
}

static inline void write_constant(
    ctx_t const *ctx, chunk_t *chunk, value_t val, uint line)
{
    uint constant = add_constant(ctx, chunk, val);
    if (constant <= 255) {
        write_chunk(ctx, chunk, e_op_constant, line);
        write_chunk(ctx, chunk, (u8)constant, line);
    } else if (constant <= 16777215) {
        write_chunk(ctx, chunk, e_op_constant_long, line);
        write_chunk(ctx, chunk, (u8)(constant & 0xFF), line);
        write_chunk(ctx, chunk, (u8)((constant >> 8) & 0xFF), line);
        write_chunk(ctx, chunk, (u8)((constant >> 16) & 0xFF), line);
    } else {
        // @TODO: a more gracious error?
        PANIC(
            "Constants overflow: "
            "one chunk can't contain more than 16777215 constants.");
    }
}

static inline void free_chunk(ctx_t const *ctx, chunk_t *chunk)
{
    FREE_ARRAY(u8, chunk->code, chunk->cap);
    free_value_array(ctx, &chunk->constants);
    free_line_info(ctx, &chunk->lines);
    *chunk = make_chunk(ctx);
}

static void print_value(ctx_t const *ctx, value_t val)
{
    OUTPUTF("%f", val);
}

#if DEBUG_TRACE_EXECUTION || DEBUG_DISASM

static void disasm_value(ctx_t const *ctx, value_t val)
{
    LOGF_NONL("%f", val);
}

static uint disasm_simple_instruction(
    ctx_t const *ctx, char const *name, uint at)
{
    LOGF("%s", name);
    return at + 1;
}

static uint disasm_constant_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at)
{
    u8 constant = chunk->code[at + 1];
    LOGF_NONL("%s %u (", name, constant);
    disasm_value(ctx, chunk->constants.values[constant]);
    LOG(")");
    return at + 2;
}

static uint disasm_long_constant_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at)
{
    uint constant =
        chunk->code[at + 1]
        | ((uint)chunk->code[at + 2] << 8)
        | ((uint)chunk->code[at + 3] << 16);
    LOGF_NONL("%s %u (", name, constant);
    disasm_value(ctx, chunk->constants.values[constant]);
    LOG(")");
    return at + 4;
}

static uint disasm_instruction(ctx_t const *ctx, chunk_t const *chunk, uint at)
{
    {
        isize chars = LOGF_NONL("%u:", at);
        for (isize i = chars; i < 8; ++i)
            LOG_NONL(" ");
    }

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
        isize chars = LOGF_NONL("%u ", chunk->lines.entries[line_info_id].line);
        for (isize i = chars; i < 6; ++i)
            LOG_NONL(" ");
    } else {
        LOG_NONL("|");
        for (isize i = 0; i < 5; ++i)
            LOG_NONL(" ");
    }

    u8 instr = chunk->code[at];
    switch (instr) {
    case e_op_constant:
        return disasm_constant_instruction(
            ctx, "OP_CONSTANT", chunk, at);
    case e_op_constant_long:
        return disasm_long_constant_instruction(
            ctx, "OP_CONSTANT_LONG", chunk, at);
    case e_op_negate:
        return disasm_simple_instruction(
            ctx, "OP_NEGATE", at);
    case e_op_add:
        return disasm_simple_instruction(
            ctx, "OP_ADD", at);
    case e_op_subtract:
        return disasm_simple_instruction(
            ctx, "OP_SUBTRACT", at);
    case e_op_multiply:
        return disasm_simple_instruction(
            ctx, "OP_MULTIPLY", at);
    case e_op_divide:
        return disasm_simple_instruction(
            ctx, "OP_DIVIDE", at);
    case e_op_return:
        return disasm_simple_instruction(
            ctx, "OP_RETURN", at);
    default:
        LOGF("Unknown opcode %d", (int)instr);
        return at + 1;
    }
}

#if DEBUG_DISASM

static void disasm_chunk(
    ctx_t const *ctx, chunk_t const *chunk, char const *name)
{
    LOGF("== %s ==", name);
    for (uint off = 0; off < chunk->cnt;)
        off = disasm_instruction(ctx, chunk, off); 
}

#define DISASSEMBLE_CHUNK(chunk_, name_) disasm_chunk(ctx, (chunk_), (name_))

#endif

#endif

#ifndef DISASSEMBLE_CHUNK
#define DISASSEMBLE_CHUNK(...)
#endif

#define VM_STACK_SEGMENT_SIZE 256

typedef struct vm_stack_segment {
    value_t values[VM_STACK_SEGMENT_SIZE];
    struct vm_stack_segment *prev;
    struct vm_stack_segment *next;
} vm_stack_segment_t;

typedef struct vm {
    chunk_t *chunk;
    u8 *ip;
    vm_stack_segment_t stack;
    vm_stack_segment_t *cur_stack_seg; 
    value_t *stack_top;
} vm_t;

static void reset_stack(ctx_t const *ctx, vm_t *vm)
{
    (void)ctx;
    vm->cur_stack_seg = &vm->stack;
    vm->stack_top = vm->stack.values;
}

static void init_vm(ctx_t const *ctx, vm_t *vm)
{
    reset_stack(ctx, vm);
}

#define PUSH_STACK_SEG(vm_)                                    \
    do {                                                       \
        vm_stack_segment_t *new_seg_ =                         \
            (vm_stack_segment_t *)reallocate(                  \
                    ctx, NULL, 0, sizeof(vm_stack_segment_t)); \
        (vm_)->cur_stack_seg->next = new_seg_;                 \
        new_seg_->prev = (vm_)->cur_stack_seg;                 \
        new_seg_->next = NULL;                                 \
        (vm_)->cur_stack_seg = new_seg_;                       \
        (vm_)->stack_top = new_seg_->values;                   \
    } while (0)

#define POP_STACK_SEG(vm_)                                        \
    do {                                                          \
        ASSERT((vm_)->cur_stack_seg->prev);                       \
        vm_stack_segment_t *old_seg_ = (vm_)->cur_stack_seg;      \
        (vm_)->cur_stack_seg = old_seg_->prev;                    \
        (vm_)->cur_stack_seg->next = NULL;                        \
        reallocate(ctx, old_seg_, sizeof(vm_stack_segment_t), 0); \
        (vm_)->stack_top =                                        \
            (vm_)->cur_stack_seg->values + VM_STACK_SEGMENT_SIZE; \
    } while (0)

static void push_stack(ctx_t const *ctx, vm_t *vm, value_t val)
{
    if (vm->stack_top - vm->cur_stack_seg->values >= VM_STACK_SEGMENT_SIZE)
        PUSH_STACK_SEG(vm);

    *vm->stack_top++ = val;
}

static value_t pop_stack(ctx_t const *ctx, vm_t *vm)
{
    if (vm->stack_top == vm->cur_stack_seg->values)
        POP_STACK_SEG(vm);

    return *(--vm->stack_top);
}

typedef enum {
    e_interp_ok,
    e_interp_compile_err,
    e_interp_runtime_err
} interp_result_t;

static interp_result_t run_vm(ctx_t const *ctx, vm_t *vm)
{
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG()     \
    (vm->chunk->constants.values \
        [(u32)READ_BYTE() | ((u32)READ_BYTE() << 8) | ((u32)READ_BYTE() << 16)])
#define BINARY_OP(op_) \
    do {                                                    \
        f64 b_ = pop_stack(ctx, vm);                        \
        *(vm->stack_top - 1) = *(vm->stack_top - 1) op_ b_; \
    } while (0)

    for (;;) {
#if DEBUG_TRACE_EXECUTION
        LOG_NONL("              ");
        for (vm_stack_segment_t const *seg = &vm->stack; seg; seg = seg->next) {
            for (
                value_t const
                    *slot = seg->values,
                    *end = seg == vm->cur_stack_seg
                        ? vm->stack_top
                        : seg->values + VM_STACK_SEGMENT_SIZE;
                slot < end;
                ++slot)
            {
                LOG_NONL("[ ");
                disasm_value(ctx, *slot);
                LOG_NONL(" ]");
            }
        }
        LOG("");
        disasm_instruction(ctx, vm->chunk, (uint)(vm->ip - vm->chunk->code));
#endif

        u8 instr;
        switch (instr = READ_BYTE()) {
        case e_op_constant: {
            value_t constant = READ_CONSTANT();
            push_stack(ctx, vm, constant);
        } break;

        case e_op_constant_long: {
            value_t constant = READ_CONSTANT_LONG();
            push_stack(ctx, vm, constant);
        } break;

        case e_op_add:
            BINARY_OP(+);
            break;

        case e_op_subtract:
            BINARY_OP(-);
            break;

        case e_op_multiply:
            BINARY_OP(*);
            break;

        case e_op_divide:
            BINARY_OP(/); // @TODO: zero div check
            break;

        case e_op_negate:
            *(vm->stack_top - 1) = -*(vm->stack_top - 1);
            break;

        case e_op_return:
            print_value(ctx, pop_stack(ctx, vm));
            OUTPUT("\n");
            return e_interp_ok;

        default:
            ASSERT(0);
            break;
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef BINARY_OP
}

static interp_result_t interpret_chunk(
    ctx_t const *ctx, vm_t *vm, chunk_t *chunk)
{
    vm->chunk = chunk;
    vm->ip = chunk->code;
    return run_vm(ctx, vm);
}

typedef enum {
  // Single-character tokens.
  e_tt_left_paren, e_tt_right_paren,
  e_tt_left_brace, e_tt_right_brace,
  e_tt_comma, e_tt_dot, e_tt_minus, e_tt_plus,
  e_tt_semicolon, e_tt_slash, e_tt_star,
  // One or two character tokens.
  e_tt_bang, e_tt_bang_equal,
  e_tt_equal, e_tt_equal_equal,
  e_tt_greater, e_tt_greater_equal,
  e_tt_less, e_tt_less_equal,
  // Literals.
  e_tt_identifier, e_tt_string, e_tt_number,
  // Keywords.
  e_tt_and, e_tt_class, e_tt_else, e_tt_false,
  e_tt_for, e_tt_fun, e_tt_if, e_tt_nil, e_tt_or,
  e_tt_print, e_tt_return, e_tt_super, e_tt_this,
  e_tt_true, e_tt_var, e_tt_while,

  e_tt_error, e_tt_eof
} token_type_t;

typedef struct {
    token_type_t type;
    char const *start;
    int len;
    int line;
} token_t;

#define MAKE_TOKEN(t_, sc_) \
    ((token_t){             \
        (t_), (sc_)->start, (int)((sc_)->cur - (sc_)->start), (sc_)->line})
#define ERROR_TOKEN(m_, sc_) \
    ((token_t){e_tt_error, (m_), STRLITLEN(m_), (sc_)->line})

#define IS_ALPHA(c_)                    \
    (((c_) >= 'A' && (c_) <= 'Z')       \
        || ((c_) >= 'a' && (c_) <= 'z') \
        || (c_) == '_')
#define IS_DIGIT(c_) ((c_) >= '0' && (c_) <= '9')

typedef struct {
    char const *start;
    char const *cur;
    char const *end;
    int line;
} scanner_t;

static b32 scanner_is_at_end(ctx_t const *ctx, scanner_t const *scanner)
{
    (void)ctx;
    return scanner->cur == scanner->end;
}

static char scanner_advance(ctx_t const *ctx, scanner_t *scanner)
{
    (void)ctx;
    return *scanner->cur++;
}

static b32 scanner_match(ctx_t const *ctx, char e, scanner_t *scanner)
{
    (void)ctx;
    if (scanner_is_at_end(ctx, scanner))
        return false;
    if (*scanner->cur != e)
        return false;
    ++scanner->cur;
    return true;
}

static char scanner_peek(ctx_t const *ctx, scanner_t const *scanner)
{
    (void)ctx;
    return *scanner->cur;
}

static char scanner_peek_next(ctx_t const *ctx, scanner_t const *scanner)
{
    return scanner_is_at_end(ctx, scanner) ? '\0' : scanner->cur[1];
}

static void skip_ws(ctx_t const *ctx, scanner_t *scanner)
{
    for (;;) {
        switch (scanner_peek(ctx, scanner)) {
        case ' ':
        case '\t':
        case '\r':
            break;
        case '\n':
            ++scanner->line;
            break;
        case '/':
            if (scanner_peek_next(ctx, scanner) == '/') {
                while (
                    scanner_peek(ctx, scanner) != '\n' &&
                    !scanner_is_at_end(ctx, scanner))
                {
                    scanner_advance(ctx, scanner);
                }
            } else {
                return;
            }
        default:
            return;
        }

        scanner_advance(ctx, scanner);
    }
}

static token_t scan_string(ctx_t const *ctx, scanner_t *scanner)
{
    char c;
    while (
        (c = scanner_peek(ctx, scanner)) != '"' &&
        !scanner_is_at_end(ctx, scanner))
    {
        if (c == '\n')
            ++scanner->line;
        scanner_advance(ctx, scanner);
    }

    if (scanner_is_at_end(ctx, scanner))
        return ERROR_TOKEN("Unterminated string.", scanner);

    scanner_advance(ctx, scanner);
    return MAKE_TOKEN(e_tt_string, scanner);
}

static token_t scan_number(ctx_t const *ctx, scanner_t *scanner)
{
    while (IS_DIGIT(scanner_peek(ctx, scanner)))
        scanner_advance(ctx, scanner);

    if (scanner_peek(ctx, scanner) == '.') {
        scanner_advance(ctx, scanner);
        while (IS_DIGIT(scanner_peek(ctx, scanner)))
            scanner_advance(ctx, scanner);
    }

    return MAKE_TOKEN(e_tt_number, scanner);
}

static token_type_t scanner_check_kw(
    ctx_t const *ctx,
    int start, string_t rest, token_type_t type,
    scanner_t const *scanner)
{
    (void)ctx;
    if (scanner->cur - scanner->start == (isize)start + (isize)rest.len) {
        char const *p = scanner->start + start;
        while (rest.len--) {
            if (*p++ != *rest.p++)
                goto ret_id;
        }
        return type;
    }

ret_id:
    return e_tt_identifier;
}

static token_type_t scanner_id_type(ctx_t const *ctx, scanner_t const *scanner)
{
    switch (scanner->start[0]) {
    case 'a':
        return scanner_check_kw(ctx, 1, LITSTR("nd"), e_tt_and, scanner);
    case 'c':
        return scanner_check_kw(ctx, 1, LITSTR("lass"), e_tt_class, scanner);
    case 'e':
        return scanner_check_kw(ctx, 1, LITSTR("lse"), e_tt_else, scanner);
    case 'f':
        if (scanner->cur - scanner->start > 1) {
            switch (scanner->start[1]) {
                case 'a':
                    return scanner_check_kw(
                        ctx, 2, LITSTR("lse"), e_tt_false, scanner);
                case 'o':
                    return scanner_check_kw(
                        ctx, 2, LITSTR("r"), e_tt_for, scanner);
                case 'u':
                    return scanner_check_kw(
                        ctx, 2, LITSTR("n"), e_tt_fun, scanner);
                default:
                    break;
            }
        }
        break;
    case 'i':
        return scanner_check_kw(ctx, 1, LITSTR("f"), e_tt_if, scanner);
    case 'n':
        return scanner_check_kw(ctx, 1, LITSTR("il"), e_tt_nil, scanner);
    case 'o':
        return scanner_check_kw(ctx, 1, LITSTR("r"), e_tt_or, scanner);
    case 'p':
        return scanner_check_kw(ctx, 1, LITSTR("rint"), e_tt_print, scanner);
    case 'r':
        return scanner_check_kw(ctx, 1, LITSTR("eturn"), e_tt_return, scanner);
    case 's':
        return scanner_check_kw(ctx, 1, LITSTR("uper"), e_tt_super, scanner);
    case 't':
        if (scanner->cur - scanner->start > 1) {
            switch (scanner->start[1]) {
                case 'h':
                    return scanner_check_kw(
                        ctx, 2, LITSTR("is"), e_tt_this, scanner);
                case 'r':
                    return scanner_check_kw(
                        ctx, 2, LITSTR("ue"), e_tt_true, scanner);
                default:
                    break;
            }
        }
        break;
    case 'v':
        return scanner_check_kw(ctx, 1, LITSTR("ar"), e_tt_var, scanner);
    case 'w':
        return scanner_check_kw(ctx, 1, LITSTR("hile"), e_tt_while, scanner);
    default:
        break;
    }

    return e_tt_identifier;
}

static token_t scan_id(ctx_t const *ctx, scanner_t *scanner)
{
    while (
        IS_ALPHA(scanner_peek(ctx, scanner)) ||
        IS_DIGIT(scanner_peek(ctx, scanner)))
    {
        scanner_advance(ctx, scanner);
    }
    return MAKE_TOKEN(scanner_id_type(ctx, scanner), scanner);
}

static token_t scan_token(ctx_t const *ctx, scanner_t *scanner)
{
    skip_ws(ctx, scanner);
    scanner->start = scanner->cur;

    if (scanner_is_at_end(ctx, scanner))
        return MAKE_TOKEN(e_tt_eof, scanner);

    char c = scanner_advance(ctx, scanner);

    if (IS_ALPHA(c))
        return scan_id(ctx, scanner);
    if (IS_DIGIT(c))
        return scan_number(ctx, scanner);

    switch (c) {
    case '(':
        return MAKE_TOKEN(e_tt_left_paren, scanner);
    case ')':
        return MAKE_TOKEN(e_tt_right_paren, scanner);
    case '{':
        return MAKE_TOKEN(e_tt_left_brace, scanner);
    case '}':
        return MAKE_TOKEN(e_tt_right_brace, scanner);
    case ';':
        return MAKE_TOKEN(e_tt_semicolon, scanner);
    case ',':
        return MAKE_TOKEN(e_tt_comma, scanner);
    case '.':
        return MAKE_TOKEN(e_tt_dot, scanner);
    case '-':
        return MAKE_TOKEN(e_tt_minus, scanner);
    case '+':
        return MAKE_TOKEN(e_tt_plus, scanner);
    case '/':
        return MAKE_TOKEN(e_tt_slash, scanner);
    case '*':
        return MAKE_TOKEN(e_tt_star, scanner);
    case '!':
        return MAKE_TOKEN(
            scanner_match(ctx, '=', scanner)
                ? e_tt_bang_equal
                : e_tt_bang,
            scanner);
    case '=':
        return MAKE_TOKEN(
            scanner_match(ctx, '=', scanner)
                ? e_tt_equal_equal
                : e_tt_equal,
            scanner);
    case '<':
        return MAKE_TOKEN(
            scanner_match(ctx, '=', scanner)
                ? e_tt_less_equal
                : e_tt_less,
            scanner);
    case '>':
        return MAKE_TOKEN(
            scanner_match(ctx, '=', scanner)
                ? e_tt_greater_equal
                : e_tt_greater,
            scanner);
    case '"':
        return scan_string(ctx, scanner);
    }

    return ERROR_TOKEN("Unexpected character.", scanner);
}

static void compile(ctx_t const *ctx, string_t source)
{
    scanner_t scanner = {source.p, source.p, source.p + source.len, 1}; 
    int cur_line = -1;
    for (;;) {
        token_t tok = scan_token(ctx, &scanner);

        // @TEST
        if (tok.line != cur_line) {
            isize chars = LOGF_NONL("%u ", tok.line);
            for (isize i = chars; i < 6; ++i)
                LOG_NONL(" ");
            cur_line = tok.line;
        } else {
            LOG_NONL("|");
            for (isize i = 0; i < 5; ++i)
                LOG_NONL(" ");
        }

        {
            isize chars = LOGF_NONL("%u", tok.type);
            if (chars < 2)
                LOG_NONL(" ");
            LOGF(" %S", (string_t){(char *)tok.start, tok.len});
        }

        if (tok.type == e_tt_eof)
            break;
    }
}

static interp_result_t interpret(ctx_t const *ctx, vm_t *vm, string_t source)
{
    compile(ctx, source);

    (void)vm;

    (void)interpret_chunk;
    (void)disasm_chunk;

    return e_interp_ok;
}

// @TODO: fix for files with <
static void repl(ctx_t const *ctx, vm_t *vm)
{
    char line[1024];
    usize buffered_chars = 0;
    b32 eof = false;

    while (!eof || buffered_chars) {
        OUTPUT("> ");

        isize chars_read = 0;

        if (!eof) {
            chars_read = io_read(
                ctx, &ctx->os->hstdin,
                (u8 *)line + buffered_chars, sizeof(line) - buffered_chars);
        }
        if (chars_read <= 0) {
            eof = true;
            chars_read = 0;
        }

        buffered_chars += (usize)chars_read;
        usize line_break = 0;
        while (line_break < buffered_chars && line[line_break] != '\n')
            ++line_break;

        if (line_break == buffered_chars) {
            LOG("Max repl line is 1023 characters long");
            buffered_chars = 0;
            continue;
        }

        line[line_break] = '\0';

        string_t source = {line, line_break};
        interpret(ctx, vm, source);

        buffered_chars -= line_break + 1;
        char *src = &line[line_break + 1], *dst = line;
        for (usize i = 0; i < buffered_chars; ++i)
            *dst++ = *src++;
    }
}

static void run_file(ctx_t const *ctx, vm_t *vm, char const *fname)
{
    io_file_t f = io_read_open_file(ctx, fname);
    if (!io_file_is_valid(ctx, &f)) {
        LOGF("Failed to open script '%s'.", fname);
        os_exit(ctx, 65);
    }

    char *script = (char *)reallocate(ctx, NULL, 0, f.len + 1);
    usize chars_read = (usize)io_read(ctx, &f.ioh, (u8 *)script, f.len);
    VERIFY(chars_read == f.len, "Failed to read script.");
    script[f.len] = '\0';

    string_t source = {script, f.len};
    interp_result_t res = interpret(ctx, vm, source);

    if (res == e_interp_compile_err)
        os_exit(ctx, 70);
    else if (res == e_interp_runtime_err)
        os_exit(ctx, 75);
}

static int lox_main(ctx_t const *ctx)
{
    vm_t vm = {0};
    init_vm(ctx, &vm);

    if (ctx->os->argc == 1) {
        repl(ctx, &vm);
    } else if (ctx->os->argc == 2) {
        run_file(ctx, &vm, ctx->os->argv[1]);
    } else {
        LOG("Usage: clox [file].");
        os_exit(ctx, 64);
    }

    return 0;
}

