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

#define ALLOCATE(type_, count_) \
    (type_ *)reallocate(ctx, NULL, 0, sizeof(type_) * (count_))
#define FREE(ptr_) reallocate(ctx, ptr_, 0, 0)

#define GROW_CAP(cap_) ((cap_) < 8 ? 8 : 2 * (cap_))
#define GROW_ARRAY(type_, ptr_, oldcap_, newcap_) \
    (type_ *)reallocate(                          \
        ctx, (ptr_), (oldcap_) * sizeof(type_), (newcap_) * sizeof(type_))
#define FREE_ARRAY(type_, ptr_, cap_) \
    reallocate(ctx, (ptr_), sizeof(type_) * (cap_), 0)

static inline u32 hash_string(ctx_t const *ctx, char const *p, uint len)
{
    (void)ctx;
     u32 hash = 2166136261u;
     for (uint i = 0; i < len; i++) {
         hash ^= (u32)p[i];
         hash *= 16777619;
     }
     return hash;
}

typedef enum {
    e_vt_nil,
    e_vt_bool,
    e_vt_number,
    e_vt_obj,

    e_vt_vacant = -1,
} value_type_t;

typedef struct obj obj_t;
typedef struct obj_string obj_string_t;

typedef struct {
    value_type_t type;
    union {
        b32 boolean;
        f64 number;
        obj_t *obj;
    } as;
} value_t;

#define IS_BOOL(value_)    ((value_).type == e_vt_bool)
#define IS_NIL(value_)     ((value_).type == e_vt_nil)
#define IS_NUMBER(value_)  ((value_).type == e_vt_number)
#define IS_OBJ(value_)     ((value_).type == e_vt_obj)
#define IS_VACANT(value_)  ((value_).type == e_vt_vacant)

#define AS_BOOL(value_)    ((value_).as.boolean)
#define AS_NUMBER(value_)  ((value_).as.number)
#define AS_OBJ(value_)     ((value_).as.obj)

#define BOOL_VAL(value_)   ((value_t){e_vt_bool,   {.boolean = (value_)}})
#define NIL_VAL            ((value_t){e_vt_nil,    {.number  = 0.0     }})
#define NUMBER_VAL(value_) ((value_t){e_vt_number, {.number  = (value_)}})
#define OBJ_VAL(value_)    ((value_t){e_vt_obj,    {.obj     = (value_)}})
#define VACANT_VAL         ((value_t){e_vt_vacant, {.obj     = NULL    }})

#define VACANT_VAL_NAMED(nm_obj_) ((value_t){e_vt_vacant, {.obj = (nm_obj_)}})

static b32 is_truthy(ctx_t const *ctx, value_t val)
{
    (void)ctx;
    return !IS_NIL(val) && (!IS_BOOL(val) || AS_BOOL(val));
}

static b32 is_falsey(ctx_t const *ctx, value_t val)
{
    (void)ctx;
    return !is_truthy(ctx, val);
}

typedef enum {
    e_ot_string,
} obj_type_t;

typedef struct obj {
    obj_type_t type;
    struct obj *next;
} obj_t;

typedef enum {
    e_st_allocated,
    e_st_ref
} obj_string_type_t;

typedef struct obj_string {
    obj_t obj;
    obj_string_type_t type;
    uint len;
    u32 hash;
    char *p;
} obj_string_t;

static inline b32 is_obj_type(ctx_t const *ctx, value_t val, obj_type_t ot)
{
    (void)ctx;
    return IS_OBJ(val) && AS_OBJ(val)->type == ot;
}

static inline string_t obj_as_string(ctx_t const *ctx, value_t val)
{
    (void)ctx;
    obj_string_t *ostr = (obj_string_t *)AS_OBJ(val);
    return (string_t){ostr->p, (u64)ostr->len};
}

#define OBJ_TYPE(value_)      (AS_OBJ(value_)->type)

#define IS_STRING(value_)     (is_obj_type(ctx, (value_), e_ot_string))

#define AS_STRING_OBJ(value_) ((obj_string_t *)AS_OBJ(value_))
#define AS_STRING(value_)     (obj_as_string(ctx, (value_)))
#define AS_CSTRING(value_)    (((obj_string_t *)AS_OBJ(value_))->p)

static b32 are_equal(ctx_t const *ctx, value_t v1, value_t v2)
{
    (void)ctx;
    if (v1.type != v2.type)
        return false;

    switch (v1.type) {
    case e_vt_bool:
        return AS_BOOL(v1) == AS_BOOL(v2);
    case e_vt_nil:
        return true;
    case e_vt_number:
        return ABS(AS_NUMBER(v1) - AS_NUMBER(v2)) < 1e-16;
    case e_vt_obj:
        // Due to string interning equal strings are always equal obj ptrs.
        return AS_OBJ(v1) == AS_OBJ(v2);
    case e_vt_vacant:
        ASSERT(0);
        return false;
    }
}

#define TABLE_MAX_LOAD_FACTOR 0.75

typedef struct {
    obj_string_t *key;
    value_t value;
} table_entry_t;

typedef struct {
    uint cnt, cap;
    table_entry_t *entries;
} table_t;

static table_t make_table(ctx_t const *ctx)
{
    (void)ctx;
    return (table_t){0, 0, NULL};
}

static void free_table(ctx_t const *ctx, table_t *table)
{
    if (!table || !table->entries)
        return;
    FREE_ARRAY(table_entry_t, table->entries, table->cap);
    *table = make_table(ctx);
}

static table_entry_t *table_find_entry(
    ctx_t const *ctx, table_entry_t const *entries, uint cap,
    obj_string_t const *key)
{
    (void)ctx;
    u32 idx = key->hash % cap;
    table_entry_t const *tombstone = NULL;
    for (;;) {
        table_entry_t const *e = &entries[idx];
        if (!e->key) {
            if (IS_NIL(e->value))
                return (table_entry_t *)(tombstone ? tombstone : e);
            else if (!tombstone)
                tombstone = e;
        } else if (e->key == key) {
            return (table_entry_t *)e;
        }

        idx = (idx + 1) % cap;
    }
}

static void table_adjust_cap(
    ctx_t const *ctx, table_t *table, uint new_cap)
{
    table_entry_t *entries = ALLOCATE(table_entry_t, new_cap);
    memset(entries, 0, new_cap * sizeof(table_entry_t));

    table->cnt = 0;
    for (uint i = 0; i < MIN(table->cap, new_cap); ++i) {
        table_entry_t *e = &table->entries[i];
        if (!e->key)
            continue;

        table_entry_t *dest =
            table_find_entry(ctx, entries, new_cap, e->key);
        dest->key = e->key;
        dest->value = e->value;
        ++table->cnt;
    }

    if (table->entries)
        FREE_ARRAY(table_entry_t, table->entries, table->cap);
    table->entries = entries;
    table->cap = new_cap;
}

static b32 table_set(
    ctx_t const *ctx, table_t *table, obj_string_t const *key, value_t val)
{
    if ((f64)(table->cnt + 1) > (f64)table->cap * TABLE_MAX_LOAD_FACTOR) {
        uint cap = GROW_CAP(table->cap);
        table_adjust_cap(ctx, table, cap);
    }

    table_entry_t *entry =
        table_find_entry(ctx, table->entries, table->cap, key);
    b32 key_is_new = !entry->key;
    if (key_is_new && IS_NIL(entry->value))
        ++table->cnt;

    entry->key = (obj_string_t *)key;
    entry->value = val;
    return key_is_new;
}

static void table_add_all(
    ctx_t const *ctx, table_t *to, table_t *from)
{
    for (uint i = 0; i < from->cap; ++i) {
        table_entry_t *e = &from->entries[i];
        if (e->key)
            table_set(ctx, to, e->key, e->value);
    }
}

static b32 table_get(
    ctx_t const *ctx, table_t const *table,
    obj_string_t const *key, value_t *val)
{
    if (!table->cnt)
        return false;

    table_entry_t const *entry =
        table_find_entry(ctx, table->entries, table->cap, key);
    if (!entry->key)
        return false;

    *val = entry->value;
    return true;
}

static b32 table_delete(
    ctx_t const *ctx, table_t *table, obj_string_t const *key)
{
    if (!table->cnt)
        return false;

    table_entry_t *entry =
        table_find_entry(ctx, table->entries, table->cap, key);
    if (!entry->key)
        return false;

    entry->key = NULL;
    entry->value = VACANT_VAL; // tombstone
    return true;
}

static obj_string_t *table_find_string(
    ctx_t const *ctx, table_t const *table, char const *s, uint len, u32 hash)
{
    (void)ctx;
    if (!table->cnt)
        return NULL;
    u32 idx = hash % table->cap;
    for (;;) {
        table_entry_t const *e = &table->entries[idx];
        if (!e->key) {
            if (IS_NIL(e->value))
                return NULL;
        } else if (
            e->key->len == len &&
            e->key->hash == hash &&
            string_eq(
                (string_t){(char *)s, (u64)len},
                (string_t){e->key->p, (u64)e->key->len}))
        {
            return e->key;
        }

        idx = (idx + 1) % table->cap;
    }
}

typedef enum {
    e_op_constant,
    e_op_constant_long,
    e_op_nil,
    e_op_true,
    e_op_false,
    e_op_pop,
    e_op_define_glob,
    e_op_define_glob_long,
    e_op_get_glob,
    e_op_get_glob_long,
    e_op_set_glob,
    e_op_set_glob_long,
    e_op_eq,
    e_op_neq,
    e_op_gt,
    e_op_ge,
    e_op_lt,
    e_op_le,
    e_op_add,
    e_op_subtract,
    e_op_multiply,
    e_op_divide,
    e_op_not,
    e_op_negate,
    e_op_print,
    e_op_return,
} opcode_t;

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
        for (uint i = old_cap; i < arr->cap; ++i)
            arr->values[i] = VACANT_VAL;
    }

    arr->values[arr->cnt++] = val;
}

static inline void free_value_array(ctx_t const *ctx, value_array_t *arr)
{
    if (!arr || !arr->values)
        return;
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
    if (!info || !info->entries)
        return;
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

static inline void write_id_ref(
    ctx_t const *ctx, chunk_t *chunk,
    opcode_t op, opcode_t long_op, uint id, uint line)
{
    if (id <= 255) {
        write_chunk(ctx, chunk, (u8)op, line);
        write_chunk(ctx, chunk, (u8)id, line);
    } else if (id <= 16777215) {
        write_chunk(ctx, chunk, (u8)long_op, line);
        write_chunk(ctx, chunk, (u8)(id & 0xFF), line);
        write_chunk(ctx, chunk, (u8)((id >> 8) & 0xFF), line);
        write_chunk(ctx, chunk, (u8)((id >> 16) & 0xFF), line);
    } else {
        // @TODO: a more gracious error?
        PANIC(
            "Constants overflow: "
            "one chunk can't contain more than 16777215 consts/entities.");
    }
}

static inline void write_constant(
    ctx_t const *ctx, chunk_t *chunk,
    opcode_t op, opcode_t long_op, value_t val, uint line)
{
    uint constant = add_constant(ctx, chunk, val);
    write_id_ref(ctx, chunk, op, long_op, constant, line);
}

static inline void free_chunk(ctx_t const *ctx, chunk_t *chunk)
{
    if (!chunk)
        return;
    if (chunk->code)
        FREE_ARRAY(u8, chunk->code, chunk->cap);
    free_value_array(ctx, &chunk->constants);
    free_line_info(ctx, &chunk->lines);
    *chunk = make_chunk(ctx);
}

static void print_obj(ctx_t const *ctx, io_handle_t *hnd, value_t val)
{
    switch (OBJ_TYPE(val)) {
    case e_ot_string:
        fmt_print(ctx, hnd, "%S", AS_STRING(val));
        break;
    }
}

static void print_value(ctx_t const *ctx, value_t val)
{
    switch (val.type) {
    case e_vt_bool:
        OUTPUTF("%s", AS_BOOL(val) ? "true" : "false");
        break;
    case e_vt_nil:
        OUTPUT("nil");
        break;
    case e_vt_number:
        OUTPUTF("%f", AS_NUMBER(val));
        break;
    case e_vt_obj:
        print_obj(ctx, &ctx->os->hstdout, val);
        break;
    case e_vt_vacant:
        ASSERT(0);
        break;
    }
}

#if DEBUG_TRACE_EXECUTION || DEBUG_PRINT_CODE

static void disasm_value(ctx_t const *ctx, value_t val)
{
    switch (val.type) {
    case e_vt_bool:
        LOGF_NONL("%s", AS_BOOL(val) ? "true" : "false");
        break;
    case e_vt_nil:
        LOG_NONL("nil");
        break;
    case e_vt_number:
        LOGF_NONL("%f", AS_NUMBER(val));
        break;
    case e_vt_obj:
        print_obj(ctx, &ctx->os->hstderr, val);
        break;
    case e_vt_vacant:
        LOG_NONL("<vacant>");
        break;
    }
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

static uint disasm_gvar_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at)
{
    u8 gvar = chunk->code[at + 1];
    LOGF("%s %u", name, gvar);
    return at + 2;
}

static uint disasm_long_gvar_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at)
{
    uint gvar =
        chunk->code[at + 1]
        | ((uint)chunk->code[at + 2] << 8)
        | ((uint)chunk->code[at + 3] << 16);
    LOGF("%s %u", name, gvar);
    return at + 4;
}

static uint disasm_instruction(
    ctx_t const *ctx, chunk_t const *chunk, uint at)
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
    case e_op_nil:
        return disasm_simple_instruction(
            ctx, "OP_NIL", at);
    case e_op_true:
        return disasm_simple_instruction(
            ctx, "OP_TRUE", at);
    case e_op_false:
        return disasm_simple_instruction(
            ctx, "OP_FALSE", at);
    case e_op_pop:
        return disasm_simple_instruction(
            ctx, "OP_POP", at);
    case e_op_define_glob:
        return disasm_gvar_instruction(
            ctx, "OP_DEFINE_GLOB", chunk, at);
    case e_op_define_glob_long:
        return disasm_long_gvar_instruction(
            ctx, "OP_DEFINE_GLOB_LONG", chunk, at);
    case e_op_get_glob:
        return disasm_gvar_instruction(
            ctx, "OP_GET_GLOB", chunk, at);
    case e_op_get_glob_long:
        return disasm_long_gvar_instruction(
            ctx, "OP_GET_GLOB_LONG", chunk, at);
    case e_op_set_glob:
        return disasm_gvar_instruction(
            ctx, "OP_SET_GLOB", chunk, at);
    case e_op_set_glob_long:
        return disasm_long_gvar_instruction(
            ctx, "OP_SET_GLOB_LONG", chunk, at);
    case e_op_eq:
        return disasm_simple_instruction(
            ctx, "OP_EQ", at);
    case e_op_neq:
        return disasm_simple_instruction(
            ctx, "OP_NEQ", at);
    case e_op_gt:
        return disasm_simple_instruction(
            ctx, "OP_GT", at);
    case e_op_ge:
        return disasm_simple_instruction(
            ctx, "OP_GE", at);
    case e_op_lt:
        return disasm_simple_instruction(
            ctx, "OP_LT", at);
    case e_op_le:
        return disasm_simple_instruction(
            ctx, "OP_LE", at);
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
    case e_op_not:
        return disasm_simple_instruction(
            ctx, "OP_NOT", at);
    case e_op_negate:
        return disasm_simple_instruction(
            ctx, "OP_NEGATE", at);
    case e_op_print:
        return disasm_simple_instruction(
            ctx, "OP_PRINT", at);
    case e_op_return:
        return disasm_simple_instruction(
            ctx, "OP_RETURN", at);
    default:
        LOGF("Unknown opcode %d", (int)instr);
        return at + 1;
    }
}

#if DEBUG_PRINT_CODE

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
    value_t *stack_head;
    table_t strings;
    table_t gvar_map;
    value_array_t gvars;
    obj_t *objects;
} vm_t;

static inline obj_t *allocate_obj(
    ctx_t const *ctx, vm_t *vm, usize sz, obj_type_t ot)
{
    ASSERT(sz > sizeof(obj_t));
    obj_t *obj = (obj_t *)reallocate(ctx, NULL, 0, sz);
    obj->type = ot;
    obj->next = vm->objects;
    vm->objects = obj;
    return obj;
}

static inline obj_t *allocate_string(ctx_t const *ctx, vm_t *vm, uint len)
{
    obj_t *obj = allocate_obj(
        ctx, vm, sizeof(obj_string_t) + len + 1, e_ot_string);
    ((obj_string_t *)obj)->type = e_st_allocated;
    ((obj_string_t *)obj)->p = (char *)(obj) + sizeof(obj_string_t);
    ((obj_string_t *)obj)->len = len;
    return obj;
}

#define ALLOCATE_OBJ(type_, ot_, vm_) \
    ((type_ *)allocate_obj(ctx, (vm_), sizeof(type_), (ot_)))

#define ALLOCATE_STR(len_, vm_) \
    ((obj_string_t *)allocate_string(ctx, (vm_), (len_)))

static inline obj_t *add_string_ref(
    ctx_t const *ctx, vm_t *vm, char const *p, uint len)
{
    u32 hash = hash_string(ctx, p, len);
    obj_string_t *str = table_find_string(ctx, &vm->strings, p, len, hash);
    if (!str) {
        str = ALLOCATE_OBJ(obj_string_t, e_ot_string, vm);
        str->type = e_st_ref;
        str->p = (char *)p;
        str->len = len;
        str->hash = hash;
        table_set(ctx, &vm->strings, str, NIL_VAL);
    }

    return (obj_t *)str;
}

static void reset_stack(ctx_t const *ctx, vm_t *vm)
{
    (void)ctx;
    vm->cur_stack_seg = &vm->stack;
    vm->stack_head = vm->stack.values;
}

static void init_vm(ctx_t const *ctx, vm_t *vm)
{
    reset_stack(ctx, vm);
    vm->strings = make_table(ctx);
    vm->gvar_map = make_table(ctx);
    vm->gvars = make_value_array(ctx);
    vm->objects = NULL;
}

static void free_vm(ctx_t const *ctx, vm_t *vm)
{
    free_table(ctx, &vm->strings);
    free_table(ctx, &vm->gvar_map);
    free_value_array(ctx, &vm->gvars);
    obj_t *obj = vm->objects;
    while (obj) {
        obj_t *next = obj->next;
        FREE(obj);
        obj = next;
    }
}

#define PUSH_STACK_SEG(vm_)                                             \
    do {                                                                \
        vm_stack_segment_t *new_seg_ = ALLOCATE(vm_stack_segment_t, 1); \
        (vm_)->cur_stack_seg->next = new_seg_;                          \
        new_seg_->prev = (vm_)->cur_stack_seg;                          \
        new_seg_->next = NULL;                                          \
        (vm_)->cur_stack_seg = new_seg_;                                \
        (vm_)->stack_head = new_seg_->values;                           \
    } while (0)

#define POP_STACK_SEG(vm_)                                        \
    do {                                                          \
        vm_stack_segment_t *old_seg_ = (vm_)->cur_stack_seg;      \
        (vm_)->cur_stack_seg = old_seg_->prev;                    \
        (vm_)->cur_stack_seg->next = NULL;                        \
        FREE(old_seg_);                                           \
        (vm_)->stack_head =                                       \
            (vm_)->cur_stack_seg->values + VM_STACK_SEGMENT_SIZE; \
    } while (0)

static void push_stack(ctx_t const *ctx, vm_t *vm, value_t val)
{
    if (vm->stack_head - vm->cur_stack_seg->values >= VM_STACK_SEGMENT_SIZE)
        PUSH_STACK_SEG(vm);

    *vm->stack_head++ = val;
}

static value_t pop_stack(ctx_t const *ctx, vm_t *vm)
{
    ASSERT(vm->stack_head > vm->cur_stack_seg->values);
    value_t res = *(--vm->stack_head);

    if (vm->stack_head == vm->cur_stack_seg->values && vm->cur_stack_seg->prev)
        POP_STACK_SEG(vm);

    return res;
}

#define STACK_TOP(vm_) (*(vm->stack_head - 1))

static value_t peek_stack(ctx_t const *ctx, vm_t const *vm, int dist)
{
    (void)ctx;
    if (dist == 0)
        return STACK_TOP(vm);

    value_t const *p = vm->stack_head - 1;
    vm_stack_segment_t const *s = vm->cur_stack_seg;
    while (s && p - s->values < dist) {
        dist -= (p - s->values);
        s = s->prev;
        p = &s->values[VM_STACK_SEGMENT_SIZE - 1];
    }
    ASSERT(s);
    return p[-dist];
}

typedef enum {
    e_interp_ok,
    e_interp_compile_err,
    e_interp_runtime_err
} interp_result_t;

static void runtime_error(ctx_t const *ctx, vm_t *vm, char const *fmt, ...)
{
    VA_LIST args;
    VA_START(args, fmt);
    fmt_vprint(ctx, &ctx->os->hstderr, fmt, args);
    VA_END(args);

    uint instr = (uint)(vm->ip - vm->chunk->code - 1);
    uint line = 0;
    for (usize i = 0; i < vm->chunk->lines.cnt; ++i) {
        if (instr < vm->chunk->lines.entries[i].instr_range_end) {
            line = vm->chunk->lines.entries[i].line;
            break;
        }
    }

    LOGF("\n[line %d] in script", line);

    reset_stack(ctx, vm);
}

static interp_result_t run_chunk(ctx_t const *ctx, vm_t *vm)
{
#define READ_BYTE() (*vm->ip++)
#define READ_LONG() \
    ((u32)READ_BYTE() | ((u32)READ_BYTE() << 8) | ((u32)READ_BYTE() << 16))
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm->chunk->constants.values[READ_LONG()])
#define READ_STRING() AS_STRING_OBJ(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING_OBJ(READ_CONSTANT_LONG())
#define BINARY_OP(op_, vt_)                                      \
    do {                                                         \
        if (!IS_NUMBER(peek_stack(ctx, vm, 0)) ||                \
            !IS_NUMBER(peek_stack(ctx, vm, 1))) {                \
            runtime_error(ctx, vm, "Operands must be numbers."); \
            return e_interp_runtime_err;                         \
        }                                                        \
        f64 b_ = AS_NUMBER(pop_stack(ctx, vm));                  \
        STACK_TOP(vm) = vt_(AS_NUMBER(STACK_TOP(vm)) op_ b_);    \
    } while (0)

    for (;;) {
#if DEBUG_TRACE_EXECUTION
        LOG_NONL("              ");
        for (vm_stack_segment_t const *seg = &vm->stack; seg; seg = seg->next) {
            for (
                value_t const
                    *slot = seg->values,
                    *end = seg == vm->cur_stack_seg
                        ? vm->stack_head
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

        case e_op_nil:
            push_stack(ctx, vm, NIL_VAL);
            break;
        case e_op_true:
            push_stack(ctx, vm, BOOL_VAL(true));
            break;
        case e_op_false:
            push_stack(ctx, vm, BOOL_VAL(false));
            break;

        case e_op_pop:
            pop_stack(ctx, vm);
            break;

        case e_op_define_glob:
            vm->gvars.values[READ_BYTE()] = pop_stack(ctx, vm);
            break;

        case e_op_define_glob_long:
            vm->gvars.values[READ_LONG()] = pop_stack(ctx, vm);
            break;

        case e_op_get_glob: {
            uint gid = READ_BYTE();
            if (IS_VACANT(vm->gvars.values[gid])) {
                ASSERT(AS_OBJ(vm->gvars.values[gid]));
                runtime_error(
                    ctx, vm, "Undefined variable '%S'.",
                    AS_STRING(vm->gvars.values[gid]));
                return e_interp_runtime_err;
            }
            push_stack(ctx, vm, vm->gvars.values[gid]);
        } break;

        case e_op_get_glob_long: {
            uint gid = READ_LONG();
            if (IS_VACANT(vm->gvars.values[gid])) {
                ASSERT(AS_OBJ(vm->gvars.values[gid]));
                runtime_error(
                    ctx, vm, "Undefined variable '%S'.",
                    AS_STRING(vm->gvars.values[gid]));
                return e_interp_runtime_err;
            }
            push_stack(ctx, vm, vm->gvars.values[gid]);
        } break;

        case e_op_set_glob: {
            uint gid = READ_BYTE();
            if (IS_VACANT(vm->gvars.values[gid])) {
                ASSERT(AS_OBJ(vm->gvars.values[gid]));
                runtime_error(
                    ctx, vm, "Undefined variable '%S'.",
                    AS_STRING(vm->gvars.values[gid]));
                return e_interp_runtime_err;
            }
            vm->gvars.values[gid] = STACK_TOP(vm);
        } break;

        case e_op_set_glob_long: {
            uint gid = READ_LONG();
            if (IS_VACANT(vm->gvars.values[gid])) {
                ASSERT(AS_OBJ(vm->gvars.values[gid]));
                runtime_error(
                    ctx, vm, "Undefined variable '%S'.",
                    AS_STRING(vm->gvars.values[gid]));
                return e_interp_runtime_err;
            }
            vm->gvars.values[gid] = STACK_TOP(vm);
        } break;

        case e_op_eq: {
            value_t b = pop_stack(ctx, vm);
            STACK_TOP(vm) = BOOL_VAL(are_equal(ctx, STACK_TOP(vm), b));
        } break;
        case e_op_neq: {
            value_t b = pop_stack(ctx, vm);
            STACK_TOP(vm) = BOOL_VAL(!are_equal(ctx, STACK_TOP(vm), b));
        } break;

        case e_op_gt:
            BINARY_OP(>, BOOL_VAL);
            break;
        case e_op_ge:
            BINARY_OP(>=, BOOL_VAL);
            break;
        case e_op_lt:
            BINARY_OP(<, BOOL_VAL);
            break;
        case e_op_le:
            BINARY_OP(<=, BOOL_VAL);
            break;
        case e_op_add:
            if (IS_STRING(peek_stack(ctx, vm, 0)) &&
                IS_STRING(peek_stack(ctx, vm, 1)))
            {
                string_t b = AS_STRING(pop_stack(ctx, vm));
                string_t a = AS_STRING(STACK_TOP(vm));
                obj_string_t *c = ALLOCATE_STR((uint)(a.len + b.len), vm);
                memcpy(c->p, a.p, a.len);
                memcpy(c->p + a.len, b.p, b.len);
                c->p[c->len] = '\0';
                c->hash = hash_string(ctx, c->p, c->len);
                obj_string_t *str =
                    table_find_string(ctx, &vm->strings, c->p, c->len, c->hash);
                if (str) {
                    FREE(c);
                } else {
                    str = c;
                    table_set(ctx, &vm->strings, str, NIL_VAL);
                }
                STACK_TOP(vm) = OBJ_VAL((obj_t *)str);
            } else if (
                IS_NUMBER(peek_stack(ctx, vm, 0)) &&
                IS_NUMBER(peek_stack(ctx, vm, 1)))
            {
                f64 b = AS_NUMBER(pop_stack(ctx, vm));
                STACK_TOP(vm) = NUMBER_VAL(AS_NUMBER(STACK_TOP(vm)) + b);
            } else {
                runtime_error(ctx, vm, "Operands must be numbers.");
                return e_interp_runtime_err;
            }
            break;
        case e_op_subtract:
            BINARY_OP(-, NUMBER_VAL);
            break;
        case e_op_multiply:
            BINARY_OP(*, NUMBER_VAL);
            break;
        case e_op_divide:
            BINARY_OP(/, NUMBER_VAL); // @TODO: zero div check
            break;

        case e_op_not:
            STACK_TOP(vm) = BOOL_VAL(is_falsey(ctx, STACK_TOP(vm)));
            break;
        case e_op_negate:
            if (!IS_NUMBER(STACK_TOP(vm))) {
                runtime_error(ctx, vm, "Operand must be a number.");
                return e_interp_runtime_err;
            }
            STACK_TOP(vm) = NUMBER_VAL(-AS_NUMBER(STACK_TOP(vm)));
            break;

        case e_op_print:
#if DEBUG_TRACE_EXECUTION
            LOG("");
#endif
            print_value(ctx, pop_stack(ctx, vm));
            OUTPUT("\n");
            break;

        case e_op_return:
            return e_interp_ok;

        default:
            ASSERT(0);
            break;
        }
    }

#undef READ_BYTE
#undef READ_STRING
#undef READ_STRING_LONG
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef BINARY_OP
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
            break;
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
    default:
        ASSERT(0);
        break;
    }

    return ERROR_TOKEN("Unexpected character.", scanner);
}

typedef struct {
    token_t cur;
    token_t prev;
    scanner_t *scanner;
    b32 had_error;
    b32 panic_mode;
} parser_t;

static void parser_error_at(
    ctx_t const *ctx, token_t const *tok, char const *msg, parser_t *parser)
{
    if (parser->panic_mode)
        return;
    parser->panic_mode = true;
    LOGF_NONL("[line %d] Error", tok->line);
    if (tok->type == e_tt_eof)
        LOG_NONL(" at end");
    else if (tok->type == e_tt_error)
        ;
    else
        LOGF_NONL(" at '%S'", (string_t){(char *)tok->start, (usize)tok->len});
    LOGF(": %s", msg);
    parser->had_error = true;
}

static void parser_error_at_current(
    ctx_t const *ctx, char const *msg, parser_t *parser)
{
    parser_error_at(ctx, &parser->cur, msg, parser);
}

static void parser_error(
    ctx_t const *ctx, char const *msg, parser_t *parser)
{
    parser_error_at(ctx, &parser->prev, msg, parser);
}

static void parser_advance(ctx_t const *ctx, parser_t *parser)
{
    parser->prev = parser->cur;
    for (;;) {
        parser->cur = scan_token(ctx, parser->scanner);
        if (parser->cur.type != e_tt_error)
            break;

        parser_error_at_current(ctx, parser->cur.start, parser);
    }
}

static b32 parser_match(ctx_t const *ctx, token_type_t tt, parser_t *parser)
{
    if (parser->cur.type == tt) {
        parser_advance(ctx, parser);
        return true;
    }
    
    return false;
}

static b32 parser_check(ctx_t const *ctx, token_type_t tt, parser_t *parser)
{
    (void)ctx;
    return parser->cur.type == tt;
}

static void parser_consume(
    ctx_t const *ctx, token_type_t tt, char const *err_msg, parser_t *parser)
{
    if (parser->cur.type == tt) {
        parser_advance(ctx, parser);
        return;
    }

    parser_error_at_current(ctx, err_msg, parser);
}

static void parser_sync(ctx_t const *ctx, parser_t *parser)
{
    parser->panic_mode = false;

    while (parser->cur.type != e_tt_eof) {
        if (parser->prev.type == e_tt_semicolon)
            return;

        switch (parser->cur.type) {
        case e_tt_class:
        case e_tt_fun:
        case e_tt_var:
        case e_tt_for:
        case e_tt_if:
        case e_tt_while:
        case e_tt_print:
        case e_tt_return:
            return;

        default:
            break;
        }

        parser_advance(ctx, parser);
    }
}

typedef struct {
    int depth;
} lvar_semdata_t;

typedef struct {
    uint cnt, cap;
    lvar_semdata_t *datas;
} lvar_semdata_array_t;

static inline lvar_semdata_array_t make_lvar_semdata_array(ctx_t const *ctx)
{
    (void)ctx;
    return (lvar_semdata_array_t){0};
}

static inline void write_lvar_semdata_array(
    ctx_t const *ctx, lvar_semdata_array_t *arr, lvar_semdata_t val)
{
    if (arr->cap < arr->cnt + 1) {
        uint old_cap = arr->cap;
        arr->cap = GROW_CAP(old_cap);
        arr->datas = GROW_ARRAY(lvar_semdata_t, arr->datas, old_cap, arr->cap);
    }

    arr->datas[arr->cnt++] = val;
}

static inline void free_lvar_semdata_array(
    ctx_t const *ctx, lvar_semdata_array_t *arr)
{
    if (!arr || !arr->datas)
        return;
    FREE_ARRAY(lvar_semdata_t, arr->datas, arr->cap);
    *arr = make_lvar_semdata_array(ctx);
}

typedef struct {
    parser_t *parser;
    chunk_t *compiling_chunk;
    table_t lvar_map;
    lvar_semdata_array_t lvars;
    int current_scope_depth;
} compiler_t;

static void emit_byte(ctx_t const *ctx, u8 byte, compiler_t *compiler)
{
    write_chunk(
        ctx, compiler->compiling_chunk, byte,
        (uint)compiler->parser->prev.line);
}

static void emit_constant(ctx_t const *ctx, value_t val, compiler_t *compiler)
{
    write_constant(
        ctx, compiler->compiling_chunk,
        e_op_constant, e_op_constant_long, val,
        (uint)compiler->parser->prev.line);
}

static void emit_id_ref(
    ctx_t const *ctx,
    uint id, opcode_t op, opcode_t long_op,
    compiler_t *compiler)
{
    write_id_ref(
        ctx, compiler->compiling_chunk,
        op, long_op, id, (uint)compiler->parser->prev.line);
}

static void emit_return(ctx_t const *ctx, compiler_t *compiler)
{
    emit_byte(ctx, e_op_return, compiler);
}

static uint register_gvar_name(ctx_t const *ctx, vm_t *vm, token_t name)
{
    obj_string_t *str =
        (obj_string_t *)add_string_ref(ctx, vm, name.start, (uint)(name.len));
    value_t id;
    if (!table_get(ctx, &vm->gvar_map, str, &id)) {
        write_value_array(ctx, &vm->gvars, VACANT_VAL_NAMED((obj_t *)str));
        id = NUMBER_VAL((f64)(vm->gvars.cnt - 1));
        table_set(ctx, &vm->gvar_map, str, id);
    }

    return (uint)AS_NUMBER(id);
}

static void begin_compiler(ctx_t const *ctx, compiler_t *compiler)
{
    compiler->lvar_map = make_table(ctx);
    compiler->lvars = make_lvar_semdata_array(ctx);
    compiler->current_scope_depth = 0;
}

static void end_compiler(ctx_t const *ctx, compiler_t *compiler)
{
    emit_return(ctx, compiler);
#if DEBUG_PRINT_CODE
    if (!compiler->parser->had_error)
        DISASSEMBLE_CHUNK(compiler->compiling_chunk, "code");
#endif
}

static void begin_scope(ctx_t const *ctx, compiler_t *compiler)
{
    (void)ctx;
    ++compiler->current_scope_depth;
}

static void end_scope(ctx_t const *ctx, compiler_t *compiler)
{
    (void)ctx;
    --compiler->current_scope_depth;
}

typedef enum {
    e_prec_none,
    e_prec_assignment, // =
    e_prec_or,         // or
    e_prec_and,        // and
    e_prec_eql,        // == !=
    e_prec_cmp,        // < > <= >=
    e_prec_term,       // + -
    e_prec_factor,     // * /
    e_prec_unary,      // ! -
    e_prec_call,       // . ()
    e_prec_primary
} precedence_t;

typedef void (*parse_fn_t)(ctx_t const *, compiler_t *, vm_t *, b32);

typedef struct {
    parse_fn_t prefix;
    parse_fn_t infix;
    precedence_t precedence;
} parse_rule_t;

static void compile_expression(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm);

static void compile_number(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_string(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_literal(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_variable(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_grouping(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_unary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_binary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);

// @TODO: ternary ?: once we have 1) bools 2) branches for short circtuiting

parse_rule_t const c_parse_rules[] = {
    [e_tt_left_paren]    = {&compile_grouping, NULL,            e_prec_none  },
    [e_tt_right_paren]   = {NULL,              NULL,            e_prec_none  },
    [e_tt_left_brace]    = {NULL,              NULL,            e_prec_none  }, 
    [e_tt_right_brace]   = {NULL,              NULL,            e_prec_none  },
    [e_tt_comma]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_dot]           = {NULL,              NULL,            e_prec_none  },
    [e_tt_minus]         = {&compile_unary,    &compile_binary, e_prec_term  },
    [e_tt_plus]          = {NULL,              &compile_binary, e_prec_term  },
    [e_tt_semicolon]     = {NULL,              NULL,            e_prec_none  },
    [e_tt_slash]         = {NULL,              &compile_binary, e_prec_factor},
    [e_tt_star]          = {NULL,              &compile_binary, e_prec_factor},
    [e_tt_bang]          = {&compile_unary,    NULL,            e_prec_none  },
    [e_tt_bang_equal]    = {NULL,              &compile_binary, e_prec_eql   },
    [e_tt_equal]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_equal_equal]   = {NULL,              &compile_binary, e_prec_eql   },
    [e_tt_greater]       = {NULL,              &compile_binary, e_prec_cmp   },
    [e_tt_greater_equal] = {NULL,              &compile_binary, e_prec_cmp   },
    [e_tt_less]          = {NULL,              &compile_binary, e_prec_cmp   },
    [e_tt_less_equal]    = {NULL,              &compile_binary, e_prec_cmp   },
    [e_tt_identifier]    = {&compile_variable, NULL,            e_prec_none  },
    [e_tt_string]        = {&compile_string,   NULL,            e_prec_none  },
    [e_tt_number]        = {&compile_number,   NULL,            e_prec_none  },
    [e_tt_and]           = {NULL,              NULL,            e_prec_none  },
    [e_tt_class]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_else]          = {NULL,              NULL,            e_prec_none  },
    [e_tt_false]         = {&compile_literal,  NULL,            e_prec_none  },
    [e_tt_for]           = {NULL,              NULL,            e_prec_none  },
    [e_tt_fun]           = {NULL,              NULL,            e_prec_none  },
    [e_tt_if]            = {NULL,              NULL,            e_prec_none  },
    [e_tt_nil]           = {&compile_literal,  NULL,            e_prec_none  },
    [e_tt_or]            = {NULL,              NULL,            e_prec_none  },
    [e_tt_print]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_return]        = {NULL,              NULL,            e_prec_none  },
    [e_tt_super]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_this]          = {NULL,              NULL,            e_prec_none  },
    [e_tt_true]          = {&compile_literal,  NULL,            e_prec_none  },
    [e_tt_var]           = {NULL,              NULL,            e_prec_none  },
    [e_tt_while]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_error]         = {NULL,              NULL,            e_prec_none  },
    [e_tt_eof]           = {NULL,              NULL,            e_prec_none  },
};

static parse_rule_t const *get_rule(ctx_t const *ctx, token_type_t tt)
{
    (void)ctx;
    return &c_parse_rules[tt];
}

static void compile_precedence(
    ctx_t const *ctx, precedence_t prec, compiler_t *compiler, vm_t *vm)
{
    parser_advance(ctx, compiler->parser);
    parse_fn_t prefix_rule = get_rule(ctx, compiler->parser->prev.type)->prefix;
    if (!prefix_rule) {
        parser_error(ctx, "Expected expression.", compiler->parser);
        return;
    }

    b32 can_assign = prec <= e_prec_assignment;
    prefix_rule(ctx, compiler, vm, can_assign);

    while (prec < get_rule(ctx, compiler->parser->cur.type)->precedence) {
        parser_advance(ctx, compiler->parser);
        parse_fn_t infix_rule =
            get_rule(ctx, compiler->parser->prev.type)->infix;
        infix_rule(ctx, compiler, vm, can_assign);
    }

    if (can_assign && parser_match(ctx, e_tt_equal, compiler->parser))
        parser_error(ctx, "Invalid assignment target.", compiler->parser);
}

static void compile_number(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)vm;
    (void)can_assign;
    char const *str = compiler->parser->prev.start;
    f64 num = 0.0;
    while (IS_DIGIT(*str))
        num = num * 10.0 + (f64)(*str++ - '0');
    if (*str++ == '.') {
        f64 frac = 0.0, mul = 0.1;
        while (IS_DIGIT(*str)) {
            frac += mul * (f64)(*str++ - '0');
            mul *= 0.1;
        }
        num += frac;
    }

    emit_constant(ctx, NUMBER_VAL(num), compiler);
}

static void compile_string(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    obj_t *str = add_string_ref(
        ctx, vm, compiler->parser->prev.start + 1,
        (uint)(compiler->parser->prev.len - 2));
    emit_constant(ctx, OBJ_VAL(str), compiler);
}

static void compile_literal(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)vm;
    (void)can_assign;
    switch (compiler->parser->prev.type) {
    case e_tt_false:
        emit_byte(ctx, e_op_false, compiler);
        break;
    case e_tt_nil:
        emit_byte(ctx, e_op_nil, compiler);
        break;
    case e_tt_true:
        emit_byte(ctx, e_op_true, compiler);
        break;
    default:
        return;
    }
}

static void compile_named_variable(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm,
    token_t name, b32 can_assign)
{
    uint vid = register_gvar_name(ctx, vm, name);

    if (can_assign && parser_match(ctx, e_tt_equal, compiler->parser)) {
        compile_expression(ctx, compiler, vm);
        emit_id_ref(ctx, vid, e_op_set_glob, e_op_set_glob_long, compiler);
    } else {
        emit_id_ref(ctx, vid, e_op_get_glob, e_op_get_glob_long, compiler);
    }
}

static void compile_variable(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    compile_named_variable(
        ctx, compiler, vm, compiler->parser->prev, can_assign);
}

static void compile_expression(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    compile_precedence(ctx, e_prec_assignment, compiler, vm);
}

static void compile_grouping(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    compile_expression(ctx, compiler, vm);
    parser_consume(
        ctx, e_tt_right_paren, "Expect ')' after expression.",
        compiler->parser);
}

static void compile_unary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    token_type_t tt = compiler->parser->prev.type;

    compile_precedence(ctx, e_prec_unary, compiler, vm);

    switch (tt) {
    case e_tt_bang:
        emit_byte(ctx, e_op_not, compiler);
        break;
    case e_tt_minus:
        emit_byte(ctx, e_op_negate, compiler);
        break;
    default:
        ASSERT(0);
        break;
    }
}

static void compile_binary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    token_type_t tt = compiler->parser->prev.type;
    parse_rule_t const *rule = get_rule(ctx, tt);
    compile_precedence(ctx, (precedence_t)(rule->precedence + 1), compiler, vm);

    switch (tt) {
    case e_tt_bang_equal:
        emit_byte(ctx, e_op_neq, compiler);
        break;
    case e_tt_equal_equal:
        emit_byte(ctx, e_op_eq, compiler);
        break;
    case e_tt_greater:
        emit_byte(ctx, e_op_gt, compiler);
        break;
    case e_tt_greater_equal:
        emit_byte(ctx, e_op_ge, compiler);
        break;
    case e_tt_less:
        emit_byte(ctx, e_op_lt, compiler);
        break;
    case e_tt_less_equal:
        emit_byte(ctx, e_op_le, compiler);
        break;
    case e_tt_plus:
        emit_byte(ctx, e_op_add, compiler);
        break;
    case e_tt_minus:
        emit_byte(ctx, e_op_subtract, compiler);
        break;
    case e_tt_star:
        emit_byte(ctx, e_op_multiply, compiler);
        break;
    case e_tt_slash:
        emit_byte(ctx, e_op_divide, compiler);
        break;
    default:
        ASSERT(0);
        break;
    }
}

static void compile_decl(ctx_t const *ctx, compiler_t *compiler, vm_t *vm);

static uint compile_new_variable(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, char const *err)
{
    parser_consume(ctx, e_tt_identifier, err, compiler->parser);
    return register_gvar_name(ctx, vm, compiler->parser->prev);
}

static void compile_expr_stmt(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    compile_expression(ctx, compiler, vm);
    parser_consume(
        ctx, e_tt_semicolon, "Expected ';' after expression.",
        compiler->parser);
    emit_byte(ctx, e_op_pop, compiler);
}

static void compile_block(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    while (
        !parser_check(ctx, e_tt_right_brace, compiler->parser) &&
        !parser_check(ctx, e_tt_eof, compiler->parser))
    {
        compile_decl(ctx, compiler, vm);
    }

    parser_consume(
        ctx, e_tt_right_brace, "Expected '}' after block.", compiler->parser);
}

static void compile_print(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    compile_expression(ctx, compiler, vm);
    parser_consume(
        ctx, e_tt_semicolon, "Expected ';' after value.", compiler->parser);
    emit_byte(ctx, e_op_print, compiler);
}

static void compile_stmt(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    if (parser_match(ctx, e_tt_print, compiler->parser)) {
        compile_print(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_left_brace, compiler->parser)) {
        begin_scope(ctx, compiler);
        compile_block(ctx, compiler, vm);
        end_scope(ctx, compiler);
    } else {
        compile_expr_stmt(ctx, compiler, vm);
    }
}

static void compile_var_decl(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    uint glob = compile_new_variable(
        ctx, compiler, vm, "Expected variable name.");

    if (parser_match(ctx, e_tt_equal, compiler->parser))
        compile_expression(ctx, compiler, vm);
    else
        emit_byte(ctx, e_op_nil, compiler);

    parser_consume(
        ctx, e_tt_semicolon, "Expected ';' after var decl.", compiler->parser);

    emit_id_ref(ctx, glob, e_op_define_glob, e_op_define_glob_long, compiler);
}

static void compile_decl(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    if (parser_match(ctx, e_tt_var, compiler->parser))
        compile_var_decl(ctx, compiler, vm);
    else
        compile_stmt(ctx, compiler, vm);

    if (compiler->parser->panic_mode)
        parser_sync(ctx, compiler->parser);
}

static b32 compile(ctx_t const *ctx, string_t source, chunk_t *chunk, vm_t *vm)
{
    scanner_t scanner = {source.p, source.p, source.p + source.len, 1}; 
    parser_t parser = {0};
    compiler_t compiler = {0};

    parser.scanner = &scanner;
    compiler.parser = &parser;
    compiler.compiling_chunk = chunk;

    begin_compiler(ctx, &compiler);

    parser_advance(ctx, &parser);
    while (!parser_match(ctx, e_tt_eof, &parser))
        compile_decl(ctx, &compiler, vm);

    end_compiler(ctx, &compiler);

    return !parser.had_error;
}

static interp_result_t interpret(ctx_t const *ctx, vm_t *vm, string_t source)
{
    chunk_t chunk = make_chunk(ctx);

    if (!compile(ctx, source, &chunk, vm)) {
        free_chunk(ctx, &chunk);
        return e_interp_compile_err;
    }

    vm->chunk = &chunk;
    vm->ip = chunk.code;

    interp_result_t res = run_chunk(ctx, vm);

    free_chunk(ctx, &chunk);
    return res;
}

static void repl(ctx_t const *ctx, vm_t *vm)
{
    char line[1024];
    usize buffered_chars = 0;
    b32 eof = false;

    struct source_lines {
        string_t line;
        struct source_lines *next;
    } *lines_storage = NULL;

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
            if (buffered_chars == 0)
                break;
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

        struct source_lines *node = ALLOCATE(struct source_lines, 1);
        node->line = (string_t){ALLOCATE(char, line_break + 1), line_break};
        node->next = lines_storage;
        lines_storage = node;

        memcpy(node->line.p, line, line_break + 1);

        interpret(ctx, vm, node->line);

        buffered_chars -= line_break + 1;
        char *src = &line[line_break + 1], *dst = line;
        for (usize i = 0; i < buffered_chars; ++i)
            *dst++ = *src++;
    }

    while (lines_storage) {
        struct source_lines *tmp = lines_storage;
        lines_storage = lines_storage->next;
        FREE(tmp->line.p);
        FREE(tmp);
    }
}

static void run_file(ctx_t const *ctx, vm_t *vm, char const *fname)
{
    io_file_t f = io_read_open_file(ctx, fname);
    if (!io_file_is_valid(ctx, &f)) {
        LOGF("Failed to open script '%s'.", fname);
        os_exit(ctx, 65);
    }

    char *script = ALLOCATE(char, f.len + 1);
    usize chars_read = (usize)io_read(ctx, &f.ioh, (u8 *)script, f.len);
    VERIFY(chars_read == f.len, "Failed to read script.");
    script[f.len] = '\0';

    string_t source = {script, f.len};
    interp_result_t res = interpret(ctx, vm, source);

    FREE(script);

    if (res == e_interp_compile_err)
        os_exit(ctx, 70);
    else if (res == e_interp_runtime_err)
        os_exit(ctx, 75);
}

static int lox_main(ctx_t const *ctx)
{
    (void)table_add_all;
    (void)table_delete;

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

    free_vm(ctx, &vm);

    return 0;
}

