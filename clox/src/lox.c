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

typedef struct value value_t;

typedef struct {
    uint cnt, cap;
    value_t *values;
} value_array_t;

typedef struct {
    uint line;
    uint instr_range_end;
} line_info_entry_t;

typedef struct {
    uint cnt, cap;
    line_info_entry_t *entries;
} line_info_t;

typedef struct {
    uint cnt, cap;
    u8 *code;
    value_array_t constants;
    line_info_t lines;
    uint current_line;
} chunk_t;

typedef enum {
    e_vt_nil,
    e_vt_bool,
    e_vt_number,
    e_vt_obj,

    e_vt_vacant = -1,
} value_type_t;

typedef struct obj obj_t;
typedef struct obj_string obj_string_t;
typedef struct obj_function obj_function_t;
typedef struct obj_native obj_native_t;
typedef struct obj_closure obj_closure_t;
typedef struct obj_upvalue obj_upvalue_t;

typedef struct value {
    value_type_t type;
    union {
        b32 boolean;
        f64 number;
        obj_t *obj;
    } as;
} value_t;

#define IS_BOOL(val_)    ((val_).type == e_vt_bool)
#define IS_NIL(val_)     ((val_).type == e_vt_nil)
#define IS_NUMBER(val_)  ((val_).type == e_vt_number)
#define IS_OBJ(val_)     ((val_).type == e_vt_obj)
#define IS_VACANT(val_)  ((val_).type == e_vt_vacant)

#define AS_BOOL(val_)    ((val_).as.boolean)
#define AS_NUMBER(val_)  ((val_).as.number)
#define AS_OBJ(val_)     ((val_).as.obj)

#define BOOL_VAL(val_)   ((value_t){e_vt_bool,   {.boolean = (val_)        }})
#define NIL_VAL          ((value_t){e_vt_nil,    {.number  = 0.0           }})
#define NUMBER_VAL(val_) ((value_t){e_vt_number, {.number  = (val_)        }})
#define OBJ_VAL(val_)    ((value_t){e_vt_obj,    {.obj     = (obj_t*)(val_)}})
#define VACANT_VAL       ((value_t){e_vt_vacant, {.obj     = NULL          }})

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

// Ugly AF forwards for native functions
typedef struct vm vm_t;
typedef struct native_ctx native_ctx_t;
typedef value_t (*native_fn_t)(int, value_t *, native_ctx_t *);
static void define_native(
    ctx_t const *ctx, vm_t *vm, string_t name, native_fn_t fn, int arity);
static void native_runtime_error(native_ctx_t *nctx, char const *fmt, ...);

typedef enum {
    e_ot_string,
    e_ot_function,
    e_ot_native,
    e_ot_closure,
    e_ot_upvalue,
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

typedef enum {
    e_ft_function,
    e_ft_script
} obj_function_type_t;

typedef struct obj_function {
    obj_t obj;
    int arity;
    uint upvalue_cnt;
    chunk_t chunk;
    obj_string_t *name;
} obj_function_t;

typedef struct obj_native {
    obj_t obj;
    int arity;
    native_fn_t fn;
} obj_native_t;

typedef struct obj_closure {
    obj_t obj;
    obj_function_t *fn;
    obj_upvalue_t **upvalues;
    uint upvalue_cnt;
} obj_closure_t;

typedef struct obj_upvalue {
    obj_t obj;
    value_t *location;
    uint stack_location;
    value_t closed;
    struct obj_upvalue *next;
} obj_upvalue_t;

static inline b32 is_obj_type(ctx_t const *ctx, value_t val, obj_type_t ot)
{
    (void)ctx;
    return IS_OBJ(val) && AS_OBJ(val)->type == ot;
}

#define OSTR_TO_STR(ostr_) ((string_t){(ostr_)->p, (u64)(ostr_)->len})

static inline string_t obj_as_string(ctx_t const *ctx, value_t val)
{
    (void)ctx;
    return OSTR_TO_STR((obj_string_t *)AS_OBJ(val));
}

#define OBJ_TYPE(value_)      (AS_OBJ(value_)->type)

#define IS_STRING(value_)     (is_obj_type(ctx, (value_), e_ot_string))
#define IS_FUNCTION(value_)   (is_obj_type(ctx, (value_), e_ot_function))
#define IS_NATIVE(value_)     (is_obj_type(ctx, (value_), e_ot_native))
#define IS_CLOSURE(value_)    (is_obj_type(ctx, (value_), e_ot_closure))

#define AS_STRING_OBJ(value_) ((obj_string_t *)AS_OBJ(value_))
#define AS_STRING(value_)     (obj_as_string(ctx, (value_)))
#define AS_CSTRING(value_)    (((obj_string_t *)AS_OBJ(value_))->p)
#define AS_FUNCTION(value_)   ((obj_function_t *)AS_OBJ(value_))
#define AS_NATIVE_DT(value_)  ((obj_native_t *)AS_OBJ(value_))
#define AS_NATIVE(value_)     (((obj_native_t *)AS_OBJ(value_))->fn)
#define AS_CLOSURE(value_)    ((obj_closure_t *)AS_OBJ(value_))

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
    e_op_popn,
    e_op_popn_long,
    e_op_define_glob,
    e_op_define_glob_long,
    e_op_get_glob,
    e_op_get_glob_long,
    e_op_set_glob,
    e_op_set_glob_long,
    e_op_get_loc,
    e_op_get_loc_long,
    e_op_set_loc,
    e_op_set_loc_long,
    e_op_get_upvalue,
    e_op_get_upvalue_long,
    e_op_set_upvalue,
    e_op_set_upvalue_long,
    e_op_closure,
    e_op_closure_long,
    e_op_close_upvalue,
    e_op_eq,
    e_op_eq_preserve_lhs,
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
    e_op_jump,
    e_op_jump_if_false,
    e_op_jump_if_true,
    e_op_loop,
    e_op_call,
    e_op_call_long,
    e_op_return,
} opcode_t;

typedef enum {
    f_uv_local = 1,
    f_uv_long = 1 << 1
} upvalue_flags_t;

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
    case e_ot_function:
        if (AS_FUNCTION(val)->name)
            fmt_print(ctx, hnd, "<fn %S>", OSTR_TO_STR(AS_FUNCTION(val)->name));
        else
            fmt_print(ctx, hnd, "<script>");
        break;
    case e_ot_closure:
        fmt_print(ctx, hnd, "<closure ");
        print_obj(ctx, hnd, OBJ_VAL(AS_CLOSURE(val)->fn));
        fmt_print(ctx, hnd, ">");
        break;
    case e_ot_native:
        fmt_print(ctx, hnd, "<native fn>");
        break;
    case e_ot_upvalue:
        fmt_print(ctx, hnd, "<upvalue>");
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

static uint disasm_id_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at)
{
    u8 gvar = chunk->code[at + 1];
    LOGF("%s %u", name, gvar);
    return at + 2;
}

static uint disasm_long_id_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at)
{
    uint gvar =
        chunk->code[at + 1]
        | ((uint)chunk->code[at + 2] << 8)
        | ((uint)chunk->code[at + 3] << 16);
    LOGF("%s %u", name, gvar);
    return at + 4;
}

static uint disasm_short_jump_instruction(
    ctx_t const *ctx, char const *name, chunk_t const *chunk, uint at, int dir)
{
    int jump = chunk->code[at + 1] | ((int)chunk->code[at + 2] << 8);
    LOGF("%s %d -> %d", name, at, (int)at + 3 + jump * dir);
    return at + 3;
}

static uint disasm_upvalue_list(
    ctx_t const *ctx, chunk_t const *chunk, uint at, uint count)
{
    for (uint i = 0; i < count; ++i) {
        u8 flags = chunk->code[at++];
        uint index = chunk->code[at++];
        if (flags & f_uv_long) {
            index |= (uint)chunk->code[at++] << 8;
            index |= (uint)chunk->code[at++] << 16;
        }
        {
            isize chars = LOGF_NONL("%u:", at);
            for (isize j = chars; j < 8; ++j)
                LOG_NONL(" ");
            LOG_NONL("|");
            for (isize j = 0; j < 7; ++j)
                LOG_NONL(" ");
        }
        LOGF("%s %u", (flags & f_uv_local) ? "local" : "upvalue", index);
    }
    return at;
}

static obj_function_t *disasm_peek_function(
    ctx_t const *ctx, chunk_t const *chunk, uint at)
{
    (void)ctx;
    return AS_FUNCTION(chunk->constants.values[chunk->code[at + 1]]);
}

static obj_function_t *disasm_peek_long_function(
    ctx_t const *ctx, chunk_t const *chunk, uint at)
{
    (void)ctx;
    uint constant =
        chunk->code[at + 1]
        | ((uint)chunk->code[at + 2] << 8)
        | ((uint)chunk->code[at + 3] << 16);
    return AS_FUNCTION(chunk->constants.values[constant]);
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
    case e_op_popn:
        return disasm_id_instruction(
            ctx, "OP_POPN", chunk, at);
    case e_op_popn_long:
        return disasm_long_id_instruction(
            ctx, "OP_POPN", chunk, at);
    case e_op_define_glob:
        return disasm_id_instruction(
            ctx, "OP_DEFINE_GLOB", chunk, at);
    case e_op_define_glob_long:
        return disasm_long_id_instruction(
            ctx, "OP_DEFINE_GLOB_LONG", chunk, at);
    case e_op_get_glob:
        return disasm_id_instruction(
            ctx, "OP_GET_GLOB", chunk, at);
    case e_op_get_glob_long:
        return disasm_long_id_instruction(
            ctx, "OP_GET_GLOB_LONG", chunk, at);
    case e_op_set_glob:
        return disasm_id_instruction(
            ctx, "OP_SET_GLOB", chunk, at);
    case e_op_set_glob_long:
        return disasm_long_id_instruction(
            ctx, "OP_SET_GLOB_LONG", chunk, at);
    case e_op_get_loc:
        return disasm_id_instruction(
            ctx, "OP_GET_LOC", chunk, at);
    case e_op_get_loc_long:
        return disasm_long_id_instruction(
            ctx, "OP_GET_LOC_LONG", chunk, at);
    case e_op_set_loc:
        return disasm_id_instruction(
            ctx, "OP_SET_LOC", chunk, at);
    case e_op_set_loc_long:
        return disasm_long_id_instruction(
            ctx, "OP_SET_LOC_LONG", chunk, at);
    case e_op_get_upvalue:
        return disasm_id_instruction(
            ctx, "OP_GET_UPVALUE", chunk, at);
    case e_op_get_upvalue_long:
        return disasm_long_id_instruction(
            ctx, "OP_GET_UPVALUE_LONG", chunk, at);
    case e_op_set_upvalue:
        return disasm_id_instruction(
            ctx, "OP_SET_UPVALUE", chunk, at);
    case e_op_set_upvalue_long:
        return disasm_long_id_instruction(
            ctx, "OP_SET_UPVALUE_LONG", chunk, at);
    case e_op_closure:
        return disasm_upvalue_list(ctx, chunk, 
            disasm_constant_instruction(ctx, "OP_CLOSURE", chunk, at),
            disasm_peek_function(ctx, chunk, at)->upvalue_cnt);
    case e_op_closure_long:
        return disasm_upvalue_list(ctx, chunk, 
            disasm_long_constant_instruction(ctx, "OP_CLOSURE_LONG", chunk, at),
            disasm_peek_long_function(ctx, chunk, at)->upvalue_cnt);
    case e_op_close_upvalue:
        return disasm_simple_instruction(
            ctx, "OP_CLOSE_UPVALUE", at);
    case e_op_eq:
        return disasm_simple_instruction(
            ctx, "OP_EQ", at);
    case e_op_eq_preserve_lhs:
        return disasm_simple_instruction(
            ctx, "OP_EQ_PRESERVE_LHS", at);
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
    case e_op_jump:
        return disasm_short_jump_instruction(
            ctx, "OP_JUMP", chunk, at, 1);
    case e_op_jump_if_false:
        return disasm_short_jump_instruction(
            ctx, "OP_JUMP_IF_FALSE", chunk, at, 1);
    case e_op_jump_if_true:
        return disasm_short_jump_instruction(
            ctx, "OP_JUMP_IF_TRUE", chunk, at, 1);
    case e_op_loop:
        return disasm_short_jump_instruction(
            ctx, "OP_LOOP", chunk, at, -1);
    case e_op_call:
        return disasm_id_instruction(
            ctx, "OP_CALL", chunk, at);
    case e_op_call_long:
        return disasm_long_id_instruction(
            ctx, "OP_CALL_LONG", chunk, at);
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
    ctx_t const *ctx, chunk_t const *chunk, string_t name)
{
    LOGF("== %S ==", name);
    for (uint off = 0; off < chunk->cnt;)
        off = disasm_instruction(ctx, chunk, off); 
    LOG("");
}

#define DISASSEMBLE_CHUNK(chunk_, name_) disasm_chunk(ctx, (chunk_), (name_))

#endif

#endif

#ifndef DISASSEMBLE_CHUNK
#define DISASSEMBLE_CHUNK(...)
#endif

#define VM_STACK_SEGMENT_SIZE 256
#define VM_MAX_CALL_FRAMES 65536

typedef struct vm_stack_segment {
    value_t values[VM_STACK_SEGMENT_SIZE];
    struct vm_stack_segment *prev;
    struct vm_stack_segment *next;
} vm_stack_segment_t;

typedef struct call_frame {
    obj_closure_t *c;
    obj_function_t *f;
    u8 *ip;
    uint bp;
    struct call_frame *next;
} call_frame_t;

typedef struct native_ctx {
    ctx_t const *ctx;
    vm_t *vm;
} native_ctx_t;

static value_t clock_native(int arg_count, value_t *args, native_ctx_t *nctx)
{
    (void)args;
    ctx_t const *ctx = nctx->ctx;
    ASSERT(arg_count == 0);
    return NUMBER_VAL(os_time(ctx) / 1000.0);
}

static value_t readch_native(int arg_count, value_t *args, native_ctx_t *nctx)
{
    (void)args;
    ctx_t const *ctx = nctx->ctx;
    ASSERT(arg_count == 0);
    char res = '\0';
    isize br = io_read(ctx, &ctx->os->hstdin, (u8 *)&res, 1);
    if (br == 0) {
        return NIL_VAL; // eof
    } else if (br != 1) {
        native_runtime_error(nctx, "readch failed.");
        return VACANT_VAL;
    }
    return NUMBER_VAL((f64)res);
}

typedef struct vm {
    vm_stack_segment_t stack;
    vm_stack_segment_t *cur_stack_seg; 
    uint stack_seg_count;
    value_t *stack_head;
    call_frame_t *frame;
    uint frame_count;
    table_t strings;
    table_t gvar_map;
    value_array_t gvars;
    obj_upvalue_t *open_upvalues;
    obj_t *objects;
    native_ctx_t nctx;
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

#define ALLOCATE_OBJ(type_, ot_, vm_) \
    ((type_ *)allocate_obj(ctx, (vm_), sizeof(type_), (ot_)))

static inline obj_t *allocate_string(ctx_t const *ctx, vm_t *vm, uint len)
{
    obj_t *obj = allocate_obj(
        ctx, vm, sizeof(obj_string_t) + len + 1, e_ot_string);
    ((obj_string_t *)obj)->type = e_st_allocated;
    ((obj_string_t *)obj)->p = (char *)(obj) + sizeof(obj_string_t);
    ((obj_string_t *)obj)->len = len;
    return obj;
}

#define ALLOCATE_STR(len_, vm_) \
    ((obj_string_t *)allocate_string(ctx, (vm_), (len_)))

static inline obj_function_t *allocate_function(ctx_t const *ctx, vm_t *vm)
{
    obj_function_t *func = ALLOCATE_OBJ(obj_function_t, e_ot_function, vm);
    func->arity = 0;
    func->upvalue_cnt = 0;
    func->name = NULL;
    func->chunk = make_chunk(ctx);
    return func;
}

static inline obj_closure_t *allocate_closure(
    ctx_t const *ctx, vm_t *vm, uint upvalue_cnt)
{
    obj_upvalue_t **uvs = ALLOCATE(obj_upvalue_t *, upvalue_cnt);
    memset(uvs, 0, sizeof(*uvs) * upvalue_cnt);
    obj_closure_t *cl = ALLOCATE_OBJ(obj_closure_t, e_ot_closure, vm);
    cl->fn = NULL;
    cl->upvalues = uvs;
    cl->upvalue_cnt = upvalue_cnt;
    return cl;
}

static inline obj_upvalue_t *allocate_upvalue(ctx_t const *ctx, vm_t *vm)
{
    obj_upvalue_t *uv = ALLOCATE_OBJ(obj_upvalue_t, e_ot_upvalue, vm);
    uv->location = NULL;
    uv->stack_location = (uint)(-1);
    uv->closed = NIL_VAL;
    uv->next = NULL;
    return uv;
}

static inline void free_object(ctx_t const *ctx, obj_t *obj)
{
    switch (obj->type) {
    case e_ot_function: {
        obj_function_t *f = (obj_function_t *)obj;
        free_chunk(ctx, &f->chunk);
        FREE(f);
    } break;
    case e_ot_closure: {
        obj_closure_t *c = (obj_closure_t *)obj;
        FREE_ARRAY(obj_upvalue_t *, c->upvalues, c->upvalue_cnt);
        FREE(c);
    } break;
    default:
        FREE(obj);
        break;
    }
}

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
    memset(&vm->stack, 0, sizeof(vm->stack));
    vm->cur_stack_seg = &vm->stack;
    vm->stack_head = vm->stack.values;
    vm->stack_seg_count = 1;
    vm->frame = NULL;
    vm->frame_count = 0;
}

static uint stack_size(ctx_t const *ctx, vm_t *vm)
{
    (void)ctx;
    return
        (vm->stack_seg_count - 1) * VM_STACK_SEGMENT_SIZE +
        (uint)(vm->stack_head - vm->cur_stack_seg->values);
}

static void vruntime_error(
    ctx_t const *ctx, vm_t *vm, char const *fmt, VA_LIST args)
{
    fmt_vprint(ctx, &ctx->os->hstderr, fmt, args);

    for (call_frame_t *frame = vm->frame; frame; frame = frame->next) {
        obj_function_t *f = frame->f;
        chunk_t *chunk = &f->chunk;
        u8 *ip = frame->ip;

        uint instr = (uint)(ip - chunk->code - 1);
        uint line = 0;
        for (usize i = 0; i < chunk->lines.cnt; ++i) {
            if (instr < chunk->lines.entries[i].instr_range_end) {
                line = chunk->lines.entries[i].line;
                break;
            }
        }

        LOGF_NONL("\n[line %d] in ", line);
        if (f->name)
            LOGF_NONL("%S()", OSTR_TO_STR(f->name));
        else
            LOG_NONL("script");
    }

    LOG("");
    reset_stack(ctx, vm);
}

static void runtime_error(ctx_t const *ctx, vm_t *vm, char const *fmt, ...)
{
    VA_LIST args;
    VA_START(args, fmt);
    vruntime_error(ctx, vm, fmt, args);
    VA_END(args);
}

static void native_runtime_error(native_ctx_t *nctx, char const *fmt, ...)
{
    VA_LIST args;
    VA_START(args, fmt);
    vruntime_error(nctx->ctx, nctx->vm, fmt, args);
    VA_END(args);
}

static call_frame_t *push_frame(ctx_t const *ctx, vm_t *vm)
{
    if (vm->frame_count >= VM_MAX_CALL_FRAMES) {
        runtime_error(ctx, vm, "Stack overflow.");
        return NULL;
    }
    call_frame_t *frame = ALLOCATE(call_frame_t, 1);
    if (vm->frame)
        frame->next = vm->frame;
    else
        frame->next = NULL;
    vm->frame = frame;
    ++vm->frame_count;
    return frame;
}

static void pop_frame(ctx_t const *ctx, vm_t *vm)
{
    ASSERT(vm->frame);
    call_frame_t *frame = vm->frame;
    vm->frame = frame->next;
    --vm->frame_count;
    FREE(frame);
}

static void init_vm(ctx_t const *ctx, vm_t *vm)
{
    reset_stack(ctx, vm);
    vm->strings = make_table(ctx);
    vm->gvar_map = make_table(ctx);
    vm->gvars = make_value_array(ctx);
    vm->objects = NULL;
    vm->open_upvalues = NULL;
    vm->nctx.ctx = ctx;
    vm->nctx.vm = vm;
    define_native(ctx, vm, LITSTR("clock"), &clock_native, 0);
    define_native(ctx, vm, LITSTR("readch"), &readch_native, 0);
}

static void free_vm(ctx_t const *ctx, vm_t *vm)
{
    free_table(ctx, &vm->strings);
    free_table(ctx, &vm->gvar_map);
    free_value_array(ctx, &vm->gvars);
    vm_stack_segment_t *seg = vm->stack.next;
    while (seg) {
        vm_stack_segment_t *tmp = seg;
        seg = seg->next;
        FREE(tmp);
    }
    while (vm->frame)
        pop_frame(ctx, vm);

    // @FIXME
    // botched refs in obj list, maybe mem corruption (=> inf loop here).
    // Should be fixed by the point of GC.
#if 0
    obj_t *obj = vm->objects;
    while (obj) {
        obj_t *next = obj->next;
        free_object(ctx, obj);
        obj = next;
    }
#endif
}

#define PUSH_STACK_SEG(vm_)                                             \
    do {                                                                \
        vm_stack_segment_t *new_seg_ = ALLOCATE(vm_stack_segment_t, 1); \
        (vm_)->cur_stack_seg->next = new_seg_;                          \
        new_seg_->prev = (vm_)->cur_stack_seg;                          \
        new_seg_->next = NULL;                                          \
        (vm_)->cur_stack_seg = new_seg_;                                \
        (vm_)->stack_head = new_seg_->values;                           \
        ++vm->stack_seg_count;                                          \
    } while (0)

#define POP_STACK_SEG(vm_)                                        \
    do {                                                          \
        vm_stack_segment_t *old_seg_ = (vm_)->cur_stack_seg;      \
        (vm_)->cur_stack_seg = old_seg_->prev;                    \
        (vm_)->cur_stack_seg->next = NULL;                        \
        FREE(old_seg_);                                           \
        (vm_)->stack_head =                                       \
            (vm_)->cur_stack_seg->values + VM_STACK_SEGMENT_SIZE; \
        --vm->stack_seg_count;                                    \
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

static void popn_stack(ctx_t const *ctx, vm_t *vm, uint n)
{
    uint full_segments_size = (vm->stack_seg_count - 1) * VM_STACK_SEGMENT_SIZE;
    uint head_segment_size = (uint)(vm->stack_head - vm->cur_stack_seg->values);
    ASSERT(n <= full_segments_size + head_segment_size);
    while (n >= head_segment_size && vm->cur_stack_seg->prev) {
        POP_STACK_SEG(vm);
        n -= head_segment_size;
        head_segment_size = VM_STACK_SEGMENT_SIZE;
    }

    vm->stack_head -= n;
}

static value_t *at_stack(ctx_t const *ctx, vm_t *vm, uint at)
{
    uint full_segments_size = (vm->stack_seg_count - 1) * VM_STACK_SEGMENT_SIZE;
    ASSERT(at < full_segments_size +
        (uint)(vm->stack_head - vm->cur_stack_seg->values));
    vm_stack_segment_t *seg = vm->cur_stack_seg;
    while (at < full_segments_size) {
        ASSERT(seg->prev);
        ASSERT(full_segments_size >= VM_STACK_SEGMENT_SIZE);
        full_segments_size -= VM_STACK_SEGMENT_SIZE;
        seg = seg->prev;
    }
    return &seg->values[at - full_segments_size];
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

static inline u16 read_short_ip(ctx_t const *ctx, u8 **ip)
{
    (void)ctx;
    *ip += 2;
    return (u16)(*ip)[-2] | (u16)((u16)(*ip)[-1] << 8);
}

static inline u32 read_long_ip(ctx_t const *ctx, u8 **ip)
{
    (void)ctx;
    *ip += 3;
    return (u32)(*ip)[-3] | ((u32)(*ip)[-2] << 8) | ((u32)(*ip)[-1] << 16);
}

static b32 callf(
    ctx_t const *ctx, obj_function_t *f, int arg_count, vm_t *vm)
{
    if (arg_count != f->arity) {
        runtime_error(
            ctx, vm, "Expected %d arguments, got %d.", f->arity, arg_count);
        return false;
    }

    call_frame_t *frame = push_frame(ctx, vm);
    if (!frame)
        return false;
    frame->c = NULL;
    frame->f = f;
    frame->ip = f->chunk.code;
    frame->bp = stack_size(ctx, vm) - (uint)arg_count - 1;
    return true;
}

static b32 callc(
    ctx_t const *ctx, obj_closure_t *c, int arg_count, vm_t *vm)
{
    obj_function_t *f = c->fn;
    if (arg_count != f->arity) {
        runtime_error(
            ctx, vm, "Expected %d arguments, got %d.", f->arity, arg_count);
        return false;
    }

    call_frame_t *frame = push_frame(ctx, vm);
    if (!frame)
        return false;
    frame->c = c;
    frame->f = f;
    frame->ip = f->chunk.code;
    frame->bp = stack_size(ctx, vm) - (uint)arg_count - 1;
    return true;
}

static b32 call_value(
    ctx_t const *ctx, value_t callee, uint arg_count, vm_t *vm)
{
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
        case e_ot_closure:
            return callc(ctx, AS_CLOSURE(callee), (int)arg_count, vm);
        case e_ot_function:
            return callf(ctx, AS_FUNCTION(callee), (int)arg_count, vm);
        case e_ot_native: {
            native_fn_t native = AS_NATIVE(callee);
            uint expected_arity = (uint)AS_NATIVE_DT(callee)->arity;
            if (arg_count != expected_arity) {
                runtime_error(
                    ctx, vm, "Expected %d arguments, got %d.",
                    expected_arity, arg_count);
                return false;
            }
            value_t *args = arg_count ? ALLOCATE(value_t, arg_count) : NULL;
            for (uint d = arg_count - 1, s = 0; s < arg_count; --d, ++s)
                args[d] = peek_stack(ctx, vm, (int)s);
            value_t res = native((int)arg_count, args, &vm->nctx);
            if (res.type == e_vt_vacant)
                return false;
            popn_stack(ctx, vm, arg_count + 1);
            push_stack(ctx, vm, res);
            if (args)
                FREE(args);
            return true;
        }
        default:
            break;
        }
    }

    runtime_error(ctx, vm, "Can only call functions and classes.");
    return false;
}

static obj_upvalue_t *capture_upvalue(ctx_t const *ctx, vm_t *vm, uint at)
{
    obj_upvalue_t *prev = NULL;
    obj_upvalue_t *upvalue = vm->open_upvalues;
    while (upvalue && upvalue->stack_location > at) {
        prev = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue && upvalue->stack_location == at)
        return upvalue;

    value_t *var = at_stack(ctx, vm, at);
    obj_upvalue_t *new_upvalue = allocate_upvalue(ctx, vm);
    new_upvalue->location = var;
    new_upvalue->stack_location = at;

    new_upvalue->next = upvalue;
    if (!prev)
        vm->open_upvalues = new_upvalue;
    else
        prev->next = new_upvalue;

    return new_upvalue;
}

static void close_upvalues(ctx_t const *ctx, vm_t *vm, uint last)
{
    (void)ctx;
    while (vm->open_upvalues && vm->open_upvalues->stack_location >= last) {
         obj_upvalue_t *upvalue = vm->open_upvalues;
         upvalue->closed = *upvalue->location;
         upvalue->location = &upvalue->closed;
         vm->open_upvalues = upvalue->next;
    }
}

static interp_result_t run(ctx_t const *ctx, vm_t *vm)
{
    call_frame_t *frame;
    chunk_t *chunk;
    u8 *ip;
    uint bp;

#define REINIT_CACHED_VARS()      \
    do {                          \
        frame = vm->frame;        \
        chunk = &frame->f->chunk; \
        ip = frame->ip;           \
        bp = frame->bp;           \
    } while (0)

    REINIT_CACHED_VARS();

#define READ_BYTE() (*(ip++))
#define READ_SHORT() read_short_ip(ctx, &ip)
#define READ_LONG() read_long_ip(ctx, &ip)
#define READ_CONSTANT() (chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (chunk->constants.values[READ_LONG()])
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
#define CAPTURE_UPVALUES(cl_)                                                \
    do {                                                                     \
        for (uint i_ = 0; i_ < (cl_)->upvalue_cnt; ++i_) {                   \
            u8 flags_ = READ_BYTE();                                         \
            uint index_ = (flags_ & f_uv_long) ? READ_LONG() : READ_BYTE();  \
            if (flags_ & f_uv_local)                                         \
                (cl_)->upvalues[i_] = capture_upvalue(ctx, vm, bp + index_); \
            else                                                             \
                (cl_)->upvalues[i_] = frame->c->upvalues[index_];            \
        }                                                                    \
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
        disasm_instruction(ctx, chunk, (uint)(ip - chunk->code));
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
        case e_op_popn:
            popn_stack(ctx, vm, READ_BYTE());
            break;
        case e_op_popn_long:
            popn_stack(ctx, vm, READ_LONG());
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

        case e_op_get_loc:
            push_stack(ctx, vm, *at_stack(ctx, vm, bp + READ_BYTE()));
            break;
        case e_op_get_loc_long:
            push_stack(ctx, vm, *at_stack(ctx, vm, bp + READ_LONG()));
            break;
        case e_op_set_loc:
            *at_stack(ctx, vm, bp + READ_BYTE()) = STACK_TOP(vm);
            break;
        case e_op_set_loc_long:
            *at_stack(ctx, vm, bp + READ_LONG()) = STACK_TOP(vm);
            break;

        case e_op_get_upvalue:
            push_stack(ctx, vm, *frame->c->upvalues[READ_BYTE()]->location);
            break;
        case e_op_get_upvalue_long:
            push_stack(ctx, vm, *frame->c->upvalues[READ_LONG()]->location);
            break;
        case e_op_set_upvalue:
            *frame->c->upvalues[READ_BYTE()]->location = STACK_TOP(vm);
            break;
        case e_op_set_upvalue_long:
            *frame->c->upvalues[READ_LONG()]->location = STACK_TOP(vm);
            break;

        case e_op_closure: {
            obj_function_t *fn = AS_FUNCTION(READ_CONSTANT());
            obj_closure_t *closure = allocate_closure(ctx, vm, fn->upvalue_cnt);
            closure->fn = fn;
            push_stack(ctx, vm, OBJ_VAL(closure));
            CAPTURE_UPVALUES(closure);
        } break;
        case e_op_closure_long: {
            obj_function_t *fn = AS_FUNCTION(READ_CONSTANT_LONG());
            obj_closure_t *closure = allocate_closure(ctx, vm, fn->upvalue_cnt);
            closure->fn = fn;
            push_stack(ctx, vm, OBJ_VAL(closure));
            CAPTURE_UPVALUES(closure);
        } break;

        case e_op_close_upvalue:
            close_upvalues(ctx, vm, stack_size(ctx, vm) - 1);
            pop_stack(ctx, vm);
            break;

        case e_op_eq: {
            value_t b = pop_stack(ctx, vm);
            STACK_TOP(vm) = BOOL_VAL(are_equal(ctx, STACK_TOP(vm), b));
        } break;
        case e_op_eq_preserve_lhs:
            STACK_TOP(vm) = BOOL_VAL(
                are_equal(ctx, peek_stack(ctx, vm, 1), peek_stack(ctx, vm, 0)));
            break;
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

        case e_op_jump:
            ip += READ_SHORT();
            break;

        case e_op_jump_if_false: {
            u16 offset = READ_SHORT();
            if (is_falsey(ctx, STACK_TOP(vm)))
                ip += offset;
        } break;
        case e_op_jump_if_true: {
            u16 offset = READ_SHORT();
            if (is_truthy(ctx, STACK_TOP(vm)))
                ip += offset;
        } break;

        case e_op_loop:
            ip -= READ_SHORT();
            break;

        case e_op_call: {
            u8 arg_count = READ_BYTE();
            frame->ip = ip;
            if (!call_value(ctx, peek_stack(ctx, vm, arg_count), arg_count, vm))
                return e_interp_runtime_err;
            REINIT_CACHED_VARS();
        } break;
        case e_op_call_long: {
            u32 arg_count = READ_LONG();
            frame->ip = ip;
            if (!call_value(
                ctx, peek_stack(ctx, vm, (int)arg_count), arg_count, vm))
            {
                return e_interp_runtime_err;
            }
            REINIT_CACHED_VARS();
        } break;

        case e_op_return: {
            value_t result = pop_stack(ctx, vm);
            close_upvalues(ctx, vm, bp);
            pop_frame(ctx, vm);
            if (!vm->frame) {
                pop_stack(ctx, vm);
                return e_interp_ok;
            }
            uint values_to_pop = stack_size(ctx, vm) - frame->bp;
            popn_stack(ctx, vm, values_to_pop);
            push_stack(ctx, vm, result);
            REINIT_CACHED_VARS();
        } break;

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
  e_tt_colon, e_tt_question,
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
  e_tt_true, e_tt_var, e_tt_let, e_tt_while, e_tt_switch,
  e_tt_case, e_tt_default, e_tt_continue, e_tt_break,

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
        return scanner_check_kw(
            ctx, 1, LITSTR("nd"), e_tt_and, scanner);
    case 'b':
        return scanner_check_kw(
            ctx, 1, LITSTR("reak"), e_tt_break, scanner);
    case 'c':
        if (scanner->cur - scanner->start > 1) {
            switch (scanner->start[1]) {
            case 'a':
                return scanner_check_kw(
                    ctx, 2, LITSTR("se"), e_tt_case, scanner);
            case 'l':
                return scanner_check_kw(
                    ctx, 2, LITSTR("ass"), e_tt_class, scanner);
            case 'o':
                return scanner_check_kw(
                    ctx, 2, LITSTR("ntinue"), e_tt_continue, scanner);
            }
        }
        break;
    case 'd':
        return scanner_check_kw(
            ctx, 1, LITSTR("efault"), e_tt_default, scanner);
    case 'e':
        return scanner_check_kw(
            ctx, 1, LITSTR("lse"), e_tt_else, scanner);
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
    case 'l':
        return scanner_check_kw(ctx, 1, LITSTR("et"), e_tt_let, scanner);
    case 'n':
        return scanner_check_kw(ctx, 1, LITSTR("il"), e_tt_nil, scanner);
    case 'o':
        return scanner_check_kw(ctx, 1, LITSTR("r"), e_tt_or, scanner);
    case 'p':
        return scanner_check_kw(ctx, 1, LITSTR("rint"), e_tt_print, scanner);
    case 'r':
        return scanner_check_kw(ctx, 1, LITSTR("eturn"), e_tt_return, scanner);
    case 's':
        if (scanner->cur - scanner->start > 1) {
            switch (scanner->start[1]) {
            case 'u':
                return scanner_check_kw(
                    ctx, 2, LITSTR("per"), e_tt_super, scanner);
            case 'w':
                return scanner_check_kw(
                    ctx, 2, LITSTR("itch"), e_tt_switch, scanner);
            }
        }
        break;
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
    case ':':
        return MAKE_TOKEN(e_tt_colon, scanner);
    case '?':
        return MAKE_TOKEN(e_tt_question, scanner);
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
    case '&':
        if (scanner_match(ctx, '&', scanner))
            return MAKE_TOKEN(e_tt_and, scanner);
        break;
    case '|':
        if (scanner_match(ctx, '|', scanner))
            return MAKE_TOKEN(e_tt_or, scanner);
        break;
    case '"':
        return scan_string(ctx, scanner);
    default:
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
        case e_tt_let:
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

typedef enum {
    e_lst_block,
    e_lst_loop_body,
    e_lst_for_loop,
} lvar_scope_type_t;

typedef struct {
    uint cnt, cap;
    uint *items;
} uint_array_t;

static inline uint_array_t make_uint_array(ctx_t const *ctx)
{
    (void)ctx;
    return (uint_array_t){0};
}

static inline void write_uint_array(
    ctx_t const *ctx, uint_array_t *arr, uint val)
{
    if (arr->cap < arr->cnt + 1) {
        uint old_cap = arr->cap;
        arr->cap = GROW_CAP(old_cap);
        arr->items = GROW_ARRAY(uint, arr->items, old_cap, arr->cap);
    }

    arr->items[arr->cnt++] = val;
}

#if 0
static inline void free_uint_array(ctx_t const *ctx, uint_array_t *arr)
{
    if (!arr || !arr->items)
        return;
    FREE_ARRAY(int, arr->items, arr->cap);
    *arr = make_uint_array(ctx);
}
#endif

typedef struct {
    table_t map;
    int cnt_in_prev_blocks;
    uint initialized_cnt;
    uint_array_t captured_ids;
    lvar_scope_type_t type;
    uint loop_start_mark;
    uint loop_break_mark;
} lvar_scope_t;

typedef struct {
    uint vid;
    b32 is_local;
    b32 is_const;
} upvalue_t;

typedef struct {
    uint cnt, cap;
    upvalue_t *uvs;
} upvalue_array_t;

static inline upvalue_array_t make_upvalue_array(ctx_t const *ctx)
{
    (void)ctx;
    return (upvalue_array_t){0};
}

static inline void write_upvalue_array(
    ctx_t const *ctx, upvalue_array_t *arr, upvalue_t val)
{
    if (arr->cap < arr->cnt + 1) {
        uint old_cap = arr->cap;
        arr->cap = GROW_CAP(old_cap);
        arr->uvs = GROW_ARRAY(upvalue_t, arr->uvs, old_cap, arr->cap);
    }

    arr->uvs[arr->cnt++] = val;
}

static inline void free_upvalue_array(ctx_t const *ctx, upvalue_array_t *arr)
{
    if (!arr || !arr->uvs)
        return;
    FREE_ARRAY(upvalue_t, arr->uvs, arr->cap);
    *arr = make_upvalue_array(ctx);
}

typedef struct compiler {
    struct compiler *enclosing;
    parser_t *parser;
    obj_function_t *compiling_func;
    obj_function_type_t compiling_func_type;
    lvar_scope_t scope_lvars[256];
    int current_scope_depth;
    upvalue_array_t upvalues;
    lvar_scope_t *top_loop_block;
    uint top_loop_start_mark;
    uint top_loop_break_mark;
} compiler_t;

#define CUR_CHUNK(comp_) (&(comp_)->compiling_func->chunk)

static void emit_byte(ctx_t const *ctx, u8 byte, compiler_t *compiler)
{
    write_chunk(
        ctx, CUR_CHUNK(compiler), byte,
        (uint)compiler->parser->prev.line);
}

static void emit_constant(ctx_t const *ctx, value_t val, compiler_t *compiler)
{
    write_constant(
        ctx, CUR_CHUNK(compiler),
        e_op_constant, e_op_constant_long, val,
        (uint)compiler->parser->prev.line);
}

static void emit_closure(ctx_t const *ctx, value_t val, compiler_t *compiler)
{
    write_constant(
        ctx, CUR_CHUNK(compiler),
        e_op_closure, e_op_closure_long, val,
        (uint)compiler->parser->prev.line);
}

static void emit_id_ref(
    ctx_t const *ctx,
    uint id, opcode_t op, opcode_t long_op,
    compiler_t *compiler)
{
    write_id_ref(
        ctx, CUR_CHUNK(compiler),
        op, long_op, id, (uint)compiler->parser->prev.line);
}

static uint emit_jump(ctx_t const *ctx, u8 opc, compiler_t *compiler)
{
    emit_byte(ctx, opc, compiler);
    emit_byte(ctx, 0xFF, compiler);
    emit_byte(ctx, 0xFF, compiler);
    return CUR_CHUNK(compiler)->cnt - 2;
}

static void patch_jump(ctx_t const *ctx, uint at, compiler_t *compiler)
{
    int jump = (int)CUR_CHUNK(compiler)->cnt - (int)at - 2;
    if (jump > 0xFFFF)
        parser_error(ctx, "Too much code to jump over.", compiler->parser);

    CUR_CHUNK(compiler)->code[at] = jump & 0xFF;
    CUR_CHUNK(compiler)->code[at + 1] = (jump >> 8) & 0xFF;
}

static void emit_loop(ctx_t const *ctx, uint at, compiler_t *compiler)
{
    emit_byte(ctx, e_op_loop, compiler);

    int off = (int)CUR_CHUNK(compiler)->cnt - (int)at + 2;
    if (off > 0xFFFF)
        parser_error(ctx, "Too much code in loop body.", compiler->parser);

    emit_byte(ctx, off & 0xFF, compiler);
    emit_byte(ctx, (off >> 8) & 0xFF, compiler);
}

static void emit_return(ctx_t const *ctx, compiler_t *compiler)
{
    emit_byte(ctx, e_op_nil, compiler);
    emit_byte(ctx, e_op_return, compiler);
}

typedef struct {
    uint vid;
    b32 is_const;
    b32 is_captured;
} var_info_t;

typedef enum {
    f_vi_const = 1,
    f_vi_captured = 1 << 1
} var_info_packed_flags_t;

static f64 pack_var_info(ctx_t const *ctx, var_info_t info)
{
    u8 flags = 0;
    if (info.is_const)
        flags |= f_vi_const;
    if (info.is_captured)
        flags |= f_vi_captured;
    f64 packed = (f64)info.vid;
    u64 bits = *(u64 *)&packed;
    ASSERT((bits & 0xFF) == 0);
    bits |= (u64)flags;
    return *(f64 *)&bits;
}

static var_info_t unpack_var_info(ctx_t const *ctx, f64 packed)
{
    (void)ctx;
    var_info_t vi = {0};
    u64 bits = *(u64 *)&packed;
    u8 flags = (u8)(bits & 0xFF);
    u64 vid_bits = bits & ~0xFFull;
    vi.vid = (uint)(*(f64 *)&vid_bits);
    vi.is_const = flags & f_vi_const;
    vi.is_captured = flags & f_vi_captured;
    return vi;
}

static var_info_t register_gvar(
    ctx_t const *ctx, vm_t *vm, token_t name, b32 is_const)
{
    obj_string_t *str =
        (obj_string_t *)add_string_ref(ctx, vm, name.start, (uint)(name.len));
    value_t id;
    if (!table_get(ctx, &vm->gvar_map, str, &id)) {
        write_value_array(ctx, &vm->gvars, VACANT_VAL_NAMED((obj_t *)str));
        var_info_t vi = {vm->gvars.cnt - 1, is_const, false};
        id = NUMBER_VAL(pack_var_info(ctx, vi));
        table_set(ctx, &vm->gvar_map, str, id);
    }

    f64 packed = AS_NUMBER(id);
    return unpack_var_info(ctx, packed);
}

static uint register_lvar(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm,
    token_t name, b32 is_const)
{
    obj_string_t *str =
        (obj_string_t *)add_string_ref(ctx, vm, name.start, (uint)name.len);
    lvar_scope_t *scope = &compiler->scope_lvars[compiler->current_scope_depth];
    table_t *scope_lvar_table = &scope->map;
    uint id = scope_lvar_table->cnt;
    var_info_t vi = {id + (uint)scope->cnt_in_prev_blocks, is_const, false};
    value_t lvar_id = NUMBER_VAL(pack_var_info(ctx, vi));
    if (!table_set(ctx, scope_lvar_table, str, lvar_id)) {
        parser_error(
            ctx,
            "Local variable redeclaration in same scope.",
            compiler->parser);
        return (uint)(-1);
    }

    return id;
}

static void mark_lvar_initialzied(
    ctx_t const *ctx, compiler_t *compiler, uint vid)
{
    if (compiler->current_scope_depth == 0)
        return;
    lvar_scope_t *scope = &compiler->scope_lvars[compiler->current_scope_depth];
    table_t *scope_lvar_table = &scope->map;
    ASSERT(scope_lvar_table->cnt == scope->initialized_cnt + 1);
    ASSERT(vid == scope->initialized_cnt);
    ++scope->initialized_cnt;
}

static var_info_t resolve_lvar(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm,
    token_t name, b32 capture_on_found)
{
    if (compiler->current_scope_depth == 0)
        return (var_info_t){(uint)(-1), true, false};
    obj_string_t *str =
        (obj_string_t *)add_string_ref(ctx, vm, name.start, (uint)(name.len));
    for (int i = compiler->current_scope_depth; i >= 1; --i) {
        lvar_scope_t *scope = &compiler->scope_lvars[i];
        value_t id_val;
        if (table_get(ctx, &scope->map, str, &id_val)) {
            f64 packed = AS_NUMBER(id_val);
            var_info_t vi = unpack_var_info(ctx, packed);
            if (vi.vid >=
                scope->initialized_cnt + (uint)scope->cnt_in_prev_blocks)
            {
                parser_error(
                    ctx,
                    "Can't read local variable in it's own initializer.",
                    compiler->parser);
                vi.vid = (uint)(-1);
                vi.is_const = true;
                vi.is_captured = false;
            } else if (capture_on_found && !vi.is_captured) {
                vi.is_captured = true;
                value_t new_val = NUMBER_VAL(pack_var_info(ctx, vi));
                table_set(ctx, &scope->map, str, new_val);
                write_uint_array(ctx, &scope->captured_ids, vi.vid);
            }
            return vi;
        }
    }
    return (var_info_t){(uint)(-1), true, false};
}

static var_info_t add_upvalue(
    ctx_t const *ctx, compiler_t *compiler,
    uint vid, b32 is_local, b32 is_const)
{
    (void)ctx;
    uint uid = compiler->upvalues.cnt;

    for (uint i = 0; i < uid; ++i) {
        upvalue_t const *uv = &compiler->upvalues.uvs[i];
        if (uv->vid == vid && uv->is_local == is_local)
            return (var_info_t){i, is_const, true};
    }

    write_upvalue_array(ctx,
        &compiler->upvalues, (upvalue_t){vid, is_local, is_const});

    return (var_info_t){
        (uint)compiler->compiling_func->upvalue_cnt++, is_const, true};
}

static var_info_t resolve_upvalue(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, token_t name)
{
    if (!compiler->enclosing)
        return (var_info_t){(uint)(-1), true, false};

    var_info_t lres = resolve_lvar(ctx, compiler->enclosing, vm, name, true);
    if (lres.vid != (uint)(-1))
        return add_upvalue(ctx, compiler, lres.vid, true, lres.is_const);

    var_info_t ures = resolve_upvalue(ctx, compiler->enclosing, vm, name);
    if (ures.vid != (uint)(-1))
        return add_upvalue(ctx, compiler, ures.vid, false, ures.is_const);

    return (var_info_t){(uint)(-1), true, false};
}

static void define_native(
    ctx_t const *ctx, vm_t *vm, string_t name, native_fn_t fn, int arity)
{
    obj_t *oname = add_string_ref(ctx, vm, name.p, (uint)name.len);
    push_stack(ctx, vm, OBJ_VAL(oname));
    obj_native_t *on = ALLOCATE_OBJ(obj_native_t, e_ot_native, vm);
    on->fn = fn;
    on->arity = arity;
    push_stack(ctx, vm, OBJ_VAL(on));
    var_info_t rr = register_gvar(
        ctx, vm, (token_t){e_tt_identifier, name.p, (int)name.len, -1}, true);
    vm->gvars.values[rr.vid] = OBJ_VAL(on);
    popn_stack(ctx, vm, 2);
}

static void begin_compiler(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, compiler_t *enc)
{
    compiler->enclosing = enc;

    for (uint i = 0; i < ARRCNT(compiler->scope_lvars); ++i) {
        compiler->scope_lvars[i].map = make_table(ctx);
        compiler->scope_lvars[i].cnt_in_prev_blocks = 0;
        compiler->scope_lvars[i].initialized_cnt = 0;
    }
    compiler->current_scope_depth = 0;
    compiler->upvalues = make_upvalue_array(ctx);
    compiler->top_loop_block = NULL;
    compiler->top_loop_start_mark = (uint)(-1);
    compiler->top_loop_break_mark = (uint)(-1);

    uint dummy = register_lvar(
        ctx, compiler, vm, (token_t){(token_type_t)0, "", 0, 0}, true);
    mark_lvar_initialzied(ctx, compiler, dummy);
}

static obj_function_t *end_compiler(ctx_t const *ctx, compiler_t *compiler)
{
    emit_return(ctx, compiler);
    for (uint i = 0; i < ARRCNT(compiler->scope_lvars); ++i)
        free_table(ctx, &compiler->scope_lvars[i].map);

    obj_function_t *func = compiler->compiling_func;
#if DEBUG_PRINT_CODE
    if (!compiler->parser->had_error) {
        DISASSEMBLE_CHUNK(
            &func->chunk,
            func->name ? OSTR_TO_STR(func->name) : LITSTR("<script>"));
    }
#endif
    
    return func;
}

static void begin_scope(
    ctx_t const *ctx, compiler_t *compiler, lvar_scope_type_t type)
{
    (void)ctx;
    if (compiler->current_scope_depth >= 255) {
        parser_error(
            ctx,
            "Can't have more than 255 nested blocks in one chunk.",
            compiler->parser);
        return;
    }
    ++compiler->current_scope_depth;
    if (type == e_lst_block && compiler->top_loop_start_mark != (uint)(-1)) {
        compiler->top_loop_block =
            &compiler->scope_lvars[compiler->current_scope_depth];
        compiler->top_loop_block->loop_start_mark =
            compiler->top_loop_start_mark;
        compiler->top_loop_block->loop_break_mark =
            compiler->top_loop_break_mark;
        compiler->top_loop_start_mark = (uint)(-1);
        compiler->top_loop_break_mark = (uint)(-1);
        type = e_lst_loop_body;
    }
    if (compiler->current_scope_depth > 0) {
        lvar_scope_t *prev_scope =
            &compiler->scope_lvars[compiler->current_scope_depth - 1];
        lvar_scope_t *cur_scope =
            &compiler->scope_lvars[compiler->current_scope_depth];

        cur_scope->cnt_in_prev_blocks =
            prev_scope->cnt_in_prev_blocks + (int)prev_scope->map.cnt;
        cur_scope->initialized_cnt = 0;
    }
    compiler->scope_lvars[compiler->current_scope_depth].type = type;
}

static void end_scope(ctx_t const *ctx, compiler_t *compiler)
{
    (void)ctx;
    ASSERT(compiler->current_scope_depth);
    lvar_scope_t *scope = &compiler->scope_lvars[compiler->current_scope_depth];
    table_t *scope_lvar_table = &scope->map;
    if (scope->captured_ids.cnt) {
        // Sort in decreasing order
        for (
            uint *r = scope->captured_ids.items + 1;
            r < scope->captured_ids.items + scope->captured_ids.cnt;
            ++r)
        {
            uint *p;
            for (p = r - 1; p >= scope->captured_ids.items; --p) {
                if (*p > *r)
                    break;
            }
            if (p < r - 1) {
                uint rr = *r;
                uint *dst = r, *src = r - 1;
                for (isize i = 0; i < (r - p - 1); ++i)
                    *dst-- = *src--;
                *(p + 1) = rr;
            }
        }

        int next_cap_id = 0;
        int next_id = (int)(scope_lvar_table->cnt - 1);
        int pop_run = 0;
        while (next_id >= 0) {
            if (
                (uint)next_cap_id < scope->captured_ids.cnt &&
                scope->captured_ids.items[next_cap_id] ==
                    (uint)(next_id + scope->cnt_in_prev_blocks))
            {
                if (pop_run == 1) {
                    emit_byte(ctx, e_op_pop, compiler);
                } else if (pop_run > 1) {
                    emit_id_ref(
                        ctx, (uint)pop_run,
                        e_op_popn, e_op_popn_long, compiler);
                }
                pop_run = 0;
                emit_byte(ctx, e_op_close_upvalue, compiler);
                ++next_cap_id;
            } else {
                ++pop_run;
            }
            --next_id;
        }
        if (pop_run == 1) {
            emit_byte(ctx, e_op_pop, compiler);
        } else if (pop_run > 1) {
            emit_id_ref(
                ctx, (uint)pop_run,
                e_op_popn, e_op_popn_long, compiler);
        }
    } else if (scope_lvar_table->cnt == 1) {
        emit_byte(ctx, e_op_pop, compiler);
    } else if (scope_lvar_table->cnt > 1) {
        emit_id_ref(
            ctx, scope_lvar_table->cnt, e_op_popn, e_op_popn_long, compiler);
    }

    if (compiler->top_loop_block == scope) {
        --compiler->top_loop_block;
        while (
            compiler->top_loop_block >= compiler->scope_lvars &&
            compiler->top_loop_block->type != e_lst_loop_body)
        {
            --compiler->top_loop_block;
        }
        if (compiler->top_loop_block < compiler->scope_lvars)
            compiler->top_loop_block = NULL;
    }

    memset(
        scope_lvar_table->entries, 0,
        sizeof(table_entry_t) * scope_lvar_table->cap);
    scope_lvar_table->cnt = 0;
    scope->initialized_cnt = 0;

    memset(
        scope->captured_ids.items, 0,
        sizeof(int) * scope->captured_ids.cap);
    scope->captured_ids.cnt = 0;

    --compiler->current_scope_depth;
}

typedef enum {
    e_prec_none,
    e_prec_assignment, // =
    e_prec_choice,     // ?:
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
static void compile_fun(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_grouping(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_call(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_unary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_binary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_ternary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_and(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);
static void compile_or(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign);

parse_rule_t const c_parse_rules[] = {
    [e_tt_left_paren]    = {&compile_grouping, &compile_call,    e_prec_call  },
    [e_tt_right_paren]   = {NULL,              NULL,             e_prec_none  },
    [e_tt_left_brace]    = {NULL,              NULL,             e_prec_none  },
    [e_tt_right_brace]   = {NULL,              NULL,             e_prec_none  },
    [e_tt_comma]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_dot]           = {NULL,              NULL,             e_prec_none  },
    [e_tt_minus]         = {&compile_unary,    &compile_binary,  e_prec_term  },
    [e_tt_plus]          = {NULL,              &compile_binary,  e_prec_term  },
    [e_tt_semicolon]     = {NULL,              NULL,             e_prec_none  },
    [e_tt_slash]         = {NULL,              &compile_binary,  e_prec_factor},
    [e_tt_star]          = {NULL,              &compile_binary,  e_prec_factor},
    [e_tt_colon]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_question]      = {NULL,              &compile_ternary, e_prec_choice},
    [e_tt_bang]          = {&compile_unary,    NULL,             e_prec_none  },
    [e_tt_bang_equal]    = {NULL,              &compile_binary,  e_prec_eql   },
    [e_tt_equal]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_equal_equal]   = {NULL,              &compile_binary,  e_prec_eql   },
    [e_tt_greater]       = {NULL,              &compile_binary,  e_prec_cmp   },
    [e_tt_greater_equal] = {NULL,              &compile_binary,  e_prec_cmp   },
    [e_tt_less]          = {NULL,              &compile_binary,  e_prec_cmp   },
    [e_tt_less_equal]    = {NULL,              &compile_binary,  e_prec_cmp   },
    [e_tt_identifier]    = {&compile_variable, NULL,             e_prec_none  },
    [e_tt_string]        = {&compile_string,   NULL,             e_prec_none  },
    [e_tt_number]        = {&compile_number,   NULL,             e_prec_none  },
    [e_tt_and]           = {NULL,              &compile_and,     e_prec_and   },
    [e_tt_class]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_else]          = {NULL,              NULL,             e_prec_none  },
    [e_tt_false]         = {&compile_literal,  NULL,             e_prec_none  },
    [e_tt_for]           = {NULL,              NULL,             e_prec_none  },
    [e_tt_fun]           = {&compile_fun,      NULL,             e_prec_none  },
    [e_tt_if]            = {NULL,              NULL,             e_prec_none  },
    [e_tt_nil]           = {&compile_literal,  NULL,             e_prec_none  },
    [e_tt_or]            = {NULL,              &compile_or,      e_prec_or    },
    [e_tt_print]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_return]        = {NULL,              NULL,             e_prec_none  },
    [e_tt_super]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_this]          = {NULL,              NULL,             e_prec_none  },
    [e_tt_true]          = {&compile_literal,  NULL,             e_prec_none  },
    [e_tt_var]           = {NULL,              NULL,             e_prec_none  },
    [e_tt_let]           = {NULL,              NULL,             e_prec_none  },
    [e_tt_while]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_switch]        = {NULL,              NULL,             e_prec_none  },
    [e_tt_case]          = {NULL,              NULL,             e_prec_none  },
    [e_tt_default]       = {NULL,              NULL,             e_prec_none  },
    [e_tt_continue]      = {NULL,              NULL,             e_prec_none  },
    [e_tt_break]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_error]         = {NULL,              NULL,             e_prec_none  },
    [e_tt_eof]           = {NULL,              NULL,             e_prec_none  },
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

    while (prec <= get_rule(ctx, compiler->parser->cur.type)->precedence) {
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
    u8 get_op, set_op, get_lop, set_lop;
    var_info_t resolved = resolve_lvar(ctx, compiler, vm, name, false);
    if (resolved.vid != (uint)(-1)) {
        get_op = e_op_get_loc;
        get_lop = e_op_get_loc_long;
        set_op = e_op_set_loc;
        set_lop = e_op_set_loc_long;
    } else if (
        (resolved = resolve_upvalue(ctx, compiler, vm, name)).vid
        != (uint)(-1))
    {
        get_op = e_op_get_upvalue;
        get_lop = e_op_get_upvalue_long;
        set_op = e_op_set_upvalue;
        set_lop = e_op_set_upvalue_long;
    } else {
        resolved = register_gvar(ctx, vm, name, false);
        get_op = e_op_get_glob;
        get_lop = e_op_get_glob_long;
        set_op = e_op_set_glob;
        set_lop = e_op_set_glob_long;
    }

    if (can_assign && parser_match(ctx, e_tt_equal, compiler->parser)) {
        if (resolved.is_const) {
            parser_error(
                ctx, "Can't assign to an immutable variable.",
                compiler->parser);
            return;
        }
        compile_expression(ctx, compiler, vm);
        emit_id_ref(ctx, resolved.vid, set_op, set_lop, compiler);
    } else {
        emit_id_ref(ctx, resolved.vid, get_op, get_lop, compiler);
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

static uint compile_arg_list(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    uint arg_count = 0;
    if (!parser_check(ctx, e_tt_right_paren, compiler->parser)) {
        do {
            compile_expression(ctx, compiler, vm);
            ++arg_count;
        } while (parser_match(ctx, e_tt_comma, compiler->parser));
    }
    parser_consume(ctx, e_tt_right_paren,
        "Expected ')' after arg list", compiler->parser);
    return arg_count;
}

static void compile_call(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    uint arg_count = compile_arg_list(ctx, compiler, vm);
    emit_id_ref(ctx, arg_count, e_op_call, e_op_call_long, compiler);
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

static void compile_ternary(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    token_type_t tt = compiler->parser->prev.type;
    ASSERT(tt == e_tt_question);
    parse_rule_t const *rule = get_rule(ctx, tt);

    uint lhs_jump = emit_jump(ctx, e_op_jump_if_false, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    compile_precedence(ctx, (precedence_t)(rule->precedence + 1), compiler, vm);
    uint end_jump = emit_jump(ctx, e_op_jump, compiler);

    parser_consume(
        ctx, e_tt_colon, "Expected ':' after choice lhs.", compiler->parser);

    patch_jump(ctx, lhs_jump, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    compile_precedence(ctx, (precedence_t)(rule->precedence + 1), compiler, vm);

    patch_jump(ctx, end_jump, compiler);
}

static void compile_and(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    uint end_jump = emit_jump(ctx, e_op_jump_if_false, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    compile_precedence(ctx, e_prec_and, compiler, vm);

    patch_jump(ctx, end_jump, compiler);
}

static void compile_or(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    uint end_jump = emit_jump(ctx, e_op_jump_if_true, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    compile_precedence(ctx, e_prec_or, compiler, vm);

    patch_jump(ctx, end_jump, compiler);
}

static void compile_decl(ctx_t const *ctx, compiler_t *compiler, vm_t *vm);
static void compile_stmt(ctx_t const *ctx, compiler_t *compiler, vm_t *vm);
static void compile_block(ctx_t const *ctx, compiler_t *compiler, vm_t *vm);
static void compile_var_decl(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 is_const);

static void compile_fun(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 can_assign)
{
    (void)can_assign;
    compiler_t *push_compiler = ALLOCATE(compiler_t, 1);
    push_compiler->parser = compiler->parser;
    push_compiler->compiling_func = allocate_function(ctx, vm);
    push_compiler->compiling_func_type = e_ft_function;

    push_compiler->compiling_func->name =
        (obj_string_t *)add_string_ref(
            ctx, vm,
            push_compiler->parser->prev.start,
            (uint)push_compiler->parser->prev.len);

    begin_compiler(ctx, push_compiler, vm, compiler);
    begin_scope(ctx, push_compiler, e_lst_block);

    parser_consume(ctx, e_tt_left_paren,
        "Expected '(' before function args", push_compiler->parser);
    if (!parser_check(ctx, e_tt_right_paren, push_compiler->parser)) {
        do {
            ++push_compiler->compiling_func->arity;
            parser_consume(ctx, e_tt_identifier,
                "Expected param name.", compiler->parser);
            uint vid = register_lvar(
                ctx, push_compiler, vm, compiler->parser->prev, false);
            mark_lvar_initialzied(ctx, push_compiler, vid);
        } while (parser_match(ctx, e_tt_comma, push_compiler->parser));
    }
    parser_consume(ctx, e_tt_right_paren,
        "Expected ')' after function args", push_compiler->parser);
    parser_consume(ctx, e_tt_left_brace,
        "Expected '{' before function body", push_compiler->parser);
    compile_block(ctx, push_compiler, vm);

    obj_function_t *func = end_compiler(ctx, push_compiler);

    if (func->upvalue_cnt) {
        emit_closure(ctx, OBJ_VAL(func), compiler);

        for (uint i = 0; i < func->upvalue_cnt; ++i) {
            uint vid = push_compiler->upvalues.uvs[i].vid;
            u8 flags = 0;
            flags |= push_compiler->upvalues.uvs[i].is_local ? f_uv_local : 0;
            flags |= vid > 255 ? f_uv_long : 0;
            emit_byte(ctx, flags, compiler);
            emit_byte(ctx, (u8)(vid & 0xFF), compiler);
            if (flags & f_uv_long) {
                if (vid > 16777215) {
                    parser_error(
                            ctx, "Can't have more than 16777215 upvalues",
                            compiler->parser);
                    return;
                }
                emit_byte(ctx, (u8)((vid >> 8) & 0xFF), compiler);
                emit_byte(ctx, (u8)((vid >> 16) & 0xFF), compiler);
            }
        }

        free_upvalue_array(ctx, &push_compiler->upvalues);
    } else {
        emit_constant(ctx, OBJ_VAL(func), compiler);
    }

    FREE(push_compiler);
}

static uint compile_new_variable(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm,
    char const *err, b32 is_const)
{
    parser_consume(ctx, e_tt_identifier, err, compiler->parser);
    if (compiler->current_scope_depth == 0) {
        return register_gvar(
            ctx, vm, compiler->parser->prev, is_const).vid;
    } else {
        return register_lvar(
            ctx, compiler, vm, compiler->parser->prev, is_const);
    }
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

static void compile_break(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    (void)vm;
    parser_consume(ctx, e_tt_semicolon,
        "Expected ';' after break.", compiler->parser);
    if (!compiler->top_loop_block) {
        parser_error(
            ctx, "Can't continue outside of a loop.", compiler->parser);
        return;
    }
    uint scopes_to_pop = (uint)(
        compiler->current_scope_depth -
        (compiler->top_loop_block - compiler->scope_lvars));
    if (compiler->top_loop_block > compiler->scope_lvars &&
        compiler->top_loop_block[-1].type == e_lst_for_loop)
    {
        ++scopes_to_pop;
    }
    lvar_scope_t *s = compiler->top_loop_block;
    uint lvars = 0;
    while (scopes_to_pop--)
        lvars += (s++)->map.cnt;
    if (lvars == 1)
        emit_byte(ctx, e_op_pop, compiler);
    else if (lvars > 1)
        emit_id_ref(ctx, lvars, e_op_popn, e_op_popn_long, compiler);

    emit_loop(ctx, compiler->top_loop_block->loop_break_mark, compiler);
}

static void compile_continue(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    (void)vm;
    parser_consume(ctx, e_tt_semicolon,
        "Expected ';' after continue.", compiler->parser);
    if (!compiler->top_loop_block) {
        parser_error(
            ctx, "Can't continue outside of a loop.", compiler->parser);
        return;
    }
    uint scopes_to_pop = (uint)(
        compiler->current_scope_depth -
        (compiler->top_loop_block - compiler->scope_lvars));
    lvar_scope_t *s = compiler->top_loop_block;
    uint lvars = 0;
    while (scopes_to_pop--)
        lvars += (s++)->map.cnt;
    if (lvars == 1)
        emit_byte(ctx, e_op_pop, compiler);
    else if (lvars > 1)
        emit_id_ref(ctx, lvars, e_op_popn, e_op_popn_long, compiler);

    emit_loop(ctx, compiler->top_loop_block->loop_start_mark, compiler);
}

static void compile_return(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    if (compiler->compiling_func_type == e_ft_script) {
        parser_error(
            ctx, "Can't return from top level code.", compiler->parser);
        return;
    }

    if (parser_match(ctx, e_tt_semicolon, compiler->parser)) {
        emit_return(ctx, compiler);
    } else {
        compile_expression(ctx, compiler, vm);
        parser_consume(ctx, e_tt_semicolon,
            "Expected ';' after return value.", compiler->parser);
        emit_byte(ctx, e_op_return, compiler);
    }
}

static void compile_switch(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    parser_consume(ctx, e_tt_left_paren,
        "Expected '(' after switch.", compiler->parser);
    compile_expression(ctx, compiler, vm);
    parser_consume(ctx, e_tt_right_paren,
        "Expected ')' after switch argument.", compiler->parser);
    parser_consume(ctx, e_tt_left_brace,
        "Expected '{' before switch body.", compiler->parser);

    uint init_jump = emit_jump(ctx, e_op_jump, compiler);
    uint break_counter = CUR_CHUNK(compiler)->cnt;
    uint break_jump = emit_jump(ctx, e_op_jump, compiler);

    uint case_jump = init_jump;

    if (parser_match(ctx, e_tt_case, compiler->parser)) {
        do {
            patch_jump(ctx, case_jump, compiler);
            if (case_jump != init_jump)
                emit_byte(ctx, e_op_pop, compiler);
            compile_expression(ctx, compiler, vm);
            emit_byte(ctx, e_op_eq_preserve_lhs, compiler);
            case_jump = emit_jump(ctx, e_op_jump_if_false, compiler);
            emit_byte(ctx, e_op_pop, compiler);
            parser_consume(ctx, e_tt_colon,
                "Expected ':' after case mark.", compiler->parser);
            while (
                !parser_check(ctx, e_tt_case, compiler->parser) &&
                !parser_check(ctx, e_tt_default, compiler->parser) &&
                !parser_check(ctx, e_tt_right_brace, compiler->parser))
            {
                compile_stmt(ctx, compiler, vm);
            }
            emit_loop(ctx, break_counter, compiler);
        } while (parser_match(ctx, e_tt_case, compiler->parser));
    }

    if (parser_match(ctx, e_tt_default, compiler->parser)) {
        patch_jump(ctx, case_jump, compiler);
        if (case_jump != init_jump)
            emit_byte(ctx, e_op_pop, compiler);
        case_jump = (uint)(-1);
        parser_consume(ctx, e_tt_colon,
            "Expected ':' after default mark.", compiler->parser);
        while (!parser_check(ctx, e_tt_right_brace, compiler->parser))
            compile_stmt(ctx, compiler, vm);
    } else if (case_jump != init_jump) {
        patch_jump(ctx, case_jump, compiler);
        emit_byte(ctx, e_op_pop, compiler);
    }

    parser_consume(ctx, e_tt_right_brace,
        "Expected '}' after switch body.", compiler->parser);

    patch_jump(ctx, break_jump, compiler);
    if (case_jump == init_jump)
        patch_jump(ctx, init_jump, compiler);
    emit_byte(ctx, e_op_pop, compiler);
}

static void compile_for(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    begin_scope(ctx, compiler, e_lst_for_loop);
    
    token_t loop_var = {0};
    b32 loop_var_is_const = false;
    uint loop_var_vid = (uint)(-1);

    parser_consume(ctx, e_tt_left_paren,
        "Expected '(' after for.", compiler->parser);
    if (parser_match(ctx, e_tt_semicolon, compiler->parser)) {

    } else if (parser_match(ctx, e_tt_var, compiler->parser)) {
        loop_var = compiler->parser->cur;
        loop_var_is_const = false;
        compile_var_decl(ctx, compiler, vm, false);
    } else if (parser_match(ctx, e_tt_let, compiler->parser)) {
        loop_var = compiler->parser->cur;
        loop_var_is_const = true;
        compile_var_decl(ctx, compiler, vm, true);
    } else {
        compile_expr_stmt(ctx, compiler, vm);
    }

    if (loop_var.type == e_tt_identifier) {
        loop_var_vid =
            (uint)compiler
                ->scope_lvars[compiler->current_scope_depth]
                .cnt_in_prev_blocks;
    }

    uint start_jump = emit_jump(ctx, e_op_jump, compiler);

    uint break_counter = CUR_CHUNK(compiler)->cnt;
    uint break_jump = emit_jump(ctx, e_op_jump, compiler);

    uint loop_start = CUR_CHUNK(compiler)->cnt;
    patch_jump(ctx, start_jump, compiler);

    uint exit_jump = (uint)(-1);

    if (!parser_match(ctx, e_tt_semicolon, compiler->parser)) {
        compile_expression(ctx, compiler, vm);
        parser_consume(ctx, e_tt_semicolon,
            "Expected ';' after for condition.", compiler->parser);
        exit_jump = emit_jump(ctx, e_op_jump_if_false, compiler);
        emit_byte(ctx, e_op_pop, compiler);
    }

    if (!parser_match(ctx, e_tt_right_paren, compiler->parser)) {
        uint body_jump = emit_jump(ctx, e_op_jump, compiler);
        uint increment_start = CUR_CHUNK(compiler)->cnt;
        compile_expression(ctx, compiler, vm);
        emit_byte(ctx, e_op_pop, compiler);
        parser_consume(ctx, e_tt_right_paren,
            "Expected ')' after for clause.", compiler->parser);

        emit_loop(ctx, loop_start, compiler);
        loop_start = increment_start;
        patch_jump(ctx, body_jump, compiler);
    }

    compiler->top_loop_start_mark = loop_start;
    compiler->top_loop_break_mark = break_counter;
    {
        if (loop_var.type == e_tt_identifier) {
            begin_scope(ctx, compiler, e_lst_block);
            uint vid =
                register_lvar(ctx, compiler, vm, loop_var, loop_var_is_const);
            mark_lvar_initialzied(ctx, compiler, vid);
            emit_id_ref(ctx,
                loop_var_vid, e_op_get_loc, e_op_get_loc_long, compiler);
            emit_id_ref(ctx,
                vid + (uint)compiler
                    ->scope_lvars[compiler->current_scope_depth]
                    .cnt_in_prev_blocks,
                e_op_set_loc, e_op_set_loc_long, compiler);
        }
        compile_stmt(ctx, compiler, vm);
        if (loop_var.type == e_tt_identifier)
            end_scope(ctx, compiler);
    }
    compiler->top_loop_start_mark = (uint)(-1);
    compiler->top_loop_break_mark = (uint)(-1);

    emit_loop(ctx, loop_start, compiler);

    if (exit_jump != (uint)(-1)) {
        patch_jump(ctx, exit_jump, compiler);
        emit_byte(ctx, e_op_pop, compiler);
    }
    patch_jump(ctx, break_jump, compiler);

    end_scope(ctx, compiler);
}

static void compile_while(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    uint start_jump = emit_jump(ctx, e_op_jump, compiler);

    uint break_counter = CUR_CHUNK(compiler)->cnt;
    uint break_jump = emit_jump(ctx, e_op_jump, compiler);

    uint loop_start = CUR_CHUNK(compiler)->cnt;
    patch_jump(ctx, start_jump, compiler);

    parser_consume(ctx, e_tt_left_paren,
        "Expected '(' after while.", compiler->parser);
    compile_expression(ctx, compiler, vm);
    parser_consume(ctx, e_tt_right_paren,
        "Expected ')' after condition.", compiler->parser);

    uint exit_jump = emit_jump(ctx, e_op_jump_if_false, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    compiler->top_loop_start_mark = loop_start;
    compiler->top_loop_break_mark = break_counter;
    compile_stmt(ctx, compiler, vm);
    compiler->top_loop_start_mark = (uint)(-1);
    compiler->top_loop_break_mark = (uint)(-1);

    emit_loop(ctx, loop_start, compiler);

    patch_jump(ctx, exit_jump, compiler);
    emit_byte(ctx, e_op_pop, compiler);
    patch_jump(ctx, break_jump, compiler);
}

static void compile_if(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    parser_consume(ctx, e_tt_left_paren,
        "Expected '(' after if.", compiler->parser);
    compile_expression(ctx, compiler, vm);
    parser_consume(ctx, e_tt_right_paren,
        "Expected ')' after condition.", compiler->parser);

    uint then_jump = emit_jump(ctx, e_op_jump_if_false, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    compile_stmt(ctx, compiler, vm);

    uint else_jump = emit_jump(ctx, e_op_jump, compiler);

    patch_jump(ctx, then_jump, compiler);
    emit_byte(ctx, e_op_pop, compiler);

    if (parser_match(ctx, e_tt_else, compiler->parser))
        compile_stmt(ctx, compiler, vm);

    patch_jump(ctx, else_jump, compiler);
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
    } else if (parser_match(ctx, e_tt_if, compiler->parser)) {
        compile_if(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_while, compiler->parser)) {
        compile_while(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_for, compiler->parser)) {
        compile_for(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_switch, compiler->parser)) {
        compile_switch(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_return, compiler->parser)) {
        compile_return(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_continue, compiler->parser)) {
        compile_continue(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_break, compiler->parser)) {
        compile_break(ctx, compiler, vm);
    } else if (parser_match(ctx, e_tt_left_brace, compiler->parser)) {
        begin_scope(ctx, compiler, e_lst_block);
        compile_block(ctx, compiler, vm);
        end_scope(ctx, compiler);
    } else {
        compile_expr_stmt(ctx, compiler, vm);
    }
}

static void compile_func_decl(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    uint vid = compile_new_variable(
        ctx, compiler, vm, "Expected function name.", true);
    mark_lvar_initialzied(ctx, compiler, vid);

    compile_fun(ctx, compiler, vm, false);

    if (compiler->current_scope_depth == 0) {
        emit_id_ref(
            ctx, vid, e_op_define_glob, e_op_define_glob_long, compiler);
    }
}

static void compile_var_decl(
    ctx_t const *ctx, compiler_t *compiler, vm_t *vm, b32 is_const)
{
    uint vid = compile_new_variable(
        ctx, compiler, vm, "Expected variable name.", is_const);

    if (parser_match(ctx, e_tt_equal, compiler->parser))
        compile_expression(ctx, compiler, vm);
    else
        emit_byte(ctx, e_op_nil, compiler);

    parser_consume(
        ctx, e_tt_semicolon, "Expected ';' after var decl.", compiler->parser);

    if (compiler->current_scope_depth == 0) {
        emit_id_ref(
            ctx, vid, e_op_define_glob, e_op_define_glob_long, compiler);
    } else {
        mark_lvar_initialzied(ctx, compiler, vid);
    }
}

static void compile_decl(ctx_t const *ctx, compiler_t *compiler, vm_t *vm)
{
    if (parser_match(ctx, e_tt_var, compiler->parser))
        compile_var_decl(ctx, compiler, vm, false);
    else if (parser_match(ctx, e_tt_let, compiler->parser))
        compile_var_decl(ctx, compiler, vm, true);
    else if (parser_match(ctx, e_tt_fun, compiler->parser))
        compile_func_decl(ctx, compiler, vm);
    else
        compile_stmt(ctx, compiler, vm);

    if (compiler->parser->panic_mode)
        parser_sync(ctx, compiler->parser);
}

static obj_function_t *compile(ctx_t const *ctx, string_t source, vm_t *vm)
{
    scanner_t scanner = {source.p, source.p, source.p + source.len, 1}; 
    parser_t parser = {0};
    compiler_t compiler = {0};

    parser.scanner = &scanner;
    compiler.parser = &parser;
    compiler.compiling_func = allocate_function(ctx, vm);
    compiler.compiling_func_type = e_ft_script;

    begin_compiler(ctx, &compiler, vm, NULL);

    parser_advance(ctx, &parser);
    while (!parser_match(ctx, e_tt_eof, &parser))
        compile_decl(ctx, &compiler, vm);

    obj_function_t *func = end_compiler(ctx, &compiler);
    return parser.had_error ? NULL : func;
}

static interp_result_t interpret(ctx_t const *ctx, vm_t *vm, string_t source)
{
    obj_function_t *func = compile(ctx, source, vm);
    if (!func)
        return e_interp_compile_err;

    push_stack(ctx, vm, OBJ_VAL(func));
    callf(ctx, func, 0, vm);

    return run(ctx, vm);
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

