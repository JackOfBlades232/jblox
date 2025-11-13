#pragma once

#include "defs.h"

typedef struct {
    char *p;
    u64 len;
} string_t;

static inline b32 string_is_valid(string_t const *s)
{
    return s->p ? true : false;
}

static inline b32 string_is_empty(string_t const *s)
{
    return (s->p && s->len > 0) ? false : true;
}

#define LITSTR(litcstr_) ((string_t){(litcstr_), STRLITLEN(litcstr_)})

static inline string_t string_from_cstr(char *cstr)
{
    string_t res = {cstr, 0};
    if (cstr) {
        while (*cstr++)
            ++res.len;
    }
    return res;
}

static inline b32 string_eq(string_t s1, string_t s2)
{
    if (s1.len != s2.len)
        return false;
    while (s1.len--) {
        if (*s1.p++ != *s2.p++)
            return false;
    }
    return true;
}
