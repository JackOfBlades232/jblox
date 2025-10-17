#pragma once

#include "defs.hpp"
#include "value.hpp"

enum token_type_t {
    e_tt_eof = 0,

    e_tt_left_paren,
    e_tt_right_paren,
    e_tt_left_brace,
    e_tt_right_brace,

    e_tt_comma,
    e_tt_question,
    e_tt_colon,
    e_tt_dot,
    e_tt_minus,
    e_tt_plus,
    e_tt_semicolon,
    e_tt_slash,
    e_tt_star,
    e_tt_bang,
    e_tt_bang_equal,

    e_tt_equal,
    e_tt_equal_equal,

    e_tt_greater,
    e_tt_greater_equal,

    e_tt_less,
    e_tt_less_equal,

    e_tt_identifier,
    e_tt_string,
    e_tt_number,

    e_tt_and,
    e_tt_class,
    e_tt_mixin,
    e_tt_else,
    e_tt_false,
    e_tt_fun,
    e_tt_for,
    e_tt_if,
    e_tt_nil,
    e_tt_or,
    e_tt_with,

    e_tt_print,
    e_tt_return,
    e_tt_super,
    e_tt_inner,
    e_tt_this,
    e_tt_true,
    e_tt_var,
    e_tt_while,
    e_tt_break,
};

inline constexpr string_view c_tt_debug_names[] =
{
    "EOF", "LEFT-PAREN", "RIGHT-PAREN", "LEFT-BRACE", "RIGHT-BRACE",
    "COMMA", "QUESTION MARK", "COLON", "DOT", "MINUS", "PLUS", "SEMICOLON",
    "SLASH", "STAR", "BANG", "BANG-EQUAL", "EQUAL", "EQUAL-EQUAL",
    "GREATER", "GREATER-EQUAL", "LESS", "LESS-EQUAL",
    "IDENTIFIER", "STRING", "NUMBER", "AND", "CLASS", "MIXIN", "ELSE", "FALSE",
    "FUN", "FOR", "IF", "NIL", "OR", "WITH", "PRINT", "RETURN",
    "SUPER", "INNER", "THIS", "TRUE", "VAR", "WHILE", "BREAK",
};

inline const unordered_map<string_view, token_type_t> c_reserved_id_types{
    {"and", e_tt_and},
    {"class", e_tt_class},
    {"mixin", e_tt_mixin},
    {"else", e_tt_else},
    {"false", e_tt_false},
    {"for", e_tt_for},
    {"fun", e_tt_fun},
    {"if", e_tt_if},
    {"nil", e_tt_nil},
    {"or", e_tt_or},
    {"with", e_tt_with},
    {"print", e_tt_print},
    {"return", e_tt_return},
    {"super", e_tt_super},
    {"inner", e_tt_inner},
    {"this", e_tt_this},
    {"true", e_tt_true},
    {"var", e_tt_var},
    {"while", e_tt_while},
    {"break", e_tt_break},
};

inline bool is_unary(token_type_t type)
{
    switch (type) {
    case e_tt_minus:
    case e_tt_bang:
        return true;

    default:
        return false;
    }
}

inline bool is_binary(token_type_t type)
{
    switch (type) {
    case e_tt_comma:
    case e_tt_minus:
    case e_tt_plus:
    case e_tt_slash:
    case e_tt_star:
    case e_tt_bang:
    case e_tt_bang_equal:
    case e_tt_equal:
    case e_tt_equal_equal:
    case e_tt_greater:
    case e_tt_greater_equal:
    case e_tt_less:
    case e_tt_less_equal:
    case e_tt_and:
    case e_tt_or:
    case e_tt_with:
        return true;

    default:
        return false;
    }
}

struct token_t {
    token_type_t type;
    string_view lexeme;
    LoxValue literal;
    usize line;
};

inline string to_string(token_t tok)
{
    return format(
        "<{}> ({}) : line {}",
        tok.lexeme, c_tt_debug_names[tok.type], tok.line);
}

inline bool operator==(token_t t1, token_t t2) {
    if (t1.type == t2.type &&
        t1.lexeme.data() == t2.lexeme.data() &&
        t1.lexeme.size() == t2.lexeme.size())
    {
        // assert(t1.literal == t2.literal);
        assert(t1.line == t2.line);
        return true;
    }

    return false;
}

namespace std
{
template <>
struct hash<token_t> {
    usize operator()(token_t tok) const {
        size_t seed = 0;
        hash_combine(seed, int(tok.type));
        hash_combine(seed, u64(tok.lexeme.data()));
        hash_combine(seed, u64(tok.lexeme.size()));
        return seed;
    }
};
}

inline const token_t c_implicit_this_tok =
    token_t{e_tt_this, "this", c_nil, usize(-1)};
inline const token_t c_implicit_super_tok =
    token_t{e_tt_super, "super", c_nil, usize(-1)};
inline const token_t c_implicit_inner_tok =
    token_t{e_tt_inner, "inner", c_nil, usize(-1)};
