#pragma once

#include "defs.hpp"
#include "lox.hpp"

enum token_type_t {
    e_tt_eof = 0,

    e_tt_left_paren,
    e_tt_right_paren,
    e_tt_left_brace,
    e_tt_right_brace,

    e_tt_comma,
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
    e_tt_else,
    e_tt_false,
    e_tt_fun,
    e_tt_for,
    e_tt_if,
    e_tt_nil,
    e_tt_or,

    e_tt_print,
    e_tt_return,
    e_tt_super,
    e_tt_this,
    e_tt_true,
    e_tt_var,
    e_tt_while
};

inline constexpr string_view c_tt_debug_names[] =
{
    "EOF", "LEFT-PAREN", "RIGHT-PAREN", "LEFT-BRACE", "RIGHT-BRACE",
    "COMMA", "DOT", "MINUS", "PLUS", "SEMICOLON", "SLASH", "STAR",
    "BANG", "BANG-EQUAL", "EQUAL", "EQUAL-EQUAL", "GREATER", "GREATER-EQUAL",
    "LESS", "LESS-EQUAL", "IDENTIFIER", "STRING", "NUMBER",
    "AND", "CLASS", "ELSE", "FALSE", "FUN", "FOR", "IF", "NIL", "OR",
    "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE"
};

inline const unordered_map<string_view, token_type_t> c_reserved_id_types{
    {"and", e_tt_and},
    {"class", e_tt_class},
    {"else", e_tt_else},
    {"false", e_tt_false},
    {"for", e_tt_for},
    {"fun", e_tt_fun},
    {"if", e_tt_if},
    {"nil", e_tt_nil},
    {"or", e_tt_or},
    {"print", e_tt_print},
    {"return", e_tt_return},
    {"super", e_tt_super},
    {"this", e_tt_this},
    {"true", e_tt_true},
    {"var", e_tt_var},
    {"while", e_tt_while}
};

struct token_t {
    token_type_t type;
    string_view lexeme;
    LoxObject literal;
    usize line;
};

inline string to_string(token_t tok)
{
    return format(
        "<{}> ({}) : line {}",
        tok.lexeme, c_tt_debug_names[tok.type], tok.line);
}

inline void error(lox_t &lox, token_t tok, string_view message)
{
    if (tok.type == e_tt_eof) {
        report(lox, tok.line, " at end", message);
    } else {
        report(
            lox, tok.line,
            string_view{string{"at '"} + string{tok.lexeme} + string{"'"}},
            message);
    }
}
