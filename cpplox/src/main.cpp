#include <defs.hpp>

enum errc_t {
    e_ec_ok = 0,
    e_ec_arg_error = 1,
    e_ec_sys_error = 2,
    e_ec_code_error = 13,
};

struct lox_t {
    bool had_error = false;
};

struct nil_t {};

// @WIP
class LoxObject {
    variant<nil_t, string, double> m_val{nil_t{}};

public:
    LoxObject() = default;

    LoxObject(const LoxObject &) = default;
    LoxObject(LoxObject &&) = default;
    LoxObject &operator=(const LoxObject &) = default;
    LoxObject &operator=(LoxObject &&) = default;

    LoxObject(const string &s) : m_val{s} {}
    LoxObject(string &&s) : m_val{move(s)} {}
    LoxObject &operator=(const string &s) {
        m_val = s;
        return *this;
    }
    LoxObject &operator=(string &&s) {
        m_val = move(s);
        return *this;
    }
    LoxObject(double d) : m_val{d} {}
    LoxObject &operator=(double d) {
        m_val = d;
        return *this;
    }

    bool IsNil() const { return holds_alternative<nil_t>(m_val); }
    bool IsString() const { return holds_alternative<string>(m_val); }
    bool IsNumber() const { return holds_alternative<double>(m_val); }

    string &GetString() {
        assert(IsString());
        return get<string>(m_val);
    }
    const string &GetString() const
        { return const_cast<LoxObject *>(this)->GetString(); }
    double &GetNumber() {
        assert(IsNumber());
        return get<double>(m_val);
    }
    const double &GetNumber() const
        { return const_cast<LoxObject *>(this)->GetNumber(); }
};

void report(lox_t &lox, int line, string_view location, string_view message)
{
    println(stderr, "[line {}] Error {}: {}", line, location, message);
    lox.had_error = true;
}

void error(lox_t &lox, int line, string_view message)
{
    report(lox, line, "", message);
}

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

static constexpr string_view c_tt_debug_names[] =
{
    "EOF", "LEFT-PAREN", "RIGHT-PAREN", "LEFT-BRACE", "RIGHT-BRACE",
    "COMMA", "DOT", "MINUS", "PLUS", "SEMICOLON", "SLASH", "STAR",
    "BANG", "BANG-EQUAL", "EQUAL", "EQUAL-EQUAL", "GREATER", "GREATER-EQUAL",
    "LESS", "LESS-EQUAL", "IDENTIFIER", "STRING", "NUMBER",
    "AND", "CLASS", "ELSE", "FALSE", "FUN", "FOR", "IF", "NIL", "OR",
    "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE"
};

static const unordered_map<string_view, token_type_t> c_reserved_id_types{
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

string to_string(token_t tok)
{
    return format(
        "<{}> ({}) : line {}",
        tok.lexeme, c_tt_debug_names[tok.type], tok.line);
}

struct scanner_t {
    string_view source;
    vector<token_t> tokens{};
    usize start = 0, current = 0, line = 0;
    lox_t *lox;
};

bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

bool is_alpha(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}

bool is_alphanumeric(char c)
{
    return is_alpha(c) || is_digit(c);
}

bool done(const scanner_t &scanner)
{
    return scanner.current >= scanner.source.size();
}

char advance(scanner_t &scanner)
{
    return scanner.source[scanner.current++];
}

bool match(scanner_t &scanner, char expected)
{
    if (done(scanner) || scanner.source[scanner.current] != expected)
        return false;

    advance(scanner);
    return true;
}

int match_n(scanner_t &scanner, string_view cs)
{
    int count = 0;
    for (char c : cs) {
        if (!match(scanner, c))
            break;
        ++count;
    }
    return count;
}

char peek(const scanner_t &scanner)
{
    return done(scanner) ? '\0' : scanner.source[scanner.current];
}

char peek_next(const scanner_t &scanner)
{
    return scanner.current + 1 >= scanner.source.size() ?
        '\0' : scanner.source[scanner.current + 1];
}

string_view cur_lexeme(const scanner_t &scanner)
{
    return scanner.source.substr(
        scanner.start, scanner.current - scanner.start);
}

void add_token(scanner_t &scanner, token_type_t tt, LoxObject lit = {})
{
    scanner.tokens.push_back({tt, cur_lexeme(scanner), lit, scanner.line});
}

void skip_oneline_comment(scanner_t &scanner)
{
    while (peek(scanner) != '\n' && !done(scanner))
        advance(scanner);
}

void skip_multiline_comment(scanner_t &scanner)
{
    int balance = 1;

    while (balance > 0 && !done(scanner)) {
        if (int matches = match_n(scanner, "/*"); matches > 0) {
            if (matches == 2)
                ++balance;
            continue;
        }
        if (int matches = match_n(scanner, "*/"); matches > 0) {
            if (matches == 2)
                --balance;
            continue;
        }

        if (peek(scanner) == '\n')
            ++scanner.line;
        advance(scanner);
    }
}

void scan_string(scanner_t &scanner)
{
    while (peek(scanner) != '"' && !done(scanner)) {
        if (peek(scanner) == '\n')
            ++scanner.line;
        advance(scanner);
    }

    if (done(scanner)) {
        error(*scanner.lox, scanner.line, "Unterminated string.");
        return;
    }

    advance(scanner);

    string value{
        scanner.source.substr(
            scanner.start + 1, scanner.current - scanner.start - 2)
    };
    add_token(scanner, e_tt_string, move(value));
}

void scan_number(scanner_t &scanner)
{
    while (is_digit(peek(scanner)))
        advance(scanner);

    if (peek(scanner) == '.') {
        advance(scanner);
        while (is_digit(peek(scanner)))
            advance(scanner);
    }

    double value = stod(string{cur_lexeme(scanner)});
    add_token(scanner, e_tt_number, value);  
}

void scan_ident(scanner_t &scanner)
{
    while (is_alphanumeric(peek(scanner)))
        advance(scanner);

    token_type_t tt = e_tt_identifier;
    if (auto it = c_reserved_id_types.find(cur_lexeme(scanner));
        it != c_reserved_id_types.end())
    {
        tt = it->second;
    }

    add_token(scanner, tt);
}

void scan_one_token(scanner_t &scanner)
{
    char c = advance(scanner);
    switch (c) {
    case '(': add_token(scanner, e_tt_left_paren); break;
    case ')': add_token(scanner, e_tt_right_paren); break;
    case '{': add_token(scanner, e_tt_left_brace); break;
    case '}': add_token(scanner, e_tt_right_brace); break;
    case ',': add_token(scanner, e_tt_comma); break;
    case '.': add_token(scanner, e_tt_dot); break;
    case '-': add_token(scanner, e_tt_minus); break;
    case '+': add_token(scanner, e_tt_plus); break;
    case ';': add_token(scanner, e_tt_semicolon); break;
    case '*': add_token(scanner, e_tt_star); break;    

    case '!':
        add_token(
            scanner, match(scanner, '=') ? e_tt_bang_equal : e_tt_bang);
        break;
    case '=':
        add_token(
            scanner, match(scanner, '=') ? e_tt_equal_equal : e_tt_equal);
        break;
    case '<':
        add_token(
            scanner, match(scanner, '=') ? e_tt_less_equal : e_tt_less);
        break;
    case '>':
        add_token(
            scanner, match(scanner, '=') ? e_tt_greater_equal : e_tt_greater);
        break;

    case '/':
        if (match(scanner, '/'))
            skip_oneline_comment(scanner);
        else if (match(scanner, '*'))
            skip_multiline_comment(scanner);
        else
            add_token(scanner, e_tt_slash);

    case ' ':
    case '\r':
    case '\t':
        break;
    case '\n':
        ++scanner.line;
        break;

    case '"': scan_string(scanner); break;

    default:
        if (is_digit(c))
            scan_number(scanner);
        else if (is_alpha(c))
            scan_ident(scanner);
        else
            error(*scanner.lox, scanner.line, "Unexpected character.");
        break;
    }
}

vector<token_t> scan_tokens(string_view code, lox_t &lox)
{
    scanner_t scanner{.source = code, .lox = &lox};

    while (!done(scanner)) {
        scanner.start = scanner.current;
        scan_one_token(scanner);
    }

    scanner.tokens.push_back(
        {e_tt_eof, code.substr(code.size(), 0), {}, scanner.line + 1});

    return move(scanner.tokens);
}

errc_t run(string_view code, lox_t &lox)
{
    vector<token_t> tokens = scan_tokens(code, lox);
    if (lox.had_error)
        return e_ec_code_error;
    
    // @TEST
    for (auto const &tok : tokens)
        println("{}", to_string(tok));

    return e_ec_ok;
}

optional<string> read_whole_file(char const *fn)
{
    FILE *f = fopen(fn, "rb");
    if (!f) {
        perror(fn);
        return nullopt;
    }
    DEFER(fclose(f));

    optional<string> res;
    res.emplace();

    fseek(f, 0, SEEK_END);
    usize len = ftell(f);
    fseek(f, 0, SEEK_SET);

    res->resize(len);
    if (fread(res->data(), len, 1, f) != 1) {
        perror(fn);
        return nullopt;
    }
        
    return res;
}

errc_t run_file(char const *fn, lox_t &lox)
{
    if (auto bytes = read_whole_file(fn))
        return run(*bytes, lox);
    else
        return e_ec_sys_error;
}

errc_t run_prompt(lox_t &lox)
{
    for (;;) {
        print("> ");
        string line{};

        getline(cin, line);
        if (!cin.good())
            break;

        run(line, lox);
        lox.had_error = false;
    }

    if (!cin.eof()) {
        println(stderr, "Input error (unknown)");
        return e_ec_sys_error;
    }

    return e_ec_ok;
}

void show_header()
{
    println("jb-cpplox, version {}", VERSION);
}

int main(int argc, char **argv)
{
    span<char const *const> args{argv + 1, usize(argc - 1)};

    lox_t lox{};
    
    if (args.size() > 1) {
        show_header();
        println(stderr, "Usage: cpplox [file]");
        return e_ec_arg_error;
    } else if (args.size() == 1) {
        return run_file(args[0], lox);
    } else {
        show_header();
        return run_prompt(lox);
    }
}
