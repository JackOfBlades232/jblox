#include <lox.hpp>
#include <tokens.hpp>
#include <ast.hpp>

enum errc_t {
    e_ec_ok = 0,
    e_ec_arg_error = 1,
    e_ec_sys_error = 2,
    e_ec_code_error = 13,
};

class AstPrinter : public IVisitor {
    string accum{};

public:
    string Print(Expr const &expr) {
        accum.clear();
        expr.Accept(*this);
        return move(accum);
    }

    void VisitUnaryExpr(const UnaryExpr &unary) override {
        Parenthesize(unary.op.lexeme, {unary.right});
    }
    void VisitGroupingExpr(const GroupingExpr &grouping) override {
        Parenthesize("group", {grouping.expr});
    }
    void VisitLiteralExpr(const LiteralExpr &literal) override {
        accum.append(to_string(literal.value));
    }
    void VisitBinaryExpr(const BinaryExpr &binary) override {
        Parenthesize(binary.op.lexeme, {binary.left, binary.right});
    }

private:
    void Parenthesize(string_view name, initializer_list<Expr const *> exprs) {
        accum.append("(");
        accum.append(name);
        for (Expr const *expr : exprs) {
            accum.append(" ");
            expr->Accept(*this);
        }
        accum.append(")");
    }
};

class RpnAstPrinter : public IVisitor {
    string accum{};

public:
    string Print(Expr const &expr) {
        accum.clear();
        expr.Accept(*this);
        return move(accum);
    }

    void VisitUnaryExpr(const UnaryExpr &unary) override {
        LiteralExpr zero{0.0};
        Output({&zero, unary.right}, unary.op.lexeme);
    }
    void VisitGroupingExpr(const GroupingExpr &grouping) override {
        grouping.expr->Accept(*this);
    }
    void VisitLiteralExpr(const LiteralExpr &literal) override {
        accum.append(to_string(literal.value));
    }
    void VisitBinaryExpr(const BinaryExpr &binary) override {
        Output({binary.left, binary.right}, binary.op.lexeme);
    }

private:
    void Output(initializer_list<Expr const *> exprs, string_view op) {
        assert(exprs.size());
        if (!accum.empty())
            accum.append(" ");
        (*exprs.begin())->Accept(*this);
        for (Expr const *expr : span{exprs}.subspan(1)) {
            accum.append(" ");
            expr->Accept(*this);
        }
        accum.append(" ");
        accum.append(op);
    }
};

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

    // @TEST
    Expr const *root = new BinaryExpr{
        new UnaryExpr{
            token_t{e_tt_minus, "-", c_nil, 1},
            new LiteralExpr{123.0}
        },
        token_t{e_tt_star, "*", c_nil, 1},
        new GroupingExpr{new LiteralExpr{45.67}}
    };
    DEFER(delete root);
    println("Example AST:\n{}", AstPrinter{}.Print(*root));
    println("Example AST in rpn:\n{}", RpnAstPrinter{}.Print(*root));
    
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
