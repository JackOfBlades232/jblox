#include <lox.hpp>
#include <value.hpp>
#include <tokens.hpp>
#include <ast.hpp>
#include <semantic.hpp>

enum errc_t {
    e_ec_ok = 0,
    e_ec_arg_error = 1,
    e_ec_sys_error = 2,
    e_ec_parsing_error = 13,
    e_ec_semantic_error = 69,
    e_ec_runtime_error = 4221,
};

struct scanner_t {
    string_view source;
    vector<token_t> tokens{};
    usize start = 0, current = 0, line = 1;
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

bool done(scanner_t const &scanner)
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

char peek(scanner_t const &scanner)
{
    return done(scanner) ? '\0' : scanner.source[scanner.current];
}

char peek_next(scanner_t const &scanner)
{
    return scanner.current + 1 >= scanner.source.size() ?
        '\0' : scanner.source[scanner.current + 1];
}

string_view cur_lexeme(scanner_t const &scanner)
{
    return scanner.source.substr(
        scanner.start, scanner.current - scanner.start);
}

void add_token(scanner_t &scanner, token_type_t tt, LoxValue lit = {})
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

    f64 value = stod(string{cur_lexeme(scanner)});
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
    case ':': add_token(scanner, e_tt_colon); break;
    case '?': add_token(scanner, e_tt_question); break;
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

    case '|':
        if (match(scanner, '|'))
            add_token(scanner, e_tt_or);
        break;
    case '&':
        if (match(scanner, '&'))
            add_token(scanner, e_tt_and);
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

struct parser_t {
    span<token_t const> tokens;
    usize current = 0;
    bool in_loop = false;
    lox_t *lox;
};

struct parse_exception_t {};

parse_exception_t error(parser_t &parser, token_t tok, string_view message)
{
    error(*parser.lox, tok, message);
    return parse_exception_t{};
}

token_t peek(parser_t const &parser)
{
    return parser.current < parser.tokens.size() ?
        parser.tokens[parser.current] :
        token_t{e_tt_eof};
}

token_t previous(parser_t const &parser)
{
    assert(parser.current > 0);
    return parser.tokens[parser.current - 1];
}

token_t advance(parser_t &parser)
{
    if (parser.current < parser.tokens.size())
        ++parser.current;
    return previous(parser);
}

bool check(parser_t const &parser, token_type_t type)
{
    if (parser.current >= parser.tokens.size())
        return false;
    return peek(parser).type == type;
}

bool match(parser_t &parser, token_type_t type)
{
    if (check(parser, type)) {
        advance(parser);
        return true;
    }
    return false;
}

bool done(parser_t const &parser)
{
    return peek(parser).type == e_tt_eof;
}

bool match(parser_t &parser, initializer_list<token_type_t> token_types)
{
    for (token_type_t type : token_types) {
        if (check(parser, type)) {
            advance(parser);
            return true;
        }
    }

    return false;
}

bool check_n(parser_t const &parser, initializer_list<token_type_t> token_types)
{
    for (usize i = parser.current; token_type_t type : token_types) {
        if (i >= parser.tokens.size() || parser.tokens[i].type != type)
            return false;
        ++i;
    }

    return true;
}

token_t consume(parser_t &parser, token_type_t type, string_view message)
{
    if (check(parser, type))
        return advance(parser);

    throw error(parser, peek(parser), message);
}

expr_ptr_t parse_expr(parser_t &parser);
expr_ptr_t parse_assignment(parser_t &parser);
stmt_ptr_t parse_block(parser_t &parser);

expr_ptr_t parse_functional(parser_t &parser)
{
    consume(
        parser, e_tt_left_paren, "Expected '(' after func/method name.");
    
    vector<token_t> params{};
    if (!check(parser, e_tt_right_paren)) {
        do {
            if (params.size() >= 255) {
                error(parser, peek(parser),
                    "Can't define a func/method with more than 255 params.");
            }

            params.push_back(
                consume(parser, e_tt_identifier, "Expected parameter name."));
        } while (match(parser, e_tt_comma));
    }

    consume(
        parser, e_tt_right_paren, "Expected ')' after parameter list.");

    consume(
        parser, e_tt_left_brace, "Expected '{' before func/method body.");
    auto body =
        move(static_cast<BlockStmt *>(parse_block(parser).get())->stmts);

    return make_shared<FunctionalExpr>(move(params), move(body));
}

expr_ptr_t parse_primary(parser_t &parser)
{
    if (match(parser, e_tt_false))
        return make_shared<LiteralExpr>(false);
    if (match(parser, e_tt_true))
        return make_shared<LiteralExpr>(true);
    if (match(parser, e_tt_nil))
        return make_shared<LiteralExpr>(c_nil);
    if (match(parser, {e_tt_number, e_tt_string}))
        return make_shared<LiteralExpr>(previous(parser).literal);
    if (match(parser, e_tt_identifier))
        return make_shared<VariableExpr>(previous(parser));
    if (match(parser, e_tt_fun))
        return parse_functional(parser);

    if (match(parser, e_tt_left_paren)) {
        expr_ptr_t expr = parse_expr(parser);
        consume(parser, e_tt_right_paren, "Expected ')' after expression.");
        return make_shared<GroupingExpr>(expr);
    }

    throw error(parser, peek(parser), "Expected expression.");
}

expr_ptr_t finish_parsing_call(parser_t &parser, expr_ptr_t callee)
{
    vector<expr_ptr_t> args{};
    if (!check(parser, e_tt_right_paren)) {
        do {
            // Disallow >255 args, as that will be a hard limit for clox,
            // and we want conforming implementations
            if (args.size() >= 255) {
                error(*parser.lox, peek(parser),
                    "Can't pass more than 255 args to a call.");
            }

            // Not seq, as commas as operators are not accepted here
            args.push_back(parse_assignment(parser));
        } while (match(parser, e_tt_comma));
    }

    token_t paren = consume(
        parser, e_tt_right_paren, "Expected ')' after argument list.");

    return make_shared<CallExpr>(callee, paren, move(args));
}

expr_ptr_t parse_call(parser_t &parser)
{
    expr_ptr_t expr = parse_primary(parser);

    for (;;) {
        if (match(parser, e_tt_left_paren))
            expr = finish_parsing_call(parser, expr);
        else
            break;
    }

    return expr;
}

expr_ptr_t parse_unary(parser_t &parser)
{
    if (match(parser, {e_tt_bang, e_tt_minus})) {
        token_t op = previous(parser);
        expr_ptr_t right = parse_unary(parser);
        return make_shared<UnaryExpr>(op, right);
    }

    return parse_call(parser);
}

template <auto t_child_parser, token_type_t ...t_allowed_ops>
expr_ptr_t parse_left_associative_chain(parser_t &parser)
{
    while (!is_unary(peek(parser).type) && is_binary(peek(parser).type)) {
        error(*parser.lox, peek(parser), "Leading binary operator.");
        advance(parser);
    }

    expr_ptr_t expr = t_child_parser(parser);
    
    while (match(parser, {t_allowed_ops...})) {
        token_t op = previous(parser);
        expr_ptr_t right = t_child_parser(parser);
        expr = make_shared<BinaryExpr>(expr, op, right);
    }

    return parser.lox->had_error ? nullptr : expr ;
}

expr_ptr_t parse_factor(parser_t &parser)
{
    return parse_left_associative_chain<
            parse_unary, e_tt_star, e_tt_slash
        >(parser);
}

expr_ptr_t parse_term(parser_t &parser)
{
    return parse_left_associative_chain<
            parse_factor, e_tt_plus, e_tt_minus
        >(parser);
}

expr_ptr_t parse_comparison(parser_t &parser)
{
    return parse_left_associative_chain<
            parse_term,
            e_tt_greater, e_tt_greater_equal, e_tt_less, e_tt_less_equal
        >(parser);
}

expr_ptr_t parse_equality(parser_t &parser)
{
    return parse_left_associative_chain<
            parse_comparison, e_tt_bang_equal, e_tt_equal_equal
        >(parser);
}

expr_ptr_t parse_conjunction(parser_t &parser)
{
    return parse_left_associative_chain<parse_equality, e_tt_and>(parser);
}

expr_ptr_t parse_disjunction(parser_t &parser)
{
    return parse_left_associative_chain<parse_conjunction, e_tt_or>(parser);
}

expr_ptr_t parse_choice(parser_t &parser)
{
    expr_ptr_t expr = parse_disjunction(parser);
    
    if (match(parser, e_tt_question)) {
        token_t op0 = previous(parser);
        expr_ptr_t second = parse_choice(parser);
        consume(parser, e_tt_colon, "Expected ':' in ternary operator.");
        token_t op1 = previous(parser);
        expr_ptr_t third = parse_choice(parser);
        expr = make_shared<TernaryExpr>(expr, op0, second, op1, third);
    }

    return expr;
}

expr_ptr_t parse_assignment(parser_t &parser)
{
    expr_ptr_t expr = parse_choice(parser);
    
    if (match(parser, e_tt_equal)) {
        token_t eq = previous(parser);
        expr_ptr_t value = parse_assignment(parser);

        // @TODO: is_lvalue or smth here, requires more robust AssignmentExpr
        if (auto *variable = dynamic_cast<VariableExpr *>(expr.get()))
            return make_shared<AssignmentExpr>(variable->id, value);

        error(parser, eq, "Invalid assignment target.");
    }

    return expr;
}

expr_ptr_t parse_sequence(parser_t &parser)
{
    return parse_left_associative_chain<parse_assignment, e_tt_comma>(parser);
}

expr_ptr_t parse_expr(parser_t &parser)
{
    return parse_sequence(parser);
}

stmt_ptr_t parse_decl(parser_t &parser);
stmt_ptr_t parse_stmt(parser_t &parser);
stmt_ptr_t parse_var_decl(parser_t &parser);

stmt_ptr_t parse_expression_stmt(parser_t &parser)
{
    expr_ptr_t expr = parse_expr(parser);
    if (parser.lox->config.is_repl && done(parser))
        return make_shared<ReplExprStmt>(expr);

    consume(parser, e_tt_semicolon, "Expected ';' after expression.");
    return make_shared<ExpressionStmt>(expr);
}

stmt_ptr_t parse_for_stmt(parser_t &parser)
{
    consume(parser, e_tt_left_paren, "Expected '(' after for.");

    stmt_ptr_t init = {};
    if (match(parser, e_tt_semicolon))
        ;
    else if (match(parser, e_tt_var))
        init = parse_var_decl(parser);
    else
        init = parse_expression_stmt(parser);

    expr_ptr_t cond = {};
    if (!check(parser, e_tt_semicolon))
        cond = parse_expr(parser);
    consume(parser, e_tt_semicolon, "Expected ';' after loop condition");

    expr_ptr_t increment = {};
    if (!check(parser, e_tt_right_paren))
        increment = parse_expr(parser);

    consume(parser, e_tt_right_paren, "Expected ')' after for clause.");

    bool const was_in_loop = exchange(parser.in_loop, true);
    stmt_ptr_t body = parse_stmt(parser);
    parser.in_loop = was_in_loop;

    // Now, desugar into { init; while (cond) { body; increment; } }

    if (increment) {
        body = make_shared<BlockStmt>(
            vector<stmt_ptr_t>{body, make_shared<ExpressionStmt>(increment)});
    }

    if (!cond)
        cond = make_shared<LiteralExpr>(true);

    body = make_shared<WhileStmt>(cond, body);

    if (init)
        body = make_shared<BlockStmt>(vector<stmt_ptr_t>{init, body});

    return body;
}

stmt_ptr_t parse_if_stmt(parser_t &parser)
{
    consume(parser, e_tt_left_paren, "Expected '(' after if.");
    expr_ptr_t cond = parse_expr(parser);
    consume(parser, e_tt_right_paren, "Expected ')' after if condition.");

    stmt_ptr_t then_branch = parse_stmt(parser);
    stmt_ptr_t else_branch = {};
    if (match(parser, e_tt_else))
        else_branch = parse_stmt(parser);

    return make_shared<IfStmt>(cond, then_branch, else_branch);
}

stmt_ptr_t parse_while_stmt(parser_t &parser)
{
    consume(parser, e_tt_left_paren, "Expected '(' after while.");
    expr_ptr_t cond = parse_expr(parser);
    consume(parser, e_tt_right_paren, "Expected ')' after while condition.");

    bool const was_in_loop = exchange(parser.in_loop, true);
    stmt_ptr_t body = parse_stmt(parser);
    parser.in_loop = was_in_loop;

    return make_shared<WhileStmt>(cond, body);
}

stmt_ptr_t parse_print_stmt(parser_t &parser)
{
    expr_ptr_t val = parse_expr(parser);
    consume(parser, e_tt_semicolon, "Expected ';' after value.");
    return make_shared<PrintStmt>(val);
}

stmt_ptr_t parse_return_stmt(parser_t &parser)
{
    token_t kw = previous(parser);
    expr_ptr_t val{};
    if (!check(parser, e_tt_semicolon))
        val = parse_expr(parser);
    
    consume(parser, e_tt_semicolon, "Expected ';' after return value.");
    return make_shared<ReturnStmt>(kw, val);
}

stmt_ptr_t parse_block(parser_t &parser)
{
    vector<stmt_ptr_t> stmts{};

    while (!done(parser) && !check(parser, e_tt_right_brace))
        stmts.push_back(parse_decl(parser));

    consume(parser, e_tt_right_brace, "Expected '}' after block.");
    return make_shared<BlockStmt>(move(stmts));
}

stmt_ptr_t parse_break(parser_t &parser)
{
    token_t kw = previous(parser);
    if (!parser.in_loop)
        error(parser, kw, "Encountered 'break' outside a loop.");
    consume(parser, e_tt_semicolon, "Expected ';' after break.");
    return make_shared<BreakStmt>(kw);
}

stmt_ptr_t parse_stmt(parser_t &parser)
{
    if (match(parser, e_tt_for))
        return parse_for_stmt(parser);
    if (match(parser, e_tt_if))
        return parse_if_stmt(parser);
    if (match(parser, e_tt_while))
        return parse_while_stmt(parser);
    if (match(parser, e_tt_print))
        return parse_print_stmt(parser);
    if (match(parser, e_tt_return))
        return parse_return_stmt(parser);
    if (match(parser, e_tt_left_brace))
        return parse_block(parser);
    if (match(parser, e_tt_break))
        return parse_break(parser);

    return parse_expression_stmt(parser);
}

stmt_ptr_t parse_func_decl(parser_t &parser)
{
    token_t name = consume(
         parser, e_tt_identifier, "Expected func/method name.");
    expr_ptr_t func = parse_functional(parser);
    return make_shared<FuncDeclStmt>(
        name, move(*dynamic_cast<FunctionalExpr *>(func.get())));
}

stmt_ptr_t parse_var_decl(parser_t &parser)
{
    token_t const id =
        consume(parser, e_tt_identifier, "Expected variable name.");

    expr_ptr_t init{};
    if (match(parser, e_tt_equal))
        init = parse_expr(parser);

    consume(parser, e_tt_semicolon, "Expected ';' after var declaration.");
    return make_shared<VarStmt>(id, init);
}

void sync_parser(parser_t &parser)
{
    advance(parser);

    // Sync point is stmt boundry
    while (!done(parser)) {
        if (previous(parser).type == e_tt_semicolon)
            break;

        switch (peek(parser).type) {
        case e_tt_class:
        case e_tt_fun:
        case e_tt_var:
        case e_tt_for:
        case e_tt_if:
        case e_tt_while:
        case e_tt_print:
            return;

        default:
            break;
        }

        advance(parser);
    }
}

stmt_ptr_t parse_decl(parser_t &parser)
{
    try {
        if (check_n(parser, {e_tt_fun, e_tt_identifier})) {
            advance(parser);
            return parse_func_decl(parser);
        }
        if (match(parser, e_tt_var))
            return parse_var_decl(parser);

        return parse_stmt(parser);
    } catch (parse_exception_t) {
        sync_parser(parser);
        return {};
    }
}

vector<stmt_ptr_t> parse(span<token_t const> tokens, lox_t &lox)
{
    parser_t parser{.tokens = tokens, .lox = &lox};
    vector<stmt_ptr_t> stmts{};

    while (!done(parser))
        stmts.push_back(parse_decl(parser));

    return stmts;
}

class AstPrinter : public IExprVisitor, public IStmtVisitor {
    string m_accum{};

public:
    string Print(Expr const &expr) {
        m_accum.clear();
        expr.Accept(*this);
        return move(m_accum);
    }
    string Print(Stmt const &stmt) {
        m_accum.clear();
        stmt.Accept(*this);
        return move(m_accum);
    }

    void VisitAssignmentExpr(AssignmentExpr const &assignment) override {
        m_accum.append("(assign ");
        m_accum.append(assignment.target.lexeme);
        m_accum.append(" ");
        assignment.val->Accept(*this);
        m_accum.append(")");
    }
    void VisitUnaryExpr(UnaryExpr const &unary) override
        { Parenthesize(unary.op.lexeme, {unary.right}); }
    void VisitGroupingExpr(GroupingExpr const &grouping) override
        { Parenthesize("group", {grouping.expr}); }
    void VisitLiteralExpr(LiteralExpr const &literal) override
        { m_accum.append(to_dbg_string(literal.value)); }
    void VisitBinaryExpr(BinaryExpr const &binary) override
        { Parenthesize(binary.op.lexeme, {binary.left, binary.right}); }
    void VisitTernaryExpr(TernaryExpr const &ternary) override {
        m_accum.append("(");
        ternary.first->Accept(*this);
        m_accum.append(" ");
        m_accum.append(ternary.op0.lexeme);
        m_accum.append(" ");
        ternary.second->Accept(*this);
        m_accum.append(" ");
        m_accum.append(ternary.op1.lexeme);
        m_accum.append(" ");
        ternary.third->Accept(*this);
        m_accum.append(")");
    }
    void VisitCallExpr(CallExpr const &call) override {
        m_accum.append("(call ");
        call.callee->Accept(*this);
        m_accum.append(" (");
        bool comma = false;
        for (auto const &arg : call.args) {
            if (exchange(comma, true))
                m_accum.append(", ");
            arg->Accept(*this);
        }
        m_accum.append("))");
    }
    void VisitVariableExpr(VariableExpr const &variable) override
        { m_accum.append(variable.id.lexeme); }
    void VisitFunctionalExpr(FunctionalExpr const &func) override {
        m_accum.append("(lambda ");
        m_accum.append(" (");
        bool comma = false;
        for (auto const &param : func.params) {
            if (exchange(comma, true))
                m_accum.append(", ");
            m_accum.append(param.lexeme);
        }
        m_accum.append(") {\n");
        for (auto const &stmt : func.body)
            stmt->Accept(*this);
        m_accum.append("})");
    }

    void VisitBlockStmt(BlockStmt const &block) override {
        m_accum.append("{\n");
        for (auto const &stmt : block.stmts)
            stmt->Accept(*this);
        m_accum.append("}\n");
    }
    void VisitExpressionStmt(ExpressionStmt const &expression) override {
        expression.expr->Accept(*this);
        m_accum.append(";\n");
    }
    void VisitFuncDeclStmt(FuncDeclStmt const &func_decl) override {
        m_accum.append("declare func/method ");
        m_accum.append(func_decl.name.lexeme);
        m_accum.append(" (");
        bool comma = false;
        for (auto const &param : func_decl.func.params) {
            if (exchange(comma, true))
                m_accum.append(", ");
            m_accum.append(param.lexeme);
        }
        m_accum.append(")\n{\n");
        for (auto const &stmt : func_decl.func.body)
            stmt->Accept(*this);
        m_accum.append("}\n");
    }
    void VisitIfStmt(IfStmt const &if_stmt) override {
        m_accum.append("if (");
        if_stmt.cond->Accept(*this);
        m_accum.append(");\n");
        if_stmt.then_branch->Accept(*this);
        if (if_stmt.else_branch) {
            m_accum.append("else\n");
            if_stmt.else_branch->Accept(*this);
        }
        m_accum.append("endif\n");
    }
    void VisitWhileStmt(WhileStmt const &while_stmt) override {
        m_accum.append("while (");
        while_stmt.cond->Accept(*this);
        m_accum.append(");\n");
        while_stmt.body->Accept(*this);
        m_accum.append("endwhile\n");
    }
    void VisitPrintStmt(PrintStmt const &print) override {
        m_accum.append("print (");
        print.val->Accept(*this);
        m_accum.append(");\n");
    }
    void VisitReturnStmt(ReturnStmt const &ret) override {
        m_accum.append("return ");
        ret.value->Accept(*this);
        m_accum.append(";\n");
    }
    void VisitBreakStmt(BreakStmt const &) override
        { m_accum.append("break;\n"); }
    void VisitVarStmt(VarStmt const &var) override {
        m_accum.append("var decl ");
        m_accum.append(var.id.lexeme);
        if (var.init) {
            m_accum.append(" := (");
            var.init->Accept(*this);
            m_accum.append(")");
        }
        m_accum.append(";\n");
    }
    void VisitReplExprStmt(ReplExprStmt const &e) override {
        e.expr->Accept(*this);
        m_accum.append(" [eval]\n");
    }

private:
    void Parenthesize(string_view name, initializer_list<expr_ptr_t> exprs) {
        m_accum.append("(");
        m_accum.append(name);
        for (expr_ptr_t const &expr : exprs) {
            m_accum.append(" ");
            expr->Accept(*this);
        }
        m_accum.append(")");
    }
};

void interpret(span<stmt_ptr_t const> stmts, lox_t &lox)
{
    try {
        for (auto const &stmt : stmts)
            lox.interp.Execute(*stmt);
    } catch (runtime_error_t err) {
        rt_error(lox, err);
    }
}

errc_t run(string_view code, lox_t &lox)
{
    vector<token_t> const tokens = scan_tokens(code, lox);
    if (lox.had_error)
        return e_ec_parsing_error;
    else if (tokens.size() <= 1) // Only eof
        return e_ec_ok;

    if (lox.config.print_tokens) {
        for (auto const &tok : tokens)
            println("{}", to_string(tok));
    }

    vector<stmt_ptr_t> const stmts = parse(tokens, lox);
    if (lox.had_error)
        return e_ec_parsing_error;

    if (lox.config.print_ast) {
        println("Ast:");
        for (auto const &stmt : stmts)
            print("{}", AstPrinter{}.Print(*stmt));
    }

    Resolver{&lox.interp, &lox}.Resolve(stmts);
    if (lox.had_error)
        return e_ec_semantic_error;

    interpret(stmts, lox);
    if (lox.had_runtime_error)
        return e_ec_runtime_error;

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
    println(stderr, "jb-cpplox, version {}", VERSION);
}

void show_usage()
{
    show_header();
    println(stderr, "Usage: cpplox [file|args]...");
    println(stderr, "Args:");
    println(stderr, "    --print-tokens : print tokenizer output.");
    println(stderr, "    --print-ast : print parser output in infix notation.");
    println(stderr, "    --help/-h : print this message and exit.");
    
}

int main(int argc, char **argv)
{
    span<char const *const> args{argv + 1, usize(argc - 1)};

    lox_t lox{};

    char const *fn = nullptr;

    for (char const *const a : args) {
        string_view arg{a};
        if (arg == "-h" || arg == "--help") {
            show_usage();
            return e_ec_ok;
        } else if (arg == "--print-tokens") {
            lox.config.print_tokens = true;
        } else if (arg == "--print-ast") {
            lox.config.print_ast = true;
        } else if (!fn) {
            fn = a;
        } else {
            show_usage();
            return e_ec_arg_error;
        }
    }
    
    if (fn) {
        return run_file(fn, lox);
    } else {
        lox.config.is_repl = true;
        show_header();
        return run_prompt(lox);
    }
}
