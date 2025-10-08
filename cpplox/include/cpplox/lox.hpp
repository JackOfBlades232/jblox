#pragma once

#include "defs.hpp"
#include "value.hpp"
#include "tokens.hpp"

#include <ast.hpp>

struct runtime_error_t {
    token_t tok;
    string message;
};

struct break_signal_t {};
struct return_signal_t {
    LoxValue val;
};

// @TODO: currently we are leaking environments every time we return a
// closed function -- it holds a ref to the closure, and the closure holds
// the environment stack. Do mark and sweep from actual live environment
// to break strong cycles. This will also be important when objects become
// a thing.
// @TODO: separate concept of 'gc-d ref' and 'callable or object' --
// a LoxValue can hold a callable or and object, not an environment.
// However, when we do some form of mark & sweep, environments have to
// participate.

class Environment : public ILoxEntity {
    lox_entity_ptr_t m_parent{};
    unordered_map<string, optional<LoxValue>> m_values{};

public:
    explicit Environment(lox_entity_ptr_t const &parent) : m_parent{parent} {}

    Environment() = default;
    Environment(Environment const &) = default;
    Environment(Environment &&) = default;
    Environment &operator=(Environment const &) = default;
    Environment &operator=(Environment &&) = default;

    string ToString() const override { return "<environment>"; }

    Environment *Parent() {
        if (!m_parent)
            return nullptr;
        assert(dynamic_cast<Environment *>(m_parent.get()));
        return static_cast<Environment *>(m_parent.get());
    }
    Environment const *Parent() const
        { return const_cast<Environment *>(this)->Parent(); }

    void Define(string_view name, optional<LoxValue> const &val)
        { m_values.insert_or_assign(string{name}, val); }

    LoxValue Lookup(token_t name) const {
        if (auto it = m_values.find(string{name.lexeme});
            it != m_values.end())
        {
            if (it->second) {
                return *it->second;
            } else {
                throw runtime_error_t{name, format(
                    "Accessing uninitialized variable '{}'", name.lexeme)};
            }
        }
        if (Parent())
            return Parent()->Lookup(name);

        throw runtime_error_t{
            name, format("Undefined variable '{}'", name.lexeme)};
    }

    void Assign(token_t name, LoxValue const &val) {
        if (auto it = m_values.find(string{name.lexeme});
            it != m_values.end())
        {
            it->second = val;
            return;
        }
        if (Parent()) {
            Parent()->Assign(name, val);
            return;
        }

        throw runtime_error_t{
            name, format("Undefined variable '{}'", name.lexeme)};
    }
};

using environment_ptr_t = shared_ptr<Environment>;

class LoxFunction : public ILoxCallable {
    FunctionalExpr m_def;
    environment_ptr_t m_closure;
    optional<token_t> m_name{};

public:
    LoxFunction(FuncDeclStmt const &decl, environment_ptr_t const &closure)
        : m_def{*static_cast<FunctionalExpr const *>(decl.func.get())}
        , m_closure{closure}
        , m_name{decl.name} {

        assert(dynamic_cast<FunctionalExpr const *>(decl.func.get()));
    }
    LoxFunction(FunctionalExpr const &def, environment_ptr_t const &closure)
        : m_def{def}, m_closure{closure} {}

    LoxFunction(LoxFunction const &) = default;
    LoxFunction(LoxFunction &&) = default;
    LoxFunction &operator=(LoxFunction const &) = default;
    LoxFunction &operator=(LoxFunction &&) = default;

    int Arity() const override { return m_def.params.size(); }
    string ToString() const override
        { return m_name ? format("<fn {}>", m_name->lexeme) : "<anon fn>"; }

    LoxValue Call(Interpreter &, span<LoxValue>) override;
};

class Interpreter : public IExprVisitor, public IStmtVisitor {
    environment_ptr_t m_global_env{};
    environment_ptr_t m_cur_env{};

    LoxValue *m_dest = nullptr;

    friend class LoxFunction;

    class Clock : public ILoxCallable {
        LoxValue Call(Interpreter &, span<LoxValue>) override
            { return g_hires_timer.MsFromProgramStart() / 1000.0; }
        int Arity() const override { return 0; }
        string ToString() const override { return "<native fn>"; }
    };

public:
    Interpreter()
        : m_global_env{make_shared<Environment>()}
        , m_cur_env{m_global_env}
    {
        m_global_env->Define("clock", make_ent<Clock>());
    }

    ~Interpreter() {}

    LoxValue Evaluate(Expr const &expr) {
        LoxValue res;
        m_dest = &res;
        expr.Accept(*this);
        return res;
    }
    void Execute(Stmt const &stmt) { stmt.Accept(*this); }

    void VisitAssignmentExpr(AssignmentExpr const &assignment) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        LoxValue const val = Evaluate(*assignment.val);
        m_cur_env->Assign(assignment.target, val);
        *prev_dest = val;
    }
    void VisitUnaryExpr(UnaryExpr const &unary) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        LoxValue val = Evaluate(*unary.right);
        DEFER(*prev_dest = val);

        switch (unary.op.type) {
        case e_tt_minus:
            CheckNumberOperand(unary.op, val);
            val = -val.GetNumber();
            break;
        case e_tt_bang:
            val = !is_truthy(val);
            break;
        default:
            assert(0);
        }
    }
    void VisitGroupingExpr(GroupingExpr const &grouping) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = Evaluate(*grouping.expr);
    }
    void VisitLiteralExpr(LiteralExpr const &literal) override {
        assert(m_dest);
        *m_dest = literal.value;
    }
    void VisitBinaryExpr(BinaryExpr const &binary) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        LoxValue val;
        DEFER(*prev_dest = val);

        LoxValue const l = Evaluate(*binary.left);

        // Short circuit on logical bin ops
        if (binary.op.type == e_tt_and && !is_truthy(l)) {
            val = false;
            return;
        } else if (binary.op.type == e_tt_or && is_truthy(l)) {
            val = true;
            return;
        }

        LoxValue const r = Evaluate(*binary.right);

        switch (binary.op.type) {
        case e_tt_slash:
            CheckNumberOperands(binary.op, l, r);
            if (r.GetNumber() == 0.0)
                throw runtime_error_t{binary.op, "Division by zero."};
            val = l.GetNumber() / r.GetNumber();
            break;
        case e_tt_star:
            CheckNumberOperands(binary.op, l, r);
            val = l.GetNumber() * r.GetNumber();
            break;
        case e_tt_minus:
            CheckNumberOperands(binary.op, l, r);
            val = l.GetNumber() - r.GetNumber();
            break;
        case e_tt_plus:
            if (l.IsNumber() && r.IsNumber()) {
                val = l.GetNumber() + r.GetNumber();
            } else if (l.IsString() && r.IsString()) {
                val = l.GetString() + r.GetString();
            } else if (
                (l.IsNumber() && r.IsString()) ||
                (l.IsString() && r.IsNumber()))
            {
                val = to_string(l) + to_string(r);
            } else {
                throw runtime_error_t{
                    binary.op, "Operands must be numbers or strings."};
            }
            break;

        case e_tt_greater:
            CheckNumberOperands(binary.op, l, r);
            val = l.GetNumber() > r.GetNumber();
            break;
        case e_tt_greater_equal:
            CheckNumberOperands(binary.op, l, r);
            val = l.GetNumber() >= r.GetNumber();
            break;
        case e_tt_less:
            CheckNumberOperands(binary.op, l, r);
            val = l.GetNumber() < r.GetNumber();
            break;
        case e_tt_less_equal:
            CheckNumberOperands(binary.op, l, r);
            val = l.GetNumber() <= r.GetNumber();
            break;

        case e_tt_bang_equal:
            val = !are_equal(l, r);
            break;
        case e_tt_equal_equal:
            val = are_equal(l, r);
            break;

        // Here we handle only the non-short-circuit case
        case e_tt_and:
        case e_tt_or:
            val = is_truthy(r);
            break;

        case e_tt_comma:
            val = r;
            break;

        default:
            assert(0);
        }
    }
    void VisitTernaryExpr(TernaryExpr const &ternary) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        assert(
            ternary.op0.type == e_tt_question &&
            ternary.op1.type == e_tt_colon);
        LoxValue const sw = Evaluate(*ternary.first);
        LoxValue const val =
            is_truthy(sw) ?
            Evaluate(*ternary.second) :
            Evaluate(*ternary.third);

        *prev_dest = val;
    }
    void VisitCallExpr(CallExpr const &call) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        LoxValue callee = Evaluate(*call.callee);
        vector<LoxValue> arg_values{};
        for (auto const &arg : call.args)
            arg_values.emplace_back(Evaluate(*arg));

        if (!callee.IsCallable()) {
            throw runtime_error_t{
                call.paren, "Can only call functions and classes."};
        }

        ILoxCallable &callable = callee.GetCallable();
        if (int const arity = callable.Arity(); arity != arg_values.size()) {
            throw runtime_error_t{
                call.paren, "Expected " + to_string(arity) +
                " args, but got " + to_string(arg_values.size()) + "."};
        }

        *prev_dest = callable.Call(*this, arg_values);
    }
    void VisitVariableExpr(VariableExpr const &variable) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = m_cur_env->Lookup(variable.id);
    }
    void VisitFunctionalExpr(FunctionalExpr const &func) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = make_ent<LoxFunction>(func, m_cur_env);
    }

    void VisitBlockStmt(BlockStmt const &block) override
        { ExecuteBlock(block.stmts, make_shared<Environment>(m_cur_env)); }
    void VisitExpressionStmt(ExpressionStmt const &expression) override
        { Evaluate(*expression.expr); }
    void VisitFuncDeclStmt(FuncDeclStmt const &func_decl) override {
        m_cur_env->Define(
            func_decl.name.lexeme, make_ent<LoxFunction>(func_decl, m_cur_env));
    }
    void VisitIfStmt(IfStmt const &if_stmt) override {
        if (is_truthy(Evaluate(*if_stmt.cond)))
            Execute(*if_stmt.then_branch);
        else if (if_stmt.else_branch)
            Execute(*if_stmt.else_branch);
    }
    void VisitWhileStmt(WhileStmt const &while_stmt) override {
        while (is_truthy(Evaluate(*while_stmt.cond))) {
            try {
                Execute(*while_stmt.body);
            } catch (break_signal_t) {
                break;
            }
        }
    }
    void VisitPrintStmt(PrintStmt const &print) override
        { println("{}", to_string(Evaluate(*print.val))); }
    void VisitReturnStmt(ReturnStmt const &ret) override {
        LoxValue retval = c_nil;
        if (ret.value)
            retval = Evaluate(*ret.value);

        throw return_signal_t{move(retval)};
    }
    void VisitBreakStmt(BreakStmt const &) override
        { throw break_signal_t{}; }
    void VisitVarStmt(VarStmt const &var) override {
        optional<LoxValue> val{};
        if (var.init)
            val = Evaluate(*var.init);

        m_cur_env->Define(var.id.lexeme, val);
    }
    void VisitReplExprStmt(ReplExprStmt const &e) override
        { println("{}", to_string(Evaluate(*e.expr))); }

private:
    void CheckNumberOperand(token_t tok, LoxValue const &operand) const {
        if (operand.IsNumber())
            return;
        throw runtime_error_t{tok, "Operand must be a number."};
    }
    void CheckNumberOperands(
        token_t tok, LoxValue const &l, LoxValue const &r) const {

        if (l.IsNumber() && r.IsNumber())
            return;
        throw runtime_error_t{tok, "Operands must be numbers."};
    }

    void ExecuteBlock(
        span<stmt_ptr_t const> stmts, environment_ptr_t const &env) {

        environment_ptr_t prev = m_cur_env;
        DEFERM(m_cur_env = move(prev));
        m_cur_env = move(env);
        for (auto const &stmt : stmts)
            Execute(*stmt);
    }
};

inline LoxValue LoxFunction::Call(Interpreter &interp, span<LoxValue> args)
{
    environment_ptr_t env = make_shared<Environment>(m_closure);
    for (usize i = 0; auto const &param : m_def.params)
        env->Define(param.lexeme, args[i++]);
    
    try {
        interp.ExecuteBlock(m_def.body, env);
        return c_nil;
    } catch (return_signal_t ret) {
        return move(ret.val);
    }
}

struct lox_config_t {
    bool print_tokens : 1 = false;
    bool print_ast : 1 = false;
    bool is_repl : 1 = false;
};

struct lox_t {
    Interpreter interp{};
    bool had_error = false;
    bool had_runtime_error = false;
    lox_config_t config = {};
};

inline void report(
    lox_t &lox, int line, string_view location, string_view message)
{
    println(stderr, "[line {}] Error {}: {}", line, location, message);
    lox.had_error = true;
}

inline void error(lox_t &lox, int line, string_view message)
{
    report(lox, line, "", message);
}

inline void error(lox_t &lox, token_t tok, string_view message)
{
    if (tok.type == e_tt_eof) {
        report(lox, tok.line, "at end", message);
    } else {
        report(
            lox, tok.line,
            string_view{string{"at '"} + string{tok.lexeme} + string{"'"}},
            message);
    }
}

inline void rt_error(lox_t &lox, runtime_error_t err)
{
    println(stderr, "{}\n[line {}]", err.message, err.tok.line);
    lox.had_runtime_error = true;
}
