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

struct environment_t {
    environment_t *parent = nullptr;
    unordered_map<string, optional<LoxValue>> values{};
};

inline void define(
    environment_t &env, string_view name, optional<LoxValue> const &val)
{
    env.values.insert_or_assign(string{name}, val);
}

inline LoxValue lookup(environment_t const &env, token_t name)
{
    if (auto it = env.values.find(string{name.lexeme});
        it != env.values.end())
    {
        if (it->second) {
            return *it->second;
        } else {
            throw runtime_error_t{
                name,
                format("Accessing uninitialized variable '{}'", name.lexeme)};
        }
    }
    if (env.parent)
        return lookup(*env.parent, name);

    throw runtime_error_t{name, format("Undefined variable '{}'", name.lexeme)};
}

inline void assign(environment_t &env, token_t name, LoxValue const &val)
{
    if (auto it = env.values.find(string{name.lexeme});
        it != env.values.end())
    {
        it->second = val;
        return;
    }
    if (env.parent) {
        assign(*env.parent, name, val);
        return;
    }

    throw runtime_error_t{name, format("Undefined variable '{}'", name.lexeme)};
}

class Interpreter : public IExprVisitor, public IStmtVisitor {
    environment_t m_global_env{};

    environment_t *m_cur_env = &m_global_env;
    LoxValue *m_dest = nullptr;

public:
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
        assign(*m_cur_env, assignment.target, val);
        *prev_dest = val;
    }
    void VisitUnaryExpr(UnaryExpr const &unary) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        LoxValue val = Evaluate(*unary.right);
        DEFERM(*prev_dest = val);

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
        DEFERM(*prev_dest = val);

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
        // @TODO
    }
    void VisitVariableExpr(VariableExpr const &variable) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = lookup(*m_cur_env, variable.id);
    }

    void VisitBlockStmt(BlockStmt const &block) override
        { ExecuteBlock(block.stmts, environment_t{m_cur_env}); }
    void VisitExpressionStmt(ExpressionStmt const &expression) override
        { Evaluate(*expression.expr); }
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
    void VisitBreakStmt(BreakStmt const &) override
        { throw break_signal_t{}; }
    void VisitVarStmt(VarStmt const &var) override {
        optional<LoxValue> val{};
        if (var.init)
            val = Evaluate(*var.init);

        define(*m_cur_env, var.id.lexeme, val);
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

    void ExecuteBlock(span<stmt_ptr_t const> stmts, environment_t &&env) {
        environment_t *prev = m_cur_env;
        DEFERM(m_cur_env = prev);
        m_cur_env = &env;
        for (auto const &stmt : stmts)
            Execute(*stmt);
    }
};

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
