#pragma once

#include "ast.hpp"
#include "tokens.hpp"
#include "value.hpp"
#include "lox.hpp"

class Resolver : public IExprVisitor, public IStmtVisitor {
    struct var_record_t {
        token_t decl = {};
        int id = -1;
        bool defined : 1 = false;
        bool used : 1 = false;
    };
    struct scope_t {
        int var_id_allocator = 0;
        unordered_map<string_view, var_record_t> vars{};
    };
    enum func_traversal_state_t {
        e_fts_none, e_fts_in_function
    };
    enum loop_traversal_state_t {
        e_lts_none, e_lts_in_while
    };

    vector<scope_t> m_scopes{};
    func_traversal_state_t m_func_traversal_state = e_fts_none;
    loop_traversal_state_t m_loop_traversal_state = e_lts_none;
    Interpreter *m_interp = nullptr;
    lox_t *m_lox = nullptr;

#define STATE_GUARD(state_, value_)   \
    auto state_ ## _prev_ = (state_); \
    state_ = (value_);                \
    DEFERM(state_ = state_ ## _prev_)

public:
    Resolver(Interpreter *interp, lox_t *lox) : m_interp{interp}, m_lox{lox} {}

    ~Resolver() {}

    void Resolve(span<stmt_ptr_t const> stmts) {
        for (auto const &stmt : stmts)
            Resolve(*stmt);
    }

    void VisitAssignmentExpr(AssignmentExpr const &assignment) override {
        Resolve(*assignment.val);
        ResolveLocal(assignment, assignment.target);
    }
    void VisitUnaryExpr(UnaryExpr const &unary) override
        { Resolve(*unary.right); }
    void VisitGroupingExpr(GroupingExpr const &grouping) override
        { Resolve(*grouping.expr); }
    void VisitLiteralExpr(LiteralExpr const &) override {}
    void VisitBinaryExpr(BinaryExpr const &binary) override {
        Resolve(*binary.left);
        Resolve(*binary.right);
    }
    void VisitTernaryExpr(TernaryExpr const &ternary) override {
        Resolve(*ternary.first);
        Resolve(*ternary.second);
        Resolve(*ternary.third);
    }
    void VisitCallExpr(CallExpr const &call) override {
        Resolve(*call.callee);
        for (auto const &arg : call.args)
            Resolve(*arg);
    }
    void VisitVariableExpr(VariableExpr const &variable) override {
        if (auto *scope = CurrentScope()) {
            if (auto it = scope->vars.find(variable.id.lexeme);
                it != scope->vars.end() && !it->second.defined)
            {
                error(*m_lox, variable.id,
                    "Can't read local variable inside it's initializer.");
            }
        }

        ResolveLocal(variable, variable.id);
    }
    void VisitFunctionalExpr(FunctionalExpr const &func) override
        { ResolveFunc(func, e_fts_in_function); }

    void VisitBlockStmt(BlockStmt const &block) override {
        BeginScope();
        Resolve(block.stmts);
        EndScope();
    }
    void VisitExpressionStmt(ExpressionStmt const &expression) override
        { Resolve(*expression.expr); }
    void VisitFuncDeclStmt(FuncDeclStmt const &func_decl) override {
        Declare(func_decl.name);
        Define(func_decl.name);
        ResolveFunc(func_decl.func, e_fts_in_function);
    }
    void VisitIfStmt(IfStmt const &if_stmt) override {
        Resolve(*if_stmt.cond);
        Resolve(*if_stmt.then_branch);
        if (if_stmt.else_branch)
            Resolve(*if_stmt.else_branch);
    }
    void VisitWhileStmt(WhileStmt const &while_stmt) override {
        STATE_GUARD(m_loop_traversal_state, e_lts_in_while);
        Resolve(*while_stmt.cond);
        Resolve(*while_stmt.body);
    }
    void VisitPrintStmt(PrintStmt const &print) override
        { Resolve(*print.val); }
    void VisitReturnStmt(ReturnStmt const &ret) override {
        if (m_func_traversal_state == e_fts_none)
            error(*m_lox, ret.keyword, "Can't return at top level.");
        if (ret.value)
            Resolve(*ret.value);
    }
    void VisitBreakStmt(BreakStmt const &br) override {
        if (m_loop_traversal_state == e_lts_none)
            error(*m_lox, br.keyword, "Can't break while not in a loop.");
    }
    void VisitVarStmt(VarStmt const &var) override {
        Declare(var.id);
        if (var.init)
            Resolve(*var.init);
        Define(var.id);
    }
    void VisitReplExprStmt(ReplExprStmt const &e) override { Resolve(*e.expr); }

private:
    void Resolve(Stmt const &stmt) { stmt.Accept(*this); }
    void Resolve(Expr const &expr) { expr.Accept(*this); }

    void ResolveLocal(Expr const &expr, token_t id) {
        for (isize i = m_scopes.size() - 1; i >= 0; --i) {
            auto &scope = m_scopes[i];
            if (scope.vars.contains(id.lexeme)) {
                auto &var = scope.vars.at(id.lexeme);
                var.used = true;
                m_interp->ResolveReference(
                    expr, m_scopes.size() - i - 1, var.id);
                return;
            }
        }
    }

    void ResolveFunc(FunctionalExpr const &func, func_traversal_state_t type) {
        STATE_GUARD(m_func_traversal_state, type);
        STATE_GUARD(m_loop_traversal_state, e_lts_none);

        BeginScope();
        for (token_t param : func.params) {
            Declare(param);
            Define(param);
        }
        Resolve(func.body);
        EndScope();
    }

    void BeginScope() { m_scopes.emplace_back(); }
    void EndScope() {
        for (auto const &[_, rec] : m_scopes.back().vars) {
            if (!rec.used)
                error(*m_lox, rec.decl, "Unused local variable.");
        }
        m_scopes.pop_back();
    }

    scope_t *CurrentScope()
        { return m_scopes.empty() ? nullptr : &m_scopes.back(); }
    scope_t const *CurrentScope() const
        { return const_cast<Resolver *>(this)->CurrentScope(); }

    void Declare(token_t name) {
        if (auto *scope = CurrentScope()) {
            auto [it, inserted] = scope->vars.emplace(
                name.lexeme, var_record_t{name, scope->var_id_allocator++});
            if (!inserted) {
                error(*m_lox, name,
                    "A variable with this name already exists in this scope.");
            }

            m_interp->ResolveDeclaration(name, it->second.id);
        }
    }
    void Define(token_t name) {
        if (auto *scope = CurrentScope())
            scope->vars.at(name.lexeme).defined = true;
    }

#undef STATE_GUARD
};
