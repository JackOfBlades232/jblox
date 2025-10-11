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

class Environment : public ILoxEntity {
    lox_entity_ptr_t m_parent{}; // Opaque for self reference + strong ref
    vector<optional<LoxValue>> m_values{};

public:
    explicit Environment(lox_entity_ptr_t const &parent) : m_parent{parent} {}
    Environment() = default;

    void Define(int id, optional<LoxValue> const &val) {
        if (m_values.size() <= id)
            m_values.resize(id + 1, nullopt);
        m_values[id] = val;
    }

    LoxValue Lookup(int id, token_t name) const {
        if (auto const &var = m_values[id]) {
            return *var;
        } else {
            throw runtime_error_t{name, format(
                "Accessing uninitialized variable '{}'", name.lexeme)};
        }
    }

    LoxValue LookupAt(int id, int depth, token_t name) const
        { return Ancestor(depth)->Lookup(id, name); }

    void Assign(int id, LoxValue const &val) { m_values[id] = val; }

    void AssignAt(int id, int depth, LoxValue const &val)
        { Ancestor(depth)->Assign(id, val); }

private:
    Environment *Parent() {
        if (!m_parent)
            return nullptr;
        assert(dynamic_cast<Environment *>(m_parent.get()));
        return static_cast<Environment *>(m_parent.get());
    }
    Environment const *Parent() const
        { return const_cast<Environment *>(this)->Parent(); }
    Environment *Ancestor(int depth) {
        auto *env = this;
        for (; depth > 0 && Parent(); --depth)
            env = env->Parent();
        return env;
    }
    Environment const *Ancestor(int depth) const
        { return const_cast<Environment *>(this)->Ancestor(depth); }
};

class GlobalEnvironment : public ILoxEntity {
    unordered_map<string, optional<LoxValue>> m_values{};

public:
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

        throw runtime_error_t{
            name, format("Undefined variable '{}'", name.lexeme)};
    }
};

using environment_ptr_t = shared_ptr<Environment>;
using global_environment_ptr_t = shared_ptr<GlobalEnvironment>;

class LoxFunction;
class LoxClass;
class LoxInstance;

class LoxFunction : public ILoxCallable {
    FunctionalExpr m_def;
    environment_ptr_t m_closure;
    optional<token_t> m_name{};
    bool m_init = false;

public:
    LoxFunction(
            FuncDeclStmt const &decl,
            environment_ptr_t const &closure,
            bool init)
        : m_def{decl.func}
        , m_closure{closure}
        , m_name{decl.name}
        , m_init{init} {}
    LoxFunction(
            FunctionalExpr const &def,
            environment_ptr_t const &closure,
            bool init)
        : m_def{def}, m_closure{closure}, m_init{init} {}
    LoxFunction(
            FunctionalExpr const &def,
            optional<token_t> const &name,
            environment_ptr_t const &closure,
            bool init)
        : m_def{def}, m_name{name}, m_closure{closure}, m_init{init} {}

    int Arity() const override { return m_def.params.size(); }
    string ToString() const override
        { return m_name ? format("<fn {}>", m_name->lexeme) : "<anon fn>"; }

    LoxValue Call(Interpreter &, span<LoxValue>) override;

    LoxValue Bind(Interpreter &, LoxValue const &);
};

using lox_function_ptr_t = shared_ptr<LoxFunction>;

class LoxClass : public ILoxCallable, public enable_shared_from_this<LoxClass> {
    string_view m_name;
    unordered_map<string_view, lox_function_ptr_t> m_methods{};

    using method_map_t = decay_t<decltype(m_methods)>;

public:
    explicit LoxClass(string_view name, method_map_t methods)
        : m_name{name}, m_methods{move(methods)} {}

    string ToString() const override { return string{m_name}; }
    int Arity() const override {
        if (auto init = FindMethod("init"))
            return init->Arity();
        return 0;
    }

    LoxValue Call(Interpreter &, span<LoxValue>) override;

    lox_function_ptr_t FindMethod(string_view name) const {
        if (auto it = m_methods.find(name); it != m_methods.end())
            return it->second;
        return nullptr;
    }
};

using lox_class_ptr_t = shared_ptr<LoxClass>;

class LoxInstance
    : public ILoxInstance
    , public enable_shared_from_this<LoxInstance> {

    lox_class_ptr_t m_class{};
    unordered_map<string_view, LoxValue> m_fields{};

public:
    explicit LoxInstance(lox_class_ptr_t const &cls) : m_class{cls} {}

    string ToString() const override
        { return format("<{} instance>", m_class->ToString()); }

    LoxValue Get(Interpreter &, token_t const &) override;
    void Set(token_t const &, LoxValue const &) override;
};

using lox_instance_ptr_t = shared_ptr<LoxInstance>;

class Interpreter : public IExprVisitor, public IStmtVisitor {
    struct resolved_var_t {
        int id;
        int depth;
    };

    global_environment_ptr_t m_global_env{};
    environment_ptr_t m_cur_env{};

    unordered_map<Expr const *, resolved_var_t> m_resolved_local_refs{};
    unordered_map<token_t, int> m_resolved_local_decls{};

    LoxValue *m_dest = nullptr;

    friend class LoxFunction;
    friend class LoxClass;
    friend class LoxInstance;

    struct Clock : public ILoxCallable {
        LoxValue Call(Interpreter &, span<LoxValue>) override
            { return g_hires_timer.MsFromProgramStart() / 1000.0; }
        int Arity() const override { return 0; }
        string ToString() const override { return "<native fn>"; }
    };

public:
    Interpreter()
        : m_global_env{make_shared<GlobalEnvironment>()}
        { m_global_env->Define("clock", make_object<Clock>()); }

    ~Interpreter() {}

    LoxValue Evaluate(Expr const &expr) {
        LoxValue res;
        m_dest = &res;
        expr.Accept(*this);
        return res;
    }
    void Execute(Stmt const &stmt) { stmt.Accept(*this); }

    void ResolveReference(Expr const &e, int depth, int id) {
        m_resolved_local_refs.insert_or_assign(&e, resolved_var_t{id, depth});
    }
    void ResolveDeclaration(token_t name, int id)
        { m_resolved_local_decls.insert_or_assign(name, id); }

    void VisitAssignmentExpr(AssignmentExpr const &assignment) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        LoxValue const val = Evaluate(*assignment.val);

        auto it = m_resolved_local_refs.find(&assignment);
        if (it == m_resolved_local_refs.end())
            m_global_env->Assign(assignment.target, val);
        else
            m_cur_env->AssignAt(it->second.id, it->second.depth, val);

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
    void VisitGetExpr(GetExpr const &get) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        LoxValue obj = Evaluate(*get.obj);
        if (!obj.IsInstance())
            throw runtime_error_t{get.name, "Only instances have properties."};

        *prev_dest = obj.GetInstance().Get(*this, get.name);
    }
    void VisitSetExpr(SetExpr const &set) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        LoxValue obj = Evaluate(*set.obj);
        if (!obj.IsInstance())
            throw runtime_error_t{set.name, "Only instances have fields."};

        LoxValue const val = Evaluate(*set.value);
        obj.GetInstance().Set(set.name, val);

        *prev_dest = val;
    }
    void VisitThisExpr(ThisExpr const &t) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        LoxValue const val = LookupVar(t.keyword, t); 
        *prev_dest = val;
    }
    void VisitVariableExpr(VariableExpr const &variable) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = LookupVar(variable.id, variable);
    }
    void VisitFunctionalExpr(FunctionalExpr const &func) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = make_object<LoxFunction>(func, m_cur_env, false);
    }

    void VisitBlockStmt(BlockStmt const &block) override
        { ExecuteBlock(block.stmts, make_shared<Environment>(m_cur_env)); }
    void VisitExpressionStmt(ExpressionStmt const &expression) override
        { Evaluate(*expression.expr); }
    void VisitFuncDeclStmt(FuncDeclStmt const &func_decl) override {
        DefineVar(
            func_decl.name,
            make_object<LoxFunction>(func_decl, m_cur_env, false));
    }
    void VisitClassDeclStmt(ClassDeclStmt const &class_decl) override {
        unordered_map<string_view, lox_function_ptr_t> methods{};
        for (auto const &method : class_decl.methods) {
            methods.emplace(
                method.name.lexeme,
                make_shared<LoxFunction>(
                    method, m_cur_env, method.name.lexeme == "init"));
        }

        DefineVar(
            class_decl.name,
            make_object<LoxClass>(class_decl.name.lexeme, move(methods)));
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
        DefineVar(var.id, val);
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

    LoxValue LookupVar(token_t name, Expr const &expr) const {
        auto it = m_resolved_local_refs.find(&expr);
        if (it == m_resolved_local_refs.end()) // unresolved == presumed global
            return m_global_env->Lookup(name);
        else
            return m_cur_env->LookupAt(it->second.id, it->second.depth, name);
    }

    void DefineVar(token_t name, optional<LoxValue> const &val) {
        if (!m_cur_env)
            m_global_env->Define(name.lexeme, val);
        else
            m_cur_env->Define(m_resolved_local_decls.at(name), val);
    }
};

inline LoxValue LoxFunction::Call(Interpreter &interp, span<LoxValue> args)
{
    environment_ptr_t env = make_shared<Environment>(m_closure);
    for (usize i = 0; auto const &param : m_def.params)
        env->Define(interp.m_resolved_local_decls.at(param), args[i++]);

    // This is a hack -- we know that implicit this is always at id 0
    auto findThis = [&, this] {
        return m_closure->Lookup(
            interp.m_resolved_local_decls.at(c_implicit_this_tok), {});
    };
    
    try {
        interp.ExecuteBlock(m_def.body, env);
    } catch (return_signal_t ret) {
        if (m_init)
            return findThis();
        return move(ret.val);
    }

    return m_init ? findThis() : c_nil;
}

inline LoxValue LoxFunction::Bind(
    Interpreter &interp, LoxValue const &inst)
{
    environment_ptr_t env = make_shared<Environment>(m_closure);
    env->Define(
        interp.m_resolved_local_decls.at(c_implicit_this_tok),
        inst);
    return make_object<LoxFunction>(m_def, m_name, env, m_init);
}

inline LoxValue LoxClass::Call(Interpreter &interp, span<LoxValue> args)
{
    auto inst = make_object<LoxInstance>(shared_from_this());
    auto init = FindMethod("init");
    if (init)
        init->Bind(interp, inst).GetCallable().Call(interp, args);
    return inst;
}

inline LoxValue LoxInstance::Get(Interpreter &interp, token_t const &name)
{
    if (auto it = m_fields.find(name.lexeme); it != m_fields.end())
        return it->second;

    if (auto m = m_class->FindMethod(name.lexeme)) {
        return m->Bind(
            interp, static_pointer_cast<ILoxObject>(shared_from_this()));
    }

    throw runtime_error_t{name, format("Undefined property '{}'", name.lexeme)};
}

inline void LoxInstance::Set(token_t const &name, LoxValue const &val)
{
    m_fields.insert_or_assign(name.lexeme, val);
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
