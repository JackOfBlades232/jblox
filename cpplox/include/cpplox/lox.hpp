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

struct statistics_t {
    usize gc_invocations = 0;
    usize total_entities = 0;
    usize leaked_entities = 0;
    usize max_live_entities = 0;
    usize max_sweeped_in_one_call = 0;
};

inline constexpr usize c_gc_ent_count_threshold = 32;

class Environment;
class GlobalEnvironment;

using environment_ptr_t = shared_ptr<Environment>;
using global_environment_ptr_t = shared_ptr<GlobalEnvironment>;

class Environment : public ILoxEntity {
    environment_ptr_t m_parent{};
    vector<optional<LoxValue>> m_values{};

public:
    explicit Environment(environment_ptr_t const &parent)
        : m_parent{parent} {}
    Environment() = default;

    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        for (auto const &val : m_values) {
            if (val)
                mark_val(*val, refs);
        }
        mark_opt(m_parent, refs);
    }
    void Sweep() override {
        m_parent = {};
        m_values = {};
    }

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
    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        for (auto const &[_, val] : m_values) {
            if (val)
                mark_val(*val, refs);
        }
    }
    void Sweep() override { m_values = {}; }

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

class LoxFunction;
class LoxClass;
class LoxMixin;
class LoxInstance;

using lox_function_ptr_t = shared_ptr<LoxFunction>;
using lox_class_ptr_t = shared_ptr<LoxClass>;
using lox_instance_ptr_t = shared_ptr<LoxInstance>;

class LoxFunction
    : public ILoxCallable
    , public enable_shared_from_this<LoxFunction> {

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

    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        mark_opt(m_closure, refs);
    }
    void Sweep() override { m_closure = {}; }

    int Arity() const override { return m_def.params.size(); }
    string ToString() const override
        { return m_name ? format("<fn {}>", m_name->lexeme) : "<anon fn>"; }

    LoxValue Call(Interpreter &, span<LoxValue>) override;

    LoxValue Bind(
        Interpreter &,
        lox_object_ptr_t const &,
        span<lox_function_ptr_t const> = {});

    optional<token_t> GetName() const { return m_name; }
};

struct method_lookup_result_t {
    lox_function_ptr_t main = nullptr;
    vector<lox_function_ptr_t> inner_chain{};

    operator bool() const { return main != nullptr; }
};

class LoxClass
    : public ILoxCallable
    , public ILoxInstance
    , public enable_shared_from_this<LoxClass> {

    optional<token_t> m_name{};
    lox_class_ptr_t m_superclass{};
    bool m_inverted_inheritance = false;
    unordered_map<string_view, lox_function_ptr_t> m_methods{};
    unordered_map<string_view, lox_function_ptr_t> m_static_methods{};
    unordered_map<string_view, lox_function_ptr_t> m_getters{};

    using method_map_t = decay_t<decltype(m_methods)>;

public:
    LoxClass(
            optional<token_t> const &name,
            lox_class_ptr_t superclass,
            bool inverted_inheritance,
            method_map_t methods,
            method_map_t static_methods,
            method_map_t getters)
        : m_name{name}
        , m_superclass{superclass}
        , m_inverted_inheritance{inverted_inheritance}
        , m_methods{move(methods)}
        , m_static_methods{move(static_methods)}
        , m_getters{move(getters)} {}

    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        mark_opt(m_superclass, refs);
        mark_map(m_methods, refs);
        mark_map(m_static_methods, refs);
        mark_map(m_getters, refs);
    }
    void Sweep() override {
        m_superclass = {};
        m_methods = {};
        m_static_methods = {};
        m_getters = {};
    }

    string ToString() const override
        { return m_name ? string{m_name->lexeme} :  "<anon class>"; }
    int Arity() const override {
        if (auto [init, _] = FindMethod("init"))
            return init->Arity();
        return 0;
    }

    LoxValue Call(Interpreter &, span<LoxValue>) override;

    LoxValue Get(Interpreter &, token_t const &) override;
    void Set(Interpreter &, token_t const &, LoxValue const &) override;

    lox_class_ptr_t Clone(Interpreter &) const;
    void Mixin(LoxMixin const &);

    method_lookup_result_t FindMethod(string_view name) const
        { return FindMethodInMap(&LoxClass::m_methods, name); }
    method_lookup_result_t FindStaticMethod(string_view name) const
        { return FindMethodInMap(&LoxClass::m_static_methods, name); }
    method_lookup_result_t FindGetter(string_view name) const
        { return FindMethodInMap(&LoxClass::m_getters, name); }

public:
    method_lookup_result_t FindMethodInMap(
        method_map_t const LoxClass::*map_member, string_view name) const {

        lox_function_ptr_t local_method = nullptr;
        if (auto it = (this->*map_member).find(name);
            it != (this->*map_member).end())
        {
            local_method = it->second;
        }

        if (m_superclass && m_inverted_inheritance) {
            if (auto [m, i] = m_superclass->FindMethodInMap(map_member, name)) {
                auto inner_chain = move(i);
                if (local_method)
                    inner_chain.emplace_back(local_method);
                return {m, move(inner_chain)};
            }
        }

        if (local_method)
            return {local_method, {}};

        if (m_superclass && !m_inverted_inheritance)
            return m_superclass->FindMethodInMap(map_member, name);

        return {};
    }
};

class LoxMixin : public ILoxObject {
    optional<token_t> m_name{};
    vector<lox_function_ptr_t> m_methods{};
    vector<lox_function_ptr_t> m_static_methods{};
    vector<lox_function_ptr_t> m_getters{};

public:
    LoxMixin(
            optional<token_t> const &name,
            vector<lox_function_ptr_t> methods,
            vector<lox_function_ptr_t> static_methods,
            vector<lox_function_ptr_t> getters)
        : m_name{name}
        , m_methods{move(methods)}
        , m_static_methods{move(static_methods)}
        , m_getters{move(getters)} {}

    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        mark_list(m_methods, refs);
        mark_list(m_static_methods, refs);
        mark_list(m_getters, refs);
    }
    void Sweep() override {
        m_methods = {};
        m_static_methods = {};
        m_getters = {};
    }

    string ToString() const override
        { return m_name ? string{m_name->lexeme} : "<anon class>"; }

    span<lox_function_ptr_t const> GetMethods() const
        { return m_methods; }
    span<lox_function_ptr_t const> GetStaticMethods() const
        { return m_static_methods; }
    span<lox_function_ptr_t const> GetGetters() const
        { return m_getters; }
};

class LoxModule : public ILoxInstance {

    optional<token_t> m_name{};
    environment_ptr_t m_closure{};

public:
    LoxModule(
        optional<token_t> const &name,
        ModuleExpr const &mod,
        Interpreter &interp);

    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        mark_opt(m_closure, refs);
    }
    void Sweep() override { m_closure = {}; }

    string ToString() const override
        { return m_name ? string{m_name->lexeme} : "<anon module>"; }

    LoxValue Get(Interpreter &, token_t const &) override;
    void Set(Interpreter &, token_t const &, LoxValue const &) override;
};

class LoxInstance
    : public ILoxInstance
    , public enable_shared_from_this<LoxInstance> {

    lox_class_ptr_t m_class{};
    unordered_map<string_view, LoxValue> m_fields{};

public:
    explicit LoxInstance(lox_class_ptr_t const &cls) : m_class{cls} {}

    void Mark(entity_ref_collection_t &refs) const override
    {
        if (!register_ref(this, refs))
            return;
        mark_opt(m_class, refs);
        for (auto const &[_, f] : m_fields)
            mark_val(f, refs);
    }
    void Sweep() override {
        m_class = {};
        m_fields = {};
    }

    string ToString() const override
        { return format("<{} instance>", m_class->ToString()); }

    LoxValue Get(Interpreter &, token_t const &) override;
    void Set(Interpreter &, token_t const &, LoxValue const &) override;
};

class Interpreter : public IExprVisitor, public IStmtVisitor {
    struct resolved_var_t {
        int id;
        int depth;
    };

    vector<lox_entity_ptr_t> m_live_entities{};

    global_environment_ptr_t m_global_env{};
    environment_ptr_t m_cur_env{};
    vector<lox_entity_ptr_t> m_suspended_entities{};

    unordered_map<Expr const *, resolved_var_t> m_resolved_local_refs{};
    unordered_map<token_t, int> m_resolved_local_decls{};

    statistics_t m_stats{};

    LoxValue *m_dest = nullptr;

    friend class LoxFunction;
    friend class LoxClass;
    friend class LoxMixin;
    friend class LoxModule;
    friend class LoxInstance;

    class Clock : public ILoxCallable {
        HiresTimer m_timer{};

    public:
        LoxValue Call(Interpreter &, span<LoxValue>) override
            { return m_timer.MsFromStart() / 1000.0; }
        int Arity() const override { return 0; }
        void Mark(entity_ref_collection_t &refs) const override
            { register_ref(this, refs); }
        void Sweep() override {}
        string ToString() const override { return "<native fn>"; }
    };

public:
    Interpreter()
        : m_global_env{MakeEnt<GlobalEnvironment>()}
        { m_global_env->Define("clock", MakeObj<Clock>()); }

    ~Interpreter() {
        m_cur_env = {};
        m_global_env = {};
        CollectGarbage(true);

        m_stats.total_entities += m_live_entities.size();
        m_stats.leaked_entities = m_live_entities.size();

        println(stderr, "Stats:");
        println(stderr, "  Total {} entities ever live.",
            m_stats.total_entities);
        println(stderr, "  Leaked {} entities.",
            m_stats.leaked_entities);
        println(stderr, "  Max {} concurrent live entities.",
            m_stats.max_live_entities);
        println(stderr, "  Total {} GC invocations.",
            m_stats.gc_invocations);
        println(stderr, "  Max {} collected entities in one GC invocation.",
            m_stats.max_sweeped_in_one_call);
    }

    LoxValue Evaluate(Expr const &expr) {
        LoxValue res;
        m_dest = &res;
        expr.Accept(*this);
        return res;
    }
    void Execute(Stmt const &stmt) {
        stmt.Accept(*this);
        CollectGarbage();
    }

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

        case e_tt_with: {
            CheckWithOperands(binary.op, l, r);
            auto cval =
                dynamic_cast<LoxClass *>(l.GetObject().get())->Clone(*this);
            cval->Mixin(*dynamic_cast<LoxMixin *>(r.GetObject().get()));
            val = static_pointer_cast<ILoxObject>(cval);
        } break;

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
        obj.GetInstance().Set(*this, set.name, val);

        *prev_dest = val;
    }
    void VisitThisExpr(ThisExpr const &t) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        LoxValue const val = LookupVar(t.keyword, t); 
        *prev_dest = val;
    }
    void VisitSuperExpr(SuperExpr const &s) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        auto [super_id, depth] = m_resolved_local_refs.find(&s)->second;
        lox_class_ptr_t superclass = dynamic_pointer_cast<LoxClass>(
            m_cur_env->LookupAt(super_id, depth, s.keyword).GetObject());

        if (auto [m, ic] = superclass->FindStaticMethod(s.method.lexeme)) {
            *prev_dest = m->Bind(
                *this, static_pointer_cast<ILoxObject>(superclass), ic);
            return;
        }

        auto this_id = m_resolved_local_decls.find(c_implicit_this_tok)->second;
        lox_instance_ptr_t inst = dynamic_pointer_cast<LoxInstance>(
            m_cur_env->LookupAt(this_id, depth - 1, c_implicit_super_tok)
                .GetObject()
        );

        if (auto [m, ic] = superclass->FindMethod(s.method.lexeme)) {
            *prev_dest = m->Bind(
                *this, static_pointer_cast<ILoxObject>(inst), ic);
            return;
        }
        if (auto [m, ic] = superclass->FindGetter(s.method.lexeme)) {
            *prev_dest = m
                ->Bind(*this, static_pointer_cast<ILoxObject>(inst), ic)
                .GetCallable()
                .Call(*this, {});
            return;
        }

        throw runtime_error_t{
            s.method, format("Undefined property '{}'", s.method.lexeme)};
    }
    void VisitInnerExpr(InnerExpr const &i) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);

        auto [inner_id, depth] = m_resolved_local_refs.find(&i)->second;
        auto inner_obj = m_cur_env->LookupAt(inner_id, depth, i.keyword);

        // An arbitrary choice -- if ther is no inner, we don't eval args
        // and we return nil
        if (inner_obj.IsNil()) {
            *prev_dest = c_nil;
            return;
        }

        lox_function_ptr_t inner =
            dynamic_pointer_cast<LoxFunction>(inner_obj.GetObject());

        vector<LoxValue> arg_values{};
        for (auto const &arg : i.args)
            arg_values.emplace_back(Evaluate(*arg));

        if (int const arity = inner->Arity(); arity != arg_values.size()) {
            throw runtime_error_t{
                i.paren, "Expected " + to_string(arity) +
                " args, but got " + to_string(arg_values.size()) + "."};
        }

        *prev_dest = inner->Call(*this, arg_values);
    }
    void VisitVariableExpr(VariableExpr const &variable) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = LookupVar(variable.id, variable);
    }
    void VisitFunctionalExpr(FunctionalExpr const &func) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = MakeObj<LoxFunction>(func, m_cur_env, false);
    }
    void VisitClassyExpr(ClassyExpr const &cls) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = EvaluateClass(cls);
    }
    void VisitMixinExpr(MixinExpr const &mixin) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = EvaluateMixin(mixin);
    }
    void VisitModuleExpr(ModuleExpr const &mod) override {
        assert(m_dest);
        LoxValue *prev_dest = exchange(m_dest, nullptr);
        *prev_dest = MakeObj<LoxModule>(nullopt, mod, *this);
    }

    void VisitBlockStmt(BlockStmt const &block) override
        { ExecuteBlock(block.stmts, MakeEnt<Environment>(m_cur_env)); }
    void VisitExpressionStmt(ExpressionStmt const &expression) override
        { Evaluate(*expression.expr); }
    void VisitFuncDeclStmt(FuncDeclStmt const &func_decl) override {
        DefineVar(
            func_decl.name,
            MakeObj<LoxFunction>(func_decl, m_cur_env, false));
    }
    void VisitClassDeclStmt(ClassDeclStmt const &class_decl) override
        { EvaluateClass(class_decl.cls, class_decl.name); }
    void VisitMixinDeclStmt(MixinDeclStmt const &mixin_decl) override
        { EvaluateMixin(mixin_decl.mixin, mixin_decl.name); }
    void VisitModuleDeclStmt(ModuleDeclStmt const &mod_decl) override {
        DefineVar(
            mod_decl.name,
            MakeObj<LoxModule>(mod_decl.name, mod_decl.mod, *this));
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

    void CheckWithOperands(
        token_t tok, LoxValue const &l, LoxValue const &r) const {

        if (l.IsObject() && r.IsObject() &&
            dynamic_cast<LoxClass *>(l.GetObject().get()) &&
            dynamic_cast<LoxMixin *>(r.GetObject().get()))
        {
            return;
        }
        throw runtime_error_t{tok, "Operands must be a class and a mixin."};
    }

    void ExecuteBlock(
        span<stmt_ptr_t const> stmts, environment_ptr_t const &env) {

        environment_ptr_t prev = m_cur_env;
        DEFERM(m_cur_env = move(prev));
        m_cur_env = move(env);
        for (auto const &stmt : stmts)
            Execute(*stmt);
    }

    lox_object_ptr_t EvaluateClass(
        ClassyExpr const &cls, optional<token_t> name = nullopt) {

        lox_class_ptr_t superclass{};
        if (cls.superclass) {
            auto superclass_val = Evaluate(*cls.superclass);
            superclass = superclass_val.IsObject() ?
                dynamic_pointer_cast<LoxClass>(superclass_val.GetObject()) :
                nullptr;
            if (!superclass) {
                throw runtime_error_t{
                    cls.inheritance_keyword,
                    "Superclass value must be a class."};
            }
        }

        unordered_map<string_view, lox_function_ptr_t> methods{};
        unordered_map<string_view, lox_function_ptr_t> static_methods{};
        unordered_map<string_view, lox_function_ptr_t> getters{};

        {
            environment_ptr_t prev = m_cur_env;
            DEFERM(m_cur_env = move(prev));
            m_cur_env = MakeEnt<Environment>(m_cur_env);

            if (cls.superclass &&
                cls.inheritance_keyword.type == e_tt_less)
            {
                m_cur_env->Define(
                    m_resolved_local_decls.at(c_implicit_super_tok),
                    static_pointer_cast<ILoxObject>(superclass));
            }

            for (auto const &method : cls.methods) {
                auto const &m =
                    *static_cast<FuncDeclStmt const *>(method.get());
                methods.emplace(
                    m.name.lexeme,
                    MakeEnt<LoxFunction>(m, m_cur_env, m.name.lexeme == "init")
                );
            }
            for (auto const &method : cls.static_methods) {
                auto const &m =
                    *static_cast<FuncDeclStmt const *>(method.get());
                static_methods.emplace(
                    m.name.lexeme, MakeEnt<LoxFunction>(m, m_cur_env, false));
            }
            for (auto const &getter : cls.getters) {
                auto const &g =
                    *static_cast<FuncDeclStmt const *>(getter.get());
                getters.emplace(
                    g.name.lexeme, MakeEnt<LoxFunction>(g, m_cur_env, false));
            }
        }

        auto cls_value = MakeObj<LoxClass>(
            name, move(superclass),
            cls.inheritance_keyword.type == e_tt_greater,
            move(methods), move(static_methods), move(getters));
        auto cls_concrete = dynamic_pointer_cast<LoxClass>(cls_value);

        for (auto const &mixin_expr : cls.mixins) {
            auto mixin_val = Evaluate(*mixin_expr);
            auto mixin = mixin_val.IsObject() ?
                dynamic_pointer_cast<LoxMixin>(mixin_val.GetObject()) :
                nullptr;
            if (!mixin) {
                throw runtime_error_t{
                    cls.mixin_keyword,
                    "Mixin value must be a mixin."};
            }

            cls_concrete->Mixin(*mixin);
        }

        if (name)
            DefineVar(*name, cls_value);

        if (auto [init, inner_init_chain] =
            cls_concrete->FindStaticMethod("init"))
        {
            init->Bind(*this, cls_value, inner_init_chain)
                .GetCallable()
                .Call(*this, {}); 
        }

        return cls_value;
    }

    lox_object_ptr_t EvaluateMixin(
        MixinExpr const &mixin, optional<token_t> name = nullopt) {

        vector<lox_function_ptr_t> methods{};
        vector<lox_function_ptr_t> static_methods{};
        vector<lox_function_ptr_t> getters{};

        {
            environment_ptr_t prev = m_cur_env;
            DEFERM(m_cur_env = move(prev));
            m_cur_env = MakeEnt<Environment>(m_cur_env);

            for (auto const &method : mixin.methods) {
                auto const &m =
                    *static_cast<FuncDeclStmt const *>(method.get());
                methods.emplace_back(
                    MakeEnt<LoxFunction>(m, m_cur_env, m.name.lexeme == "init")
                );
            }
            for (auto const &method : mixin.static_methods) {
                auto const &m =
                    *static_cast<FuncDeclStmt const *>(method.get());
                static_methods.emplace_back(
                     MakeEnt<LoxFunction>(m, m_cur_env, false));
            }
            for (auto const &getter : mixin.getters) {
                auto const &g =
                    *static_cast<FuncDeclStmt const *>(getter.get());
                getters.emplace_back(
                    MakeEnt<LoxFunction>(g, m_cur_env, false));
            }
        }

        auto mixin_value = MakeObj<LoxMixin>(
            name, move(methods), move(static_methods), move(getters));

        if (name)
            DefineVar(*name, mixin_value);

        return mixin_value;
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

    template <class T, class ...TArgs>
        requires constructible_from<T, TArgs...> && derived_from<T, ILoxEntity>
    shared_ptr<T> MakeEnt(TArgs &&...args) {
        auto ptr = make_shared<T>(forward<TArgs>(args)...);
        m_live_entities.push_back(ptr);
        return ptr;
    }

    template <class T, class ...TArgs>
        requires constructible_from<T, TArgs...> && derived_from<T, ILoxObject>
    lox_object_ptr_t MakeObj(TArgs &&...args) {
        return lox_object_ptr_t{MakeEnt<T>(forward<TArgs>(args)...)};
    }
    
    void CollectGarbage(bool force = false) {
        if (m_live_entities.size() < c_gc_ent_count_threshold && !force)
            return;

        ++m_stats.gc_invocations;
        m_stats.max_live_entities =
            max(m_stats.max_live_entities, m_live_entities.size());

        entity_ref_collection_t reachables{};
        mark_opt(m_cur_env, reachables);
        mark_opt(m_global_env, reachables);
        for (auto const &ent : m_suspended_entities)
            mark_opt(ent, reachables);

        usize sweeped = 0;
        for (auto &ent : m_live_entities) {
            if (!reachables.contains(ent.get())) {
                ++sweeped;
                ent->Sweep();
            }
        }

        m_stats.max_sweeped_in_one_call =
            max(m_stats.max_sweeped_in_one_call, sweeped);
        m_stats.total_entities += sweeped;

        auto end = remove_if(
            m_live_entities.begin(), m_live_entities.end(),
            [&](auto const &ent) { return ent.use_count() <= 1; });

        m_live_entities.resize(end - m_live_entities.begin());
    }
};

inline LoxValue LoxFunction::Call(Interpreter &interp, span<LoxValue> args)
{
    environment_ptr_t env = interp.MakeEnt<Environment>(m_closure);
    for (usize i = 0; auto const &param : m_def.params)
        env->Define(interp.m_resolved_local_decls.at(param), args[i++]);

    auto findThis = [&, this] {
        return m_closure->Lookup(
            interp.m_resolved_local_decls.at(c_implicit_this_tok), {});
    };

    interp.m_suspended_entities.push_back(interp.m_cur_env);
    interp.m_suspended_entities.push_back(shared_from_this());
    DEFER(interp.m_suspended_entities.pop_back());
    DEFER(interp.m_suspended_entities.pop_back());
    
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
    Interpreter &interp,
    lox_object_ptr_t const &inst,
    span<lox_function_ptr_t const> inner_chain)
{
    environment_ptr_t env = interp.MakeEnt<Environment>(m_closure);
    env->Define(
        interp.m_resolved_local_decls.at(c_implicit_this_tok),
        inst);
    auto bound_inner = c_nil;
    if (inner_chain.size()) {
        bound_inner = inner_chain.front()->Bind(
            interp, inst, inner_chain.subspan(1));
    }
    env->Define(
        interp.m_resolved_local_decls.at(c_implicit_inner_tok), bound_inner);
    return interp.MakeObj<LoxFunction>(m_def, m_name, env, m_init);
}

inline LoxValue LoxClass::Call(Interpreter &interp, span<LoxValue> args)
{
    auto inst = interp.MakeObj<LoxInstance>(shared_from_this());
    auto [init, inner_init_chain] = FindMethod("init");
    if (init) {
        init->Bind(interp, inst, inner_init_chain)
            .GetCallable()
            .Call(interp, args);
    }
    return inst;
}

inline LoxValue LoxClass::Get(Interpreter &interp, token_t const &name)
{
    if (auto [m, ic] = FindStaticMethod(name.lexeme)) {
        return m->Bind(
            interp, static_pointer_cast<ILoxObject>(shared_from_this()), ic);
    }

    throw runtime_error_t{
        name, format("Undefined static method '{}'", name.lexeme)};
}

inline void LoxClass::Set(
    Interpreter &, token_t const &name, LoxValue const &)
{
    throw runtime_error_t{name, "Can't define properties on classes."};
}

inline lox_class_ptr_t LoxClass::Clone(Interpreter &interp) const
{
    LoxClass clone = *this;
    return dynamic_pointer_cast<LoxClass>(
        interp.MakeObj<LoxClass>(move(clone)));
}

inline void LoxClass::Mixin(LoxMixin const &mixin)
{
    for (auto const &method : mixin.GetMethods())
        m_methods.emplace(method->GetName()->lexeme, method);
    for (auto const &method : mixin.GetStaticMethods())
        m_static_methods.emplace(method->GetName()->lexeme, method);
    for (auto const &getter : mixin.GetGetters())
        m_getters.emplace(getter->GetName()->lexeme, getter);
}

inline LoxModule::LoxModule(
    optional<token_t> const &name, ModuleExpr const &mod, Interpreter &interp)
{
    m_closure = interp.MakeEnt<Environment>(interp.m_cur_env);
    interp.ExecuteBlock(mod.body, m_closure);
    // @TODO: we need to convert the closure to a proper dict right about
    // now. This is problematic because of the whole resolved ids stuff.
    // Hmm...
}

inline LoxValue LoxModule::Get(Interpreter &interp, token_t const &name)
{
    // @TODO: query off of the resolved dict
    return {};
}

inline void LoxModule::Set(
    Interpreter &interp, token_t const &name, LoxValue const &val)
{
    // @TODO:
    // 1) Check that it is a var, and not a func/class/mixin
    // 2) Set
}

inline LoxValue LoxInstance::Get(Interpreter &interp, token_t const &name)
{
    if (auto it = m_fields.find(name.lexeme); it != m_fields.end())
        return it->second;

    if (auto [m, ic] = m_class->FindMethod(name.lexeme)) {
        return m->Bind(
            interp, static_pointer_cast<ILoxObject>(shared_from_this()), ic);
    }

    if (auto [m, ic] = m_class->FindGetter(name.lexeme)) {
        return m->Bind(
            interp, static_pointer_cast<ILoxObject>(shared_from_this()), ic)
            .GetCallable()
            .Call(interp, {});
    }

    throw runtime_error_t{name, format("Undefined property '{}'", name.lexeme)};
}

inline void LoxInstance::Set(
    Interpreter &, token_t const &name, LoxValue const &val)
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
