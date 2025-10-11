#pragma once

#include "defs.hpp"

struct nil_t {};

struct ILoxEntity {
    virtual ~ILoxEntity() {};
};

struct ILoxObject : ILoxEntity {
    virtual string ToString() const = 0;
};

using lox_entity_ptr_t = shared_ptr<ILoxEntity>;
using lox_object_ptr_t = shared_ptr<ILoxObject>;

template <derived_from<ILoxEntity> T, class ...TArgs>
    requires constructible_from<T, TArgs...>
inline lox_entity_ptr_t make_ent(TArgs &&...args)
{
    return lox_entity_ptr_t{make_shared<T>(forward<TArgs>(args)...)};
}

template <derived_from<ILoxObject> T, class ...TArgs>
    requires constructible_from<T, TArgs...>
inline lox_object_ptr_t make_object(TArgs &&...args)
{
    return lox_object_ptr_t{make_shared<T>(forward<TArgs>(args)...)};
}

struct ILoxCallable;
struct ILoxInstance;
struct token_t;

class LoxValue {
    variant<nil_t, bool, string, f64, lox_object_ptr_t> m_val{nil_t{}};

public:
    LoxValue() = default;

    LoxValue(LoxValue const &) = default;
    LoxValue(LoxValue &&) = default;
    LoxValue &operator=(LoxValue const &) = default;
    LoxValue &operator=(LoxValue &&) = default;

    LoxValue(nil_t) : m_val{nil_t{}} {}
    LoxValue &operator=(nil_t) {
        m_val = nil_t{};
        return *this;
    }
    LoxValue(bool b) : m_val{b} {}
    LoxValue &operator=(bool b) {
        m_val = b;
        return *this;
    }
    LoxValue(string const &s) : m_val{s} {}
    LoxValue(string &&s) : m_val{move(s)} {}
    LoxValue &operator=(string const &s) {
        m_val = s;
        return *this;
    }
    LoxValue &operator=(string &&s) {
        m_val = move(s);
        return *this;
    }
    LoxValue(f64 d) : m_val{d} {}
    LoxValue &operator=(f64 d) {
        m_val = d;
        return *this;
    }
    LoxValue(lox_object_ptr_t const &e) : m_val{e} {}
    LoxValue(lox_object_ptr_t &&e) : m_val{move(e)} {}
    LoxValue &operator=(lox_object_ptr_t const &e) {
        m_val = e;
        return *this;
    }
    LoxValue &operator=(lox_object_ptr_t &&e) {
        m_val = move(e);
        return *this;
    }

    bool IsNil() const { return holds_alternative<nil_t>(m_val); }
    bool IsBool() const { return holds_alternative<bool>(m_val); }
    bool IsString() const { return holds_alternative<string>(m_val); }
    bool IsNumber() const { return holds_alternative<f64>(m_val); }
    bool IsObject() const { return holds_alternative<lox_object_ptr_t>(m_val); }

    bool IsCallable() const;
    bool IsInstance() const;

    bool &GetBool() {
        assert(IsBool());
        return get<bool>(m_val);
    }
    bool GetBool() const
        { return const_cast<LoxValue *>(this)->GetBool(); }
    string &GetString() {
        assert(IsString());
        return get<string>(m_val);
    }
    string const &GetString() const
        { return const_cast<LoxValue *>(this)->GetString(); }
    f64 &GetNumber() {
        assert(IsNumber());
        return get<f64>(m_val);
    }
    f64 GetNumber() const
        { return const_cast<LoxValue *>(this)->GetNumber(); }
    lox_object_ptr_t &GetObject() {
        assert(IsObject());
        return get<lox_object_ptr_t>(m_val);
    }
    lox_object_ptr_t const &GetObject() const
        { return const_cast<LoxValue *>(this)->GetObject(); }

    ILoxCallable &GetCallable();
    ILoxCallable const &GetCallable() const
        { return const_cast<LoxValue *>(this)->GetCallable(); }
    ILoxInstance &GetInstance();
    ILoxInstance const &GetInstance() const
        { return const_cast<LoxValue *>(this)->GetInstance(); }
};

class Interpreter;

struct ILoxCallable : virtual ILoxObject {
    virtual LoxValue Call(Interpreter &, span<LoxValue>) = 0;
    virtual int Arity() const = 0;
};

struct ILoxInstance : virtual ILoxObject {
    virtual LoxValue Get(Interpreter &, token_t const &) = 0;
    virtual void Set(token_t const &, LoxValue const &) = 0;
};

inline bool LoxValue::IsCallable() const
{
    return IsObject() && dynamic_cast<ILoxCallable const *>(GetObject().get());
}
inline bool LoxValue::IsInstance() const
{
    return IsObject() && dynamic_cast<ILoxInstance const *>(GetObject().get());
}

inline ILoxCallable &LoxValue::GetCallable()
{
    assert(IsCallable());
    return *dynamic_cast<ILoxCallable *>(GetObject().get());
}
inline ILoxInstance &LoxValue::GetInstance()
{
    assert(IsInstance());
    return *dynamic_cast<ILoxInstance *>(GetObject().get());
}

inline const LoxValue c_nil{};

inline bool is_truthy(LoxValue const &val)
{
    if (val.IsNil())
        return false;
    else if (val.IsBool())
        return val.GetBool();
    else
        return true; // @HUH: empty string and 0 to false too?
}

inline bool are_equal(LoxValue const &l, LoxValue const &r)
{
    if (l.IsNil() && r.IsNil())
        return true;
    if (l.IsBool() && r.IsBool())
        return l.GetBool() == r.GetBool();
    if (l.IsString() && r.IsString())
        return l.GetString() == r.GetString();
    if (l.IsNumber() && r.IsNumber())
        return abs(l.GetNumber() - r.GetNumber()) < DBL_EPSILON;
    else
        return false;
}

inline string to_string(LoxValue const &val)
{
    if (val.IsNil()) {
        return "nil";
    } else if (val.IsString()) {
        return val.GetString();
    } else if (val.IsBool()) {
        return val.GetBool() ? "true" : "false";
    } else if (val.IsNumber()) {
        string s = to_string(val.GetNumber());
        if (s.find('.') != string::npos) {
            size_t nl = s.length();
            for (; nl > 0 && s[nl - 1] == '0'; --nl)
                ;
            if (nl > 0 && s[nl - 1] == '.')
                --nl;
            s.resize(nl);
        }
        return s;
    } else { // object
        return val.GetObject()->ToString();
    }
}

inline string to_dbg_string(LoxValue const &val)
{
    if (val.IsString())
        return format("\"{}\"", to_string(val));
    else
        return to_string(val);
}
