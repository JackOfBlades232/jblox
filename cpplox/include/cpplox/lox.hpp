#pragma once

#include "defs.hpp"

struct lox_t {
    bool had_error = false;
};

struct nil_t {};

// @WIP
class LoxObject {
    variant<nil_t, bool, string, double> m_val{nil_t{}};

public:
    LoxObject() = default;

    LoxObject(const LoxObject &) = default;
    LoxObject(LoxObject &&) = default;
    LoxObject &operator=(const LoxObject &) = default;
    LoxObject &operator=(LoxObject &&) = default;

    LoxObject(nil_t) : m_val{nil_t{}} {}
    LoxObject &operator=(nil_t) {
        m_val = nil_t{};
        return *this;
    }
    LoxObject(bool b) : m_val{b} {}
    LoxObject &operator=(bool b) {
        m_val = b;
        return *this;
    }
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
    bool IsBool() const { return holds_alternative<bool>(m_val); }
    bool IsString() const { return holds_alternative<string>(m_val); }
    bool IsNumber() const { return holds_alternative<double>(m_val); }

    bool &GetBool() {
        assert(IsBool());
        return get<bool>(m_val);
    }
    bool GetBool() const
        { return const_cast<LoxObject *>(this)->GetBool(); }
    string &GetString() {
        assert(IsString());
        return get<string>(m_val);
    }
    string_view GetString() const
        { return string_view{const_cast<LoxObject *>(this)->GetString()}; }
    double &GetNumber() {
        assert(IsNumber());
        return get<double>(m_val);
    }
    double GetNumber() const
        { return const_cast<LoxObject *>(this)->GetNumber(); }
};

inline const LoxObject c_nil{};

inline string to_string(const LoxObject &obj)
{
    if (obj.IsNil())
        return "nil";
    else if (obj.IsString())
        return format("\"{}\"", obj.GetString());
    else if (obj.IsBool())
        return obj.GetBool() ? "true" : "false";
    else // number
        return to_string(obj.GetNumber());
}

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
