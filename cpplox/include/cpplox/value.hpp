#pragma once

#include "defs.hpp"

struct nil_t {};

class LoxValue {
    variant<nil_t, bool, string, double> m_val{nil_t{}};

public:
    LoxValue() = default;

    LoxValue(const LoxValue &) = default;
    LoxValue(LoxValue &&) = default;
    LoxValue &operator=(const LoxValue &) = default;
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
    LoxValue(const string &s) : m_val{s} {}
    LoxValue(string &&s) : m_val{move(s)} {}
    LoxValue &operator=(const string &s) {
        m_val = s;
        return *this;
    }
    LoxValue &operator=(string &&s) {
        m_val = move(s);
        return *this;
    }
    LoxValue(double d) : m_val{d} {}
    LoxValue &operator=(double d) {
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
        { return const_cast<LoxValue *>(this)->GetBool(); }
    string &GetString() {
        assert(IsString());
        return get<string>(m_val);
    }
    string const &GetString() const
        { return const_cast<LoxValue *>(this)->GetString(); }
    double &GetNumber() {
        assert(IsNumber());
        return get<double>(m_val);
    }
    double GetNumber() const
        { return const_cast<LoxValue *>(this)->GetNumber(); }
};

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
    } else {
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
    }
}

inline string to_dbg_string(LoxValue const &val)
{
    if (val.IsString())
        return format("\"{}\"", to_string(val));
    else
        return to_string(val);
}
