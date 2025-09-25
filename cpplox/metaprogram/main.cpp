#include <defs.hpp>

enum field_type_t {
    e_ft_obj,
    e_ft_ptr
};

struct field_t {
    string_view type;
    string_view name;
    field_type_t kind = e_ft_obj;
};

using ast_def_t = unordered_map<string_view, vector<field_t>>;

void define_ast(FILE *f, string_view base, ast_def_t &&def)
{
    println(f, "#pragma once\n");
    println(f, "#include <lox.hpp>");
    println(f, "#include <tokens.hpp>\n");

    for (const auto &[node, _] : def)
        println(f, "struct {}{};", node, base);

    println(f, "\nstruct IVisitor {{", base);
    for (const auto &[node, _] : def) {
        println(f,
            "    virtual void Visit{}{}(const {}{} &) = 0;",
            node, base, node, base);
    }
    println(f, "}};\n");

    println(f, "struct {} {{", base);
    println(f, "    virtual ~{}() {{}}", base);
    println(f, "    virtual void Accept(IVisitor &) const = 0;");
    println(f, "}};\n");

    for (const auto &[node, fields] : def) {
        println(f, "struct {}{} : {} {{", node, base, base);
        bool needs_two_constructors = false, needs_cleanup = false;
        for (auto [type, name, kind] : fields) {
            if (kind == e_ft_obj) {
                println(f, "    {} {};", type, name);
                needs_two_constructors = true;
            } else if (kind == e_ft_ptr) {
                println(f, "    {} *{};", type, name);
                needs_cleanup = true;
            }
        }
        print(f, "    {}{}(", node, base);
        if (!fields.empty()) {
            auto [type, name, kind] = fields[0];
            print(f, "{} {}{}", type, kind == e_ft_ptr ? "*" : "&&", name);
        }
        for (auto [type, name, kind] : span{fields}.subspan(1))
            print(f, ", {} {}{}", type, kind == e_ft_ptr ? "*" : "&&", name);
        println(f, ")");
        print(f, "        : ");
        if (!fields.empty()) {
            auto [_, name, kind] = fields[0];
            if (kind == e_ft_ptr)
                print(f, "{}{{{}}}", name, name);
            else
                print(f, "{}{{move({})}}", name, name);
        }
        for (auto [_, name, kind] : span{fields}.subspan(1)) {
            if (kind == e_ft_ptr)
                print(f, ", {}{{{}}}", name, name);
            else
                print(f, ", {}{{move({})}}", name, name);
        }
        println(f, " {{}}");
        if (needs_two_constructors) {
            print(f, "    {}{}(", node, base);
            if (!fields.empty()) {
                auto [type, name, kind] = fields[0];
                if (kind == e_ft_obj)
                    print(f, "const {} &{}", type, name);
                else
                    print(f, "{} *{}", type, name);
            }
            for (auto [type, name, kind] : span{fields}.subspan(1)) {
                if (kind == e_ft_obj)
                    print(f, ", const {} &{}", type, name);
                else
                    print(f, ", {} *{}", type, name);
            }
            println(f, ")");
            print(f, "        : ");
            if (!fields.empty()) {
                auto [_1, name, _2] = fields[0];
                print(f, "{}{{{}}}", name, name);
            }
            for (auto [_1, name, _2] : span{fields}.subspan(1))
                print(f, ", {}{{{}}}", name, name);
            println(f, " {{}}");
        }
        if (needs_cleanup) {
            println(f, "    ~{}{}() {{", node, base);
            for (auto [_, name, kind] : fields) {
                if (kind == e_ft_ptr)
                    println(f, "        delete {};", name);
            }
            println(f, "    }}");
        }
        println(f,
            "    void Accept(IVisitor &v) const override "
            "{{ v.Visit{}{}(*this); }}", node, base);
        println(f, "}};\n");
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        println(stderr, "Usage: metaprogram [out dir]");
        return 1;
    }

    char const *out_dir = argv[1];
    string const ast_fn = format("{}/ast.hpp", out_dir);

    FILE *astf = fopen(ast_fn.c_str(), "wb");
    if (!astf) {
        println(stderr, "Failed to open {} for write.", ast_fn);
        return 1;
    }
    DEFER(fclose(astf));

    define_ast(astf, "Expr",
        {
            {"Binary", {
                {"Expr", "left", e_ft_ptr}, 
                {"token_t", "op"}, 
                {"Expr", "right", e_ft_ptr}
            }},
            {"Grouping", {{"Expr", "expr", e_ft_ptr}}},
            {"Literal", {{"LoxObject", "value"}}},
            {"Unary", {
                {"token_t", "op"},
                {"Expr", "right", e_ft_ptr}
            }}
        });
}
