#include <defs.hpp>

struct field_t {
    string_view type;
    string_view name;
};

using ast_def_t = unordered_map<string_view, vector<field_t>>;

void define_header(FILE *f)
{
    println(f, "#pragma once\n");
    println(f, "#include <defs.hpp>");
    println(f, "#include <value.hpp>");
    println(f, "#include <tokens.hpp>\n");
}

void define_ast(FILE *f, string_view base, ast_def_t &&def)
{
    string base_lc{};
    base_lc.resize(base.size());
    transform(
        base.begin(), base.end(), base_lc.begin(),
        [](char c) { return tolower(c); });

    for (const auto &[node, _] : def)
        println(f, "struct {}{};", node, base);

    println(f, "\nstruct I{}Visitor {{", base);
    for (const auto &[node, _] : def) {
        println(f,
            "    virtual void Visit{}{}({}{} const &) = 0;",
            node, base, node, base);
    }
    println(f, "}};\n");

    println(f, "struct {} {{", base);
    println(f, "    virtual ~{}() {{}}", base);
    println(f, "    virtual void Accept(I{}Visitor &) const = 0;", base);
    println(f, "}};\n");

    println(f, "using {}_ptr_t = shared_ptr<{}>;\n", base_lc, base);

    for (const auto &[node, fields] : def) {
        println(f, "struct {}{} : {} {{", node, base, base);
        for (auto [type, name] : fields)
            println(f, "    {} {};", type, name);
        print(f, "\n    {}{}(", node, base);
        for (size_t i = 0; auto [type, name] : fields) {
            print(f, "\n            {} a_{}", type, name);
            if (i++ != fields.size() - 1)
                print(f, ",");
        }
        print(f, ")");
        for (size_t i = 0; auto [type, name] : fields) {
            print(f, "\n        {} {}{{move(a_{})}}",
                i++ == 0 ? ':' : ',', name, name);
        }
        println(f, " {{}}");
        println(f,
            "    void Accept(I{}Visitor &v) const override "
            "{{ v.Visit{}{}(*this); }}", base, node, base);
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

    define_header(astf);
    define_ast(astf, "Expr",
        {
            {"Assignment", {
                {"token_t", "target"},
                {"expr_ptr_t", "val"}
            }},
            {"Binary", {
                {"expr_ptr_t", "left"}, 
                {"token_t", "op"}, 
                {"expr_ptr_t", "right"}
            }},
            {"Ternary", {
                {"expr_ptr_t", "first"}, 
                {"token_t", "op0"}, 
                {"expr_ptr_t", "second"},
                {"token_t", "op1"}, 
                {"expr_ptr_t", "third"}
            }},
            {"Call", {
                {"expr_ptr_t", "callee"},
                {"token_t", "paren"},
                {"vector<expr_ptr_t>", "args"}
            }},
            {"Grouping", {{"expr_ptr_t", "expr"}}},
            {"Literal", {{"LoxValue", "value"}}},
            {"Unary", {
                {"token_t", "op"},
                {"expr_ptr_t", "right"}
            }},
            {"Variable", {{"token_t", "id"}}}
        });
    define_ast(astf, "Stmt",
        {
            {"Block", {{"vector<stmt_ptr_t>", "stmts"}}},
            {"Expression", {{"expr_ptr_t", "expr"}}},
            {"If", {
                {"expr_ptr_t", "cond"},
                {"stmt_ptr_t", "then_branch"},
                {"stmt_ptr_t", "else_branch"},
            }},
            {"While", {
                {"expr_ptr_t", "cond"},
                {"stmt_ptr_t", "body"}
            }},
            {"Print", {{"expr_ptr_t", "val"}}},
            {"Break", {}},
            {"Var", {
                {"token_t", "id"},
                {"expr_ptr_t", "init"}
            }},
            {"ReplExpr", {{"expr_ptr_t", "expr"}}},
        });
}
