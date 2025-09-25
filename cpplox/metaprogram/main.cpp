#include <defs.hpp>

struct field_t {
    string_view type;
    string_view name;
};

using ast_def_t = unordered_map<string_view, vector<field_t>>;

void define_ast(FILE *f, string_view base, ast_def_t &&def)
{
    println(f, "#pragma once\n");
    println(f, "#include <lox.hpp>");
    println(f, "#include <tokens.hpp>\n");

    println(f, "struct {} : {{", base);
    println(f, "    virtual ~{}() {{}}", base);
    println(f, "}};\n");

    for (const auto &[node, fields] : def) {
        println(f, "struct {} : {} {{", node, base);
        for (auto [type, name] : fields)
            println(f, "    {} {};", type, name);
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
                {"Expr", "left"}, 
                {"token_t", "operator"}, 
                {"Expr", "right"}
            }},
            {"Grouping", {{"Expr", "expression"}}},
            {"Literal", {{"LoxObject", "value"}}},
            {"Unary", {
                {"token_t", "operator"},
                {"Expr", "right"}
            }}
        });
}
