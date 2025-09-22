#include <defs.hpp>

enum errc_t {
    e_ec_ok = 0,
    e_ec_arg_error = 1,
    e_ec_sys_error = 2,
    e_ec_code_error = 13,
};

struct lox_t {
    bool had_error = false;
};

void report(lox_t &lox, int line, string_view location, string_view message)
{
    println(stderr, "[line {}] Error {}: {}", line, location, message);
    lox.had_error = true;
}

void error(lox_t &lox, int line, string_view message)
{
    report(lox, line, "", message);
}

// @TODO
struct token_t : string_view {};

vector<token_t> scan_tokens(string_view code, lox_t &lox)
{
    // @TODO
    return {};
}

errc_t run(string_view code, lox_t &lox)
{
    vector<token_t> tokens = scan_tokens(code, lox);
    if (lox.had_error)
        return e_ec_code_error;
    
    // @TEST
    for (auto const &tok : tokens)
        println("{}", tok);

    return e_ec_ok;
}

optional<string> read_whole_file(char const *fn)
{
    FILE *f = fopen(fn, "rb");
    if (!f) {
        perror(fn);
        return nullopt;
    }
    DEFER(fclose(f));

    optional<string> res;
    res.emplace();

    fseek(f, 0, SEEK_END);
    usize len = ftell(f);
    fseek(f, 0, SEEK_SET);

    res->resize(len);
    if (fread(res->data(), len, 1, f) != 1) {
        perror(fn);
        return nullopt;
    }
        
    return res;
}

errc_t run_file(char const *fn, lox_t &lox)
{
    if (auto bytes = read_whole_file(fn))
        return run(*bytes, lox);
    else
        return e_ec_sys_error;
}

errc_t run_prompt(lox_t &lox)
{
    for (;;) {
        print("> ");
        string line{};

        getline(cin, line);
        if (!cin.good())
            break;

        run(line, lox);
        lox.had_error = false;
    }

    if (!cin.eof()) {
        println(stderr, "Input error (unknown)");
        return e_ec_sys_error;
    }

    return e_ec_ok;
}

void show_header()
{
    println("jb-cpplox, version {}", VERSION);
}

int main(int argc, char **argv)
{
    span<char const *const> args{argv + 1, usize(argc - 1)};

    lox_t lox{};
    
    if (args.size() > 1) {
        show_header();
        println(stderr, "Usage: cpplox [file]");
        return e_ec_arg_error;
    } else if (args.size() == 1) {
        return run_file(args[0], lox);
    } else {
        show_header();
        return run_prompt(lox);
    }
}
