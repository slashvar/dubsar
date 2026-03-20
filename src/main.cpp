#include <argparse/argparse.hpp>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <string>

#include "ast.h"
#include "diagnostics.h"
#include "printer.h"
#include "resolver.h"
#include "symbol_table.h"
#include "type_checker.h"
#include "unify.h"

extern int yyparse();

// Flex buffer API. The concrete yy_buffer_state type is defined in the
// generated lexer.cpp; only an opaque forward declaration is needed here.
struct yy_buffer_state;
extern yy_buffer_state* yy_scan_string(
    const char*);  // NOLINT(cppcoreguidelines-owning-memory,readability-identifier-naming)
extern void yy_delete_buffer(
    yy_buffer_state*);  // NOLINT(cppcoreguidelines-owning-memory,readability-identifier-naming)

namespace {
struct lex_buffer_deleter {
    void operator()(yy_buffer_state* b) const noexcept { yy_delete_buffer(b); }
};
using unique_lex_buffer = std::unique_ptr<yy_buffer_state, lex_buffer_deleter>;
}  // namespace

int main(int argc, char** argv) {
    argparse::ArgumentParser program("dubsar", "0.1.0");
    program.add_argument("input-file").help("source file to compile");
    program.add_argument("--no-check")
        .help("skip type checking (parse and print only)")
        .default_value(false)
        .implicit_value(true);

    try {
        program.parse_args(argc, argv);
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        std::cerr << program;
        return 1;
    }

    auto input_file = program.get<std::string>("input-file");
    auto no_check = program.get<bool>("--no-check");

    const std::filesystem::path input_path{input_file};

    std::ifstream file{input_path};
    if (!file) {
        std::cerr << std::format("Error: Cannot open file '{}'\n", input_path.string());
        return 1;
    }

    const std::string content{std::istreambuf_iterator<char>(file), {}};
    const unique_lex_buffer buffer{yy_scan_string(content.c_str())};

    if (const int result = yyparse(); result != 0) {
        std::cerr << std::format("Parse failed with error code {}\n", result);
        return 1;
    }

    if (root) {
        // Semantic passes (unless --no-check)
        if (!no_check) {
            diagnostics diag;
            symbol_table symtab;
            type_env env;

            resolver res{symtab, env, diag};
            res.resolve(*root);

            if (!diag.has_errors()) {
                type_checker tc{symtab, env, diag};
                root->accept(tc);
            }

            if (diag.has_errors()) {
                diag.emit(std::cerr);
                return 1;
            }
        }

        printer p{std::cout};
        root->accept(p);
        std::cout << '\n';
    }

    return 0;
}
