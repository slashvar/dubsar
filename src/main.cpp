#include <cstring>
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
    bool no_check = false;
    const char* input_file = nullptr;

    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "--no-check") == 0) {
            no_check = true;
        } else if (input_file == nullptr) {
            input_file = argv[i];
        } else {
            std::cerr << std::format("Usage: {} [--no-check] <input-file>\n", argv[0]);
            return 1;
        }
    }

    if (input_file == nullptr) {
        std::cerr << std::format("Usage: {} [--no-check] <input-file>\n", argv[0]);
        return 1;
    }

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
