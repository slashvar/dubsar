#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <string>

#include "ast.h"
#include "printer.h"

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
    if (argc != 2) {
        std::cerr << std::format("Usage: {} <input-file>\n", argv[0]);
        return 1;
    }

    const std::filesystem::path input_path{argv[1]};

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
        printer p{std::cout};
        root->accept(p);
        std::cout << '\n';
    }

    return 0;
}
