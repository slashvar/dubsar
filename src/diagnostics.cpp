#include "diagnostics.h"

#include <algorithm>
#include <format>

#include "ast.h"

void diagnostics::error(const ast_node& node, std::string message) {
    entries_.push_back({severity::error, node.line, std::move(message)});
}

void diagnostics::error(int line, std::string message) {
    entries_.push_back({severity::error, line, std::move(message)});
}

void diagnostics::warning(const ast_node& node, std::string message) {
    entries_.push_back({severity::warning, node.line, std::move(message)});
}

void diagnostics::warning(int line, std::string message) {
    entries_.push_back({severity::warning, line, std::move(message)});
}

bool diagnostics::has_errors() const noexcept {
    return std::ranges::any_of(entries_,
                               [](const diag_entry& e) { return e.sev == severity::error; });
}

bool diagnostics::has_diagnostics() const noexcept { return !entries_.empty(); }

void diagnostics::emit(std::ostream& err) const {
    for (const auto& e : entries_) {
        const char* prefix = e.sev == severity::error ? "error" : "warning";
        err << std::format("line {}: {}: {}\n", e.line, prefix, e.message);
    }
}
