#include "symbol_table.h"

// ── Type registry ────────────────────────────────────────────────────────────

void symbol_table::register_type(const std::string& name, type_info info) {
    types_[name] = std::move(info);
}

type_info* symbol_table::lookup_type(const std::string& name) {
    auto it = types_.find(name);
    if (it == types_.end()) {
        return nullptr;
    }
    return &it->second;
}

// ── Function registry ────────────────────────────────────────────────────────

void symbol_table::register_function(const std::string& name, type_ptr type) {
    functions_[name] = std::move(type);
}

type_ptr symbol_table::lookup_function(const std::string& name) const {
    auto it = functions_.find(name);
    if (it == functions_.end()) {
        return nullptr;
    }
    return it->second;
}

// ── Scope management ─────────────────────────────────────────────────────────

void symbol_table::push_scope() { scopes_.emplace_back(); }

void symbol_table::pop_scope() { scopes_.pop_back(); }

void symbol_table::bind(const std::string& name, symbol_entry entry) {
    scopes_.back()[name] = std::move(entry);
}

symbol_entry* symbol_table::lookup(const std::string& name) {
    // Search from innermost to outermost scope
    for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return &found->second;
        }
    }
    return nullptr;
}
