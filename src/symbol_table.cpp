#include "symbol_table.h"

#include <ranges>

// ── Type info ────────────────────────────────────────────────────────────────

type_ptr type_info::find_field(const std::string& label) const {
    for (const type_info* t = this; t != nullptr; t = t->parent) {
        if (auto found = t->field_row->find_entry(label)) {
            return found;
        }
    }
    return nullptr;
}

type_ptr type_info::find_method(const std::string& name) const {
    return method_row->find_entry(name);
}

bool type_info::inherits_from(const type_info* other) const {
    for (const type_info* t = this; t != nullptr; t = t->parent) {
        if (t == other) {
            return true;
        }
    }
    return false;
}

// ── Type registry ────────────────────────────────────────────────────────────

type_info& symbol_table::register_type(const std::string& name) {
    type_info& info = types_[name];
    info.name = name;
    info.field_row = std::make_shared<row_type_t>(std::vector<row_entry>{}, nullptr);
    info.method_row = std::make_shared<row_type_t>(std::vector<row_entry>{}, nullptr);
    return info;
}

type_info* symbol_table::lookup_type(const std::string& name) {
    auto it = types_.find(name);
    return it == types_.end() ? nullptr : &it->second;
}

// ── Function registry ────────────────────────────────────────────────────────

void symbol_table::register_function(const std::string& name, type_ptr type) {
    functions_[name] = std::move(type);
}

type_ptr symbol_table::lookup_function(const std::string& name) const {
    auto it = functions_.find(name);
    return it == functions_.end() ? nullptr : it->second;
}

// ── Scope management ─────────────────────────────────────────────────────────

void symbol_table::push_scope() { scopes_.emplace_back(); }

void symbol_table::pop_scope() { scopes_.pop_back(); }

void symbol_table::bind(const std::string& name, symbol_entry entry) {
    scopes_.back()[name] = std::move(entry);
}

symbol_entry* symbol_table::lookup(const std::string& name) {
    for (auto& scope : std::views::reverse(scopes_)) {
        if (auto found = scope.find(name); found != scope.end()) {
            return &found->second;
        }
    }
    return nullptr;
}
