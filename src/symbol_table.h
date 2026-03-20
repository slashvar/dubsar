#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <string>
#include <unordered_map>
#include <vector>

#include "types.h"

class ast_node;

// ── Symbol entry ─────────────────────────────────────────────────────────────

struct symbol_entry {
    std::string name;
    type_ptr type;
    bool is_ref = false;
    const ast_node* decl_site = nullptr;
};

// ── Type info (registered struct/interface) ──────────────────────────────────

struct type_info {
    std::string name;
    type_ptr field_row;   // row_type_t for struct fields
    type_ptr method_row;  // row_type_t for methods
    type_info* parent = nullptr;
    bool is_interface = false;
};

// ── Scoped symbol table ──────────────────────────────────────────────────────

class symbol_table {
public:
    // Type registry (global)
    void register_type(const std::string& name, type_info info);
    type_info* lookup_type(const std::string& name);

    // Function registry (global)
    void register_function(const std::string& name, type_ptr type);
    type_ptr lookup_function(const std::string& name) const;

    // Scope management
    void push_scope();
    void pop_scope();
    void bind(const std::string& name, symbol_entry entry);
    symbol_entry* lookup(const std::string& name);

private:
    std::unordered_map<std::string, type_info> types_;
    std::unordered_map<std::string, type_ptr> functions_;
    std::vector<std::unordered_map<std::string, symbol_entry>> scopes_;
};

#endif  // SYMBOL_TABLE_H
