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

// A declared struct or interface.  Both rows are non-null once registered.
struct type_info {
    std::string name;
    row_ptr field_row;
    row_ptr method_row;
    type_info* parent = nullptr;
    bool is_interface = false;

    // Looks the field up on this type, then on its ancestors.
    [[nodiscard]] type_ptr find_field(const std::string& label) const;

    // Looks the method up on this type only; methods are not inherited.
    [[nodiscard]] type_ptr find_method(const std::string& name) const;

    // True when `other` is this type or one of its ancestors.
    [[nodiscard]] bool inherits_from(const type_info* other) const;
};

// ── Scoped symbol table ──────────────────────────────────────────────────────

class symbol_table {
public:
    // Registers `name` with empty rows.  The caller must check for a duplicate
    // first: re-registering an existing name replaces it.
    type_info& register_type(const std::string& name);
    type_info* lookup_type(const std::string& name);

    void register_function(const std::string& name, type_ptr type);
    [[nodiscard]] type_ptr lookup_function(const std::string& name) const;

    void push_scope();
    void pop_scope();
    void bind(const std::string& name, symbol_entry entry);

    // Searches innermost scope outwards.  Returns nullptr when unbound.
    symbol_entry* lookup(const std::string& name);

private:
    std::unordered_map<std::string, type_info> types_;
    std::unordered_map<std::string, type_ptr> functions_;
    // Index 0 is the global scope, so a binding is always possible.
    std::vector<std::unordered_map<std::string, symbol_entry>> scopes_{1};
};

#endif  // SYMBOL_TABLE_H
