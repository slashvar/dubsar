#include "resolver.h"

#include <format>
#include <unordered_set>

#include "ast.h"

// ── Helpers ──────────────────────────────────────────────────────────────────

type_ptr resolver::resolve_type_or_fresh(const std::string& type_str) {
    if (type_str.empty()) {
        return env_.fresh_var();
    }
    return parse_type_string(type_str);
}

type_ptr resolver::build_fun_type(const std::vector<std::unique_ptr<param_node>>& params,
                                  const std::string& return_type) {
    std::vector<type_ptr> param_types;
    param_types.reserve(params.size());
    for (const auto& p : params) {
        param_types.push_back(resolve_type_or_fresh(p->type));
    }
    auto ret = resolve_type_or_fresh(return_type);
    return std::make_shared<fun_type_t>(std::move(param_types), std::move(ret));
}

// ── Entry point ──────────────────────────────────────────────────────────────

void resolver::resolve(const program_node& prog) {
    // Pass 1: register type names
    current_pass_ = pass::register_names;
    prog.accept(*this);

    // Pass 2: fill in details (fields, methods, function signatures)
    current_pass_ = pass::fill_details;
    prog.accept(*this);
}

// ── Visitor implementations ──────────────────────────────────────────────────

void resolver::visit(const program_node& node) {
    for (const auto& decl : node.declarations) {
        decl->accept(*this);
    }
}

void resolver::visit(const type_decl_node& node) {
    if (current_pass_ == pass::register_names) {
        if (symtab_.lookup_type(node.name) != nullptr) {
            diag_.error(node, std::format("duplicate type name '{}'", node.name));
            return;
        }
        type_info info;
        info.name = node.name;
        symtab_.register_type(node.name, std::move(info));
    } else {
        // Set context for the body visitor
        current_type_name_ = node.name;
        node.body->accept(*this);
        current_type_name_.clear();
    }
}

void resolver::visit(const struct_type_node& node) {
    if (current_pass_ != pass::fill_details) {
        return;
    }

    auto* ti = symtab_.lookup_type(current_type_name_);
    if (ti == nullptr) {
        return;  // Already reported
    }

    // Build field row
    std::vector<row_entry> fields;
    std::unordered_set<std::string> seen_fields;
    for (const auto& f : node.fields) {
        auto field_type = resolve_type_or_fresh(f->type);
        if (!seen_fields.insert(f->name).second) {
            diag_.error(*f, std::format("duplicate field '{}' in struct '{}'", f->name,
                                        current_type_name_));
        }
        fields.push_back({f->name, std::move(field_type)});
    }

    // Handle parent type (inheritance)
    type_info* parent_info = nullptr;
    if (!node.parent.empty()) {
        parent_info = symtab_.lookup_type(node.parent);
        if (parent_info == nullptr) {
            diag_.error(0, std::format("undefined parent type '{}' for struct '{}'", node.parent,
                                       current_type_name_));
        } else if (parent_info->is_interface) {
            diag_.error(0, std::format("cannot inherit from interface '{}'", node.parent));
            parent_info = nullptr;
        } else if (parent_info->field_row != nullptr) {
            // Prepend parent fields
            auto* parent_row = static_cast<row_type_t*>(parent_info->field_row.get());
            std::vector<row_entry> all_fields;
            all_fields.reserve(parent_row->entries.size() + fields.size());
            for (const auto& e : parent_row->entries) {
                all_fields.push_back(e);
            }
            for (auto& e : fields) {
                all_fields.push_back(std::move(e));
            }
            fields = std::move(all_fields);
        }
    }

    // Structs are closed rows (tail = nullptr)
    ti->field_row = std::make_shared<row_type_t>(std::move(fields), nullptr);
    ti->method_row = std::make_shared<row_type_t>(std::vector<row_entry>{}, nullptr);
    ti->parent = parent_info;
    ti->is_interface = false;
}

void resolver::visit(const interface_type_node& node) {
    if (current_pass_ != pass::fill_details) {
        return;
    }

    auto* ti = symtab_.lookup_type(current_type_name_);
    if (ti == nullptr) {
        return;
    }

    // Interfaces have no fields, only methods.
    // Methods form an open row (tail = fresh type variable ρ).
    std::vector<row_entry> methods;
    for (const auto& m : node.methods) {
        auto method_type = build_fun_type(m->params, m->return_type);
        methods.push_back({m->name, std::move(method_type)});
    }

    ti->field_row = std::make_shared<row_type_t>(std::vector<row_entry>{}, nullptr);
    // Interface method row is OPEN (tail = fresh var for row polymorphism)
    ti->method_row = std::make_shared<row_type_t>(std::move(methods), env_.fresh_var());
    ti->is_interface = true;
}

void resolver::visit(const func_decl_node& node) {
    if (current_pass_ != pass::fill_details) {
        return;
    }

    if (symtab_.lookup_function(node.name) != nullptr) {
        diag_.error(node, std::format("duplicate function name '{}'", node.name));
        return;
    }

    auto fun_type = build_fun_type(node.params, node.return_type);
    symtab_.register_function(node.name, fun_type);
}

void resolver::visit(const method_decl_node& node) {
    if (current_pass_ != pass::fill_details) {
        return;
    }

    auto* ti = symtab_.lookup_type(node.type_name);
    if (ti == nullptr) {
        diag_.error(node, std::format("method on undefined type '{}'", node.type_name));
        return;
    }

    auto method_type = build_fun_type(node.params, node.return_type);

    // Add method to the type's method row
    if (ti->method_row != nullptr) {
        auto* mr = static_cast<row_type_t*>(ti->method_row.get());
        mr->entries.push_back({node.name, std::move(method_type)});
    } else {
        std::vector<row_entry> entries;
        entries.push_back({node.name, std::move(method_type)});
        ti->method_row = std::make_shared<row_type_t>(std::move(entries), nullptr);
    }
}
