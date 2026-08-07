#include "resolver.h"

#include <format>
#include <unordered_set>

#include "ast.h"

// ── Helpers ──────────────────────────────────────────────────────────────────

type_ptr resolver::build_fun_type(const std::vector<std::unique_ptr<param_node>>& params,
                                  const std::string& return_type) {
    std::vector<type_ptr> param_types;
    param_types.reserve(params.size());
    for (const auto& p : params) {
        param_types.push_back(type_or_fresh(env_, p->type));
    }
    return std::make_shared<fun_type_t>(std::move(param_types), type_or_fresh(env_, return_type));
}

// ── Entry point ──────────────────────────────────────────────────────────────

void resolver::resolve(const program_node& prog) {
    for (const pass p : {pass::register_names, pass::fill_types, pass::link}) {
        current_pass_ = p;
        prog.accept(*this);
    }
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
        symtab_.register_type(node.name);
        return;
    }
    // The body needs to know which type it belongs to.
    current_type_name_ = node.name;
    node.body->accept(*this);
    current_type_name_.clear();
}

void resolver::visit(const struct_type_node& node) {
    auto* ti = symtab_.lookup_type(current_type_name_);
    if (ti == nullptr) {
        return;  // Duplicate type name, already reported.
    }

    if (current_pass_ == pass::fill_types) {
        std::unordered_set<std::string> seen;
        for (const auto& f : node.fields) {
            if (!seen.insert(f->name).second) {
                diag_.error(*f, std::format("duplicate field '{}' in struct '{}'", f->name,
                                            current_type_name_));
            }
            ti->field_row->entries.push_back({f->name, type_or_fresh(env_, f->type)});
        }
        return;
    }

    // Parents are linked only once every type is complete, so the checks below
    // do not depend on declaration order.
    if (node.parent.empty()) {
        return;
    }
    auto* parent = symtab_.lookup_type(node.parent);
    if (parent == nullptr) {
        diag_.error(node, std::format("undefined parent type '{}' for struct '{}'", node.parent,
                                      current_type_name_));
    } else if (parent->is_interface) {
        diag_.error(node, std::format("cannot inherit from interface '{}'", node.parent));
    } else if (parent->inherits_from(ti)) {
        diag_.error(node, std::format("cyclic inheritance involving '{}'", current_type_name_));
    } else {
        ti->parent = parent;
    }
}

void resolver::visit(const interface_type_node& node) {
    if (current_pass_ != pass::fill_types) {
        return;
    }
    auto* ti = symtab_.lookup_type(current_type_name_);
    if (ti == nullptr) {
        return;
    }

    for (const auto& m : node.methods) {
        ti->method_row->entries.push_back({m->name, build_fun_type(m->params, m->return_type)});
    }
    // An open tail lets any type with at least these methods satisfy the row.
    ti->method_row->tail = env_.fresh_var();
    ti->is_interface = true;
}

void resolver::visit(const func_decl_node& node) {
    if (current_pass_ != pass::link) {
        return;
    }
    if (symtab_.lookup_function(node.name) != nullptr) {
        diag_.error(node, std::format("duplicate function name '{}'", node.name));
        return;
    }
    symtab_.register_function(node.name, build_fun_type(node.params, node.return_type));
}

void resolver::visit(const method_decl_node& node) {
    if (current_pass_ != pass::link) {
        return;
    }
    auto* ti = symtab_.lookup_type(node.type_name);
    if (ti == nullptr) {
        diag_.error(node, std::format("method on undefined type '{}'", node.type_name));
        return;
    }
    if (ti->find_method(node.name) != nullptr) {
        diag_.error(node,
                    std::format("duplicate method '{}' on type '{}'", node.name, node.type_name));
        return;
    }
    ti->method_row->entries.push_back({node.name, build_fun_type(node.params, node.return_type)});
}
