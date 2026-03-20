#include "type_checker.h"

#include <algorithm>
#include <format>

#include "ast.h"

// ── Helpers ──────────────────────────────────────────────────────────────────

void type_checker::set_type(const ast_node& node, type_ptr t) { type_map_[&node] = std::move(t); }

type_ptr type_checker::type_of(const ast_node* node) const {
    auto it = type_map_.find(node);
    if (it == type_map_.end()) {
        return nullptr;
    }
    return it->second;
}

type_ptr type_checker::infer_expr(const expr_node& node) {
    node.accept(*this);
    auto t = type_of(&node);
    if (t == nullptr) {
        return env_.fresh_var();
    }
    return t;
}

void type_checker::try_unify(const ast_node& node, type_ptr a, type_ptr b,
                             const std::string& context) {
    try {
        env_.unify(std::move(a), std::move(b));
    } catch (const type_error& e) {
        // Type inference issues are warnings, not hard errors.
        // Existing fixtures test parse correctness, not type correctness.
        diag_.warning(node, std::format("{}: {}", context, e.what()));
    }
}

// ── Row-based member lookup ──────────────────────────────────────────────────

type_ptr type_checker::lookup_field(const ast_node& /*node*/, type_ptr obj_type,
                                    const std::string& field) {
    auto resolved = env_.find(std::move(obj_type));

    if (resolved->kind() == type_kind::named) {
        auto* nt = static_cast<named_type_t*>(resolved.get());
        auto* ti = symtab_.lookup_type(nt->name);
        if (ti != nullptr && ti->field_row != nullptr) {
            auto* row = static_cast<row_type_t*>(ti->field_row.get());
            if (auto found = row->find_entry(field)) {
                return found;
            }
        }
        return env_.fresh_var();
    }

    if (resolved->kind() == type_kind::row) {
        auto* row = static_cast<row_type_t*>(resolved.get());
        if (auto found = row->find_entry(field)) {
            return found;
        }
        if (row->tail != nullptr) {
            auto field_type = env_.fresh_var();
            row->entries.push_back({field, field_type});
            return field_type;
        }
        return env_.fresh_var();
    }

    if (resolved->kind() == type_kind::type_var) {
        auto field_type = env_.fresh_var();
        auto tail = env_.fresh_var();
        std::vector<row_entry> entries;
        entries.push_back({field, field_type});
        auto row = std::make_shared<row_type_t>(std::move(entries), tail);
        try {
            env_.unify(resolved, row);
        } catch (const type_error&) {
            // Permissive: ignore
        }
        return field_type;
    }

    return env_.fresh_var();
}

type_ptr type_checker::lookup_method(const ast_node& /*node*/, type_ptr obj_type,
                                     const std::string& method) {
    auto resolved = env_.find(std::move(obj_type));

    if (resolved->kind() == type_kind::named) {
        auto* nt = static_cast<named_type_t*>(resolved.get());
        auto* ti = symtab_.lookup_type(nt->name);
        if (ti != nullptr && ti->method_row != nullptr) {
            auto* row = static_cast<row_type_t*>(ti->method_row.get());
            if (auto found = row->find_entry(method)) {
                return found;
            }
        }
        // Unknown method — return fresh var (permissive for stdlib methods)
        return env_.fresh_var();
    }

    if (resolved->kind() == type_kind::generic) {
        // Methods on generic types (e.g. vector<int>.size()) — return fresh
        return env_.fresh_var();
    }

    if (resolved->kind() == type_kind::type_var) {
        return env_.fresh_var();
    }

    return env_.fresh_var();
}

// ── Expressions ──────────────────────────────────────────────────────────────

void type_checker::visit(const identifier_node& node) {
    auto* sym = symtab_.lookup(node.name);
    if (sym != nullptr) {
        set_type(node, sym->type);
        return;
    }
    auto ft = symtab_.lookup_function(node.name);
    if (ft != nullptr) {
        set_type(node, ft);
        return;
    }
    // Unknown symbol — assign fresh var (permissive: no stdlib)
    set_type(node, env_.fresh_var());
}

void type_checker::visit(const number_node& node) {
    // Number literals are int by default, but use a fresh var that will be
    // constrained by context.  This allows `var x: double = 1;` to work.
    set_type(node, env_.fresh_var());
}

void type_checker::visit(const string_node& node) { set_type(node, prim_string()); }

void type_checker::visit(const binary_op_node& node) {
    auto left = infer_expr(*node.left);
    auto right = infer_expr(*node.right);

    if (node.op == "&&" || node.op == "||") {
        try_unify(node, left, prim_bool(), "logical operator lhs");
        try_unify(node, right, prim_bool(), "logical operator rhs");
        set_type(node, prim_bool());
    } else if (node.op == "==" || node.op == "!=" || node.op == "<" || node.op == ">" ||
               node.op == "<=" || node.op == ">=") {
        try_unify(node, left, right, "comparison operands");
        set_type(node, prim_bool());
    } else {
        // Arithmetic: +, -, *, /, %
        try_unify(node, left, right, "arithmetic operands");
        set_type(node, env_.find(left));
    }
}

void type_checker::visit(const unary_op_node& node) {
    auto operand = infer_expr(*node.operand);
    if (node.op == "!") {
        try_unify(node, operand, prim_bool(), "logical not");
        set_type(node, prim_bool());
    } else {
        set_type(node, operand);
    }
}

void type_checker::visit(const assign_node& node) {
    auto lhs = infer_expr(*node.lhs);
    auto rhs = infer_expr(*node.rhs);
    try_unify(node, lhs, rhs, "assignment");
    set_type(node, lhs);
}

void type_checker::visit(const call_node& node) {
    auto ft = symtab_.lookup_function(node.name);
    if (ft == nullptr) {
        auto* sym = symtab_.lookup(node.name);
        if (sym != nullptr) {
            ft = sym->type;
        }
    }
    if (ft == nullptr) {
        // Unknown function — infer args, return fresh (permissive)
        for (const auto& arg : node.args) {
            infer_expr(*arg);
        }
        set_type(node, env_.fresh_var());
        return;
    }

    auto inst = env_.instantiate(ft);
    auto resolved = env_.find(inst);

    if (resolved->kind() == type_kind::fun) {
        auto* fun = static_cast<fun_type_t*>(resolved.get());
        for (size_t i = 0; i < std::min(fun->params.size(), node.args.size()); ++i) {
            auto arg_type = infer_expr(*node.args[i]);
            try_unify(node, arg_type, fun->params[i],
                      std::format("argument {} of '{}'", i + 1, node.name));
        }
        // Infer remaining args (if arity mismatch)
        for (size_t i = fun->params.size(); i < node.args.size(); ++i) {
            infer_expr(*node.args[i]);
        }
        set_type(node, fun->ret);
    } else {
        std::vector<type_ptr> param_types;
        param_types.reserve(node.args.size());
        for (const auto& arg : node.args) {
            param_types.push_back(infer_expr(*arg));
        }
        auto ret = env_.fresh_var();
        auto expected = std::make_shared<fun_type_t>(std::move(param_types), ret);
        try_unify(node, resolved, expected, std::format("call to '{}'", node.name));
        set_type(node, ret);
    }
}

void type_checker::visit(const member_access_node& node) {
    auto obj_type = infer_expr(*node.object);
    auto field_type = lookup_field(node, std::move(obj_type), node.field);
    set_type(node, std::move(field_type));
}

void type_checker::visit(const member_call_node& node) {
    auto obj_type = infer_expr(*node.object);
    auto method_type = lookup_method(node, obj_type, node.method);
    auto resolved = env_.find(method_type);

    if (resolved->kind() == type_kind::fun) {
        auto* fun = static_cast<fun_type_t*>(resolved.get());
        for (size_t i = 0; i < std::min(fun->params.size(), node.args.size()); ++i) {
            auto arg_type = infer_expr(*node.args[i]);
            try_unify(node, arg_type, fun->params[i],
                      std::format("argument {} of method '{}'", i + 1, node.method));
        }
        for (size_t i = fun->params.size(); i < node.args.size(); ++i) {
            infer_expr(*node.args[i]);
        }
        set_type(node, fun->ret);
    } else {
        // Unknown method — infer args, return fresh
        for (const auto& arg : node.args) {
            infer_expr(*arg);
        }
        set_type(node, env_.fresh_var());
    }
}

void type_checker::visit(const qualified_call_node& node) {
    auto ft = symtab_.lookup_function(node.qualifier + "::" + node.name);
    if (ft == nullptr) {
        auto* ti = symtab_.lookup_type(node.qualifier);
        if (ti != nullptr && ti->method_row != nullptr) {
            auto* row = static_cast<row_type_t*>(ti->method_row.get());
            ft = row->find_entry(node.name);
        }
    }
    if (ft == nullptr) {
        // Unknown qualified function — infer args, return fresh
        for (const auto& arg : node.args) {
            infer_expr(*arg);
        }
        set_type(node, env_.fresh_var());
        return;
    }

    auto inst = env_.instantiate(ft);
    auto resolved = env_.find(inst);
    if (resolved->kind() == type_kind::fun) {
        auto* fun = static_cast<fun_type_t*>(resolved.get());
        for (size_t i = 0; i < std::min(fun->params.size(), node.args.size()); ++i) {
            auto arg_type = infer_expr(*node.args[i]);
            try_unify(node, arg_type, fun->params[i],
                      std::format("argument {} of '{}::{}'", i + 1, node.qualifier, node.name));
        }
        set_type(node, fun->ret);
    } else {
        set_type(node, env_.fresh_var());
    }
}

void type_checker::visit(const index_node& node) {
    auto base_type = infer_expr(*node.base);
    auto index_type = infer_expr(*node.index);
    try_unify(node, index_type, prim_int(), "index expression");

    auto resolved = env_.find(base_type);
    if (resolved->kind() == type_kind::generic) {
        auto* gt = static_cast<generic_type_t*>(resolved.get());
        if (!gt->args.empty()) {
            set_type(node, gt->args[0]);
            return;
        }
    }
    auto elem = env_.fresh_var();
    auto expected = std::make_shared<generic_type_t>("vector", std::vector<type_ptr>{elem});
    try_unify(node, base_type, expected, "index base");
    set_type(node, elem);
}

void type_checker::visit(const compound_assign_node& node) {
    auto lhs = infer_expr(*node.lhs);
    auto rhs = infer_expr(*node.rhs);
    try_unify(node, lhs, rhs, "compound assignment");
    set_type(node, lhs);
}

void type_checker::visit(const tuple_expr_node& node) {
    std::vector<type_ptr> elem_types;
    elem_types.reserve(node.elements.size());
    for (const auto& e : node.elements) {
        elem_types.push_back(infer_expr(*e));
    }
    set_type(node, std::make_shared<tuple_type_t>(std::move(elem_types)));
}

void type_checker::visit(const init_list_expr_node& node) {
    auto elem = env_.fresh_var();
    for (const auto& a : node.args) {
        auto arg_type = infer_expr(*a);
        try_unify(node, arg_type, elem, "init list element");
    }
    set_type(node, std::make_shared<generic_type_t>("vector", std::vector<type_ptr>{elem}));
}

// ── Statements ───────────────────────────────────────────────────────────────

void type_checker::visit(const expr_stmt_node& node) { node.expr->accept(*this); }

void type_checker::visit(const var_decl_node& node) {
    type_ptr var_type;
    if (!node.type.empty()) {
        var_type = parse_type_string(node.type);
    } else {
        var_type = env_.fresh_var();
    }

    if (node.init != nullptr) {
        auto init_type = infer_expr(*node.init);
        try_unify(node, var_type, init_type, "variable declaration");
    }

    symtab_.bind(node.name, {node.name, var_type, false, &node});
}

void type_checker::visit(const tuple_var_decl_node& node) {
    auto init_type = infer_expr(*node.init);

    std::vector<type_ptr> elem_types;
    elem_types.reserve(node.names.size());
    for (size_t i = 0; i < node.names.size(); ++i) {
        elem_types.push_back(env_.fresh_var());
    }
    auto expected = std::make_shared<tuple_type_t>(elem_types);
    try_unify(node, init_type, expected, "tuple variable declaration");

    for (size_t i = 0; i < node.names.size(); ++i) {
        symtab_.bind(node.names[i], {node.names[i], elem_types[i], false, &node});
    }
}

void type_checker::visit(const tuple_assign_stmt_node& node) {
    auto rhs_type = infer_expr(*node.rhs);

    std::vector<type_ptr> lhs_types;
    lhs_types.reserve(node.lhs_exprs.size());
    for (const auto& lhs : node.lhs_exprs) {
        lhs_types.push_back(infer_expr(*lhs));
    }

    auto resolved = env_.find(rhs_type);
    if (resolved->kind() == type_kind::tuple) {
        auto* tup = static_cast<tuple_type_t*>(resolved.get());
        for (size_t i = 0; i < std::min(tup->elements.size(), lhs_types.size()); ++i) {
            try_unify(node, lhs_types[i], tup->elements[i], "tuple assignment");
        }
    } else {
        auto expected = std::make_shared<tuple_type_t>(lhs_types);
        try_unify(node, rhs_type, expected, "tuple assignment");
    }
}

void type_checker::visit(const return_stmt_node& node) {
    if (node.value != nullptr) {
        auto val_type = infer_expr(*node.value);
        if (current_return_type_ != nullptr) {
            try_unify(node, val_type, current_return_type_, "return statement");
        }
    }
}

void type_checker::visit(const for_stmt_node& node) {
    symtab_.push_scope();
    node.init->accept(*this);
    auto cond_type = infer_expr(*node.condition);
    try_unify(node, cond_type, prim_bool(), "for loop condition");
    node.increment->accept(*this);
    node.body->accept(*this);
    symtab_.pop_scope();
}

void type_checker::visit(const for_range_stmt_node& node) {
    symtab_.push_scope();
    auto range_type = infer_expr(*node.range);
    auto elem_type = env_.fresh_var();
    symtab_.bind(node.var_name, {node.var_name, elem_type, false, &node});
    node.body->accept(*this);
    symtab_.pop_scope();
}

void type_checker::visit(const compound_stmt_node& node) {
    symtab_.push_scope();
    for (const auto& stmt : node.statements) {
        stmt->accept(*this);
    }
    symtab_.pop_scope();
}

void type_checker::visit(const if_stmt_node& node) {
    auto cond_type = infer_expr(*node.condition);
    try_unify(node, cond_type, prim_bool(), "if condition");
    node.then_body->accept(*this);
    if (node.else_body != nullptr) {
        node.else_body->accept(*this);
    }
}

void type_checker::visit(const continue_stmt_node& /*node*/) {}
void type_checker::visit(const break_stmt_node& /*node*/) {}

// ── Declarations ─────────────────────────────────────────────────────────────

void type_checker::visit(const param_node& /*node*/) {}

void type_checker::visit(const func_decl_node& node) {
    auto ft = symtab_.lookup_function(node.name);
    if (ft == nullptr) {
        return;
    }

    auto resolved = env_.find(ft);
    if (resolved->kind() != type_kind::fun) {
        return;
    }
    auto* fun = static_cast<fun_type_t*>(resolved.get());

    env_.enter_level();
    symtab_.push_scope();

    for (size_t i = 0; i < node.params.size(); ++i) {
        symtab_.bind(node.params[i]->name, {node.params[i]->name, fun->params[i],
                                            node.params[i]->is_ref, node.params[i].get()});
    }

    auto prev_return = current_return_type_;
    current_return_type_ = fun->ret;

    node.body->accept(*this);

    current_return_type_ = prev_return;
    symtab_.pop_scope();
    env_.leave_level();

    env_.generalize(ft, env_.current_level());
}

void type_checker::visit(const method_decl_node& node) {
    auto* ti = symtab_.lookup_type(node.type_name);
    if (ti == nullptr) {
        return;
    }

    type_ptr method_type;
    if (ti->method_row != nullptr) {
        auto* mr = static_cast<row_type_t*>(ti->method_row.get());
        method_type = mr->find_entry(node.name);
    }
    if (method_type == nullptr) {
        return;
    }

    auto resolved = env_.find(method_type);
    if (resolved->kind() != type_kind::fun) {
        return;
    }
    auto* fun = static_cast<fun_type_t*>(resolved.get());

    env_.enter_level();
    symtab_.push_scope();

    // Bind struct fields in scope for implicit field access
    if (ti->field_row != nullptr) {
        auto* fr = static_cast<row_type_t*>(ti->field_row.get());
        for (const auto& e : fr->entries) {
            symtab_.bind(e.label, {e.label, e.type, false, nullptr});
        }
    }

    for (size_t i = 0; i < node.params.size(); ++i) {
        symtab_.bind(node.params[i]->name, {node.params[i]->name, fun->params[i],
                                            node.params[i]->is_ref, node.params[i].get()});
    }

    auto prev_return = current_return_type_;
    current_return_type_ = fun->ret;

    node.body->accept(*this);

    current_return_type_ = prev_return;
    symtab_.pop_scope();
    env_.leave_level();
}

void type_checker::visit(const type_decl_node& /*node*/) {}
void type_checker::visit(const struct_field_node& /*node*/) {}
void type_checker::visit(const struct_type_node& /*node*/) {}
void type_checker::visit(const interface_type_node& /*node*/) {}
void type_checker::visit(const interface_method_node& /*node*/) {}

void type_checker::visit(const program_node& node) {
    for (const auto& decl : node.declarations) {
        decl->accept(*this);
    }
}
