#include "type_checker.h"

#include <format>

#include "ast.h"

// ── Helpers ──────────────────────────────────────────────────────────────────

void type_checker::set_type(const ast_node& node, type_ptr t) { type_map_[&node] = std::move(t); }

type_ptr type_checker::type_of(const ast_node* node) const {
    auto it = type_map_.find(node);
    return it == type_map_.end() ? nullptr : it->second;
}

type_ptr type_checker::infer_expr(const expr_node& node) {
    node.accept(*this);
    auto t = type_of(&node);
    return t == nullptr ? env_.fresh_var() : t;
}

void type_checker::try_unify(const ast_node& node, type_ptr a, type_ptr b,
                             const std::string& context) {
    try {
        env_.unify(std::move(a), std::move(b));
    } catch (const type_error& e) {
        diag_.warning(node, std::format("{}: {}", context, e.what()));
    }
}

type_ptr type_checker::infer_call(const ast_node& node, const type_ptr& callee,
                                  const std::vector<std::unique_ptr<expr_node>>& args,
                                  const std::string& callee_name) {
    auto resolved = env_.find(callee);

    if (resolved->kind() == type_kind::fun) {
        auto* fun = static_cast<fun_type_t*>(resolved.get());
        for (size_t i = 0; i < args.size(); ++i) {
            auto arg_type = infer_expr(*args[i]);
            if (i < fun->params.size()) {
                try_unify(node, arg_type, fun->params[i],
                          std::format("argument {} of {}", i + 1, callee_name));
            }
        }
        return fun->ret;
    }

    // The callee is not known to be a function yet, so constrain it to be one.
    std::vector<type_ptr> param_types;
    param_types.reserve(args.size());
    for (const auto& arg : args) {
        param_types.push_back(infer_expr(*arg));
    }
    auto ret = env_.fresh_var();
    try_unify(node, resolved, std::make_shared<fun_type_t>(std::move(param_types), ret),
              std::format("call to {}", callee_name));
    return ret;
}

// ── Row-based member lookup ──────────────────────────────────────────────────

type_ptr type_checker::lookup_field(type_ptr obj_type, const std::string& field) {
    auto resolved = env_.find(std::move(obj_type));

    switch (resolved->kind()) {
        case type_kind::named: {
            auto* ti = symtab_.lookup_type(static_cast<named_type_t*>(resolved.get())->name);
            if (ti != nullptr) {
                if (auto found = ti->find_field(field)) {
                    return found;
                }
            }
            break;
        }
        case type_kind::row: {
            auto* row = static_cast<row_type_t*>(resolved.get());
            if (auto found = row->find_entry(field)) {
                return found;
            }
            // An open row grows a new field rather than rejecting it.
            if (row->tail != nullptr) {
                auto field_type = env_.fresh_var();
                row->entries.push_back({field, field_type});
                return field_type;
            }
            break;
        }
        case type_kind::type_var: {
            // Constrain the unknown object to be a record holding this field.
            auto field_type = env_.fresh_var();
            auto row = std::make_shared<row_type_t>(std::vector<row_entry>{{field, field_type}},
                                                    env_.fresh_var());
            try {
                env_.unify(resolved, row);
            } catch (const type_error&) {
                // Permissive: leave the object type as it was.
            }
            return field_type;
        }
        default:
            break;
    }
    return env_.fresh_var();
}

type_ptr type_checker::lookup_method(type_ptr obj_type, const std::string& method) {
    auto resolved = env_.find(std::move(obj_type));

    if (resolved->kind() == type_kind::named) {
        auto* ti = symtab_.lookup_type(static_cast<named_type_t*>(resolved.get())->name);
        if (ti != nullptr) {
            if (auto found = ti->find_method(method)) {
                return found;
            }
        }
    }
    // Permissive: stdlib and generic-type methods are not declared anywhere.
    return env_.fresh_var();
}

// ── Expressions ──────────────────────────────────────────────────────────────

void type_checker::visit(const identifier_node& node) {
    if (auto* sym = symtab_.lookup(node.name)) {
        set_type(node, sym->type);
        return;
    }
    if (auto ft = symtab_.lookup_function(node.name)) {
        set_type(node, std::move(ft));
        return;
    }
    set_type(node, env_.fresh_var());  // Permissive: no stdlib yet.
}

// A literal starts as a variable so that context can pick the numeric type,
// which is what makes `var x: double = 1;` check.
void type_checker::visit(const number_node& node) { set_type(node, env_.fresh_var()); }

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
    auto callee = symtab_.lookup_function(node.name);
    if (callee == nullptr) {
        if (auto* sym = symtab_.lookup(node.name)) {
            callee = sym->type;
        }
    }
    if (callee == nullptr) {
        callee = env_.fresh_var();  // Permissive: undeclared function.
    }
    set_type(node, infer_call(node, env_.instantiate(std::move(callee)), node.args,
                              std::format("'{}'", node.name)));
}

void type_checker::visit(const member_access_node& node) {
    auto obj_type = infer_expr(*node.object);
    set_type(node, lookup_field(std::move(obj_type), node.field));
}

void type_checker::visit(const member_call_node& node) {
    auto obj_type = infer_expr(*node.object);
    auto method_type = lookup_method(std::move(obj_type), node.method);
    set_type(node,
             infer_call(node, method_type, node.args, std::format("method '{}'", node.method)));
}

void type_checker::visit(const qualified_call_node& node) {
    auto callee = symtab_.lookup_function(node.qualifier + "::" + node.name);
    if (callee == nullptr) {
        if (auto* ti = symtab_.lookup_type(node.qualifier)) {
            callee = ti->find_method(node.name);
        }
    }
    if (callee == nullptr) {
        callee = env_.fresh_var();  // Permissive: no module system yet.
    }
    set_type(node, infer_call(node, env_.instantiate(std::move(callee)), node.args,
                              std::format("'{}::{}'", node.qualifier, node.name)));
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
    try_unify(node, base_type,
              std::make_shared<generic_type_t>("vector", std::vector<type_ptr>{elem}),
              "index base");
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
        try_unify(node, infer_expr(*a), elem, "init list element");
    }
    set_type(node, std::make_shared<generic_type_t>("vector", std::vector<type_ptr>{elem}));
}

// ── Statements ───────────────────────────────────────────────────────────────

void type_checker::visit(const expr_stmt_node& node) { node.expr->accept(*this); }

void type_checker::visit(const var_decl_node& node) {
    auto var_type = type_or_fresh(env_, node.type);
    if (node.init != nullptr) {
        try_unify(node, var_type, infer_expr(*node.init), "variable declaration");
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
    try_unify(node, init_type, std::make_shared<tuple_type_t>(elem_types),
              "tuple variable declaration");

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
    try_unify(node, rhs_type, std::make_shared<tuple_type_t>(std::move(lhs_types)),
              "tuple assignment");
}

void type_checker::visit(const return_stmt_node& node) {
    if (node.value != nullptr && current_return_type_ != nullptr) {
        try_unify(node, infer_expr(*node.value), current_return_type_, "return statement");
    }
}

void type_checker::visit(const for_stmt_node& node) {
    symtab_.push_scope();
    node.init->accept(*this);
    try_unify(node, infer_expr(*node.condition), prim_bool(), "for loop condition");
    node.increment->accept(*this);
    node.body->accept(*this);
    symtab_.pop_scope();
}

void type_checker::visit(const for_range_stmt_node& node) {
    symtab_.push_scope();
    infer_expr(*node.range);
    // The element type stays free: `range()` has no declared signature yet.
    symtab_.bind(node.var_name, {node.var_name, env_.fresh_var(), false, &node});
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
    try_unify(node, infer_expr(*node.condition), prim_bool(), "if condition");
    node.then_body->accept(*this);
    if (node.else_body != nullptr) {
        node.else_body->accept(*this);
    }
}

void type_checker::visit(const continue_stmt_node& /*node*/) {}
void type_checker::visit(const break_stmt_node& /*node*/) {}

// ── Declarations ─────────────────────────────────────────────────────────────

void type_checker::visit(const param_node& /*node*/) {}

void type_checker::bind_fields(const type_info& ti) {
    if (ti.parent != nullptr) {
        bind_fields(*ti.parent);  // Bind ancestors first so own fields shadow them.
    }
    for (const auto& e : ti.field_row->entries) {
        symtab_.bind(e.label, {e.label, e.type, false, nullptr});
    }
}

void type_checker::check_body(const fun_type_t& fun,
                              const std::vector<std::unique_ptr<param_node>>& params,
                              const stmt_node& body, const type_info* fields) {
    env_.enter_level();
    symtab_.push_scope();

    if (fields != nullptr) {
        bind_fields(*fields);
    }
    for (size_t i = 0; i < params.size(); ++i) {
        symtab_.bind(params[i]->name,
                     {params[i]->name, fun.params[i], params[i]->is_ref, params[i].get()});
    }

    auto prev_return = current_return_type_;
    current_return_type_ = fun.ret;
    body.accept(*this);
    current_return_type_ = std::move(prev_return);

    symtab_.pop_scope();
    env_.leave_level();
}

void type_checker::visit(const func_decl_node& node) {
    auto ft = symtab_.lookup_function(node.name);
    if (ft == nullptr) {
        return;
    }
    auto resolved = env_.find(ft);
    if (resolved->kind() != type_kind::fun) {
        return;
    }

    check_body(*static_cast<fun_type_t*>(resolved.get()), node.params, *node.body, nullptr);
    // Top-level functions are generalized, so each call site gets fresh vars.
    env_.generalize(ft, env_.current_level());
}

void type_checker::visit(const method_decl_node& node) {
    auto* ti = symtab_.lookup_type(node.type_name);
    if (ti == nullptr) {
        return;
    }
    auto method_type = ti->find_method(node.name);
    if (method_type == nullptr) {
        return;
    }
    auto resolved = env_.find(method_type);
    if (resolved->kind() != type_kind::fun) {
        return;
    }

    // A method body sees its type's fields without an explicit receiver.
    check_body(*static_cast<fun_type_t*>(resolved.get()), node.params, *node.body, ti);
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
