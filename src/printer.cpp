#include "printer.h"

#include <format>
#include <string>

#include "ast.h"

// ── Helpers ───────────────────────────────────────────────────────────────────

void printer::pad() const { out_ << std::string(indent_, ' '); }

// ── Expressions ───────────────────────────────────────────────────────────────

void printer::visit(const identifier_node& node) { out_ << node.name; }

void printer::visit(const number_node& node) { out_ << node.value; }

void printer::visit(const string_node& node) { out_ << std::format("\"{}\"", node.value); }

// Always parenthesised, so re-parsing cannot change the tree shape.
void printer::visit(const binary_op_node& node) {
    out_ << '(';
    node.left->accept(*this);
    out_ << std::format(" {} ", node.op);
    node.right->accept(*this);
    out_ << ')';
}

void printer::visit(const unary_op_node& node) {
    if (node.op == "post++" || node.op == "post--") {
        node.operand->accept(*this);
        out_ << (node.op == "post++" ? "++" : "--");
    } else {
        out_ << node.op;
        node.operand->accept(*this);
    }
}

void printer::visit(const assign_node& node) {
    node.lhs->accept(*this);
    out_ << " = ";
    node.rhs->accept(*this);
}

// ── Statements ────────────────────────────────────────────────────────────────

void printer::visit(const expr_stmt_node& node) { node.expr->accept(*this); }

// The trailing semicolon comes from the enclosing statement list.
void printer::visit(const var_decl_node& node) {
    out_ << "var " << node.name;
    if (!node.type.empty()) out_ << " : " << node.type;
    if (node.init) {
        out_ << " = ";
        node.init->accept(*this);
    }
}

void printer::visit(const return_stmt_node& node) {
    out_ << "return";
    if (node.value) {
        out_ << ' ';
        node.value->accept(*this);
    }
}

// The header separators are printed here because var_decl omits its semicolon.
void printer::visit(const for_stmt_node& node) {
    out_ << "for ";
    node.init->accept(*this);
    out_ << "; ";
    node.condition->accept(*this);
    out_ << "; ";
    node.increment->accept(*this);
    out_ << ' ';
    node.body->accept(*this);
}

void printer::visit(const compound_stmt_node& node) {
    out_ << "{\n";
    indent_ += 4;
    for (const auto& stmt : node.statements) {
        pad();
        stmt->accept(*this);
        if (stmt->needs_semicolon()) out_ << ';';
        out_ << '\n';
    }
    indent_ -= 4;
    pad();
    out_ << '}';
}

// ── Parameters ────────────────────────────────────────────────────────────────

void printer::visit(const param_node& node) {
    out_ << node.name;
    if (!node.is_ref && node.type.empty()) {
        return;
    }
    out_ << " : ";
    if (node.is_ref) {
        out_ << (node.type.empty() ? "ref" : "ref ");
    }
    out_ << node.type;
}

// ── Declarations ──────────────────────────────────────────────────────────────

void printer::visit(const func_decl_node& node) {
    out_ << std::format("fun {}(", node.name);
    print_list(node.params);
    out_ << ')';
    if (!node.return_type.empty()) out_ << std::format(" -> {}", node.return_type);
    out_ << ' ';
    node.body->accept(*this);
}

void printer::visit(const struct_field_node& node) {
    out_ << std::format("{}: {};", node.name, node.type);
}

void printer::visit(const struct_type_node& node) {
    out_ << (node.parent.empty() ? "struct" : std::format("struct : {}", node.parent));
    out_ << " {\n";
    indent_ += 4;
    for (const auto& f : node.fields) {
        pad();
        f->accept(*this);
        out_ << '\n';
    }
    indent_ -= 4;
    pad();
    out_ << '}';
}

void printer::visit(const method_decl_node& node) {
    out_ << std::format("fun {}::{}(", node.type_name, node.name);
    print_list(node.params);
    out_ << ')';
    if (!node.return_type.empty()) out_ << std::format(" -> {}", node.return_type);
    out_ << ' ';
    node.body->accept(*this);
}

void printer::visit(const type_decl_node& node) {
    out_ << std::format("type {} = ", node.name);
    node.body->accept(*this);
}

void printer::visit(const interface_type_node& node) {
    out_ << "interface {\n";
    indent_ += 4;
    for (const auto& m : node.methods) {
        pad();
        m->accept(*this);
        out_ << '\n';
    }
    indent_ -= 4;
    pad();
    out_ << '}';
}

void printer::visit(const interface_method_node& node) {
    out_ << node.name << '(';
    print_list(node.params);
    out_ << ')';
    if (!node.return_type.empty()) out_ << std::format(" -> {}", node.return_type);
    out_ << ';';
}

void printer::visit(const member_call_node& node) {
    node.object->accept(*this);
    out_ << '.' << node.method << '(';
    print_list(node.args);
    out_ << ')';
}

void printer::visit(const member_access_node& node) {
    node.object->accept(*this);
    out_ << '.' << node.field;
}

void printer::visit(const qualified_call_node& node) {
    out_ << std::format("{}::{}(", node.qualifier, node.name);
    print_list(node.args);
    out_ << ')';
}

void printer::visit(const call_node& node) {
    out_ << node.name << '(';
    print_list(node.args);
    out_ << ')';
}

void printer::visit(const index_node& node) {
    node.base->accept(*this);
    out_ << '[';
    node.index->accept(*this);
    out_ << ']';
}

void printer::visit(const compound_assign_node& node) {
    node.lhs->accept(*this);
    out_ << std::format(" {} ", node.op);
    node.rhs->accept(*this);
}

void printer::visit(const if_stmt_node& node) {
    out_ << "if ";
    node.condition->accept(*this);
    out_ << ' ';
    node.then_body->accept(*this);
    if (node.else_body) {
        out_ << " else ";
        node.else_body->accept(*this);
    }
}

void printer::visit(const continue_stmt_node& /*node*/) { out_ << "continue"; }

void printer::visit(const break_stmt_node& /*node*/) { out_ << "break"; }

void printer::visit(const init_list_expr_node& node) {
    out_ << '{';
    print_list(node.args);
    out_ << '}';
}

void printer::visit(const tuple_expr_node& node) { print_list(node.elements); }

void printer::visit(const tuple_var_decl_node& node) {
    out_ << "var ";
    for (bool first = true; const auto& name : node.names) {
        if (!first) out_ << ", ";
        out_ << name;
        first = false;
    }
    out_ << " = ";
    node.init->accept(*this);
}

void printer::visit(const tuple_assign_stmt_node& node) {
    print_list(node.lhs_exprs);
    out_ << " = ";
    node.rhs->accept(*this);
}

void printer::visit(const for_range_stmt_node& node) {
    out_ << std::format("for var {} = ", node.var_name);
    node.range->accept(*this);
    out_ << ' ';
    node.body->accept(*this);
}

// Top-level declarations are separated by a blank line.
void printer::visit(const program_node& node) {
    for (bool first = true; const auto& decl : node.declarations) {
        if (!first) out_ << '\n';
        decl->accept(*this);
        out_ << '\n';
        first = false;
    }
}
