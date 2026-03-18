#include "printer.h"

#include <format>
#include <string>

#include "ast.h"

// ── Helpers ───────────────────────────────────────────────────────────────────

void Printer::pad() const { out_ << std::string(indent_, ' '); }
void Printer::pad(int extra) const { out_ << std::string(indent_ + extra, ' '); }

void Printer::printParams(const std::vector<std::unique_ptr<ParamNode>>& params) {
    for (bool first = true; const auto& p : params) {
        if (!first) out_ << ", ";
        p->accept(*this);
        first = false;
    }
}

// ── Expressions ───────────────────────────────────────────────────────────────

void Printer::visit(const IdentifierNode& node) { out_ << node.name; }

void Printer::visit(const NumberNode& node) { out_ << node.value; }

void Printer::visit(const StringNode& node) { out_ << std::format("\"{}\"", node.value); }

// Parentheses guarantee round-trip correctness regardless of precedence context.
void Printer::visit(const BinaryOpNode& node) {
    out_ << '(';
    node.left->accept(*this);
    out_ << std::format(" {} ", node.op);
    node.right->accept(*this);
    out_ << ')';
}

void Printer::visit(const UnaryOpNode& node) {
    if (node.op == "post++" || node.op == "post--") {
        node.operand->accept(*this);
        out_ << (node.op == "post++" ? "++" : "--");
    } else {
        out_ << node.op;
        node.operand->accept(*this);
    }
}

void Printer::visit(const AssignNode& node) {
    node.lhs->accept(*this);
    out_ << " = ";
    node.rhs->accept(*this);
}

// ── Statements ────────────────────────────────────────────────────────────────

void Printer::visit(const ExprStmtNode& node) { node.expr->accept(*this); }

// Prints without trailing semicolon; CompoundStmtNode adds it via
// needsSemicolon().
void Printer::visit(const VarDeclNode& node) {
    if (node.type.empty()) {
        out_ << std::format("var {} = ", node.name);
    } else {
        out_ << std::format("var {} : {} = ", node.name, node.type);
    }
    node.init->accept(*this);
}

// Prints without trailing semicolon; CompoundStmtNode adds it via
// needsSemicolon().
void Printer::visit(const ReturnStmtNode& node) {
    out_ << "return ";
    node.value->accept(*this);
}

// Grammar: for var_decl expr ; expr body
// var_decl omits its own semicolon, so ForStmtNode supplies it here.
void Printer::visit(const ForStmtNode& node) {
    out_ << "for ";
    node.init->accept(*this);
    out_ << "; ";
    node.condition->accept(*this);
    out_ << "; ";
    node.increment->accept(*this);
    out_ << ' ';
    node.body->accept(*this);
}

void Printer::visit(const CompoundStmtNode& node) {
    out_ << "{\n";
    indent_ += 4;
    for (const auto& stmt : node.statements) {
        pad();
        stmt->accept(*this);
        if (stmt->needsSemicolon()) out_ << ';';
        out_ << '\n';
    }
    indent_ -= 4;
    pad();
    out_ << '}';
}

// ── Parameters ────────────────────────────────────────────────────────────────

void Printer::visit(const ParamNode& node) {
    if (node.isRef && node.type.empty()) {
        out_ << std::format("{} : ref", node.name);
    } else if (!node.type.empty() && !node.isRef) {
        out_ << std::format("{} : {}", node.name, node.type);
    } else if (!node.type.empty() && node.isRef) {
        out_ << std::format("{} : ref {}", node.name, node.type);
    } else {
        out_ << node.name;
    }
}

// ── Declarations ──────────────────────────────────────────────────────────────

void Printer::visit(const FuncDeclNode& node) {
    out_ << std::format("fun {}(", node.name);
    printParams(node.params);
    out_ << ')';
    if (!node.returnType.empty()) out_ << std::format(" -> {}", node.returnType);
    out_ << ' ';
    node.body->accept(*this);
}

void Printer::visit(const StructFieldNode& node) {
    out_ << std::format("{}: {};", node.name, node.type);
}

void Printer::visit(const StructTypeNode& node) {
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

void Printer::visit(const MethodDeclNode& node) {
    out_ << std::format("fun {}::{}(", node.typeName, node.name);
    printParams(node.params);
    out_ << ')';
    if (!node.returnType.empty()) out_ << std::format(" -> {}", node.returnType);
    out_ << ' ';
    node.body->accept(*this);
}

void Printer::visit(const TypeDeclNode& node) {
    out_ << std::format("type {} = ", node.name);
    node.body->accept(*this);
}

void Printer::visit(const InterfaceTypeNode& node) {
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

void Printer::visit(const InterfaceMethodNode& node) {
    out_ << node.name << '(';
    printParams(node.params);
    out_ << ')';
    if (!node.returnType.empty()) out_ << std::format(" -> {}", node.returnType);
    out_ << ';';
}

void Printer::visit(const MemberCallNode& node) {
    node.object->accept(*this);
    out_ << '.' << node.method << '(';
    for (bool first = true; const auto& a : node.args) {
        if (!first) out_ << ", ";
        a->accept(*this);
        first = false;
    }
    out_ << ')';
}

void Printer::visit(const QualifiedCallNode& node) {
    out_ << std::format("{}::{}(", node.qualifier, node.name);
    for (bool first = true; const auto& a : node.args) {
        if (!first) out_ << ", ";
        a->accept(*this);
        first = false;
    }
    out_ << ')';
}

void Printer::visit(const CallNode& node) {
    out_ << node.name << '(';
    for (bool first = true; const auto& a : node.args) {
        if (!first) out_ << ", ";
        a->accept(*this);
        first = false;
    }
    out_ << ')';
}

void Printer::visit(const IndexNode& node) {
    node.base->accept(*this);
    out_ << '[';
    node.index->accept(*this);
    out_ << ']';
}

void Printer::visit(const CompoundAssignNode& node) {
    node.lhs->accept(*this);
    out_ << std::format(" {} ", node.op);
    node.rhs->accept(*this);
}

void Printer::visit(const IfStmtNode& node) {
    out_ << "if ";
    node.condition->accept(*this);
    out_ << ' ';
    node.then_body->accept(*this);
    if (node.else_body) {
        out_ << " else ";
        node.else_body->accept(*this);
    }
}

void Printer::visit(const ContinueStmtNode&) { out_ << "continue"; }

void Printer::visit(const InitListExprNode& node) {
    out_ << '{';
    for (bool first = true; const auto& a : node.args) {
        if (!first) out_ << ", ";
        a->accept(*this);
        first = false;
    }
    out_ << '}';
}

void Printer::visit(const TupleExprNode& node) {
    for (bool first = true; const auto& e : node.elements) {
        if (!first) out_ << ", ";
        e->accept(*this);
        first = false;
    }
}

void Printer::visit(const TupleVarDeclNode& node) {
    out_ << "var ";
    for (bool first = true; const auto& name : node.names) {
        if (!first) out_ << ", ";
        out_ << name;
        first = false;
    }
    out_ << " = ";
    node.init->accept(*this);
}

void Printer::visit(const TupleAssignStmtNode& node) {
    for (bool first = true; const auto& name : node.lhs_names) {
        if (!first) out_ << ", ";
        out_ << name;
        first = false;
    }
    out_ << " = ";
    node.rhs->accept(*this);
}

void Printer::visit(const ForRangeStmtNode& node) {
    out_ << std::format("for var {} = ", node.varName);
    node.range->accept(*this);
    out_ << ' ';
    node.body->accept(*this);
}

void Printer::visit(const ProgramNode& node) {
    // C++20 init-statement in range-for for blank-line separation.
    for (bool first = true; const auto& decl : node.declarations) {
        if (!first) out_ << '\n';
        decl->accept(*this);
        out_ << '\n';
        first = false;
    }
}
