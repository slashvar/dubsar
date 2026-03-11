#ifndef PRINTER_H
#define PRINTER_H

#include <ostream>

#include "visitor.h"

// Visitor that serialises an AST back to valid dubsar source code.
// The output is stable under round-trip: parsing the output and running the
// Printer again produces identical text.
class Printer : public Visitor {
public:
    explicit Printer(std::ostream& out) : out_(out) {}

    void visit(const IdentifierNode&) override;
    void visit(const NumberNode&) override;
    void visit(const StringNode&) override;
    void visit(const BinaryOpNode&) override;
    void visit(const UnaryOpNode&) override;
    void visit(const AssignNode&) override;
    void visit(const ExprStmtNode&) override;
    void visit(const ParamNode&) override;
    void visit(const VarDeclNode&) override;
    void visit(const ReturnStmtNode&) override;
    void visit(const ForStmtNode&) override;
    void visit(const CompoundStmtNode&) override;
    void visit(const FuncDeclNode&) override;
    void visit(const StructFieldNode&) override;
    void visit(const StructTypeNode&) override;
    void visit(const MethodDeclNode&) override;
    void visit(const TypeDeclNode&) override;
    void visit(const InterfaceTypeNode&) override;
    void visit(const InterfaceMethodNode&) override;
    void visit(const MemberCallNode&) override;
    void visit(const QualifiedCallNode&) override;
    void visit(const CallNode&) override;
    void visit(const IndexNode&) override;
    void visit(const CompoundAssignNode&) override;
    void visit(const IfStmtNode&) override;
    void visit(const TupleExprNode&) override;
    void visit(const TupleVarDeclNode&) override;
    void visit(const TupleAssignStmtNode&) override;
    void visit(const ForRangeStmtNode&) override;
    void visit(const ContinueStmtNode&) override;
    void visit(const InitListExprNode&) override;
    void visit(const ProgramNode&) override;

private:
    std::ostream& out_;
    int indent_ = 0;

    // Emit `indent_` spaces.
    void pad() const;
    // Emit `indent_ + extra` spaces without changing indent_.
    void pad(int extra) const;
};

#endif  // PRINTER_H
