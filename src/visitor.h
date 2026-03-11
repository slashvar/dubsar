#ifndef VISITOR_H
#define VISITOR_H

// Forward declarations of every concrete AST node type.
// Including this header does not pull in the full AST definition.
class IdentifierNode;
class NumberNode;
class StringNode;
class BinaryOpNode;
class UnaryOpNode;
class AssignNode;
class ExprStmtNode;
class ParamNode;
class VarDeclNode;
class ReturnStmtNode;
class ForStmtNode;
class CompoundStmtNode;
class FuncDeclNode;
class StructFieldNode;
class StructTypeNode;
class MethodDeclNode;
class TypeDeclNode;
class InterfaceTypeNode;
class InterfaceMethodNode;
class MemberCallNode;
class QualifiedCallNode;
class CallNode;
class IndexNode;
class CompoundAssignNode;
class IfStmtNode;
class TupleExprNode;
class TupleVarDeclNode;
class TupleAssignStmtNode;
class ForRangeStmtNode;
class ContinueStmtNode;
class InitListExprNode;
class ProgramNode;

// Abstract visitor interface.  One visit() overload per concrete node type
// provides static dispatch on the node's type without touching the nodes
// themselves.  New visitors (pretty-printer, type-checker, code-gen, …) are
// added by implementing this interface.
class Visitor {
public:
    virtual void visit(const IdentifierNode&) = 0;
    virtual void visit(const NumberNode&) = 0;
    virtual void visit(const StringNode&) = 0;
    virtual void visit(const BinaryOpNode&) = 0;
    virtual void visit(const UnaryOpNode&) = 0;
    virtual void visit(const AssignNode&) = 0;
    virtual void visit(const ExprStmtNode&) = 0;
    virtual void visit(const ParamNode&) = 0;
    virtual void visit(const VarDeclNode&) = 0;
    virtual void visit(const ReturnStmtNode&) = 0;
    virtual void visit(const ForStmtNode&) = 0;
    virtual void visit(const CompoundStmtNode&) = 0;
    virtual void visit(const FuncDeclNode&) = 0;
    virtual void visit(const StructFieldNode&) = 0;
    virtual void visit(const StructTypeNode&) = 0;
    virtual void visit(const MethodDeclNode&) = 0;
    virtual void visit(const TypeDeclNode&) = 0;
    virtual void visit(const InterfaceTypeNode&) = 0;
    virtual void visit(const InterfaceMethodNode&) = 0;
    virtual void visit(const MemberCallNode&) = 0;
    virtual void visit(const QualifiedCallNode&) = 0;
    virtual void visit(const CallNode&) = 0;
    virtual void visit(const IndexNode&) = 0;
    virtual void visit(const CompoundAssignNode&) = 0;
    virtual void visit(const IfStmtNode&) = 0;
    virtual void visit(const TupleExprNode&) = 0;
    virtual void visit(const TupleVarDeclNode&) = 0;
    virtual void visit(const TupleAssignStmtNode&) = 0;
    virtual void visit(const ForRangeStmtNode&) = 0;
    virtual void visit(const ContinueStmtNode&) = 0;
    virtual void visit(const InitListExprNode&) = 0;
    virtual void visit(const ProgramNode&) = 0;
    virtual ~Visitor() = default;
};

#endif  // VISITOR_H
