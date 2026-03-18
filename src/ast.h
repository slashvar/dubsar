#ifndef AST_H
#define AST_H

#include <memory>
#include <string>
#include <vector>

#include "visitor.h"

// Forward declarations
class ASTNode;
class ExprNode;
class StmtNode;
class DeclNode;
class TypeBodyNode;
class ProgramNode;
class FuncDeclNode;
class VarDeclNode;
class ForStmtNode;
class CompoundStmtNode;
class ExprStmtNode;
class ParamNode;
class TypeDeclNode;
class StructTypeNode;
class StructFieldNode;
class MethodDeclNode;
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

// Global root for the AST - defined in ast.cpp
extern std::unique_ptr<ProgramNode> root;

class ASTNode {
public:
    virtual void accept(Visitor& v) const = 0;
    [[nodiscard]] virtual bool needsSemicolon() const noexcept { return false; }
    virtual ~ASTNode() = default;
};

class ExprNode : public ASTNode {};

class StmtNode : public ASTNode {};

// DeclNode inherits StmtNode: declarations are valid statement-context nodes,
// which avoids undefined-behaviour casts in compound-statement handling.
class DeclNode : public StmtNode {};

// Abstract base for type bodies (struct and interface).
class TypeBodyNode : public ASTNode {};

class IdentifierNode : public ExprNode {
public:
    std::string name;
    explicit IdentifierNode(std::string n) noexcept : name(std::move(n)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class NumberNode : public ExprNode {
public:
    int value;
    explicit NumberNode(int v) noexcept : value(v) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class StringNode : public ExprNode {
public:
    std::string value;
    explicit StringNode(std::string v) noexcept : value(std::move(v)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class BinaryOpNode : public ExprNode {
public:
    std::string op;
    std::unique_ptr<ExprNode> left;
    std::unique_ptr<ExprNode> right;
    BinaryOpNode(std::string o, ExprNode* l, ExprNode* r) : op(std::move(o)), left(l), right(r) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class UnaryOpNode : public ExprNode {
public:
    std::string op;
    std::unique_ptr<ExprNode> operand;
    UnaryOpNode(std::string o, ExprNode* a) : op(std::move(o)), operand(a) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class AssignNode : public ExprNode {
public:
    std::unique_ptr<ExprNode> lhs;
    std::unique_ptr<ExprNode> rhs;
    AssignNode(ExprNode* l, ExprNode* r) : lhs(l), rhs(r) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

// Wraps an expression used in statement position, eliminating the need for
// an undefined ExprNode* -> StmtNode* reinterpret cast.
class ExprStmtNode : public StmtNode {
public:
    std::unique_ptr<ExprNode> expr;
    explicit ExprStmtNode(ExprNode* e) noexcept : expr(e) {}
    void accept(Visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needsSemicolon() const noexcept override { return true; }
};

class ParamNode : public DeclNode {
public:
    std::string name;
    std::string type;
    bool isRef;
    ParamNode(std::string n, std::string t, bool r)
        : name(std::move(n)), type(std::move(t)), isRef(r) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class VarDeclNode : public StmtNode {
public:
    std::string name;
    std::string type;
    std::unique_ptr<ExprNode> init;
    VarDeclNode(std::string n, std::string t, ExprNode* i)
        : name(std::move(n)), type(std::move(t)), init(i) {}
    void accept(Visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needsSemicolon() const noexcept override { return true; }
};

class ReturnStmtNode : public StmtNode {
public:
    std::unique_ptr<ExprNode> value;
    explicit ReturnStmtNode(ExprNode* v) noexcept : value(v) {}
    void accept(Visitor& vis) const override { vis.visit(*this); }
    [[nodiscard]] bool needsSemicolon() const noexcept override { return true; }
};

class ForStmtNode : public StmtNode {
public:
    std::unique_ptr<StmtNode> init;
    std::unique_ptr<ExprNode> condition;
    std::unique_ptr<ExprNode> increment;
    std::unique_ptr<StmtNode> body;
    ForStmtNode(StmtNode* i, ExprNode* c, ExprNode* inc, StmtNode* b)
        : init(i), condition(c), increment(inc), body(b) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class CompoundStmtNode : public StmtNode {
public:
    std::vector<std::unique_ptr<StmtNode>> statements;
    void add(StmtNode* s) { statements.emplace_back(s); }
    void accept(Visitor& v) const override { v.visit(*this); }
};

class FuncDeclNode : public DeclNode {
public:
    std::string name;
    std::vector<std::unique_ptr<ParamNode>> params;
    std::string returnType;
    std::unique_ptr<StmtNode> body;
    FuncDeclNode(std::string n, std::string rt, StmtNode* b)
        : name(std::move(n)), returnType(std::move(rt)), body(b) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class StructFieldNode : public DeclNode {
public:
    std::string name;
    std::string type;
    StructFieldNode(std::string n, std::string t) : name(std::move(n)), type(std::move(t)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class StructTypeNode : public TypeBodyNode {
public:
    std::string name;
    std::string parent;
    std::vector<std::unique_ptr<StructFieldNode>> fields;
    StructTypeNode(std::string n, std::string p) : name(std::move(n)), parent(std::move(p)) {}
    void addField(StructFieldNode* f) { fields.emplace_back(f); }
    void accept(Visitor& v) const override { v.visit(*this); }
};

class MethodDeclNode : public DeclNode {
public:
    std::string typeName;
    std::string name;
    std::vector<std::unique_ptr<ParamNode>> params;
    std::string returnType;
    std::unique_ptr<StmtNode> body;
    MethodDeclNode(std::string tn, std::string n, std::string rt, StmtNode* b)
        : typeName(std::move(tn)), name(std::move(n)), returnType(std::move(rt)), body(b) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class TypeDeclNode : public DeclNode {
public:
    std::string name;
    std::unique_ptr<TypeBodyNode> body;
    TypeDeclNode(std::string n, TypeBodyNode* b) : name(std::move(n)), body(b) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class InterfaceMethodNode : public ASTNode {
public:
    std::string name;
    std::vector<std::unique_ptr<ParamNode>> params;
    std::string returnType;
    InterfaceMethodNode(std::string n, std::string rt)
        : name(std::move(n)), returnType(std::move(rt)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class InterfaceTypeNode : public TypeBodyNode {
public:
    std::vector<std::unique_ptr<InterfaceMethodNode>> methods;
    void addMethod(InterfaceMethodNode* m) { methods.emplace_back(m); }
    void accept(Visitor& v) const override { v.visit(*this); }
};

class MemberCallNode : public ExprNode {
public:
    std::unique_ptr<ExprNode> object;
    std::string method;
    std::vector<std::unique_ptr<ExprNode>> args;
    MemberCallNode(ExprNode* obj, std::string m) : object(obj), method(std::move(m)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class QualifiedCallNode : public ExprNode {
public:
    std::string qualifier;
    std::string name;
    std::vector<std::unique_ptr<ExprNode>> args;
    QualifiedCallNode(std::string q, std::string n) : qualifier(std::move(q)), name(std::move(n)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class CallNode : public ExprNode {
public:
    std::string name;
    std::vector<std::unique_ptr<ExprNode>> args;
    explicit CallNode(std::string n) : name(std::move(n)) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class IndexNode : public ExprNode {
public:
    std::unique_ptr<ExprNode> base;
    std::unique_ptr<ExprNode> index;
    IndexNode(ExprNode* b, ExprNode* i) : base(b), index(i) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class CompoundAssignNode : public ExprNode {
public:
    std::string op;
    std::unique_ptr<ExprNode> lhs;
    std::unique_ptr<ExprNode> rhs;
    CompoundAssignNode(std::string o, ExprNode* l, ExprNode* r)
        : op(std::move(o)), lhs(l), rhs(r) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class IfStmtNode : public StmtNode {
public:
    std::unique_ptr<ExprNode> condition;
    std::unique_ptr<StmtNode> then_body;
    std::unique_ptr<StmtNode> else_body;  // nullptr if no else
    IfStmtNode(ExprNode* c, StmtNode* t, StmtNode* e) : condition(c), then_body(t), else_body(e) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

// Represents a tuple expression: e1, e2, ... (used in return and tuple-assign).
// NOT part of the general expr grammar to avoid conflicts with arg-list commas.
class TupleExprNode : public ExprNode {
public:
    std::vector<std::unique_ptr<ExprNode>> elements;
    void add(ExprNode* e) { elements.emplace_back(e); }
    void accept(Visitor& v) const override { v.visit(*this); }
};

class TupleVarDeclNode : public StmtNode {
public:
    std::vector<std::string> names;
    std::unique_ptr<ExprNode> init;
    TupleVarDeclNode(std::vector<std::string> ns, ExprNode* i) : names(std::move(ns)), init(i) {}
    void accept(Visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needsSemicolon() const noexcept override { return true; }
};

class TupleAssignStmtNode : public StmtNode {
public:
    std::vector<std::string> lhs_names;
    std::unique_ptr<ExprNode> rhs;
    TupleAssignStmtNode(std::vector<std::string> ns, ExprNode* r)
        : lhs_names(std::move(ns)), rhs(r) {}
    void accept(Visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needsSemicolon() const noexcept override { return true; }
};

class ContinueStmtNode : public StmtNode {
public:
    void accept(Visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needsSemicolon() const noexcept override { return true; }
};

class InitListExprNode : public ExprNode {
public:
    std::vector<std::unique_ptr<ExprNode>> args;
    void accept(Visitor& v) const override { v.visit(*this); }
};

class ForRangeStmtNode : public StmtNode {
public:
    std::string varName;
    std::unique_ptr<ExprNode> range;
    std::unique_ptr<StmtNode> body;
    ForRangeStmtNode(std::string n, ExprNode* r, StmtNode* b)
        : varName(std::move(n)), range(r), body(b) {}
    void accept(Visitor& v) const override { v.visit(*this); }
};

class ProgramNode : public ASTNode {
public:
    std::vector<std::unique_ptr<DeclNode>> declarations;
    void add(DeclNode* d) { declarations.emplace_back(d); }
    void accept(Visitor& v) const override { v.visit(*this); }
};

#endif  // AST_H
