#ifndef PARSER_TYPES_H
#define PARSER_TYPES_H

// Minimal type definitions for the parser and lexer
// This file provides forward declarations and token definitions

#include <string>
#include <vector>

// Forward declarations - actual types defined in ast.h
class ASTNode;
class ParamNode;
class DeclNode;
class StmtNode;
class ExprNode;
class ProgramNode;
class FuncDeclNode;
class StructTypeNode;
class StructFieldNode;
class MethodDeclNode;
class VarDeclNode;
class ForStmtNode;
class ReturnStmtNode;
class CompoundStmtNode;
class NumberNode;
class StringNode;
class IdentifierNode;
class BinaryOpNode;
class UnaryOpNode;
class AssignNode;
class TypeBodyNode;
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

#endif  // PARSER_TYPES_H