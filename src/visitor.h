#ifndef VISITOR_H
#define VISITOR_H

#include "ast_fwd.h"

// One visit() overload per concrete AST node type.  Implement this interface to
// add a pass (printer, resolver, type checker, code generator, …).
class visitor {
public:
    virtual void visit(const identifier_node&) = 0;
    virtual void visit(const number_node&) = 0;
    virtual void visit(const string_node&) = 0;
    virtual void visit(const binary_op_node&) = 0;
    virtual void visit(const unary_op_node&) = 0;
    virtual void visit(const assign_node&) = 0;
    virtual void visit(const expr_stmt_node&) = 0;
    virtual void visit(const param_node&) = 0;
    virtual void visit(const var_decl_node&) = 0;
    virtual void visit(const return_stmt_node&) = 0;
    virtual void visit(const for_stmt_node&) = 0;
    virtual void visit(const compound_stmt_node&) = 0;
    virtual void visit(const func_decl_node&) = 0;
    virtual void visit(const struct_field_node&) = 0;
    virtual void visit(const struct_type_node&) = 0;
    virtual void visit(const method_decl_node&) = 0;
    virtual void visit(const type_decl_node&) = 0;
    virtual void visit(const interface_type_node&) = 0;
    virtual void visit(const interface_method_node&) = 0;
    virtual void visit(const member_call_node&) = 0;
    virtual void visit(const member_access_node&) = 0;
    virtual void visit(const qualified_call_node&) = 0;
    virtual void visit(const call_node&) = 0;
    virtual void visit(const index_node&) = 0;
    virtual void visit(const compound_assign_node&) = 0;
    virtual void visit(const if_stmt_node&) = 0;
    virtual void visit(const tuple_expr_node&) = 0;
    virtual void visit(const tuple_var_decl_node&) = 0;
    virtual void visit(const tuple_assign_stmt_node&) = 0;
    virtual void visit(const for_range_stmt_node&) = 0;
    virtual void visit(const continue_stmt_node&) = 0;
    virtual void visit(const break_stmt_node&) = 0;
    virtual void visit(const init_list_expr_node&) = 0;
    virtual void visit(const program_node&) = 0;
    virtual ~visitor() = default;
};

#endif  // VISITOR_H
