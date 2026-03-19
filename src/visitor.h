#ifndef VISITOR_H
#define VISITOR_H

// Forward declarations of every concrete AST node type.
// Including this header does not pull in the full AST definition.
class identifier_node;
class number_node;
class string_node;
class binary_op_node;
class unary_op_node;
class assign_node;
class expr_stmt_node;
class param_node;
class var_decl_node;
class return_stmt_node;
class for_stmt_node;
class compound_stmt_node;
class func_decl_node;
class struct_field_node;
class struct_type_node;
class method_decl_node;
class type_decl_node;
class interface_type_node;
class interface_method_node;
class member_call_node;
class member_access_node;
class qualified_call_node;
class call_node;
class index_node;
class compound_assign_node;
class if_stmt_node;
class tuple_expr_node;
class tuple_var_decl_node;
class tuple_assign_stmt_node;
class for_range_stmt_node;
class continue_stmt_node;
class break_stmt_node;
class init_list_expr_node;
class program_node;

// Abstract visitor interface.  One visit() overload per concrete node type
// provides static dispatch on the node's type without touching the nodes
// themselves.  New visitors (pretty-printer, type-checker, code-gen, …) are
// added by implementing this interface.
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
