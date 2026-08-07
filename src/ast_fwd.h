#ifndef AST_FWD_H
#define AST_FWD_H

// Forward declarations of every AST class.  Included by visitor.h and, via the
// Bison %code requires block, by the generated parser.hpp.
class ast_node;
class expr_node;
class stmt_node;
class decl_node;
class type_body_node;
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

#endif  // AST_FWD_H
