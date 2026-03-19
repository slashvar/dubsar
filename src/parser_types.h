#ifndef PARSER_TYPES_H
#define PARSER_TYPES_H

// Minimal type definitions for the parser and lexer
// This file provides forward declarations and token definitions

#include <string>
#include <vector>

// Forward declarations - actual types defined in ast.h
class ast_node;
class param_node;
class decl_node;
class stmt_node;
class expr_node;
class program_node;
class func_decl_node;
class struct_type_node;
class struct_field_node;
class method_decl_node;
class var_decl_node;
class for_stmt_node;
class return_stmt_node;
class compound_stmt_node;
class number_node;
class string_node;
class identifier_node;
class binary_op_node;
class unary_op_node;
class assign_node;
class type_body_node;
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
class init_list_expr_node;

#endif  // PARSER_TYPES_H
