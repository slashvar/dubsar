#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

#include <unordered_map>

#include "diagnostics.h"
#include "symbol_table.h"
#include "types.h"
#include "unify.h"
#include "visitor.h"

class ast_node;

// Type inference visitor.  Walks the AST after name resolution, infers types
// for expressions, and checks type consistency.  Expression types are stored
// in a side table to avoid modifying AST node classes.
class type_checker : public visitor {
public:
    type_checker(symbol_table& symtab, type_env& env, diagnostics& diag)
        : symtab_(symtab), env_(env), diag_(diag) {}

    // Side table: maps AST nodes to their inferred types.
    [[nodiscard]] type_ptr type_of(const ast_node* node) const;

    // visitor interface
    void visit(const class identifier_node&) override;
    void visit(const class number_node&) override;
    void visit(const class string_node&) override;
    void visit(const class binary_op_node&) override;
    void visit(const class unary_op_node&) override;
    void visit(const class assign_node&) override;
    void visit(const class expr_stmt_node&) override;
    void visit(const class param_node&) override;
    void visit(const class var_decl_node&) override;
    void visit(const class return_stmt_node&) override;
    void visit(const class for_stmt_node&) override;
    void visit(const class compound_stmt_node&) override;
    void visit(const class func_decl_node&) override;
    void visit(const class struct_field_node&) override;
    void visit(const class struct_type_node&) override;
    void visit(const class method_decl_node&) override;
    void visit(const class type_decl_node&) override;
    void visit(const class interface_type_node&) override;
    void visit(const class interface_method_node&) override;
    void visit(const class member_call_node&) override;
    void visit(const class member_access_node&) override;
    void visit(const class qualified_call_node&) override;
    void visit(const class call_node&) override;
    void visit(const class index_node&) override;
    void visit(const class compound_assign_node&) override;
    void visit(const class if_stmt_node&) override;
    void visit(const class tuple_expr_node&) override;
    void visit(const class tuple_var_decl_node&) override;
    void visit(const class tuple_assign_stmt_node&) override;
    void visit(const class for_range_stmt_node&) override;
    void visit(const class continue_stmt_node&) override;
    void visit(const class break_stmt_node&) override;
    void visit(const class init_list_expr_node&) override;
    void visit(const class program_node&) override;

private:
    symbol_table& symtab_;
    type_env& env_;
    diagnostics& diag_;

    // Expression type side table
    std::unordered_map<const ast_node*, type_ptr> type_map_;

    // Current function's return type (for return statement checking)
    type_ptr current_return_type_;

    void set_type(const ast_node& node, type_ptr t);
    type_ptr infer_expr(const class expr_node& node);
    void try_unify(const ast_node& node, type_ptr a, type_ptr b, const std::string& context);

    // Row-based member lookup helpers
    type_ptr lookup_field(const ast_node& node, type_ptr obj_type, const std::string& field);
    type_ptr lookup_method(const ast_node& node, type_ptr obj_type, const std::string& method);
};

#endif  // TYPE_CHECKER_H
