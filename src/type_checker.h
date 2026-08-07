#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

#include <unordered_map>

#include "diagnostics.h"
#include "symbol_table.h"
#include "types.h"
#include "unify.h"
#include "visitor.h"

// Infers expression types over a resolved AST and reports inconsistencies as
// warnings.  Inferred types live in a side table, so AST nodes stay untouched.
class type_checker : public visitor {
public:
    type_checker(symbol_table& symtab, type_env& env, diagnostics& diag)
        : symtab_(symtab), env_(env), diag_(diag) {}

    // Returns the inferred type of `node`, or nullptr when it has none.
    [[nodiscard]] type_ptr type_of(const ast_node* node) const;

    void visit(const identifier_node&) override;
    void visit(const number_node&) override;
    void visit(const string_node&) override;
    void visit(const binary_op_node&) override;
    void visit(const unary_op_node&) override;
    void visit(const assign_node&) override;
    void visit(const expr_stmt_node&) override;
    void visit(const param_node&) override;
    void visit(const var_decl_node&) override;
    void visit(const return_stmt_node&) override;
    void visit(const for_stmt_node&) override;
    void visit(const compound_stmt_node&) override;
    void visit(const func_decl_node&) override;
    void visit(const struct_field_node&) override;
    void visit(const struct_type_node&) override;
    void visit(const method_decl_node&) override;
    void visit(const type_decl_node&) override;
    void visit(const interface_type_node&) override;
    void visit(const interface_method_node&) override;
    void visit(const member_call_node&) override;
    void visit(const member_access_node&) override;
    void visit(const qualified_call_node&) override;
    void visit(const call_node&) override;
    void visit(const index_node&) override;
    void visit(const compound_assign_node&) override;
    void visit(const if_stmt_node&) override;
    void visit(const tuple_expr_node&) override;
    void visit(const tuple_var_decl_node&) override;
    void visit(const tuple_assign_stmt_node&) override;
    void visit(const for_range_stmt_node&) override;
    void visit(const continue_stmt_node&) override;
    void visit(const break_stmt_node&) override;
    void visit(const init_list_expr_node&) override;
    void visit(const program_node&) override;

private:
    symbol_table& symtab_;
    type_env& env_;
    diagnostics& diag_;

    std::unordered_map<const ast_node*, type_ptr> type_map_;

    // Return type of the function body being checked, if any.
    type_ptr current_return_type_;

    void set_type(const ast_node& node, type_ptr t);

    // Visits `node` and returns its type, or a fresh variable if it has none.
    type_ptr infer_expr(const expr_node& node);

    // Unifies, downgrading failure to a warning on `node`.
    void try_unify(const ast_node& node, type_ptr a, type_ptr b, const std::string& context);

    // Infers the arguments against `callee` and returns the call's result type.
    // `callee_name` only appears in diagnostics.
    type_ptr infer_call(const ast_node& node, const type_ptr& callee,
                        const std::vector<std::unique_ptr<expr_node>>& args,
                        const std::string& callee_name);

    // Return the member's type, or a fresh variable when it is not declared.
    type_ptr lookup_field(type_ptr obj_type, const std::string& field);
    type_ptr lookup_method(type_ptr obj_type, const std::string& method);

    // Binds `ti`'s fields, inherited ones included, in the current scope.
    void bind_fields(const type_info& ti);

    // Checks a function or method body in a fresh scope holding its parameters.
    // Pass the owning type in `fields` for a method, nullptr for a function.
    void check_body(const fun_type_t& fun, const std::vector<std::unique_ptr<param_node>>& params,
                    const stmt_node& body, const type_info* fields);
};

#endif  // TYPE_CHECKER_H
