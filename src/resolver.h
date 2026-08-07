#ifndef RESOLVER_H
#define RESOLVER_H

#include "diagnostics.h"
#include "symbol_table.h"
#include "types.h"
#include "unify.h"
#include "visitor.h"

// Registers the declared types, their members, and the function signatures, so
// that the type checker can resolve names regardless of declaration order.
class resolver : public visitor {
public:
    resolver(symbol_table& symtab, type_env& env, diagnostics& diag)
        : symtab_(symtab), env_(env), diag_(diag) {}

    void resolve(const program_node& prog);

    void visit(const program_node&) override;
    void visit(const func_decl_node&) override;
    void visit(const method_decl_node&) override;
    void visit(const type_decl_node&) override;
    void visit(const struct_type_node&) override;
    void visit(const interface_type_node&) override;

    // Resolution only looks at declarations, so every other node is a no-op.
    void visit(const identifier_node&) override {}
    void visit(const number_node&) override {}
    void visit(const string_node&) override {}
    void visit(const binary_op_node&) override {}
    void visit(const unary_op_node&) override {}
    void visit(const assign_node&) override {}
    void visit(const expr_stmt_node&) override {}
    void visit(const param_node&) override {}
    void visit(const var_decl_node&) override {}
    void visit(const return_stmt_node&) override {}
    void visit(const for_stmt_node&) override {}
    void visit(const compound_stmt_node&) override {}
    void visit(const struct_field_node&) override {}
    void visit(const interface_method_node&) override {}
    void visit(const member_call_node&) override {}
    void visit(const member_access_node&) override {}
    void visit(const qualified_call_node&) override {}
    void visit(const call_node&) override {}
    void visit(const index_node&) override {}
    void visit(const compound_assign_node&) override {}
    void visit(const if_stmt_node&) override {}
    void visit(const tuple_expr_node&) override {}
    void visit(const tuple_var_decl_node&) override {}
    void visit(const tuple_assign_stmt_node&) override {}
    void visit(const for_range_stmt_node&) override {}
    void visit(const continue_stmt_node&) override {}
    void visit(const break_stmt_node&) override {}
    void visit(const init_list_expr_node&) override {}

private:
    symbol_table& symtab_;
    type_env& env_;
    diagnostics& diag_;

    // resolve() runs the whole program through each pass in declaration order.
    enum class pass { register_names, fill_types, link };
    pass current_pass_ = pass::register_names;

    // Type currently being filled, set by visit(type_decl_node).
    std::string current_type_name_;

    type_ptr build_fun_type(const std::vector<std::unique_ptr<param_node>>& params,
                            const std::string& return_type);
};

#endif  // RESOLVER_H
