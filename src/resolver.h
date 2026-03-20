#ifndef RESOLVER_H
#define RESOLVER_H

#include "diagnostics.h"
#include "symbol_table.h"
#include "types.h"
#include "unify.h"
#include "visitor.h"

// Name resolution visitor.  Two sub-passes over top-level declarations:
//   1. Register all type names (forward-declaration effect)
//   2. Fill in struct fields, interface methods, function signatures, parents
class resolver : public visitor {
public:
    resolver(symbol_table& symtab, type_env& env, diagnostics& diag)
        : symtab_(symtab), env_(env), diag_(diag) {}

    // Entry point: runs both sub-passes.
    void resolve(const class program_node& prog);

    // visitor interface — only the top-level nodes are meaningful.
    void visit(const class program_node&) override;
    void visit(const class func_decl_node&) override;
    void visit(const class method_decl_node&) override;
    void visit(const class type_decl_node&) override;
    void visit(const class struct_type_node&) override;
    void visit(const class interface_type_node&) override;

    // No-ops for nodes not relevant during resolution.
    void visit(const class identifier_node&) override {}
    void visit(const class number_node&) override {}
    void visit(const class string_node&) override {}
    void visit(const class binary_op_node&) override {}
    void visit(const class unary_op_node&) override {}
    void visit(const class assign_node&) override {}
    void visit(const class expr_stmt_node&) override {}
    void visit(const class param_node&) override {}
    void visit(const class var_decl_node&) override {}
    void visit(const class return_stmt_node&) override {}
    void visit(const class for_stmt_node&) override {}
    void visit(const class compound_stmt_node&) override {}
    void visit(const class struct_field_node&) override {}
    void visit(const class interface_method_node&) override {}
    void visit(const class member_call_node&) override {}
    void visit(const class member_access_node&) override {}
    void visit(const class qualified_call_node&) override {}
    void visit(const class call_node&) override {}
    void visit(const class index_node&) override {}
    void visit(const class compound_assign_node&) override {}
    void visit(const class if_stmt_node&) override {}
    void visit(const class tuple_expr_node&) override {}
    void visit(const class tuple_var_decl_node&) override {}
    void visit(const class tuple_assign_stmt_node&) override {}
    void visit(const class for_range_stmt_node&) override {}
    void visit(const class continue_stmt_node&) override {}
    void visit(const class break_stmt_node&) override {}
    void visit(const class init_list_expr_node&) override {}

private:
    symbol_table& symtab_;
    type_env& env_;
    diagnostics& diag_;

    // Sub-pass tracking
    enum class pass { register_names, fill_details };
    pass current_pass_ = pass::register_names;

    // Context for type_decl -> struct/interface body visitor chain
    std::string current_type_name_;

    // Helpers
    type_ptr resolve_type_or_fresh(const std::string& type_str);
    type_ptr build_fun_type(const std::vector<std::unique_ptr<class param_node>>& params,
                            const std::string& return_type);
};

#endif  // RESOLVER_H
