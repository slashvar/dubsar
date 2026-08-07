#ifndef PRINTER_H
#define PRINTER_H

#include <memory>
#include <ostream>
#include <vector>

#include "visitor.h"

// Serialises an AST back to dubsar source.  The output is round-trip stable:
// parsing it and printing again yields identical text.
class printer : public visitor {
public:
    explicit printer(std::ostream& out) : out_(out) {}

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
    std::ostream& out_;
    int indent_ = 0;

    // Emits `indent_` spaces.
    void pad() const;

    // Emits the nodes comma-separated, without any surrounding brackets.
    template <typename Node>
    void print_list(const std::vector<std::unique_ptr<Node>>& nodes) {
        for (bool first = true; const auto& n : nodes) {
            if (!first) out_ << ", ";
            n->accept(*this);
            first = false;
        }
    }
};

#endif  // PRINTER_H
