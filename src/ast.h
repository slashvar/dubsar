#ifndef AST_H
#define AST_H

#include <memory>
#include <string>
#include <vector>

#include "visitor.h"

// Forward declarations
class ast_node;
class expr_node;
class stmt_node;
class decl_node;
class type_body_node;
class program_node;
class func_decl_node;
class var_decl_node;
class for_stmt_node;
class compound_stmt_node;
class expr_stmt_node;
class param_node;
class type_decl_node;
class struct_type_node;
class struct_field_node;
class method_decl_node;
class interface_type_node;
class interface_method_node;
class member_call_node;
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

// Global root for the AST - defined in ast.cpp
extern std::unique_ptr<program_node> root;

class ast_node {
public:
    virtual void accept(visitor& v) const = 0;
    [[nodiscard]] virtual bool needs_semicolon() const noexcept { return false; }
    virtual ~ast_node() = default;
};

class expr_node : public ast_node {};

class stmt_node : public ast_node {};

// decl_node inherits stmt_node: declarations are valid statement-context nodes,
// which avoids undefined-behaviour casts in compound-statement handling.
class decl_node : public stmt_node {};

// Abstract base for type bodies (struct and interface).
class type_body_node : public ast_node {};

class identifier_node : public expr_node {
public:
    std::string name;
    explicit identifier_node(std::string n) noexcept : name(std::move(n)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class number_node : public expr_node {
public:
    int value;
    explicit number_node(int v) noexcept : value(v) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class string_node : public expr_node {
public:
    std::string value;
    explicit string_node(std::string v) noexcept : value(std::move(v)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class binary_op_node : public expr_node {
public:
    std::string op;
    std::unique_ptr<expr_node> left;
    std::unique_ptr<expr_node> right;
    binary_op_node(std::string o, expr_node* l, expr_node* r)
        : op(std::move(o)), left(l), right(r) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class unary_op_node : public expr_node {
public:
    std::string op;
    std::unique_ptr<expr_node> operand;
    unary_op_node(std::string o, expr_node* a) : op(std::move(o)), operand(a) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class assign_node : public expr_node {
public:
    std::unique_ptr<expr_node> lhs;
    std::unique_ptr<expr_node> rhs;
    assign_node(expr_node* l, expr_node* r) : lhs(l), rhs(r) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

// Wraps an expression used in statement position, eliminating the need for
// an undefined expr_node* -> stmt_node* reinterpret cast.
class expr_stmt_node : public stmt_node {
public:
    std::unique_ptr<expr_node> expr;
    explicit expr_stmt_node(expr_node* e) noexcept : expr(e) {}
    void accept(visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class param_node : public decl_node {
public:
    std::string name;
    std::string type;
    bool is_ref;
    param_node(std::string n, std::string t, bool r)
        : name(std::move(n)), type(std::move(t)), is_ref(r) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class var_decl_node : public stmt_node {
public:
    std::string name;
    std::string type;
    std::unique_ptr<expr_node> init;
    var_decl_node(std::string n, std::string t, expr_node* i)
        : name(std::move(n)), type(std::move(t)), init(i) {}
    void accept(visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class return_stmt_node : public stmt_node {
public:
    std::unique_ptr<expr_node> value;
    explicit return_stmt_node(expr_node* v) noexcept : value(v) {}
    void accept(visitor& vis) const override { vis.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class for_stmt_node : public stmt_node {
public:
    std::unique_ptr<stmt_node> init;
    std::unique_ptr<expr_node> condition;
    std::unique_ptr<expr_node> increment;
    std::unique_ptr<stmt_node> body;
    for_stmt_node(stmt_node* i, expr_node* c, expr_node* inc, stmt_node* b)
        : init(i), condition(c), increment(inc), body(b) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class compound_stmt_node : public stmt_node {
public:
    std::vector<std::unique_ptr<stmt_node>> statements;
    void add(stmt_node* s) { statements.emplace_back(s); }
    void accept(visitor& v) const override { v.visit(*this); }
};

class func_decl_node : public decl_node {
public:
    std::string name;
    std::vector<std::unique_ptr<param_node>> params;
    std::string return_type;
    std::unique_ptr<stmt_node> body;
    func_decl_node(std::string n, std::string rt, stmt_node* b)
        : name(std::move(n)), return_type(std::move(rt)), body(b) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class struct_field_node : public decl_node {
public:
    std::string name;
    std::string type;
    struct_field_node(std::string n, std::string t) : name(std::move(n)), type(std::move(t)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class struct_type_node : public type_body_node {
public:
    std::string name;
    std::string parent;
    std::vector<std::unique_ptr<struct_field_node>> fields;
    struct_type_node(std::string n, std::string p) : name(std::move(n)), parent(std::move(p)) {}
    void add_field(struct_field_node* f) { fields.emplace_back(f); }
    void accept(visitor& v) const override { v.visit(*this); }
};

class method_decl_node : public decl_node {
public:
    std::string type_name;
    std::string name;
    std::vector<std::unique_ptr<param_node>> params;
    std::string return_type;
    std::unique_ptr<stmt_node> body;
    method_decl_node(std::string tn, std::string n, std::string rt, stmt_node* b)
        : type_name(std::move(tn)), name(std::move(n)), return_type(std::move(rt)), body(b) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class type_decl_node : public decl_node {
public:
    std::string name;
    std::unique_ptr<type_body_node> body;
    type_decl_node(std::string n, type_body_node* b) : name(std::move(n)), body(b) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class interface_method_node : public ast_node {
public:
    std::string name;
    std::vector<std::unique_ptr<param_node>> params;
    std::string return_type;
    interface_method_node(std::string n, std::string rt)
        : name(std::move(n)), return_type(std::move(rt)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class interface_type_node : public type_body_node {
public:
    std::vector<std::unique_ptr<interface_method_node>> methods;
    void add_method(interface_method_node* m) { methods.emplace_back(m); }
    void accept(visitor& v) const override { v.visit(*this); }
};

class member_call_node : public expr_node {
public:
    std::unique_ptr<expr_node> object;
    std::string method;
    std::vector<std::unique_ptr<expr_node>> args;
    member_call_node(expr_node* obj, std::string m) : object(obj), method(std::move(m)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class qualified_call_node : public expr_node {
public:
    std::string qualifier;
    std::string name;
    std::vector<std::unique_ptr<expr_node>> args;
    qualified_call_node(std::string q, std::string n)
        : qualifier(std::move(q)), name(std::move(n)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class call_node : public expr_node {
public:
    std::string name;
    std::vector<std::unique_ptr<expr_node>> args;
    explicit call_node(std::string n) : name(std::move(n)) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class index_node : public expr_node {
public:
    std::unique_ptr<expr_node> base;
    std::unique_ptr<expr_node> index;
    index_node(expr_node* b, expr_node* i) : base(b), index(i) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class compound_assign_node : public expr_node {
public:
    std::string op;
    std::unique_ptr<expr_node> lhs;
    std::unique_ptr<expr_node> rhs;
    compound_assign_node(std::string o, expr_node* l, expr_node* r)
        : op(std::move(o)), lhs(l), rhs(r) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class if_stmt_node : public stmt_node {
public:
    std::unique_ptr<expr_node> condition;
    std::unique_ptr<stmt_node> then_body;
    std::unique_ptr<stmt_node> else_body;  // nullptr if no else
    if_stmt_node(expr_node* c, stmt_node* t, stmt_node* e)
        : condition(c), then_body(t), else_body(e) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

// Represents a tuple expression: e1, e2, ... (used in return and tuple-assign).
// NOT part of the general expr grammar to avoid conflicts with arg-list commas.
class tuple_expr_node : public expr_node {
public:
    std::vector<std::unique_ptr<expr_node>> elements;
    void add(expr_node* e) { elements.emplace_back(e); }
    void accept(visitor& v) const override { v.visit(*this); }
};

class tuple_var_decl_node : public stmt_node {
public:
    std::vector<std::string> names;
    std::unique_ptr<expr_node> init;
    tuple_var_decl_node(std::vector<std::string> ns, expr_node* i)
        : names(std::move(ns)), init(i) {}
    void accept(visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class tuple_assign_stmt_node : public stmt_node {
public:
    std::vector<std::string> lhs_names;
    std::unique_ptr<expr_node> rhs;
    tuple_assign_stmt_node(std::vector<std::string> ns, expr_node* r)
        : lhs_names(std::move(ns)), rhs(r) {}
    void accept(visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class continue_stmt_node : public stmt_node {
public:
    void accept(visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class break_stmt_node : public stmt_node {
public:
    void accept(visitor& v) const override { v.visit(*this); }
    [[nodiscard]] bool needs_semicolon() const noexcept override { return true; }
};

class init_list_expr_node : public expr_node {
public:
    std::vector<std::unique_ptr<expr_node>> args;
    void accept(visitor& v) const override { v.visit(*this); }
};

class for_range_stmt_node : public stmt_node {
public:
    std::string var_name;
    std::unique_ptr<expr_node> range;
    std::unique_ptr<stmt_node> body;
    for_range_stmt_node(std::string n, expr_node* r, stmt_node* b)
        : var_name(std::move(n)), range(r), body(b) {}
    void accept(visitor& v) const override { v.visit(*this); }
};

class program_node : public ast_node {
public:
    std::vector<std::unique_ptr<decl_node>> declarations;
    void add(decl_node* d) { declarations.emplace_back(d); }
    void accept(visitor& v) const override { v.visit(*this); }
};

#endif  // AST_H
