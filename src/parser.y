// Emitted into both parser.cpp and the generated parser.hpp, so that every file
// including parser.hpp (notably the lexer) can name the %union member types.
%code requires {
#include <string>
#include <vector>
#include "ast_fwd.h"
}

%{
#include <cstdio>
#include <cstdlib>
#include <format>
#include <iostream>
#include <memory>

#include "ast.h"

extern int yylex();
extern int yylineno;
extern char* yytext;
void yyerror(const char* s);

// Records the current source line on a freshly allocated node.
template <typename T>
T* loc(T* node) { node->line = yylineno; return node; }

// Bison's %union is a C union, so semantic values are raw owning pointers: every
// action moves them into an AST node or a local unique_ptr guard, and the
// %destructor rules free whatever error recovery discards.
%}

%union {
    int intval;
    std::string* stringval;
    ast_node* node;
    std::vector<ast_node*>* nodevec;
    std::vector<param_node*>* paramvec;
    std::vector<std::string*>* namevec;
}

%token <stringval> IDENTIFIER
%token <intval> NUMBER
%token FUN VAR REF RETURN FOR IF ELSE CONTINUE BREAK
%token LE GE EQ NE AND OR
%token INC DEC
%token PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ
%token TYPE STRUCT STRING_TYPE INTERFACE
%token INT_TYPE BOOL_TYPE BYTE_TYPE FLOAT_TYPE DOUBLE_TYPE CHAR_TYPE INTEGER_TYPE
%token COLONCOLON
%token ARROW
%token <stringval> STRING_LITERAL

%type <node> expr stmt decl func_decl compound_stmt
%type <node> for_stmt return_stmt var_decl
%type <node> type_decl struct_type program method_decl
%type <node> interface_type interface_method param
%type <node> if_stmt tuple_var_decl tuple_assign_stmt for_range_stmt continue_stmt break_stmt
%type <node> tuple_expr tuple_rhs
%type <paramvec> param_list_opt param_list
%type <stringval> type_spec method_name base_type sized_int_type inner_type ret_type_opt
%type <nodevec> stmt_list_opt stmt_list field_list_opt field_list
%type <nodevec> interface_method_list interface_method_list_opt
%type <nodevec> arg_list_opt arg_list lvalue_list
%type <namevec> name_list

%left '=' PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ
%left AND OR
%left LE GE '<' '>' EQ NE
%left '+' '-'
%left '*' '/' '%'
%right '!' INC DEC
%precedence '[' '.'

%start program

/* Cleanup discarded stack values during error recovery. */
%destructor { delete $$; } <node>
%destructor { delete $$; } <stringval>
%destructor { if ($$) { for (auto* n : *$$) delete n; delete $$; } } <nodevec>
%destructor { if ($$) { for (auto* p : *$$) delete p; delete $$; } } <paramvec>
%destructor { if ($$) { for (auto* s : *$$) delete s; delete $$; } } <namevec>

%%

program
    : stmt_list_opt
        {
            root = std::make_unique<program_node>();
            root->line = yylineno;
            auto list = std::unique_ptr<std::vector<ast_node*>>($1);
            for (auto* n : *list) {
                root->add(static_cast<decl_node*>(n));
            }
            // 'root' owns the tree now; the <node> %destructor deletes nullptr.
            $$ = nullptr;
        }
    ;

stmt_list_opt
    : stmt_list
    | %empty { $$ = new std::vector<ast_node*>(); }
    ;

stmt_list
    : stmt
        {
            $$ = new std::vector<ast_node*>();
            $$->push_back($1);
        }
    | stmt_list stmt
        {
            $$ = $1;
            $$->push_back($2);
        }
    ;

stmt
    : decl
    | var_decl
    | tuple_var_decl
    | for_stmt
    | for_range_stmt
    | return_stmt
    | if_stmt
    | continue_stmt
    | break_stmt
    | tuple_assign_stmt
    | expr ';'
        { $$ = loc(new expr_stmt_node(static_cast<expr_node*>($1))); }
    | compound_stmt
    ;

decl
    : func_decl
    | type_decl
    | method_decl
    ;

// An empty ret_type_opt yields an empty string, which every consumer reads as
// "no annotation".
ret_type_opt
    : %empty          { $$ = new std::string(); }
    | ARROW type_spec { $$ = $2; }
    ;

func_decl
    : FUN IDENTIFIER '(' param_list_opt ')' ret_type_opt compound_stmt
        {
            auto name = std::unique_ptr<std::string>($2);
            auto ret  = std::unique_ptr<std::string>($6);
            auto* node = loc(new func_decl_node(
                std::move(*name), std::move(*ret), static_cast<stmt_node*>($7)));
            auto params = std::unique_ptr<std::vector<param_node*>>($4);
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    ;

type_decl
    : TYPE IDENTIFIER '=' struct_type
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = loc(new type_decl_node(std::move(*name),
                                    static_cast<type_body_node*>($4)));
        }
    | TYPE IDENTIFIER '=' interface_type
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = loc(new type_decl_node(std::move(*name),
                                    static_cast<type_body_node*>($4)));
        }
    ;

struct_type
    : STRUCT '{' field_list_opt '}'
        {
            auto* node = loc(new struct_type_node("", ""));
            auto list = std::unique_ptr<std::vector<ast_node*>>($3);
            for (auto* n : *list) node->add_field(static_cast<struct_field_node*>(n));
            $$ = node;
        }
    | STRUCT ':' IDENTIFIER '{' field_list_opt '}'
        {
            auto parent = std::unique_ptr<std::string>($3);
            auto* node = loc(new struct_type_node("", std::move(*parent)));
            auto list = std::unique_ptr<std::vector<ast_node*>>($5);
            for (auto* n : *list) node->add_field(static_cast<struct_field_node*>(n));
            $$ = node;
        }
    ;

interface_type
    : INTERFACE '{' interface_method_list_opt '}'
        {
            auto* node = loc(new interface_type_node());
            auto list = std::unique_ptr<std::vector<ast_node*>>($3);
            for (auto* n : *list) node->add_method(static_cast<interface_method_node*>(n));
            $$ = node;
        }
    ;

interface_method_list_opt
    : interface_method_list
    | %empty { $$ = new std::vector<ast_node*>(); }
    ;

interface_method_list
    : interface_method
        {
            $$ = new std::vector<ast_node*>();
            $$->push_back($1);
        }
    | interface_method_list interface_method
        {
            $$ = $1;
            $$->push_back($2);
        }
    ;

interface_method
    : method_name '(' param_list_opt ')' ret_type_opt ';'
        {
            auto mname = std::unique_ptr<std::string>($1);
            auto ret   = std::unique_ptr<std::string>($5);
            auto* node = loc(new interface_method_node(std::move(*mname), std::move(*ret)));
            auto params = std::unique_ptr<std::vector<param_node*>>($3);
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    ;

// Base type keywords are valid method names, e.g. the `string()` of a stringer.
method_name
    : IDENTIFIER { $$ = $1; }
    | base_type  { $$ = $1; }
    ;

field_list_opt
    : field_list
    | %empty { $$ = new std::vector<ast_node*>(); }
    ;

field_list
    : IDENTIFIER ':' type_spec ';'
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = new std::vector<ast_node*>();
            $$->push_back(loc(new struct_field_node(std::move(*name), std::move(*type))));
        }
    | field_list IDENTIFIER ':' type_spec ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            $$ = $1;
            $$->push_back(loc(new struct_field_node(std::move(*name), std::move(*type))));
        }
    ;

method_decl
    : FUN IDENTIFIER COLONCOLON IDENTIFIER '(' param_list_opt ')' ret_type_opt compound_stmt
        {
            auto type_name   = std::unique_ptr<std::string>($2);
            auto method_name = std::unique_ptr<std::string>($4);
            auto ret         = std::unique_ptr<std::string>($8);
            auto* node = loc(new method_decl_node(
                std::move(*type_name), std::move(*method_name),
                std::move(*ret), static_cast<stmt_node*>($9)));
            auto params = std::unique_ptr<std::vector<param_node*>>($6);
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    ;

param_list_opt
    : param_list
    | %empty { $$ = new std::vector<param_node*>(); }
    ;

param_list
    : param
        {
            $$ = new std::vector<param_node*>();
            $$->push_back(static_cast<param_node*>($1));
        }
    | param_list ',' param
        {
            $$ = $1;
            $$->push_back(static_cast<param_node*>($3));
        }
    ;

// `ref` may precede or follow the type; both spell the same by-reference param.
param
    : IDENTIFIER
        {
            auto name = std::unique_ptr<std::string>($1);
            $$ = loc(new param_node(std::move(*name), "", false));
        }
    | IDENTIFIER ':' REF
        {
            auto name = std::unique_ptr<std::string>($1);
            $$ = loc(new param_node(std::move(*name), "", true));
        }
    | IDENTIFIER ':' type_spec
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = loc(new param_node(std::move(*name), std::move(*type), false));
        }
    | IDENTIFIER ':' type_spec REF
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = loc(new param_node(std::move(*name), std::move(*type), true));
        }
    | IDENTIFIER ':' REF type_spec
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($4);
            $$ = loc(new param_node(std::move(*name), std::move(*type), true));
        }
    ;

type_spec
    : IDENTIFIER      { $$ = $1; }
    | base_type       { $$ = $1; }
    | sized_int_type  { $$ = $1; }
    | IDENTIFIER '<' inner_type '>'
        {
            auto outer = std::unique_ptr<std::string>($1);
            auto inner = std::unique_ptr<std::string>($3);
            $$ = new std::string(std::format("{}<{}>", *outer, *inner));
        }
    ;

base_type
    : STRING_TYPE   { $$ = new std::string("string"); }
    | INT_TYPE      { $$ = new std::string("int"); }
    | BOOL_TYPE     { $$ = new std::string("bool"); }
    | BYTE_TYPE     { $$ = new std::string("byte"); }
    | FLOAT_TYPE    { $$ = new std::string("float"); }
    | DOUBLE_TYPE   { $$ = new std::string("double"); }
    | CHAR_TYPE     { $$ = new std::string("char"); }
    ;

sized_int_type
    : INTEGER_TYPE '<' NUMBER '>'
        { $$ = new std::string(std::format("integer<{}>", $3)); }
    | INTEGER_TYPE '<' '+' NUMBER '>'
        { $$ = new std::string(std::format("integer<+{}>", $4)); }
    ;

inner_type
    : IDENTIFIER      { $$ = $1; }
    | base_type       { $$ = $1; }
    | sized_int_type  { $$ = $1; }
    ;

var_decl
    : VAR IDENTIFIER '=' expr ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = loc(new var_decl_node(std::move(*name), "",
                                   static_cast<expr_node*>($4)));
        }
    | VAR IDENTIFIER ':' type_spec '=' expr ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            $$ = loc(new var_decl_node(std::move(*name), std::move(*type),
                                   static_cast<expr_node*>($6)));
        }
    | VAR IDENTIFIER ':' type_spec '=' '{' arg_list_opt '}' ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            auto* init = loc(new init_list_expr_node());
            auto args = std::unique_ptr<std::vector<ast_node*>>($7);
            for (auto* a : *args) init->args.emplace_back(static_cast<expr_node*>(a));
            $$ = loc(new var_decl_node(std::move(*name), std::move(*type), init));
        }
    | VAR IDENTIFIER ':' type_spec ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            $$ = loc(new var_decl_node(std::move(*name), std::move(*type), nullptr));
        }
    ;

for_stmt
    : FOR var_decl expr ';' expr compound_stmt
        {
            $$ = loc(new for_stmt_node(
                static_cast<stmt_node*>($2),
                static_cast<expr_node*>($3),
                static_cast<expr_node*>($5),
                static_cast<stmt_node*>($6)));
        }
    | FOR expr ';' expr ';' expr compound_stmt
        {
            $$ = loc(new for_stmt_node(
                loc(new expr_stmt_node(static_cast<expr_node*>($2))),
                static_cast<expr_node*>($4),
                static_cast<expr_node*>($6),
                static_cast<stmt_node*>($7)));
        }
    ;

return_stmt
    : RETURN expr ';'
        { $$ = loc(new return_stmt_node(static_cast<expr_node*>($2))); }
    | RETURN tuple_expr ';'
        { $$ = loc(new return_stmt_node(static_cast<expr_node*>($2))); }
    | RETURN ';'
        { $$ = loc(new return_stmt_node(nullptr)); }
    ;

if_stmt
    : IF expr compound_stmt
        {
            $$ = loc(new if_stmt_node(static_cast<expr_node*>($2),
                                  static_cast<stmt_node*>($3), nullptr));
        }
    | IF expr compound_stmt ELSE compound_stmt
        {
            $$ = loc(new if_stmt_node(static_cast<expr_node*>($2),
                                  static_cast<stmt_node*>($3),
                                  static_cast<stmt_node*>($5)));
        }
    ;

continue_stmt
    : CONTINUE ';'
        { $$ = loc(new continue_stmt_node()); }
    ;

break_stmt
    : BREAK ';'
        { $$ = loc(new break_stmt_node()); }
    ;

for_range_stmt
    : FOR VAR IDENTIFIER '=' expr compound_stmt
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = loc(new for_range_stmt_node(std::move(*name),
                                         static_cast<expr_node*>($5),
                                         static_cast<stmt_node*>($6)));
        }
    ;

name_list
    : IDENTIFIER ',' IDENTIFIER
        {
            $$ = new std::vector<std::string*>();
            $$->push_back($1);
            $$->push_back($3);
        }
    | name_list ',' IDENTIFIER
        {
            $$ = $1;
            $$->push_back($3);
        }
    ;

// %prec '=' resolves the shift/reduce conflict when '=' follows the last
// lvalue_list element: the reduction (same level, %left) beats the shift.
lvalue_list
    : expr ',' expr %prec '='
        {
            $$ = new std::vector<ast_node*>();
            $$->push_back($1);
            $$->push_back($3);
        }
    | lvalue_list ',' expr %prec '='
        {
            $$ = $1;
            $$->push_back($3);
        }
    ;

tuple_var_decl
    : VAR name_list '=' expr ';'
        {
            auto names_raw = std::unique_ptr<std::vector<std::string*>>($2);
            std::vector<std::string> names;
            for (auto* s : *names_raw) { names.push_back(std::move(*s)); delete s; }
            $$ = loc(new tuple_var_decl_node(std::move(names),
                                         static_cast<expr_node*>($4)));
        }
    ;

// Mirrors return_stmt: the right-hand side may be a tuple or a single value.
tuple_rhs
    : expr       { $$ = $1; }
    | tuple_expr { $$ = $1; }
    ;

tuple_assign_stmt
    : lvalue_list '=' tuple_rhs ';'
        {
            auto lhs_raw = std::unique_ptr<std::vector<ast_node*>>($1);
            std::vector<std::unique_ptr<expr_node>> lhs;
            for (auto* n : *lhs_raw) lhs.emplace_back(static_cast<expr_node*>(n));
            $$ = loc(new tuple_assign_stmt_node(std::move(lhs),
                                            static_cast<expr_node*>($3)));
        }
    ;

tuple_expr
    : expr ',' expr
        {
            auto* node = loc(new tuple_expr_node());
            node->add(static_cast<expr_node*>($1));
            node->add(static_cast<expr_node*>($3));
            $$ = node;
        }
    | tuple_expr ',' expr
        {
            auto* node = static_cast<tuple_expr_node*>($1);
            node->add(static_cast<expr_node*>($3));
            $$ = node;
        }
    ;

compound_stmt
    : '{' stmt_list_opt '}'
        {
            auto* node = loc(new compound_stmt_node());
            auto list = std::unique_ptr<std::vector<ast_node*>>($2);
            for (auto* n : *list) node->add(static_cast<stmt_node*>(n));
            $$ = node;
        }
    ;

expr
    : NUMBER
        { $$ = loc(new number_node($1)); }
    | STRING_LITERAL
        {
            auto s = std::unique_ptr<std::string>($1);
            $$ = loc(new string_node(std::move(*s)));
        }
    | IDENTIFIER
        {
            auto id = std::unique_ptr<std::string>($1);
            $$ = loc(new identifier_node(std::move(*id)));
        }
    | expr '+' expr
        { $$ = loc(new binary_op_node("+", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '-' expr
        { $$ = loc(new binary_op_node("-", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '*' expr
        { $$ = loc(new binary_op_node("*", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '/' expr
        { $$ = loc(new binary_op_node("/", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr LE expr
        { $$ = loc(new binary_op_node("<=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr GE expr
        { $$ = loc(new binary_op_node(">=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '<' expr
        { $$ = loc(new binary_op_node("<", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '>' expr
        { $$ = loc(new binary_op_node(">", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr EQ expr
        { $$ = loc(new binary_op_node("==", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr NE expr
        { $$ = loc(new binary_op_node("!=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr AND expr
        { $$ = loc(new binary_op_node("&&", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr OR expr
        { $$ = loc(new binary_op_node("||", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '%' expr
        { $$ = loc(new binary_op_node("%", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr '=' expr
        { $$ = loc(new assign_node(static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr PLUS_EQ expr
        { $$ = loc(new compound_assign_node("+=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr MINUS_EQ expr
        { $$ = loc(new compound_assign_node("-=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr STAR_EQ expr
        { $$ = loc(new compound_assign_node("*=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | expr SLASH_EQ expr
        { $$ = loc(new compound_assign_node("/=", static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | '!' expr
        { $$ = loc(new unary_op_node("!", static_cast<expr_node*>($2))); }
    | INC expr
        { $$ = loc(new unary_op_node("++", static_cast<expr_node*>($2))); }
    | DEC expr
        { $$ = loc(new unary_op_node("--", static_cast<expr_node*>($2))); }
    | expr INC
        { $$ = loc(new unary_op_node("post++", static_cast<expr_node*>($1))); }
    | expr DEC
        { $$ = loc(new unary_op_node("post--", static_cast<expr_node*>($1))); }
    | '(' expr ')'
        { $$ = $2; }
    | expr '[' expr ']'
        { $$ = loc(new index_node(static_cast<expr_node*>($1), static_cast<expr_node*>($3))); }
    | IDENTIFIER '(' arg_list_opt ')'
        {
            auto name = std::unique_ptr<std::string>($1);
            auto* node = loc(new call_node(std::move(*name)));
            auto args = std::unique_ptr<std::vector<ast_node*>>($3);
            for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            $$ = node;
        }
    | expr '.' IDENTIFIER
        {
            auto fname = std::unique_ptr<std::string>($3);
            $$ = loc(new member_access_node(
                static_cast<expr_node*>($1), std::move(*fname)));
        }
    | expr '.' method_name '(' arg_list_opt ')'
        {
            auto mname = std::unique_ptr<std::string>($3);
            auto* node = loc(new member_call_node(
                static_cast<expr_node*>($1), std::move(*mname)));
            auto args = std::unique_ptr<std::vector<ast_node*>>($5);
            for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            $$ = node;
        }
    | IDENTIFIER COLONCOLON method_name '(' arg_list_opt ')'
        {
            auto qual  = std::unique_ptr<std::string>($1);
            auto mname = std::unique_ptr<std::string>($3);
            auto* node = loc(new qualified_call_node(std::move(*qual), std::move(*mname)));
            auto args = std::unique_ptr<std::vector<ast_node*>>($5);
            for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            $$ = node;
        }
    ;

arg_list_opt
    : arg_list
    | %empty { $$ = new std::vector<ast_node*>(); }
    ;

arg_list
    : expr
        {
            $$ = new std::vector<ast_node*>();
            $$->push_back($1);
        }
    | arg_list ',' expr
        {
            $$ = $1;
            $$->push_back($3);
        }
    ;

%%

void yyerror(const char* s) {
    std::cerr << std::format("Error at line {}: {}\n", yylineno, s);
}
