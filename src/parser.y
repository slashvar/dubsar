// %code requires is placed in both parser.cpp and the generated parser.hpp,
// making these types available to any file that includes parser.hpp (e.g. the
// lexer).
%code requires {
#include <string>
#include <vector>
#include "parser_types.h"
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

// NOTE: Bison's %union is a C union and cannot hold non-trivially-destructible
// types such as std::unique_ptr. Raw owning pointers are therefore used inside
// grammar semantic values. Ownership is transferred into AST node unique_ptr
// members immediately at each construction site. String pointers are cleaned
// up via local unique_ptr guards so that no explicit delete calls are needed.
// %destructor rules handle cleanup of values discarded during error recovery.
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
%token COLONCOLON
%token ARROW
%token <stringval> STRING_LITERAL

%type <node> expr stmt decl func_decl compound_stmt
%type <node> for_stmt return_stmt var_decl
%type <node> type_decl struct_type program method_decl
%type <node> interface_type interface_method
%type <node> if_stmt tuple_var_decl tuple_assign_stmt for_range_stmt continue_stmt break_stmt
%type <node> tuple_expr tuple_rhs
%type <paramvec> param_list_opt param_list
%type <stringval> type_spec method_name
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
%left '[' '.'

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
            auto list = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($1));
            if (list) {
                for (auto* n : *list) {
                    root->add(static_cast<decl_node*>(n));
                }
            }
            // Ownership is held by the global 'root'; set $$ to nullptr so
            // the <node> %destructor safely calls delete nullptr on cleanup.
            $$ = nullptr;
        }
    ;

stmt_list_opt
    : stmt_list
    | /* empty */ { $$ = new std::vector<ast_node*>(); }
    ;

stmt_list
    : stmt
        {
            $$ = new std::vector<ast_node*>();
            if ($1) $$->push_back($1);
        }
    | stmt_list stmt
        {
            $$ = $1;
            if ($2) $$->push_back($2);
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
        {
            // Wrap in expr_stmt_node to give the expression a proper stmt_node
            // type, avoiding an undefined expr_node* -> stmt_node* cast.
            $$ = new expr_stmt_node(static_cast<expr_node*>($1));
        }
    | compound_stmt
    ;

decl
    : func_decl
    | type_decl
    | method_decl
        { $$ = $1; }
    ;

func_decl
    : FUN IDENTIFIER '(' param_list_opt ')' compound_stmt
        {
            auto name = std::unique_ptr<std::string>($2);
            auto* node = new func_decl_node(
                std::move(*name), std::string{}, static_cast<stmt_node*>($6));
            auto params = std::unique_ptr<std::vector<param_node*>>(
                static_cast<std::vector<param_node*>*>($4));
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    | FUN IDENTIFIER '(' param_list_opt ')' ARROW type_spec compound_stmt
        {
            auto name    = std::unique_ptr<std::string>($2);
            auto retType = std::unique_ptr<std::string>($7);
            auto* node = new func_decl_node(
                std::move(*name), std::move(*retType), static_cast<stmt_node*>($8));
            auto params = std::unique_ptr<std::vector<param_node*>>(
                static_cast<std::vector<param_node*>*>($4));
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    ;

type_decl
    : TYPE IDENTIFIER '=' struct_type
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = new type_decl_node(std::move(*name),
                                    static_cast<type_body_node*>($4));
        }
    | TYPE IDENTIFIER '=' interface_type
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = new type_decl_node(std::move(*name),
                                    static_cast<type_body_node*>($4));
        }
    ;

struct_type
    : STRUCT '{' field_list_opt '}'
        {
            auto* node = new struct_type_node("", "");
            auto list = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($3));
            if (list) {
                for (auto* n : *list)
                    node->add_field(static_cast<struct_field_node*>(n));
            }
            $$ = node;
        }
    | STRUCT ':' IDENTIFIER '{' field_list_opt '}'
        {
            auto parent = std::unique_ptr<std::string>($3);
            auto* node = new struct_type_node("", std::move(*parent));
            auto list = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($5));
            if (list) {
                for (auto* n : *list)
                    node->add_field(static_cast<struct_field_node*>(n));
            }
            $$ = node;
        }
    ;

interface_type
    : INTERFACE '{' interface_method_list_opt '}'
        {
            auto* node = new interface_type_node();
            auto list = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($3));
            if (list) {
                for (auto* n : *list)
                    node->add_method(static_cast<interface_method_node*>(n));
            }
            $$ = node;
        }
    ;

interface_method_list_opt
    : interface_method_list
    | /* empty */ { $$ = new std::vector<ast_node*>(); }
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
    : method_name '(' param_list_opt ')' ';'
        {
            auto mname = std::unique_ptr<std::string>($1);
            auto* node = new interface_method_node(std::move(*mname), std::string{});
            auto params = std::unique_ptr<std::vector<param_node*>>(
                static_cast<std::vector<param_node*>*>($3));
            if (params) {
                for (auto* p : *params) node->params.emplace_back(p);
            }
            $$ = node;
        }
    | method_name '(' param_list_opt ')' ARROW type_spec ';'
        {
            auto mname   = std::unique_ptr<std::string>($1);
            auto retType = std::unique_ptr<std::string>($6);
            auto* node = new interface_method_node(std::move(*mname), std::move(*retType));
            auto params = std::unique_ptr<std::vector<param_node*>>(
                static_cast<std::vector<param_node*>*>($3));
            if (params) {
                for (auto* p : *params) node->params.emplace_back(p);
            }
            $$ = node;
        }
    ;

method_name
    : IDENTIFIER { $$ = $1; }
    | STRING_TYPE { $$ = new std::string("string"); }
    ;

field_list_opt
    : field_list
    | /* empty */ { $$ = new std::vector<ast_node*>(); }
    ;

field_list
    : IDENTIFIER ':' type_spec ';'
        {
            auto fieldName = std::unique_ptr<std::string>($1);
            auto fieldType = std::unique_ptr<std::string>($3);
            $$ = new std::vector<ast_node*>();
            $$->push_back(new struct_field_node(std::move(*fieldName),
                                                std::move(*fieldType)));
        }
    | field_list IDENTIFIER ':' type_spec ';'
        {
            auto fieldName = std::unique_ptr<std::string>($2);
            auto fieldType = std::unique_ptr<std::string>($4);
            $$ = $1;
            $$->push_back(new struct_field_node(std::move(*fieldName),
                                                std::move(*fieldType)));
        }
    ;

method_decl
    : FUN IDENTIFIER COLONCOLON IDENTIFIER '(' param_list_opt ')' compound_stmt
        {
            auto typeName   = std::unique_ptr<std::string>($2);
            auto methodName = std::unique_ptr<std::string>($4);
            auto* node = new method_decl_node(
                std::move(*typeName), std::move(*methodName),
                std::string{}, static_cast<stmt_node*>($8));
            auto params = std::unique_ptr<std::vector<param_node*>>(
                static_cast<std::vector<param_node*>*>($6));
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    | FUN IDENTIFIER COLONCOLON IDENTIFIER '(' param_list_opt ')' ARROW type_spec compound_stmt
        {
            auto typeName   = std::unique_ptr<std::string>($2);
            auto methodName = std::unique_ptr<std::string>($4);
            auto retType    = std::unique_ptr<std::string>($9);
            auto* node = new method_decl_node(
                std::move(*typeName), std::move(*methodName),
                std::move(*retType), static_cast<stmt_node*>($10));
            auto params = std::unique_ptr<std::vector<param_node*>>(
                static_cast<std::vector<param_node*>*>($6));
            for (auto* p : *params) node->params.emplace_back(p);
            $$ = node;
        }
    ;

param_list_opt
    : param_list
    | /* empty */ { $$ = new std::vector<param_node*>(); }
    ;

param_list
    : IDENTIFIER
        {
            auto name = std::unique_ptr<std::string>($1);
            $$ = new std::vector<param_node*>();
            $$->push_back(new param_node(std::move(*name), "", false));
        }
    | IDENTIFIER ':' REF
        {
            auto name = std::unique_ptr<std::string>($1);
            $$ = new std::vector<param_node*>();
            $$->push_back(new param_node(std::move(*name), "", true));
        }
    | IDENTIFIER ':' type_spec
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = new std::vector<param_node*>();
            $$->push_back(new param_node(std::move(*name), std::move(*type), false));
        }
    | IDENTIFIER ':' type_spec REF
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = new std::vector<param_node*>();
            $$->push_back(new param_node(std::move(*name), std::move(*type), true));
        }
    | IDENTIFIER ':' REF type_spec
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($4);
            $$ = new std::vector<param_node*>();
            $$->push_back(new param_node(std::move(*name), std::move(*type), true));
        }
    | param_list ',' IDENTIFIER
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = $1;
            $$->push_back(new param_node(std::move(*name), "", false));
        }
    | param_list ',' IDENTIFIER ':' REF
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = $1;
            $$->push_back(new param_node(std::move(*name), "", true));
        }
    | param_list ',' IDENTIFIER ':' type_spec
        {
            auto name = std::unique_ptr<std::string>($3);
            auto type = std::unique_ptr<std::string>($5);
            $$ = $1;
            $$->push_back(new param_node(std::move(*name), std::move(*type), false));
        }
    | param_list ',' IDENTIFIER ':' type_spec REF
        {
            auto name = std::unique_ptr<std::string>($3);
            auto type = std::unique_ptr<std::string>($5);
            $$ = $1;
            $$->push_back(new param_node(std::move(*name), std::move(*type), true));
        }
    | param_list ',' IDENTIFIER ':' REF type_spec
        {
            auto name = std::unique_ptr<std::string>($3);
            auto type = std::unique_ptr<std::string>($6);
            $$ = $1;
            $$->push_back(new param_node(std::move(*name), std::move(*type), true));
        }
    ;

type_spec
    : IDENTIFIER { $$ = $1; }
    | STRING_TYPE { $$ = new std::string("string"); }
    | IDENTIFIER '<' IDENTIFIER '>'
        {
            auto outer = std::unique_ptr<std::string>($1);
            auto inner = std::unique_ptr<std::string>($3);
            $$ = new std::string(std::format("{}<{}>", *outer, *inner));
        }
    | IDENTIFIER '<' STRING_TYPE '>'
        {
            auto outer = std::unique_ptr<std::string>($1);
            $$ = new std::string(std::format("{}<string>", *outer));
        }
    ;

var_decl
    : VAR IDENTIFIER '=' expr ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = new var_decl_node(std::move(*name), "",
                                   static_cast<expr_node*>($4));
        }
    | VAR IDENTIFIER ':' type_spec '=' expr ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            $$ = new var_decl_node(std::move(*name), std::move(*type),
                                   static_cast<expr_node*>($6));
        }
    | VAR IDENTIFIER ':' type_spec '=' '{' arg_list_opt '}' ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            auto* node = new init_list_expr_node();
            auto args = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($7));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            }
            $$ = new var_decl_node(std::move(*name), std::move(*type), node);
        }
    | VAR IDENTIFIER ':' type_spec ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            $$ = new var_decl_node(std::move(*name), std::move(*type), nullptr);
        }
    ;

for_stmt
    : FOR var_decl expr ';' expr compound_stmt
        {
            $$ = new for_stmt_node(
                static_cast<stmt_node*>($2),
                static_cast<expr_node*>($3),
                static_cast<expr_node*>($5),
                static_cast<stmt_node*>($6));
        }
    | FOR expr ';' expr ';' expr compound_stmt
        {
            $$ = new for_stmt_node(
                new expr_stmt_node(static_cast<expr_node*>($2)),
                static_cast<expr_node*>($4),
                static_cast<expr_node*>($6),
                static_cast<stmt_node*>($7));
        }
    ;

return_stmt
    : RETURN expr ';'
        { $$ = new return_stmt_node(static_cast<expr_node*>($2)); }
    | RETURN tuple_expr ';'
        { $$ = new return_stmt_node(static_cast<expr_node*>($2)); }
    | RETURN ';'
        { $$ = new return_stmt_node(nullptr); }
    ;

if_stmt
    : IF expr compound_stmt
        {
            $$ = new if_stmt_node(static_cast<expr_node*>($2),
                                  static_cast<stmt_node*>($3), nullptr);
        }
    | IF expr compound_stmt ELSE compound_stmt
        {
            $$ = new if_stmt_node(static_cast<expr_node*>($2),
                                  static_cast<stmt_node*>($3),
                                  static_cast<stmt_node*>($5));
        }
    ;

continue_stmt
    : CONTINUE ';'
        { $$ = new continue_stmt_node(); }
    ;

break_stmt
    : BREAK ';'
        { $$ = new break_stmt_node(); }
    ;

for_range_stmt
    : FOR VAR IDENTIFIER '=' expr compound_stmt
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = new for_range_stmt_node(std::move(*name),
                                         static_cast<expr_node*>($5),
                                         static_cast<stmt_node*>($6));
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
            $$ = new tuple_var_decl_node(std::move(names),
                                         static_cast<expr_node*>($4));
        }
    ;

// Accepts both a tuple expression and a plain expression on the RHS of a
// tuple assignment, mirroring the same duality in return_stmt.
tuple_rhs
    : expr       { $$ = $1; }
    | tuple_expr { $$ = $1; }
    ;

tuple_assign_stmt
    : lvalue_list '=' tuple_rhs ';'
        {
            auto lhs_raw = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($1));
            std::vector<std::unique_ptr<expr_node>> lhs;
            for (auto* n : *lhs_raw) lhs.emplace_back(static_cast<expr_node*>(n));
            $$ = new tuple_assign_stmt_node(std::move(lhs),
                                            static_cast<expr_node*>($3));
        }
    ;

tuple_expr
    : expr ',' expr
        {
            auto* node = new tuple_expr_node();
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
            auto* node = new compound_stmt_node();
            auto list = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($2));
            if (list) {
                for (auto* n : *list)
                    node->add(static_cast<stmt_node*>(n));
            }
            $$ = node;
        }
    ;

expr
    : NUMBER
        { $$ = new number_node($1); }
    | STRING_LITERAL
        {
            auto s = std::unique_ptr<std::string>($1);
            $$ = new string_node(std::move(*s));
        }
    | IDENTIFIER
        {
            auto id = std::unique_ptr<std::string>($1);
            $$ = new identifier_node(std::move(*id));
        }
    | expr '+' expr
        { $$ = new binary_op_node("+", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '-' expr
        { $$ = new binary_op_node("-", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '*' expr
        { $$ = new binary_op_node("*", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '/' expr
        { $$ = new binary_op_node("/", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr LE expr
        { $$ = new binary_op_node("<=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr GE expr
        { $$ = new binary_op_node(">=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '<' expr
        { $$ = new binary_op_node("<", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '>' expr
        { $$ = new binary_op_node(">", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr EQ expr
        { $$ = new binary_op_node("==", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr NE expr
        { $$ = new binary_op_node("!=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr AND expr
        { $$ = new binary_op_node("&&", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr OR expr
        { $$ = new binary_op_node("||", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '=' expr
        { $$ = new assign_node(static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | '!' expr
        { $$ = new unary_op_node("!", static_cast<expr_node*>($2)); }
    | INC expr
        { $$ = new unary_op_node("++", static_cast<expr_node*>($2)); }
    | DEC expr
        { $$ = new unary_op_node("--", static_cast<expr_node*>($2)); }
    | expr INC
        { $$ = new unary_op_node("post++", static_cast<expr_node*>($1)); }
    | expr DEC
        { $$ = new unary_op_node("post--", static_cast<expr_node*>($1)); }
    | '(' expr ')'
        { $$ = $2; }
    | expr '%' expr
        { $$ = new binary_op_node("%", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr '[' expr ']'
        { $$ = new index_node(static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr PLUS_EQ expr
        { $$ = new compound_assign_node("+=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr MINUS_EQ expr
        { $$ = new compound_assign_node("-=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr STAR_EQ expr
        { $$ = new compound_assign_node("*=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | expr SLASH_EQ expr
        { $$ = new compound_assign_node("/=", static_cast<expr_node*>($1), static_cast<expr_node*>($3)); }
    | IDENTIFIER '(' arg_list_opt ')'
        {
            auto name = std::unique_ptr<std::string>($1);
            auto* node = new call_node(std::move(*name));
            auto args = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($3));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            }
            $$ = node;
        }
    | expr '.' IDENTIFIER
        {
            auto fname = std::unique_ptr<std::string>($3);
            $$ = new member_access_node(
                static_cast<expr_node*>($1), std::move(*fname));
        }
    | expr '.' method_name '(' arg_list_opt ')'
        {
            auto mname = std::unique_ptr<std::string>($3);
            auto* node = new member_call_node(
                static_cast<expr_node*>($1), std::move(*mname));
            auto args = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($5));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            }
            $$ = node;
        }
    | IDENTIFIER COLONCOLON method_name '(' arg_list_opt ')'
        {
            auto qual  = std::unique_ptr<std::string>($1);
            auto mname = std::unique_ptr<std::string>($3);
            auto* node = new qualified_call_node(std::move(*qual), std::move(*mname));
            auto args = std::unique_ptr<std::vector<ast_node*>>(
                static_cast<std::vector<ast_node*>*>($5));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<expr_node*>(a));
            }
            $$ = node;
        }
    ;

arg_list_opt
    : arg_list
    | /* empty */ { $$ = new std::vector<ast_node*>(); }
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
