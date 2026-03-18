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
    ASTNode* node;
    std::vector<ASTNode*>* nodevec;
    std::vector<ParamNode*>* paramvec;
    std::vector<std::string*>* namevec;
}

%token <stringval> IDENTIFIER
%token <intval> NUMBER
%token FUN VAR REF RETURN FOR IF ELSE CONTINUE
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
%type <node> if_stmt tuple_var_decl tuple_assign_stmt for_range_stmt continue_stmt
%type <node> tuple_expr
%type <paramvec> param_list_opt param_list
%type <stringval> type_spec method_name
%type <nodevec> stmt_list_opt stmt_list field_list_opt field_list
%type <nodevec> interface_method_list interface_method_list_opt
%type <nodevec> arg_list_opt arg_list
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
            root = std::make_unique<ProgramNode>();
            auto list = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($1));
            if (list) {
                for (auto* n : *list) {
                    root->add(static_cast<DeclNode*>(n));
                }
            }
            // Ownership is held by the global 'root'; set $$ to nullptr so
            // the <node> %destructor safely calls delete nullptr on cleanup.
            $$ = nullptr;
        }
    ;

stmt_list_opt
    : stmt_list
    | /* empty */ { $$ = new std::vector<ASTNode*>(); }
    ;

stmt_list
    : stmt
        {
            $$ = new std::vector<ASTNode*>();
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
    | tuple_assign_stmt
    | expr ';'
        {
            // Wrap in ExprStmtNode to give the expression a proper StmtNode
            // type, avoiding an undefined ExprNode* -> StmtNode* cast.
            $$ = new ExprStmtNode(static_cast<ExprNode*>($1));
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
            auto* node = new FuncDeclNode(
                std::move(*name), std::string{}, static_cast<StmtNode*>($6));
            auto params = std::unique_ptr<std::vector<ParamNode*>>(
                static_cast<std::vector<ParamNode*>*>($4));
            if (params) {
                for (auto* p : *params) node->params.emplace_back(p);
            }
            $$ = node;
        }
    | FUN IDENTIFIER '(' param_list_opt ')' ARROW type_spec compound_stmt
        {
            auto name    = std::unique_ptr<std::string>($2);
            auto retType = std::unique_ptr<std::string>($7);
            auto* node = new FuncDeclNode(
                std::move(*name), std::move(*retType), static_cast<StmtNode*>($8));
            auto params = std::unique_ptr<std::vector<ParamNode*>>(
                static_cast<std::vector<ParamNode*>*>($4));
            if (params) {
                for (auto* p : *params) node->params.emplace_back(p);
            }
            $$ = node;
        }
    ;

type_decl
    : TYPE IDENTIFIER '=' struct_type
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = new TypeDeclNode(std::move(*name),
                                  static_cast<TypeBodyNode*>($4));
        }
    | TYPE IDENTIFIER '=' interface_type
        {
            auto name = std::unique_ptr<std::string>($2);
            $$ = new TypeDeclNode(std::move(*name),
                                  static_cast<TypeBodyNode*>($4));
        }
    ;

struct_type
    : STRUCT '{' field_list_opt '}'
        {
            auto* node = new StructTypeNode("", "");
            auto list = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($3));
            if (list) {
                for (auto* n : *list)
                    node->addField(static_cast<StructFieldNode*>(n));
            }
            $$ = node;
        }
    | STRUCT ':' IDENTIFIER '{' field_list_opt '}'
        {
            auto parent = std::unique_ptr<std::string>($3);
            auto* node = new StructTypeNode("", std::move(*parent));
            auto list = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($5));
            if (list) {
                for (auto* n : *list)
                    node->addField(static_cast<StructFieldNode*>(n));
            }
            $$ = node;
        }
    ;

interface_type
    : INTERFACE '{' interface_method_list_opt '}'
        {
            auto* node = new InterfaceTypeNode();
            auto list = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($3));
            if (list) {
                for (auto* n : *list)
                    node->addMethod(static_cast<InterfaceMethodNode*>(n));
            }
            $$ = node;
        }
    ;

interface_method_list_opt
    : interface_method_list
    | /* empty */ { $$ = new std::vector<ASTNode*>(); }
    ;

interface_method_list
    : interface_method
        {
            $$ = new std::vector<ASTNode*>();
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
            auto* node = new InterfaceMethodNode(std::move(*mname), std::string{});
            auto params = std::unique_ptr<std::vector<ParamNode*>>(
                static_cast<std::vector<ParamNode*>*>($3));
            if (params) {
                for (auto* p : *params) node->params.emplace_back(p);
            }
            $$ = node;
        }
    | method_name '(' param_list_opt ')' ARROW type_spec ';'
        {
            auto mname   = std::unique_ptr<std::string>($1);
            auto retType = std::unique_ptr<std::string>($6);
            auto* node = new InterfaceMethodNode(std::move(*mname), std::move(*retType));
            auto params = std::unique_ptr<std::vector<ParamNode*>>(
                static_cast<std::vector<ParamNode*>*>($3));
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
    | /* empty */ { $$ = new std::vector<ASTNode*>(); }
    ;

field_list
    : IDENTIFIER ':' type_spec ';'
        {
            auto fieldName = std::unique_ptr<std::string>($1);
            auto fieldType = std::unique_ptr<std::string>($3);
            $$ = new std::vector<ASTNode*>();
            $$->push_back(new StructFieldNode(std::move(*fieldName),
                                              std::move(*fieldType)));
        }
    | field_list IDENTIFIER ':' type_spec ';'
        {
            auto fieldName = std::unique_ptr<std::string>($2);
            auto fieldType = std::unique_ptr<std::string>($4);
            $$ = $1;
            $$->push_back(new StructFieldNode(std::move(*fieldName),
                                              std::move(*fieldType)));
        }
    ;

method_decl
    : FUN IDENTIFIER COLONCOLON IDENTIFIER '(' param_list_opt ')' compound_stmt
        {
            auto typeName   = std::unique_ptr<std::string>($2);
            auto methodName = std::unique_ptr<std::string>($4);
            // param_list_opt is not yet represented in MethodDeclNode; free to avoid leaks.
            auto params = std::unique_ptr<std::vector<ParamNode*>>(
                static_cast<std::vector<ParamNode*>*>($6));
            if (params) {
                for (auto* p : *params) delete p;
            }
            $$ = new MethodDeclNode(
                std::move(*typeName), std::move(*methodName),
                std::string{}, static_cast<StmtNode*>($8));
        }
    | FUN IDENTIFIER COLONCOLON IDENTIFIER '(' param_list_opt ')' ARROW type_spec compound_stmt
        {
            auto typeName   = std::unique_ptr<std::string>($2);
            auto methodName = std::unique_ptr<std::string>($4);
            auto retType    = std::unique_ptr<std::string>($9);
            // param_list_opt is not yet represented in MethodDeclNode; free to avoid leaks.
            auto params = std::unique_ptr<std::vector<ParamNode*>>(
                static_cast<std::vector<ParamNode*>*>($6));
            if (params) {
                for (auto* p : *params) delete p;
            }
            $$ = new MethodDeclNode(
                std::move(*typeName), std::move(*methodName),
                std::move(*retType), static_cast<StmtNode*>($10));
        }
    ;

param_list_opt
    : param_list
    | /* empty */ { $$ = new std::vector<ParamNode*>(); }
    ;

param_list
    : IDENTIFIER
        {
            auto name = std::unique_ptr<std::string>($1);
            $$ = new std::vector<ParamNode*>();
            $$->push_back(new ParamNode(std::move(*name), "", false));
        }
    | IDENTIFIER ':' REF
        {
            auto name = std::unique_ptr<std::string>($1);
            $$ = new std::vector<ParamNode*>();
            $$->push_back(new ParamNode(std::move(*name), "", true));
        }
    | IDENTIFIER ':' type_spec
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = new std::vector<ParamNode*>();
            $$->push_back(new ParamNode(std::move(*name), std::move(*type), false));
        }
    | IDENTIFIER ':' type_spec REF
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($3);
            $$ = new std::vector<ParamNode*>();
            $$->push_back(new ParamNode(std::move(*name), std::move(*type), true));
        }
    | IDENTIFIER ':' REF type_spec
        {
            auto name = std::unique_ptr<std::string>($1);
            auto type = std::unique_ptr<std::string>($4);
            $$ = new std::vector<ParamNode*>();
            $$->push_back(new ParamNode(std::move(*name), std::move(*type), true));
        }
    | param_list ',' IDENTIFIER
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = $1;
            $$->push_back(new ParamNode(std::move(*name), "", false));
        }
    | param_list ',' IDENTIFIER ':' REF
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = $1;
            $$->push_back(new ParamNode(std::move(*name), "", true));
        }
    | param_list ',' IDENTIFIER ':' type_spec
        {
            auto name = std::unique_ptr<std::string>($3);
            auto type = std::unique_ptr<std::string>($5);
            $$ = $1;
            $$->push_back(new ParamNode(std::move(*name), std::move(*type), false));
        }
    | param_list ',' IDENTIFIER ':' type_spec REF
        {
            auto name = std::unique_ptr<std::string>($3);
            auto type = std::unique_ptr<std::string>($5);
            $$ = $1;
            $$->push_back(new ParamNode(std::move(*name), std::move(*type), true));
        }
    | param_list ',' IDENTIFIER ':' REF type_spec
        {
            auto name = std::unique_ptr<std::string>($3);
            auto type = std::unique_ptr<std::string>($6);
            $$ = $1;
            $$->push_back(new ParamNode(std::move(*name), std::move(*type), true));
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
            $$ = new VarDeclNode(std::move(*name), "",
                                 static_cast<ExprNode*>($4));
        }
    | VAR IDENTIFIER ':' type_spec '=' expr ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            $$ = new VarDeclNode(std::move(*name), std::move(*type),
                                 static_cast<ExprNode*>($6));
        }
    | VAR IDENTIFIER ':' type_spec '=' '{' arg_list_opt '}' ';'
        {
            auto name = std::unique_ptr<std::string>($2);
            auto type = std::unique_ptr<std::string>($4);
            auto* node = new InitListExprNode();
            auto args = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($7));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<ExprNode*>(a));
            }
            $$ = new VarDeclNode(std::move(*name), std::move(*type), node);
        }
    ;

for_stmt
    : FOR var_decl expr ';' expr compound_stmt
        {
            $$ = new ForStmtNode(
                static_cast<StmtNode*>($2),
                static_cast<ExprNode*>($3),
                static_cast<ExprNode*>($5),
                static_cast<StmtNode*>($6));
        }
    | FOR expr ';' expr ';' expr compound_stmt
        {
            $$ = new ForStmtNode(
                new ExprStmtNode(static_cast<ExprNode*>($2)),
                static_cast<ExprNode*>($4),
                static_cast<ExprNode*>($6),
                static_cast<StmtNode*>($7));
        }
    ;

return_stmt
    : RETURN expr ';'
        { $$ = new ReturnStmtNode(static_cast<ExprNode*>($2)); }
    | RETURN tuple_expr ';'
        { $$ = new ReturnStmtNode(static_cast<ExprNode*>($2)); }
    ;

if_stmt
    : IF expr compound_stmt
        {
            $$ = new IfStmtNode(static_cast<ExprNode*>($2),
                                static_cast<StmtNode*>($3), nullptr);
        }
    | IF expr compound_stmt ELSE compound_stmt
        {
            $$ = new IfStmtNode(static_cast<ExprNode*>($2),
                                static_cast<StmtNode*>($3),
                                static_cast<StmtNode*>($5));
        }
    ;

continue_stmt
    : CONTINUE ';'
        { $$ = new ContinueStmtNode(); }
    ;

for_range_stmt
    : FOR VAR IDENTIFIER '=' expr compound_stmt
        {
            auto name = std::unique_ptr<std::string>($3);
            $$ = new ForRangeStmtNode(std::move(*name),
                                      static_cast<ExprNode*>($5),
                                      static_cast<StmtNode*>($6));
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

tuple_var_decl
    : VAR name_list '=' expr ';'
        {
            auto names_raw = std::unique_ptr<std::vector<std::string*>>($2);
            std::vector<std::string> names;
            for (auto* s : *names_raw) { names.push_back(std::move(*s)); delete s; }
            $$ = new TupleVarDeclNode(std::move(names),
                                      static_cast<ExprNode*>($4));
        }
    ;

tuple_assign_stmt
    : name_list '=' tuple_expr ';'
        {
            auto names_raw = std::unique_ptr<std::vector<std::string*>>($1);
            std::vector<std::string> names;
            for (auto* s : *names_raw) { names.push_back(std::move(*s)); delete s; }
            $$ = new TupleAssignStmtNode(std::move(names),
                                         static_cast<ExprNode*>($3));
        }
    | name_list '=' expr ';'
        {
            auto names_raw = std::unique_ptr<std::vector<std::string*>>($1);
            std::vector<std::string> names;
            for (auto* s : *names_raw) { names.push_back(std::move(*s)); delete s; }
            $$ = new TupleAssignStmtNode(std::move(names),
                                         static_cast<ExprNode*>($3));
        }
    ;

tuple_expr
    : expr ',' expr
        {
            auto* node = new TupleExprNode();
            node->add(static_cast<ExprNode*>($1));
            node->add(static_cast<ExprNode*>($3));
            $$ = node;
        }
    | tuple_expr ',' expr
        {
            auto* node = static_cast<TupleExprNode*>($1);
            node->add(static_cast<ExprNode*>($3));
            $$ = node;
        }
    ;

compound_stmt
    : '{' stmt_list_opt '}'
        {
            auto* node = new CompoundStmtNode();
            auto list = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($2));
            if (list) {
                for (auto* n : *list)
                    node->add(static_cast<StmtNode*>(n));
            }
            $$ = node;
        }
    ;

expr
    : NUMBER
        { $$ = new NumberNode($1); }
    | STRING_LITERAL
        {
            auto s = std::unique_ptr<std::string>($1);
            $$ = new StringNode(std::move(*s));
        }
    | IDENTIFIER
        {
            auto id = std::unique_ptr<std::string>($1);
            $$ = new IdentifierNode(std::move(*id));
        }
    | expr '+' expr
        { $$ = new BinaryOpNode("+", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '-' expr
        { $$ = new BinaryOpNode("-", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '*' expr
        { $$ = new BinaryOpNode("*", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '/' expr
        { $$ = new BinaryOpNode("/", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr LE expr
        { $$ = new BinaryOpNode("<=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr GE expr
        { $$ = new BinaryOpNode(">=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '<' expr
        { $$ = new BinaryOpNode("<", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '>' expr
        { $$ = new BinaryOpNode(">", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr EQ expr
        { $$ = new BinaryOpNode("==", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr NE expr
        { $$ = new BinaryOpNode("!=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr AND expr
        { $$ = new BinaryOpNode("&&", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr OR expr
        { $$ = new BinaryOpNode("||", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '=' expr
        { $$ = new AssignNode(static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | '!' expr
        { $$ = new UnaryOpNode("!", static_cast<ExprNode*>($2)); }
    | INC expr
        { $$ = new UnaryOpNode("++", static_cast<ExprNode*>($2)); }
    | DEC expr
        { $$ = new UnaryOpNode("--", static_cast<ExprNode*>($2)); }
    | expr INC
        { $$ = new UnaryOpNode("post++", static_cast<ExprNode*>($1)); }
    | expr DEC
        { $$ = new UnaryOpNode("post--", static_cast<ExprNode*>($1)); }
    | '(' expr ')'
        { $$ = $2; }
    | expr '%' expr
        { $$ = new BinaryOpNode("%", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr '[' expr ']'
        { $$ = new IndexNode(static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr PLUS_EQ expr
        { $$ = new CompoundAssignNode("+=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr MINUS_EQ expr
        { $$ = new CompoundAssignNode("-=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr STAR_EQ expr
        { $$ = new CompoundAssignNode("*=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | expr SLASH_EQ expr
        { $$ = new CompoundAssignNode("/=", static_cast<ExprNode*>($1), static_cast<ExprNode*>($3)); }
    | IDENTIFIER '(' arg_list_opt ')'
        {
            auto name = std::unique_ptr<std::string>($1);
            auto* node = new CallNode(std::move(*name));
            auto args = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($3));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<ExprNode*>(a));
            }
            $$ = node;
        }
    | expr '.' method_name '(' arg_list_opt ')'
        {
            auto mname = std::unique_ptr<std::string>($3);
            auto* node = new MemberCallNode(
                static_cast<ExprNode*>($1), std::move(*mname));
            auto args = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($5));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<ExprNode*>(a));
            }
            $$ = node;
        }
    | IDENTIFIER COLONCOLON method_name '(' arg_list_opt ')'
        {
            auto qual  = std::unique_ptr<std::string>($1);
            auto mname = std::unique_ptr<std::string>($3);
            auto* node = new QualifiedCallNode(std::move(*qual), std::move(*mname));
            auto args = std::unique_ptr<std::vector<ASTNode*>>(
                static_cast<std::vector<ASTNode*>*>($5));
            if (args) {
                for (auto* a : *args) node->args.emplace_back(static_cast<ExprNode*>(a));
            }
            $$ = node;
        }
    ;

arg_list_opt
    : arg_list
    | /* empty */ { $$ = new std::vector<ASTNode*>(); }
    ;

arg_list
    : expr
        {
            $$ = new std::vector<ASTNode*>();
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
