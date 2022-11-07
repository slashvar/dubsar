#include <cassert>
#include <iostream>
#include <peglib.h>

const auto grammar = R"(
    # Global declaration Grammar
    GLOBAL_DECL <- (FUN / CLASS)*

    # Class declaration Grammar
    CLASS       <- 'class' IDENT CLASS_BLOCK?
    CLASS_BLOCK <- '{' (DECL / FUN)* '}'

    # Function Grammar
    FUN         <- 'fun' IDENT '(' PARAM_LIST? ')' (':' IDENT)? BLOCK
    PARAM_ATTR  <- 'ref' / 'const'
    PARAM       <- PARAM_ATTR* IDENT (':' IDENT)?
    PARAM_LIST  <- PARAM (',' PARAM_LIST)*

    # Block Grammar
    BLOCK       <- '{' STATEMENT* '}'

    # Statement Grammar
    STATEMENT   <- DECL / IF / LOOP / EXPR_STM / RETURN
    DECL        <- 'var' IDENT ( ':' IDENT)? ('=' EXPRESSION)? ';'
    IF          <- 'if' EXPRESSION BLOCK ELSE?
    ELSE        <- 'else' BLOCK
    LOOP        <- 'for' (((DECL / (EXPRESSION ';')) EXPRESSION ';' EXPRESSION) / EXPRESSION) BLOCK
    EXPR_STM    <- EXPRESSION ';'
    RETURN      <- 'return' EXPRESSION? ';'

	# Expression Grammar
	EXPRESSION  <- LEFT_OP (OPERATOR LEFT_OP)* {
                         precedence
                           L = += -= *= /=
                           L < > == <= >=
                           L - + or
                           L / * and
                       }
    LEFT_OP     <- L_OPERATOR? CALL
    L_OPERATOR  <- '*' / '++' / '--' / '+' / '-' / 'not'
	CALL		<- ARRAY CALL_OP?
	CALL_OP		<- '(' EXPRLIST? ')'
	ARRAY		<- ATOM ARRAY_OP?
	ARRAY_OP	<- '[' EXPRLIST ']'
    ATOM        <- IDENT / NUMBER / '(' EXPRESSION ')'
	EXPRLIST	<- EXPRESSION (',' EXPRESSION)*
    OPERATOR    <- 'and' / 'or' / '<' / '>' / '==' / '<=' / '>=' / '=' / '+=' / '-=' / '*=' / '/=' / '+' / '-' / '*' / '/'
    NUMBER      <- < '-'? [0-9]+ >
	IDENT		<- < [a-zA-Z][a-zA-Z0-9]* >
    %whitespace <- [ \t\r\n]*
)";

void pp(const peg::Ast& ast)
{
    if (ast.name == "ARRAY") {
        pp(*ast.nodes[0]);
        std::cout << "[";
        pp(*ast.nodes[1]);
        std::cout << "]";
        return;
    }
    if (ast.name == "CALL") {
        pp(*ast.nodes[0]);
        std::cout << "(";
        pp(*ast.nodes[1]);
        std::cout << ")";
        return;
    }
    if (ast.name == "EXPRLIST") {
        if (ast.nodes.empty()) {
            return;
        }
        auto sep = "";
        for (auto&& node : ast.nodes) {
            std::cout << sep;
            sep = ", ";
            pp(*node);
        }
        return;
    }
    std::cout << ast.token;
    const auto& nodes = ast.nodes;
    if (nodes.size() > 0) {
        std::cout << "(";
        for (auto&& node : nodes) {
            pp(*node);
        }
        std::cout << ")";
    }
}

auto main(int, char** argv) -> int
{
    peg::parser parser(grammar);
    parser.enable_ast();

    auto                      expr = argv[1];
    std::shared_ptr<peg::Ast> ast;
    if (parser.parse(expr, ast)) {
        ast = parser.optimize_ast(ast);
        std::cout << ast_to_s(ast);
        std::cout << "\n";
    }
}