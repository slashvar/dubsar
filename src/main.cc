#include <cassert>
#include <iostream>
#include <peglib.h>

const auto grammar = R"(
    # Global declaration Grammar
    GLOBAL_DECL <- (_ / FUN / CLASS)*

    # Type grammar
    TYPENAME    <- FLOAT / INTEGER / IDENT
    FLOAT       <- 'float' | 'double'
    INTEGER     <- 'integer<' ('8' | '16' | '32' | '64') '>'

    # Class declaration Grammar
    CLASS       <- 'class' (TYPE_PARAMS)? IDENT INIT_PARAMS? CLASS_BLOCK?
    TYPE_PARAMS <- '<' IDENT (',' IDENT)* '>'
    INIT_PARAMS <- '(' PARAM_LIST ')'
    CLASS_BLOCK <- '{' _* DELETE? _* (_ / DECL / FUN)* '}'
    DELETE      <- 'delete' BLOCK

    # Function Grammar
    FUN         <- 'fun' IDENT '(' PARAM_LIST? ')' (':' TYPENAME)? BLOCK
    PARAM_ATTR  <- 'ref' / 'const'
    PARAM       <- PARAM_ATTR* IDENT (':' TYPENAME)?
    PARAM_LIST  <- PARAM (',' PARAM_LIST)*

    # Block Grammar
    BLOCK       <- '{' STATEMENT* '}'

    # Statement Grammar
    STATEMENT   <- _ / DECL / IF / LOOP / EXPR_STM / RETURN
    DECL        <- DECL_TYPE DECL_LIST ('=' EXPRESSION)? ';'
    DECL_ID     <- IDENT (':' TYPENAME)?
    DECL_LIST   <- DECL_ID (',' DECL_ID)*
    DECL_TYPE   <- 'var' / 'ref'
    IF          <- 'if' (DECL / (EXPRESSION ';'))? EXPRESSION BLOCK ELSE?
    ELSE        <- 'else' BLOCK
    LOOP        <- 'for' (((DECL / (EXPRESSION ';')) EXPRESSION ';' EXPRESSION) / EXPRESSION) BLOCK
    EXPR_STM    <- EXPRESSION ';'
    RETURN      <- 'return' EXPRESSION? ';'

	# Expression Grammar
    EXPRESSION  <- COMMA_EXPR (',' COMMA_EXPR)*
    COMMA_EXPR  <- VALUE_EXPR / '{' EXPRLIST? '}'
    VALUE_EXPR  <- LEFT_OP (OPERATOR LEFT_OP)* {
                         precedence
                           L = += -= *= /=
                           L < > == <= >=
                           L - + | or
                           L / * % & ^ and
                       }
    LEFT_OP     <- L_OPERATOR? RIGHT_OP
    L_OPERATOR  <- '*' / '++' / '--' / '+' / '-' / '~' / 'not'
    RIGHT_OP    <- ATOM R_OPERATOR*
    R_OPERATOR  <- ARRAY_OP / CALL_OP / FIELD_OP
    FIELD_OP    <- '.' IDENT
	CALL_OP		<- '(' EXPRLIST? ')'
	ARRAY_OP	<- '[' EXPRLIST ']'
    ATOM        <- CAST / IDENT / NUMBER / '(' EXPRESSION ')' / STR
    CAST        <- '(' EXPRESSION ':' TYPENAME ')'
	EXPRLIST	<- EXPRESSION (',' EXPRESSION)*
    OPERATOR    <- 'and' / 'or' / '<' / '>' / '==' / '<=' / '>=' / '=' / '+=' / '-=' / '*=' / '/=' / '+' / '-' / '*' / '/' / '%' / '&' / '|' / '^'
    NUMBER      <- < '-'? [0-9]+ >
	IDENT		<- < [a-zA-Z_][a-zA-Z0-9_]* >
    STR         <- '"' (!'"' .)* '"'

    ~_          <- COMMS
    %whitespace <- [ \t\r\n]*
    CBEGIN      <- '/*'
    CEND        <- '*/'
    COMMS       <- CBEGIN COMMS_INT* CEND
    COMMS_INT   <- COMMS / (!CBEGIN !CEND ANY)
    ANY         <- .
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
    parser.set_logger([](size_t line, size_t col, const std::string& msg) {
        std::clog << "Syntax error line " << line << " col " << col << ": " << msg << std::endl;
    });

    auto                      expr = argv[1];
    std::shared_ptr<peg::Ast> ast;
    if (parser.parse(expr, ast)) {
        ast = parser.optimize_ast(ast);
        std::cout << ast_to_s(ast);
        std::cout << "\n";
    }
}