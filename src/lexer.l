%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Include AST types for the lexer
#include "parser_types.h"
#include "ast.h"

// Define YYSTYPE union for the lexer - must match parser.y
typedef union YYSTYPE {
    int intval;
    std::string* stringval;
    ASTNode* node;
    std::vector<ASTNode*>* nodevec;
    std::vector<ParamNode*>* paramvec;
    std::vector<std::string*>* namevec;
} YYSTYPE;

extern YYSTYPE yylval;
%}

%option noyywrap
%option yylineno

%%

[ \t\r\n]+            { /* skip whitespace */ }
"//".*               { /* skip single-line comments */ }
"/*"([^*]|\*[^/])*"*/"  { /* skip multi-line comments */ }

"fun"                { return FUN; }
"var"                { return VAR; }
"ref"                { return REF; }
"return"             { return RETURN; }
"for"                { return FOR; }
"type"               { return TYPE; }
"struct"             { return STRUCT; }
"string"             { return STRING_TYPE; }
"interface"          { return INTERFACE; }
"if"                 { return IF; }
"else"               { return ELSE; }
"continue"           { return CONTINUE; }

"<="                 { return LE; }
">="                 { return GE; }
"=="                 { return EQ; }
"!="                 { return NE; }
"&&"                 { return AND; }
"||"                 { return OR; }
"++"                 { return INC; }
"--"                 { return DEC; }
"::"                 { return COLONCOLON; }
"+="                 { return PLUS_EQ; }
"-="                 { return MINUS_EQ; }
"->"                 { return ARROW; }
"*="                 { return STAR_EQ; }
"/="                 { return SLASH_EQ; }

[a-zA-Z_][a-zA-Z0-9_]*  {
    yylval.stringval = new std::string(yytext);
    return IDENTIFIER;
}

[0-9]+               {
    yylval.intval = atoi(yytext);
    return NUMBER;
}

\"([^\\"]|\\.)*\"     {
    yylval.stringval = new std::string(yytext + 1, strlen(yytext) - 2);
    return STRING_LITERAL;
}

.                    { return yytext[0]; }

%%