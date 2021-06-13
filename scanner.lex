%{
/* Declarations section */
#include <stdio.h>
#include "Semantics.h"
#include "parser.tab.hpp"
#include "hw3_output.hpp"
%}

%option yylineno
%option noyywrap
%option nounput
whitespace  ([\r\n\t ])

%%

void                                                                yylval=new TypeNode(yytext); return VOID;
int                                                                 yylval=new TypeNode(yytext); return INT;
byte                                                                yylval=new TypeNode(yytext); return BYTE;
b                                                                   yylval=new TypeNode(yytext); return B;
bool                                                                yylval=new TypeNode(yytext); return BOOL;
and                                                                 yylval=new TypeNode(yytext); return AND;
or                                                                  yylval=new TypeNode(yytext); return OR;
not                                                                 yylval=new TypeNode(yytext); return NOT;
true                                                                yylval=new TypeNode(yytext); return TRUE;
false                                                               yylval=new TypeNode(yytext); return FALSE;
return                                                              yylval=new TypeNode(yytext); return RETURN;
if                                                                  yylval=new TypeNode(yytext); return IF;
else                                                                yylval=new TypeNode(yytext); return ELSE;
while                                                               yylval=new TypeNode(yytext); return WHILE;
break                                                               yylval=new TypeNode(yytext); return BREAK;
continue                                                            yylval=new TypeNode(yytext); return CONTINUE;
switch                                                              yylval=new TypeNode(yytext); return SWITCH;
case                                                                yylval=new TypeNode(yytext); return CASE;
default                                                             yylval=new TypeNode(yytext); return DEFAULT;
(\:)                                                                yylval=new TypeNode(yytext); return COLON;
(\;)                                                                yylval=new TypeNode(yytext); return SC;
(\,)                                                                yylval=new TypeNode(yytext); return COMMA;
(\()                                                                yylval=new TypeNode(yytext); return LPAREN;
(\))                                                                yylval=new TypeNode(yytext); return RPAREN;
(\{)                                                                yylval=new TypeNode(yytext); return LBRACE;
(\})                                                                yylval=new TypeNode(yytext); return RBRACE;
(=)                                                                 yylval=new TypeNode(yytext); return ASSIGN;
(==|!=)                                                             yylval=new TypeNode(yytext); return EQ_NEQ_RELOP;
(<|>|<=|>=)                                                         yylval=new TypeNode(yytext); return REL_RELOP;
(\+|\-)                                                             yylval=new TypeNode(yytext); return ADD_SUB_BINOP;
(\*|\/)                                                             yylval=new TypeNode(yytext); return MUL_DIV_BINOP;
\/\/[^\r\n]*(\r|\n|\r\n)?                                            ;
[a-zA-Z][a-zA-Z0-9]*                                                yylval=new TypeNode(yytext); return ID;
0|[1-9][0-9]*                                                       yylval=new TypeNode(yytext); return NUM;
{whitespace}                                                         ;
\"([^\n\r\"\\]|\\[rnt"\\])+\"                                       yylval=new TypeNode(yytext); return STRING;
.                                                                    {output::errorLex(yylineno); exit(0);};

%%