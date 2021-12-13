%option noyywrap
%{
    #define YY_NO_UNPUT
    #define YY_NO_INPUT

    #include "parser.h"
    #include <ostream>
    #include <fstream>
    #include <math.h>
    using namespace std;

    extern FILE *yyin; 
    extern FILE *yyout;
    extern bool dump_tokens;



    void DEBUG_FOR_LAB4(std::string s){
        std::string DEBUG_INFO = "[DEBUG LAB4]: \t" + s + "\n";
        fputs(DEBUG_INFO.c_str(), yyout);
    }
%}

DECIMIAL ([1-9][0-9]*|0)
OCTNUMBER 0[0-7]+
HEXNUMBER 0[Xx][0-9a-fA-F]+
ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]


commentbegin "/*"
commentelement .|\n
commentend "*/"
%x COMMENT
commentlinebegin "//"
commentlineelement .
commentlineend \n
%x COMMENTLINE

%%

"int" {
    /*
    * Questions: 
    *   Q1: Why we need to return INT in further labs?
    *   Q2: What is "INT" actually?
    */
    if(dump_tokens)
        DEBUG_FOR_LAB4("INT\tint");
    return INT;
}
"const" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("CONST\t\tconst\t\t");
    
    return CONST;
     
}
"void" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("VOID\tvoid");
    return VOID;
}
"if" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("IF\tif");
    return IF;
};
"else" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("ELSE\telse");
    return ELSE;
};
"while" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("WHILE\t\twhile\t\t");
    
    return WHILE;
    
}
"break" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("BREAK\t\tbreak\t\t");
    
    return BREAK;
    
}
"continue" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("CONTINUE\t\tcontinue\t\t");
    
    return CONTINUE;
    
}
"for" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("FOR\t\tfor\t\t");
    
    return FOR;
    
}
"return" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RETURN\treturn");
    return RETURN;
}

"=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("ASSIGN\t=");
    return ASSIGN;
}
"<" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LESS\t<");
    return LESS;
}
">" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("GRAN\t>");
    return LESS;
}
"+" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("ADD\t+");
    return ADD;
}
"-" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("SUB\t-");
    return SUB;
}
"*" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("MULT\t\t*\t\t");
    
    return MULT;
    
}
"/" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("DIV\t\t/\t\t");
    
    return DIV;
    
}
"==" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("EQ\t\t==\t\t");
    
    return EQ;
    
}
">=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("GRANEQ\t\t>=\t\t");
    
    return GRANEQ;
    
}
"<=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LESSEQ\t\t<=\t\t");
    
    return LESSEQ;
    
}
"!=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("NEQ\t\t!=\t\t");
    
    return NEQ;
    
}
"+=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("PLUSASSIGN\t\t+=\t\t");
    
    return PLUSASSIGN;
    
}
"-=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("SUBASSIGN\t\t-=\t\t");
    
    return SUBASSIGN;
    
}
"*=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("MULASSIGN\t\t*=\t\t");
    
    return MULASSIGN;
    
}
"/=" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("DIVASSIGN\t\t/=\t\t");
    
    return DIVASSIGN;
    
}
";" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("SEMICOLON\t;");
    return SEMICOLON;
}
"(" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LPAREN\t(");
    return LPAREN;
}
")" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RPAREN\t)");
    return RPAREN;
}
"{" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LBRACE\t{");
    return LBRACE;
}
"}" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RBRACE\t}");
    return RBRACE;
}
"[" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("LBRAK\t\t[\t\t");
    
    return LBRAK;
    
}
"]" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("RBRAK\t\t[\t\t");
    
    return RBRAK;
    
}

"%" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("MOD\t\t%\t\t");
    
    return MOD;
    
}
"&&" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("AND\t\t&&\t\t");
    
    return AND;
    
}
"||" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("OR\t\t||\t\t");
    
    return OR;
    
}
"!" {
    if(dump_tokens)
        DEBUG_FOR_LAB4("NOT\t\t!\t\t");
    
    return NOT;
    
}
"," {
    if(dump_tokens)
        DEBUG_FOR_LAB4("SEP\t\t,\t\t");
    
    return SEP;
    
}
{OCTNUMBER} {
    if(dump_tokens){
         DEBUG_FOR_LAB4(yytext);
    }
    string s;
    string s1;
    s1=yytext;
    int sum=0;
    int i=0;
    int n=atoi(yytext);
    while(n!=0)
    {
        sum+=(n%10)*pow(8,i);
        i++;
        n/=10;
    }
       
    s1=to_string(sum);
    const char* s2=s1.c_str();
    yylval.itype = atoi(s2);
       
    return INTEGER;
    
}
{DECIMIAL} {
    if(dump_tokens)
        DEBUG_FOR_LAB4(yytext);
    yylval.itype = atoi(yytext);
    return INTEGER;
}

{HEXNUMBER} {
    if(dump_tokens){
        DEBUG_FOR_LAB4(yytext);
    }
    string s;
        string s1;
        s=yytext;
        int len=s.length();
        int sum=0;
        int tem=0;
        for(int i=2;i<len;i++)
        {
            if (s[i] >= '0' && s[i] <= '9')
            tem = s[i] - '0';       //当字符是0~9时保持原数不变
            if (s[i] >= 'a' && s[i] <= 'z')
            tem = s[i] - 'a' + 10;
            if (s[i] >= 'A' && s[i] <= 'Z')
            tem = s[i] - 'A' + 10;
            sum += tem * pow(16, len - i-1);
        }
        s1=to_string(sum);
        const char* s2=s1.c_str();
        yylval.itype = atoi(s2);
    
    return INTEGER;
    
}

{ID} {
    if(dump_tokens)
        DEBUG_FOR_LAB4(yytext);
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ID;
}

{commentbegin} {BEGIN COMMENT;}
<COMMENT>{commentelement} {}
<COMMENT>{commentend} {BEGIN INITIAL;}
{commentlinebegin} {BEGIN COMMENTLINE;}
<COMMENTLINE>{commentlineelement} {}
<COMMENTLINE>{commentlineend} {BEGIN INITIAL;}


{EOL} yylineno++;
{WHITE}
%%
