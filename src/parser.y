%code top{
    #include <iostream>
    #include <assert.h>
    #include <string.h>
    #include <math.h>
    #include "parser.h"
    extern Ast ast;
    int yylex();
    int yyerror( char const * );
    bool first=true;
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"

}

%union {
    int itype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
}

%start Program
%token <strtype> ID 
%token <itype> INTEGER
%token IF ELSE
%token INT VOID
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token ADD SUB OR AND LESS ASSIGN FOR
%token RETURN CONST WHILE BREAK CONTINUE MULT DIV EQ GRAN
%token GRANEQ LESSEQ NEQ PLUSASSIGN SUBASSIGN MULASSIGN DIVASSIGN LBRAK RBRAK MOD NOT SEP 

%nterm <type> Type
%nterm <exprtype> Exp AddExp Cond LOrExp PrimaryExp LVal RelExp LAndExp
%nterm <exprtype> UnaryExp MulExp EqExp
%nterm <stmttype> Stmts Stmt AssignStmt BlockStmt IfStmt ReturnStmt DeclStmt FuncDef
%nterm <stmttype> VarDecl VarList ListUnit
%nterm <stmttype> ConstListUnit ConstList ConstDecl
%nterm <stmttype> EmptyStmt ExpStmt WhileStmt BreakStmt ContinueStmt
%nterm <stmttype> FuncFParamUnit FuncFParams 
%nterm <stmttype> FuncRParamUnit FuncRParams 

%precedence THEN
%precedence ELSE
%%

Program
    : Stmts {
        ast.setRoot($1);

    }
    ;
Stmts
    : Stmt {$$=$1;}
    | Stmts Stmt{
        //如果在当前符号表找不到可以去全局符号表找。
        $$ = new SeqNode($1, $2);
        fprintf(stderr, "created a seqnode\n");

    }
    ;
Stmt
    : AssignStmt {$$=$1;}
    | EmptyStmt {$$=$1;}
    | ExpStmt{$$=$1;}
    | BlockStmt {$$=$1;}
    | IfStmt {$$=$1;}
    | ReturnStmt {$$=$1;}
    | DeclStmt {$$=$1;}
    | FuncDef {$$=$1;}
    | ContinueStmt {$$=$1;}
    | BreakStmt {$$=$1;}
    | WhileStmt {$$=$1;}
    ;
EmptyStmt
    : SEMICOLON{
        $$ =new EmptyStmt();
    }
    ;
ExpStmt
    : Exp SEMICOLON{
        $$ =new ExpStmt($1);

    }
    ;
WhileStmt
    : WHILE LPAREN Cond RPAREN Stmt{
        $$=new WhileStmt($3,$5);
    }
    ;
BreakStmt
    : BREAK SEMICOLON{
        $$=new BreakStmt();
    }
    ;
ContinueStmt
    : CONTINUE SEMICOLON{
        $$=new ContinueStmt();
    }
    ;
AssignStmt
    :
    LVal ASSIGN Exp SEMICOLON {
        $$ = new AssignStmt($1, $3);
        fprintf(stderr, "AssignStmt\n");

    }
    ;
BlockStmt
    :   LBRACE 
        {identifiers = new SymbolTable(identifiers);
            fprintf(stderr, "blockstmt begin\n");
        } 
        Stmts RBRACE 
        {
            $$ = new CompoundStmt($3);
            SymbolTable *top = identifiers;
            identifiers = identifiers->getPrev();
            fprintf(stderr, "blockstmt end\n");
            delete top;
        }
        |LBRACE RBRACE 
        {$$ = new EmptyStmt();}
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN {
        $$ = new IfStmt($3, $5);
        fprintf(stderr, "ifstmt\n");

    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt {
        fprintf(stderr, "ifelsestmt\n");
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;
ReturnStmt
    : RETURN SEMICOLON{
        //返回值表达式 是空
        $$ = new ReturnStmt(nullptr);
    }
    | RETURN Exp SEMICOLON{
        $$ = new ReturnStmt($2);
        fprintf(stderr, "return\n");
    }
    ;

//Exp######################################################

LVal
    : ID {
        SymbolEntry *se;
        se = identifiers->lookup($1);
        if(se == nullptr)
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            //assert(se != nullptr);
        }
        $$ = new Id(se);
        delete []$1;
    }
    ;
Exp
    :
    AddExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;}
    ;
PrimaryExp
    : LPAREN Exp RPAREN {$$=$2;}
    | LVal {
        $$ = $1;
    }
    //整数
    | INTEGER {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);

    }
    ;
UnaryExp 
    : PrimaryExp {$$=$1;}
    | ADD UnaryExp{
        SymbolEntry* se= new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$=new UnaryExpr(se, UnaryExpr::ADD,$2);
    }
    | SUB UnaryExp{
        SymbolEntry* se= new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$=new UnaryExpr(se, UnaryExpr::SUB,$2);

    }
    | NOT UnaryExp{
        SymbolEntry* se= new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$=new UnaryExpr(se, UnaryExpr::NOT,$2);

    }
    | ID LPAREN RPAREN{
        SymbolEntry *se;
        se=identifiers->lookup($1);
        if(se==nullptr)
            fprintf(stderr, "nullptr!\n");
        fprintf(stderr, "found %s!\n",(char*)$1);
        $$=new CallFunc(se, $1);


    }
    | ID LPAREN FuncRParams RPAREN{
        SymbolEntry *se;
        se=identifiers->lookup($1);
        if(se==nullptr)
            fprintf(stderr, "nullptr!\n");
        fprintf(stderr, "found %s!\n",(char*)$1);
        $$=new CallFunc(se, $1,$3);


    }
    ;
MulExp
    : UnaryExp {$$ = $1;}
    | MulExp MULT UnaryExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MULT, $1, $3);
    }
    | MulExp DIV UnaryExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    | MulExp MOD UnaryExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;
AddExp
    :
    MulExp {$$ = $1;}
    |
    AddExp ADD MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        fprintf(stderr, "BinaryExpr\n");
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    |
    AddExp SUB MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;
RelExp
    : AddExp {$$ = $1;}
    | RelExp LESS AddExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    | RelExp GRAN AddExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GRAN, $1, $3);
    }
    | RelExp LESSEQ AddExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQ, $1, $3);
    }
    | RelExp GRANEQ AddExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GRANEQ, $1, $3);
    }
    ;
EqExp
    : RelExp {$$ = $1;}
    | EqExp EQ RelExp {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQ, $1, $3);
    }
    | EqExp NEQ RelExp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NEQ, $1, $3);
    }
    ;
LAndExp
    :
    EqExp {$$ = $1;}
    |
    LAndExp AND EqExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    :
    LAndExp {$$ = $1;}
    |
    LOrExp OR LAndExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;

//Exp######################################################

Type
    : INT {
        $$ = TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    ;

//DeclStmt#################################################

DeclStmt
    : VarDecl SEMICOLON{$$=$1;}
    | ConstDecl SEMICOLON{$$=$1;}
    ;

ConstDecl
    : CONST Type ConstList{$$=$3;}
    ;
ConstList
    : ConstListUnit{$$=$1;}
    | ConstList SEP ConstListUnit{$$= new SeqNode($1,$3);}
    ;
ConstListUnit
    :ID ASSIGN Exp{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        fprintf(stderr, "identifier \"%s\" is defined\n", (char*)$1);
        $$ = new ConstDeclAssignStmt(new Id(se),$3);
        delete []$1;
    }
    ;

VarDecl
    :Type VarList{$$=$2;}
    ;
VarList
    : ListUnit{$$=$1;}
    | VarList SEP ListUnit{$$= new SeqNode($1,$3);}
    ;
ListUnit
    : ID{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        fprintf(stderr, "identifier \"%s\" is defined\n", (char*)$1);
        $$ = new DeclStmt(new Id(se));
        delete []$1;

    }
    | ID ASSIGN Exp{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        fprintf(stderr, "identifier \"%s\" is defined\n", (char*)$1);
        $$ = new DeclAssignStmt(new Id(se),$3);
        delete []$1;
    }
    ;

//DeclStmt#################################################

//Func#####################################################

FuncDef
    :
    Type ID 
    LPAREN RPAREN{
        if(first){
            SymbolEntry *se;
            char* a[6]={(char*)"getint",
                        (char*)"getch",
                        (char*)"getarray",
                        (char*)"putint",
                        (char*)"putch",
                        (char*)"putarray"};
            for(int i=0;i<6;i++){
                se = new IdentifierSymbolEntry(TypeSystem::intType, a[i], identifiers->getLevel());
                identifiers->install(a[i],se);
                fprintf(stderr, "function \"%s\" is defined\n", (char*)a[i]);
            }

            first=false;
        }
        Type *funcType;
        funcType = new FunctionType($1,{});
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        fprintf(stderr, "function \"%s\" is defined\n", (char*)$2);


        identifiers = new SymbolTable(identifiers);
    }
    BlockStmt{
        fprintf(stderr, "function\n");
        SymbolEntry *se;
        se = identifiers->lookup($2);

        if(se==nullptr)
            return 1;
        //assert(se != nullptr);
        $$ = new FunctionDef(se, $6);
        //$$->output(identifiers->getLevel());
        //fprintf(stderr, "function");


        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    | Type ID
    LPAREN FuncFParams RPAREN{
        if(first){
            SymbolEntry *se;
            char* a[6]={(char*)"getint",
                        (char*)"getch",
                        (char*)"getarray",
                        (char*)"putint",
                        (char*)"putch",
                        (char*)"putarray"};
            for(int i=0;i<6;i++){
                se = new IdentifierSymbolEntry(TypeSystem::intType, a[i], identifiers->getLevel());
                identifiers->install(a[i],se);
                fprintf(stderr, "function \"%s\" is defined\n", (char*)a[i]);
            }

            first=false;
        }
        Type *funcType;
        funcType = new FunctionType($1,{});
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        fprintf(stderr, "function \"%s\" is defined\n", (char*)$2);


        identifiers = new SymbolTable(identifiers);
    } 
    BlockStmt{
        fprintf(stderr, "functionpara\n");
        SymbolEntry *se;
        se = identifiers->lookup($2);

        if(se==nullptr)
            return 1;
        //assert(se != nullptr);
        $$ = new FunctionDef(se, $4,$7);
        //$$->output(identifiers->getLevel());
        //fprintf(stderr, "function");


        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
//函数的形参
FuncFParams
    : FuncFParamUnit{$$=$1;}
    | FuncFParams SEP FuncFParamUnit{
        $$=new SeqNode($1,$3);
    }
    ;
FuncFParamUnit
    : Type ID{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        fprintf(stderr, "FuncFParamUnit \"%s\" is defined\n", (char*)$2);
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    | Type ID ASSIGN Exp{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        fprintf(stderr, "FuncFParamUnit \"%s\" is defined\n", (char*)$2);
        $$ = new DeclAssignStmt(new Id(se),$4);
        delete []$2;
    }
    ;
//函数的实参
FuncRParams
    : FuncRParamUnit{$$=$1;}
    | FuncRParams SEP FuncRParamUnit{$$=new SeqNode($1,$3);}
    ;
FuncRParamUnit
    : Exp{$$=new ExpStmt($1);}
    ;

//Func#####################################################

%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
