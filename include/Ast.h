#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include "Operand.h"

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node
{
private:
    static int counter;
    int seq;
protected:
    std::vector<Instruction*> true_list;
    std::vector<Instruction*> false_list;
    static IRBuilder *builder;
    void backPatch(std::vector<Instruction*> &list, BasicBlock*bb);
    std::vector<Instruction*> merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2);

public:
    Node();
    int getSeq() const {return seq;};
    // 什么是IRBuilder?
    static void setIRBuilder(IRBuilder*ib) {builder = ib;};
    virtual void output(int level) = 0;
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;
    // 其实没太搞懂
    std::vector<Instruction*>& trueList() {return true_list;}
    std::vector<Instruction*>& falseList() {return false_list;}
};

class ExprNode : public Node
{
protected:
    SymbolEntry *symbolEntry;
    Operand *dst;   // The result of the subtree is stored into destination(dst).
public:
    ExprNode(SymbolEntry *symbolEntry) : symbolEntry(symbolEntry){};
    Operand* getOperand() {return dst;};
    SymbolEntry* getSymPtr() {return symbolEntry;};
};
class UnaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr;
public:
    enum {ADD, SUB, NOT};
    UnaryExpr(SymbolEntry *se, int op, ExprNode*expr) : ExprNode(se), op(op), expr(expr){};
    void output(int level);
    void typeCheck();
    void genCode();
};
class BinaryExpr : public ExprNode
{
private:
    // 每个ExprNode都有dst
    int op;
    ExprNode *expr1, *expr2;
public:
    enum {ADD, SUB, AND, OR, LESS,GRAN,LESSEQ,GRANEQ,EQ,NEQ,MULT,DIV,MOD};
    BinaryExpr(SymbolEntry *se, int op, ExprNode*expr1, ExprNode*expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2){dst = new Operand(se);};
    void output(int level);
    void typeCheck();
    void genCode();
};

class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se){dst = new Operand(se);};
    void output(int level);
    void typeCheck();
    void genCode();
};

class Id : public ExprNode
{
public:
    Id(SymbolEntry *se) : ExprNode(se){SymbolEntry *temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp);};
    void output(int level);
    void typeCheck();
    void genCode();
};

class StmtNode : public Node
{};
class ExpStmt : public StmtNode{
private:
    ExprNode* expr;
public:
    ExpStmt(ExprNode* expr):expr(expr){};
    void output(int level);//这个究竟是个啥
    void typeCheck();
    void genCode();
};
class EmptyStmt : public StmtNode{
public:
    EmptyStmt(){};
    void output(int level);//这个究竟是个啥
    void typeCheck();
    void genCode();
};
class BreakStmt : public StmtNode{
public:
    BreakStmt(){};
    void output(int level);//这个究竟是个啥
    void typeCheck();
    void genCode();
};
class ContinueStmt : public StmtNode{
public:
    ContinueStmt(){};
    void output(int level);//这个究竟是个啥
    void typeCheck();
    void genCode();
};
class WhileStmt : public StmtNode{
private:
    ExprNode* cond;
    StmtNode* stmt;
public:
    WhileStmt(ExprNode*cond, StmtNode*stmt):cond(cond),stmt(stmt){};
    void output(int level);//这个究竟是个啥
    void typeCheck();
    void genCode();
};
class CompoundStmt : public StmtNode
{
private:
    StmtNode *stmt;
public:
    CompoundStmt(StmtNode *stmt) : stmt(stmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode
{
private:
    StmtNode *stmt1, *stmt2;
public:
    SeqNode(StmtNode *stmt1, StmtNode *stmt2) : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class DeclStmt : public StmtNode
{
private:
    
public:
    Id *id;
    DeclStmt(Id *id) : id(id){};
    void output(int level);
    void typeCheck();
    void genCode();
};
class DeclAssignStmt : public DeclStmt
{
public:
    ExprNode *expr;

    DeclAssignStmt(Id *id,ExprNode*expr) : DeclStmt(id),expr(expr){};
    void output(int level);
    void typeCheck();
    void genCode();

};
class ConstDeclAssignStmt : public DeclAssignStmt
{
public:
    ConstDeclAssignStmt(Id *id,ExprNode*expr) : DeclAssignStmt(id,expr){};
    void output(int level);
    void typeCheck();
    void genCode();

};
class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class IfElseStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
    StmtNode *elseStmt;
public:
    IfElseStmt(ExprNode *cond, StmtNode *thenStmt, StmtNode *elseStmt) : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;
public:
    ReturnStmt(ExprNode*retValue) : retValue(retValue) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;
public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class FunctionDef : public StmtNode
{
private:
    // 为啥这里需要一个表项
    SymbolEntry *se;
    StmtNode*para=nullptr;
    StmtNode *stmt;
public:
    FunctionDef(SymbolEntry *se, StmtNode *stmt) : se(se), stmt(stmt){};
    FunctionDef(SymbolEntry *se, StmtNode *para,StmtNode *stmt) : se(se), para(para),stmt(stmt){};
    
    void output(int level);
    void typeCheck();
    void genCode();
};
class CallFunc : public ExprNode
{
private:
    char* funcName;
    StmtNode* para=nullptr;
public:
    CallFunc(SymbolEntry *se,char* funcName) : ExprNode(se),funcName(funcName){};
    CallFunc(SymbolEntry *se,char* funcName,StmtNode* para) : ExprNode(se),funcName(funcName),para(para){};
    void output(int level);
    void typeCheck();
    void genCode();
};
class Ast
{
private:
    Node* root;
public:
    Ast() {root = nullptr;}
    void setRoot(Node*n) {root = n;}
    void output();
    void typeCheck();
    void genCode(Unit *unit);
};

#endif
