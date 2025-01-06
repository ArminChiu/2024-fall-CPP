#ifndef _ASTNODEVISITOR_
#define _ASTNODEVISITOR_

#include "AstNode.h"

class AstNode_Visitor {
   public:
    virtual void visit(comp_unit_node& node) = 0;
    virtual void visit(comp_unit_child_node& node) = 0;
    virtual void visit(global_def_node& node) = 0;
    virtual void visit(func_def_node& node) = 0;
    virtual void visit(expr_node& node) = 0;
    virtual void visit(cond_node& node) = 0;
    virtual void visit(binop_expr_node& node) = 0;
    virtual void visit(unaryop_expr_node& node) = 0;
    virtual void visit(lval_node& node) = 0;
    virtual void visit(number_node& node) = 0;
    virtual void visit(stmt_node& node) = 0;
    virtual void visit(var_def_stmt_node& node) = 0;
    virtual void visit(var_def_node& node) = 0;
    virtual void visit(assign_stmt_node& node) = 0;
    virtual void visit(func_call_stmt_node& node) = 0;
    virtual void visit(block_node& node) = 0;
    virtual void visit(if_stmt_node& node) = 0;
    virtual void visit(while_stmt_node& node) = 0;
    virtual void visit(empty_stmt_node& node) = 0;
};

#endif