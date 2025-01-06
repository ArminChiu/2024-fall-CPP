#include "AstNode_Visitor.h"

void comp_unit_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void comp_unit_child_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void global_def_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void func_def_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void expr_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void cond_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void binop_expr_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void unaryop_expr_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void lval_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void number_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void var_def_stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void var_def_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void assign_stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void func_call_stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void block_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void if_stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void while_stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}
void empty_stmt_node::accept(AstNode_Visitor& visitor) {
    visitor.visit(*this);
}