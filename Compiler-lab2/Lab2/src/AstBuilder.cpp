#include "AstBuilder.h"

namespace antlrcpp {
antlrcpp::Any AstBuilder::visitCompUnit(SafeCParser::CompUnitContext* ctx) {
    auto result = new comp_unit_node;
    auto compile_units = ctx->children;

    result->line = ctx->getStart()->getLine();
    result->pos = ctx->getStart()->getCharPositionInLine();

    // Ignore the last EOF in the for loop.
    for (auto i = 0; i < compile_units.size() - 1; i++) {
        if (auto decl =
                dynamic_cast<SafeCParser::DeclContext*>(compile_units[i])) {
            // global define
            auto global_def_stmt_n = visit(decl).as<stmt_node*>();
            auto var_def_stmt_n =
                dynamic_cast<var_def_stmt_node*>(global_def_stmt_n);
            result->comp_units.push_back(
                ptr<comp_unit_child_node>(var_def_stmt_n));

        } else if (auto func_def = dynamic_cast<SafeCParser::FuncDefContext*>(
                       compile_units[i])) {
            // function define
            auto func_def_n = visit(func_def).as<func_def_node*>();
            result->comp_units.push_back(ptr<comp_unit_child_node>(func_def_n));

        } else {
            // Error.
            log(compile_units[i]->getText());
            assert(0 && "Unknown Compile Unit.");
        }
    }
    return result;
}

antlrcpp::Any AstBuilder::visitDecl(SafeCParser::DeclContext* ctx) {

    if (auto const_decl = ctx->constDecl()) {
        return visit(const_decl);

    } else if (auto var_decl = ctx->varDecl()) {
        return visit(var_decl);

    } else {
        assert(0 && "Unknown DeclContext.");
    }
}

antlrcpp::Any AstBuilder::visitFuncDef(SafeCParser::FuncDefContext* ctx) {
    // DONE
    auto result = new func_def_node; // 函数定义节点
    result->line = ctx->getStart()->getLine(); // 行号
    result->pos = ctx->getStart()->getCharPositionInLine(); // 列号
    result->name = ctx->Identifier()->getSymbol()->getText(); // 函数名
    result->body.reset(visit(ctx->block()).as<block_node*>()); // 函数体

    return result; // 返回函数定义节点
}

antlrcpp::Any AstBuilder::visitConstDecl(SafeCParser::ConstDeclContext* ctx) {
    // DONE
    auto result = new var_def_stmt_node; // 变量定义语句节点

    result->line = ctx->getStart()->getLine(); // 行号
    result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

    BType var_type; // 变量类型
    if (ctx->bType()->Int()) { // 若为整型
        var_type = BType::INT; // 设置为整型
    } else {
        assert(0 && "Unknown bType."); // 未知类型
    }

    for (auto const_def : ctx->constDef()) { // 遍历所有常量定义
        auto var_def_n = visit(const_def).as<var_def_node*>(); // 获取常量定义节点
        var_def_n->btype = var_type; // 设置变量类型
        var_def_n->is_const = true; // 设置为常量

        result->var_defs.push_back(ptr<var_def_node>(var_def_n)); // 添加到变量定义列表
    }

    return dynamic_cast<stmt_node*>(result); // 返回变量定义语句节点
}

antlrcpp::Any AstBuilder::visitConstDef(SafeCParser::ConstDefContext* ctx) {
    auto result = new var_def_node; // 创建变量定义节点
    if (auto array = ctx->array()) { // 若为数组
        // DONE: Array
        auto array_n = visit(array).as<var_def_node*>(); // 获取数组定义节点
        delete (result); // 删除变量定义节点
        result = array_n; // 设置为数组定义节点
        for (auto exp : ctx->exp()) { // 遍历所有表达式
            auto exp_n = visit(exp).as<expr_node*>(); // 获取表达式节点
            result->initializers.push_back(ptr<expr_node>(exp_n)); // 添加到初始化列表
        }
        // 获取数组长度
        if (result->array_length == NULL && result->initializers.size() > 0) { // 若数组长度为空且初始化列表不为空
            auto length_n = new number_node; // 创建数字节点
            length_n->btype = BType::INT; // 设置为整型
            length_n->number = result->initializers.size(); // 设置为初始化列表长度
            if (result->is_obc) { // 若为OBC数组
                length_n->line = array->obcArray() // 设置行号
                                     ->unobcArray()
                                     ->LeftBracket()
                                     ->getSymbol()
                                     ->getLine();
                length_n->pos = array->obcArray() // 设置列号
                                    ->unobcArray()
                                    ->LeftBracket()
                                    ->getSymbol()
                                    ->getCharPositionInLine();
            } else { // 若为非OBC数组
                length_n->line =
                    array->unobcArray()->LeftBracket()->getSymbol()->getLine(); // 设置行号
                length_n->pos = array->unobcArray() // 设置列号
                                    ->LeftBracket()
                                    ->getSymbol()
                                    ->getCharPositionInLine();
            }
            result->array_length.reset(length_n); // 设置数组长度
        }
    } else if (ctx->Identifier()) { // 若为标识符
        // DONE: Scalar
        result->line = ctx->getStart()->getLine(); // 设置行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 设置列号
        result->name = ctx->Identifier()->getSymbol()->getText(); // 设置变量名
        result->array_length = NULL; // 设置数组长度为空
        result->is_obc = false; // 设置为非OBC数组
        auto exp_n = visit(ctx->exp(0)).as<expr_node*>(); // 获取表达式节点
        result->initializers.push_back(ptr<expr_node>(exp_n)); // 添加到初始化列表
    } else { // 未知类型
        assert(0 && "Unknown ConstDef");
    }
    return result; // 返回变量定义节点
}

antlrcpp::Any AstBuilder::visitVarDecl(SafeCParser::VarDeclContext* ctx) {
    // DONE
    auto result = new var_def_stmt_node; // 变量定义语句节点

    result->line = ctx->getStart()->getLine(); // 行号
    result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

    BType var_type; // 变量类型
    if (ctx->bType()->Int()) { // 若为整型
        var_type = BType::INT; // 设置为整型
    } else {
        assert(0 && "Unknown bType."); // 未知类型
    }

    for (auto var_def : ctx->varDef()) { // 遍历所有变量定义
        auto var_def_n = visit(var_def).as<var_def_node*>(); // 获取变量定义节点
        var_def_n->btype = var_type; // 设置变量类型
        var_def_n->is_const = false; // 设置为非常量

        result->var_defs.push_back(ptr<var_def_node>(var_def_n)); // 添加到变量定义列表
    }

    return dynamic_cast<stmt_node*>(result); // 返回变量定义语句节点
}

antlrcpp::Any AstBuilder::visitBType(SafeCParser::BTypeContext* ctx) {
    /* Nothing to do here. */
    return NULL;
}

antlrcpp::Any AstBuilder::visitVarDef(SafeCParser::VarDefContext* ctx) {
    auto result = new var_def_node; // 创建变量定义节点
    if (auto array = ctx->array()) { // 若为数组
        // DONE: Array
        auto array_n = visit(array).as<var_def_node*>(); // 获取数组定义节点
        delete (result); // 删除变量定义节点
        result = array_n; // 设置为数组定义节点
        if (ctx->Assign()) { // 若有赋值
            for (auto exp : ctx->exp()) { // 遍历所有表达式
                auto exp_n = visit(exp).as<expr_node*>(); // 获取表达式节点
                result->initializers.push_back(ptr<expr_node>(exp_n)); // 添加到初始化列表
            }
        }
        // 获取数组长度
        if (result->array_length == NULL && result->initializers.size() > 0) { // 若数组长度为空且初始化列表不为空
            auto length_n = new number_node; // 创建数字节点
            length_n->btype = BType::INT; // 设置为整型
            length_n->number = result->initializers.size(); // 设置为初始化列表长度
            if (result->is_obc) { // 若为OBC数组
                length_n->line = array->obcArray() // 设置行号
                                     ->unobcArray()
                                     ->LeftBracket()
                                     ->getSymbol()
                                     ->getLine();
                length_n->pos = array->obcArray() // 设置列号
                                    ->unobcArray()
                                    ->LeftBracket()
                                    ->getSymbol()
                                    ->getCharPositionInLine();
            } else { // 若为非OBC数组
                length_n->line = array->unobcArray()->LeftBracket()->getSymbol()->getLine(); // 设置行号
                length_n->pos = array->unobcArray() // 设置列号
                                    ->LeftBracket()
                                    ->getSymbol()
                                    ->getCharPositionInLine();
            }
            result->array_length.reset(length_n); // 设置数组长度
        }
    } else if (ctx->Identifier()) {
        // DONE: Scalar
        result->line = ctx->getStart()->getLine(); // 设置行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 设置列号
        result->name = ctx->Identifier()->getSymbol()->getText(); // 设置变量名
        result->array_length = NULL; // 设置数组长度为空 // TODO
        result->is_obc = false; // 设置为非OBC数组
        result->is_const = false; // 设置为非常量
        if (ctx->Assign()) { // 若有赋值
            auto exp_n = visit(ctx->exp(0)).as<expr_node*>(); // 获取表达式节点
            result->initializers.push_back(ptr<expr_node>(exp_n)); // 添加到初始化列表
        }
    } else {
        assert(0 && "Unknown VarDef");
    }
    return result; // 返回变量定义节点
}

antlrcpp::Any AstBuilder::visitArray(SafeCParser::ArrayContext* ctx) {
    // DONE
    if (auto obc_array = ctx->obcArray()) { // 若为OBC数组
        return visit(obc_array);
    } else if (auto unobc_array = ctx->unobcArray()) { // 若为非OBC数组
        return visit(unobc_array);
    } else {
        assert(0 && "Unknown Array");
    }
}

antlrcpp::Any AstBuilder::visitObcArray(SafeCParser::ObcArrayContext* ctx) {
    // DONE
    auto result = visitUnobcArray(ctx->unobcArray()).as<var_def_node*>();
    result->is_obc = true;
    return result;
}

antlrcpp::Any AstBuilder::visitUnobcArray(SafeCParser::UnobcArrayContext* ctx) {
    // DONE
    auto result = new var_def_node; // 创建变量定义节点
    result->line = ctx->getStart()->getLine(); // 设置行号
    result->pos = ctx->getStart()->getCharPositionInLine(); // 设置列号
    result->name = ctx->Identifier()->getSymbol()->getText(); // 设置变量名
    result->array_length = NULL; // 设置数组长度为空
    result->is_obc = false; // 设置为非OBC数组

    if (auto exp = ctx->exp()) { // 若有表达式
        auto exp_n = visit(exp).as<expr_node*>(); // 获取表达式节点
        result->array_length = ptr<expr_node>(exp_n); // 设置数组长度
    }
    return result;
}

antlrcpp::Any AstBuilder::visitBlock(SafeCParser::BlockContext* ctx) {
    auto result = new block_node;

    result->line = ctx->getStart()->getLine();
    result->pos = ctx->getStart()->getCharPositionInLine();

    for (auto block_item : ctx->blockItem()) {
        auto block_item_n = visit(block_item).as<stmt_node*>();
        result->body.push_back(ptr<stmt_node>(block_item_n));
    }
    return result;
}

antlrcpp::Any AstBuilder::visitBlockItem(SafeCParser::BlockItemContext* ctx) {
    // DONE
    if (ctx->decl()) { // 若为声明
        return visit(ctx->decl());
    } else if (ctx->stmt()) { // 若为语句
        return visit(ctx->stmt());
    } else {
        assert(0 && "Unknown blockItem.");
    }
}

antlrcpp::Any AstBuilder::visitStmt(SafeCParser::StmtContext* ctx) {
    /*stmt
    : block 
    | SemiColon
    | lval Assign exp SemiColon 
    | If LeftParen cond RightParen stmt (Else stmt)? 
    | While LeftParen cond RightParen stmt 
    | Identifier LeftParen RightParen SemiColon; */
    if (ctx->block()) { // 返回赋值语句节点
        auto result = visit(ctx->block()).as<block_node*>(); // 获取块节点
        return dynamic_cast<stmt_node*>(result); // 返回块节点
    } else if (ctx->lval()) { // block
        auto result = new assign_stmt_node; // 赋值语句节点
        result->line = ctx->getStart()->getLine(); // 行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

        auto lval_n = visit(ctx->lval()).as<lval_node*>(); // 获取左值节点
        auto exp_n = visit(ctx->exp()).as<expr_node*>(); // 获取表达式节点
        result->target = ptr<lval_node>(lval_n); // 设置左值
        result->value = ptr<expr_node>(exp_n); // 设置表达式

        return dynamic_cast<stmt_node*>(result); // lval Assign exp SemiColon
    } else if (ctx->If()) { // If LeftParen cond RightParen stmt (Else stmt)?
        auto result = new if_stmt_node; // If语句节点

        result->line = ctx->getStart()->getLine(); // 行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

        auto cond = ctx->cond(); // 获取条件
        auto if_body = ctx->stmt(0); // 获取If体
        result->cond.reset(visit(cond).as<cond_node*>()); // 设置条件
        result->if_body.reset(visit(if_body).as<stmt_node*>()); // 设置If体

        if (ctx->Else()) { // 若有Else
            auto else_body = ctx->stmt(1); // 获取Else体
            result->else_body.reset(visit(else_body).as<stmt_node*>()); // 设置Else体
        }

        return dynamic_cast<stmt_node*>(result); // 返回If语句节点
    } else if (ctx->While()) { // While LeftParen cond RightParen stmt
        auto result = new while_stmt_node; // While语句节点

        result->line = ctx->getStart()->getLine(); // 行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

        auto cond = ctx->cond(); // 获取条件
        auto while_body = ctx->stmt(0); // 获取While体
        result->cond.reset(visit(cond).as<cond_node*>()); // 设置条件
        result->body.reset(visit(while_body).as<stmt_node*>()); // 设置While体

        return dynamic_cast<stmt_node*>(result); // 返回While语句节点
    } else if (ctx->Identifier()) { // Identifier LeftParen RightParen SemiColon
        auto result = new func_call_stmt_node; // 函数调用语句节点

        result->line = ctx->getStart()->getLine(); // 行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

        result->name = ctx->Identifier()->getSymbol()->getText(); // 设置函数名

        return dynamic_cast<stmt_node*>(result); // 返回函数调用语句节点
    } else if (ctx->SemiColon()) { // Empty stmt
        return NULL; // 空语句
    } else {
        assert(0 && "Unknown stmt."); // 未知语句
    }
}

antlrcpp::Any AstBuilder::visitCond(SafeCParser::CondContext* ctx) {
    // DONE
    /*cond: LeftParen cond RightParen
    | exp (Equal | NonEqual | Less | Greater | LessEqual | GreaterEqual) exp;*/
    if (ctx->cond()) { // LeftParen cond RightParen
        return visit(ctx->cond());
    } else if (ctx->exp().size() == 2) { // exp (Equal | NonEqual | Less | Greater | LessEqual | GreaterEqual) exp
        auto result = new cond_node; // 条件节点
        result->line = ctx->getStart()->getLine(); // 行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

        result->lhs.reset(visit(ctx->exp(0)).as<expr_node*>()); // 左表达式
        result->rhs.reset(visit(ctx->exp(1)).as<expr_node*>()); // 右表达式

        if (ctx->Equal()) {
            result->op = RelOp::EQUAL;
        } else if (ctx->NonEqual()) {
            result->op = RelOp::NON_EQUAL;
        } else if (ctx->Less()) {
            result->op = RelOp::LESS;
        } else if (ctx->Greater()) {
            result->op = RelOp::GREATER;
        } else if (ctx->LessEqual()) {
            result->op = RelOp::LESS_EQUAL;
        } else if (ctx->GreaterEqual()) {
            result->op = RelOp::GREATER_EQUAL;
        } else {
            assert(0 && "Unknown Relationship Operator.");
        }
        return result;
    } else {
        assert(0 && "Unknown Cond.");
    }
}

antlrcpp::Any AstBuilder::visitLval(SafeCParser::LvalContext* ctx) {
    // TODO
    auto result = new lval_node; // 左值节点
    result->line = ctx->getStart()->getLine(); // 设置行号
    result->pos = ctx->getStart()->getCharPositionInLine(); // 设置列号
    result->name = ctx->Identifier()->getSymbol()->getText(); // 设置变量名
    if (auto index = ctx->exp()) { // Identifier LeftBracket exp RightBracket
        auto exp_n = visit(index).as<expr_node*>(); // 获取表达式节点
        result->array_index = ptr<expr_node>(exp_n); // 设置数组索引
    }
    return result; // 返回左值节点
}

antlrcpp::Any AstBuilder::visitNumber(SafeCParser::NumberContext* ctx) {
    auto result = new number_node;
    result->btype = BType::INT;
    result->line = ctx->getStart()->getLine();
    result->pos = ctx->getStart()->getCharPositionInLine();
    auto num_str = ctx->IntConst()->getText();
    // Hex
    if (num_str.size() > 2 && num_str[0] == '0' &&
        (num_str[1] == 'x' || num_str[1] == 'X')) {
        result->number = std::stoi(num_str, 0, 16);
        // Dec
    } else {
        result->number = std::stoi(num_str, 0, 10);
    }
    return result;
}

antlrcpp::Any AstBuilder::visitExp(SafeCParser::ExpContext* ctx) {
    // DONE
    auto exps = ctx->exp();
    if (ctx->lval()) { // lval
        auto result = visit(ctx->lval()).as<lval_node*>();
        return dynamic_cast<expr_node*>(result);
    } else if (ctx->number()) { // number
        auto result = visit(ctx->number()).as<number_node*>();
        return dynamic_cast<expr_node*>(result);
    } else if (exps.size() == 2) { // exp (Plus | Minus | Multiply | Divide | Modulo) exp
        auto result = new binop_expr_node;
        result->line = ctx->getStart()->getLine();
        result->pos = ctx->getStart()->getCharPositionInLine();

        if (ctx->Plus()) {
            result->op = BinOp::PLUS;
        } else if (ctx->Minus()) {
            result->op = BinOp::MINUS;
        } else if (ctx->Multiply()) {
            result->op = BinOp::MULTIPLY;
        } else if (ctx->Divide()) {
            result->op = BinOp::DIVIDE;
        } else if (ctx->Modulo()) {
            result->op = BinOp::MODULO;
        }
        result->lhs.reset(visit(exps[0]).as<expr_node*>()); // 左表达式
        result->rhs.reset(visit(exps[1]).as<expr_node*>()); // 右表达式

        return dynamic_cast<expr_node*>(result); // 返回二元表达式节点
    } else if (exps.size() == 1) { // (Plus | Minus) exp
        auto result = new unaryop_expr_node;
        result->line = ctx->getStart()->getLine(); // 行号
        result->pos = ctx->getStart()->getCharPositionInLine(); // 列号

        if (ctx->Plus()) {
            result->op = UnaryOp::PLUS;
        } else if (ctx->Minus()) {
            result->op = UnaryOp::MINUS;
        }
        result->rhs.reset(visit(exps[0]).as<expr_node*>()); // 右表达式

        return dynamic_cast<expr_node*>(result); // 返回一元表达式节点
    } else if (ctx->LeftParen() && ctx->RightParen()) { // LeftParen exp RightParen
        return visit(ctx->exp(0));
    }else {
        assert(0 && "Unknown exp.");
    }
}

ptr<ast_node> AstBuilder::operator()(antlr4::tree::ParseTree* ctx) {
    auto result = visit(ctx);

    if (result.is<ast_node*>())
        return ptr<ast_node>(result.as<ast_node*>());
    if (result.is<comp_unit_node*>())
        return ptr<ast_node>(result.as<comp_unit_node*>());
    if (result.is<comp_unit_child_node*>())
        return ptr<ast_node>(result.as<comp_unit_child_node*>());
    if (result.is<func_def_node*>())
        return ptr<ast_node>(result.as<func_def_node*>());
    if (result.is<expr_node*>())
        return ptr<ast_node>(result.as<expr_node*>());
    if (result.is<cond_node*>())
        return ptr<ast_node>(result.as<cond_node*>());
    if (result.is<binop_expr_node*>())
        return ptr<ast_node>(result.as<binop_expr_node*>());
    if (result.is<unaryop_expr_node*>())
        return ptr<ast_node>(result.as<unaryop_expr_node*>());
    if (result.is<lval_node*>())
        return ptr<ast_node>(result.as<lval_node*>());
    if (result.is<number_node*>())
        return ptr<ast_node>(result.as<number_node*>());
    if (result.is<stmt_node*>())
        return ptr<ast_node>(result.as<stmt_node*>());
    if (result.is<var_def_stmt_node*>())
        return ptr<ast_node>(result.as<var_def_stmt_node*>());
    if (result.is<assign_stmt_node*>())
        return ptr<ast_node>(result.as<assign_stmt_node*>());
    if (result.is<func_call_stmt_node*>())
        return ptr<ast_node>(result.as<func_call_stmt_node*>());
    if (result.is<block_node*>())
        return ptr<ast_node>(result.as<block_node*>());
    if (result.is<if_stmt_node*>())
        return ptr<ast_node>(result.as<if_stmt_node*>());
    if (result.is<while_stmt_node*>())
        return ptr<ast_node>(result.as<while_stmt_node*>());
    if (result.is<empty_stmt_node*>())
        return ptr<ast_node>(result.as<empty_stmt_node*>());

    return ptr<ast_node>(result.as<ast_node*>());
}
}  // namespace antlrcpp
