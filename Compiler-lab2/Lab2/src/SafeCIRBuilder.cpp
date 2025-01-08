#include "SafeCIRBuilder.h"
#include <llvm-15/llvm/IR/Value.h>

// For lval and rval identification.
// Expect the function f to return an rval.
#define EXPECT_RVAL(f) val_type_stack.push(FLAG::RVAL); \
                       f; \
                       val_type_stack.pop();
// Expect the function f to return an lval.
#define EXPECT_LVAL(f) val_type_stack.push(FLAG::LVAL); \
                       f; \
                       val_type_stack.pop();
// Is current expected to return an lval.
#define IS_EXPECT_LVAL() val_type_stack.top() == FLAG::LVAL

SafeCIRBuilder::SafeCIRBuilder(llvm::LLVMContext& ctx)
    : context(ctx), builder(ctx) {}

SafeCIRBuilder::~SafeCIRBuilder() {}

void SafeCIRBuilder::obc_check(llvm::Value* index, int array_length,
                               int node_line, int node_pos, std::string name) {
    // Insert code to check if index is in [0, length).
    // If not in range, call obc_check_error to report error.
    // Basic logic:
    //          ... (current insert point)
    //          cmp = index < 0 || index >= length
    //          br cmp, check_fail, check_success
    //      check_fail bb:
    //          call obc_check(node_line, node_pos, name)
    //          ret void
    //      check_success bb:
    //          ... (next insert point here)

    // DONE: Implement.
    llvm::Function* function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* current_bb = builder.GetInsertBlock();
    llvm::BasicBlock* check_fail_bb = llvm::BasicBlock::Create(context, "check_fail", function);
    llvm::BasicBlock* check_success_bb = llvm::BasicBlock::Create(context, "check_success", function);
    
    // 比较 index < 0
    llvm::Value* isNegative = builder.CreateICmpSLT(index, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), "isNegative");
    // 比较 index >= array_length
    llvm::Value* isOutOfRange = builder.CreateICmpSGE(index, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), array_length), "isOutOfRange");
    // 将两个比较结果进行 OR 操作
    llvm::Value* cmp = builder.CreateOr(isNegative, isOutOfRange, "cmp");
    // 根据 cmp 结果进行条件分支
    builder.CreateCondBr(cmp, check_fail_bb, check_success_bb);
    // 在 check_fail 块中插入错误处理代码
    builder.SetInsertPoint(check_fail_bb);
        llvm::Value *arg0_val = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), node_line);
        llvm::Value *arg1_val = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), node_pos);
        llvm::Value *arg2_val = llvm::ConstantDataArray::getString(context, name);
        llvm::Value *arg0_ptr = lookup_variable("arg0").val_ptr;
        llvm::Value *arg1_ptr = lookup_variable("arg1").val_ptr;
        llvm::Value *arg2_ptr = lookup_variable("arg2").val_ptr;
        llvm::Function *check_err = functions["obc_check_error"];
        builder.CreateStore(arg0_val, arg0_ptr);
        builder.CreateStore(arg1_val, arg1_ptr);
        builder.CreateStore(arg2_val, arg2_ptr);
        builder.CreateCall(check_err, {});
    builder.CreateRetVoid();

    builder.SetInsertPoint(check_success_bb);
    /*  
        // Call obc_check_error to report error: 
        llvm::Value *arg0_val = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), node_line);
        llvm::Value *arg1_val = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), node_pos);
        llvm::Value *arg2_val = llvm::ConstantDataArray::getString(context, name);
        llvm::Value *arg0_ptr = lookup_variable("arg0").val_ptr;
        llvm::Value *arg1_ptr = lookup_variable("arg1").val_ptr;
        llvm::Value *arg2_ptr = lookup_variable("arg2").val_ptr;
        llvm::Function *check_err = functions["obc_check_error"];
        builder.CreateStore(arg0_val, arg0_ptr);
        builder.CreateStore(arg1_val, arg1_ptr);
        builder.CreateStore(arg2_val, arg2_ptr);
        builder.CreateCall(check_err, {});
    */

    return;
}

void SafeCIRBuilder::visit(comp_unit_node& node) {
    std::cout << "visit(comp_unit_node& node) 被调用" << std::endl;
    for (auto comp_unit : node.comp_units) {
        if (auto global_var_decl =
                std::dynamic_pointer_cast<var_def_stmt_node>(comp_unit)) {
            
            cur_scope = FLAG::GLOBAL_SCOPE;
            global_var_decl->accept(*this);

        } else if (auto global_func_def =
                       std::dynamic_pointer_cast<func_def_node>(comp_unit)) {

            global_func_def->accept(*this);

        }
    }
}

void SafeCIRBuilder::visit(comp_unit_child_node& node) {
    // Nothing to do here.
    return;
}

void SafeCIRBuilder::visit(global_def_node& node) {
    // Nothing to do here.
    return;
}

void SafeCIRBuilder::visit(func_def_node& node) {
    current_function = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false),
        llvm::GlobalValue::LinkageTypes::ExternalLinkage, node.name,
        module.get());
    functions[node.name] = current_function;
    auto entry = llvm::BasicBlock::Create(context, "entry", current_function);
    builder.SetInsertPoint(entry);
    bb_count = 0;
    auto body = node.body;
    body->accept(*this);
    builder.CreateRetVoid();
}

void SafeCIRBuilder::visit(block_node& node) {
    enter_scope();
    for (auto stmt : node.body) {
        cur_scope = FLAG::BLOCK_SCOPE;
        if (auto var_def_statement =
                std::dynamic_pointer_cast<var_def_stmt_node>(stmt)){
            var_def_statement->accept(*this);
        } else if (auto var_define =
                std::dynamic_pointer_cast<var_def_node>(stmt)){
            var_define->accept(*this);
        } else if (auto assign_statement =
                std::dynamic_pointer_cast<assign_stmt_node>(stmt)){
            assign_statement->accept(*this);
        } else if (auto func_call_statement =
                std::dynamic_pointer_cast<func_call_stmt_node>(stmt)){
            func_call_statement->accept(*this);
        } else if (auto block = 
                std::dynamic_pointer_cast<block_node>(stmt)){
            block->accept(*this);
        } else if (auto if_statement =
                std::dynamic_pointer_cast<if_stmt_node>(stmt)){
            if_statement->accept(*this);
        } else if (auto while_statement =
                std::dynamic_pointer_cast<while_stmt_node>(stmt)){
            while_statement->accept(*this);
        } else if (auto empty_statement =
                std::dynamic_pointer_cast<empty_stmt_node>(stmt)){
            empty_statement->accept(*this);
        }
        cur_scope = FLAG::GLOBAL_SCOPE;
    }
    exit_scope();
}

void SafeCIRBuilder::visit(stmt_node& node) {
    // Nothing to do here.
}

void SafeCIRBuilder::visit(var_def_stmt_node& node) {
    for (auto var_defs : node.var_defs){
        var_defs->accept(*this);
    }
}

void SafeCIRBuilder::visit(func_call_stmt_node& node) {
    if (functions.count(node.name) == 0) {
        std::cerr << node.line << ":" << node.pos << ": function " << node.name
                  << " is not declared" << std::endl;
        error_flag = true;
        return;
    }
    builder.CreateCall(functions[node.name], {});
}

void SafeCIRBuilder::visit(empty_stmt_node& node) {
    // Nothing to do here.
}

void SafeCIRBuilder::visit(expr_node& node) {
    // Nothing to do here.
}

void SafeCIRBuilder::visit(cond_node& node) { 
    std::cout << "visit(cond_node& node) 被调用" << std::endl;
    // DONE: handle condition expression.
    int lhs_const, rhs_const=0;
    bool res;   
    bool if_lhs_const, if_rhs_const = false;
    llvm::Value *lhs_value;
    llvm::Value *rhs_value;
    llvm::Value *result;
    // 访问左操作数
    EXPECT_RVAL(node.lhs->accept(*this));
    if_lhs_const = get_int_result(lhs_const);
    if (!if_lhs_const && !get_result_as_value(&lhs_value)) {
        std::cerr << node.line << ":" << node.pos
                  << ": left operand of condition expression must be a constant or a variable"
                  << std::endl;
        return;
    }

    // 访问右操作数
    EXPECT_RVAL(node.rhs->accept(*this));
    if_rhs_const = get_int_result(rhs_const);
    if (!if_rhs_const && !get_result_as_value(&rhs_value)) {
        std::cerr << node.line << ":" << node.pos
                  << ": left operand of condition expression must be a constant or a variable"
                  << std::endl;
        return;
    }

    if (if_lhs_const && if_rhs_const) {
        switch (node.op) {
            case RelOp::EQUAL:
                res = (lhs_const == rhs_const) ? true : false;
                break;
            case RelOp::NON_EQUAL:
                res = (lhs_const != rhs_const) ? true : false;
                break;
            case RelOp::LESS:
                res = (lhs_const < rhs_const) ? true : false;
                break;
            case RelOp::LESS_EQUAL:
                res = (lhs_const <= rhs_const) ? true : false;
                break;
            case RelOp::GREATER:
                res = (lhs_const > rhs_const) ? true : false;
                break;  
            case RelOp::GREATER_EQUAL:
                res = (lhs_const >= rhs_const) ? true : false;
                break; 
            default:
                std::cerr << node.line << ":" << node.pos << ": unknown condition operator"
                << std::endl;
        }
        set_int_result(res);

    }
    else{
        if (if_lhs_const) {
            lhs_value = builder.getInt32(lhs_const);
        }
        if (if_rhs_const) {
            rhs_value = builder.getInt32(rhs_const);
        }
        switch (node.op) {
            case RelOp::EQUAL:
                result = builder.CreateICmpEQ(lhs_value, rhs_value);
                break;
            case RelOp::NON_EQUAL:
                result = builder.CreateICmpNE(lhs_value, rhs_value);
                break;
            case RelOp::LESS:
                result = builder.CreateICmpSLT(lhs_value, rhs_value);
                break;
            case RelOp::LESS_EQUAL:
                result = builder.CreateICmpSLE(lhs_value, rhs_value);
                break;
            case RelOp::GREATER:
                result = builder.CreateICmpSGT(lhs_value, rhs_value);
                break;
            case RelOp::GREATER_EQUAL:
                result = builder.CreateICmpSGE(lhs_value, rhs_value);
                break;
            default:
                std::cerr << node.line << ":" << node.pos << ": unknown condition operator"
                << std::endl;
        }
        set_value_result(result);
    }
}

void SafeCIRBuilder::visit(number_node& node) {
    std::cout << "visit(number_node& node) 被调用" << std::endl;
    set_int_result(node.number);
}

void SafeCIRBuilder::visit(binop_expr_node& node) {
    // DONE: handle binary operation.
    /*
        Visit lhs and rhs, there can be 2 cases:
        1. If lhs and rhs are both constant, we can calculate the result directly and provide a 
          constant result to the parent node:
            for example: "1+2" -> set_int_result(1+2)
        2. If lhs and rhs are not both constant, build an instruction for the binary op:
            for example: "a+1" -> %res = add %a, 1; set_value_result(res)
            for example: "a+b" -> %res = add %a, %b; set_value_result(res)
    */
    std::cout << "visit(binop_expr_node& node) 被调用" << std::endl;
    int lhs_const, rhs_const, res_const = 0;
    bool if_lhs_const, if_rhs_const;
    llvm::Value *lhs_value;
    llvm::Value *rhs_value;
    llvm::Value *result;

    EXPECT_RVAL(node.lhs->accept(*this));
    if_lhs_const = get_int_result(lhs_const);
    if (!if_lhs_const && !get_result_as_value(&lhs_value)) {
        std::cerr << node.line << ":" << node.pos
                  << ": left operand of binary operation must be a constant or a variable"
                  << std::endl;
        return;
    }

    EXPECT_RVAL(node.rhs->accept(*this));
    if_rhs_const = get_int_result(rhs_const);
    if (!if_rhs_const && !get_result_as_value(&rhs_value)) {
        std::cerr << node.line << ":" << node.pos
                  << ": left operand of binary operation must be a constant or a variable"
                  << std::endl;
        return;
    }

    if (if_lhs_const && if_rhs_const) {
        switch (node.op) {
            case BinOp::PLUS:
                res_const = lhs_const + rhs_const;
                break;
            case BinOp::MINUS:
                res_const = lhs_const - rhs_const;
                break;
            case BinOp::MULTIPLY:
                res_const = lhs_const * rhs_const;
                break;
            case BinOp::DIVIDE:
                res_const = lhs_const / rhs_const;
                break;
            case BinOp::MODULO:
                res_const = lhs_const % rhs_const;
                break;  
            default:
                std::cerr << node.line << ":" << node.pos << ": unknown binary operator"
                << std::endl;
        }
        set_int_result(res_const);

    } else{
        if (if_lhs_const) {
            lhs_value = builder.getInt32(lhs_const);
        }
        if (if_rhs_const) {
            rhs_value = builder.getInt32(rhs_const);
        }
        switch (node.op) {
            case BinOp::PLUS:
                result = builder.CreateAdd(lhs_value, rhs_value);
                break;
            case BinOp::MINUS:
                result = builder.CreateSub(lhs_value, rhs_value);
                break;
            case BinOp::MULTIPLY:
                result = builder.CreateMul(lhs_value, rhs_value);
                break;
            case BinOp::DIVIDE:
                result = builder.CreateSDiv(lhs_value, rhs_value);
                break;
            case BinOp::MODULO:
                result = builder.CreateSRem(lhs_value, rhs_value);
                break;
            default:
                std::cerr << node.line << ":" << node.pos << ": unknown binary operator"
                << std::endl;
        }
        set_value_result(result);
    }
}


void SafeCIRBuilder::visit(unaryop_expr_node& node) {
    // DONE: handle unary operation.
    int operand_const = 0;
    int res_const = 0;
    bool if_operand_const;
    llvm::Value *operand_value;
    llvm::Value *result;

    EXPECT_RVAL(node.rhs->accept(*this)); 
    if_operand_const = get_int_result(operand_const);
    if (!if_operand_const && !get_result_as_value(&operand_value)) {
        std::cerr << node.line << ":" << node.pos
                  << ": operand of unary operator must be a constant or a variable"
                  << std::endl;
        return;
    }

    if (if_operand_const) {
        switch (node.op) {
            case UnaryOp::PLUS:
                res_const = operand_const;
                break;
            case UnaryOp::MINUS:
                res_const = -operand_const;
                break;
            default:
                std::cerr << node.line << ":" << node.pos << ": unknown unary operator"
                << std::endl;
        }
        set_int_result(res_const);

    } else{
        switch (node.op) {
            case UnaryOp::PLUS:
                result = operand_value;
                break;
            case UnaryOp::MINUS:
                result = builder.CreateNeg(operand_value);
                break;
            default:
                std::cerr << node.line << ":" << node.pos << ": unknown unary operator"
                << std::endl;
        }
        set_value_result(result);
    }
}

void SafeCIRBuilder::visit(lval_node& node) {
    std::cout << "visit(lval_node& node) 被调用" << std::endl;
    auto name = node.name;
    std::cout << "lval name is " << name << std::endl;
    VarInfo var_info = lookup_variable(name);
    if (!var_info.is_valid()) {
        std::cerr << node.line << ":" << node.pos << ": variable '" << name
                  << "' is not declared" << std::endl;
        error_flag = true;
        return;
    }

    /*
        Use IS_EXPECT_LVAL() to check if the parent node expects an lval or an rval. And
          the parent must use EXPECT_LVAL() or EXPECT_RVAL() properly.
        If lval is expected, we can directly return the lval of the variable:
            for example: in "%a = 1" and we are visiting "%a", 
            we can return the llvm::Value* of %a.
        If rval is expected, we need to load the value of the variable and return the loaded value.
            for example: in "1 + *%a" and we are visiting "%a", 
            we need to return the loaded value of %a, create "%tmp = load %a" and then set_value_result(%tmp).
    */

    if (!var_info.is_array) {
        std::cout << "判定为非数组" << std::endl;
        // DONE: handle scalar lval
        if (IS_EXPECT_LVAL()) {
                set_value_result(var_info.val_ptr);
        }
                
        else {
            llvm::Value *load_val = builder.CreateLoad(var_info.val_ptr->getType()->getPointerElementType(), var_info.val_ptr, "loadtmp");
            set_value_result(load_val);
        }
    } else {
        std::cout << "判定为数组" << std::endl;
        std::cout << "数组大小: " << var_info.array_length << std::endl;
        // DONE: handle array lval and call obc_check to insert obc check code for obc array.

        llvm::Value* array_ptr = var_info.val_ptr;
        if (node.array_index == nullptr) {
            if (IS_EXPECT_LVAL()) {
                    set_value_result(array_ptr);
            } else {
                llvm::Value *load_val = builder.CreateLoad(array_ptr->getType()->getPointerElementType(), array_ptr, "loadtmp");
                set_value_result(load_val);
            }
            return;
        } else {

            if (!array_ptr) {
                std::cerr << "Error: array_ptr is null" << std::endl;
                return;
            }

            var_info.val_ptr->getType();
            std::cout << "准备访问索引" << std::endl;
            EXPECT_RVAL(node.array_index->accept(*this));
            std::cout << "访问索引结束" << std::endl;
            llvm::Value *index_value;
            if (!get_result_as_value(&index_value)) {
                std::cerr << node.line << ":" << node.pos
                          << ": array index must be a constant or a vailable" 
                          << std::endl;
                return;
            }  
            std::cout << "索引值获取完成" << std::endl;

            if (var_info.is_obc){
                obc_check(index_value, var_info.array_length, node.line, node.pos, name);
            }
            
            std::cout << "array_ptr type: " << std::endl;
            if (!array_ptr) {
                std::cerr << "Error: array_ptr is null" << std::endl;
                return;
            }

            // 打印 array_ptr 和 index_value 的类型信息
            array_ptr->getType()->getPointerElementType()->print(llvm::outs());
            std::cout << std::endl;
            
            llvm::Value* element_ptr = builder.CreateGEP(array_ptr->getType()->getPointerElementType(), array_ptr, {builder.getInt32(0), index_value}, "elementptr");
            std::cout << "元素地址计算完成" << std::endl;
            if (IS_EXPECT_LVAL()) 
                // 如果期望左值，返回数组元素的地址
                set_value_result(element_ptr);
            else {
                // 如果期望右值，加载数组元素的值并返回加载后的值
                llvm::Value* load_val = builder.CreateLoad(element_ptr->getType()->getPointerElementType(), element_ptr, "loadtmp");
                set_value_result(load_val);
            }
        }
        return;
    }
}

void SafeCIRBuilder::visit(var_def_node& node) {
    std::cout << "visit(var_def_node& node) 被调用" << std::endl;
    std::string name = node.name;
    bool is_const = node.is_const;
    bool is_obc = node.is_obc;
    int res_const = 0;
    ptr<expr_node> array_length = node.array_length;
    ptr_vector<expr_node> initializers = node.initializers;

    if (cur_scope == FLAG::GLOBAL_SCOPE) {  // global define
        llvm::GlobalVariable* global_variable;
        llvm::Constant *init_value;

        if (!array_length) {  // not an array
            
            // DONE: create and declare global scalar
            if (!node.initializers.empty()) {
                EXPECT_RVAL(node.initializers[0]->accept(*this));
                if (!get_int_result(res_const)) {
                    std::cerr << node.line << ":" << node.pos
                              << ": initializer must be a constant." 
                              << std::endl;
                    return;
                } else {
                    // init_value = llvm::cast<llvm::Constant>(builder.getInt32(res_const));
                    init_value = llvm::ConstantInt::get(context, llvm::APInt(32, res_const));
                }
            } else {
                // init_value = llvm::cast<llvm::Constant>(builder.getInt32(0));
                init_value = llvm::ConstantInt::get(context, llvm::APInt(32, 0));
            }
            global_variable = new llvm::GlobalVariable(
                *module, 
                llvm::Type::getInt32Ty(context), 
                is_const, 
                llvm::GlobalValue::ExternalLinkage,
                init_value, name);
            if (!declare_variable(name, global_variable, is_const, false, is_obc, 0)){
                std::cerr << node.line << ":" << node.pos << ":"
                          << " variable '" << name << "' is declared more than one times" 
                          << std::endl;
                error_flag = true;
                return;
            }

        } else {              // is an array
            // DONE: create and declare global array
            int array_length;
            EXPECT_RVAL(node.array_length->accept(*this));
            if (!get_int_result(array_length)) {
                std::cerr << node.line << ":" << node.pos
                          << ": array length must be a constant" 
                          << std::endl;
                return;
            } else {
                if (array_length < 0){
                    std::cerr << node.line << ":" << node.pos << ":"
                              << " size of array '" << name << "' is not positive" 
                              << std::endl;
                    error_flag = true;
                    return;
                }
                llvm::ArrayType *array_type = llvm::ArrayType::get(llvm::Type::getInt32Ty(context), array_length);

                if (!node.initializers.empty()) {
                    std::vector<llvm::Constant*> elements;
                    for (auto init : node.initializers) {
                        EXPECT_RVAL(init->accept(*this));
                        if (!get_int_result(res_const)) {
                            std::cerr << node.line << ":" << node.pos << ":"
                                    << "initializer must be a constant" 
                                    << std::endl;
                            return;
                        }
                        elements.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, res_const))); 
                    }
                    if (elements.size() > array_length) {
                        std::cerr << node.line << ":" << node.pos << ":"
                                  << " excess elements in the initializer of array '" << name <<"'"
                                  << std::endl;
                        error_flag = true;
                        return;
                    }
                    for (size_t i = elements.size(); i < array_length; ++i) {
                        elements.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));
                    }
                    init_value = llvm::ConstantArray::get(array_type, elements);
                } else {
                    init_value = llvm::ConstantAggregateZero::get(array_type);
                }
        
                global_variable = new llvm::GlobalVariable(
                    *module, 
                    llvm::ArrayType::get(llvm::Type::getInt32Ty(context), array_length), 
                    is_const, 
                    llvm::GlobalValue::ExternalLinkage,
                    init_value, name);

                if(!declare_variable(name, global_variable, is_const, true, is_obc, array_length)){
                    std::cerr << node.line << ":" << node.pos << ":"
                          << " variable '" << name << "' is declared more than one times" 
                          << std::endl;
                    error_flag = true;
                    return;
                }
            }
        }

    } else {  // local define
        llvm::Value *local_variable;
        llvm::Value *init_value;

        if (!array_length) {  // not an array
            // DONE: create and declare local scalar

            llvm::AllocaInst *local_variable 
            = builder.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, name);

            if (!node.initializers.empty()) {
                EXPECT_RVAL(node.initializers[0]->accept(*this));
                if (!get_result_as_value(&init_value)) {
                    std::cerr << node.line << ":" << node.pos
                              << ": initializer must be a constant or a variable." 
                              << std::endl;
                    return;
                }
            } else {   
                init_value = llvm::ConstantInt::get(context, llvm::APInt(32, 0));
            }  
            builder.CreateStore(init_value, local_variable);
            if(!declare_variable(name, local_variable, is_const, false, is_obc, 0)){
                std::cerr << node.line << ":" << node.pos << ":"
                          << " variable '" << name << "' is declared more than one times" 
                          << std::endl;
                error_flag = true;
                return;
            }
        } else { // is an array
            // DONE: create and declare local array
            int array_length;
            llvm::ArrayType *array_type;            
            llvm::AllocaInst* local_variable;
            EXPECT_RVAL(node.array_length->accept(*this));
            if (!get_int_result(array_length)) {
                std::cerr << node.line << ":" << node.pos
                          << ": array length must be a constant" 
                          << std::endl;
                return;
            } else {
                array_type = llvm::ArrayType::get(llvm::Type::getInt32Ty(context), array_length);
                local_variable = builder.CreateAlloca(array_type, nullptr, name);
                std::cout << "local_variable type: " << std::endl;
                local_variable->getType()->print(llvm::outs());
                std::cout << std::endl;

                if (!node.initializers.empty()) {
                    std::vector<llvm::Value*> elements;
                    for (auto init : node.initializers) {
                        EXPECT_RVAL(init->accept(*this));
                        if (!get_result_as_value(&init_value)) {
                            std::cerr << node.line << ":" << node.pos << ":"
                                      << "initializer must be a constant or a variable" 
                                      << std::endl;
                            return;
                        }
                        elements.push_back(init_value);
                    }
                    for (size_t i = 0; i < elements.size(); ++i) {
                        llvm::Value* idx = builder.getInt32(i);
                        llvm::Value* ptr = builder.CreateGEP(array_type, local_variable, {builder.getInt32(0), idx});
                        builder.CreateStore(elements[i], ptr);
                    }
                    for (size_t j = elements.size(); j < array_length; ++j) {
                        llvm::Value* idx = builder.getInt32(j);
                        llvm::Value* ptr = builder.CreateGEP(array_type, local_variable, {builder.getInt32(0), idx});
                        builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), ptr);
                    }
              
              
                }
            }    
            if(!declare_variable(name, local_variable, is_const, true, is_obc, array_length)){
                std::cerr << node.line << ":" << node.pos << ":"
                          << " variable '" << name << "' is declared more than one times" 
                          << std::endl;
                error_flag = true;
                return;
            }
        }
        
    }
}

void SafeCIRBuilder::visit(assign_stmt_node& node) {
    // DONE: get target's rval and store at value's lval.
    auto name = node.target->name;
    VarInfo var_info = lookup_variable(name);
    if (var_info.is_const) {
        std::cerr << node.line << ":" << node.pos
                  << ": assignment of read-only variable '" << name << "'"
                  << std::endl;
        error_flag = true;
        return;
    } else {
        std::cout << "visit(assign_stmt_node& node) 被调用" << std::endl;
        EXPECT_LVAL(node.target->accept(*this));
        llvm::Value* target_lval;
        get_result_as_value(&target_lval);
        EXPECT_RVAL(node.value->accept(*this));
        llvm::Value* value_rval;
        get_result_as_value(&value_rval);
        // 将赋值表达式的右值存储到目标变量的左值
        builder.CreateStore(value_rval, target_lval);
    }
    
}

void SafeCIRBuilder::visit(if_stmt_node& node) {
    std::cout << "visit(if_stmt_node& node) 被调用" << std::endl;
    // DONE: implement if-else statement.

    // 获取当前插入点所在的基本块所属的函数
    llvm::Function* function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* if_bb = llvm::BasicBlock::Create(context, "if_true", function);
    llvm::BasicBlock* else_bb = llvm::BasicBlock::Create(context, "if_false", function);
    llvm::BasicBlock* end_bb = llvm::BasicBlock::Create(context, "if_end", function);
    llvm::Value* cond_value;

    EXPECT_RVAL(node.cond->accept(*this));
    get_result_as_value(&cond_value);
    
    builder.CreateCondBr(cond_value, if_bb, else_bb);

    // 生成if分支时的处理代码
    builder.SetInsertPoint(if_bb);
        node.if_body->accept(*this);
        builder.CreateBr(end_bb);

    // 生成else分支时的处理代码
    builder.SetInsertPoint(else_bb);
        if (node.else_body) {
            node.else_body->accept(*this);
        }
        builder.CreateBr(end_bb);

    builder.SetInsertPoint(end_bb);
    
}

void SafeCIRBuilder::visit(while_stmt_node& node) {
    std::cout << "visit(while_stmt_node& node) 被调用" << std::endl;
    // DONE: implement while statement.

    // 获取当前插入点所在的基本块所属的函数
    llvm::Function* function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(context, "while_cond", function);
    llvm::BasicBlock* loop_bb = llvm::BasicBlock::Create(context, "while_loop", function);
    llvm::BasicBlock* end_bb = llvm::BasicBlock::Create(context, "while_end", function);
    llvm::Value* condValue;

    builder.CreateBr(cond_bb);
    builder.SetInsertPoint(cond_bb);
    EXPECT_RVAL(node.cond->accept(*this));
    get_result_as_value(&condValue);
    builder.CreateCondBr(condValue, loop_bb, end_bb);

    builder.SetInsertPoint(loop_bb);
    node.body->accept(*this);
    builder.CreateBr(cond_bb);

    builder.SetInsertPoint(end_bb);

}

void SafeCIRBuilder::log(std::string info) {
    if (debug) {
        std::cerr << info << std::endl;
    }
};

void SafeCIRBuilder::build(std::string name, std::shared_ptr<ast_node> node) {
    // Initialize environment.
    module = std::make_unique<llvm::Module>(name, context);
    runtime = std::make_unique<runtime_info>(module.get());

    // global
    enter_scope();
    for (auto t : runtime->get_language_symbols()) {
        llvm::GlobalValue* val;
        std::string name;
        bool is_function;
        bool is_const;
        bool is_array;
        bool is_obc;
        int array_length;
        std::tie(name, val, is_function, is_const, is_array, is_obc,
                 array_length) = t;
        if (is_function)
            functions[name] = static_cast<llvm::Function*>(val);
        else
            declare_variable(name, val, is_const, is_array, is_obc,
                             array_length);
    }

    cur_scope = FLAG::GLOBAL_SCOPE;
    error_flag = false;

    // Start building by starting iterate over the syntax tree.
    node->accept(*this);

    // Finish by clear IRBuilder's insertion point and moving away built module.
    builder.ClearInsertionPoint();
    exit_scope();

    if (error_flag) {
        module.release();
        runtime.release();
    }
}

std::unique_ptr<llvm::Module> SafeCIRBuilder::get_module() {
    return std::move(module);
}

std::unique_ptr<runtime_info> SafeCIRBuilder::get_runtime_info() {
    return std::move(runtime);
}

void SafeCIRBuilder::enter_scope() {
    scoped_variables.emplace_back();
    scoped_variables.back().scope_type = cur_scope;
    cur_scope = FLAG::BLOCK_SCOPE;
}

void SafeCIRBuilder::exit_scope() {
    scoped_variables.pop_back();
    cur_scope = scoped_variables.back().scope_type;
}

SafeCIRBuilder::VarInfo SafeCIRBuilder::lookup_variable(std::string name) {
    // DONE: find the nearest decalred variable `name`

    // 从最近的作用域开始遍历查找变量
    for (auto scope = scoped_variables.rbegin(); scope != scoped_variables.rend(); ++scope) {
        int if_found = scope->variable_map.count(name);
        if (if_found) {

            return scope->variable_map[name]; // 找到变量，返回其名称
        }
    }
    return VarInfo(); // Return an invalid VarInfo if `name` not found.
}

bool SafeCIRBuilder::declare_variable(std::string name, llvm::Value* var_ptr,
                                      bool is_const, bool is_array, bool is_obc,
                                      int array_length) {
    if (scoped_variables.back().variable_map.count(name))
        return false;
    scoped_variables.back().variable_map[name] = 
        VarInfo(var_ptr, is_const, is_array, is_obc, array_length);
    return true;
}

void SafeCIRBuilder::set_int_result(int val) {
    has_int_result = true;
    int_result = val;
}

bool SafeCIRBuilder::get_int_result(int &val) {
    if (has_int_result) {
        val = int_result;
        has_int_result = false;
        return true;
    } else {
        return false;
    }
}

void SafeCIRBuilder::set_value_result(llvm::Value* val) {
    has_value_result = true;
    value_result = val;
}

bool SafeCIRBuilder::get_value_result(llvm::Value** val) {
    if (has_value_result) {
        *val = value_result;
        has_value_result = false;
        return true;
    } else {
        return false;
    }
}

bool SafeCIRBuilder::get_result_as_value(llvm::Value** val) {
    if (has_value_result) {
        *val = value_result;
        has_value_result = false;
        return true;
    } else if (has_int_result) {
        *val = builder.getInt32(int_result);
        has_int_result = false;
        return true;
    } else {
        return false;
    }
}
