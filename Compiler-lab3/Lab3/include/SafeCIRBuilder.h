#ifndef _SAFEC_IR_BUILDER_
#define _SAFEC_IR_BUILDER_

#include <deque>
#include <iostream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <stack>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include "AstNode.h"
#include "AstNode_Visitor.h"
#include "runtime.h"


class SafeCIRBuilder : public AstNode_Visitor {
   public:

    llvm::LLVMContext& context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<runtime_info> runtime;
    std::vector<llvm::BasicBlock *> obc_check_record;

    // value_result and int_result are used for saving visiting result.
    llvm::Value* value_result;
    int int_result;
    bool has_int_result = false;
    bool has_value_result = false;

    // methods for managing int/value result.
    void set_value_result(llvm::Value* val);
    void set_int_result(int val);
    bool get_int_result(int &val);
    bool get_value_result(llvm::Value** val);
    bool get_result_as_value(llvm::Value** val);

    llvm::Function* current_function;
    int bb_count;

    enum FLAG {
        // For identifying var type.
        LVAL=0,
        RVAL,

        // For identifying scope.
        GLOBAL_SCOPE,
        BLOCK_SCOPE
    };

    FLAG cur_scope = FLAG::GLOBAL_SCOPE;
    std::stack<FLAG> val_type_stack;
    bool error_flag;

    bool debug = false;

    SafeCIRBuilder(llvm::LLVMContext& ctx);
    ~SafeCIRBuilder();

    void obc_check(llvm::Value* index, int array_length, int node_line,
                   int node_pos, std::string name);

    void visit(comp_unit_node& node);
    void visit(comp_unit_child_node& node);
    void visit(global_def_node& node);
    void visit(func_def_node& node);
    void visit(block_node& node);
    void visit(stmt_node& node);
    void visit(func_call_stmt_node& node);
    void visit(empty_stmt_node& node);
    void visit(expr_node& node);
    void visit(cond_node& node);
    void visit(number_node& node);
    void visit(binop_expr_node& node);
    void visit(unaryop_expr_node& node);
    void visit(lval_node& node);
    void visit(var_def_stmt_node& node);
    void visit(var_def_node& node);
    void visit(assign_stmt_node& node);
    void visit(if_stmt_node& node);
    void visit(while_stmt_node& node);

    void log(std::string info);

    void build(std::string name, std::shared_ptr<ast_node> node);

    std::unique_ptr<llvm::Module> get_module();
    std::unique_ptr<runtime_info> get_runtime_info();

   private:

    struct VarInfo {
        llvm::Value* val_ptr;
        bool is_const;
        bool is_array;
        bool is_obc;
        int array_length;
        VarInfo(llvm::Value* val_ptr, bool is_const, bool is_array, bool is_obc, int array_length)
            : val_ptr(val_ptr), is_const(is_const), is_array(is_array), is_obc(is_obc),
              array_length(array_length) {};
        VarInfo() : val_ptr(nullptr), is_const(false), is_array(false), is_obc(false),
                    array_length(0) {};
        bool is_valid() { return val_ptr != nullptr; };
    };

    struct StackElement {
        FLAG scope_type;
        std::unordered_map<std::string, VarInfo> variable_map;
    };

    void enter_scope();
    void exit_scope();

    // Find the variable by its name, the variable in the nearest scope will be returned.
    VarInfo lookup_variable(std::string name);

    bool declare_variable(std::string name, llvm::Value* var_ptr, bool is_const,
                          bool is_array, bool is_obc, int array_length);

    // [variables in global scope, ...]
    std::vector<StackElement> scoped_variables;

    // Function Name : llvm::Function*
    std::unordered_map<std::string, llvm::Function*> functions;
};

#endif