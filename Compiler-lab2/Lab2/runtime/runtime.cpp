#include <iostream>
#include "runtime.h"
#include "io.h"

#include <llvm/IR/Type.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>

using namespace std;
using namespace llvm;

runtime_info::runtime_info(Module *module)
{
    input_var = new GlobalVariable(*module,
                                   Type::getInt32Ty(module->getContext()),
                                   false,
                                   GlobalValue::ExternalLinkage,
                                   ConstantInt::get(Type::getInt32Ty(module->getContext()), 0),
                                   "input_var");

    output_var = new GlobalVariable(*module,
                                    Type::getInt32Ty(module->getContext()),
                                    false,
                                    GlobalValue::ExternalLinkage,
                                    ConstantInt::get(Type::getInt32Ty(module->getContext()), 0),
                                    "output_var");

    arg0 = new GlobalVariable(*module,
                              Type::getInt32Ty(module->getContext()),
                              false,
                              GlobalValue::ExternalLinkage,
                              ConstantInt::get(Type::getInt32Ty(module->getContext()), 0),
                              "arg0");
    arg1 = new GlobalVariable(*module,
                             Type::getInt32Ty(module->getContext()),
                             false,
                             GlobalValue::ExternalLinkage,
                             ConstantInt::get(Type::getInt32Ty(module->getContext()), 0),
                             "arg1");

    Constant *safec = ConstantDataArray::getString(module->getContext(),"SafeC IR Builder", true);
    arg2 = new GlobalVariable(*module,
                             safec->getType(),
                             false,
                             GlobalValue::ExternalLinkage,
                             safec,
                             "arg2");

    auto input_impl = Function::Create(FunctionType::get(Type::getVoidTy(module->getContext()),
                                                         {Type::getInt32PtrTy(module->getContext())},
                                                         false),
                                       GlobalValue::LinkageTypes::ExternalLinkage,
                                       "input_impl",
                                       module);
    auto output_impl = Function::Create(FunctionType::get(Type::getVoidTy(module->getContext()),
                                                          {Type::getInt32PtrTy(module->getContext())},
                                                          false),
                                        GlobalValue::LinkageTypes::ExternalLinkage,
                                        "output_impl",
                                        module);
    auto obc_check_error_impl = Function::Create(FunctionType::get(Type::getVoidTy(module->getContext()),
                                                                   {Type::getInt32PtrTy(module->getContext()), Type::getInt32PtrTy(module->getContext())},
                                                                   false),
                                                 GlobalValue::LinkageTypes::ExternalLinkage,
                                                 "obc_check_error_impl",
                                                 module);

    IRBuilder<> builder(module->getContext());

    input_func = Function::Create(FunctionType::get(Type::getVoidTy(module->getContext()), {}, false),
                                  GlobalValue::LinkageTypes::ExternalLinkage,
                                  "input",
                                  module);
    builder.SetInsertPoint(BasicBlock::Create(module->getContext(), "entry", input_func));
    builder.CreateCall(input_impl, {input_var});
    builder.CreateRetVoid();

    output_func = Function::Create(FunctionType::get(Type::getVoidTy(module->getContext()), {}, false),
                                   GlobalValue::LinkageTypes::ExternalLinkage,
                                   "output",
                                   module);
    builder.SetInsertPoint(BasicBlock::Create(module->getContext(), "entry", output_func));
    builder.CreateCall(output_impl, {output_var});
    builder.CreateRetVoid();

    obc_check_error_func = Function::Create(FunctionType::get(Type::getVoidTy(module->getContext()), {}, false),
                                            GlobalValue::LinkageTypes::ExternalLinkage,
                                            "obc_check_error",
                                            module);
    builder.SetInsertPoint(BasicBlock::Create(module->getContext(), "entry", obc_check_error_func));
    builder.CreateCall(obc_check_error_impl, {arg0, arg1, arg2});
    builder.CreateRetVoid();
}

using namespace string_literals;

vector<tuple<string, llvm::GlobalValue *, bool, bool, bool, bool, int>> runtime_info::get_language_symbols()
{
    return {
        make_tuple("input_var"s, input_var, false, false, false, false, 0),
        make_tuple("output_var"s, output_var, false, false, false, false, 0),
        make_tuple("arg0"s, arg0, false, false, false, false, 0),
        make_tuple("arg1"s, arg1, false, false, false, false, 0),
        make_tuple("arg2"s, arg2, false, false, false, false, 0),
        make_tuple("input"s, input_func, true, false, false, false, 0),
        make_tuple("output"s, output_func, true, false, false, false, 0),
        make_tuple("obc_check_error"s, obc_check_error_func, true, false, false, false, 0)};
}

vector<tuple<string, void *>> runtime_info::get_runtime_symbols()
{
    return {
        make_tuple("input_impl"s, (void *)&::input),
        make_tuple("output_impl"s, (void *)&::output),
        make_tuple("obc_check_error_impl"s, (void *)&::obc_check_error)};
}
