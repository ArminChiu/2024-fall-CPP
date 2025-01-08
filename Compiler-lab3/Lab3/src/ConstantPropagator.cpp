#include "ConstantPropagator.h"

char ConstValuePass::ID = 0;
int KSet::kset_size = 16;

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out, const ConstValueMap &cv) {
    // DONE: Implement if you want to print ConstValueMap with something like: llvm::outs() << cv;
    // 打印ConstValueMap
    out << "ConstValueMap:\n";
    for (auto it = cv.begin(); it != cv.end(); it++) { // 遍历cv
        out << *(it->first) << " : " << it->second->toString() << "\n"; // 打印key和value
    }
    return out;
}

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out, const ConstValueState &state) {
    // DONE: Implement if you want to print ConstValueState with something like: llvm::outs() << state;
    // 打印ConstValueState
    out << "ConstValueState:\n";
    out << state.cvmap; // 打印cvmap
    return out;
}

ConstantPropagatorVisitor::ConstantPropagatorVisitor() { }

ConstantPropagatorVisitor::ConstantPropagatorVisitor(llvm::Module *module, int kset_size, DataflowBBResult<ConstValueState>::Type* res) : M(module), result(res) {
    // for all global variables
    KSet::kset_size = kset_size;
    for (auto it = module->global_begin(); it != module->global_end(); it++) {
        global_variable.insert(&(*it));
    }
}

void ConstantPropagatorVisitor::compDFVal(llvm::Instruction* inst, ConstValueState* state) {
    // TODO: hanldes different kinds of instructions
    // 处理不同类型的指令
    if (llvm::BinaryOperator *binOp = llvm::dyn_cast<llvm::BinaryOperator>(inst)){ // 如果是二元操作符
        llvm::Value* op1 = binOp->getOperand(0); // 操作数1
        llvm::Value* op2 = binOp->getOperand(1); // 操作数2
        KSet* op1_kset = new KSet(); // 操作数1的KSet
        KSet* op2_kset = new KSet(); // 操作数2的KSet
        KSet* res_kset = new KSet(); // 结果的KSet

        if (llvm::ConstantInt *op1_const = llvm::dyn_cast<llvm::ConstantInt>(op1)) { // 如果操作数1是常数
            op1_kset->const_vals.insert(op1_const->getSExtValue()); // 将常数加入KSet
        } else if (state->cvmap.find(op1) != state->cvmap.end()) { // 如果操作数1在cvmap中
            op1_kset = state->cvmap[op1]; // 获取操作数1的KSet
        }

        if (llvm::ConstantInt *op2_const = llvm::dyn_cast<llvm::ConstantInt>(op2)) { // 如果操作数2是常数
            op2_kset->const_vals.insert(op2_const->getSExtValue()); // 将常数加入KSet
        } else if (state->cvmap.find(op2) != state->cvmap.end()) { // 如果操作数2在cvmap中
            op2_kset = state->cvmap[op2]; // 获取操作数2的KSet
        }
        
        //if (op1_kset->top || op2_kset->top || op1_kset->const_vals.size() == 0 || op2_kset->const_vals.size() == 0) { 
        // 如果操作数1或操作数2是top或者操作数1或操作数2是空
        if (op1_kset->top || op2_kset->top) { // 如果操作数1或操作数2是top
            res_kset->top = true; // 结果是top
        } else { // 否则计算结果
            for (auto it1 = op1_kset->const_vals.begin(); it1 != op1_kset->const_vals.end(); it1++) { // 遍历操作数1的KSet
                for (auto it2 = op2_kset->const_vals.begin(); it2 != op2_kset->const_vals.end(); it2++) { // 遍历操作数2的KSet
                    int res = 0; // 结果
                    int flag = 0; // 标志
                    switch (binOp->getOpcode()) { // 根据操作符计算结果
                        case llvm::Instruction::Add:
                            res = *it1 + *it2;
                            flag = 1;
                            break;
                        case llvm::Instruction::Sub:
                            res = *it1 - *it2;
                            flag = 1;
                            break;
                        case llvm::Instruction::Mul:
                            res = *it1 * *it2;
                            flag = 1;
                            break;
                        case llvm::Instruction::SDiv:
                            if (*it2 != 0) { // 除数不为0
                                res = *it1 / *it2;
                                flag = 1;
                                break;
                            } else { // 除数为0
                                res_kset->top = true; // 结果是top
                                break;
                            }
                        case llvm::Instruction::Or:
                            res = *it1 | *it2;
                            flag = 1;
                            break;
                        default:
                            res_kset->top = true; // 结果是top
                            break;
                    }
                    if (flag) { // 如果计算成功
                        res_kset->const_vals.insert(res); // 将结果加入KSet
                    }
                }
            }
        }
        state->cvmap[inst] = res_kset; // 将结果加入cvmap
    } else if (llvm::ICmpInst *icmp = llvm::dyn_cast<llvm::ICmpInst>(inst)) { // 如果是比较指令
        llvm::Value* op1 = icmp->getOperand(0); // 操作数1
        llvm::Value* op2 = icmp->getOperand(1); // 操作数2
        KSet* op1_kset = new KSet(); // 操作数1的KSet
        KSet* op2_kset = new KSet(); // 操作数2的KSet
        KSet* res_kset = new KSet(); // 结果的KSet
        res_kset->top = false;

        if (llvm::ConstantInt *op1_const = llvm::dyn_cast<llvm::ConstantInt>(op1)) { // 如果操作数1是常数
            op1_kset->const_vals.insert(op1_const->getSExtValue()); // 将常数加入KSet
        } else if (state->cvmap.find(op1) != state->cvmap.end()) { // 如果操作数1在cvmap中
            op1_kset = state->cvmap[op1]; // 获取操作数1的KSet
        }

        if (llvm::ConstantInt *op2_const = llvm::dyn_cast<llvm::ConstantInt>(op2)) { // 如果操作数2是常数
            op2_kset->const_vals.insert(op2_const->getSExtValue()); // 将常数加入KSet
        } else if (state->cvmap.find(op2) != state->cvmap.end()) { // 如果操作数2在cvmap中
            op2_kset = state->cvmap[op2]; // 获取操作数2的KSet
        }

        if (op1_kset->top || op2_kset->top) { // 如果操作数1或操作数2是top
            res_kset->top = true; // 结果是top
        } else { // 否则计算结果
            for (auto it1 = op1_kset->const_vals.begin(); it1 != op1_kset->const_vals.end(); it1++) { // 遍历操作数1的KSet
                for (auto it2 = op2_kset->const_vals.begin(); it2 != op2_kset->const_vals.end(); it2++) { // 遍历操作数2的KSet
                    bool res = false; // 结果
                    switch (icmp->getPredicate()) { // 根据比较符计算结果
                        case llvm::CmpInst::ICMP_EQ:
                            res = *it1 == *it2;
                            break;
                        case llvm::CmpInst::ICMP_NE:
                            res = *it1 != *it2;
                            break;
                        case llvm::CmpInst::ICMP_SGT:
                            res = *it1 > *it2;
                            break;
                        case llvm::CmpInst::ICMP_SGE:
                            res = *it1 >= *it2;
                            break;
                        case llvm::CmpInst::ICMP_SLT:
                            res = *it1 < *it2;
                            break;
                        case llvm::CmpInst::ICMP_SLE:
                            res = *it1 <= *it2;
                            break;
                        default:
                            res_kset->top = true; // 结果是top
                            break;
                    }
                    if (res) { // 如果计算成功
                        res_kset->const_vals.insert(1); // 将结果加入KSet
                    } else {
                        res_kset->const_vals.insert(0); // 将结果加入KSet
                    }
                }
            }
        }
        state->cvmap[inst] = res_kset; // 将结果加入cvmap
    } else if (llvm::LoadInst *load = llvm::dyn_cast<llvm::LoadInst>(inst)) { // 如果是load指令
        llvm::Value* ptr = load->getPointerOperand(); // 指针
        KSet* ptr_kset = new KSet(); // 指针的KSet
        KSet* res_kset = new KSet(); // 结果的KSet

        if (state->cvmap.find(ptr) != state->cvmap.end()) { // 如果指针在cvmap中
            ptr_kset = state->cvmap[ptr]; // 获取指针的KSet
            res_kset->top = ptr_kset->top; // 结果的top与指针的top相同
            res_kset->const_vals = ptr_kset->const_vals; // 结果的常数与指针的常数相同
        } else if (global_variable.find(ptr) != global_variable.end()) { // 如果指针是全局变量
            llvm::GlobalVariable* global_var = llvm::dyn_cast<llvm::GlobalVariable>(ptr); // 获取全局变量
            llvm::ConstantInt* global_val = llvm::dyn_cast<llvm::ConstantInt>(global_var->getInitializer()); // 获取全局变量的值
            res_kset->const_vals.insert(global_val->getSExtValue()); // 将全局变量的值加入KSet
        } else { // 否则结果是top
            res_kset->top = true;
        }
        state->cvmap[inst] = res_kset; // 将结果加入cvmap
    } else if (llvm::StoreInst *store = llvm::dyn_cast<llvm::StoreInst>(inst)) { // 如果是store指令
        llvm::Value* ptr = store->getPointerOperand(); // 指针
        llvm::Value* val = store->getValueOperand(); // 值
        KSet* ptr_kset = nullptr; // 指针的KSet
        KSet* val_kset = nullptr; // 值的KSet

        if (state->cvmap.find(ptr) != state->cvmap.end()) { // 如果指针在cvmap中
            ptr_kset = state->cvmap[ptr]; // 获取指针的KSet
        } else { // 否则初始化一个新的KSet
            ptr_kset = new KSet();
            state->cvmap[ptr] = ptr_kset;
        }

        if (llvm::ConstantInt *val_const = llvm::dyn_cast<llvm::ConstantInt>(val)) { // 如果值是常数
            ptr_kset->top = false; // 指针的KSet不是top
            ptr_kset->const_vals.clear(); // 清空指针的KSet
            ptr_kset->const_vals.insert(val_const->getSExtValue()); // 将值加入KSet
        } else if (state->cvmap.find(val) != state->cvmap.end()) { // 如果值在cvmap中
            val_kset = state->cvmap[val]; // 获取值的KSet
            ptr_kset->top = val_kset->top; // 指针的KSet的top与值的KSet的top相同
            ptr_kset->const_vals = val_kset->const_vals; // 指针的KSet的常数与值的KSet的常数相同
        } else { // 否则指针的KSet是top
            ptr_kset->top = true;
            ptr_kset->const_vals.clear();
        }
    } else if (llvm::GetElementPtrInst *gep = llvm::dyn_cast<llvm::GetElementPtrInst>(inst)) { // 如果是GetElementPtr指令
        llvm::Value* idx = gep->getOperand(2); // 索引
        llvm::Value* ptr = gep->getPointerOperand(); // 指针
        KSet* idx_kset = new KSet(); // 索引的KSet
        KSet* ptr_kset = new KSet(); // 指针的KSet
        KSet* res_kset = new KSet(); // 结果的KSet

        if (llvm::ConstantInt *idx_const = llvm::dyn_cast<llvm::ConstantInt>(idx)) { // 如果索引是常数
            idx_kset->const_vals.insert(idx_const->getSExtValue()); // 将索引加入KSet
        } else if (state->cvmap.find(idx) != state->cvmap.end()) { // 如果索引在cvmap中
            idx_kset = state->cvmap[idx]; // 获取索引的KSet
        }

        if (llvm::ConstantInt *ptr_const = llvm::dyn_cast<llvm::ConstantInt>(ptr)) { // 如果指针是常数
            ptr_kset->const_vals.insert(ptr_const->getSExtValue()); // 将指针加入KSet
        } else if (state->cvmap.find(ptr) != state->cvmap.end()) { // 如果指针在cvmap中
            ptr_kset = state->cvmap[ptr]; // 获取指针的KSet
        }

        if (idx_kset->top || ptr_kset->top) { // 如果索引或指针是top
            res_kset->top = true; // 结果是top
            state->cvmap[inst] = res_kset; // 将结果加入cvmap
        } else { // 否则
            res_kset->top = false; // 结果不是top
            if (ptr_kset->const_vals.size() == 0) { // 如果指针的KSet是空
                res_kset->const_vals = idx_kset->const_vals; // 结果的KSet等于索引的KSet
            } else { // 否则
                res_kset->const_vals = ptr_kset->const_vals; // 结果的KSet等于指针的KSet
            }
            state->cvmap[inst] = res_kset; // 将结果加入cvmap
        }
    }
}

void ConstantPropagatorVisitor::merge(ConstValueState *dest, ConstValueState *src) {
    // DONE
    // 合并两个ConstValueState
    // 遍历src的cvmap
    for (auto it = src->cvmap.begin(); it != src->cvmap.end(); it++) {
        llvm::Value* key = it->first; // key
        KSet* value = it->second; // value
        if (dest->cvmap.find(key) == dest->cvmap.end()) { // 如果key不在dest的cvmap中
            if (value->top) { // 如果value是top
                dest->cvmap[key] = new KSet(); // 将key加入dest的cvmap
                dest->cvmap[key]->top = true; // key的KSet是top
            } else { // 否则
                dest->cvmap[key] = new KSet(*value); // 将key加入dest的cvmap
            }
        } else { // 否则
            if (dest->cvmap[key]->top || value->top) { // 如果key的KSet或value是top
                dest->cvmap[key]->top = true; // key的KSet是top
                dest->cvmap[key]->const_vals.clear(); // 清空key的KSet
            } else { // 否则
                dest->cvmap[key]->const_vals.insert(value->const_vals.begin(), value->const_vals.end()); // 将value的常数加入key的KSet
            }
        }
    }
}

void ConstantPropagatorVisitor::initGlobal(ConstValueState *state) {
    // DONE
    // 初始化全局变量
    state->cvmap.clear( );
}

ConstValuePass::ConstValuePass(std::vector<llvm::BasicBlock *> obc_check_record, int ksize) : ModulePass(ID) {
    check_redundant = obc_check_record;
    kset_size = ksize;
}

bool ConstValuePass::runOnModule(llvm::Module &M) {
    ConstantPropagatorVisitor visitor(&M, kset_size, &result);

    std::set<llvm::Function *> f_worklist;
    for (auto &F : M) {
        if (F.isIntrinsic())
            continue;
        if (F.getName() == "obc_check_error" ||
            F.getName() == "input" ||
            F.getName() == "output" ||
            F.getName() == "input_impl" ||
            F.getName() == "output_impl")
            continue;
        f_worklist.insert(&F);
    }

    // DONE: Compute dataflow information for each function in f_worklist.
    // 分析每个函数的数据流信息
    for (auto *F : f_worklist) { // 遍历f_worklist
        // 初始化入口状态
        ConstValueState in_state; // 入口状态
        visitor.initGlobal(&in_state); // 初始化全局变量
        // 调用前向分析
        typename DataflowBBResult<ConstValueState>::Type out_state;
        compForwardDataflow(F, &visitor, &out_state, in_state); // 前向分析

        for (auto &BB : *F) { // 存储结果
        result[&BB] = out_state[&BB];
        }

        for (auto &BB : *F) { // 检查冗余
            if (BB.getName() == "obc_check") {
                check_redundant.push_back(&BB);
            }
        }
    }

    removeRedundant();
    return true;
}

void ConstValuePass::removeRedundant() {
    // TODO: You may need to adjust this to fit your implementation.
    llvm::outs() << "============== Remove Start ==============\n";
    for (auto check_bb : check_redundant) {
        auto br_ins = check_bb->getTerminator();
        auto ok_bb = br_ins->getSuccessor(0);
        auto err_bb = br_ins->getSuccessor(1);

        llvm::Instruction* cond_val = llvm::dyn_cast<llvm::Instruction>(br_ins->getOperand(0));
        llvm::Instruction* upper_cmp = llvm::dyn_cast<llvm::Instruction>(cond_val->getOperand(0));
        llvm::Instruction* lower_cmp = llvm::dyn_cast<llvm::Instruction>(cond_val->getOperand(1));
        
        llvm::Value* idx_val = upper_cmp->getOperand(0);
        llvm::Value* uppper_bound = upper_cmp->getOperand(1);
        llvm::Value* lower_bound = lower_cmp->getOperand(1);

        // idx belongs to [lower, upper)
        if (llvm::isa<llvm::ConstantInt>(uppper_bound) && llvm::isa<llvm::ConstantInt>(lower_bound)) {
            int upper = llvm::dyn_cast<llvm::ConstantInt>(uppper_bound)->getSExtValue();
            int lower = llvm::dyn_cast<llvm::ConstantInt>(lower_bound)->getSExtValue();
            llvm::outs() << "idx: " << *idx_val << " in [" << lower << ", " << upper << ")\n";

            if (result.find(check_bb) == result.end()) {
                continue;
            }

            auto bb_res = result[check_bb].second;
            if (bb_res.cvmap.find(idx_val) != bb_res.cvmap.end()) {
                llvm::outs() << "Index val:" << bb_res.cvmap[idx_val]->toString() << "\n";
                bool should_remove = true;
                if (bb_res.cvmap[idx_val]->top) {
                    should_remove = false;
                } else {
                    for (auto val : bb_res.cvmap[idx_val]->const_vals) {
                        if (val < lower || val >= upper) {
                            should_remove = false;
                            break;
                        }
                    }
                }

                if (should_remove) {
                    llvm::outs() << "Removing:" << *check_bb << "\n";
                    bool pre_removed = false;
                    for (auto it = llvm::pred_begin(check_bb), et = llvm::pred_end(check_bb); it != et; ++it) {
                        auto pred = *it;
                        auto last_ins = pred->getTerminator();
                        if(llvm::BranchInst* jmp_ins = llvm::dyn_cast<llvm::BranchInst>(last_ins)) {
                            jmp_ins->setSuccessor(0, ok_bb);
                            pre_removed = true;
                            break;
                        }
                    }

                    assert(pre_removed &&
                        "The last instruction of the previous basic block is not a branch instruction.");

                    check_bb->eraseFromParent();
                    err_bb->eraseFromParent();
                }
            }
        } else {
            assert(0 && "Uppper or lower is not constant value.");
        }
    }
}