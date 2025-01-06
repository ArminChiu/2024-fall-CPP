#ifndef _DATAFLOW_H_
#define _DATAFLOW_H_

#include <set>
#include <map>
#include <utility>
#include <iostream>

#include <llvm/IR/Module.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Support/raw_ostream.h>


// 指令 -> <输入状态, 输出状态>
template <class T>
struct DataflowInstResult
{
    // 类型别名，用于表示从指令到输入和输出状态的映射
	typedef typename std::map<llvm::Instruction *, std::pair<T, T>> Type;
};

// 提供详细结果集的虚拟类
// 对于每个基本块，我们计算其输入数据流值和输出数据流值
// 基本块 -> <输入状态, 输出状态>
template<class T>
struct DataflowBBResult {
    // 类型别名，用于表示从基本块到输入和输出状态的映射
    typedef typename std::map<llvm::BasicBlock *, std::pair<T, T>> Type;
};

// 基础数据流访问者类，定义数据流函数
template <class T>
class DataflowVisitor {
public:
    llvm::Module *module; // 指向LLVM模块的指针

    virtual ~DataflowVisitor() {}

    /*
        为每个指令调用的数据流函数
        @inst 指令
        @dfval 输入数据流值
        @return 如果dfval更改，则为true
    */
    virtual void compDFVal(llvm::Instruction *inst, T *state) = 0;

    /* 
        为每个基本块调用的数据流函数
        @block 基本块
        @dfval 输入数据流值
        @isforward true表示向前计算dfval，否则向后
    */
    virtual void compDFVal(llvm::BasicBlock *block, T *state, bool isforward)
    {
        if (isforward == true) { // 向前计算
            if (block->getName() == "entry" && block->getParent()->getName() == "main") { // 如果是main函数的入口块
                initGlobal(state); // 初始化全局变量
            } 
            for (llvm::BasicBlock::iterator ii=block->begin(), ie=block->end(); ii!=ie; ++ii) { // 遍历基本块的每个指令
                llvm::Instruction * inst = &*ii; // 获取指令
                compDFVal(inst, state); // 计算数据流值
            }
        } else { // 向后计算
            for (llvm::BasicBlock::reverse_iterator ii=block->rbegin(), ie=block->rend(); ii != ie; ++ii) { // 逆序遍历基本块的每个指令
                llvm::Instruction * inst = &*ii; // 获取指令
                compDFVal(inst, state); // 计算数据流值
            }
        }
    }

    /*
        合并两个dfvals，dest将是合并的结果
        @return 如果dest更改，则为true
    */
    virtual void merge( T *dest, T *src )
    {}

    virtual void initGlobal(T *state)
    {}
};

/*
    使用用户提供的访问者函数计算前向迭代固定点数据流函数。
    请注意，调用者必须确保该函数实际上是单调函数，否则固定点可能不会终止。
    @param fn 函数
    @param visitor 用于计算数据流值的函数
    @param result 数据流的结果
    @param initval 初始数据流值
*/
template<class T>
void compForwardDataflow(llvm::Function *fn, DataflowVisitor<T> *visitor, typename DataflowBBResult<T>::Type *result, const T & initval) {
    // DONE: Implement forward dataflow analysis framework.
    std::vector<llvm::BasicBlock *> worklist; // 工作列表

    // 初始化工作列表
    for (llvm::BasicBlock &bb : *fn) { // 遍历函数的每个基本块
        if (bb.getName() == "entry" && bb.getParent()->getName() == "main") { // 如果是main函数的入口块
            (*result)[&bb].first = initval;
        } else {
            visitor->initGlobal(&(*result)[&bb].first);
        }
        visitor->initGlobal(&(*result)[&bb].second);
        worklist.push_back(&bb); // 将基本块加入工作列表
    }

    // 迭代计算数据流值
    while (!worklist.empty()) { // 当工作列表不为空
        llvm::BasicBlock *bb = worklist.back(); // 获取工作列表的最后一个基本块
        worklist.pop_back(); // 弹出最后一个基本块
        
        T in, out;
        visitor->initGlobal(&in);
        if (bb->getName() == "entry" && bb->getParent()->getName() == "main") { // 如果是main函数的入口块
            in = initval;
        } else {
            for (llvm::pred_iterator pi = llvm::pred_begin(bb), pe = llvm::pred_end(bb); pi != pe; ++pi) { // 遍历基本块的前驱
                llvm::BasicBlock *pred = *pi; // 获取前驱
                visitor->merge(&in, &(*result)[pred].second); // 合并前驱的输出数据流值
            }
        }
        (*result)[bb].first = in; // 设置基本块的输入数据流值

        // 计算基本块的输出数据流值
        if (bb->getName().startswith("obc_err_")) { // 如果是obc_err_开头的基本块
            (*result)[bb].second = in; // 输出数据流值等于输入数据流值
        } else {
            visitor->compDFVal(bb, &out, true); // 计算数据流值
            if (out != (*result)[bb].second) { // 如果输出数据流值发生变化
                (*result)[bb].second = out; // 更新输出数据流值
                for (llvm::succ_iterator si = llvm::succ_begin(bb), se = llvm::succ_end(bb); si != se; ++si) { // 遍历基本块的后继
                    llvm::BasicBlock *succ = *si; // 获取后继
                    worklist.push_back(succ); // 将后继加入工作列表
                }
            }
        }
    }
}

template<class T>
void compBackwardDataflow(llvm::Function *fn,DataflowVisitor<T> *visitor, typename DataflowBBResult<T>::Type *result, const T &initval)
{}

#endif