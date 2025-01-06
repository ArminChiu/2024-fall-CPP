#ifndef _CONSTANTPROPAGATOR_H_
#define _CONSTANTPROPAGATOR_H_

#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Support/raw_ostream.h>

#include "Dataflow.h"

struct KSet {
    static int kset_size;

    bool top = false; // is TOP set.
    std::set<int> const_vals; // constant values in the set.

    KSet() {};
    KSet(int val) { const_vals.insert(val); };
    KSet(KSet& kset) {
        kset.update_top_status();
        top = kset.top;
        const_vals = kset.const_vals;
    };

    void update_top_status() {
        if (const_vals.size() > kset_size) {
            top = true;
        }
    }

    bool operator == (const KSet & other) const {
        // DONE: Overload operator == . (Reference: https://www.runoob.com/cplusplus/cpp-overloading.html)
        // 如果两个集合不都是TOP，则不相等
        if (top != other.top) {
            return false;
        }
        // 如果两个集合都是TOP，则相等
        if (top && other.top) {
            return true;
        }
        // 如果两个集合都不是TOP，则比较const_vals
        return const_vals == other.const_vals;
    }

    bool operator != (const KSet & other) const {
        return !operator==(other);
    }

    std::string toString() {
        std::string res = "";
        if (top) {
            res = "TOP";
        } else {
            for (auto val : const_vals) {
                res += std::to_string(val) + " ";
            }
        }
        return res;
    }

    // TODO: add other variables or methods if you want to.
};

typedef std::unordered_map<llvm::Value *, KSet *> ConstValueMap;
typedef std::unordered_set<llvm::Function *> FunctionSet;

struct ConstValueState {
    ConstValueMap cvmap; // map: value -> kset

    ConstValueState() {};

    ConstValueState(const ConstValueState& state) {
        for (auto it = state.cvmap.begin(); it != state.cvmap.end(); it++) {
            cvmap[it->first] = new KSet(*(it->second));
        }
    }

    bool operator == (const ConstValueState & other) const {
        // Overload operator == . (Reference: https://www.runoob.com/cplusplus/cpp-overloading.html)
        if (cvmap.size() != other.cvmap.size()) {
            return false;
        }
        for (auto it = cvmap.begin(); it != cvmap.end(); it++) {
            auto res = other.cvmap.find(it->first);
            if (res == other.cvmap.end()) {
                return false;
            }
            if (*(it->second) != *((*res).second)) {
                return false;
            }
        }
        return true;
    }

    bool operator != (const ConstValueState & other) const {
        return !operator==(other);
    }

    // Assignment operator: deep copy.
    ConstValueState &operator = (const ConstValueState & other) {
        cvmap.clear();
        for (auto it = other.cvmap.begin(); it != other.cvmap.end(); it++) {
            cvmap[it->first] = new KSet(*(it->second));
        }
        return *this;
    }

    // TODO: add other variables or methods if you want to.
};


inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out, const ConstValueMap &cv);
inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out, const ConstValueState &state);

class ConstantPropagatorVisitor : public DataflowVisitor<struct ConstValueState> {
public:
    // GLOBAL VALUE
    std::unordered_set<llvm::Value *> global_variable;
    // result[bb] -> (in state, out state)
    DataflowBBResult<ConstValueState>::Type* result;
    // TODO: add members
    llvm::Module* M;
    ConstantPropagatorVisitor();
    ConstantPropagatorVisitor(llvm::Module *module, int kset_size,
                                DataflowBBResult<ConstValueState>::Type* res);
    ~ConstantPropagatorVisitor() {};

    // Instructions are in the <llvm/IR/Instructions.h>
    void compDFVal(llvm::Instruction *inst, ConstValueState * state) override;

    // merge(dest, src) => dest
    void merge(ConstValueState * dest, ConstValueState * src) override;

    void initGlobal(ConstValueState *state) override;

    // TODO: handle some insts
    // e.g. void handleStoreInst(llvm::StoreInst* inst, ConstValueState* state);
};

class ConstValuePass : public llvm::ModulePass {
private:
    // map: basicblock->[in_state, out_state]
    DataflowBBResult<ConstValueState>::Type result;
    // obc check record
    std::vector<llvm::BasicBlock *> check_redundant;
    // The size of KSet
    int kset_size = 16;

public:
    static char ID;
    ConstValuePass(std::vector<llvm::BasicBlock *> obc_check_record, int kset_size);

    bool runOnModule(llvm::Module &M) override;

    void removeRedundant();
};

#endif