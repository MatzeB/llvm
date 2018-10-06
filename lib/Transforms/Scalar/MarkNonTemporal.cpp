#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"
#include "llvm/Transforms/Scalar.h"

using namespace llvm;

enum MarkNonTemporalStyle {
  Disable,
  InnerLoopsLoopCarried
};

static cl::opt<MarkNonTemporalStyle> Style("mark-nontemporal", cl::Hidden,
  cl::desc("Choose how to mark nontemporal stores"),
  cl::values(clEnumVal(Disable, "do nothing"),
             clEnumVal(InnerLoopsLoopCarried,
                       "inner loops for stores with loop carried addresses")),
  cl::init(InnerLoopsLoopCarried)
);

namespace {
class MarkNonTemporal : public FunctionPass {
public:
  static char ID;
  MarkNonTemporal() : FunctionPass(ID) {
    initializeMarkNonTemporalPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    FunctionPass::getAnalysisUsage(AU);
    AU.addRequired<LoopInfoWrapperPass>();
    AU.setPreservesCFG();
  }

  bool runOnFunction(Function &F) override;

private:
  void processLoop(Loop &L);
  bool isLoopCarried(const Value *V);

  const LoopInfo *LI;
  MDNode *One;
  SmallPtrSet<const Instruction*, 32> LoopCarriedAddresses;
  SmallPtrSet<const Instruction*, 32> NonLoopCarriedAddresses;
};

}

char MarkNonTemporal::ID = 0;
INITIALIZE_PASS_BEGIN(MarkNonTemporal, "mark-non-temporal",
                      "Mark loop stores as non temporal", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(MarkNonTemporal, "mark-non-temporal",
                    "Mark loop stores as non temporal", false, false)

FunctionPass *llvm::createMarkNonTemporalPass() {
  return new MarkNonTemporal();
}

// Check whether the address is calculated based on a loop phi value...
bool MarkNonTemporal::isLoopCarried(const Value *V) {
  const Instruction *I = dyn_cast<Instruction>(V);
  if (!I)
    return false;

  if (NonLoopCarriedAddresses.count(I))
    return false;
  if (LoopCarriedAddresses.count(I))
    return true;

  bool Result;
  if (const PHINode *P = dyn_cast<PHINode>(I)) {
    const BasicBlock *BB = P->getParent();
    if (LI->isLoopHeader(BB)) {
      LoopCarriedAddresses.insert(I);
      return true;
    }

    // Add into sets to break loops.
    NonLoopCarriedAddresses.insert(I);
    // Check if any of the operands is loop carried.
    Result = llvm::any_of(P->incoming_values(), [&](const Value *V) {
      return isLoopCarried(V);
    });
    NonLoopCarriedAddresses.erase(I);
  } else if (const GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I)) {
    Result = isLoopCarried(GEP->getPointerOperand()) ||
      llvm::any_of(GEP->indices(), [&](const Value *V) {
        return isLoopCarried(V);
      });
  } else if (I->isCast()) {
    Result = isLoopCarried(I->getOperand(0));
  } else if (I->isBinaryOp()) {
    Result = isLoopCarried(I->getOperand(0)) || isLoopCarried(I->getOperand(1));
  } else {
    return false;
  }

  if (Result) {
    LoopCarriedAddresses.insert(I);
  } else {
    NonLoopCarriedAddresses.insert(I);
  }
  return Result;
}

void MarkNonTemporal::processLoop(Loop &L) {
  if (L.empty()) {
    // For innermost loops mark stores as nontemporal.
    for (BasicBlock *BB : L.blocks()) {
      for (Instruction &I : *BB) {
        if (StoreInst *S = dyn_cast<StoreInst>(&I)) {
          if (!S->getMetadata(LLVMContext::MD_nontemporal) &&
              isLoopCarried(S->getPointerOperand()))
            S->setMetadata(LLVMContext::MD_nontemporal, One);
        }
      }
    }
  } else {
    for (Loop *ChildL : L) {
      processLoop(*ChildL);
    }
  }
}

bool MarkNonTemporal::runOnFunction(Function &F) {
  if (Style == Disable)
    return false;
  if (skipFunction(F))
    return false;

  LoopInfoWrapperPass &LIW = getAnalysis<LoopInfoWrapperPass>();
  LI = &LIW.getLoopInfo();
  LLVMContext &C = F.getContext();
  One = MDNode::get(C, ConstantAsMetadata::get(ConstantInt::get(
                                               Type::getInt32Ty(C), 1)));
  LoopCarriedAddresses.clear();
  NonLoopCarriedAddresses.clear();

  for (Loop *L : *LI) {
    processLoop(*L);
  }

  return true;
}
