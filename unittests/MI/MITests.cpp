#include "MITests.h"
#include "gtest/gtest.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MIRParser/MIRParser.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

static void initLLVM() {
  using namespace llvm;
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  PassRegistry *Registry = PassRegistry::getPassRegistry();
  initializeCore(*Registry);
  initializeCodeGen(*Registry);
}

std::unique_ptr<llvm::TargetMachine>
createTargetMachine(const char *TripleString) {
  Triple TargetTriple(TripleString);
  std::string Error;
  const Target *T = TargetRegistry::lookupTarget("", TargetTriple, Error);
  if (!T) {
    errs() << "Warning: No target available for triple '" << TripleString
           << "'\n";
    return nullptr;
  }

  TargetOptions Options;
  return std::unique_ptr<TargetMachine>(
      T->createTargetMachine(TargetTriple.getTriple(), "", "", Options, None,
                             CodeModel::Default, CodeGenOpt::Aggressive));
}

std::unique_ptr<Module> parseMIR(LLVMContext &Context,
    legacy::PassManagerBase &PM, std::unique_ptr<MIRParser> &MIR,
    const TargetMachine &TM, StringRef MIRCode, const char *FuncName) {
  SMDiagnostic Diagnostic;
  std::unique_ptr<MemoryBuffer> MBuffer = MemoryBuffer::getMemBuffer(MIRCode);
  MIR = createMIRParser(std::move(MBuffer), Context);
  if (!MIR)
    return nullptr;

  std::unique_ptr<Module> M = MIR->parseLLVMModule();
  if (!M)
    return nullptr;

  M->setDataLayout(TM.createDataLayout());

  Function *F = M->getFunction(FuncName);
  if (!F)
    return nullptr;

  MachineModuleInfo *MMI = new MachineModuleInfo(&TM);
  MMI->setMachineFunctionInitializer(MIR.get());
  PM.add(MMI);

  return M;
}

struct TestPass : public MachineFunctionPass {
  static char ID;
  TestPass() : MachineFunctionPass(ID) {
    // We should never call this but always use PM.add(new TestPass(...))
    abort();
  }
  TestPass(TestFunction T) : MachineFunctionPass(ID), T(T) {
  }

  bool runOnMachineFunction(MachineFunction &MF) override {
    LiveIntervals &LIS = getAnalysis<LiveIntervals>();
    T(MF, LIS);
    EXPECT_TRUE(MF.verify(this));
    return true;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
    AU.addRequired<LiveIntervals>();
    AU.addPreserved<LiveIntervals>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
private:
  TestFunction T;
};

Pass *createTestPass(TestFunction F) {
  return new TestPass(F);
}

char TestPass::ID = 0;

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  initLLVM();
  return RUN_ALL_TESTS();
}
