#include "MITests.h"
#include "gtest/gtest.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MIRParser/MIRParser.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBundle.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetRegisterInfo.h"

using namespace llvm;

namespace {

static MachineInstr &getMI(MachineFunction &MF, unsigned At,
                           unsigned BlockNum) {
  MachineBasicBlock &MBB = *MF.getBlockNumbered(BlockNum);

  unsigned I = 0;
  for (MachineInstr &MI : MBB) {
    if (I == At)
      return MI;
    ++I;
  }
  llvm_unreachable("Instruction not found");
}

/**
 * Move instruction number \p From in front of instruction number \p To and
 * update affected liveness intervals with LiveIntervalAnalysis::handleMove().
 */
static void testFinalizeBundle(MachineFunction &MF, LiveIntervals &LIS,
                               unsigned From, unsigned To) {
  MachineInstr &FromInstr = getMI(MF, From, 0);
  MachineInstr &ToInstr = getMI(MF, To, 0);

  for (MachineInstr &MI : make_range(FromInstr.getIterator(),
                                     ToInstr.getIterator()))
    MI.bundleWithSucc();

  finalizeBundle(
}

static void doTest(StringRef MIRFunc, TestFunction T) {
  LLVMContext Context;
  std::unique_ptr<TargetMachine> TM = createTargetMachine("x86_64--");
  // This test is designed for the X86 backend; stop if it is not available.
  if (!TM)
    return;

  legacy::PassManager PM;

  SmallString<160> S;
  StringRef MIRString = (Twine(R"MIR(
---
...
name: func
body: |
  bb.0:
)MIR") + Twine(MIRFunc) + Twine("...\n")).toNullTerminatedStringRef(S);
  std::unique_ptr<MIRParser> MIR;
  std::unique_ptr<Module> M = parseMIR(Context, PM, MIR, *TM, MIRString,
                                       "func");

  PM.add(createTestPass(T));

  PM.run(*M);
}

} // End of anonymous namespace.

TEST(TestFunction, SomeTest) {
  // Value defined.
  doTest(R"MIR(
    %0 : gr64 = IMPLICIT_DEF
    %1 : gr64 = IMPLICIT_DEF
    NOOP implicit %0
    NOOP implicit %1
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testFinalizeBundle(MF, LIS, 2, 3);
  });
}
