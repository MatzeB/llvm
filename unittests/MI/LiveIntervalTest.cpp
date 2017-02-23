#include "MITests.h"
#include "gtest/gtest.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MIRParser/MIRParser.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/IR/LegacyPassManager.h"

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
static void testHandleMove(MachineFunction &MF, LiveIntervals &LIS,
                           unsigned From, unsigned To, unsigned BlockNum = 0) {
  MachineInstr &FromInstr = getMI(MF, From, BlockNum);
  MachineInstr &ToInstr = getMI(MF, To, BlockNum);

  MachineBasicBlock &MBB = *FromInstr.getParent();
  MBB.splice(ToInstr.getIterator(), &MBB, FromInstr.getIterator());
  LIS.handleMove(FromInstr, true);
}

static void liveIntervalTest(StringRef MIRFunc, TestFunction T) {
  LLVMContext Context;
  // As we lack a dedicated always available target for unittests, we go for
  // "AMDGPU" to be able to test normal and subregister liveranges.
  std::unique_ptr<TargetMachine> TM = createTargetMachine("amdgcn--");
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

TEST(TestFunction, MoveUpDef) {
  // Value defined.
  liveIntervalTest(R"MIR(
    S_NOP 0
    S_NOP 0
    early-clobber %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 1);
  });
}

TEST(TestFunction, MoveUpRedef) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 1);
  });
}

TEST(TestFunction, MoveUpEarlyDef) {
  liveIntervalTest(R"MIR(
    S_NOP 0
    S_NOP 0
    early-clobber %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 1);
  });
}

TEST(TestFunction, MoveUpEarlyRedef) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    early-clobber %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 1);
  });
}

TEST(TestFunction, MoveUpKill) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 1);
  });
}

TEST(TestFunction, MoveUpKillFollowing) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit %0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 1);
  });
}

// TODO: Construct a situation where we have intervals following a hole
// while still having connected components.

TEST(TestFunction, MoveDownDef) {
  // Value defined.
  liveIntervalTest(R"MIR(
    S_NOP 0
    early-clobber %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 2);
  });
}

TEST(TestFunction, MoveDownRedef) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    S_NOP 0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 2);
  });
}

TEST(TestFunction, MoveDownEarlyDef) {
  liveIntervalTest(R"MIR(
    S_NOP 0
    early-clobber %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 2);
  });
}

TEST(TestFunction, MoveDownEarlyRedef) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    early-clobber %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    S_NOP 0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 2);
  });
}

TEST(TestFunction, MoveDownKill) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0, implicit %0
    S_NOP 0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 2);
  });
}

TEST(TestFunction, MoveDownKillFollowing) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit %0
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 2);
  });
}

TEST(TestFunction, MoveUndefUse) {
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0, implicit undef %0
    S_NOP 0, implicit %0
    S_NOP 0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 1, 3);
  });
}

TEST(TestFunction, MoveUpValNos) {
  // handleMoveUp() had a bug where it would reuse the value number of the
  // destination segment, even though we have no guarntee that this valno wasn't
  // used in other segments.
  liveIntervalTest(R"MIR(
    successors: %bb.1, %bb.2
    %0 : sreg_64 = IMPLICIT_DEF
    S_CBRANCH_VCCNZ %bb.2, implicit undef %vcc
    S_BRANCH %bb.1
  bb.2:
    S_NOP 0, implicit %0
  bb.1:
    successors: %bb.2
    %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    %0 = IMPLICIT_DEF implicit %0(tied-def 0)
    S_BRANCH %bb.2
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 2, 0, 2);
  });
}

TEST(TestFunction, MoveOverUndefUse0) {
  // findLastUseBefore() used by handleMoveUp() must ignore undef operands.
  liveIntervalTest(R"MIR(
    %0 : sreg_64 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit undef %0
    %0 = IMPLICIT_DEF implicit %0(tied-def 0)
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 3, 1);
  });
}

TEST(TestFunction, MoveOverUndefUse1) {
  // findLastUseBefore() used by handleMoveUp() must ignore undef operands.
  liveIntervalTest(R"MIR(
    %sgpr0 = IMPLICIT_DEF
    S_NOP 0
    S_NOP 0, implicit undef %sgpr0
    %sgpr0 = IMPLICIT_DEF implicit %sgpr0(tied-def 0)
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    testHandleMove(MF, LIS, 3, 1);
  });
}

TEST(TestFunction, SubRegMoveDown) {
  // Subregister ranges can have holes inside a basic block. Check for a
  // movement of the form 32->150 in a liverange [16, 32) [100,200).
  liveIntervalTest(R"MIR(
    successors: %bb.1, %bb.2
    %0 : sreg_64 = IMPLICIT_DEF
    S_CBRANCH_VCCNZ %bb.2, implicit undef %vcc
    S_BRANCH %bb.1
  bb.2:
    successors: %bb.1
    S_NOP 0, implicit %0.sub0
    S_NOP 0, implicit %0.sub1
    S_NOP 0
    undef %0.sub0 = IMPLICIT_DEF
    %0.sub1 = IMPLICIT_DEF
  bb.1:
    S_NOP 0, implicit %0
)MIR", [](MachineFunction &MF, LiveIntervals &LIS) {
    // Scheduler behaviour: Clear def,read-undef flag and move.
    MachineInstr &MI = getMI(MF, 3, /*BlockNum=*/1);
    MI.getOperand(0).setIsUndef(false);
    testHandleMove(MF, LIS, 1, 4, /*BlockNum=*/1);
  });
}
