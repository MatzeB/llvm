//===- RegisterCoalescer.cpp - Generic Register Coalescing Interface -------==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the generic RegisterCoalescer interface which
// is used as the common interface used by all clients and
// implementations of register coalescing.
//
//===----------------------------------------------------------------------===//

// TODO: sort includes
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetSubtargetInfo.h"
using namespace llvm;

#define DEBUG_TYPE "prespill"

namespace {
  class PreSpill : public MachineFunctionPass {
    MachineRegisterInfo *MRI;
    LiveIntervals *LIS;
    RegisterClassInfo RegClassInfo;

  public:
    static char ID;
    PreSpill();
    void getAnalysisUsage(AnalysisUsage &AU) const override;

    const char *getPassName() const override {
      return "Pre Spilling";
    }

    bool runOnMachineFunction(MachineFunction &MF) override;
  };
}

char PreSpill::ID = 0;

PreSpill::PreSpill()
  : MachineFunctionPass(PreSpill::ID) {
  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeSlotIndexesPass(Registry);
  initializeLiveIntervalsPass(Registry);
  initializeMachineLoopInfoPass(Registry);
}

void PreSpill::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesCFG();
  AU.addRequired<LiveIntervals>();
  AU.addPreserved<LiveIntervals>();
  AU.addRequired<SlotIndexes>();
  AU.addPreserved<SlotIndexes>();
  AU.addRequired<MachineLoopInfo>();
  AU.addPreserved<MachineLoopInfo>();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool PreSpill::runOnMachineFunction(MachineFunction &MF) {
  MRI = &MF.getRegInfo();
  LIS = &getAnalysis<LiveIntervals>();

  RegClassInfo.runOnMachineFunction(MF);

  errs() << "Hello PreSpill from " << MF.getName() << '\n';
  return false;
}

FunctionPass *llvm::createPreSpillPass() {
  return new PreSpill();
}
