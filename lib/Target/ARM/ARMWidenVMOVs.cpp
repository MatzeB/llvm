//===-- ARMWidenVMOVs.cpp - ARM Instruction Information -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that widens single register movs (VMOVS) to double
// register movs (VMOVD) where possible. We prefer the VMOVD because it may be
// changed into a VORR that can go down the NEON pipeline.
//
//===----------------------------------------------------------------------===//

#include "ARM.h"
#include "ARMBaseRegisterInfo.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Target/TargetInstrInfo.h"
using namespace llvm;

#define DEBUG_TYPE "arm-widen-vmovs"

namespace {
  struct WidenVMovs : public MachineFunctionPass {
    static char ID;
    WidenVMovs() : MachineFunctionPass(ID) {}

    bool runOnMachineFunction(MachineFunction &MF) override;

    const char *getPassName() const override {
      return "ARM widen VMovs";
    }
  };
  char WidenVMovs::ID = 0;
}

bool WidenVMovs::runOnMachineFunction(MachineFunction &MF) {
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();
  LivePhysRegs LiveRegs;
  bool Changed = false;

  for (MachineFunction::iterator B = MF.begin(), BE = MF.end(); B != BE; ++B) {
    MachineBasicBlock &BB = *B;

    // This iterator points to the place in the current basic block to where
    // we have tracked liveness (we track it only on demand).
    MachineBasicBlock::reverse_iterator Simulated = BB.rbegin();
    // See if the block contains any copy that we might want to expand.
    for (MachineBasicBlock::reverse_iterator I = BB.rbegin(), E = BB.rend();
         I != E; ++I) {
      MachineInstr &MI = *I;
      if (MI.getOpcode() != ARM::VMOVS)
        continue;

      // Look for a copy between even S-registers.  That is where we keep floats
      // when using NEON v2f32 instructions for f32 arithmetic.
      unsigned DstRegS = MI.getOperand(0).getReg();
      unsigned SrcRegS = MI.getOperand(1).getReg();
      assert(ARM::SPRRegClass.contains(DstRegS, SrcRegS));
      unsigned DstRegD = TRI->getMatchingSuperReg(DstRegS, ARM::ssub_0,
                                                  &ARM::DPRRegClass);
      unsigned SrcRegD = TRI->getMatchingSuperReg(SrcRegS, ARM::ssub_0,
                                                  &ARM::DPRRegClass);
      if (DstRegD == 0 || SrcRegD == 0)
        continue;

      // Simulate liveness to the current instruction and check if the upper
      // half of DstRegD is free.
      if (Simulated == BB.rbegin()) {
        LiveRegs.init(TRI);
        LiveRegs.addLiveOuts(&BB);
      }
      while (Simulated != I) {
        LiveRegs.stepBackward(*Simulated);
        ++Simulated;
      }
      unsigned DestRegUpper = TRI->getSubReg(DstRegD, ARM::ssub_1);
      assert(DestRegUpper != 0);
      if (LiveRegs.contains(DestRegUpper))
        continue;

      // All clear, widen the COPY.
      DEBUG(dbgs() << "widening:    " << MI);

      // Change the opcode and operands.
      MI.setDesc(TII->get(ARM::VMOVD));
      MI.getOperand(0).setReg(DstRegD);
      MI.getOperand(1).setReg(SrcRegD);

      DEBUG(dbgs() << "replaced by: " << MI);
      Changed = true;
    }
  }
  return Changed;
}

FunctionPass *llvm::createARMWidenVMOVPass() {
  return new WidenVMovs();
}
