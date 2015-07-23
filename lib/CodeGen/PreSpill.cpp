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

#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/machineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"

using namespace llvm;

#define DEBUG_TYPE "prespill"

namespace {
  class PreSpill : public MachineFunctionPass {
    MachineRegisterInfo *MRI;
    LiveIntervals *LIS;
    RegisterClassInfo RegClassInfo;
    const TargetRegisterInfo *TRI;

    unsigned *Offsets;
    unsigned NPressureUnits;

    bool inWorkset(unsigned Reg, const unsigned *WorkSet) const;

    void putIntoWorkset(unsigned Reg, unsigned PSet, unsigned *WorkSet) const;

    void removeFromWorkset(unsigned Reg, unsigned *WorkSet) const;

    void spillInBlock(MachineBasicBlock &MBB);

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

bool PreSpill::inWorkset(unsigned Reg, const unsigned *WorkSet) const {
  for (unsigned I = 0; I < NPressureUnits; ++I) {
    if (WorkSet[I] == Reg)
      return true;
  }
  return false;
}

void PreSpill::putIntoWorkset(unsigned Reg, unsigned PSet,
                              unsigned *WorkSet) const {
  unsigned Offset = Offsets[PSet];
  unsigned EndOffset = Offsets[PSet+1] - Offset;

  for (unsigned I = Offset; I < EndOffset; ++I) {
    if (WorkSet[I] == 0) {
      WorkSet[I] = Reg;
      return;
    }
  }

  // Nothing found, we need to spill something
  unsigned BestCost = 10000;
  unsigned BestPos = 0;
  for (unsigned I = Offset; I < EndOffset; ++I) {
    unsigned SpillCost = 1;
    if (SpillCost < BestCost) {
      BestPos = I;
      BestCost = SpillCost;
    }
  }

  // Spill WorkSet[I]

  WorkSet[BestPos] = Reg;
}

void PreSpill::removeFromWorkset(unsigned Reg, unsigned *WorkSet) const {
  for (unsigned I = 0; I < NPressureUnits; ++I) {
    if (WorkSet[I] == Reg)
      WorkSet[I] = 0;
  }
}

void PreSpill::spillInBlock(MachineBasicBlock &MBB) {
  unsigned *WorkSet = new unsigned[NPressureUnits];
  // TODO: Determine initial workset state

  for (MachineInstr &MI : MBB) {

    // Check Use Operands
    for (const MachineOperand &MO : MI.uses()) {
      if (!MO.isReg())
        continue;
      unsigned Reg = MO.getReg();
      if (Reg == 0)
        continue;

      if (inWorkset(Reg, WorkSet))
        continue;

      if (TargetRegisterInfo::isVirtualRegister(Reg)) {
        const TargetRegisterClass *RC = MRI->getRegClass(Reg);
        for (const int *Set = TRI->getRegClassPressureSets(RC); *Set != -1;
             ++Set) {
          putIntoWorkset(Reg, *Set, WorkSet);
        }
      }
    }
  }

  delete[] WorkSet;
}

bool PreSpill::runOnMachineFunction(MachineFunction &MF) {
  LIS = &getAnalysis<LiveIntervals>();

  const TargetSubtargetInfo &STI = MF.getSubtarget();
  TRI = STI.getRegisterInfo();
  MRI = &MF.getRegInfo();

  RegClassInfo.runOnMachineFunction(MF);

  // Compute number of pressure units necessary for the whole function
  NPressureUnits = 0;
  unsigned NPressureSets = TRI->getNumRegPressureSets();
  Offsets = new unsigned[NPressureSets+1];
  errs() << "NPressureSets: " << NPressureSets << '\n';
  for (unsigned I = 0; I < NPressureSets; ++I) {
    Offsets[I] = NPressureUnits;
    unsigned Limit = TRI->getRegPressureSetLimit(MF, I);
    NPressureUnits += Limit;
    const char *Name = TRI->getRegPressureSetName(I);
    errs() << " Set " << (Name ? Name : "NoName")
           << " Limit: " << Limit << '\n';
  }
  Offsets[NPressureSets] = NPressureUnits;

  for (MachineBasicBlock &MBB : MF) {
    spillInBlock(MBB);
  }

  errs() << "Do Something...\n";

  delete[] Offsets;

  return false;
}

FunctionPass *llvm::createPreSpillPass() {
  return new PreSpill();
}
