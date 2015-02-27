//===---------------------- ProcessImplicitDefs.cpp -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SetVector.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"

using namespace llvm;

#define DEBUG_TYPE "processimplicitdefs"

namespace {
/// Process IMPLICIT_DEF instructions and make sure there is one implicit_def
/// for each use. Add isUndef marker to implicit_def defs and their uses.
class ProcessImplicitDefs : public MachineFunctionPass {
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  MachineRegisterInfo *MRI;

  SmallSetVector<MachineInstr*, 16> WorkList;

  void processImplicitDef(MachineInstr &MI);
  void computeUndefLaneMask(MachineInstr &MI);
  void processInstr(MachineInstr &MI);

  bool canTurnIntoImplicitDef(MachineInstr *MI);

public:
  static char ID;

  ProcessImplicitDefs() : MachineFunctionPass(ID) {
    initializeProcessImplicitDefsPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &au) const override;

  bool runOnMachineFunction(MachineFunction &fn) override;
};
} // end anonymous namespace

char ProcessImplicitDefs::ID = 0;
char &llvm::ProcessImplicitDefsID = ProcessImplicitDefs::ID;

INITIALIZE_PASS_BEGIN(ProcessImplicitDefs, "processimpdefs",
                "Process Implicit Definitions", false, false)
INITIALIZE_PASS_END(ProcessImplicitDefs, "processimpdefs",
                "Process Implicit Definitions", false, false)

void ProcessImplicitDefs::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesCFG();
  AU.addPreserved<AliasAnalysis>();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool ProcessImplicitDefs::canTurnIntoImplicitDef(MachineInstr *MI) {
  if (!MI->isCopyLike() &&
      !MI->isInsertSubreg() &&
      !MI->isRegSequence() &&
      !MI->isPHI())
    return false;
  for (MIOperands MO(MI); MO.isValid(); ++MO)
    if (MO->isReg() && MO->isUse() && MO->readsReg())
      return false;
  return true;
}

void ProcessImplicitDefs::processImplicitDef(MachineInstr &MI) {
  DEBUG(dbgs() << "Processing " << MI);
  unsigned Reg = MI.getOperand(0).getReg();

  if (TargetRegisterInfo::isVirtualRegister(Reg)) {
    // For virtual registers, mark all uses as <undef>, and convert users to
    // implicit-def when possible.
    for (MachineOperand &MO : MRI->use_nodbg_operands(Reg)) {
      MO.setIsUndef();
      MachineInstr *UserMI = MO.getParent();
      if (canTurnIntoImplicitDef(UserMI)) {
        DEBUG(dbgs() << "Converting to IMPLICIT_DEF: " << *UserMI);
        UserMI->setDesc(TII->get(TargetOpcode::IMPLICIT_DEF));
        WorkList.insert(UserMI);
      } else if (UserMI->isInsertSubreg() || UserMI->isRegSequence()) {
        WorkList.insert(UserMI);
      }
    }
    MI.eraseFromParent();
    return;
  }

  // This is a physreg implicit-def.
  // Look for the first instruction to use or define an alias.
  MachineBasicBlock::instr_iterator UserMI = MI;
  MachineBasicBlock::instr_iterator UserE = MI.getParent()->instr_end();
  bool Found = false;
  for (++UserMI; UserMI != UserE; ++UserMI) {
    for (MIOperands MO(UserMI); MO.isValid(); ++MO) {
      if (!MO->isReg())
        continue;
      unsigned UserReg = MO->getReg();
      if (!TargetRegisterInfo::isPhysicalRegister(UserReg) ||
          !TRI->regsOverlap(Reg, UserReg))
        continue;
      // UserMI uses or redefines Reg. Set <undef> flags on all uses.
      Found = true;
      if (MO->isUse())
        MO->setIsUndef();
    }
    if (Found)
      break;
  }

  // If we found the using MI, we can erase the IMPLICIT_DEF.
  if (Found) {
    DEBUG(dbgs() << "Physreg user: " << *UserMI);
    MI.eraseFromParent();
    return;
  }

  // Using instr wasn't found, it could be in another block.
  // Leave the physreg IMPLICIT_DEF, but trim any extra operands.
  for (unsigned i = MI.getNumOperands() - 1; i; --i)
    MI.RemoveOperand(i);
  DEBUG(dbgs() << "Keeping physreg: " << MI);
}

void ProcessImplicitDefs::computeUndefLaneMask(MachineInstr &MI) {
  unsigned Reg = MI.getOperand(0).getReg();
  assert(TargetRegisterInfo::isVirtualRegister(Reg) &&
         "Should have a virtual register destination.");

  unsigned UndefLaneMask = MRI->getMaxLaneMaskForVReg(Reg);
  if (MI.isInsertSubreg()) {
    MachineOperand &MO = MI.getOperand(1);
    if (!MO.isUndef())
      return;
    if (UndefLaneMask == 0)
      return;

    unsigned SubIdx = MI.getOperand(3).getImm();
    unsigned WriteMask = TRI->getSubRegIndexLaneMask(SubIdx);
    UndefLaneMask &= ~WriteMask;
  } else if (MI.isRegSequence()) {
    // Calculate a mask of undefined lanes.
    for (unsigned i = 1, e = MI.getNumOperands(); i < e; i += 2) {
      MachineOperand &UseMO = MI.getOperand(i);
      if (UseMO.isUndef())
        continue;
      unsigned SubIdx = MI.getOperand(i+1).getImm();
      unsigned WriteMask = TRI->getSubRegIndexLaneMask(SubIdx);
      UndefLaneMask &= ~WriteMask;
    }
  } else if (MI.isCopy()) {
    unsigned DestIdx = MI.getOperand(0).getSubReg();
    MachineOperand &MO = MI.getOperand(1);
    unsigned OpUndefLaneMask = MO.getUndefLaneMask();
    UndefLaneMask = TRI->composeSubRegIndexLaneMask(DestIdx, OpUndefLaneMask);
  }

  // Propagate the undefined lanes to our users.
  if (UndefLaneMask == 0)
    return;
  for (MachineOperand &MO : MRI->use_nodbg_operands(Reg)) {
    MO.setUndefLaneMask(UndefLaneMask);
    MachineInstr *UserMI = MO.getParent();
    if (UserMI->isCopy() || UserMI->isInsertSubreg() || UserMI->isRegSequence()) {
      WorkList.insert(UserMI);
    }
  }
}

void ProcessImplicitDefs::processInstr(MachineInstr &MI) {
  if (MI.isImplicitDef()) {
    processImplicitDef(MI);
  } else {
    assert((MI.isRegSequence() || MI.isInsertSubreg() || MI.isCopy()) &&
           "Unexpected MI opcode");
    computeUndefLaneMask(MI);
  }
}

/// processImplicitDefs - Process IMPLICIT_DEF instructions and turn them into
/// <undef> operands.
bool ProcessImplicitDefs::runOnMachineFunction(MachineFunction &MF) {

  DEBUG(dbgs() << "********** PROCESS IMPLICIT DEFS **********\n"
               << "********** Function: " << MF.getName() << '\n');

  bool Changed = false;

  TII = MF.getSubtarget().getInstrInfo();
  TRI = MF.getSubtarget().getRegisterInfo();
  MRI = &MF.getRegInfo();
  assert(MRI->isSSA() && "ProcessImplicitDefs only works on SSA form.");
  assert(WorkList.empty() && "Inconsistent worklist state");

  for (MachineFunction::iterator MFI = MF.begin(), MFE = MF.end();
       MFI != MFE; ++MFI) {
    // Scan the basic block for implicit defs.
    for (MachineBasicBlock::instr_iterator MBBI = MFI->instr_begin(),
         MBBE = MFI->instr_end(); MBBI != MBBE; ++MBBI)
      if (MBBI->isImplicitDef())
        WorkList.insert(MBBI);

    if (WorkList.empty())
      continue;

    DEBUG(dbgs() << "BB#" << MFI->getNumber() << " has " << WorkList.size()
                 << " implicit defs.\n");
    Changed = true;

    // Drain the WorkList to recursively process any new implicit defs.
    do processInstr(*WorkList.pop_back_val());
    while (!WorkList.empty());
  }
  return Changed;
}
