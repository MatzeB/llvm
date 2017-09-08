//===-- RegAllocFast.cpp - A fast register allocator for debug code -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
/// \file This register allocator allocates registers to a basic block at a
/// time, attempting to keep values in registers and reusing registers as
/// appropriate.
//
// Possible ideas:
//  - Operate on regunits instead of registers.
//  - Rethink when to use aliasregs, and when super/sub regs
//  - Could invent a cooler version of getUniqueVRegDef() that ignores
//    <def,tied> when tied to the same vreg.
//  - Post pass to remove spills that we happened to never reload from?
//  - TODO: review earlyclobber + tied combination...
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SparseSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include <algorithm>
using namespace llvm;

#define DEBUG_TYPE "regalloc"

STATISTIC(NumStores, "Number of stores added");
STATISTIC(NumLoads , "Number of loads added");
STATISTIC(NumCoalesced, "Number of copies coalesced");

static RegisterRegAlloc
  fastRegAlloc("fast", "fast register allocator", createFastRegisterAllocator);

namespace {
  class RegAllocFast : public MachineFunctionPass {
  public:
    static char ID;
    RegAllocFast() : MachineFunctionPass(ID), StackSlotForVirtReg(-1) {}

  private:
    MachineFrameInfo *MFI;
    MachineRegisterInfo *MRI;
    const TargetRegisterInfo *TRI;
    const TargetInstrInfo *TII;
    RegisterClassInfo RegClassInfo;

    /// Basic block currently being allocated.
    MachineBasicBlock *MBB;

    /// Maps virtual regs to the frame index where these values are spilled.
    IndexedMap<int, VirtReg2IndexFunctor> StackSlotForVirtReg;

    /// Everything we know about a live virtual register.
    struct LiveReg {
      MachineInstr *LastUse = nullptr;  ///< Last instr to use reg.
      unsigned VirtReg;                 ///< Virtual register number.
      MCPhysReg PhysReg = 0;            ///< Currently held here.
      unsigned short LastOpNum = 0;     ///< OpNum on LastUse.
      bool LiveOut = false;             ///< Register is possibly live out.
      bool Reloaded = false;            ///< Was this value ever reloaded?

      explicit LiveReg(unsigned VirtReg) : VirtReg(VirtReg) {
        assert(TargetRegisterInfo::isVirtualRegister(VirtReg));
      }

      unsigned getSparseSetIndex() const {
        return TargetRegisterInfo::virtReg2Index(VirtReg);
      }
    };

    typedef SparseSet<LiveReg> LiveRegMap;

    /// This map contains entries for each virtual register that is currently
    /// available in a physical register.
    LiveRegMap LiveVirtRegs;

    DenseMap<unsigned, SmallVector<MachineInstr *, 4> > LiveDbgValueMap;

    BitVector MayLiveOut;

    /// Track the state of a physical register.
    enum RegState {
      /// A free register is not currently in use and can be allocated
      /// immediately without checking aliases.
      regFree,

      /// A pre-assigned register has been assigned before register allocation
      /// (e.g., setting up a call parameter), and it remains reserved until it
      /// is used.
      regPreAssigned,

      /// Used temporarily in reloadAtBegin to mark physreg that are live-in
      /// to the basic block.
      regLiveIn,

      /// A register state may also be a virtual register number, indication
      /// that the physical register is currently allocated to a virtual
      /// register. In that case, LiveVirtRegs contains the inverse mapping.
    };

    /// One of the RegState enums, or a virtreg.
    std::vector<unsigned> PhysRegState;

    SmallVector<MachineInstr*, 32> Coalesced;

    /// Set of register units.
    typedef SparseSet<unsigned> UsedInInstrSet;

    /// Set of register units that are used in the current instruction, and so
    /// cannot be allocated.
    UsedInInstrSet UsedInInstr;

    void setPhysRegState(MCPhysReg PhysReg, unsigned NewState);

    /// Mark a physreg as used in this instruction.
    void markRegUsedInInstr(MCPhysReg PhysReg) {
      for (MCRegUnitIterator Units(PhysReg, TRI); Units.isValid(); ++Units)
        UsedInInstr.insert(*Units);
    }

    void unmarkRegUsedInInstr(MCPhysReg PhysReg) {
      for (MCRegUnitIterator Units(PhysReg, TRI); Units.isValid(); ++Units)
        UsedInInstr.erase(*Units);
    }

    /// Check if a physreg or any of its aliases are used in this instruction.
    bool isRegUsedInInstr(MCPhysReg PhysReg) const {
      for (MCRegUnitIterator Units(PhysReg, TRI); Units.isValid(); ++Units)
        if (UsedInInstr.count(*Units))
          return true;
      return false;
    }

    enum : unsigned {
      spillClean = 50,
      spillDirty = 100,
      spillPrefBonus = 20,
      spillImpossible = ~0u
    };
  public:
    StringRef getPassName() const override { return "Fast Register Allocator"; }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesCFG();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    MachineFunctionProperties getRequiredProperties() const override {
      return MachineFunctionProperties().set(
          MachineFunctionProperties::Property::NoPHIs);
    }

    MachineFunctionProperties getSetProperties() const override {
      return MachineFunctionProperties().set(
          MachineFunctionProperties::Property::NoVRegs);
    }

  private:
    bool runOnMachineFunction(MachineFunction &Fn) override;
    void allocateBasicBlock(MachineBasicBlock &MBB);
    void allocateInstruction(MachineInstr &MI);
    int getStackSpaceFor(unsigned VirtReg);

    void spillVirtReg(MachineBasicBlock::iterator Before, unsigned VirtReg,
                      MCPhysReg AssignedReg, bool Kill);

    void usePhysReg(MachineInstr &MI, MCPhysReg PhysReg);
    void definePhysReg(MachineInstr &MI, MCPhysReg PhysReg);
    void displacePhysReg(MachineInstr &MI, MCPhysReg PhysReg);
    void freePhysReg(MCPhysReg PhysReg);
    unsigned calcSpillCost(MCPhysReg PhysReg) const;
    void assignVirtToPhysReg(LiveReg&, MCPhysReg PhysReg);
    LiveRegMap::iterator findLiveVirtReg(unsigned VirtReg) {
      return LiveVirtRegs.find(TargetRegisterInfo::virtReg2Index(VirtReg));
    }
    LiveRegMap::const_iterator findLiveVirtReg(unsigned VirtReg) const {
      return LiveVirtRegs.find(TargetRegisterInfo::virtReg2Index(VirtReg));
    }
    void allocVirtReg(MachineInstr &MI, LiveRegMap::iterator LRI,
                      unsigned Hint);
    void allocVirtRegUndef(MachineOperand &MO);
    void defineLiveThroughVirtReg(MachineInstr &MI, unsigned OpNum,
                                  unsigned VirtReg);
    void defineVirtReg(MachineInstr &MI, unsigned OpNum, unsigned VirtReg);
    void useVirtReg(MachineInstr &MI, unsigned OpNum, unsigned VirtReg);
    void reload(MachineBasicBlock::iterator Before, unsigned VirtReg,
                MCPhysReg PhsReg);
    void reloadAtBegin();
    void setPhysReg(MachineInstr &MI, unsigned OpNum, MCPhysReg PhysReg);

    bool mayLiveOut(const MachineOperand &MO);
    unsigned traceCopies(unsigned VirtReg) const;

    void dumpState();
  };
  char RegAllocFast::ID = 0;
}

INITIALIZE_PASS(RegAllocFast, "regallocfast", "Fast Register Allocator", false,
                false)

/// This allocates space for the specified virtual register to be held on the
/// stack.
int RegAllocFast::getStackSpaceFor(unsigned VirtReg) {
  // Find the location Reg would belong...
  int SS = StackSlotForVirtReg[VirtReg];
  // Already has space allocated?
  if (SS != -1)
    return SS;

  // Allocate a new stack object for this spill location...
  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  unsigned Size = TRI->getSpillSize(RC);
  unsigned Align = TRI->getSpillAlignment(RC);
  int FrameIdx = MFI->CreateSpillStackObject(Size, Align);

  // Assign the slot.
  StackSlotForVirtReg[VirtReg] = FrameIdx;
  return FrameIdx;
}

void RegAllocFast::spillVirtReg(MachineBasicBlock::iterator Before,
                                unsigned VirtReg, MCPhysReg AssignedReg,
                                bool Kill) {
  DEBUG(dbgs() << "Spilling " << PrintReg(VirtReg, TRI)
               << " in " << PrintReg(AssignedReg, TRI));
  int FI = getStackSpaceFor(VirtReg);
  DEBUG(dbgs() << " to stack slot #" << FI << "\n");

  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  TII->storeRegToStackSlot(*MBB, Before, AssignedReg, Kill, FI, &RC, TRI);
  ++NumStores;

#if 0
  // If this register is used by DBG_VALUE then insert new DBG_VALUE to
  // identify spilled location as the place to find corresponding variable's
  // value.
  SmallVectorImpl<MachineInstr *> &LRIDbgValues =
    LiveDbgValueMap[LRI->VirtReg];
  for (MachineInstr *DBG : LRIDbgValues) {
    MachineInstr *NewDV = buildDbgValueForSpill(*MBB, MI, *DBG, FI);
    assert(NewDV->getParent() == MBB && "dangling parent pointer");
    (void)NewDV;
    DEBUG(dbgs() << "Inserting debug info due to spill:" << "\n" << *NewDV);
  }
  // Now this register is spilled there is should not be any DBG_VALUE
  // pointing to this register because they are all pointing to spilled value
  // now.
  LRIDbgValues.clear();
  if (SpillKill)
    LR.LastUse = nullptr; // Don't kill register again
#endif
}

/// Reload all currently assigned virtual registers.
void RegAllocFast::reloadAtBegin() {
  for (MachineBasicBlock::RegisterMaskPair P : MBB->liveins()) {
    MCPhysReg Reg = P.PhysReg;
    // Set state to live-in. This possibly overrides mappings to virtual
    // registers but we don't care anymore at this point.
    setPhysRegState(Reg, regLiveIn);
  }

  // The LiveRegMap is keyed by an unsigned (the virtreg number), so the order
  // of spilling here is deterministic, if arbitrary.
  for (const LiveReg &LR : LiveVirtRegs) {
    MCPhysReg PhysReg = LR.PhysReg;
    if (PhysReg == 0)
      continue;

    if (PhysRegState[PhysReg] == regLiveIn)
      continue;

    assert(MBB != &MBB->getParent()->front() && "no reload in start block");
    reload(MBB->begin(), LR.VirtReg, PhysReg);
  }
}

/// Handle the direct use of a physical register.  Check that the register is
/// not used by a virtreg. Kill the physreg, marking it free. This may add
/// implicit kills to MO->getParent() and invalidate MO.
void RegAllocFast::usePhysReg(MachineInstr &MI, MCPhysReg Reg) {
  assert(TargetRegisterInfo::isPhysicalRegister(Reg) && "expected physreg");
  displacePhysReg(MI, Reg);
  setPhysRegState(Reg, regPreAssigned);
  markRegUsedInInstr(Reg);
}

void RegAllocFast::definePhysReg(MachineInstr &MI, MCPhysReg Reg) {
  if (PhysRegState[Reg] != regPreAssigned) {
    // pre-assigned physregs are not allowed to be live-out of a block,
    // so we should have already seen a use or it must be a dead def.
    // TODO?
    //MO.setIsDead(true);
    displacePhysReg(MI, Reg);
    setPhysRegState(Reg, regPreAssigned);
  }
}

/// Mark PhysReg as reserved or free after spilling any virtregs. This is very
/// similar to defineVirtReg except the physreg is reserved instead of
/// allocated.
void RegAllocFast::displacePhysReg(MachineInstr &MI, MCPhysReg PhysReg) {
  for (MCRegAliasIterator AI(PhysReg, TRI, true); AI.isValid(); ++AI) {
    MCPhysReg AliasReg = *AI;
    switch (unsigned VirtReg = PhysRegState[AliasReg]) {
    default: {
        LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
        assert(LRI != LiveVirtRegs.end() && "datastructures in sync");
        MachineBasicBlock::iterator ReloadBefore =
            std::next((MachineBasicBlock::iterator)MI.getIterator());
        reload(ReloadBefore, VirtReg, LRI->PhysReg);

        setPhysRegState(LRI->PhysReg, regFree);
        LRI->PhysReg = 0;
        LRI->Reloaded = true;
      }
      break;

    case regPreAssigned:
      PhysRegState[AliasReg] = regFree;
      break;
    case regFree:
      break;
    }
  }
}

void RegAllocFast::setPhysRegState(MCPhysReg PhysReg, unsigned NewState) {
  for (MCRegAliasIterator AI(PhysReg, TRI, true); AI.isValid(); ++AI)
    PhysRegState[*AI] = NewState;
}

void RegAllocFast::freePhysReg(MCPhysReg PhysReg) {
  DEBUG(dbgs() << "Freeing " << PrintReg(PhysReg, TRI) << ':');

  switch (unsigned VirtReg = PhysRegState[PhysReg]) {
  case regFree:
    DEBUG(dbgs() << '\n');
    return;
  case regPreAssigned:
    DEBUG(dbgs() << '\n');
    setPhysRegState(PhysReg, regFree);
    return;
  default: {
      LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
      assert(LRI != LiveVirtRegs.end());
      DEBUG(dbgs() << ' ' << PrintReg(LRI->VirtReg, TRI) << '\n');
      setPhysRegState(LRI->PhysReg, regFree);
      LRI->PhysReg = 0;
    }
    return;
  }
}

/// \brief Return the cost of spilling clearing out PhysReg and aliases so it is
/// free for allocation. Returns 0 when PhysReg is free or disabled with all
/// aliases disabled - it can be allocated directly.
/// \returns spillImpossible when PhysReg or an alias can't be spilled.
unsigned RegAllocFast::calcSpillCost(MCPhysReg PhysReg) const {
  switch (unsigned VirtReg = PhysRegState[PhysReg]) {
  case regFree:
    return 0;
  case regPreAssigned:
    DEBUG(dbgs() << "Cannot spill pre-assigned " << PrintReg(PhysReg, TRI)
          << '\n');
    return spillImpossible;
  default: {
    LiveRegMap::const_iterator I = findLiveVirtReg(VirtReg);
    assert(I != LiveVirtRegs.end() && "Missing VirtReg entry");
    return I->Reloaded ? spillClean : spillDirty;
  }
  }
}

/// \brief This method updates local state so that we know that PhysReg is the
/// proper container for VirtReg now.  The physical register must not be used
/// for anything else when this is called.
void RegAllocFast::assignVirtToPhysReg(LiveReg &LR, MCPhysReg PhysReg) {
  DEBUG(dbgs() << "Assigning " << PrintReg(LR.VirtReg, TRI) << " to "
               << PrintReg(PhysReg, TRI) << "\n");
  assert(LR.PhysReg == 0 && "Already assigned a physreg");
  assert(PhysReg != 0 && "Trying to assign no register");
  LR.PhysReg = PhysReg;
  setPhysRegState(PhysReg, LR.VirtReg);
}

unsigned RegAllocFast::traceCopies(unsigned VirtReg) const {
  static const unsigned Limit = 3;
  unsigned C = 0;
  unsigned Reg = VirtReg;
  do {
    MachineInstr *VRegDef = MRI->getUniqueVRegDef(Reg);
    if (VRegDef == nullptr || !VRegDef->isCopy() ||
        VRegDef->getOperand(0).getSubReg() != 0)
      return 0;
    const MachineOperand &SrcMO = VRegDef->getOperand(1);
    if (SrcMO.getSubReg() != 0)
      return 0;
    Reg = SrcMO.getReg();
    if (TargetRegisterInfo::isPhysicalRegister(Reg))
      return Reg;
  } while(TargetRegisterInfo::isVirtualRegister(Reg) && ++C <= Limit);
  return 0;
}

/// Allocates a physical register for VirtReg.
void RegAllocFast::allocVirtReg(MachineInstr &MI, LiveRegMap::iterator LRI,
                                unsigned Hint0) {
  const unsigned VirtReg = LRI->VirtReg;
  assert(LRI->PhysReg == 0);

  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  DEBUG(dbgs() << "Search register for " << PrintReg(VirtReg) << " in class "
               << TRI->getRegClassName(&RC) << "\n");

  // Try first hint.
  if (TargetRegisterInfo::isPhysicalRegister(Hint0) &&
      MRI->isAllocatable(Hint0) && RC.contains(Hint0) &&
      !isRegUsedInInstr(Hint0)) {
    // Take hint if the register is currently free.
    if (PhysRegState[Hint0] == regFree) {
      DEBUG(dbgs() << "\tPreferred Register 0: " << PrintReg(Hint0, TRI)
            << '\n');
      assignVirtToPhysReg(*LRI, Hint0);
      return;
    } else {
      DEBUG(dbgs() << "\tPreferred Register 0: " << PrintReg(Hint0, TRI)
            << "occupied\n");
    }
  }

  // Take hint when possible.
  unsigned Hint1 = traceCopies(VirtReg);
  if (TargetRegisterInfo::isPhysicalRegister(Hint1) &&
      MRI->isAllocatable(Hint1) && RC.contains(Hint1) &&
      !isRegUsedInInstr(Hint1)) {
    // Take hint if the register is currently free.
    if (PhysRegState[Hint1] == regFree) {
      DEBUG(dbgs() << "\tPreferred Register 1: " << PrintReg(Hint1, TRI)
            << '\n');
      assignVirtToPhysReg(*LRI, Hint1);
      return;
    } else {
      DEBUG(dbgs() << "\tPreferred Register 1: " << PrintReg(Hint1, TRI)
            << "occupied\n");
    }
  } else {
    Hint1 = 0;
  }

  MCPhysReg BestReg = 0;
  unsigned BestCost = spillImpossible;
  ArrayRef<MCPhysReg> AllocationOrder = RegClassInfo.getOrder(&RC);
  for (MCPhysReg PhysReg : AllocationOrder) {
    DEBUG(dbgs() << "\tRegister: " << PrintReg(PhysReg, TRI) << ' ');
    if (isRegUsedInInstr(PhysReg)) {
      DEBUG(dbgs() << "already used in instr.\n");
      continue;
    }

    unsigned Cost = calcSpillCost(PhysReg);
    DEBUG(dbgs() << "Cost: " << Cost << " BestCost: " << BestCost << "\n");
    // Immediate take a register with cost 0.
    if (Cost == 0) {
      assignVirtToPhysReg(*LRI, PhysReg);
      return;
    }
    if (PhysReg == Hint0 || PhysReg == Hint1)
      Cost -= spillPrefBonus;
    if (Cost < BestCost) {
      BestReg = PhysReg;
      BestCost = Cost;
    }
  }

  if (!BestReg) {
    // TODO: There may be a chance to assign this by experimenting/recoloring.

    // Nothing we can do: Report an error and keep going with a invalid
    // allocation.
    if (MI.isInlineAsm())
      MI.emitError("inline assembly requires more registers than available");
    else
      MI.emitError("ran out of registers during register allocation");
    BestReg = *AllocationOrder.begin();
    PhysRegState[BestReg] = regFree;
  } else {
    displacePhysReg(MI, BestReg);
  }
  assignVirtToPhysReg(*LRI, BestReg);
}

void RegAllocFast::allocVirtRegUndef(MachineOperand &MO) {
  assert(MO.isUndef() && "expected undef use");
  unsigned Reg = MO.getReg();
  assert(TargetRegisterInfo::isVirtualRegister(Reg) && "Expected virtreg");
  const TargetRegisterClass &RC = *MRI->getRegClass(Reg);
  ArrayRef<MCPhysReg> AllocationOrder = RegClassInfo.getOrder(&RC);
  for (MCPhysReg PhysReg : AllocationOrder) {
    assert(!MRI->isReserved(PhysReg) && "should have allocatable reg");
    unsigned SubRegIdx = MO.getSubReg();
    MCPhysReg FinalReg = SubRegIdx ? TRI->getSubReg(PhysReg, SubRegIdx)
                                   : PhysReg;
    MO.setReg(FinalReg);
    MO.setSubReg(0);
    return;
  }
  errs() << *MO.getParent();
  report_fatal_error("No candidate for undef virtual register use");
}

bool RegAllocFast::mayLiveOut(const MachineOperand &MO) {
  assert(TargetRegisterInfo::isVirtualRegister(MO.getReg()) &&
         "should be called with vreg def/use");
  // No vregs live out of the return block.
  if (MBB->isReturnBlock())
    return false;

  // Cheap heuristic to catch the common case where all uses of a vreg are
  // in the same basic block.
  unsigned Reg = MO.getReg();
  if (MayLiveOut.test(TargetRegisterInfo::virtReg2Index(Reg)))
    return true;

  static const unsigned Limit = 8;
  unsigned C = 0;
  for (const MachineOperand &Use : MRI->reg_nodbg_operands(Reg)) {
    if (Use.getParent()->getParent() != MBB || ++C >= Limit) {
      MayLiveOut.set(TargetRegisterInfo::virtReg2Index(Reg));
      return true;
    }
  }
  return false;
}

/// Variation of defineVirtReg() with special handling for livethrough regs
/// (tied or earlyclobber) that may interfere with preassigned uses.
void RegAllocFast::defineLiveThroughVirtReg(MachineInstr &MI, unsigned OpNum,
                                            unsigned VirtReg) {
  LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
  if (LRI != LiveVirtRegs.end()) {
    MCPhysReg PrevReg = LRI->PhysReg;
    if (PrevReg != 0 && isRegUsedInInstr(PrevReg)) {
      DEBUG(dbgs() << "Need new assignment for " << PrintReg(PrevReg, TRI)
            << " (tied/earlyclobber resolution)\n");
      freePhysReg(PrevReg);
      LRI->PhysReg = 0;
      allocVirtReg(MI, LRI, 0);
      MachineBasicBlock::iterator InsertBefore =
        std::next((MachineBasicBlock::iterator)MI.getIterator());
      DEBUG(dbgs() << "Copy " << PrintReg(LRI->PhysReg, TRI) << " to "
            << PrintReg(PrevReg, TRI) << "\n");
      BuildMI(*MBB, InsertBefore, MI.getDebugLoc(),
              TII->get(TargetOpcode::COPY), PrevReg)
        .addReg(LRI->PhysReg, llvm::RegState::Kill);
    }
  }
  return defineVirtReg(MI, OpNum, VirtReg);
}

/// Allocates a register for VirtReg and mark it as dirty.
void RegAllocFast::defineVirtReg(MachineInstr &MI, unsigned OpNum,
                                 unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  LiveRegMap::iterator LRI;
  bool New;
  std::tie(LRI, New) = LiveVirtRegs.insert(LiveReg(VirtReg));
  if (New) {
    MachineOperand &MO = MI.getOperand(OpNum);
    if (!MO.isDead()) {
      if (mayLiveOut(MO))
        LRI->LiveOut = true;
      else
        MO.setIsDead(true);
    }
  }
  if (LRI->PhysReg == 0)
    allocVirtReg(MI, LRI, 0);
  else {
    assert(!isRegUsedInInstr(LRI->PhysReg) && "TODO: preassign mismatch");
    DEBUG(dbgs() << "In def of " << PrintReg(VirtReg, TRI)
          << " use existing assignment to " << PrintReg(LRI->PhysReg, TRI)
          << '\n');
  }

  MCPhysReg PhysReg = LRI->PhysReg;
  assert(PhysReg != 0 && "Register not assigned");
  if (LRI->Reloaded || LRI->LiveOut) {
    if (!MI.isImplicitDef()) {
      MachineBasicBlock::iterator SpillBefore =
          std::next((MachineBasicBlock::iterator)MI.getIterator());
      DEBUG(dbgs() << "Spill Reason: LO: " << LRI->LiveOut << " RL: "
            << LRI->Reloaded << '\n');
      bool Kill = LRI->LastUse == nullptr;
      spillVirtReg(SpillBefore, VirtReg, PhysReg, Kill);
      LRI->LastUse = nullptr;
    }
    LRI->LiveOut = false;
    LRI->Reloaded = false;
  }
  setPhysReg(MI, OpNum, PhysReg);
}

void RegAllocFast::useVirtReg(MachineInstr &MI, unsigned OpNum,
                              unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  LiveRegMap::iterator LRI;
  bool New;
  std::tie(LRI, New) = LiveVirtRegs.insert(LiveReg(VirtReg));
  if (New) {
    MachineOperand &MO = MI.getOperand(OpNum);
    if (!MO.isKill()) {
      if (mayLiveOut(MO))
        LRI->LiveOut = true;
      else
        MO.setIsKill(true);
    }
  } else {
    assert((!MI.getOperand(OpNum).isKill() || LRI->LastUse == &MI)
            && "Invalid kill flag");
  }
  if (LRI->PhysReg == 0) {
    assert(!MI.getOperand(OpNum).isTied() && "tied op should be allocated");
    unsigned Hint = 0;
    if (MI.isCopy() && MI.getOperand(1).getSubReg() == 0) {
      Hint = MI.getOperand(0).getReg();
      assert(TargetRegisterInfo::isPhysicalRegister(Hint) &&
             "Copy destination should already be assigned");
    }
    allocVirtReg(MI, LRI, Hint);
  } else {
    // A pre-assigned use/earlyclobber should lead to the vreg getting displaced
    // earlier so we shouldn't have an assigned for a used register here.
    assert((LRI->LastUse == &MI || MI.getOperand(OpNum).isTied() ||
            !isRegUsedInInstr(LRI->PhysReg)) && "assigned reg free");
  }

  LRI->LastUse = &MI;
  LRI->LastOpNum = OpNum;
  setPhysReg(MI, OpNum, LRI->PhysReg);
}

void RegAllocFast::reload(MachineBasicBlock::iterator Before, unsigned VirtReg,
                          MCPhysReg PhysReg) {
  DEBUG(dbgs() << "Reloading " << PrintReg(VirtReg, TRI) << " into "
               << PrintReg(PhysReg, TRI) << "\n");
  int FI = getStackSpaceFor(VirtReg);
  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  TII->loadRegFromStackSlot(*MBB, Before, PhysReg, FI, &RC, TRI);
  ++NumLoads;
}

/// Changes operand OpNum in MI the refer the PhysReg, considering subregs. This
/// may invalidate any operand pointers.  Return true if the operand kills its
/// register.
void RegAllocFast::setPhysReg(MachineInstr &MI, unsigned OpNum,
                              MCPhysReg PhysReg) {
  markRegUsedInInstr(PhysReg);
  MachineOperand &MO = MI.getOperand(OpNum);
  if (!MO.getSubReg()) {
    MO.setReg(PhysReg);
    return;
  }

  // Handle subregister index.
  MO.setReg(PhysReg ? TRI->getSubReg(PhysReg, MO.getSubReg()) : 0);
  MO.setSubReg(0);

  // A kill flag implies killing the full register. Add corresponding super
  // register kill.
  if (MO.isKill()) {
    MI.addRegisterKilled(PhysReg, TRI, true);
    return;
  }

  // A <def,read-undef> of a sub-register requires an implicit def of the full
  // register.
  if (MO.isDef() && MO.isUndef()) {
    MO.setIsUndef(false);
    if (MO.isDead())
      MI.addRegisterDead(PhysReg, TRI, true);
    else
      MI.addRegisterDefined(PhysReg, TRI);
  }
}

#ifndef NDEBUG
void RegAllocFast::dumpState() {
  for (unsigned Reg = 1, E = TRI->getNumRegs(); Reg != E; ++Reg) {
    switch(unsigned VirtReg = PhysRegState[Reg]) {
    case regFree:
      break;
    case regPreAssigned:
      dbgs() << " " << TRI->getName(Reg);
      dbgs() << "[P]";
      break;
    case regLiveIn:
      llvm_unreachable("Should not have regLiveIn in map");
    default: {
      dbgs() << " " << TRI->getName(Reg);
      dbgs() << '=' << PrintReg(VirtReg);
      LiveRegMap::iterator I = findLiveVirtReg(VirtReg);
      assert(I != LiveVirtRegs.end() && "Missing VirtReg entry");
      if (I->LiveOut || I->Reloaded) {
        dbgs() << '[';
        if (I->LiveOut) dbgs() << 'O';
        if (I->Reloaded) dbgs() << 'R';
        dbgs() << ']';
      }
      assert((I->PhysReg == Reg || TRI->regsOverlap(I->PhysReg, Reg)) &&
             "Bad inverse map");
      break;
    }
    }
  }
  dbgs() << '\n';
  // Check that LiveVirtRegs is the inverse.
  for (const LiveReg &LR : LiveVirtRegs) {
    unsigned VirtReg = LR.VirtReg;
    assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
           "Bad map key");
    MCPhysReg PhysReg = LR.PhysReg;
    if (PhysReg != 0) {
      assert(TargetRegisterInfo::isPhysicalRegister(PhysReg) &&
             "Bad map value");
      assert(PhysRegState[PhysReg] == VirtReg && "Bad inverse map");
    }
  }
}
#endif

void RegAllocFast::allocateInstruction(MachineInstr &MI) {
  // Track registers used by instruction.
  UsedInInstr.clear();

  // Scan instruction, handle physreg defs.
  bool HasPhysRegUse = false;
  bool HasRegMask = false;
  bool HasVRegDef = false;
  bool HasDef = false;
  bool HasEarlyClobber = false;
  bool NeedToAssignLiveThroughs = false;
  for (const MachineOperand &MO : MI.operands()) {
    if (MO.isReg()) {
      unsigned Reg = MO.getReg();
      if (TargetRegisterInfo::isVirtualRegister(Reg)) {
        if (MO.isDef()) {
          HasDef = true;
          HasVRegDef = true;
          if (MO.isEarlyClobber()) {
            HasEarlyClobber = true;
            NeedToAssignLiveThroughs = true;
          }
          if (MO.isTied())
            NeedToAssignLiveThroughs = true;
        }
      } else if (TargetRegisterInfo::isPhysicalRegister(Reg)) {
        if (!MRI->isReserved(Reg)) {
          if (MO.isDef()) {
            HasDef = true;
            definePhysReg(MI, Reg);
            if (MO.isEarlyClobber())
              HasEarlyClobber = true;
          }
          if (MO.readsReg())
            HasPhysRegUse = true;
        }
      }
    } else if (MO.isRegMask()) {
      HasRegMask = true;
    }
  }

  // Allocate virtreg defs.
  if (HasDef) {
    if (HasVRegDef) {
      // Special handling for early clobbers: We need to assign them first
      // and cannot use any of the registers from pre-assigned uses.
      if (NeedToAssignLiveThroughs) {
        DEBUG(dbgs() << "Need to assign livethroughs\n");
        for (const MachineOperand &MO : MI.operands()) {
          if (!MO.isReg() || !MO.readsReg())
            continue;
          unsigned Reg = MO.getReg();
          if (TargetRegisterInfo::isPhysicalRegister(Reg)) {
            DEBUG(dbgs() << "mark used: " << PrintReg(Reg, TRI) << '\n');
            markRegUsedInInstr(Reg);
          }
        }
        // Assign early clobbers and tied registers first.
        for (unsigned I = 0, E = MI.getNumOperands(); I < E; ++I) {
          const MachineOperand &MO = MI.getOperand(I);
          if (!MO.isReg() || !MO.isDef())
            continue;
          if (!MO.isEarlyClobber() && !MO.isTied())
            continue;
          unsigned Reg = MO.getReg();
          if (TargetRegisterInfo::isVirtualRegister(Reg))
            defineLiveThroughVirtReg(MI, I, Reg);
        }
        // Reset UsedInInstr to just contain the preassigned defs.
        UsedInInstr.clear();
        for (const MachineOperand &MO : MI.operands()) {
          if (!MO.isReg() || !MO.isDef())
            continue;
          unsigned Reg = MO.getReg();
          if (TargetRegisterInfo::isPhysicalRegister(Reg))
            markRegUsedInInstr(Reg);
        }
      }

      // Assign virtual register defs.
      for (unsigned I = 0, E = MI.getNumOperands(); I < E; ++I) {
        MachineOperand &MO = MI.getOperand(I);
        if (!MO.isReg() || !MO.isDef())
          continue;
        unsigned Reg = MO.getReg();
        if (TargetRegisterInfo::isVirtualRegister(Reg))
          defineVirtReg(MI, I, Reg);
      }
    }

    // Free registers occupied by defs.
    // Iterate operands in reverse order, so we see the implicit super register
    // defs first (we added them earlier in case of <def,read-undef>).
    for (unsigned I = MI.getNumOperands(); I-- > 0;) {
      const MachineOperand &MO = MI.getOperand(I);
      if (!MO.isReg() || !MO.isDef())
        continue;
      // Leave tied operands alone; delay freeing of early clobbers.
      if (MO.isTied() || MO.isEarlyClobber())
        continue;
      unsigned Reg = MO.getReg();
      if (!Reg)
        continue;
      assert(TargetRegisterInfo::isPhysicalRegister(Reg));
      if (MRI->isReserved(Reg))
        continue;
      freePhysReg(Reg);
      unmarkRegUsedInInstr(Reg);
    }
  }

  // Displace clobbered registers.
  if (HasRegMask) {
    for (const MachineOperand &MO : MI.operands()) {
      if (MO.isRegMask()) {
        // MRI bookkeeping.
        MRI->addPhysRegsUsedFromRegMask(MO.getRegMask());
        // Displace clobbered values.
        // TODO: Anyway to avoid the loop over all registers?
        // We could just check the things in LiveVirtRegs, but we would also
        // need a list of live pre-assigned registers.
        for (unsigned Reg = 1, RegE = TRI->getNumRegs(); Reg != RegE; ++Reg)
          displacePhysReg(MI, Reg);
      }
    }
  }

  if (HasPhysRegUse) {
    for (const MachineOperand &MO : MI.operands()) {
      if (!MO.isReg() || !MO.readsReg())
        continue;
      unsigned Reg = MO.getReg();
      if (!TargetRegisterInfo::isPhysicalRegister(Reg))
        continue;
      if (MRI->isReserved(Reg))
        continue;
      usePhysReg(MI, Reg);
    }
  }

  // Allocate virtreg uses and insert reloads as necessary.
  for (unsigned I = 0; I < MI.getNumOperands(); ++I) {
    MachineOperand &MO = MI.getOperand(I);
    if (!MO.isReg() || !MO.isUse())
      continue;
    unsigned Reg = MO.getReg();
    if (TargetRegisterInfo::isVirtualRegister(Reg)) {
      if (MO.isUndef()) {
        allocVirtRegUndef(MO);
      } else {
        assert(!MO.isInternalRead() && "Bundles not supported");
        assert(MO.readsReg() && "reading use");
        useVirtReg(MI, I, Reg);
      }
    }
  }

  // Free early clobbers.
  if (HasEarlyClobber) {
    for (unsigned I = MI.getNumOperands(); I-- > 0; ) {
      const MachineOperand &MO = MI.getOperand(I);
      if (!MO.isReg() || !MO.isDef() || !MO.isEarlyClobber())
        continue;
      unsigned Reg = MO.getReg();
      if (!Reg)
        continue;
      assert(TargetRegisterInfo::isPhysicalRegister(Reg) &&
             "should have register assigned");
      freePhysReg(Reg);
    }
  }

  DEBUG(dbgs() << "<< " << MI);
  if (MI.isCopy() && MI.getOperand(0).getReg() == MI.getOperand(1).getReg() &&
      MI.getNumOperands() == 2) {
    DEBUG(dbgs() << "Mark identity copy for removal\n");
    Coalesced.push_back(&MI);
    ++NumCoalesced;
  }
}

void RegAllocFast::allocateBasicBlock(MachineBasicBlock &MBB) {
  this->MBB = &MBB;
  DEBUG(dbgs() << "\nAllocating " << MBB);

  PhysRegState.assign(TRI->getNumRegs(), regFree);
  assert(LiveVirtRegs.empty() && "Mapping not cleared from last block?");

  Coalesced.clear();

  // Otherwise, sequentially allocate each instruction in the MBB.
  for (MachineInstr &MI : make_range(MBB.rbegin(), MBB.rend())) {
    DEBUG(
      dbgs() << "\n>> " << MI << "Regs:";
      dumpState()
    );

    // Debug values are not allowed to change codegen in any way.
    if (MI.isDebugValue()) {
      bool ScanDbgValue = true;
      MachineInstr *DebugMI = &MI;
      while (ScanDbgValue) {
        ScanDbgValue = false;
        for (unsigned I = 0, E = DebugMI->getNumOperands(); I != E; ++I) {
          MachineOperand &MO = DebugMI->getOperand(I);
          if (!MO.isReg()) continue;
          unsigned Reg = MO.getReg();
          if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
          LiveRegMap::iterator LRI = findLiveVirtReg(Reg);
          if (LRI != LiveVirtRegs.end())
            setPhysReg(*DebugMI, I, LRI->PhysReg);
          else {
            int SS = StackSlotForVirtReg[Reg];
            if (SS == -1) {
              // We can't allocate a physreg for a DebugValue, sorry!
              DEBUG(dbgs() << "Unable to allocate vreg used by DBG_VALUE");
              MO.setReg(0);
            } else {
              // Modify DBG_VALUE now that the value is in a spill slot.
              bool IsIndirect = DebugMI->isIndirectDebugValue();
              if (IsIndirect)
                assert(DebugMI->getOperand(1).getImm() == 0 &&
                       "DBG_VALUE with nonzero offset");
#if 0
              const MDNode *Var = DebugMI->getDebugVariable();
              const MDNode *Expr = DebugMI->getDebugExpression();
              DebugLoc DL = DebugMI->getDebugLoc();
              MachineBasicBlock *MBB = DebugMI->getParent();
              assert(
                  cast<DILocalVariable>(Var)->isValidLocationForIntrinsic(DL) &&
                  "Expected inlined-at fields to agree");
              MachineInstr *NewDV = BuildMI(*MBB, MBB->erase(DebugMI), DL,
                                            TII->get(TargetOpcode::DBG_VALUE))
                                        .addFrameIndex(SS)
                                        .addImm(0U)
                                        .addMetadata(Var)
                                        .addMetadata(Expr);
              DEBUG(dbgs() << "Modifying debug info due to spill:"
                           << "\t" << *NewDV);
              // Scan NewDV operands from the beginning.
              DebugMI = NewDV;
              ScanDbgValue = true;
#else
              assert(false && "TODO: Update DBG_VALUE");
#endif
              break;
            }
          }
          LiveDbgValueMap[Reg].push_back(DebugMI);
        }
      }
      // Next instruction.
      continue;
    }

    allocateInstruction(MI);
  }

  DEBUG(
    dbgs() << "Begin Regs:";
    dumpState()
  );

  // Spill all physical registers holding virtual registers now.
  DEBUG(dbgs() << "Loading live registers at begin of block.\n");
  reloadAtBegin();

  LiveVirtRegs.clear();

  // Erase all the coalesced copies. We are delaying it until now because
  // LiveVirtRegs might refer to the instrs.
  for (MachineInstr *MI : Coalesced)
    MBB.erase(MI);

  DEBUG(MBB.dump());
}

/// Allocates registers for a function.
bool RegAllocFast::runOnMachineFunction(MachineFunction &MF) {
  DEBUG(dbgs() << "********** FAST REGISTER ALLOCATION **********\n"
               << "********** Function: " << MF.getName() << '\n');
  MRI = &MF.getRegInfo();
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  TRI = STI.getRegisterInfo();
  TII = STI.getInstrInfo();
  MFI = &MF.getFrameInfo();
  MRI->freezeReservedRegs(MF);
  RegClassInfo.runOnMachineFunction(MF);
  UsedInInstr.clear();
  UsedInInstr.setUniverse(TRI->getNumRegUnits());

  // initialize the virtual->physical register map to have a 'null'
  // mapping for all virtual registers
  unsigned NumVirtRegs = MRI->getNumVirtRegs();
  StackSlotForVirtReg.resize(NumVirtRegs);
  LiveVirtRegs.setUniverse(NumVirtRegs);
  MayLiveOut.clear();
  MayLiveOut.resize(NumVirtRegs);

  // Loop over all of the basic blocks, eliminating virtual register references
  for (MachineBasicBlock &MBB : MF)
    allocateBasicBlock(MBB);

  // All machine operands and other references to virtual registers have been
  // replaced. Remove the virtual registers.
  MRI->clearVirtRegs();

  StackSlotForVirtReg.clear();
  LiveDbgValueMap.clear();
  return true;
}

FunctionPass *llvm::createFastRegisterAllocator() {
  return new RegAllocFast();
}
