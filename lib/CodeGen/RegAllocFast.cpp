//===- RegAllocFast.cpp - A fast register allocator for debug code --------===//
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
//  - Could invent a cooler version of getUniqueVRegDef() that ignores
//    <def,tied> when tied to the same vreg.
//  - Post pass to remove spills that we happened to never reload from?
//  - TODO: review earlyclobber + tied combination...
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SparseSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <tuple>
#include <vector>

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
      MachineInstr *LastUse = nullptr; ///< Last instr to use reg.
      unsigned VirtReg;                ///< Virtual register number.
      MCPhysReg PhysReg = 0;           ///< Currently held here.
      bool LiveOut = false;            ///< Register is possibly live out.
      bool Reloaded = false;           ///< Register was reloaded.

      explicit LiveReg(unsigned VirtReg) : VirtReg(VirtReg) {
        assert(TargetRegisterInfo::isVirtualRegister(VirtReg));
      }

      unsigned getSparseSetIndex() const {
        return TargetRegisterInfo::virtReg2Index(VirtReg);
      }
    };

    using LiveRegMap = SparseSet<LiveReg>;

    /// This map contains entries for each virtual register that is currently
    /// available in a physical register.
    LiveRegMap LiveVirtRegs;

    DenseMap<unsigned, SmallVector<MachineInstr *, 4>> LiveDbgValueMap;

    /// Caches result of mayLiveOut() queries.
    BitVector MayLiveOut;

    /// Track the state of a register unit.
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

    /// One of the RegState enums for each register unit.
    std::vector<unsigned> RegUnitState;

    SmallVector<MachineInstr *, 32> Coalesced;

    /// Set of register units.
    using UsedInInstrSet = SparseSet<unsigned>;

    /// Set of register units that are used in the current instruction, and so
    /// cannot be allocated.
    UsedInInstrSet UsedInInstr;

    void setPhysRegState(MCPhysReg PhysReg, unsigned NewState);
    bool isPhysRegFree(MCPhysReg PhysReg) const;

    void setPhysRegStateFree(MCPhysReg PhysReg);

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
    bool runOnMachineFunction(MachineFunction &MF) override;
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
    void assignVirtToPhysReg(LiveReg &, MCPhysReg PhysReg);

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
    void setPhysReg(MachineInstr &MI, MachineOperand &MO, MCPhysReg PhysReg);

    bool mayLiveOut(unsigned VirtReg);
    unsigned traceCopies(unsigned VirtReg) const;
    unsigned traceCopyChain(unsigned Reg) const;

    void dumpState();
  };

} // end anonymous namespace

char RegAllocFast::ID = 0;

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
  LLVM_DEBUG(dbgs() << "Spilling " << printReg(VirtReg, TRI)
                    << " in " << printReg(AssignedReg, TRI));
  int FI = getStackSpaceFor(VirtReg);
  LLVM_DEBUG(dbgs() << " to stack slot #" << FI << "\n");

  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  TII->storeRegToStackSlot(*MBB, Before, AssignedReg, Kill, FI, &RC, TRI);
  ++NumStores;

  // If this register is used by DBG_VALUE then insert new DBG_VALUE to
  // identify spilled location as the place to find corresponding variable's
  // value.
  SmallVectorImpl<MachineInstr *> &LRIDbgValues = LiveDbgValueMap[VirtReg];
  for (MachineInstr *DBG : LRIDbgValues) {
    MachineInstr *NewDV = buildDbgValueForSpill(*MBB, Before, *DBG, FI);
    assert(NewDV->getParent() == MBB && "dangling parent pointer");
    (void)NewDV;
    LLVM_DEBUG(dbgs() << "Inserting debug info due to spill:\n" << *NewDV);
  }
  // Now this register is spilled there is should not be any DBG_VALUE
  // pointing to this register because they are all pointing to spilled value
  // now.
  LRIDbgValues.clear();
}

/// Get basic block begin insertion point.
/// This is not just MBB.begin() because surprisingly we have EH_LABEL
/// instructions marking the begin of a basic block. This means we must insert
/// new instructions after such labels...
static MachineBasicBlock::iterator
getMBBBeginInsertionPoint(MachineBasicBlock &MBB) {
  MachineBasicBlock::iterator I = MBB.begin();
  while (I != MBB.end() && I->isLabel())
    ++I;
  return I;
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
  MachineBasicBlock::iterator InsertBefore = getMBBBeginInsertionPoint(*MBB);
  for (const LiveReg &LR : LiveVirtRegs) {
    MCPhysReg PhysReg = LR.PhysReg;
    if (PhysReg == 0)
      continue;

    unsigned FirstUnit = *MCRegUnitIterator(PhysReg, TRI);
    if (RegUnitState[FirstUnit] == regLiveIn)
      continue;

    assert(MBB != &MBB->getParent()->front() && "no reload in start block");
    reload(InsertBefore, LR.VirtReg, PhysReg);
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
  displacePhysReg(MI, Reg);
  setPhysRegState(Reg, regPreAssigned);
}

/// Mark PhysReg as reserved or free after spilling any virtregs. This is very
/// similar to defineVirtReg except the physreg is reserved instead of
/// allocated.
void RegAllocFast::displacePhysReg(MachineInstr &MI, MCPhysReg PhysReg) {
  for (MCRegUnitIterator UI(PhysReg, TRI); UI.isValid(); ++UI) {
    unsigned Unit = *UI;
    switch (unsigned VirtReg = RegUnitState[Unit]) {
    default: {
        LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
        assert(LRI != LiveVirtRegs.end() && "datastructures in sync");
        MachineBasicBlock::iterator ReloadBefore =
            std::next((MachineBasicBlock::iterator)MI.getIterator());
        reload(ReloadBefore, VirtReg, LRI->PhysReg);

        setPhysRegStateFree(LRI->PhysReg);
        LRI->PhysReg = 0;
        LRI->Reloaded = true;
      }
      break;

    case regPreAssigned:
      RegUnitState[Unit] = regFree;
      break;
    case regFree:
      break;
    }
  }
}

void RegAllocFast::setPhysRegState(MCPhysReg PhysReg, unsigned NewState) {
  for (MCRegUnitIterator UI(PhysReg, TRI); UI.isValid(); ++UI)
    RegUnitState[*UI] = NewState;
}

void RegAllocFast::setPhysRegStateFree(MCPhysReg PhysReg) {
  setPhysRegState(PhysReg, regFree);
}

void RegAllocFast::freePhysReg(MCPhysReg PhysReg) {
  LLVM_DEBUG(dbgs() << "Freeing " << printReg(PhysReg, TRI) << ':');

  unsigned FirstUnit = *MCRegUnitIterator(PhysReg, TRI);
  switch (unsigned VirtReg = RegUnitState[FirstUnit]) {
  case regFree:
    LLVM_DEBUG(dbgs() << '\n');
    return;
  case regPreAssigned:
    LLVM_DEBUG(dbgs() << '\n');
    setPhysRegStateFree(PhysReg);
    return;
  default: {
      LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
      assert(LRI != LiveVirtRegs.end());
      LLVM_DEBUG(dbgs() << ' ' << printReg(LRI->VirtReg, TRI) << '\n');
      setPhysRegStateFree(LRI->PhysReg);
      LRI->PhysReg = 0;
    }
    return;
  }
}

/// Return the cost of spilling clearing out PhysReg and aliases so it is free
/// for allocation. Returns 0 when PhysReg is free or disabled with all aliases
/// disabled - it can be allocated directly.
/// \returns spillImpossible when PhysReg or an alias can't be spilled.
unsigned RegAllocFast::calcSpillCost(MCPhysReg PhysReg) const {
  for (MCRegUnitIterator UI(PhysReg, TRI); UI.isValid(); ++UI) {
    switch (unsigned VirtReg = RegUnitState[*UI]) {
    case regFree:
      break;
    case regPreAssigned:
      LLVM_DEBUG(dbgs() << "Cannot spill pre-assigned "
                        << printReg(PhysReg, TRI) << '\n');
      return spillImpossible;
    default: {
      LiveRegMap::const_iterator I = findLiveVirtReg(VirtReg);
      assert(I != LiveVirtRegs.end() && "Missing VirtReg entry");
      // TODO: it's not correct to return here, we may have additional
      // virtregs assigned to other units or even have a preassigned bit in
      // another unit...
      return I->Reloaded ? spillClean : spillDirty;
    }
    }
  }
  return 0;
}

/// This method updates local state so that we know that PhysReg is the
/// proper container for VirtReg now.  The physical register must not be used
/// for anything else when this is called.
void RegAllocFast::assignVirtToPhysReg(LiveReg &LR, MCPhysReg PhysReg) {
  LLVM_DEBUG(dbgs() << "Assigning " << printReg(LR.VirtReg, TRI) << " to "
                    << printReg(PhysReg, TRI) << "\n");
  assert(LR.PhysReg == 0 && "Already assigned a physreg");
  assert(PhysReg != 0 && "Trying to assign no register");
  LR.PhysReg = PhysReg;
  setPhysRegState(PhysReg, LR.VirtReg);
}

static bool isCoalescable(const MachineInstr &MI) {
  return MI.isCopy() && MI.getOperand(0).getSubReg() == 0 &&
         MI.getOperand(1).getSubReg() == 0;
}

unsigned RegAllocFast::traceCopyChain(unsigned Reg) const {
  static const unsigned ChainLengthLimit = 3;
  unsigned C = 0;
  do {
    if (TargetRegisterInfo::isPhysicalRegister(Reg))
      return Reg;
    assert(TargetRegisterInfo::isVirtualRegister(Reg));

    MachineInstr *VRegDef = MRI->getUniqueVRegDef(Reg);
    if (VRegDef == nullptr || !isCoalescable(*VRegDef))
      return 0;
    Reg = VRegDef->getOperand(1).getReg();
  } while(++C <= ChainLengthLimit);
  return 0;
}

/// Check if any of \p VirtReg's definitions is a copy. If it is follow the
/// chain of copies to check whether we reach a physical register we can
/// coalesce with.
unsigned RegAllocFast::traceCopies(unsigned VirtReg) const {
  static const unsigned DefLimit = 3;
  unsigned C = 0;
  for (const MachineInstr &MI : MRI->def_instructions(VirtReg)) {
    if (isCoalescable(MI)) {
      unsigned Reg = MI.getOperand(1).getReg();
      Reg = traceCopyChain(Reg);
      if (Reg != 0)
        return Reg;
    }
    if (++C >= DefLimit)
      break;
  }
  return 0;
}

bool RegAllocFast::isPhysRegFree(MCPhysReg PhysReg) const {
  for (MCRegUnitIterator UI(PhysReg, TRI); UI.isValid(); ++UI) {
    if (RegUnitState[*UI] != regFree)
      return false;
  }
  return true;
}

/// Allocates a physical register for VirtReg.
void RegAllocFast::allocVirtReg(MachineInstr &MI, LiveRegMap::iterator LRI,
                                unsigned Hint0) {
  const unsigned VirtReg = LRI->VirtReg;
  assert(LRI->PhysReg == 0);

  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  LLVM_DEBUG(dbgs() << "Search register for " << printReg(VirtReg)
                    << " in class " << TRI->getRegClassName(&RC) << "\n");

  // Try first hint.
  if (TargetRegisterInfo::isPhysicalRegister(Hint0) &&
      MRI->isAllocatable(Hint0) && RC.contains(Hint0) &&
      !isRegUsedInInstr(Hint0)) {
    // Take hint if the register is currently free.
    if (isPhysRegFree(Hint0)) {
      LLVM_DEBUG(dbgs() << "\tPreferred Register 0: " << printReg(Hint0, TRI)
                        << '\n');
      assignVirtToPhysReg(*LRI, Hint0);
      return;
    } else {
      LLVM_DEBUG(dbgs() << "\tPreferred Register 0: " << printReg(Hint0, TRI)
                        << "occupied\n");
    }
  }

  // Take hint when possible.
  unsigned Hint1 = traceCopies(VirtReg);
  if (TargetRegisterInfo::isPhysicalRegister(Hint1) &&
      MRI->isAllocatable(Hint1) && RC.contains(Hint1) &&
      !isRegUsedInInstr(Hint1)) {
    // Take hint if the register is currently free.
    if (isPhysRegFree(Hint1)) {
      LLVM_DEBUG(dbgs() << "\tPreferred Register 1: " << printReg(Hint1, TRI)
                        << '\n');
      assignVirtToPhysReg(*LRI, Hint1);
      return;
    } else {
      LLVM_DEBUG(dbgs() << "\tPreferred Register 1: " << printReg(Hint1, TRI)
                        << "occupied\n");
    }
  } else {
    Hint1 = 0;
  }

  MCPhysReg BestReg = 0;
  unsigned BestCost = spillImpossible;
  ArrayRef<MCPhysReg> AllocationOrder = RegClassInfo.getOrder(&RC);
  for (MCPhysReg PhysReg : AllocationOrder) {
    LLVM_DEBUG(dbgs() << "\tRegister: " << printReg(PhysReg, TRI) << ' ');
    if (isRegUsedInInstr(PhysReg)) {
      LLVM_DEBUG(dbgs() << "already used in instr.\n");
      continue;
    }

    unsigned Cost = calcSpillCost(PhysReg);
    LLVM_DEBUG(dbgs() << "Cost: " << Cost << " BestCost: " << BestCost << "\n");
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
    // Nothing we can do: Report an error and keep going with an invalid
    // allocation.
    if (MI.isInlineAsm())
      MI.emitError("inline assembly requires more registers than available");
    else
      MI.emitError("ran out of registers during register allocation");
    BestReg = *AllocationOrder.begin();
    setPhysRegState(BestReg, regFree);
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

/// Heuristic to identify virtual registers not living out of current block.
bool RegAllocFast::mayLiveOut(unsigned VirtReg) {
  unsigned C = 0;
  if (MayLiveOut.test(TargetRegisterInfo::virtReg2Index(VirtReg)))
    goto MayLO;

  static const unsigned Limit = 8;
  for (const MachineOperand &Use : MRI->reg_nodbg_operands(VirtReg)) {
    if (Use.getParent()->getParent() != MBB || ++C >= Limit) {
      MayLiveOut.set(TargetRegisterInfo::virtReg2Index(VirtReg));
      goto MayLO;
    }
  }
  return false;

MayLO:
  // No vregs live out without successors (such as the return block).
  return !MBB->succ_empty();
}

/// Variation of defineVirtReg() with special handling for livethrough regs
/// (tied or earlyclobber) that may interfere with preassigned uses.
void RegAllocFast::defineLiveThroughVirtReg(MachineInstr &MI, unsigned OpNum,
                                            unsigned VirtReg) {
  LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
  if (LRI != LiveVirtRegs.end()) {
    MCPhysReg PrevReg = LRI->PhysReg;
    if (PrevReg != 0 && isRegUsedInInstr(PrevReg)) {
      LLVM_DEBUG(dbgs() << "Need new assignment for " << printReg(PrevReg, TRI)
                        << " (tied/earlyclobber resolution)\n");
      freePhysReg(PrevReg);
      LRI->PhysReg = 0;
      allocVirtReg(MI, LRI, 0);
      MachineBasicBlock::iterator InsertBefore =
        std::next((MachineBasicBlock::iterator)MI.getIterator());
      LLVM_DEBUG(dbgs() << "Copy " << printReg(LRI->PhysReg, TRI) << " to "
                        << printReg(PrevReg, TRI) << "\n");
      BuildMI(*MBB, InsertBefore, MI.getDebugLoc(),
              TII->get(TargetOpcode::COPY), PrevReg)
        .addReg(LRI->PhysReg, llvm::RegState::Kill);
    }
  }
  return defineVirtReg(MI, OpNum, VirtReg);
}

/// Allocates a register for VirtReg definition. Typically the register is
/// already assigned from a use of the virtreg, however we still need to
/// perform an allocation if:
/// - It is a dead definition without any uses.
/// - The value is live out and all uses are in different basic blocks.
void RegAllocFast::defineVirtReg(MachineInstr &MI, unsigned OpNum,
                                 unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  MachineOperand &MO = MI.getOperand(OpNum);
  LiveRegMap::iterator LRI;
  bool New;
  std::tie(LRI, New) = LiveVirtRegs.insert(LiveReg(VirtReg));
  if (New) {
    // Note that we have to prime the LiveOut cache, even if a dead flags is
    // set.
    bool MayLO = mayLiveOut(VirtReg);
    if (!MO.isDead()) {
      if (MayLO)
        LRI->LiveOut = true;
      else {
        // It is a dead def without the dead flag; add the flag now.
        MO.setIsDead(true);
      }
    }
  }
  if (LRI->PhysReg == 0)
    allocVirtReg(MI, LRI, 0);
  else {
    assert(!isRegUsedInInstr(LRI->PhysReg) && "TODO: preassign mismatch");
    LLVM_DEBUG(dbgs() << "In def of " << printReg(VirtReg, TRI)
                      << " use existing assignment to "
                      << printReg(LRI->PhysReg, TRI) << '\n');
  }

  MCPhysReg PhysReg = LRI->PhysReg;
  assert(PhysReg != 0 && "Register not assigned");
  if (LRI->Reloaded || LRI->LiveOut) {
    if (!MI.isImplicitDef()) {
      MachineBasicBlock::iterator SpillBefore =
          std::next((MachineBasicBlock::iterator)MI.getIterator());
      LLVM_DEBUG(dbgs() << "Spill Reason: LO: " << LRI->LiveOut << " RL: "
                        << LRI->Reloaded << '\n');
      bool Kill = LRI->LastUse == nullptr;
      spillVirtReg(SpillBefore, VirtReg, PhysReg, Kill);
      LRI->LastUse = nullptr;
    }
    LRI->LiveOut = false;
    LRI->Reloaded = false;
  }
  markRegUsedInInstr(PhysReg);
  setPhysReg(MI, MO, PhysReg);
}

/// Allocates a register for a VirtReg use.
void RegAllocFast::useVirtReg(MachineInstr &MI, unsigned OpNum,
                              unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  MachineOperand &MO = MI.getOperand(OpNum);
  LiveRegMap::iterator LRI;
  bool New;
  std::tie(LRI, New) = LiveVirtRegs.insert(LiveReg(VirtReg));
  if (New) {
    MachineOperand &MO = MI.getOperand(OpNum);
    // Note that we have to prime the LiveOut cache, even if a kill flag is set.
    bool MayLO = mayLiveOut(VirtReg);
    if (!MO.isKill()) {
      if (MayLO)
        LRI->LiveOut = true;
      else {
        // It is a last (killing) use without the kill flag; add the flag now.
        MO.setIsKill(true);
      }
    }
  } else {
    assert((!MO.isKill() || LRI->LastUse == &MI) && "Invalid kill flag");
  }
  if (LRI->PhysReg == 0) {
    assert(!MO.isTied() && "tied op should be allocated");
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
    assert((LRI->LastUse == &MI || MO.isTied() ||
            !isRegUsedInInstr(LRI->PhysReg)) && "assigned reg free");
  }

  LRI->LastUse = &MI;
  markRegUsedInInstr(LRI->PhysReg);
  setPhysReg(MI, MO, LRI->PhysReg);
}

void RegAllocFast::reload(MachineBasicBlock::iterator Before, unsigned VirtReg,
                          MCPhysReg PhysReg) {
  LLVM_DEBUG(dbgs() << "Reloading " << printReg(VirtReg, TRI) << " into "
                    << printReg(PhysReg, TRI) << "\n");
  int FI = getStackSpaceFor(VirtReg);
  const TargetRegisterClass &RC = *MRI->getRegClass(VirtReg);
  TII->loadRegFromStackSlot(*MBB, Before, PhysReg, FI, &RC, TRI);
  ++NumLoads;
}

/// Changes operand OpNum in MI the refer the PhysReg, considering subregs. This
/// may invalidate any operand pointers.  Return true if the operand kills its
/// register.
void RegAllocFast::setPhysReg(MachineInstr &MI, MachineOperand &MO,
                              MCPhysReg PhysReg) {
  if (!MO.getSubReg()) {
    MO.setReg(PhysReg);
    MO.setIsRenamable(true);
    return;
  }

  // Handle subregister index.
  MO.setReg(PhysReg ? TRI->getSubReg(PhysReg, MO.getSubReg()) : 0);
  MO.setIsRenamable(true);
  // Note: We leave the subreg number around a little longer in case of defs.
  // This is so that the register freeing logic in allocateInstruction can still
  // recognize this as subregister defs. The code there will clear the number.
  if (!MO.isDef())
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
    if (MO.isDead())
      MI.addRegisterDead(PhysReg, TRI, true);
    else
      MI.addRegisterDefined(PhysReg, TRI);
  }
}

#ifndef NDEBUG
void RegAllocFast::dumpState() {
  for (unsigned Unit = 1, UnitE = TRI->getNumRegUnits(); Unit != UnitE;
       ++Unit) {
    switch(unsigned VirtReg = RegUnitState[Unit]) {
    case regFree:
      break;
    case regPreAssigned:
      dbgs() << " " << printRegUnit(Unit, TRI) << "[P]";
      break;
    case regLiveIn:
      llvm_unreachable("Should not have regLiveIn in map");
    default: {
      dbgs() << ' ' << printRegUnit(Unit, TRI) << '=' << printReg(VirtReg);
      LiveRegMap::iterator I = findLiveVirtReg(VirtReg);
      assert(I != LiveVirtRegs.end() && "have LiveVirtRegs entry");
      if (I->LiveOut || I->Reloaded) {
        dbgs() << '[';
        if (I->LiveOut) dbgs() << 'O';
        if (I->Reloaded) dbgs() << 'R';
        dbgs() << ']';
      }
      assert(TRI->hasRegUnit(I->PhysReg, Unit) && "inverse mapping present");
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
             "mapped to physreg");
      for (MCRegUnitIterator UI(PhysReg, TRI); UI.isValid(); ++UI)
        assert(RegUnitState[*UI] == VirtReg && "inverse map valid");
    }
  }
}
#endif

void RegAllocFast::allocateInstruction(MachineInstr &MI) {
  // The basic algorithm here is:
  // 1. Mark registers of def operands as free
  // 2. Allocate registers to use operands and place reload instructions for
  //    registers displaced by the allocation.
  //
  // However we need to handle some corner cases:
  // - pre-assigned defs and uses need to be handled before the other def/use
  //   operands are processed to avoid the allocation heuristics clashing with
  //   the pre-assignment.
  // - The "free def operands" step has to come last instead of first for tied
  //   operands and early-clobbers.

  UsedInInstr.clear();

  // Scan for special cases; Apply pre-assigned register defs to state.
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
          if (MO.isTied() || (MO.getSubReg() != 0 && !MO.isUndef()))
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
        LLVM_DEBUG(dbgs() << "Need to assign livethroughs\n");
        for (const MachineOperand &MO : MI.operands()) {
          if (!MO.isReg() || !MO.readsReg())
            continue;
          unsigned Reg = MO.getReg();
          if (TargetRegisterInfo::isPhysicalRegister(Reg)) {
            LLVM_DEBUG(dbgs() << "mark used: " << printReg(Reg, TRI) << '\n');
            markRegUsedInInstr(Reg);
          }
        }
        // Assign early clobbers and tied registers first.
        for (unsigned I = 0, E = MI.getNumOperands(); I < E; ++I) {
          const MachineOperand &MO = MI.getOperand(I);
          if (!MO.isReg() || !MO.isDef())
            continue;
          if (!MO.isEarlyClobber() && !MO.isTied() &&
              (MO.getSubReg() == 0 || MO.isUndef()))
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
      MachineOperand &MO = MI.getOperand(I);
      if (!MO.isReg() || !MO.isDef())
        continue;
      // Leave tied operands alone; delay freeing of early clobbers.
      if (MO.isTied() || MO.isEarlyClobber())
        continue;
      // subreg defs don't free the full register. We left the subreg number
      // around as a marker in setPhysReg() to recognize this case here.
      if (MO.getSubReg() != 0) {
        MO.setSubReg(0);
        continue;
      }
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

        // Displace clobbered registers.
        const uint32_t *Mask = MO.getRegMask();
        for (LiveRegMap::iterator LRI = LiveVirtRegs.begin(),
             LRIE = LiveVirtRegs.end(); LRI != LRIE; ++LRI) {
          MCPhysReg PhysReg = LRI->PhysReg;
          if (PhysReg != 0 && MachineOperand::clobbersPhysReg(Mask, PhysReg))
            displacePhysReg(MI, PhysReg);
        }
      }
    }
  }

  // Apply pre-assigned register uses to state.
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
      MachineOperand &MO = MI.getOperand(I);
      if (!MO.isReg() || !MO.isDef() || !MO.isEarlyClobber())
        continue;
      // subreg defs don't free the full register. We left the subreg number
      // around as a marker in setPhysReg() to recognize this case here.
      if (MO.getSubReg() != 0) {
        MO.setSubReg(0);
        continue;
      }

      unsigned Reg = MO.getReg();
      if (!Reg)
        continue;
      assert(TargetRegisterInfo::isPhysicalRegister(Reg) &&
             "should have register assigned");

      // We sometimes get odd situations like:
      //    early-clobber %x0 = INSTRUCTION %x0
      // which is semantically questionable as the early-clobber should
      // apply before the use. But in practice we consider the use to
      // happen before the early clobber now. Don't free the early clobber
      // register in this case.
      if (MI.readsRegister(Reg, TRI))
        continue;

      freePhysReg(Reg);
    }
  }

  LLVM_DEBUG(dbgs() << "<< " << MI);
  if (MI.isCopy() && MI.getOperand(0).getReg() == MI.getOperand(1).getReg() &&
      MI.getNumOperands() == 2) {
    LLVM_DEBUG(dbgs() << "Mark identity copy for removal\n");
    Coalesced.push_back(&MI);
    ++NumCoalesced;
  }
}

void RegAllocFast::allocateBasicBlock(MachineBasicBlock &MBB) {
  this->MBB = &MBB;
  LLVM_DEBUG(dbgs() << "\nAllocating " << MBB);

  RegUnitState.assign(TRI->getNumRegUnits(), regFree);
  assert(LiveVirtRegs.empty() && "Mapping not cleared from last block?");

  Coalesced.clear();

  // Traverse block in reverse order allocating instructions one by one.
  for (MachineInstr &MI : make_range(MBB.rbegin(), MBB.rend())) {
    LLVM_DEBUG(
      dbgs() << "\n>> " << MI << "Regs:";
      dumpState()
    );

    // Debug values are not allowed to change codegen in any way.
    if (MI.isDebugValue()) {
      MachineInstr *DebugMI = &MI;
      MachineOperand &MO = DebugMI->getOperand(0);

      // Ignore DBG_VALUEs that aren't based on virtual registers. These are
      // mostly constants and frame indices.
      if (!MO.isReg())
        continue;
      unsigned Reg = MO.getReg();
      if (!TargetRegisterInfo::isVirtualRegister(Reg))
        continue;

      // See if this virtual register has already been allocated to a physical
      // register or spilled to a stack slot.
      LiveRegMap::iterator LRI = findLiveVirtReg(Reg);
      if (LRI != LiveVirtRegs.end() && LRI->PhysReg != 0)
        setPhysReg(*DebugMI, MO, LRI->PhysReg);
      else {
        int SS = StackSlotForVirtReg[Reg];
        if (SS != -1) {
          // Modify DBG_VALUE now that the value is in a spill slot.
          updateDbgValueForSpill(*DebugMI, SS);
          LLVM_DEBUG(dbgs() << "Modifying debug info due to spill:"
                            << "\t" << *DebugMI);
          continue;
        }

        // We can't allocate a physreg for a DebugValue, sorry!
        LLVM_DEBUG(dbgs() << "Unable to allocate vreg used by DBG_VALUE");
        MO.setReg(0);
      }

      // If Reg hasn't been spilled, put this DBG_VALUE in LiveDbgValueMap so
      // that future spills of Reg will have DBG_VALUEs.
      LiveDbgValueMap[Reg].push_back(DebugMI);
      continue;
    }

    allocateInstruction(MI);
  }

  LLVM_DEBUG(
    dbgs() << "Begin Regs:";
    dumpState()
  );

  // Spill all physical registers holding virtual registers now.
  LLVM_DEBUG(dbgs() << "Loading live registers at begin of block.\n");
  reloadAtBegin();

  LiveVirtRegs.clear();

  // Erase all the coalesced copies. We are delaying it until now because
  // LiveVirtRegs might refer to the instrs.
  for (MachineInstr *MI : Coalesced)
    MBB.erase(MI);

  LLVM_DEBUG(MBB.dump());
}

/// Allocates registers for a function.
bool RegAllocFast::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** FAST REGISTER ALLOCATION **********\n"
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
