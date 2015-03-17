//===-- llc.cpp - Implement the LLVM Native Code Generator ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Small utility to dump various information about an llvm target.
//
//===----------------------------------------------------------------------===//


#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include <memory>
using namespace llvm;

static cl::opt<std::string>
TargetTriple("mtriple", cl::desc("Override target triple for module"));

static void printRegisterInfo(const TargetRegisterInfo &TRI) {
  printf("=== Classes ===\n");
  for (auto I = TRI.regclass_begin(), E = TRI.regclass_end(); I != E; ++I) {
    const TargetRegisterClass *Cls = *I;
    printf("* %s\n", TRI.getRegClassName(Cls));
    printf("\tLaneMask           %08X\n", Cls->getLaneMask());
    printf("\tHasDisjunctSubRegs %s\n", Cls->HasDisjunctSubRegs?"true":"false");
    printf("\tCoveredBySubRegs   %s\n", Cls->CoveredBySubRegs?"true":"false");
    printf("\tNumRegisters:      %u\n", Cls->getNumRegs());
    printf("\tRegisters:");
    const unsigned limit = 16;
    for (unsigned i = 0; i < limit && i < Cls->getNumRegs(); ++i) {
      printf(" %s", TRI.getName(Cls->getRegister(i)));
    }
    if (Cls->getNumRegs() >= limit)
      printf(" ...");
    printf("\n");

    printf("\tSubRegs: ");
    for (unsigned i = 1; i < TRI.getNumSubRegIndices(); ++i) {
      const TargetRegisterClass *SC = TRI.getSubClassWithSubReg(Cls, i);
      if (SC == Cls)
        printf(" %s", TRI.getSubRegIndexName(i));
    }
    printf("\n");

    printf("\tPartial SubRegs:");
    for (unsigned i = 1; i < TRI.getNumSubRegIndices(); ++i) {
      const TargetRegisterClass *SC = TRI.getSubClassWithSubReg(Cls, i);
      if (SC == nullptr || SC == Cls)
        continue;
      printf(" %s->%s", TRI.getSubRegIndexName(i), TRI.getRegClassName(SC));
    }
    printf("\n");
  }
  printf("=== SubReg Indices ===\n");
  for (unsigned i = 1; i < TRI.getNumSubRegIndices(); ++i) {
    printf("SubIdx[%2u]: M %08X %s\n", i,
           TRI.getSubRegIndexLaneMask(i),
           TRI.getSubRegIndexName(i));
  }
  printf("=== Misc ===\n");
  printf("Covering    Lanes: %08X\n", TRI.getCoveringLanes());
  printf("NonCovering Lanes: %08X\n", ~TRI.getCoveringLanes());
  printf("NumRegisters:      %u\n", TRI.getNumRegs());
  printf("NumRegUnits:       %u\n", TRI.getNumRegUnits());
  printf("NumSubRegIndices:  %u\n", TRI.getNumSubRegIndices());
}

static int printRegisterInfos(const char **argv) {
  if (MCPU == "native")
    MCPU = sys::getHostCPUName();

  Triple TheTriple;
  if (!TargetTriple.empty())
    TheTriple = Triple(Triple::normalize(TargetTriple));
  if (TheTriple.getTriple().empty())
    TheTriple.setTriple(sys::getDefaultTargetTriple());
  std::string Error;
  const Target *TheTarget = TargetRegistry::lookupTarget(MArch, TheTriple,
                                                         Error);
  if (!TheTarget) {
    errs() << argv[0] << ": " << Error;
    return 1;
  }

  TargetOptions Options = InitTargetOptionsFromCodeGenFlags();
  std::unique_ptr<TargetMachine> TM(TheTarget->createTargetMachine(
      TheTriple.getTriple(), MCPU, "", Options, RelocModel, CMModel,
      CodeGenOpt::Default));

  printf("Target: %s\n", TheTriple.normalize().c_str());

  // Create an empty function
  LLVMContext Context;
  FunctionType *FTy = TypeBuilder<void(), true>::get(Context);
  Function *F = Function::Create(FTy, GlobalValue::ExternalLinkage);

  const TargetRegisterInfo *TRI = TM->getSubtargetImpl(*F)->getRegisterInfo();
  printRegisterInfo(*TRI);
  return 0;
}

int main(int argc, const char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  // Enable debug stream buffering.
  EnableDebugBuffering = true;

  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  // Initialize targets first, so that --version shows registered targets.
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  // Initialize codegen and IR passes used by llc so that the -print-after,
  // -print-before, and -stop-after options work.
  PassRegistry *Registry = PassRegistry::getPassRegistry();
  initializeCore(*Registry);
  initializeCodeGen(*Registry);

  // Register the target printer for --version.
  cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

  cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");

  return printRegisterInfos(argv);
}
