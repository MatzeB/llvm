#include <memory>
#include <functional>
#include "llvm/ADT/StringRef.h"

namespace llvm {
class LLVMContext;
class LiveIntervals;
class MIRParser;
class MachineFunction;
class Module;
class Pass;
class TargetMachine;
namespace legacy {
class PassManagerBase;
}
}

std::unique_ptr<llvm::TargetMachine>
createTargetMachine(const char *TripleString);

typedef std::function<void(llvm::MachineFunction&,
                           llvm::LiveIntervals&)> TestFunction;

llvm::Pass *createTestPass(TestFunction F);

std::unique_ptr<llvm::Module> parseMIR(llvm::LLVMContext &Context,
    llvm::legacy::PassManagerBase &PM, std::unique_ptr<llvm::MIRParser> &MIR,
    const llvm::TargetMachine &TM,
    llvm::StringRef MIRCode, const char *FuncName);
