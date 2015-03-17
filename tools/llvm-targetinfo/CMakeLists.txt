set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  AsmPrinter
  CodeGen
  Core
  MC
  SelectionDAG
  Support
  )

# Support plugins.
set(LLVM_NO_DEAD_STRIP 1)

add_llvm_tool(llvm-targetinfo
  llvm-targetinfo.cpp
  )
set_target_properties(llvm-targetinfo PROPERTIES ENABLE_EXPORTS 1)
