set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  CodeGen
  Core
  MC
  MIRParser
  Support
  Target
  )

add_llvm_unittest(MITests
  MITests.cpp
  LiveIntervalTest.cpp
  FinalizeBundleTest.cpp
  )
