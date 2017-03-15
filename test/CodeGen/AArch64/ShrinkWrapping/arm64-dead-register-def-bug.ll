; RUN: llc -mtriple=arm64-apple-ios -enable-shrink-wrap2=true %s -o /dev/null -debug-only=shrink-wrap2 2>&1 | FileCheck %s
; FIXME: ShrinkWrap2: use MIR once we fix stack protector assert.
; REQUIRES: asserts
; This test makes sure that we don't convert callee save save / restores from
; store / load to push / pop
; (AArch64FrameLowering::onvertCalleeSaveRestoreToSPPrePostIncDec).

; CHECK-LABEL: testcase

; CHECK-NOT: This is not a register operand
; CHECK: BB#1 uses : %LR
; CHECK: **** Shrink-wrapping results ****
; CHECK-NEXT: BB#1: Saves: %LR, | Restores: %LR,
target triple = "arm64-apple-ios"

define void @testcase() {
bb1:
  br i1 undef, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  tail call void @foo()
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  tail call void @bar()
  ret void
}

declare void @foo()

declare void @bar()
