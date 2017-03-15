; RUN: llc -mcpu=cortex-a57 -aarch64-a57-fp-load-balancing-override=1 -aarch64-a57-fp-load-balancing-force-all -enable-misched=false -enable-post-misched=false -enable-shrink-wrap2=true -debug-only=shrink-wrap2 %s -o /dev/null 2>&1 | FileCheck %s
; FIXME: ShrinkWrap2: use MIR once we fix stack protector assert.
; Check that the internal state of AArch64 backend is modified in
; determineCalleeSaves before calling emitPrologue (i.e. AArch64FrameLowering).
; REQUIRE: asserts

; CHECK-LABEL: f5

; CHECK-NOT: unexpected function without stack frame but with FP

; CHECK: BB#1 uses : %LR
; CHECK: **** Shrink-wrapping results ****
; CHECK-NEXT: BB#1: Saves: %LR, | Restores: %LR,
target datalayout = "e-m:e-i64:64-i128:128-n32:64-S128"
target triple = "aarch64"

declare void @g()

define void @f5(float* nocapture %q) #0 {
entry:
  br i1 undef, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  tail call void @g()
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  store float undef, float* %q, align 4
  ret void
}

attributes #0 = { "no-frame-pointer-elim-non-leaf" }
