; RUN: llc -mtriple=aarch64-- -O0 -global-isel -global-isel-abort=0 -verify-machineinstrs -enable-shrink-wrap2=true -debug-only=shrink-wrap2 %s -o - 2>&1 | FileCheck %s
; FIXME: ShrinkWrap2: use MIR once we fix stack protector assert.
; REQUIRES: asserts
; This test causes the first MBB ID to be 2, which provoked a bug.

; CHECK-LABEL: ABIi128

; CHECK: BB#2 uses : %LR
; CHECK: **** Shrink-wrapping results ****
; CHECK-NEXT: BB#2: Saves: %LR, | Restores: %LR,

target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "aarch64--"

define i128 @ABIi128(i128 %arg1) {
  %res = fptoui fp128 undef to i128
  ret i128 %res
}
