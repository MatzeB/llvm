; RUN: llc -verify-machineinstrs -mtriple=x86_64-unknown-unknown -o - %s | FileCheck --check-prefix=CHECK --check-prefix=OPT %s
; RUN: llc -O0 -verify-machineinstrs -mtriple=x86_64-unknown-unknown -o - %s | FileCheck %s

; Parameter with swiftself should be allocated to r12.
; CHECK-LABEL: swiftself_param:
; CHECK: movq %r12, %rax
define i8 *@swiftself_param(i8* swiftself %addr0) {
    ret i8 *%addr0
}

; Check that r12 is used to pass a swiftself argument.
; CHECK-LABEL: call_swiftself:
; CHECK: movq %rdi, %r12
; CHECK: callq {{_?}}swiftself_param
define i8 *@call_swiftself(i8* %arg) {
  %res = call i8 *@swiftself_param(i8* swiftself %arg)
  ret i8 *%res
}

; r12 should be saved by the callee even if used for swiftself
; CHECK-LABEL: swiftself_clobber:
; CHECK: pushq %r12
; ...
; CHECK: popq %r12
define i8 *@swiftself_clobber(i8* swiftself %addr0) {
  call void asm sideeffect "nop", "~{r12}"()
  ret i8 *%addr0
}

; Demonstrate that we do not need any movs when calling multiple functions
; with swiftself argument.
; CHECK-LABEL: swiftself_passthrough:
; OPT: callq {{_?}}swiftself_param
; OPT-NEXT: callq {{_?}}swiftself_param
define void @swiftself_passthrough(i8* swiftself %addr0) {
  call i8 *@swiftself_param(i8* swiftself %addr0)
  call i8 *@swiftself_param(i8* swiftself %addr0)
  ret void
}
