; RUN: opt -consthoist -S < %s | FileCheck %s
target triple = "aarch64--"

; We don't want to convert constant divides because the benefit from converting
; them to a mul in the backend is larget than constant materialization savings.
define void @signed_const_division(i32 %in1, i32 %in2, i32* %addr) {
; CHECK-LABEL: @signed_const_division
; CHECK: %res1 = sdiv i32 %l1, 1000000000
; CHECK: %res2 = srem i32 %l2, 1000000000
entry:
  br label %loop

loop:
  %l1 = phi i32 [%res1, %loop], [%in1, %entry]
  %l2 = phi i32 [%res2, %loop], [%in2, %entry]
  %res1 = sdiv i32 %l1, 1000000000
  store volatile i32 %res1, i32* %addr
  %res2 = srem i32 %l2, 1000000000
  store volatile i32 %res2, i32* %addr
  %again = icmp eq i32 %res1, %res2
  br i1 %again, label %loop, label %end

end:
  ret void
}

define void @unsigned_const_division(i32 %in1, i32 %in2, i32* %addr) {
; CHECK-LABEL: @unsigned_const_division
; CHECK: %res1 = udiv i32 %l1, 1000000000
; CHECK: %res2 = urem i32 %l2, 1000000000

entry:
  br label %loop

loop:
  %l1 = phi i32 [%res1, %loop], [%in1, %entry]
  %l2 = phi i32 [%res2, %loop], [%in2, %entry]
  %res1 = udiv i32 %l1, 1000000000
  store volatile i32 %res1, i32* %addr
  %res2 = urem i32 %l2, 1000000000
  store volatile i32 %res2, i32* %addr
  %again = icmp eq i32 %res1, %res2
  br i1 %again, label %loop, label %end

end:
  ret void
}
