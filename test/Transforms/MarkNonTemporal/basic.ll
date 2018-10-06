; RUN: opt < %s -mark-non-temporal | FileCheck %s

@g = external global i64

define void @foo(i64 *%addr) {
entry:
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i2, %loop ]

  %dest = getelementptr i64, i64* %addr, i64 %i
  store i64 %i, i64* %dest
  store i64 %i, i64* @g

  %i2 = add nuw nsw i64 %i, 1
  %cond = icmp eq i64 %i2, 100
  br i1 %cond, label %exit, label %loop

exit:
  ret void
}
