; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -O0 -mtriple=mips-img-linux-gnu -mcpu=mips32r6 -relocation-model=pic < %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-PIC
; RUN: llc -O0 -mtriple=mips-img-linux-gnu -mcpu=mips32r6 -relocation-model=static < %s -o - | FileCheck %s --check-prefixes=CHECK-STATIC

declare i32 @boo(...)
declare i32 @foo(...)

define i32 @main(i32 signext %argc, i8** %argv) {
; CHECK-LABEL: main:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    lui $2, %hi(_gp_disp)
; CHECK-NEXT:    addiu $2, $2, %lo(_gp_disp)
; CHECK-NEXT:    addiu $sp, $sp, -40
; CHECK-NEXT:    .cfi_def_cfa_offset 40
; CHECK-NEXT:    sw $ra, 36($sp) # 4-byte Folded Spill
; CHECK-NEXT:    .cfi_offset 31, -4
; CHECK-NEXT:    addu $1, $2, $25
; CHECK-NEXT:    sw $1, 20($sp) # 4-byte Folded Spill
; CHECK-NEXT:    # kill: def $at killed $a1
; CHECK-NEXT:    # kill: def $at killed $a0
; CHECK-NEXT:    sw $zero, 32($sp)
; CHECK-NEXT:    sw $4, 28($sp)
; CHECK-NEXT:    sw $5, 24($sp)
; CHECK-NEXT:    lw $1, 28($sp)
; CHECK-NEXT:    slti $1, $1, 2
; CHECK-NEXT:    bnezc $1, $BB0_6
; CHECK-NEXT:    nop
; CHECK-NEXT:  # %bb.1: # %entry
; CHECK-NEXT:    bc $BB0_2
; CHECK-NEXT:  $BB0_2: # %if.then
; CHECK-NEXT:    lw $1, 28($sp)
; CHECK-NEXT:    slti $1, $1, 4
; CHECK-NEXT:    bnezc $1, $BB0_5
; CHECK-NEXT:    nop
; CHECK-NEXT:  # %bb.3: # %if.then
; CHECK-NEXT:    bc $BB0_4
; CHECK-NEXT:  $BB0_4: # %if.then2
; CHECK-NEXT:    lw $gp, 20($sp) # 4-byte Folded Reload
; CHECK-NEXT:    lw $25, %call16(boo)($gp)
; CHECK-NEXT:    jalrc $25
; CHECK-NEXT:    sw $2, 32($sp)
; CHECK-NEXT:    bc $BB0_7
; CHECK-NEXT:  $BB0_5: # %if.end
; CHECK-NEXT:    lw $gp, 20($sp) # 4-byte Folded Reload
; CHECK-NEXT:    #APP
;
; CHECK-STATIC-LABEL: main:
; CHECK-STATIC:       # %bb.0: # %entry
; CHECK-STATIC-NEXT:    addiu $sp, $sp, -32
; CHECK-STATIC-NEXT:    .cfi_def_cfa_offset 32
; CHECK-STATIC-NEXT:    sw $ra, 28($sp) # 4-byte Folded Spill
; CHECK-STATIC-NEXT:    .cfi_offset 31, -4
; CHECK-STATIC-NEXT:    # kill: def $at killed $a1
; CHECK-STATIC-NEXT:    # kill: def $at killed $a0
; CHECK-STATIC-NEXT:    sw $zero, 24($sp)
; CHECK-STATIC-NEXT:    sw $4, 20($sp)
; CHECK-STATIC-NEXT:    sw $5, 16($sp)
; CHECK-STATIC-NEXT:    lw $1, 20($sp)
; CHECK-STATIC-NEXT:    slti $1, $1, 2
; CHECK-STATIC-NEXT:    beqzc $1, $BB0_2
; CHECK-STATIC-NEXT:    nop
; CHECK-STATIC-NEXT:  # %bb.1: # %entry
; CHECK-STATIC-NEXT:    bc $BB0_7
; CHECK-STATIC-NEXT:  $BB0_2: # %entry
; CHECK-STATIC-NEXT:    j $BB0_3
; CHECK-STATIC-NEXT:    nop
; CHECK-STATIC-NEXT:  $BB0_3: # %if.then
; CHECK-STATIC-NEXT:    lw $1, 20($sp)
; CHECK-STATIC-NEXT:    slti $1, $1, 4
; CHECK-STATIC-NEXT:    bnezc $1, $BB0_6
; CHECK-STATIC-NEXT:    nop
; CHECK-STATIC-NEXT:  # %bb.4: # %if.then
; CHECK-STATIC-NEXT:    j $BB0_5
; CHECK-STATIC-NEXT:    nop
; CHECK-STATIC-NEXT:  $BB0_5: # %if.then2
; CHECK-STATIC-NEXT:    jal boo
; CHECK-STATIC-NEXT:    nop
; CHECK-STATIC-NEXT:    sw $2, 24($sp)
; CHECK-STATIC-NEXT:    j $BB0_8
; CHECK-STATIC-NEXT:    nop
; CHECK-STATIC-NEXT:  $BB0_6: # %if.end
; CHECK-STATIC-NEXT:    #APP

entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 4
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 4
  %0 = load i32, i32* %argc.addr, align 4
  %cmp = icmp sgt i32 %0, 1
  br i1 %cmp, label %if.then, label %if.end4

if.then:
  %1 = load i32, i32* %argc.addr, align 4
  %cmp1 = icmp sgt i32 %1, 3
  br i1 %cmp1, label %if.then2, label %if.end

if.then2:
  %call = call i32 bitcast (i32 (...)* @boo to i32 ()*)()
  store i32 %call, i32* %retval, align 4
  br label %return

if.end:
  call void asm sideeffect ".space 4194228", "~{$1}"()
  %call3 = call i32 bitcast (i32 (...)* @foo to i32 ()*)()
  store i32 %call3, i32* %retval, align 4
  br label %return

if.end4:
  store i32 0, i32* %retval, align 4
  br label %return

return:
  %2 = load i32, i32* %retval, align 4
  ret i32 %2

}
