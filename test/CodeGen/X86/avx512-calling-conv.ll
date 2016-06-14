; NOTE: Assertions have been autogenerated by update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-apple-darwin -mcpu=knl | FileCheck %s --check-prefix=ALL_X64 --check-prefix=KNL
; RUN: llc < %s -mtriple=x86_64-apple-darwin -mcpu=skx | FileCheck %s --check-prefix=ALL_X64 --check-prefix=SKX
; RUN: llc < %s -mtriple=i686-apple-darwin -mcpu=knl | FileCheck %s --check-prefix=KNL_X32

define <16 x i1> @test1() {
; KNL-LABEL: test1:
; KNL:       ## BB#0:
; KNL-NEXT:    vxorps %xmm0, %xmm0, %xmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: test1:
; SKX:       ## BB#0:
; SKX-NEXT:    vpxord %xmm0, %xmm0, %xmm0
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test1:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    vxorps %xmm0, %xmm0, %xmm0
; KNL_X32-NEXT:    retl
  ret <16 x i1> zeroinitializer
}

define <16 x i1> @test2(<16 x i1>%a, <16 x i1>%b) {
; KNL-LABEL: test2:
; KNL:       ## BB#0:
; KNL-NEXT:    vpmovsxbd %xmm1, %zmm1
; KNL-NEXT:    vpslld $31, %zmm1, %zmm1
; KNL-NEXT:    vpmovsxbd %xmm0, %zmm0
; KNL-NEXT:    vpslld $31, %zmm0, %zmm0
; KNL-NEXT:    vptestmd %zmm0, %zmm0, %k1
; KNL-NEXT:    vptestmd %zmm1, %zmm1, %k1 {%k1}
; KNL-NEXT:    vpbroadcastd {{.*}}(%rip), %zmm0 {%k1} {z}
; KNL-NEXT:    vpmovdb %zmm0, %xmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: test2:
; SKX:       ## BB#0:
; SKX-NEXT:    vpsllw $7, %xmm1, %xmm1
; SKX-NEXT:    vpmovb2m %xmm1, %k0
; SKX-NEXT:    vpsllw $7, %xmm0, %xmm0
; SKX-NEXT:    vpmovb2m %xmm0, %k1
; SKX-NEXT:    kandw %k0, %k1, %k0
; SKX-NEXT:    vpmovm2b %k0, %xmm0
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test2:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    vpmovsxbd %xmm1, %zmm1
; KNL_X32-NEXT:    vpslld $31, %zmm1, %zmm1
; KNL_X32-NEXT:    vpmovsxbd %xmm0, %zmm0
; KNL_X32-NEXT:    vpslld $31, %zmm0, %zmm0
; KNL_X32-NEXT:    vptestmd %zmm0, %zmm0, %k1
; KNL_X32-NEXT:    vptestmd %zmm1, %zmm1, %k1 {%k1}
; KNL_X32-NEXT:    vpbroadcastd LCPI1_0, %zmm0 {%k1} {z}
; KNL_X32-NEXT:    vpmovdb %zmm0, %xmm0
; KNL_X32-NEXT:    retl
  %c = and <16 x i1>%a, %b
  ret <16 x i1> %c
}

define <8 x i1> @test3(<8 x i1>%a, <8 x i1>%b) {
; KNL-LABEL: test3:
; KNL:       ## BB#0:
; KNL-NEXT:    vpmovsxwq %xmm1, %zmm1
; KNL-NEXT:    vpsllq $63, %zmm1, %zmm1
; KNL-NEXT:    vpmovsxwq %xmm0, %zmm0
; KNL-NEXT:    vpsllq $63, %zmm0, %zmm0
; KNL-NEXT:    vptestmq %zmm0, %zmm0, %k1
; KNL-NEXT:    vptestmq %zmm1, %zmm1, %k1 {%k1}
; KNL-NEXT:    vpbroadcastq {{.*}}(%rip), %zmm0 {%k1} {z}
; KNL-NEXT:    vpmovqw %zmm0, %xmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: test3:
; SKX:       ## BB#0:
; SKX-NEXT:    vpsllw $15, %xmm1, %xmm1
; SKX-NEXT:    vpmovw2m %xmm1, %k0
; SKX-NEXT:    vpsllw $15, %xmm0, %xmm0
; SKX-NEXT:    vpmovw2m %xmm0, %k1
; SKX-NEXT:    kandb %k0, %k1, %k0
; SKX-NEXT:    vpmovm2w %k0, %xmm0
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test3:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    vpmovsxwq %xmm1, %zmm1
; KNL_X32-NEXT:    vmovdqa64 {{.*#+}} zmm2 = [63,0,63,0,63,0,63,0,63,0,63,0,63,0,63,0]
; KNL_X32-NEXT:    vpsllvq %zmm2, %zmm1, %zmm1
; KNL_X32-NEXT:    vpmovsxwq %xmm0, %zmm0
; KNL_X32-NEXT:    vpsllvq %zmm2, %zmm0, %zmm0
; KNL_X32-NEXT:    vptestmq %zmm0, %zmm0, %k1
; KNL_X32-NEXT:    vptestmq %zmm1, %zmm1, %k1 {%k1}
; KNL_X32-NEXT:    vpbroadcastd LCPI2_1, %zmm0
; KNL_X32-NEXT:    vmovdqa64 %zmm0, %zmm0 {%k1} {z}
; KNL_X32-NEXT:    vpmovqw %zmm0, %xmm0
; KNL_X32-NEXT:    retl
  %c = and <8 x i1>%a, %b
  ret <8 x i1> %c
}

define <4 x i1> @test4(<4 x i1>%a, <4 x i1>%b) {
; KNL-LABEL: test4:
; KNL:       ## BB#0:
; KNL-NEXT:    vandps %xmm1, %xmm0, %xmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: test4:
; SKX:       ## BB#0:
; SKX-NEXT:    vpslld $31, %xmm1, %xmm1
; SKX-NEXT:    vpslld $31, %xmm0, %xmm0
; SKX-NEXT:    vptestmd %xmm0, %xmm0, %k1
; SKX-NEXT:    vptestmd %xmm1, %xmm1, %k0 {%k1}
; SKX-NEXT:    vpmovm2d %k0, %xmm0
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test4:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    vandps %xmm1, %xmm0, %xmm0
; KNL_X32-NEXT:    retl
  %c = and <4 x i1>%a, %b
  ret <4 x i1> %c
}

declare <8 x i1> @func8xi1(<8 x i1> %a)

define <8 x i32> @test5(<8 x i32>%a, <8 x i32>%b) {
; KNL-LABEL: test5:
; KNL:       ## BB#0:
; KNL-NEXT:    pushq %rax
; KNL-NEXT:  Ltmp0:
; KNL-NEXT:    .cfi_def_cfa_offset 16
; KNL-NEXT:    vpcmpgtd %ymm1, %ymm0, %ymm0
; KNL-NEXT:    vpmovdw %zmm0, %ymm0
; KNL-NEXT:    callq _func8xi1
; KNL-NEXT:    vpmovzxwd {{.*#+}} ymm0 = xmm0[0],zero,xmm0[1],zero,xmm0[2],zero,xmm0[3],zero,xmm0[4],zero,xmm0[5],zero,xmm0[6],zero,xmm0[7],zero
; KNL-NEXT:    vpslld $31, %ymm0, %ymm0
; KNL-NEXT:    vpsrad $31, %ymm0, %ymm0
; KNL-NEXT:    popq %rax
; KNL-NEXT:    retq
;
; SKX-LABEL: test5:
; SKX:       ## BB#0:
; SKX-NEXT:    pushq %rax
; SKX-NEXT:  Ltmp0:
; SKX-NEXT:    .cfi_def_cfa_offset 16
; SKX-NEXT:    vpcmpgtd %ymm1, %ymm0, %k0
; SKX-NEXT:    vpmovm2w %k0, %xmm0
; SKX-NEXT:    callq _func8xi1
; SKX-NEXT:    vpmovzxwd {{.*#+}} ymm0 = xmm0[0],zero,xmm0[1],zero,xmm0[2],zero,xmm0[3],zero,xmm0[4],zero,xmm0[5],zero,xmm0[6],zero,xmm0[7],zero
; SKX-NEXT:    vpslld $31, %ymm0, %ymm0
; SKX-NEXT:    vpsrad $31, %ymm0, %ymm0
; SKX-NEXT:    popq %rax
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test5:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    subl $12, %esp
; KNL_X32-NEXT:  Ltmp0:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 16
; KNL_X32-NEXT:    vpcmpgtd %ymm1, %ymm0, %ymm0
; KNL_X32-NEXT:    vpmovdw %zmm0, %ymm0
; KNL_X32-NEXT:    calll L_func8xi1$stub
; KNL_X32-NEXT:    vpmovzxwd {{.*#+}} ymm0 = xmm0[0],zero,xmm0[1],zero,xmm0[2],zero,xmm0[3],zero,xmm0[4],zero,xmm0[5],zero,xmm0[6],zero,xmm0[7],zero
; KNL_X32-NEXT:    vpslld $31, %ymm0, %ymm0
; KNL_X32-NEXT:    vpsrad $31, %ymm0, %ymm0
; KNL_X32-NEXT:    addl $12, %esp
; KNL_X32-NEXT:    retl
  %cmpRes = icmp sgt <8 x i32>%a, %b
  %resi = call <8 x i1> @func8xi1(<8 x i1> %cmpRes)
  %res = sext <8 x i1>%resi to <8 x i32>
  ret <8 x i32> %res
}

declare <16 x i1> @func16xi1(<16 x i1> %a)

define <16 x i32> @test6(<16 x i32>%a, <16 x i32>%b) {
; KNL-LABEL: test6:
; KNL:       ## BB#0:
; KNL-NEXT:    pushq %rax
; KNL-NEXT:  Ltmp1:
; KNL-NEXT:    .cfi_def_cfa_offset 16
; KNL-NEXT:    vpcmpgtd %zmm1, %zmm0, %k1
; KNL-NEXT:    vpbroadcastd {{.*}}(%rip), %zmm0 {%k1} {z}
; KNL-NEXT:    vpmovdb %zmm0, %xmm0
; KNL-NEXT:    callq _func16xi1
; KNL-NEXT:    vpmovzxbd {{.*#+}} zmm0 = xmm0[0],zero,zero,zero,xmm0[1],zero,zero,zero,xmm0[2],zero,zero,zero,xmm0[3],zero,zero,zero,xmm0[4],zero,zero,zero,xmm0[5],zero,zero,zero,xmm0[6],zero,zero,zero,xmm0[7],zero,zero,zero,xmm0[8],zero,zero,zero,xmm0[9],zero,zero,zero,xmm0[10],zero,zero,zero,xmm0[11],zero,zero,zero,xmm0[12],zero,zero,zero,xmm0[13],zero,zero,zero,xmm0[14],zero,zero,zero,xmm0[15],zero,zero,zero
; KNL-NEXT:    vpslld $31, %zmm0, %zmm0
; KNL-NEXT:    vpsrad $31, %zmm0, %zmm0
; KNL-NEXT:    popq %rax
; KNL-NEXT:    retq
;
; SKX-LABEL: test6:
; SKX:       ## BB#0:
; SKX-NEXT:    pushq %rax
; SKX-NEXT:  Ltmp1:
; SKX-NEXT:    .cfi_def_cfa_offset 16
; SKX-NEXT:    vpcmpgtd %zmm1, %zmm0, %k0
; SKX-NEXT:    vpmovm2b %k0, %xmm0
; SKX-NEXT:    callq _func16xi1
; SKX-NEXT:    vpmovzxbd {{.*#+}} zmm0 = xmm0[0],zero,zero,zero,xmm0[1],zero,zero,zero,xmm0[2],zero,zero,zero,xmm0[3],zero,zero,zero,xmm0[4],zero,zero,zero,xmm0[5],zero,zero,zero,xmm0[6],zero,zero,zero,xmm0[7],zero,zero,zero,xmm0[8],zero,zero,zero,xmm0[9],zero,zero,zero,xmm0[10],zero,zero,zero,xmm0[11],zero,zero,zero,xmm0[12],zero,zero,zero,xmm0[13],zero,zero,zero,xmm0[14],zero,zero,zero,xmm0[15],zero,zero,zero
; SKX-NEXT:    vpslld $31, %zmm0, %zmm0
; SKX-NEXT:    vpsrad $31, %zmm0, %zmm0
; SKX-NEXT:    popq %rax
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test6:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    subl $12, %esp
; KNL_X32-NEXT:  Ltmp1:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 16
; KNL_X32-NEXT:    vpcmpgtd %zmm1, %zmm0, %k1
; KNL_X32-NEXT:    vpbroadcastd LCPI5_0, %zmm0 {%k1} {z}
; KNL_X32-NEXT:    vpmovdb %zmm0, %xmm0
; KNL_X32-NEXT:    calll L_func16xi1$stub
; KNL_X32-NEXT:    vpmovzxbd {{.*#+}} zmm0 = xmm0[0],zero,zero,zero,xmm0[1],zero,zero,zero,xmm0[2],zero,zero,zero,xmm0[3],zero,zero,zero,xmm0[4],zero,zero,zero,xmm0[5],zero,zero,zero,xmm0[6],zero,zero,zero,xmm0[7],zero,zero,zero,xmm0[8],zero,zero,zero,xmm0[9],zero,zero,zero,xmm0[10],zero,zero,zero,xmm0[11],zero,zero,zero,xmm0[12],zero,zero,zero,xmm0[13],zero,zero,zero,xmm0[14],zero,zero,zero,xmm0[15],zero,zero,zero
; KNL_X32-NEXT:    vpslld $31, %zmm0, %zmm0
; KNL_X32-NEXT:    vpsrad $31, %zmm0, %zmm0
; KNL_X32-NEXT:    addl $12, %esp
; KNL_X32-NEXT:    retl
  %cmpRes = icmp sgt <16 x i32>%a, %b
  %resi = call <16 x i1> @func16xi1(<16 x i1> %cmpRes)
  %res = sext <16 x i1>%resi to <16 x i32>
  ret <16 x i32> %res
}

declare <4 x i1> @func4xi1(<4 x i1> %a)

define <4 x i32> @test7(<4 x i32>%a, <4 x i32>%b) {
; KNL-LABEL: test7:
; KNL:       ## BB#0:
; KNL-NEXT:    pushq %rax
; KNL-NEXT:  Ltmp2:
; KNL-NEXT:    .cfi_def_cfa_offset 16
; KNL-NEXT:    vpcmpgtd %xmm1, %xmm0, %xmm0
; KNL-NEXT:    callq _func4xi1
; KNL-NEXT:    vpslld $31, %xmm0, %xmm0
; KNL-NEXT:    vpsrad $31, %xmm0, %xmm0
; KNL-NEXT:    popq %rax
; KNL-NEXT:    retq
;
; SKX-LABEL: test7:
; SKX:       ## BB#0:
; SKX-NEXT:    pushq %rax
; SKX-NEXT:  Ltmp2:
; SKX-NEXT:    .cfi_def_cfa_offset 16
; SKX-NEXT:    vpcmpgtd %xmm1, %xmm0, %k0
; SKX-NEXT:    vpmovm2d %k0, %xmm0
; SKX-NEXT:    callq _func4xi1
; SKX-NEXT:    vpslld $31, %xmm0, %xmm0
; SKX-NEXT:    vpsrad $31, %xmm0, %xmm0
; SKX-NEXT:    popq %rax
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test7:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    subl $12, %esp
; KNL_X32-NEXT:  Ltmp2:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 16
; KNL_X32-NEXT:    vpcmpgtd %xmm1, %xmm0, %xmm0
; KNL_X32-NEXT:    calll L_func4xi1$stub
; KNL_X32-NEXT:    vpslld $31, %xmm0, %xmm0
; KNL_X32-NEXT:    vpsrad $31, %xmm0, %xmm0
; KNL_X32-NEXT:    addl $12, %esp
; KNL_X32-NEXT:    retl
  %cmpRes = icmp sgt <4 x i32>%a, %b
  %resi = call <4 x i1> @func4xi1(<4 x i1> %cmpRes)
  %res = sext <4 x i1>%resi to <4 x i32>
  ret <4 x i32> %res
}

define <8 x i1> @test7a(<8 x i32>%a, <8 x i32>%b) {
; KNL-LABEL: test7a:
; KNL:       ## BB#0:
; KNL-NEXT:    pushq %rax
; KNL-NEXT:  Ltmp3:
; KNL-NEXT:    .cfi_def_cfa_offset 16
; KNL-NEXT:    vpcmpgtd %ymm1, %ymm0, %ymm0
; KNL-NEXT:    vpmovdw %zmm0, %ymm0
; KNL-NEXT:    callq _func8xi1
; KNL-NEXT:    vpmovsxwq %xmm0, %zmm0
; KNL-NEXT:    vpsllq $63, %zmm0, %zmm0
; KNL-NEXT:    movb $85, %al
; KNL-NEXT:    kmovw %eax, %k1
; KNL-NEXT:    vptestmq %zmm0, %zmm0, %k1 {%k1}
; KNL-NEXT:    vpbroadcastq {{.*}}(%rip), %zmm0 {%k1} {z}
; KNL-NEXT:    vpmovqw %zmm0, %xmm0
; KNL-NEXT:    popq %rax
; KNL-NEXT:    retq
;
; SKX-LABEL: test7a:
; SKX:       ## BB#0:
; SKX-NEXT:    pushq %rax
; SKX-NEXT:  Ltmp3:
; SKX-NEXT:    .cfi_def_cfa_offset 16
; SKX-NEXT:    vpcmpgtd %ymm1, %ymm0, %k0
; SKX-NEXT:    vpmovm2w %k0, %xmm0
; SKX-NEXT:    callq _func8xi1
; SKX-NEXT:    vpsllw $15, %xmm0, %xmm0
; SKX-NEXT:    vpmovw2m %xmm0, %k0
; SKX-NEXT:    movb $85, %al
; SKX-NEXT:    kmovb %eax, %k1
; SKX-NEXT:    kandb %k1, %k0, %k0
; SKX-NEXT:    vpmovm2w %k0, %xmm0
; SKX-NEXT:    popq %rax
; SKX-NEXT:    retq
;
; KNL_X32-LABEL: test7a:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    subl $12, %esp
; KNL_X32-NEXT:  Ltmp3:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 16
; KNL_X32-NEXT:    vpcmpgtd %ymm1, %ymm0, %ymm0
; KNL_X32-NEXT:    vpmovdw %zmm0, %ymm0
; KNL_X32-NEXT:    calll L_func8xi1$stub
; KNL_X32-NEXT:    vpmovsxwq %xmm0, %zmm0
; KNL_X32-NEXT:    vpsllvq LCPI7_0, %zmm0, %zmm0
; KNL_X32-NEXT:    movb $85, %al
; KNL_X32-NEXT:    kmovw %eax, %k1
; KNL_X32-NEXT:    vptestmq %zmm0, %zmm0, %k1 {%k1}
; KNL_X32-NEXT:    vpbroadcastd LCPI7_1, %zmm0
; KNL_X32-NEXT:    vmovdqa64 %zmm0, %zmm0 {%k1} {z}
; KNL_X32-NEXT:    vpmovqw %zmm0, %xmm0
; KNL_X32-NEXT:    addl $12, %esp
; KNL_X32-NEXT:    retl
  %cmpRes = icmp sgt <8 x i32>%a, %b
  %resi = call <8 x i1> @func8xi1(<8 x i1> %cmpRes)
  %res = and <8 x i1>%resi,  <i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false>
  ret <8 x i1> %res
}

define <16 x i8> @test8(<16 x i8> %a1, <16 x i8> %a2, i1 %cond) {
; ALL_X64-LABEL: test8:
; ALL_X64:       ## BB#0:
; ALL_X64-NEXT:    testb $1, %dil
; ALL_X64-NEXT:    jne LBB8_2
; ALL_X64-NEXT:  ## BB#1:
; ALL_X64-NEXT:    vmovaps %zmm1, %zmm0
; ALL_X64-NEXT:  LBB8_2:
; ALL_X64-NEXT:    retq
;
; KNL_X32-LABEL: test8:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    testb $1, {{[0-9]+}}(%esp)
; KNL_X32-NEXT:    jne LBB8_2
; KNL_X32-NEXT:  ## BB#1:
; KNL_X32-NEXT:    vmovaps %zmm1, %zmm0
; KNL_X32-NEXT:  LBB8_2:
; KNL_X32-NEXT:    retl
  %res = select i1 %cond, <16 x i8> %a1, <16 x i8> %a2
  ret <16 x i8> %res
}

define i1 @test9(double %a, double %b) {
; ALL_X64-LABEL: test9:
; ALL_X64:       ## BB#0:
; ALL_X64-NEXT:    vucomisd %xmm0, %xmm1
; ALL_X64-NEXT:    setb %al
; ALL_X64-NEXT:    retq
;
; KNL_X32-LABEL: test9:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    vmovsd {{.*#+}} xmm0 = mem[0],zero
; KNL_X32-NEXT:    vucomisd {{[0-9]+}}(%esp), %xmm0
; KNL_X32-NEXT:    setb %al
; KNL_X32-NEXT:    retl
  %c = fcmp ugt double %a, %b
  ret i1 %c
}

define i32 @test10(i32 %a, i32 %b, i1 %cond) {
; ALL_X64-LABEL: test10:
; ALL_X64:       ## BB#0:
; ALL_X64-NEXT:    testb $1, %dl
; ALL_X64-NEXT:    cmovel %esi, %edi
; ALL_X64-NEXT:    movl %edi, %eax
; ALL_X64-NEXT:    retq
;
; KNL_X32-LABEL: test10:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    testb $1, {{[0-9]+}}(%esp)
; KNL_X32-NEXT:    leal {{[0-9]+}}(%esp), %eax
; KNL_X32-NEXT:    leal {{[0-9]+}}(%esp), %ecx
; KNL_X32-NEXT:    cmovnel %eax, %ecx
; KNL_X32-NEXT:    movl (%ecx), %eax
; KNL_X32-NEXT:    retl
  %c = select i1 %cond, i32 %a, i32 %b
  ret i32 %c
}

define i1 @test11(i32 %a, i32 %b) {
; ALL_X64-LABEL: test11:
; ALL_X64:       ## BB#0:
; ALL_X64-NEXT:    cmpl %esi, %edi
; ALL_X64-NEXT:    setg %al
; ALL_X64-NEXT:    retq
;
; KNL_X32-LABEL: test11:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    movl {{[0-9]+}}(%esp), %eax
; KNL_X32-NEXT:    cmpl {{[0-9]+}}(%esp), %eax
; KNL_X32-NEXT:    setg %al
; KNL_X32-NEXT:    retl
  %c = icmp sgt i32 %a, %b
  ret i1 %c
}

define i32 @test12(i32 %a1, i32 %a2, i32 %b1) {
; ALL_X64-LABEL: test12:
; ALL_X64:       ## BB#0:
; ALL_X64-NEXT:    pushq %rbp
; ALL_X64-NEXT:  Ltmp4:
; ALL_X64-NEXT:    .cfi_def_cfa_offset 16
; ALL_X64-NEXT:    pushq %r14
; ALL_X64-NEXT:  Ltmp5:
; ALL_X64-NEXT:    .cfi_def_cfa_offset 24
; ALL_X64-NEXT:    pushq %rbx
; ALL_X64-NEXT:  Ltmp6:
; ALL_X64-NEXT:    .cfi_def_cfa_offset 32
; ALL_X64-NEXT:  Ltmp7:
; ALL_X64-NEXT:    .cfi_offset %rbx, -32
; ALL_X64-NEXT:  Ltmp8:
; ALL_X64-NEXT:    .cfi_offset %r14, -24
; ALL_X64-NEXT:  Ltmp9:
; ALL_X64-NEXT:    .cfi_offset %rbp, -16
; ALL_X64-NEXT:    movl %esi, %r14d
; ALL_X64-NEXT:    movl %edi, %ebp
; ALL_X64-NEXT:    movl %edx, %esi
; ALL_X64-NEXT:    callq _test11
; ALL_X64-NEXT:    movzbl %al, %ebx
; ALL_X64-NEXT:    movl %ebp, %edi
; ALL_X64-NEXT:    movl %r14d, %esi
; ALL_X64-NEXT:    movl %ebx, %edx
; ALL_X64-NEXT:    callq _test10
; ALL_X64-NEXT:    xorl %ecx, %ecx
; ALL_X64-NEXT:    testb $1, %bl
; ALL_X64-NEXT:    cmovel %ecx, %eax
; ALL_X64-NEXT:    popq %rbx
; ALL_X64-NEXT:    popq %r14
; ALL_X64-NEXT:    popq %rbp
; ALL_X64-NEXT:    retq
;
; KNL_X32-LABEL: test12:
; KNL_X32:       ## BB#0:
; KNL_X32-NEXT:    pushl %ebx
; KNL_X32-NEXT:  Ltmp4:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 8
; KNL_X32-NEXT:    pushl %edi
; KNL_X32-NEXT:  Ltmp5:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 12
; KNL_X32-NEXT:    pushl %esi
; KNL_X32-NEXT:  Ltmp6:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 16
; KNL_X32-NEXT:    subl $16, %esp
; KNL_X32-NEXT:  Ltmp7:
; KNL_X32-NEXT:    .cfi_def_cfa_offset 32
; KNL_X32-NEXT:  Ltmp8:
; KNL_X32-NEXT:    .cfi_offset %esi, -16
; KNL_X32-NEXT:  Ltmp9:
; KNL_X32-NEXT:    .cfi_offset %edi, -12
; KNL_X32-NEXT:  Ltmp10:
; KNL_X32-NEXT:    .cfi_offset %ebx, -8
; KNL_X32-NEXT:    movl {{[0-9]+}}(%esp), %esi
; KNL_X32-NEXT:    movl {{[0-9]+}}(%esp), %edi
; KNL_X32-NEXT:    movl {{[0-9]+}}(%esp), %eax
; KNL_X32-NEXT:    movl %eax, {{[0-9]+}}(%esp)
; KNL_X32-NEXT:    movl %edi, (%esp)
; KNL_X32-NEXT:    calll _test11
; KNL_X32-NEXT:    movl %eax, %ebx
; KNL_X32-NEXT:    movzbl %al, %eax
; KNL_X32-NEXT:    movl %eax, {{[0-9]+}}(%esp)
; KNL_X32-NEXT:    movl %esi, {{[0-9]+}}(%esp)
; KNL_X32-NEXT:    movl %edi, (%esp)
; KNL_X32-NEXT:    calll _test10
; KNL_X32-NEXT:    xorl %ecx, %ecx
; KNL_X32-NEXT:    testb $1, %bl
; KNL_X32-NEXT:    cmovel %ecx, %eax
; KNL_X32-NEXT:    addl $16, %esp
; KNL_X32-NEXT:    popl %esi
; KNL_X32-NEXT:    popl %edi
; KNL_X32-NEXT:    popl %ebx
; KNL_X32-NEXT:    retl
  %cond = call i1 @test11(i32 %a1, i32 %b1)
  %res = call i32 @test10(i32 %a1, i32 %a2, i1 %cond)
  %res1 = select i1 %cond, i32 %res, i32 0
  ret i32 %res1
}
