	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.IMPORT $global$,DATA
	.IMPORT $$dyncall,MILLICODE
; gcc_compiled.:
	.IMPORT vacall_function,DATA
	.SPACE $TEXT$
	.SUBSPA $CODE$

	.align 4
	.EXPORT vacall,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR
vacall
	.PROC
	.CALLINFO FRAME=192,CALLS,SAVE_RP
	.ENTRY
	stw %r2,-20(0,%r30)
	ldo 192(%r30),%r30
	stw %r26,-228(0,%r30)
	stw %r25,-232(0,%r30)
	stw %r24,-236(0,%r30)
	ldo -120(%r30),%r19
	fstds %fr5,8(0,%r19)
	fstds %fr7,0(0,%r19)
	fstws %fr4L,-4(0,%r19)
	fstws %fr5L,-8(0,%r19)
	fstws %fr6L,-12(0,%r19)
	fstws %fr7L,-16(0,%r19)
	stw %r23,-240(0,%r30)
	stw 0,-184(0,%r30)
	ldo -224(%r30),%r19
	stw %r19,-180(0,%r30)
	stw 0,-176(0,%r30)
	stw 0,-172(0,%r30)
	stw %r28,-152(0,%r30)
	ldo -240(%r30),%r19
	stw %r19,-148(0,%r30)
	ldi 104,%r19
	stw %r19,-144(0,%r30)
	ldi 120,%r19
	stw %r19,-140(0,%r30)
	addil L'vacall_function-$global$,%r27
	ldw R'vacall_function-$global$(%r1),%r19
	ldo -184(%r30),%r26
	copy %r19,%r22
	.CALL	ARGW0=GR
	bl $$dyncall,%r31
	copy %r31,%r2
	ldw -172(0,%r30),%r19
	addi,uv -16,%r19,0
	blr,n %r19,0
	b,n L$0002
L$0042
	b L$0002
	nop
	b L$0004
	nop
	b L$0005
	nop
	b L$0006
	nop
	b L$0007
	nop
	b L$0008
	nop
	b L$0045
	nop
	b L$0045
	nop
	b L$0045
	nop
	b L$0045
	nop
	b L$0044
	nop
	b L$0044
	nop
	b L$0015
	nop
	b L$0016
	nop
	b L$0045
	nop
	b L$0018
	nop
	bl L$0048,0
	ldw -212(0,%r30),%r2
L$0004
L$0005
	ldb -160(0,%r30),%r19
	bl L$0002,0
	extrs %r19,31,8,%r28
L$0006
	bl L$0002,0
	ldb -160(0,%r30),%r28
L$0007
	ldh -160(0,%r30),%r19
	bl L$0002,0
	extrs %r19,31,16,%r28
L$0008
	bl L$0002,0
	ldh -160(0,%r30),%r28
L$0015
	ldo -152(%r30),%r19
	fldws -8(0,%r19),%fr4L
L$0045
	bl L$0002,0
	ldw -160(0,%r30),%r28
L$0016
	ldo -152(%r30),%r19
	fldds -8(0,%r19),%fr4
L$0044
	ldw -160(0,%r30),%r28
	bl L$0002,0
	ldw -156(0,%r30),%r29
L$0018
	ldw -184(0,%r30),%r19
	bb,>=,n %r19,31,L$0019
	ldw -176(0,%r30),%r28
	bl L$0048,0
	ldw -212(0,%r30),%r2
L$0019
	bb,>= %r19,30,L$0048
	ldw -212(0,%r30),%r2
	bb,>= %r19,28,L$0022
	ldw -168(0,%r30),%r19
	comib,=,n 2,%r19,L$0025
	comib,<<,n 2,%r19,L$0029
	comib,= 1,%r19,L$0049
	ldw -176(0,%r30),%r19
	bl,n L$0002,0
L$0029
	comib,= 4,%r19,L$0026
	ldw -176(0,%r30),%r19
	bl L$0048,0
	ldw -212(0,%r30),%r2
L$0025
	ldw -176(0,%r30),%r19
	bl L$0002,0
	ldh 0(0,%r19),%r28
L$0026
	bl L$0002,0
	ldw 0(0,%r19),%r28
L$0022
	ldo -1(%r19),%r19
	addi,uv -8,%r19,0
	blr,n %r19,0
	b,n L$0002
L$0041
	b L$0032
	nop
	b L$0033
	nop
	b L$0034
	nop
	b L$0035
	nop
	b L$0036
	nop
	b L$0037
	nop
	b L$0038
	nop
	b L$0039
	nop
	bl L$0048,0
	ldw -212(0,%r30),%r2
L$0032
	ldw -176(0,%r30),%r19
L$0049
	bl L$0002,0
	ldb 0(0,%r19),%r28
L$0033
	ldw -176(0,%r30),%r19
	ldb 0(0,%r19),%r20
	ldb 1(0,%r19),%r19
	zdep %r20,23,24,%r20
	bl L$0002,0
	or %r20,%r19,%r28
L$0034
	ldw -176(0,%r30),%r21
	ldb 0(0,%r21),%r19
	ldb 1(0,%r21),%r20
	ldb 2(0,%r21),%r21
	zdep %r19,15,16,%r19
	zdep %r20,23,24,%r20
	or %r19,%r20,%r19
	bl L$0002,0
	or %r19,%r21,%r28
L$0035
	bl L$0046,0
	ldw -176(0,%r30),%r22
L$0036
	ldw -176(0,%r30),%r22
	ldb 4(0,%r22),%r29
L$0046
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	ldb 3(0,%r22),%r20
	or %r19,%r21,%r19
	bl L$0002,0
	or %r19,%r20,%r28
L$0037
	ldw -176(0,%r30),%r22
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	ldb 3(0,%r22),%r20
	or %r19,%r21,%r19
	or %r19,%r20,%r28
	ldb 4(0,%r22),%r19
	ldb 5(0,%r22),%r20
	bl L$0047,0
	zdep %r19,23,24,%r19
L$0038
	ldw -176(0,%r30),%r22
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	or %r19,%r21,%r19
	ldb 3(0,%r22),%r20
	ldb 6(0,%r22),%r21
	or %r19,%r20,%r28
	ldb 4(0,%r22),%r19
	ldb 5(0,%r22),%r20
	zdep %r19,15,16,%r19
	zdep %r20,23,24,%r20
	or %r19,%r20,%r19
	bl L$0002,0
	or %r19,%r21,%r29
L$0039
	ldw -176(0,%r30),%r22
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	or %r19,%r21,%r19
	ldb 3(0,%r22),%r20
	ldb 6(0,%r22),%r21
	or %r19,%r20,%r28
	zdep %r21,23,24,%r21
	ldb 4(0,%r22),%r19
	ldb 5(0,%r22),%r20
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	ldb 7(0,%r22),%r20
	or %r19,%r21,%r19
L$0047
	or %r19,%r20,%r29
L$0002
	ldw -212(0,%r30),%r2
L$0048
	bv 0(%r2)
	ldo -192(%r30),%r30
	.EXIT
	.PROCEND
