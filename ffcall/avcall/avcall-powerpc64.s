	.file	"avcall-powerpc64.c"
	.section	".text"
	.align 2
	.globl __builtin_avcall
	.section	".opd","aw"
	.align 3
__builtin_avcall:
	.quad	.__builtin_avcall,.TOC.@tocbase,0
	.previous
	.size	__builtin_avcall,24
	.type	.__builtin_avcall,@function
	.globl	.__builtin_avcall
.__builtin_avcall:
	mflr 0
	std 31,-8(1)
	mr 31,3
	std 29,-24(1)
	std 0,16(1)
	stdu 1,-2192(1)
	ld 9,40(3)
	li 3,8
	addi 0,1,112
	subf 9,31,9
	addi 9,9,-48
	sradi 9,9,3
	extsw 9,9
	cmpd 7,3,9
	bge- 7,.L82
	addi 9,9,-8
	mtctr 9
.L83:
	sldi 9,3,3
	addi 3,3,1
	add 11,9,31
	add 9,9,0
	lfd 0,48(11)
	stfd 0,-64(9)
	bdnz .L83
.L82:
	ld 9,2096(31)
	subf 9,31,9
	addi 9,9,-2104
	sradi 9,9,3
	extsw 9,9
	cmpwi 7,9,0
	beq- 7,.L8
	cmpwi 7,9,1
	beq- 7,.L11
	cmpwi 7,9,2
	beq- 7,.L14
	cmpwi 7,9,3
	beq- 7,.L17
	cmpwi 7,9,4
	beq- 7,.L20
	cmpwi 7,9,5
	beq- 7,.L23
	cmpwi 7,9,6
	beq- 7,.L26
	cmpwi 7,9,7
	beq- 7,.L29
	cmpwi 7,9,8
	beq- 7,.L32
	cmpwi 7,9,9
	beq- 7,.L35
	cmpwi 7,9,10
	beq- 7,.L38
	cmpwi 7,9,11
	beq- 7,.L41
	cmpwi 7,9,12
	beq- 7,.L44
.L47:
	lfd 13,2200(31)
.L44:
	lfd 12,2192(31)
.L41:
	lfd 11,2184(31)
.L38:
	lfd 10,2176(31)
.L35:
	lfd 9,2168(31)
.L32:
	lfd 8,2160(31)
.L29:
	lfd 7,2152(31)
.L26:
	lfd 6,2144(31)
.L23:
	lfd 5,2136(31)
.L20:
	lfd 4,2128(31)
.L17:
	lfd 3,2120(31)
.L14:
	lfd 2,2112(31)
.L11:
	lfd 1,2104(31)
.L8:
	ld 29,0(31)
	ld 9,96(31)
	ld 0,0(29)
	ld 3,48(31)
	ld 4,56(31)
	mtctr 0
	ld 5,64(31)
	ld 6,72(31)
	ld 7,80(31)
	ld 8,88(31)
	ld 10,104(31)
	std 2,40(1)
	ld 11,16(29)
	ld 2,8(29)
	bctrl
	ld 2,40(1)
	lwz 9,24(31)
	cmpwi 7,9,1
	beq- 7,.L49
	cmpwi 7,9,0
	beq- 7,.L84
	cmpwi 7,9,2
	beq- 7,.L87
	cmpwi 7,9,3
	beq- 7,.L87
	cmpwi 7,9,4
	beq- 7,.L87
	cmpwi 7,9,5
	beq- 7,.L86
	cmpwi 7,9,6
	beq- 7,.L86
	cmpwi 7,9,7
	beq- 7,.L85
	cmpwi 7,9,8
	beq- 7,.L85
	cmpwi 7,9,9
	beq- 7,.L84
	cmpwi 7,9,10
	beq- 7,.L84
	cmpwi 7,9,11
	beq- 7,.L84
	cmpwi 7,9,12
	beq- 7,.L84
	cmpwi 7,9,13
	beq- 7,.L89
	cmpwi 7,9,14
	beq- 7,.L90
	cmpwi 7,9,15
	beq- 7,.L84
.L49:
	addi 1,1,2192
	li 3,0
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	ld 31,-8(1)
	blr
.L84:
	ld 9,16(31)
	std 3,0(9)
	addi 1,1,2192
	li 3,0
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	ld 31,-8(1)
	blr
.L87:
	ld 9,16(31)
	stb 3,0(9)
	addi 1,1,2192
	li 3,0
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	ld 31,-8(1)
	blr
.L86:
	ld 9,16(31)
	sth 3,0(9)
	b .L49
.L85:
	ld 9,16(31)
	stw 3,0(9)
	b .L49
.L89:
	ld 9,16(31)
	stfs 1,0(9)
	b .L49
.L90:
	ld 9,16(31)
	stfd 1,0(9)
	b .L49
	.long 0
	.byte 0,0,0,1,128,3,0,0
	.size	.__builtin_avcall,.-.__builtin_avcall
	.ident	"GCC: (GNU) 3.3.3 (SuSE Linux)"
