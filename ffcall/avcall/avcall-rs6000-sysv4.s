	.file	"avcall-rs6000.c"
gcc2_compiled.:
	.section	".text"
	.align 2
	.globl __builtin_avcall
	.type	 __builtin_avcall,@function
__builtin_avcall:
	.extern __mulh
	.extern __mull
	.extern __divss
	.extern __divus
	.extern __quoss
	.extern __quous
	stwu 1,-1056(1)
	mflr 0
	stw 29,1044(1)
	stw 30,1048(1)
	stw 31,1052(1)
	stw 0,1060(1)
	mr 31,3
	lwz 9,20(31)
	li 3,8
	addi 9,9,-32
	subfc 9,31,9
	srawi 9,9,2
	cmpw 0,3,9
	addi 0,1,56
	bc 4,0,.L3
	mr 10,0
	addi 8,31,32
	li 11,32
.L5:
	lwzx 0,11,8
	addi 3,3,1
	cmpw 0,3,9
	stw 0,0(10)
	addi 10,10,4
	addi 11,11,4
	bc 12,0,.L5
.L3:
	lwz 9,1056(31)
	addi 9,9,-1064
	subfc 9,31,9
	srawi. 9,9,3
	bc 12,2,.L8
	cmpwi 0,9,1
	bc 12,2,.L11
	cmpwi 0,9,2
	bc 12,2,.L14
	cmpwi 0,9,3
	bc 12,2,.L17
	cmpwi 0,9,4
	bc 12,2,.L20
	cmpwi 0,9,5
	bc 12,2,.L23
	cmpwi 0,9,6
	bc 12,2,.L26
	cmpwi 0,9,7
	bc 12,2,.L29
	cmpwi 0,9,8
	bc 12,2,.L32
	cmpwi 0,9,9
	bc 12,2,.L35
	cmpwi 0,9,10
	bc 12,2,.L38
	cmpwi 0,9,11
	bc 12,2,.L41
	cmpwi 0,9,12
	bc 12,2,.L44
	lfd 13,1160(31)
.L44:
	lfd 12,1152(31)
.L41:
	lfd 11,1144(31)
.L38:
	lfd 10,1136(31)
.L35:
	lfd 9,1128(31)
.L32:
	lfd 8,1120(31)
.L29:
	lfd 7,1112(31)
.L26:
	lfd 6,1104(31)
.L23:
	lfd 5,1096(31)
.L20:
	lfd 4,1088(31)
.L17:
	lfd 3,1080(31)
.L14:
	lfd 2,1072(31)
.L11:
	lfd 1,1064(31)
.L8:
	lwz 0,0(31)
	lwz 3,32(31)
	lwz 4,36(31)
	mtlr 0
	lwz 5,40(31)
	lwz 6,44(31)
	lwz 7,48(31)
	lwz 8,52(31)
	lwz 9,56(31)
	lwz 10,60(31)
	crxor 6,6,6
	blrl
	lwz 9,12(31)
	cmpwi 0,9,1
	mr 11,9
	bc 12,2,.L49
	cmpwi 0,9,0
	bc 12,2,.L101
	cmpwi 0,9,2
	bc 12,2,.L102
	cmpwi 0,9,3
	bc 12,2,.L102
	cmpwi 0,9,4
	bc 12,2,.L102
	cmpwi 0,9,5
	bc 12,2,.L103
	cmpwi 0,9,6
	bc 12,2,.L103
	cmpwi 0,9,7
	bc 12,2,.L101
	cmpwi 0,9,8
	bc 12,2,.L101
	cmpwi 0,9,9
	bc 12,2,.L101
	cmpwi 0,9,10
	bc 12,2,.L101
	addi 0,11,-11
	cmplwi 0,0,1
	bc 4,1,.L104
	cmpwi 0,9,13
	bc 4,2,.L72
	frsp 0,1
	lwz 9,8(31)
	stfs 0,0(9)
	b .L49
.L72:
	cmpwi 0,9,14
	bc 4,2,.L74
	lwz 9,8(31)
	stfd 1,0(9)
	b .L49
.L74:
	cmpwi 0,9,15
	bc 12,2,.L101
	cmpwi 0,9,16
	bc 4,2,.L49
	lwz 0,4(31)
	andi. 29,0,1
	bc 12,2,.L79
	lwz 9,16(31)
	cmpwi 0,9,1
	bc 4,2,.L80
	lwz 9,8(31)
	lbz 0,0(3)
	stb 0,0(9)
	b .L49
.L80:
	cmpwi 0,9,2
	bc 4,2,.L82
	lwz 9,8(31)
	lhz 0,0(3)
	sth 0,0(9)
	b .L49
.L82:
	cmpwi 0,9,4
	bc 4,2,.L84
	lwz 9,8(31)
	lwz 0,0(3)
	stw 0,0(9)
	b .L49
.L84:
	cmpwi 0,9,8
	bc 4,2,.L86
	lwz 11,8(31)
	lwz 0,0(3)
	stw 0,0(11)
	lwz 10,8(31)
	lwz 9,4(3)
	stw 9,4(10)
	b .L49
.L86:
	addi 0,9,3
	srwi 10,0,2
	addic. 10,10,-1
	bc 12,0,.L49
	slwi 11,10,2
.L90:
	lwzx 0,11,3
	lwz 9,8(31)
	addic. 10,10,-1
	stwx 0,11,9
	addi 11,11,-4
	bc 4,0,.L90
	b .L49
.L79:
	andi. 29,0,512
	bc 12,2,.L49
	lwz 0,16(31)
	cmpwi 0,0,1
	bc 4,2,.L94
.L102:
	lwz 9,8(31)
	stb 3,0(9)
	b .L49
.L94:
	cmpwi 0,0,2
	bc 4,2,.L96
.L103:
	lwz 9,8(31)
	sth 3,0(9)
	b .L49
.L96:
	cmpwi 0,0,4
	bc 4,2,.L98
.L101:
	lwz 9,8(31)
	stw 3,0(9)
	b .L49
.L98:
	cmpwi 0,0,8
	bc 4,2,.L49
.L104:
	lwz 9,8(31)
	stw 3,0(9)
	lwz 11,8(31)
	stw 4,4(11)
.L49:
	li 3,0
	lwz 0,1060(1)
	mtlr 0
	lwz 29,1044(1)
	lwz 30,1048(1)
	lwz 31,1052(1)
	la 1,1056(1)
	blr
.Lfe1:
	.size	 __builtin_avcall,.Lfe1-__builtin_avcall
	.ident	"GCC: (GNU) egcs-2.91.66 19990314 (egcs-1.1.2 release)"
