	.file	"avcall-rs6000.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
	.align 2
	.globl __builtin_avcall
	.globl .__builtin_avcall
.csect __builtin_avcall[DS]
__builtin_avcall:
	.long .__builtin_avcall, TOC[tc0], 0
.csect .text[PR]
.__builtin_avcall:
	mflr 0
	st 31,-4(1)
	st 0,8(1)
	stu 1,-1088(1)
	mr 31,3
	l 0,20(31)
	lil 3,8
	ai 0,0,-32
	sf 0,31,0
	srai 9,0,2
	cmp 1,3,9
	bc 4,4,L..3
	cal 10,32(31)
	cal 11,88(1)
L..5:
	cal 3,1(3)
	l 0,32(10)
	cmp 1,3,9
	cal 10,4(10)
	st 0,-32(11)
	cal 11,4(11)
	bc 12,4,L..5
L..3:
	l 0,1056(31)
	ai 0,0,-1060
	sf 0,31,0
	srai. 9,0,3
	bc 12,2,L..8
	cmpi 1,9,1
	bc 12,6,L..11
	cmpi 1,9,2
	bc 12,6,L..14
	cmpi 1,9,3
	bc 12,6,L..17
	cmpi 1,9,4
	bc 12,6,L..20
	cmpi 1,9,5
	bc 12,6,L..23
	cmpi 1,9,6
	bc 12,6,L..26
	cmpi 1,9,7
	bc 12,6,L..29
	cmpi 1,9,8
	bc 12,6,L..32
	cmpi 1,9,9
	bc 12,6,L..35
	cmpi 1,9,10
	bc 12,6,L..38
	cmpi 1,9,11
	bc 12,6,L..41
	cmpi 1,9,12
	bc 12,6,L..44
	lfd 13,1156(31)
L..44:
	lfd 12,1148(31)
L..41:
	lfd 11,1140(31)
L..38:
	lfd 10,1132(31)
L..35:
	lfd 9,1124(31)
L..32:
	lfd 8,1116(31)
L..29:
	lfd 7,1108(31)
L..26:
	lfd 6,1100(31)
L..23:
	lfd 5,1092(31)
L..20:
	lfd 4,1084(31)
L..17:
	lfd 3,1076(31)
L..14:
	lfd 2,1068(31)
L..11:
	lfd 1,1060(31)
L..8:
	l 3,32(31)
	l 11,0(31)
	l 4,36(31)
	l 5,40(31)
	l 6,44(31)
	l 7,48(31)
	l 8,52(31)
	l 9,56(31)
	l 10,60(31)
	l 0,0(11)
	st 2,20(1)
	l 2,4(11)
	l 11,8(11)
	mtlr 0
	brl
	l 2,20(1)
	l 0,12(31)
	cmpi 1,0,1
	bc 12,6,L..49
	cmpi 1,0,0
	bc 12,6,L..101
	cmpi 1,0,2
	bc 12,6,L..102
	cmpi 1,0,3
	bc 12,6,L..102
	cmpi 1,0,4
	bc 12,6,L..102
	cmpi 1,0,5
	bc 12,6,L..103
	cmpi 1,0,6
	bc 12,6,L..103
	cmpi 1,0,7
	bc 12,6,L..101
	cmpi 1,0,8
	bc 12,6,L..101
	cmpi 1,0,9
	bc 12,6,L..101
	cmpi 1,0,10
	bc 12,6,L..101
	l 9,12(31)
	cal 0,-11(9)
	cmpli 1,0,1
	bc 4,5,L..104
	cmpi 1,9,13
	bc 4,6,L..72
	l 9,8(31)
	frsp 0,1
	stfs 0,0(9)
	b L..49
L..72:
	cmpi 1,9,14
	bc 4,6,L..74
	l 9,8(31)
	stfd 1,0(9)
	b L..49
L..74:
	cmpi 1,9,15
	bc 12,6,L..101
	cmpi 1,9,16
	bc 4,6,L..49
	l 9,4(31)
	andil. 0,9,1
	bc 12,2,L..79
	l 0,16(31)
	cmpi 1,0,1
	bc 4,6,L..80
	l 9,8(31)
	lbz 0,0(3)
	stb 0,0(9)
	b L..49
L..80:
	cmpi 1,0,2
	bc 4,6,L..82
	l 9,8(31)
	lhz 0,0(3)
	sth 0,0(9)
	b L..49
L..82:
	cmpi 1,0,4
	bc 4,6,L..84
	l 9,8(31)
	l 0,0(3)
	st 0,0(9)
	b L..49
L..84:
	cmpi 1,0,8
	bc 4,6,L..86
	l 9,8(31)
	l 0,0(3)
	st 0,0(9)
	l 9,8(31)
	l 0,4(3)
	st 0,4(9)
	b L..49
L..86:
	ai 0,0,3
	sri 0,0,2
	ai. 0,0,-1
	bc 12,0,L..49
	sli 11,0,2
L..90:
	l 9,8(31)
	lx 0,11,3
	stx 0,11,9
	ai. 11,11,-4
	bc 4,0,L..90
	b L..49
L..79:
	andil. 0,9,512
	bc 12,2,L..49
	l 0,16(31)
	cmpi 1,0,1
	bc 4,6,L..94
L..102:
	l 9,8(31)
	stb 3,0(9)
	b L..49
L..94:
	cmpi 1,0,2
	bc 4,6,L..96
L..103:
	l 9,8(31)
	sth 3,0(9)
	b L..49
L..96:
	cmpi 1,0,4
	bc 4,6,L..98
L..101:
	l 9,8(31)
	st 3,0(9)
	b L..49
L..98:
	cmpi 1,0,8
	bc 4,6,L..49
L..104:
	l 9,8(31)
	st 3,0(9)
	l 9,8(31)
	st 4,4(9)
L..49:
	lil 3,0
	cal 1,1088(1)
	l 0,8(1)
	mtlr 0
	l 31,-4(1)
	br
LT..__builtin_avcall:
	.long 0
	.byte 0,0,32,65,128,1,1,0
	.long 0
	.long LT..__builtin_avcall-.__builtin_avcall
	.short 16
	.byte "__builtin_avcall"
_section_.text:
.csect .data[RW]
	.long _section_.text
