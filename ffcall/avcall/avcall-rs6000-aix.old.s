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
	.extern __mulh
	.extern __mull
	.extern __divss
	.extern __divus
	.extern __quoss
	.extern __quous
	mflr 0
	st 28,-16(1)
	st 29,-12(1)
	st 30,-8(1)
	st 31,-4(1)
	st 0,8(1)
	stu 1,-1096(1)
	mr 31,3
	l 9,20(31)
	lil 3,8
	cal 9,-32(9)
	sf 9,31,9
	srai 9,9,2
	cmp 0,3,9
	cal 0,56(1)
	bc 4,0,L..3
	mr 10,0
	cal 8,32(31)
	lil 11,32
L..5:
	cal 3,1(3)
	lx 0,11,8
	cmp 0,3,9
	st 0,0(10)
	cal 10,4(10)
	cal 11,4(11)
	bc 12,0,L..5
L..3:
	l 9,1056(31)
	cal 9,-1060(9)
	sf 9,31,9
	srai. 9,9,3
	bc 12,2,L..8
	cmpi 0,9,1
	bc 12,2,L..11
	cmpi 0,9,2
	bc 12,2,L..14
	cmpi 0,9,3
	bc 12,2,L..17
	cmpi 0,9,4
	bc 12,2,L..20
	cmpi 0,9,5
	bc 12,2,L..23
	cmpi 0,9,6
	bc 12,2,L..26
	cmpi 0,9,7
	bc 12,2,L..29
	cmpi 0,9,8
	bc 12,2,L..32
	cmpi 0,9,9
	bc 12,2,L..35
	cmpi 0,9,10
	bc 12,2,L..38
	cmpi 0,9,11
	bc 12,2,L..41
	cmpi 0,9,12
	bc 12,2,L..44
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
	l 0,0(31)
	l 3,32(31)
	l 4,36(31)
	l 5,40(31)
	l 6,44(31)
	l 7,48(31)
	l 8,52(31)
	l 9,56(31)
	l 10,60(31)
	mr 28,0
	st 2,20(1)
	l 29,0(28)
	l 2,4(28)
	mtlr 29
	l 11,8(28)
	brl
	l 2,20(1)
	l 9,12(31)
	cmpi 0,9,1
	mr 11,9
	bc 12,2,L..49
	cmpi 0,9,0
	bc 12,2,L..101
	cmpi 0,9,2
	bc 12,2,L..102
	cmpi 0,9,3
	bc 12,2,L..102
	cmpi 0,9,4
	bc 12,2,L..102
	cmpi 0,9,5
	bc 12,2,L..103
	cmpi 0,9,6
	bc 12,2,L..103
	cmpi 0,9,7
	bc 12,2,L..101
	cmpi 0,9,8
	bc 12,2,L..101
	cmpi 0,9,9
	bc 12,2,L..101
	cmpi 0,9,10
	bc 12,2,L..101
	cal 0,-11(11)
	cmpli 0,0,1
	bc 4,1,L..104
	cmpi 0,9,13
	bc 4,2,L..72
	l 9,8(31)
	frsp 0,1
	stfs 0,0(9)
	b L..49
L..72:
	cmpi 0,9,14
	bc 4,2,L..74
	l 9,8(31)
	stfd 1,0(9)
	b L..49
L..74:
	cmpi 0,9,15
	bc 12,2,L..101
	cmpi 0,9,16
	bc 4,2,L..49
	l 0,4(31)
	andil. 28,0,1
	bc 12,2,L..79
	l 9,16(31)
	cmpi 0,9,1
	bc 4,2,L..80
	l 9,8(31)
	lbz 0,0(3)
	stb 0,0(9)
	b L..49
L..80:
	cmpi 0,9,2
	bc 4,2,L..82
	l 9,8(31)
	lhz 0,0(3)
	sth 0,0(9)
	b L..49
L..82:
	cmpi 0,9,4
	bc 4,2,L..84
	l 9,8(31)
	l 0,0(3)
	st 0,0(9)
	b L..49
L..84:
	cmpi 0,9,8
	bc 4,2,L..86
	l 11,8(31)
	l 0,0(3)
	st 0,0(11)
	l 10,8(31)
	l 9,4(3)
	st 9,4(10)
	b L..49
L..86:
	cal 0,3(9)
	sri 10,0,2
	ai. 10,10,-1
	bc 12,0,L..49
	sli 11,10,2
L..90:
	lx 0,11,3
	l 9,8(31)
	ai. 10,10,-1
	stx 0,11,9
	cal 11,-4(11)
	bc 4,0,L..90
	b L..49
L..79:
	andil. 29,0,512
	bc 12,2,L..49
	l 0,16(31)
	cmpi 0,0,1
	bc 4,2,L..94
L..102:
	l 9,8(31)
	stb 3,0(9)
	b L..49
L..94:
	cmpi 0,0,2
	bc 4,2,L..96
L..103:
	l 9,8(31)
	sth 3,0(9)
	b L..49
L..96:
	cmpi 0,0,4
	bc 4,2,L..98
L..101:
	l 9,8(31)
	st 3,0(9)
	b L..49
L..98:
	cmpi 0,0,8
	bc 4,2,L..49
L..104:
	l 9,8(31)
	st 3,0(9)
	l 11,8(31)
	st 4,4(11)
L..49:
	lil 3,0
	cal 1,1096(1)
	l 0,8(1)
	mtlr 0
	l 28,-16(1)
	l 29,-12(1)
	l 30,-8(1)
	l 31,-4(1)
	br
LT..__builtin_avcall:
	.long 0
	.byte 0,0,32,65,128,4,1,0
	.long 0
	.long LT..__builtin_avcall-.__builtin_avcall
	.short 16
	.byte "__builtin_avcall"
_section_.text:
.csect .data[RW]
	.long _section_.text
