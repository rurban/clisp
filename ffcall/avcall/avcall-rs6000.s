	.file	"avcall-rs6000.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
.toc
LC..0:
	.tc L..23[TC],L..23
LC..1:
	.tc L..66[TC],L..66
.csect .text[PR]
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
	srai 10,0,2
	cmp 1,3,10
	bc 4,4,L..3
	cal 11,32(31)
	cal 9,88(1)
L..5:
	cal 3,1(3)
	l 0,32(11)
	cmp 1,3,10
	cal 11,4(11)
	st 0,-32(9)
	cal 9,4(9)
	bc 12,4,L..5
L..3:
	l 0,1056(31)
	ai 0,0,-1060
	sf 0,31,0
	srai 0,0,3
	cmpli 1,0,13
	bc 12,5,L..9
	l 9,LC..0(2)
	sli 0,0,2
	lx 0,9,0
	cax 0,0,9
	mtctr 0
	bctr
	.align 2
L..23:
	.long L..7-L..23
	.long L..21-L..23
	.long L..20-L..23
	.long L..19-L..23
	.long L..18-L..23
	.long L..17-L..23
	.long L..16-L..23
	.long L..15-L..23
	.long L..14-L..23
	.long L..13-L..23
	.long L..12-L..23
	.long L..11-L..23
	.long L..10-L..23
	.long L..9-L..23
L..9:
	lfd 13,1156(31)
L..10:
	lfd 12,1148(31)
L..11:
	lfd 11,1140(31)
L..12:
	lfd 10,1132(31)
L..13:
	lfd 9,1124(31)
L..14:
	lfd 8,1116(31)
L..15:
	lfd 7,1108(31)
L..16:
	lfd 6,1100(31)
L..17:
	lfd 5,1092(31)
L..18:
	lfd 4,1084(31)
L..19:
	lfd 3,1076(31)
L..20:
	lfd 2,1068(31)
L..21:
	lfd 1,1060(31)
L..7:
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
	cmpli 1,0,16
	bc 12,5,L..24
	l 9,LC..1(2)
	sli 0,0,2
	lx 0,9,0
	cax 0,0,9
	mtctr 0
	bctr
	.align 2
L..66:
	.long L..60-L..66
	.long L..24-L..66
	.long L..58-L..66
	.long L..58-L..66
	.long L..58-L..66
	.long L..59-L..66
	.long L..59-L..66
	.long L..60-L..66
	.long L..60-L..66
	.long L..60-L..66
	.long L..60-L..66
	.long L..61-L..66
	.long L..61-L..66
	.long L..38-L..66
	.long L..39-L..66
	.long L..60-L..66
	.long L..41-L..66
L..38:
	l 9,8(31)
	frsp 0,1
	stfs 0,0(9)
	b L..24
L..39:
	l 9,8(31)
	stfd 1,0(9)
	b L..24
L..41:
	l 9,4(31)
	andil. 0,9,1
	bc 12,2,L..42
	l 0,16(31)
	cmpi 1,0,2
	bc 12,6,L..45
	cmpli 1,0,2
	bc 12,5,L..54
	cmpi 1,0,1
	bc 12,6,L..44
	b L..48
L..54:
	cmpi 1,0,4
	bc 12,6,L..46
	cmpi 1,0,8
	bc 12,6,L..47
	b L..48
L..44:
	l 9,8(31)
	lbz 0,0(3)
	stb 0,0(9)
	b L..24
L..45:
	l 9,8(31)
	lhz 0,0(3)
	sth 0,0(9)
	b L..24
L..46:
	l 9,8(31)
	l 0,0(3)
	st 0,0(9)
	b L..24
L..47:
	l 9,8(31)
	l 0,0(3)
	st 0,0(9)
	l 9,8(31)
	l 0,4(3)
	st 0,4(9)
	b L..24
L..48:
	l 0,16(31)
	ai 0,0,3
	sri 0,0,2
	ai. 0,0,-1
	bc 12,0,L..24
	sli 11,0,2
L..51:
	l 9,8(31)
	lx 0,11,3
	stx 0,11,9
	ai. 11,11,-4
	bc 4,0,L..51
	b L..24
L..42:
	andil. 0,9,256
	bc 12,2,L..24
	l 0,16(31)
	cmpi 1,0,2
	bc 12,6,L..59
	cmpli 1,0,2
	bc 12,5,L..64
	cmpi 1,0,1
	bc 12,6,L..58
	b L..24
L..64:
	cmpi 1,0,4
	bc 12,6,L..60
	cmpi 1,0,8
	bc 12,6,L..61
	b L..24
L..58:
	l 9,8(31)
	stb 3,0(9)
	b L..24
L..59:
	l 9,8(31)
	sth 3,0(9)
	b L..24
L..60:
	l 9,8(31)
	st 3,0(9)
	b L..24
L..61:
	l 9,8(31)
	st 3,0(9)
	l 9,8(31)
	st 4,4(9)
L..24:
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
