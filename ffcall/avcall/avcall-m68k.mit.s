#NO_APP
gcc2_compiled.:
___gnu_compiled_c:
.text
	.even
.globl ___builtin_avcall
___builtin_avcall:
	moveml #0x3030,sp@-
	movel sp@(20),a2
	lea sp@(-1024),sp
	moveq #-32,d0
	addl a2@(20),d0
	subl a2,d0
	asrl #2,d0
	subl a1,a1
	cmpl a1,d0
	jle L3
	movel sp,a3
	lea a2@(32),a0
	.even
L5:
	movel a0@+,a3@+
	addql #1,a1
	cmpl a1,d0
	jgt L5
L3:
	moveq #16,d3
	cmpl a2@(12),d3
	jne L7
#APP
	movel a2@(8),a1
#NO_APP
L7:
	movel a2@,a0
	jbsr a0@
	movel d0,a1
	movel a2@(12),d2
	moveq #16,d3
	cmpl d2,d3
	jcs L35
LI57:
	movew pc@(L57-LI57-2:b,d2:l:2),d2
	jmp pc@(2,d2:w)
	.even
L57:
	.word L37-L57
	.word L35-L57
	.word L34-L57
	.word L34-L57
	.word L34-L57
	.word L36-L57
	.word L36-L57
	.word L37-L57
	.word L37-L57
	.word L37-L57
	.word L37-L57
	.word L38-L57
	.word L38-L57
	.word L22-L57
	.word L27-L57
	.word L37-L57
	.word L31-L57
	.even
L22:
	movel a2@(4),d2
	btst #6,d2
	jeq L23
	movel a2@(8),a0
	fmoves fp0,a0@
	jra L35
	.even
L23:
	btst #5,d2
	jeq L25
	movel a2@(8),a0
	movel d1,sp@-
	movel d0,sp@-
	fmoved sp@+,fp1
	fmoves fp1,a0@
	jra L35
	.even
L25:
	movel a2@(8),a0
	movel d0,a0@
	jra L35
	.even
L27:
	btst #6,a2@(7)
	jeq L28
	movel a2@(8),a0
	fmoved fp0,a0@
	jra L35
	.even
L28:
	movel a2@(8),a0
	movel d0,a0@
	movel d1,a0@(4)
	jra L35
	.even
L31:
	movel a2@(4),d2
	btst #9,d2
	jeq L32
	movel a2@(16),d0
	moveq #2,d3
	cmpl d0,d3
	jeq L36
	jcs L41
	moveq #1,d3
	cmpl d0,d3
	jeq L34
	jra L32
	.even
L41:
	moveq #4,d3
	cmpl d0,d3
	jeq L37
	moveq #8,d3
	cmpl d0,d3
	jeq L38
	jra L32
	.even
L34:
	movel a2@(8),a0
	movew a1,d3
	moveb d3,a0@
	jra L35
	.even
L36:
	movel a2@(8),a0
	movew a1,a0@
	jra L35
	.even
L37:
	movel a2@(8),a0
	movel a1,a0@
	jra L35
	.even
L38:
	movel a2@(8),a0
	movel a1,a0@
	movel a2@(8),a0
	movel d1,a0@(4)
	jra L35
	.even
L32:
	btst #0,d2
	jeq L35
	movel a2@(16),d0
	moveq #2,d3
	cmpl d0,d3
	jeq L45
	jcs L54
	moveq #1,d3
	cmpl d0,d3
	jeq L44
	jra L48
	.even
L54:
	moveq #4,d3
	cmpl d0,d3
	jeq L46
	moveq #8,d3
	cmpl d0,d3
	jeq L47
	jra L48
	.even
L44:
	movel a2@(8),a0
	moveb a1@,a0@
	jra L35
	.even
L45:
	movel a2@(8),a0
	movew a1@,a0@
	jra L35
	.even
L46:
	movel a2@(8),a0
	movel a1@,a0@
	jra L35
	.even
L47:
	movel a2@(8),a0
	movel a1@,a0@
	movel a2@(8),a0
	movel a1@(4),a0@(4)
	jra L35
	.even
L48:
	addql #3,d0
	lsrl #2,d0
	subql #1,d0
	jmi L35
	lea a1@(d0:l:4),a1
	.even
L51:
	movel a2@(8),a0
	movel a1@,a0@(d0:l:4)
	subql #4,a1
	dbra d0,L51
	clrw d0
	subql #1,d0
	jcc L51
L35:
	lea sp@(1024),sp
	clrl d0
	moveml sp@+,#0xc0c
	rts
