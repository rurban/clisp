#NO_APP
gcc2_compiled.:
___gnu_compiled_c:
.text
	.even
.globl ___builtin_avcall
___builtin_avcall:
	moveml #0x3038,sp@-
	movel sp@(24),a3
	addw #-1076,sp
	moveq #-32,d0
	addl a3@(20),d0
	subl a3,d0
	asrl #2,d0
	moveq #0,d1
	cmpl d1,d0
	jle L3
	movel sp,a0
	movel a3,a1
L5:
	movel a1@(32),a0@+
	addqw #4,a1
	addql #1,d1
	cmpl d1,d0
	jgt L5
L3:
	moveq #16,d3
	cmpl a3@(12),d3
	jne L7
	movel a3@(8),a3@(1092)
L7:
#APP
	moveml #32764,sp@(1024)
	movel #L8,sp@-
	movel a3@,sp@-
	moveml a3@(1056),#x7fff
	rts
#NO_APP
L8:
#APP
	moveml sp@(1024),#32764
#NO_APP
	addw #1076,sp
	moveq #16,d3
	cmpl a3@(12),d3
	jcs L32
	movel a3@(12),d2
	addl d2,d2
LI54:
	movew pc@(L54-LI54-2:b,d2:l),d2
	jmp pc@(2,d2:w)
L54:
	.word L34-L54
	.word L32-L54
	.word L31-L54
	.word L31-L54
	.word L31-L54
	.word L33-L54
	.word L33-L54
	.word L34-L54
	.word L34-L54
	.word L34-L54
	.word L34-L54
	.word L35-L54
	.word L35-L54
	.word L23-L54
	.word L26-L54
	.word L34-L54
	.word L28-L54
L23:
	btst #5,a3@(7)
	jeq L24
	movel a3@(8),a0
	movel d1,sp@-
	movel d0,sp@-
	fmoved sp@+,fp0
	fmoves fp0,a0@
	jra L32
L24:
	movel a3@(8),a0
	movel d0,a0@
	jra L32
L26:
	movel a3@(8),a0
	movel d0,a0@
	movel d1,a0@(4)
	jra L32
L28:
	btst #0,a3@(6)
	jeq L29
	movel a3@(16),d2
	moveq #2,d3
	cmpl d2,d3
	jeq L33
	jcs L38
	moveq #1,d3
	cmpl d2,d3
	jeq L31
	jra L29
L38:
	moveq #4,d3
	cmpl d2,d3
	jeq L34
	moveq #8,d3
	cmpl d2,d3
	jeq L35
	jra L29
L31:
	movel a3@(8),a0
	moveb d0,a0@
	jra L32
L33:
	movel a3@(8),a0
	movew d0,a0@
	jra L32
L34:
	movel a3@(8),a0
	movel d0,a0@
	jra L32
L35:
	movel a3@(8),a0
	movel d0,a0@
	movel a3@(8),a0
	movel d1,a0@(4)
	jra L32
L29:
	btst #0,a3@(7)
	jeq L32
	movel a3@(16),d1
	moveq #2,d3
	cmpl d1,d3
	jeq L42
	jcs L51
	moveq #1,d3
	cmpl d1,d3
	jeq L41
	jra L45
L51:
	moveq #4,d3
	cmpl d1,d3
	jeq L43
	moveq #8,d3
	cmpl d1,d3
	jeq L44
	jra L45
L41:
	movel a3@(8),a1
	movel d0,a0
	moveb a0@,a1@
	jra L32
L42:
	movel a3@(8),a1
	movel d0,a0
	movew a0@,a1@
	jra L32
L43:
	movel a3@(8),a1
	movel d0,a0
	movel a0@,a1@
	jra L32
L44:
	movel a3@(8),a0
	movel d0,a2
	movel a2@,a0@
	movel a3@(8),a1
	movew #4,a0
	movel a0@(a2:l),a1@(4)
	jra L32
L45:
	movel a3@(16),d1
	addql #3,d1
	movel d1,d2
	lsrl #2,d2
	subql #1,d2
	jmi L32
L48:
	movel a3@(8),a0
	movel d2,d1
	asll #2,d1
	movel d1,a4
	movel a4@(d0:l),a0@(d1:l)
	dbra d2,L48
	clrw d2
	subql #1,d2
	jcc L48
L32:
	moveq #0,d0
	moveml sp@+,#0x1c0c
	rts
