#NO_APP
gcc2_compiled.:
___gnu_compiled_c:
.text
	.even
.globl ___builtin_avcall
___builtin_avcall:
	moveml #0x3820,sp@-
	movel sp@(20),a2
	lea sp@(-1076),sp
	moveq #-32,d1
	addl a2@(20),d1
	subl a2,d1
	asrl #2,d1
	moveq #0,d0
	cmpl d0,d1
	jle L3
	movel sp,a1
	lea a2@(32),a0
	.even
L5:
	movel a0@+,a1@+
	addql #1,d0
	cmpl d0,d1
	jgt L5
L3:
	moveq #16,d4
	cmpl a2@(12),d4
	jne L7
	movel a2@(8),a2@(1092)
L7:
#APP
	moveml #32764,sp@(1024)
	movel #L8,sp@-
	movel a2@,sp@-
	moveml a2@(1056),#x7fff
	rts
#NO_APP
L8:
#APP
	moveml sp@(1024),#32764
#NO_APP
	lea sp@(1076),sp
	movel a2@(12),d2
	moveq #16,d4
	cmpl d2,d4
	jcs L32
	addl d2,d2
LI54:
	movew pc@(L54-LI54-2:b,d2:l),d2
	jmp pc@(2,d2:w)
	.even
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
	.even
L23:
	btst #5,a2@(7)
	jeq L24
	movel a2@(8),a2
	movel d1,sp@-
	movel d0,sp@-
	jbsr ___truncdfsf2
	addql #8,sp
	movel d0,a2@
	jra L32
	.even
L24:
	movel a2@(8),a0
	movel d0,a0@
	jra L32
	.even
L26:
	movel a2@(8),a0
	movel d0,a0@
	movel d1,a0@(4)
	jra L32
	.even
L28:
	movel a2@(4),d3
	btst #9,d3
	jeq L29
	movel a2@(16),d2
	moveq #2,d4
	cmpl d2,d4
	jeq L33
	jcs L38
	moveq #1,d4
	cmpl d2,d4
	jeq L31
	jra L29
	.even
L38:
	moveq #4,d4
	cmpl d2,d4
	jeq L34
	moveq #8,d4
	cmpl d2,d4
	jeq L35
	jra L29
	.even
L31:
	movel a2@(8),a0
	moveb d0,a0@
	jra L32
	.even
L33:
	movel a2@(8),a0
	movew d0,a0@
	jra L32
	.even
L34:
	movel a2@(8),a0
	movel d0,a0@
	jra L32
	.even
L35:
	movel a2@(8),a0
	movel d0,a0@
	movel a2@(8),a0
	movel d1,a0@(4)
	jra L32
	.even
L29:
	btst #0,d3
	jeq L32
	movel a2@(16),d1
	moveq #2,d4
	cmpl d1,d4
	jeq L42
	jcs L51
	moveq #1,d4
	cmpl d1,d4
	jeq L41
	jra L45
	.even
L51:
	moveq #4,d4
	cmpl d1,d4
	jeq L43
	moveq #8,d4
	cmpl d1,d4
	jeq L44
	jra L45
	.even
L41:
	movel a2@(8),a1
	movel d0,a0
	moveb a0@,a1@
	jra L32
	.even
L42:
	movel a2@(8),a1
	movel d0,a0
	movew a0@,a1@
	jra L32
	.even
L43:
	movel a2@(8),a1
	movel d0,a0
	movel a0@,a1@
	jra L32
	.even
L44:
	movel a2@(8),a0
	movel d0,a1
	movel a1@,a0@
	movel a2@(8),a0
	moveq #4,d0
	movel a1@(d0:l),a0@(4)
	jra L32
	.even
L45:
	addql #3,d1
	lsrl #2,d1
	subql #1,d1
	jmi L32
	movel d1,a1
	movel a1,d4
	lsll #2,d4
	movel d4,a1
	.even
L48:
	movel a2@(8),a0
	movel a1@(d0:l),a0@(a1:l)
	subql #4,a1
	dbra d1,L48
	clrw d1
	subql #1,d1
	jcc L48
L32:
	moveq #0,d0
	moveml sp@+,#0x41c
	rts
