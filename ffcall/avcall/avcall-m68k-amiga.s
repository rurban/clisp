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
	movel a2@(12),a0
	movel a0,d2
	moveq #1,d4
	cmpl a0,d4
	jeq L10
	cmpw #0,a0
	jeq L65
	moveq #2,d4
	cmpl a0,d4
	jeq L66
	moveq #3,d4
	cmpl a0,d4
	jeq L66
	moveq #4,d4
	cmpl a0,d4
	jeq L66
	moveq #5,d4
	cmpl a0,d4
	jeq L67
	moveq #6,d4
	cmpl a0,d4
	jeq L67
	moveq #7,d4
	cmpl a0,d4
	jeq L65
	moveq #8,d4
	cmpl a0,d4
	jeq L65
	moveq #9,d4
	cmpl a0,d4
	jeq L65
	moveq #10,d4
	cmpl a0,d4
	jeq L65
	moveq #-11,d4
	addl d4,d2
	moveq #1,d4
	cmpl d2,d4
	jcc L68
	moveq #13,d4
	cmpl a0,d4
	jne L33
	btst #5,a2@(7)
	jeq L34
	movel a2@(8),a2
	movel d1,sp@-
	movel d0,sp@-
	jbsr ___truncdfsf2
	addql #8,sp
	movel d0,a2@
	jra L10
	.even
L34:
	movel a2@(8),a0
	movel d0,a0@
	jra L10
	.even
L33:
	moveq #14,d4
	cmpl a0,d4
	jne L37
	movel a2@(8),a0
	movel d0,a0@
	movel d1,a0@(4)
	jra L10
	.even
L37:
	moveq #15,d4
	cmpl a0,d4
	jeq L65
	moveq #16,d4
	cmpl a0,d4
	jne L10
	movel a2@(4),d3
	btst #9,d3
	jeq L42
	movel a2@(16),d2
	moveq #1,d4
	cmpl d2,d4
	jne L43
L66:
	movel a2@(8),a0
	moveb d0,a0@
	jra L10
	.even
L43:
	moveq #2,d4
	cmpl d2,d4
	jne L46
L67:
	movel a2@(8),a0
	movew d0,a0@
	jra L10
	.even
L46:
	moveq #4,d4
	cmpl d2,d4
	jne L48
L65:
	movel a2@(8),a0
	movel d0,a0@
	jra L10
	.even
L48:
	moveq #8,d4
	cmpl d2,d4
	jne L42
L68:
	movel a2@(8),a0
	movel d0,a0@
	movel a2@(8),a0
	movel d1,a0@(4)
	jra L10
	.even
L42:
	btst #0,d3
	jeq L10
	movel a2@(16),d1
	moveq #1,d4
	cmpl d1,d4
	jne L52
	movel a2@(8),a1
	movel d0,a0
	moveb a0@,a1@
	jra L10
	.even
L52:
	moveq #2,d4
	cmpl d1,d4
	jne L54
	movel a2@(8),a1
	movel d0,a0
	movew a0@,a1@
	jra L10
	.even
L54:
	moveq #4,d4
	cmpl d1,d4
	jne L56
	movel a2@(8),a1
	movel d0,a0
	movel a0@,a1@
	jra L10
	.even
L56:
	moveq #8,d4
	cmpl d1,d4
	jne L58
	movel a2@(8),a0
	movel d0,a1
	movel a1@,a0@
	movel a2@(8),a0
	moveq #4,d0
	movel a1@(d0:l),a0@(4)
	jra L10
	.even
L58:
	addql #3,d1
	lsrl #2,d1
	subql #1,d1
	jmi L10
	movel d1,a1
	movel a1,d4
	lsll #2,d4
	movel d4,a1
	.even
L62:
	movel a2@(8),a0
	movel a1@(d0:l),a0@(a1:l)
	subql #4,a1
	dbra d1,L62
	clrw d1
	subql #1,d1
	jcc L62
L10:
	moveq #0,d0
	moveml sp@+,#0x41c
	rts
