#NO_APP
gcc2_compiled.:
___gnu_compiled_c:
.text
	.even
.globl ___builtin_avcall
___builtin_avcall:
	moveml #0x3020,sp@-
	movel sp@(16),a2
	addw #-1024,sp
	moveq #-32,d0
	addl a2@(20),d0
	subl a2,d0
	asrl #2,d0
	subl a1,a1
	cmpl a1,d0
	jle L3
	movel sp,a0
L5:
	movel a2@(32,a1:l:4),a0@+
	addqw #1,a1
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
	moveq #16,d3
	cmpl a2@(12),d3
	jcs L31
	movel a2@(12),d2
LI53:
	movew pc@(L53-LI53-2:b,d2:l:2),d2
	jmp pc@(2,d2:w)
L53:
	.word L33-L53
	.word L31-L53
	.word L30-L53
	.word L30-L53
	.word L30-L53
	.word L32-L53
	.word L32-L53
	.word L33-L53
	.word L33-L53
	.word L33-L53
	.word L33-L53
	.word L34-L53
	.word L34-L53
	.word L22-L53
	.word L25-L53
	.word L33-L53
	.word L27-L53
L22:
	btst #5,a2@(7)
	jeq L23
	movel a2@(8),a0
	movel d1,sp@-
	movel d0,sp@-
	fmoved sp@+,fp0
	fmoves fp0,a0@
	jra L31
L23:
	movel a2@(8),a0
	movel d0,a0@
	jra L31
L25:
	movel a2@(8),a0
	movel d0,a0@
	movel d1,a0@(4)
	jra L31
L27:
	btst #0,a2@(6)
	jeq L28
	movel a2@(16),d0
	moveq #2,d3
	cmpl d0,d3
	jeq L32
	jcs L37
	moveq #1,d3
	cmpl d0,d3
	jeq L30
	jra L28
L37:
	moveq #4,d3
	cmpl d0,d3
	jeq L33
	moveq #8,d3
	cmpl d0,d3
	jeq L34
	jra L28
L30:
	movel a2@(8),a0
	exg d0,a1
	moveb d0,a0@
	exg d0,a1
	jra L31
L32:
	movel a2@(8),a0
	movew a1,a0@
	jra L31
L33:
	movel a2@(8),a0
	movel a1,a0@
	jra L31
L34:
	movel a2@(8),a0
	movel a1,a0@
	movel a2@(8),a0
	movel d1,a0@(4)
	jra L31
L28:
	btst #0,a2@(7)
	jeq L31
	movel a2@(16),d0
	moveq #2,d3
	cmpl d0,d3
	jeq L41
	jcs L50
	moveq #1,d3
	cmpl d0,d3
	jeq L40
	jra L44
L50:
	moveq #4,d3
	cmpl d0,d3
	jeq L42
	moveq #8,d3
	cmpl d0,d3
	jeq L43
	jra L44
L40:
	movel a2@(8),a0
	moveb a1@,a0@
	jra L31
L41:
	movel a2@(8),a0
	movew a1@,a0@
	jra L31
L42:
	movel a2@(8),a0
	movel a1@,a0@
	jra L31
L43:
	movel a2@(8),a0
	movel a1@,a0@
	movel a2@(8),a0
	movel a1@(4),a0@(4)
	jra L31
L44:
	movel a2@(16),d0
	addql #3,d0
	lsrl #2,d0
	subql #1,d0
	jmi L31
	lea a1@(d0:l:4),a1
L47:
	movel a2@(8),a0
	movel a1@,a0@(d0:l:4)
	subqw #4,a1
	dbra d0,L47
	clrw d0
	subql #1,d0
	jcc L47
L31:
	addw #1024,sp
	clrl d0
	moveml sp@+,#0x40c
	rts
