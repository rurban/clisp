#NO_APP
gcc2_compiled.:
___gnu_compiled_c:
.text
	.even
.globl ___vacall_r
___vacall_r:
	link a6,#-32
	moveml #0x3830,sp@-
	clrl a6@(-32)
	lea a6@(8),a3
	movel a3,a6@(-28)
	clrl a6@(-24)
	clrl a6@(-20)
	movel a1,a6@(-4)
	pea a6@(-32)
	movel a0@(4),sp@-
	movel a0@,a2
	jbsr a2@
	addql #8,sp
	movel a6@(-20),d2
	moveq #15,d4
	cmpl d2,d4
	jcs L2
LI37:
	movew pc@(L37-LI37-2:b,d2:l:2),d2
	jmp pc@(2,d2:w)
	.even
L37:
	.word L2-L37
	.word L4-L37
	.word L5-L37
	.word L6-L37
	.word L7-L37
	.word L8-L37
	.word L12-L37
	.word L12-L37
	.word L11-L37
	.word L12-L37
	.word L14-L37
	.word L14-L37
	.word L15-L37
	.word L20-L37
	.word L23-L37
	.word L24-L37
	.even
L4:
L5:
	moveb a6@(-12),d0
	extbl d0
	jra L2
	.even
L6:
	clrl d0
	moveb a6@(-12),d0
	jra L2
	.even
L7:
	movew a6@(-12),d0
	extl d0
	jra L2
	.even
L8:
	clrl d0
	movew a6@(-12),d0
	jra L2
	.even
L11:
L12:
	movel a6@(-12),d0
	jra L2
	.even
L14:
	movel a6@(-12),d0
	movel a6@(-8),d1
	jra L2
	.even
L15:
	movel a6@(-32),d2
	btst #6,d2
	jeq L16
	fmoves a6@(-12),fp0
	jra L2
	.even
L16:
	btst #5,d2
	jeq L18
	fmoves a6@(-12),fp1
	fmoved fp1,sp@-
	movel sp@+,d0
	movel sp@+,d1
	jra L2
	.even
L18:
	movel a6@(-12),d0
	jra L2
	.even
L20:
	btst #6,a6@(-29)
	jeq L21
	fmoved a6@(-12),fp0
	jra L2
	.even
L21:
	movel a6@(-12),d0
	movel a6@(-8),d1
	jra L2
	.even
L23:
	movel a6@(-12),d0
	jra L39
	.even
L24:
	movel a6@(-32),d3
	btst #10,d3
	jeq L25
	movel a6@(-16),d2
	moveq #2,d4
	cmpl d2,d4
	jeq L29
	jcs L34
	moveq #1,d4
	cmpl d2,d4
	jeq L27
	jra L25
	.even
L34:
	moveq #4,d4
	cmpl d2,d4
	jeq L30
	moveq #8,d4
	cmpl d2,d4
	jeq L31
	jra L25
	.even
L27:
	movel a6@(-24),a2
	clrl d0
	moveb a2@,d0
	jra L2
	.even
L29:
	movel a6@(-24),a2
	clrl d0
	movew a2@,d0
	jra L2
	.even
L30:
	movel a6@(-24),a2
	movel a2@,d0
	jra L2
	.even
L31:
	movel a6@(-24),a2
	movel a2@,d0
	movel a2@(4),d1
	jra L2
	.even
L25:
	btst #0,d3
	jeq L2
	movel a6@(-24),d0
L39:
	movel d0,a0
L2:
	moveml a6@(-52),#0xc1c
	unlk a6
	rts
