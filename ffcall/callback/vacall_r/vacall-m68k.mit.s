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
	movel a6@(-20),a2
	movel a2,d3
	jeq L3
	moveq #1,d4
	cmpl a2,d4
	jeq L48
	moveq #2,d4
	cmpl a2,d4
	jne L6
L48:
	moveb a6@(-12),d0
	extbl d0
	jra L3
	.even
L6:
	moveq #3,d4
	cmpl a2,d4
	jne L8
	clrl d0
	moveb a6@(-12),d0
	jra L3
	.even
L8:
	moveq #4,d4
	cmpl a2,d4
	jne L10
	movew a6@(-12),d0
	extl d0
	jra L3
	.even
L10:
	moveq #5,d4
	cmpl a2,d4
	jne L12
	clrl d0
	movew a6@(-12),d0
	jra L3
	.even
L12:
	moveq #6,d4
	cmpl a2,d4
	jeq L49
	moveq #7,d4
	cmpl a2,d4
	jeq L49
	moveq #8,d4
	cmpl a2,d4
	jeq L49
	moveq #9,d4
	cmpl a2,d4
	jne L20
L49:
	movel a6@(-12),d0
	jra L3
	.even
L20:
	moveq #-10,d2
	addl a2,d2
	moveq #1,d4
	cmpl d2,d4
	jcs L22
	movel a6@(-12),d0
	movel a6@(-8),d1
	jra L3
	.even
L22:
	moveq #12,d4
	cmpl d3,d4
	jne L24
	movel a6@(-32),d2
	btst #6,d2
	jeq L25
	fmoves a6@(-12),fp0
	jra L3
	.even
L25:
	btst #5,d2
	jeq L27
	fmoves a6@(-12),fp1
	fmoved fp1,sp@-
	movel sp@+,d0
	movel sp@+,d1
	jra L3
	.even
L27:
	movel a6@(-12),d0
	jra L3
	.even
L24:
	moveq #13,d4
	cmpl a2,d4
	jne L30
	btst #6,a6@(-29)
	jeq L31
	fmoved a6@(-12),fp0
	jra L3
	.even
L31:
	movel a6@(-12),d0
	movel a6@(-8),d1
	jra L3
	.even
L30:
	moveq #14,d4
	cmpl a2,d4
	jne L34
	movel a6@(-12),d0
	jra L50
	.even
L34:
	moveq #15,d4
	cmpl a2,d4
	jne L3
	movel a6@(-32),d3
	btst #10,d3
	jeq L37
	movel a6@(-16),d2
	moveq #1,d4
	cmpl d2,d4
	jne L38
	movel a6@(-24),a2
	clrl d0
	moveb a2@,d0
	jra L3
	.even
L38:
	moveq #2,d4
	cmpl d2,d4
	jne L41
	movel a6@(-24),a2
	clrl d0
	movew a2@,d0
	jra L3
	.even
L41:
	moveq #4,d4
	cmpl d2,d4
	jne L43
	movel a6@(-24),a2
	movel a2@,d0
	jra L3
	.even
L43:
	moveq #8,d4
	cmpl d2,d4
	jne L37
	movel a6@(-24),a2
	movel a2@,d0
	movel a2@(4),d1
	jra L3
	.even
L37:
	btst #0,d3
	jeq L3
	movel a6@(-24),d0
L50:
	movel d0,a0
L3:
	moveml a6@(-52),#0xc1c
	unlk a6
	rts
