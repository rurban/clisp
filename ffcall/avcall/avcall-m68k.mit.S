#include "asmm68k.h"
.text
	.even
.globl C(__builtin_avcall)
	DECLARE_FUNCTION(__builtin_avcall)
C(__builtin_avcall:)
	moveml #0x3030,$sp@-
	movel $sp@(20),$a2
	lea $sp@(-1024),$sp
	moveq #-32,$d0
	addl $a2@(20),$d0
	subl $a2,$d0
	asrl #2,$d0
	subl $a1,$a1
	cmpl $a1,$d0
	jle L3
	movel $sp,$a3
	lea $a2@(32),$a0
	.even
L5:
	movel $a0@+,$a3@+
	addql #1,$a1
	cmpl $a1,$d0
	jgt L5
L3:
	moveq #16,$d3
	cmpl $a2@(12),$d3
	jne L7
	movel $a2@(8),$a1
L7:
	movel $a2@,$a0
	jbsr $a0@
	movel $d0,$a1
	movel $a2@(12),$a0
	movel $a0,$d2
	moveq #1,$d3
	cmpl $a0,$d3
	jeq L9
	tstl $a0
	jeq L68
	moveq #2,$d3
	cmpl $a0,$d3
	jeq L69
	moveq #3,$d3
	cmpl $a0,$d3
	jeq L69
	moveq #4,$d3
	cmpl $a0,$d3
	jeq L69
	moveq #5,$d3
	cmpl $a0,$d3
	jeq L70
	moveq #6,$d3
	cmpl $a0,$d3
	jeq L70
	moveq #7,$d3
	cmpl $a0,$d3
	jeq L68
	moveq #8,$d3
	cmpl $a0,$d3
	jeq L68
	moveq #9,$d3
	cmpl $a0,$d3
	jeq L68
	moveq #10,$d3
	cmpl $a0,$d3
	jeq L68
	moveq #-11,$d3
	addl $d3,$d2
	moveq #1,$d3
	cmpl $d2,$d3
	jcc L71
	moveq #13,$d3
	cmpl $a0,$d3
	jne L32
	movel $a2@(4),$d2
	btst #6,$d2
	jeq L33
	movel $a2@(8),$a0
	fmoves $fp0,$a0@
	jra L9
	.even
L33:
	btst #5,$d2
	jeq L35
	movel $a2@(8),$a0
	movel $d1,$sp@-
	movel $d0,$sp@-
	fmoved $sp@+,$fp1
	fmoves $fp1,$a0@
	jra L9
	.even
L35:
	movel $a2@(8),$a0
	movel $d0,$a0@
	jra L9
	.even
L32:
	moveq #14,$d3
	cmpl $a0,$d3
	jne L38
	btst #6,$a2@(7)
	jeq L39
	movel $a2@(8),$a0
	fmoved $fp0,$a0@
	jra L9
	.even
L39:
	movel $a2@(8),$a0
	movel $d0,$a0@
	movel $d1,$a0@(4)
	jra L9
	.even
L38:
	moveq #15,$d3
	cmpl $a0,$d3
	jeq L68
	moveq #16,$d3
	cmpl $a0,$d3
	jne L9
	movel $a2@(4),$d2
	btst #9,$d2
	jeq L45
	movel $a2@(16),$d0
	moveq #1,$d3
	cmpl $d0,$d3
	jne L46
L69:
	movel $a2@(8),$a0
	movew $a1,$d3
	moveb $d3,$a0@
	jra L9
	.even
L46:
	moveq #2,$d3
	cmpl $d0,$d3
	jne L49
L70:
	movel $a2@(8),$a0
	movew $a1,$a0@
	jra L9
	.even
L49:
	moveq #4,$d3
	cmpl $d0,$d3
	jne L51
L68:
	movel $a2@(8),$a0
	movel $a1,$a0@
	jra L9
	.even
L51:
	moveq #8,$d3
	cmpl $d0,$d3
	jne L45
L71:
	movel $a2@(8),$a0
	movel $a1,$a0@
	movel $a2@(8),$a0
	movel $d1,$a0@(4)
	jra L9
	.even
L45:
	btst #0,$d2
	jeq L9
	movel $a2@(16),$d0
	moveq #1,$d3
	cmpl $d0,$d3
	jne L55
	movel $a2@(8),$a0
	moveb $a1@,$a0@
	jra L9
	.even
L55:
	moveq #2,$d3
	cmpl $d0,$d3
	jne L57
	movel $a2@(8),$a0
	movew $a1@,$a0@
	jra L9
	.even
L57:
	moveq #4,$d3
	cmpl $d0,$d3
	jne L59
	movel $a2@(8),$a0
	movel $a1@,$a0@
	jra L9
	.even
L59:
	moveq #8,$d3
	cmpl $d0,$d3
	jne L61
	movel $a2@(8),$a0
	movel $a1@,$a0@
	movel $a2@(8),$a0
	movel $a1@(4),$a0@(4)
	jra L9
	.even
L61:
	addql #3,$d0
	lsrl #2,$d0
	subql #1,$d0
	jmi L9
	lea $a1@($d0:l:4),$a1
	.even
L65:
	movel $a2@(8),$a0
	movel $a1@,$a0@($d0:l:4)
	subql #4,$a1
	dbra $d0,L65
	clrw $d0
	subql #1,$d0
	jcc L65
L9:
	lea $sp@(1024),$sp
	clrl $d0
	moveml $sp@+,#0xc0c
	rts
