	.file	"vacall-m68k.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 	2
.globl vacall
	.type	 vacall,@function
vacall:
	link.w %a6,#-32
	movm.l #0x3830,-(%sp)
	clr.l -32(%a6)
	lea (8,%a6),%a3
	move.l %a3,-28(%a6)
	clr.l -24(%a6)
	clr.l -20(%a6)
	move.l %a1,-4(%a6)
	pea -32(%a6)
	move.l vacall_function,%a2
	jbsr (%a2)
	addq.l #4,%sp
	move.l -20(%a6),%a2
	move.l %a2,%d3
	jbeq .L3
	moveq.l #1,%d4
	cmp.l %a2,%d4
	jbeq .L48
	moveq.l #2,%d4
	cmp.l %a2,%d4
	jbne .L6
.L48:
	move.b -12(%a6),%d0
	extb.l %d0
	jbra .L3
	.align 	2
.L6:
	moveq.l #3,%d4
	cmp.l %a2,%d4
	jbne .L8
	clr.l %d0
	move.b -12(%a6),%d0
	jbra .L3
	.align 	2
.L8:
	moveq.l #4,%d4
	cmp.l %a2,%d4
	jbne .L10
	move.w -12(%a6),%d0
	ext.l %d0
	jbra .L3
	.align 	2
.L10:
	moveq.l #5,%d4
	cmp.l %a2,%d4
	jbne .L12
	clr.l %d0
	move.w -12(%a6),%d0
	jbra .L3
	.align 	2
.L12:
	moveq.l #6,%d4
	cmp.l %a2,%d4
	jbeq .L49
	moveq.l #7,%d4
	cmp.l %a2,%d4
	jbeq .L49
	moveq.l #8,%d4
	cmp.l %a2,%d4
	jbeq .L49
	moveq.l #9,%d4
	cmp.l %a2,%d4
	jbne .L20
.L49:
	move.l -12(%a6),%d0
	jbra .L3
	.align 	2
.L20:
	moveq.l #-10,%d2
	add.l %a2,%d2
	moveq.l #1,%d4
	cmp.l %d2,%d4
	jbcs .L22
	move.l -12(%a6),%d0
	move.l -8(%a6),%d1
	jbra .L3
	.align 	2
.L22:
	moveq.l #12,%d4
	cmp.l %d3,%d4
	jbne .L24
	move.l -32(%a6),%d2
	btst #6,%d2
	jbeq .L25
	fmove.s -12(%a6),%fp0
	jbra .L3
	.align 	2
.L25:
	btst #5,%d2
	jbeq .L27
	fmove.s -12(%a6),%fp1
	fmove.d %fp1,-(%sp)
	move.l (%sp)+,%d0
	move.l (%sp)+,%d1
	jbra .L3
	.align 	2
.L27:
	move.l -12(%a6),%d0
	jbra .L3
	.align 	2
.L24:
	moveq.l #13,%d4
	cmp.l %a2,%d4
	jbne .L30
	btst #6,-29(%a6)
	jbeq .L31
	fmove.d -12(%a6),%fp0
	jbra .L3
	.align 	2
.L31:
	move.l -12(%a6),%d0
	move.l -8(%a6),%d1
	jbra .L3
	.align 	2
.L30:
	moveq.l #14,%d4
	cmp.l %a2,%d4
	jbne .L34
	move.l -12(%a6),%d0
	jbra .L50
	.align 	2
.L34:
	moveq.l #15,%d4
	cmp.l %a2,%d4
	jbne .L3
	move.l -32(%a6),%d3
	btst #10,%d3
	jbeq .L37
	move.l -16(%a6),%d2
	moveq.l #1,%d4
	cmp.l %d2,%d4
	jbne .L38
	move.l -24(%a6),%a2
	clr.l %d0
	move.b (%a2),%d0
	jbra .L3
	.align 	2
.L38:
	moveq.l #2,%d4
	cmp.l %d2,%d4
	jbne .L41
	move.l -24(%a6),%a2
	clr.l %d0
	move.w (%a2),%d0
	jbra .L3
	.align 	2
.L41:
	moveq.l #4,%d4
	cmp.l %d2,%d4
	jbne .L43
	move.l -24(%a6),%a2
	move.l (%a2),%d0
	jbra .L3
	.align 	2
.L43:
	moveq.l #8,%d4
	cmp.l %d2,%d4
	jbne .L37
	move.l -24(%a6),%a2
	move.l (%a2),%d0
	move.l 4(%a2),%d1
	jbra .L3
	.align 	2
.L37:
	btst #0,%d3
	jbeq .L3
	move.l -24(%a6),%d0
.L50:
	move.l %d0,%a0
.L3:
	movm.l -52(%a6),#0xc1c
	unlk %a6
	rts
.Lfe1:
	.size	 vacall,.Lfe1-vacall
	.ident	"GCC: (GNU) egcs-2.91.57 19980901 (egcs-1.1 release)"
