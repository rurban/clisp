	.file	"vacall-m68k.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 	2
.globl __vacall_r
	.type	 __vacall_r,@function
__vacall_r:
	link.w %a6,#-32
	movm.l #0x3830,-(%sp)
	clr.l -32(%a6)
	lea (8,%a6),%a3
	move.l %a3,-28(%a6)
	clr.l -24(%a6)
	clr.l -20(%a6)
	move.l %a1,-4(%a6)
	pea -32(%a6)
	move.l 4(%a0),-(%sp)
	move.l (%a0),%a2
	jbsr (%a2)
	addq.l #8,%sp
	move.l -20(%a6),%d2
	moveq.l #15,%d4
	cmp.l %d2,%d4
	jbcs .L2
	.set .LI37,.+2
	move.w .L37-.LI37.b(%pc,%d2.l*2),%d2
	jmp %pc@(2,%d2:w)
	.align 	2
.L37:
	.word .L2-.L37
	.word .L4-.L37
	.word .L5-.L37
	.word .L6-.L37
	.word .L7-.L37
	.word .L8-.L37
	.word .L12-.L37
	.word .L12-.L37
	.word .L11-.L37
	.word .L12-.L37
	.word .L14-.L37
	.word .L14-.L37
	.word .L15-.L37
	.word .L20-.L37
	.word .L23-.L37
	.word .L24-.L37
	.align 	2
.L4:
.L5:
	move.b -12(%a6),%d0
	extb.l %d0
	jbra .L2
	.align 	2
.L6:
	clr.l %d0
	move.b -12(%a6),%d0
	jbra .L2
	.align 	2
.L7:
	move.w -12(%a6),%d0
	ext.l %d0
	jbra .L2
	.align 	2
.L8:
	clr.l %d0
	move.w -12(%a6),%d0
	jbra .L2
	.align 	2
.L11:
.L12:
	move.l -12(%a6),%d0
	jbra .L2
	.align 	2
.L14:
	move.l -12(%a6),%d0
	move.l -8(%a6),%d1
	jbra .L2
	.align 	2
.L15:
	move.l -32(%a6),%d2
	btst #6,%d2
	jbeq .L16
	fmove.s -12(%a6),%fp0
	jbra .L2
	.align 	2
.L16:
	btst #5,%d2
	jbeq .L18
	fmove.s -12(%a6),%fp1
	fmove.d %fp1,-(%sp)
	move.l (%sp)+,%d0
	move.l (%sp)+,%d1
	jbra .L2
	.align 	2
.L18:
	move.l -12(%a6),%d0
	jbra .L2
	.align 	2
.L20:
	btst #6,-29(%a6)
	jbeq .L21
	fmove.d -12(%a6),%fp0
	jbra .L2
	.align 	2
.L21:
	move.l -12(%a6),%d0
	move.l -8(%a6),%d1
	jbra .L2
	.align 	2
.L23:
	move.l -12(%a6),%d0
	jbra .L39
	.align 	2
.L24:
	move.l -32(%a6),%d3
	btst #10,%d3
	jbeq .L25
	move.l -16(%a6),%d2
	moveq.l #2,%d4
	cmp.l %d2,%d4
	jbeq .L29
	jbcs .L34
	moveq.l #1,%d4
	cmp.l %d2,%d4
	jbeq .L27
	jbra .L25
	.align 	2
.L34:
	moveq.l #4,%d4
	cmp.l %d2,%d4
	jbeq .L30
	moveq.l #8,%d4
	cmp.l %d2,%d4
	jbeq .L31
	jbra .L25
	.align 	2
.L27:
	move.l -24(%a6),%a2
	clr.l %d0
	move.b (%a2),%d0
	jbra .L2
	.align 	2
.L29:
	move.l -24(%a6),%a2
	clr.l %d0
	move.w (%a2),%d0
	jbra .L2
	.align 	2
.L30:
	move.l -24(%a6),%a2
	move.l (%a2),%d0
	jbra .L2
	.align 	2
.L31:
	move.l -24(%a6),%a2
	move.l (%a2),%d0
	move.l 4(%a2),%d1
	jbra .L2
	.align 	2
.L25:
	btst #0,%d3
	jbeq .L2
	move.l -24(%a6),%d0
.L39:
	move.l %d0,%a0
.L2:
	movm.l -52(%a6),#0xc1c
	unlk %a6
	rts
.Lfe1:
	.size	 __vacall_r,.Lfe1-__vacall_r
	.ident	"GCC: (GNU) egcs-2.91.57 19980901 (egcs-1.1 release)"
