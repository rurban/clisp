	.file	"avcall-m68k.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 	2
.globl __builtin_avcall
	.type	 __builtin_avcall,@function
__builtin_avcall:
	movm.l #0x3030,-(%sp)
	move.l 20(%sp),%a2
	lea (-1024,%sp),%sp
	moveq.l #-32,%d0
	add.l 20(%a2),%d0
	sub.l %a2,%d0
	asr.l #2,%d0
	sub.l %a1,%a1
	cmp.l %a1,%d0
	jble .L3
	move.l %sp,%a3
	lea (32,%a2),%a0
	.align 	2
.L5:
	move.l (%a0)+,(%a3)+
	addq.l #1,%a1
	cmp.l %a1,%d0
	jbgt .L5
.L3:
	moveq.l #16,%d3
	cmp.l 12(%a2),%d3
	jbne .L7
#APP
	move.l 8(%a2),%a1
#NO_APP
.L7:
	move.l (%a2),%a0
	jbsr (%a0)
	move.l %d0,%a1
	move.l 12(%a2),%a0
	move.l %a0,%d2
	moveq.l #1,%d3
	cmp.l %a0,%d3
	jbeq .L9
	tst.l %a0
	jbeq .L68
	moveq.l #2,%d3
	cmp.l %a0,%d3
	jbeq .L69
	moveq.l #3,%d3
	cmp.l %a0,%d3
	jbeq .L69
	moveq.l #4,%d3
	cmp.l %a0,%d3
	jbeq .L69
	moveq.l #5,%d3
	cmp.l %a0,%d3
	jbeq .L70
	moveq.l #6,%d3
	cmp.l %a0,%d3
	jbeq .L70
	moveq.l #7,%d3
	cmp.l %a0,%d3
	jbeq .L68
	moveq.l #8,%d3
	cmp.l %a0,%d3
	jbeq .L68
	moveq.l #9,%d3
	cmp.l %a0,%d3
	jbeq .L68
	moveq.l #10,%d3
	cmp.l %a0,%d3
	jbeq .L68
	moveq.l #-11,%d3
	add.l %d3,%d2
	moveq.l #1,%d3
	cmp.l %d2,%d3
	jbcc .L71
	moveq.l #13,%d3
	cmp.l %a0,%d3
	jbne .L32
	move.l 4(%a2),%d2
	btst #6,%d2
	jbeq .L33
	move.l 8(%a2),%a0
	fmove.s %fp0,(%a0)
	jbra .L9
	.align 	2
.L33:
	btst #5,%d2
	jbeq .L35
	move.l 8(%a2),%a0
	move.l %d1,-(%sp)
	move.l %d0,-(%sp)
	fmove.d (%sp)+,%fp1
	fmove.s %fp1,(%a0)
	jbra .L9
	.align 	2
.L35:
	move.l 8(%a2),%a0
	move.l %d0,(%a0)
	jbra .L9
	.align 	2
.L32:
	moveq.l #14,%d3
	cmp.l %a0,%d3
	jbne .L38
	btst #6,7(%a2)
	jbeq .L39
	move.l 8(%a2),%a0
	fmove.d %fp0,(%a0)
	jbra .L9
	.align 	2
.L39:
	move.l 8(%a2),%a0
	move.l %d0,(%a0)
	move.l %d1,4(%a0)
	jbra .L9
	.align 	2
.L38:
	moveq.l #15,%d3
	cmp.l %a0,%d3
	jbeq .L68
	moveq.l #16,%d3
	cmp.l %a0,%d3
	jbne .L9
	move.l 4(%a2),%d2
	btst #9,%d2
	jbeq .L45
	move.l 16(%a2),%d0
	moveq.l #1,%d3
	cmp.l %d0,%d3
	jbne .L46
.L69:
	move.l 8(%a2),%a0
	move.w %a1,%d3
	move.b %d3,(%a0)
	jbra .L9
	.align 	2
.L46:
	moveq.l #2,%d3
	cmp.l %d0,%d3
	jbne .L49
.L70:
	move.l 8(%a2),%a0
	move.w %a1,(%a0)
	jbra .L9
	.align 	2
.L49:
	moveq.l #4,%d3
	cmp.l %d0,%d3
	jbne .L51
.L68:
	move.l 8(%a2),%a0
	move.l %a1,(%a0)
	jbra .L9
	.align 	2
.L51:
	moveq.l #8,%d3
	cmp.l %d0,%d3
	jbne .L45
.L71:
	move.l 8(%a2),%a0
	move.l %a1,(%a0)
	move.l 8(%a2),%a0
	move.l %d1,4(%a0)
	jbra .L9
	.align 	2
.L45:
	btst #0,%d2
	jbeq .L9
	move.l 16(%a2),%d0
	moveq.l #1,%d3
	cmp.l %d0,%d3
	jbne .L55
	move.l 8(%a2),%a0
	move.b (%a1),(%a0)
	jbra .L9
	.align 	2
.L55:
	moveq.l #2,%d3
	cmp.l %d0,%d3
	jbne .L57
	move.l 8(%a2),%a0
	move.w (%a1),(%a0)
	jbra .L9
	.align 	2
.L57:
	moveq.l #4,%d3
	cmp.l %d0,%d3
	jbne .L59
	move.l 8(%a2),%a0
	move.l (%a1),(%a0)
	jbra .L9
	.align 	2
.L59:
	moveq.l #8,%d3
	cmp.l %d0,%d3
	jbne .L61
	move.l 8(%a2),%a0
	move.l (%a1),(%a0)
	move.l 8(%a2),%a0
	move.l 4(%a1),4(%a0)
	jbra .L9
	.align 	2
.L61:
	addq.l #3,%d0
	lsr.l #2,%d0
	subq.l #1,%d0
	jbmi .L9
	lea (%a1,%d0.l*4),%a1
	.align 	2
.L65:
	move.l 8(%a2),%a0
	move.l (%a1),(%a0,%d0.l*4)
	subq.l #4,%a1
	dbra %d0,.L65
	clr.w %d0
	subq.l #1,%d0
	jbcc .L65
.L9:
	lea (1024,%sp),%sp
	clr.l %d0
	movm.l (%sp)+,#0xc0c
	rts
.Lfe1:
	.size	 __builtin_avcall,.Lfe1-__builtin_avcall
	.ident	"GCC: (GNU) egcs-2.91.57 19980901 (egcs-1.1 release)"
