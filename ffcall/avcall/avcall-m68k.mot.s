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
	move.l 12(%a2),%d2
	moveq.l #16,%d3
	cmp.l %d2,%d3
	jbcs .L35
	.set .LI57,.+2
	move.w .L57-.LI57.b(%pc,%d2.l*2),%d2
	jmp %pc@(2,%d2:w)
	.align 	2
.L57:
	.word .L37-.L57
	.word .L35-.L57
	.word .L34-.L57
	.word .L34-.L57
	.word .L34-.L57
	.word .L36-.L57
	.word .L36-.L57
	.word .L37-.L57
	.word .L37-.L57
	.word .L37-.L57
	.word .L37-.L57
	.word .L38-.L57
	.word .L38-.L57
	.word .L22-.L57
	.word .L27-.L57
	.word .L37-.L57
	.word .L31-.L57
	.align 	2
.L22:
	move.l 4(%a2),%d2
	btst #6,%d2
	jbeq .L23
	move.l 8(%a2),%a0
	fmove.s %fp0,(%a0)
	jbra .L35
	.align 	2
.L23:
	btst #5,%d2
	jbeq .L25
	move.l 8(%a2),%a0
	move.l %d1,-(%sp)
	move.l %d0,-(%sp)
	fmove.d (%sp)+,%fp1
	fmove.s %fp1,(%a0)
	jbra .L35
	.align 	2
.L25:
	move.l 8(%a2),%a0
	move.l %d0,(%a0)
	jbra .L35
	.align 	2
.L27:
	btst #6,7(%a2)
	jbeq .L28
	move.l 8(%a2),%a0
	fmove.d %fp0,(%a0)
	jbra .L35
	.align 	2
.L28:
	move.l 8(%a2),%a0
	move.l %d0,(%a0)
	move.l %d1,4(%a0)
	jbra .L35
	.align 	2
.L31:
	move.l 4(%a2),%d2
	btst #9,%d2
	jbeq .L32
	move.l 16(%a2),%d0
	moveq.l #2,%d3
	cmp.l %d0,%d3
	jbeq .L36
	jbcs .L41
	moveq.l #1,%d3
	cmp.l %d0,%d3
	jbeq .L34
	jbra .L32
	.align 	2
.L41:
	moveq.l #4,%d3
	cmp.l %d0,%d3
	jbeq .L37
	moveq.l #8,%d3
	cmp.l %d0,%d3
	jbeq .L38
	jbra .L32
	.align 	2
.L34:
	move.l 8(%a2),%a0
	move.w %a1,%d3
	move.b %d3,(%a0)
	jbra .L35
	.align 	2
.L36:
	move.l 8(%a2),%a0
	move.w %a1,(%a0)
	jbra .L35
	.align 	2
.L37:
	move.l 8(%a2),%a0
	move.l %a1,(%a0)
	jbra .L35
	.align 	2
.L38:
	move.l 8(%a2),%a0
	move.l %a1,(%a0)
	move.l 8(%a2),%a0
	move.l %d1,4(%a0)
	jbra .L35
	.align 	2
.L32:
	btst #0,%d2
	jbeq .L35
	move.l 16(%a2),%d0
	moveq.l #2,%d3
	cmp.l %d0,%d3
	jbeq .L45
	jbcs .L54
	moveq.l #1,%d3
	cmp.l %d0,%d3
	jbeq .L44
	jbra .L48
	.align 	2
.L54:
	moveq.l #4,%d3
	cmp.l %d0,%d3
	jbeq .L46
	moveq.l #8,%d3
	cmp.l %d0,%d3
	jbeq .L47
	jbra .L48
	.align 	2
.L44:
	move.l 8(%a2),%a0
	move.b (%a1),(%a0)
	jbra .L35
	.align 	2
.L45:
	move.l 8(%a2),%a0
	move.w (%a1),(%a0)
	jbra .L35
	.align 	2
.L46:
	move.l 8(%a2),%a0
	move.l (%a1),(%a0)
	jbra .L35
	.align 	2
.L47:
	move.l 8(%a2),%a0
	move.l (%a1),(%a0)
	move.l 8(%a2),%a0
	move.l 4(%a1),4(%a0)
	jbra .L35
	.align 	2
.L48:
	addq.l #3,%d0
	lsr.l #2,%d0
	subq.l #1,%d0
	jbmi .L35
	lea (%a1,%d0.l*4),%a1
	.align 	2
.L51:
	move.l 8(%a2),%a0
	move.l (%a1),(%a0,%d0.l*4)
	subq.l #4,%a1
	dbra %d0,.L51
	clr.w %d0
	subq.l #1,%d0
	jbcc .L51
.L35:
	lea (1024,%sp),%sp
	clr.l %d0
	movm.l (%sp)+,#0xc0c
	rts
.Lfe1:
	.size	 __builtin_avcall,.Lfe1-__builtin_avcall
	.ident	"GCC: (GNU) egcs-2.91.57 19980901 (egcs-1.1 release)"
