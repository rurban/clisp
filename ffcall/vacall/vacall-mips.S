#include "asmmips.h"
	.file	1 "vacall-mips.c"
	.set	nobopt

 

 

 
 

	.text
	.align	2
	.globl	vacall
	DECLARE_FUNCTION(vacall)

	.extern	vacall_function, 4

	.text
	.ent	vacall
vacall:
	.frame	$fp,88,$31		
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	subu	$sp,$sp,88
	sw	$fp,80($sp)
	move	$fp,$sp
	addu	$8,$fp,88
	sw	$31,84($sp)
	sw	$4,88($fp)
	sw	$5,92($fp)
	sw	$6,96($fp)
	sw	$7,100($fp)
	lw	$25,vacall_function
	addu	$4,$fp,104
	sw	$4,48($fp)
	addu	$4,$fp,16
	s.d	$f12,64($fp)
	s.d	$f14,72($fp)
	s.s	$f12,56($fp)
	s.s	$f14,60($fp)
	sw	$0,16($fp)
	sw	$8,20($fp)
	sw	$0,24($fp)
	sw	$0,28($fp)
	.set	noreorder
	.set	nomacro
	jal	$31,$25
	sw	$0,52($fp)
	.set	macro
	.set	reorder

	lw	$5,28($fp)
	
	.set	noreorder
	.set	nomacro
	beq	$5,$0,$L3
	li	$4,0x00000001		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L39
	li	$4,0x00000002		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L6
	li	$4,0x00000003		
	.set	macro
	.set	reorder

	lb	$2,40($fp)
	j	$L3
$L6:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L8
	li	$4,0x00000004		
	.set	macro
	.set	reorder

$L39:
	lbu	$2,40($fp)
	j	$L3
$L8:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L10
	li	$4,0x00000005		
	.set	macro
	.set	reorder

	lh	$2,40($fp)
	j	$L3
$L10:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L12
	li	$4,0x00000006		
	.set	macro
	.set	reorder

	lhu	$2,40($fp)
	j	$L3
$L12:
	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,0x00000007		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,0x00000008		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,0x00000009		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	addu	$4,$5,-10
	.set	macro
	.set	reorder

	sltu	$4,$4,2
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L22
	li	$4,0x0000000c		
	.set	macro
	.set	reorder

	lw	$2,40($fp)
	lw	$3,44($fp)
	j	$L3
$L22:
	lw	$5,28($fp)
	
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L24
	li	$4,0x0000000d		
	.set	macro
	.set	reorder

	l.s	$f0,40($fp)
	j	$L3
$L24:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L26
	li	$4,0x0000000e		
	.set	macro
	.set	reorder

	l.d	$f0,40($fp)
	j	$L3
$L26:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L28
	li	$4,0x0000000f		
	.set	macro
	.set	reorder

$L40:
	lw	$2,40($fp)
	j	$L3
$L28:
	bne	$5,$4,$L3
	lw	$5,16($fp)
	
	andi	$4,$5,0x0001
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L31
	andi	$4,$5,0x0002
	.set	macro
	.set	reorder

	lw	$2,24($fp)
	j	$L3
$L31:
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L3
	li	$4,0x00000001		
	.set	macro
	.set	reorder

	lw	$5,32($fp)
	
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L34
	li	$4,0x00000002		
	.set	macro
	.set	reorder

	lw	$4,24($fp)
	
	lbu	$2,0($4)
	j	$L3
$L34:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L36
	li	$4,0x00000004		
	.set	macro
	.set	reorder

	lw	$4,24($fp)
	
	lhu	$2,0($4)
	j	$L3
$L36:
	bne	$5,$4,$L3
	lw	$4,24($fp)
	
	lw	$2,0($4)
$L3:
	move	$sp,$fp			
	lw	$31,84($sp)
	lw	$fp,80($sp)
	addu	$sp,$sp,88
	j	$31
	.end	vacall
