#include "asmmips.h"
	.file	1 "avcall-mips.c"
	.set	nobopt

 

 

 
 

	.text
	.align	2
	.globl	__builtin_avcall
	DECLARE_FUNCTION(__builtin_avcall)

	.text
	.ent	__builtin_avcall
__builtin_avcall:
	.frame	$fp,32,$31		
	.mask	0xc0010000,-8
	.fmask	0x00000000,0
	subu	$sp,$sp,32
	sw	$fp,20($sp)
	move	$fp,$sp
	sw	$16,16($sp)
	move	$16,$4
	sw	$31,24($sp)
	addu	$sp,$sp,-1032
	move	$4,$sp
	.set	volatile
	lw	$2,0($sp)
	.set	novolatile
	
	lw	$2,20($16)
	lw	$3,4($16)
	addu	$2,$2,-48
	subu	$2,$2,$16
	sra	$5,$2,2
	andi	$2,$3,0x0400
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L2
	li	$6,0x00000004		
	.set	macro
	.set	reorder

 
	l.d $f12,32($16)
 
	andi	$2,$3,0x0800
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L63
	slt	$2,$6,$5
	.set	macro
	.set	reorder

 
	l.d $f14,40($16)
 
$L2:
	slt	$2,$6,$5
$L63:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L5
	addu	$4,$4,16
	.set	macro
	.set	reorder

	addu	$3,$16,16
$L7:
	lw	$2,48($3)
	addu	$3,$3,4
	addu	$6,$6,1
	sw	$2,0($4)
	slt	$2,$6,$5
	.set	noreorder
	.set	nomacro
	bne	$2,$0,$L7
	addu	$4,$4,4
	.set	macro
	.set	reorder

$L5:
	lw	$25,0($16)
	lw	$4,48($16)
	lw	$5,52($16)
	lw	$6,56($16)
	lw	$7,60($16)
	jal	$31,$25
	move	$6,$2
	lw	$4,12($16)
	li	$2,0x00000001		
	beq	$4,$2,$L10
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L60
	li	$2,0x00000002		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,0x00000003		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,0x00000004		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,0x00000005		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L62
	li	$2,0x00000006		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L62
	li	$2,0x00000007		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L60
	li	$2,0x00000008		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L60
	li	$2,0x00000009		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L60
	li	$2,0x0000000a		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L60
	addu	$2,$4,-11
	.set	macro
	.set	reorder

	sltu	$2,$2,2
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L31
	li	$2,0x0000000d		
	.set	macro
	.set	reorder

	lw	$2,8($16)
	
	sw	$6,0($2)
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L10
	sw	$3,4($2)
	.set	macro
	.set	reorder

$L31:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L33
	li	$2,0x0000000e		
	.set	macro
	.set	reorder

	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L10
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

$L33:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L35
	li	$2,0x0000000f		
	.set	macro
	.set	reorder

	lw	$2,8($16)
	
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	j	$L64
	move	$2,$0
	.set	macro
	.set	reorder

$L35:
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L60
	li	$2,0x00000010		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L64
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$3,4($16)
	
	andi	$2,$3,0x0001
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L40
	li	$2,0x00000001		
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L41
	li	$2,0x00000002		
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lbu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L10
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L41:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L43
	li	$2,0x00000004		
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lhu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L10
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L43:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L45
	li	$2,0x00000008		
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lw	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L10
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L45:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L47
	addu	$2,$3,3
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lw	$2,0($6)
	
	sw	$2,0($3)
	lw	$3,8($16)
	lw	$2,4($6)
	.set	noreorder
	.set	nomacro
	j	$L10
	sw	$2,4($3)
	.set	macro
	.set	reorder

$L47:
	srl	$5,$2,2
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,$L10
	sll	$2,$5,2
	.set	macro
	.set	reorder

	addu	$6,$2,$6
$L51:
	lw	$2,0($6)
	addu	$6,$6,-4
	sll	$3,$5,2
	lw	$4,8($16)
	addu	$5,$5,-1
	addu	$3,$3,$4
	.set	noreorder
	.set	nomacro
	bgez	$5,$L51
	sw	$2,0($3)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L64
	move	$2,$0
	.set	macro
	.set	reorder

$L40:
	andi	$2,$3,0x0002
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L10
	li	$2,0x00000001		
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L55
	li	$2,0x00000002		
	.set	macro
	.set	reorder

$L61:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L10
	sb	$6,0($2)
	.set	macro
	.set	reorder

$L55:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L57
	li	$2,0x00000004		
	.set	macro
	.set	reorder

$L62:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L10
	sh	$6,0($2)
	.set	macro
	.set	reorder

$L57:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L64
	move	$2,$0
	.set	macro
	.set	reorder

$L60:
	lw	$2,8($16)
	
	sw	$6,0($2)
$L10:
	move	$2,$0
$L64:
	move	$sp,$fp			
	lw	$31,24($sp)
	lw	$fp,20($sp)
	lw	$16,16($sp)
	addu	$sp,$sp,32
	j	$31
	.end	__builtin_avcall
