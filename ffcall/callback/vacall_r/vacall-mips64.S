#include "asmmips.h"
	.file	1 "vacall-mips64.c"
	.set	nobopt

 

 

 
 

	.text
	.align	2
	.globl	__vacall_r
	DECLARE_FUNCTION(__vacall_r)

	.text
	.ent	__vacall_r
__vacall_r:
	.frame	$sp,224,$31		
	.mask	0x80030000,-8
	.fmask	0x00000000,0
	dsubu	$sp,$sp,224
	sd	$31,216($sp)
	sd	$17,144($sp)
	sd	$16,136($sp)
	dsubu	$sp,$sp,64
 
	
 
	daddu	$17,$sp,224
	daddu	$12,$sp,288
	sd	$4,224($sp)
	sd	$5,232($sp)
	sd	$6,240($sp)
	sd	$7,248($sp)
	sd	$8,256($sp)
	sd	$9,264($sp)
	sd	$10,272($sp)
	sd	$11,280($sp)
	s.d	$f12,136($sp)
	s.d	$f13,144($sp)
	s.d	$f14,152($sp)
	s.d	$f15,160($sp)
	s.d	$f16,168($sp)
	s.d	$f17,176($sp)
	s.d	$f18,184($sp)
	s.d	$f19,192($sp)
	s.s	$f12,100($sp)
	s.s	$f13,104($sp)
	s.s	$f14,108($sp)
	s.s	$f15,112($sp)
	s.s	$f16,116($sp)
	s.s	$f17,120($sp)
	s.s	$f18,124($sp)
	s.s	$f19,128($sp)
	sw	$0,32($sp)
	sd	$17,40($sp)
	sd	$0,48($sp)
	sw	$0,56($sp)
	sd	$12,88($sp)
	sw	$0,96($sp)
	ld	$25,0($2)
	ld	$4,8($2)
	.set	noreorder
	.set	nomacro
	jal	$31,$25
	daddu	$5,$sp,32
	.set	macro
	.set	reorder

	lw	$13,56($sp)
	
	.set	noreorder
	.set	nomacro
	beq	$13,$0,$L3
	li	$12,0x00000001		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L86
	li	$12,0x00000002		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L6
	li	$12,0x00000003		
	.set	macro
	.set	reorder

	lb	$2,72($sp)
	j	$L3
$L6:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L8
	li	$12,0x00000004		
	.set	macro
	.set	reorder

$L86:
	lbu	$2,72($sp)
	j	$L3
$L8:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L10
	li	$12,0x00000005		
	.set	macro
	.set	reorder

	lh	$2,72($sp)
	j	$L3
$L10:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L12
	li	$12,0x00000006		
	.set	macro
	.set	reorder

	lhu	$2,72($sp)
	j	$L3
$L12:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L14
	li	$12,0x00000007		
	.set	macro
	.set	reorder

	lw	$2,72($sp)
	j	$L3
$L14:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L16
	li	$12,0x00000008		
	.set	macro
	.set	reorder

	lwu	$2,72($sp)
	j	$L3
$L16:
	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L87
	li	$12,0x00000009		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L87
	li	$12,0x0000000a		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L87
	li	$12,0x0000000b		
	.set	macro
	.set	reorder

	lw	$13,56($sp)
	
	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L87
	li	$12,0x0000000c		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L26
	li	$12,0x0000000d		
	.set	macro
	.set	reorder

	l.s	$f0,72($sp)
	j	$L3
$L26:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L28
	li	$12,0x0000000e		
	.set	macro
	.set	reorder

	l.d	$f0,72($sp)
	j	$L3
$L28:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L30
	li	$12,0x0000000f		
	.set	macro
	.set	reorder

$L87:
	ld	$2,72($sp)
	j	$L3
$L30:
	bne	$13,$12,$L3
	lw	$13,32($sp)
	
	andi	$12,$13,0x0001
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L33
	andi	$12,$13,0x0400
	.set	macro
	.set	reorder

	ld	$2,48($sp)
	j	$L3
$L33:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L3
	andi	$12,$13,0x0004
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L36
	li	$12,1
	.set	macro
	.set	reorder

	ld	$13,64($sp)
	
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L37
	li	$12,2
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lbu	$2,0($12)
	j	$L3
$L37:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L39
	li	$12,4
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lhu	$2,0($12)
	j	$L3
$L39:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L41
	li	$12,8
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lwu	$2,0($12)
	j	$L3
$L41:
	bne	$13,$12,$L3
	ld	$12,48($sp)
	
	ld	$2,0($12)
	j	$L3
$L36:
	ld	$24,64($sp)
	
	sltu	$13,$0,$24
	dsll	$13,$13,32
	dsra	$13,$13,32
	sltu	$12,$24,17
	dsll	$12,$12,32
	dsra	$12,$12,32
	and	$13,$13,$12
	.set	noreorder
	.set	nomacro
	beq	$13,$0,$L45
	li	$12,1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L46
	li	$12,2
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lbu	$12,0($12)
	.set	noreorder
	.set	nomacro
	j	$L45
	dsll	$2,$12,56
	.set	macro
	.set	reorder

$L46:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L48
	li	$12,3
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lbu	$13,0($12)
	lbu	$12,1($12)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$2,$13,$12
	.set	macro
	.set	reorder

$L48:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L50
	li	$12,4
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lbu	$14,0($12)
	lbu	$13,1($12)
	lbu	$12,2($12)
	dsll	$14,$14,56
	dsll	$13,$13,48
	or	$14,$14,$13
	dsll	$12,$12,40
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$2,$14,$12
	.set	macro
	.set	reorder

$L50:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L52
	li	$12,5
	.set	macro
	.set	reorder

	ld	$15,48($sp)
	
	lbu	$12,0($15)
	lbu	$13,1($15)
	lbu	$14,2($15)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	lbu	$13,3($15)
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$13,$13,32
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$2,$12,$13
	.set	macro
	.set	reorder

$L52:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L54
	li	$12,6
	.set	macro
	.set	reorder

	ld	$15,48($sp)
	
	lbu	$12,0($15)
	lbu	$13,1($15)
	lbu	$14,2($15)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,3($15)
	or	$12,$12,$14
	lbu	$14,4($15)
	dsll	$13,$13,32
	or	$12,$12,$13
	dsll	$14,$14,24
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$2,$12,$14
	.set	macro
	.set	reorder

$L54:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L56
	li	$12,7
	.set	macro
	.set	reorder

	ld	$16,48($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$14,2($16)
	lbu	$15,3($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,4($16)
	or	$12,$12,$15
	lbu	$14,5($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	dsll	$14,$14,16
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$2,$12,$14
	.set	macro
	.set	reorder

$L56:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L58
	dsubu	$12,$24,8
	.set	macro
	.set	reorder

	ld	$15,48($sp)
	
	lbu	$12,0($15)
	lbu	$13,1($15)
	lbu	$14,2($15)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,3($15)
	or	$12,$12,$14
	lbu	$14,4($15)
	dsll	$13,$13,32
	or	$12,$12,$13
	dsll	$14,$14,24
	lbu	$13,5($15)
	or	$12,$12,$14
	lbu	$14,6($15)
	dsll	$13,$13,16
	or	$12,$12,$13
	dsll	$14,$14,8
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$2,$12,$14
	.set	macro
	.set	reorder

$L58:
	sltu	$12,$12,9
	beq	$12,$0,$L45
	ld	$16,48($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$14,2($16)
	lbu	$15,3($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,4($16)
	or	$12,$12,$15
	lbu	$14,5($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	dsll	$14,$14,16
	lbu	$13,6($16)
	or	$12,$12,$14
	lbu	$14,7($16)
	dsll	$13,$13,8
	or	$12,$12,$13
	or	$2,$12,$14
	li	$12,8
	.set	noreorder
	.set	nomacro
	beq	$24,$12,$L45
	li	$12,9
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L63
	li	$12,10
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	.set	noreorder
	.set	nomacro
	j	$L45
	dsll	$3,$12,56
	.set	macro
	.set	reorder

$L63:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L65
	li	$12,11
	.set	macro
	.set	reorder

	lbu	$13,8($16)
	lbu	$12,9($16)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$3,$13,$12
	.set	macro
	.set	reorder

$L65:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L67
	li	$12,12
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L88
	dsll	$14,$14,40
	.set	macro
	.set	reorder

$L67:
	ld	$13,64($sp)
	
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L69
	li	$12,13
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	lbu	$13,11($16)
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$13,$13,32
	.set	noreorder
	.set	nomacro
	j	$L45
	or	$3,$12,$13
	.set	macro
	.set	reorder

$L69:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L71
	li	$12,14
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,11($16)
	or	$12,$12,$14
	lbu	$14,12($16)
	dsll	$13,$13,32
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L88
	dsll	$14,$14,24
	.set	macro
	.set	reorder

$L71:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L73
	li	$12,15
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	lbu	$15,11($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,12($16)
	or	$12,$12,$15
	lbu	$14,13($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L88
	dsll	$14,$14,16
	.set	macro
	.set	reorder

$L73:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L75
	li	$12,16
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,11($16)
	or	$12,$12,$14
	lbu	$14,12($16)
	dsll	$13,$13,32
	or	$12,$12,$13
	dsll	$14,$14,24
	lbu	$13,13($16)
	or	$12,$12,$14
	lbu	$14,14($16)
	dsll	$13,$13,16
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L88
	dsll	$14,$14,8
	.set	macro
	.set	reorder

$L75:
	bne	$13,$12,$L45
	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	lbu	$15,11($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,12($16)
	or	$12,$12,$15
	lbu	$14,13($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	dsll	$14,$14,16
	lbu	$13,14($16)
	or	$12,$12,$14
	lbu	$14,15($16)
	dsll	$13,$13,8
	or	$12,$12,$13
$L88:
	or	$3,$12,$14
$L45:
	lw	$12,32($sp)
	
	andi	$12,$12,0x2000
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L78
	li	$12,4
	.set	macro
	.set	reorder

	ld	$13,64($sp)
	
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L79
	li	$12,8
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	l.s	$f0,0($12)
	j	$L78
$L79:
	bne	$13,$12,$L78
	ld	$12,48($sp)
	
	l.s	$f0,0($12)
	l.s	$f2,4($12)
$L78:
	lw	$12,32($sp)
	
	andi	$12,$12,0x4000
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L3
	li	$12,8
	.set	macro
	.set	reorder

	ld	$13,64($sp)
	
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L83
	li	$12,16
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	l.d	$f0,0($12)
	j	$L3
$L83:
	bne	$13,$12,$L3
	ld	$12,48($sp)
	
	l.d	$f0,0($12)
	l.d	$f2,8($12)
$L3:
 
	
 
	daddu	$sp,$sp,64
	ld	$31,216($sp)
	ld	$17,144($sp)
	ld	$16,136($sp)
	daddu	$sp,$sp,224
	j	$31
	.end	__vacall_r
