#include "asmmips.h"
	
	.set	nobopt
	.option pic2
	.section	.text
	.text
	.align	2
	.globl	vacall
	DECLARE_FUNCTION(vacall)
	.ent	vacall
vacall:
.LFB1:
	.frame	$sp,272,$31		
	.mask	0x900f0000,-8
	.fmask	0x00000000,0
	subu	$sp,$sp,272
.LCFI0:
	sd	$31,200($sp)
.LCFI1:
	sd	$28,192($sp)
.LCFI2:
	sd	$19,184($sp)
.LCFI3:
	sd	$18,176($sp)
.LCFI4:
	sd	$17,168($sp)
.LCFI5:
	sd	$16,160($sp)
.LCFI6:
	.set	noat
	lui	$1,%hi(%neg(%gp_rel(vacall)))
	addiu	$1,$1,%lo(%neg(%gp_rel(vacall)))
	daddu	$gp,$1,$25
	.set	at
	lw	$25,vacall_function
	sd	$4,208($sp)
	addu	$4,$sp,16
	addu	$19,$sp,208
	addu	$12,$sp,272
	sd	$5,216($sp)
	sd	$6,224($sp)
	sd	$7,232($sp)
	sd	$8,240($sp)
	sd	$9,248($sp)
	sd	$10,256($sp)
	sd	$11,264($sp)
	s.d	$f12,96($sp)
	s.d	$f13,104($sp)
	s.d	$f14,112($sp)
	s.d	$f15,120($sp)
	s.d	$f16,128($sp)
	s.d	$f17,136($sp)
	s.d	$f18,144($sp)
	s.d	$f19,152($sp)
	s.s	$f12,64($sp)
	s.s	$f13,68($sp)
	s.s	$f14,72($sp)
	s.s	$f15,76($sp)
	s.s	$f16,80($sp)
	s.s	$f17,84($sp)
	s.s	$f18,88($sp)
	s.s	$f19,92($sp)
	sw	$0,16($sp)
	sw	$19,20($sp)
	sw	$0,24($sp)
	sw	$0,28($sp)
	sw	$12,56($sp)
	sw	$0,60($sp)
	jal	$31,$25
	lw	$13,28($sp)
	
	.set	noreorder
	.set	nomacro
	beq	$13,$0,.L3
	li	$12,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L86
	li	$12,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L6
	li	$12,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L3
	lb	$2,40($sp)
	.set	macro
	.set	reorder

.L6:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L8
	li	$12,4			
	.set	macro
	.set	reorder

.L86:
	.set	noreorder
	.set	nomacro
	b	.L3
	lbu	$2,40($sp)
	.set	macro
	.set	reorder

.L8:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L10
	li	$12,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L3
	lh	$2,40($sp)
	.set	macro
	.set	reorder

.L10:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L12
	li	$12,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L3
	lhu	$2,40($sp)
	.set	macro
	.set	reorder

.L12:
	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L87
	li	$12,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L88
	li	$12,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L87
	li	$12,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L20
	li	$12,10			
	.set	macro
	.set	reorder

.L88:
	.set	noreorder
	.set	nomacro
	b	.L3
	lwu	$2,40($sp)
	.set	macro
	.set	reorder

.L20:
	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L89
	move	$14,$13
	.set	macro
	.set	reorder

	li	$12,11			
	.set	noreorder
	.set	nomacro
	bne	$14,$12,.L24
	li	$12,12			
	.set	macro
	.set	reorder

.L89:
	.set	noreorder
	.set	nomacro
	b	.L3
	ld	$2,40($sp)
	.set	macro
	.set	reorder

.L24:
	.set	noreorder
	.set	nomacro
	bne	$14,$12,.L26
	li	$12,13			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L3
	l.s	$f0,40($sp)
	.set	macro
	.set	reorder

.L26:
	.set	noreorder
	.set	nomacro
	bne	$14,$12,.L28
	li	$12,14			
	.set	macro
	.set	reorder

	l.d	$f0,40($sp)
	b	.L3
.L28:
	.set	noreorder
	.set	nomacro
	bne	$14,$12,.L30
	li	$12,15			
	.set	macro
	.set	reorder

.L87:
	.set	noreorder
	.set	nomacro
	b	.L3
	lw	$2,40($sp)
	.set	macro
	.set	reorder

.L30:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L3
	lw	$18,16($sp)
	.set	macro
	.set	reorder

	
	andi	$12,$18,0x1
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L33
	andi	$12,$18,0x400
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L3
	lw	$2,24($sp)
	.set	macro
	.set	reorder

.L33:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L3
	andi	$12,$18,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L36
	lw	$13,32($sp)
	.set	macro
	.set	reorder

	li	$12,1			
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L37
	li	$12,2			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L3
	lbu	$2,0($12)
	.set	macro
	.set	reorder

.L37:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L39
	li	$12,4			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L3
	lhu	$2,0($12)
	.set	macro
	.set	reorder

.L39:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L41
	li	$12,8			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L3
	lwu	$2,0($12)
	.set	macro
	.set	reorder

.L41:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L3
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L3
	ld	$2,0($12)
	.set	macro
	.set	reorder

.L36:
	lw	$24,32($sp)
	
	addu	$12,$24,-1
	sltu	$12,$12,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L45
	li	$12,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L46
	li	$12,2			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	lbu	$13,0($12)
	.set	noreorder
	.set	nomacro
	b	.L45
	dsll	$2,$13,56
	.set	macro
	.set	reorder

.L46:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L48
	li	$12,3			
	.set	macro
	.set	reorder

	lw	$14,24($sp)
	
	lbu	$13,0($14)
	lbu	$12,1($14)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$2,$13,$12
	.set	macro
	.set	reorder

.L48:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L50
	li	$12,4			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	lbu	$15,0($12)
	lbu	$13,1($12)
	lbu	$14,2($12)
	dsll	$15,$15,56
	dsll	$13,$13,48
	or	$15,$15,$13
	dsll	$14,$14,40
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$2,$15,$14
	.set	macro
	.set	reorder

.L50:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L52
	li	$12,5			
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
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
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$2,$12,$15
	.set	macro
	.set	reorder

.L52:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L54
	li	$12,6			
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$15,2($16)
	lbu	$14,3($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	or	$12,$12,$15
	lbu	$13,4($16)
	dsll	$14,$14,32
	or	$12,$12,$14
	dsll	$13,$13,24
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$2,$12,$13
	.set	macro
	.set	reorder

.L54:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L56
	li	$12,7			
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
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
	b	.L45
	or	$2,$12,$14
	.set	macro
	.set	reorder

.L56:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L58
	addu	$12,$24,-8
	.set	macro
	.set	reorder

	lw	$17,24($sp)
	
	lbu	$12,0($17)
	lbu	$13,1($17)
	lbu	$15,2($17)
	lbu	$14,3($17)
	lbu	$16,4($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	or	$12,$12,$15
	dsll	$14,$14,32
	or	$12,$12,$14
	dsll	$16,$16,24
	lbu	$13,5($17)
	or	$12,$12,$16
	lbu	$14,6($17)
	dsll	$13,$13,16
	or	$12,$12,$13
	dsll	$14,$14,8
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$2,$12,$14
	.set	macro
	.set	reorder

.L58:
	sltu	$12,$12,9
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L45
	lw	$17,24($sp)
	.set	macro
	.set	reorder

	
	lbu	$12,0($17)
	lbu	$13,1($17)
	lbu	$14,2($17)
	lbu	$15,3($17)
	lbu	$16,5($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,4($17)
	or	$12,$12,$15
	dsll	$16,$16,16
	lbu	$14,6($17)
	lbu	$15,7($17)
	dsll	$13,$13,24
	or	$12,$12,$13
	or	$12,$12,$16
	dsll	$14,$14,8
	or	$12,$12,$14
	li	$13,8			
	.set	noreorder
	.set	nomacro
	beq	$24,$13,.L45
	or	$2,$12,$15
	.set	macro
	.set	reorder

	li	$12,9			
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L63
	li	$12,10			
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	.set	noreorder
	.set	nomacro
	b	.L45
	dsll	$3,$12,56
	.set	macro
	.set	reorder

.L63:
	.set	noreorder
	.set	nomacro
	bnel	$24,$12,.L65
	li	$12,11			
	.set	macro
	.set	reorder

	lbu	$13,8($17)
	lbu	$12,9($17)
	dsll	$13,$13,56
	.set	noreorder
	.set	nomacro
	b	.L90
	dsll	$12,$12,48
	.set	macro
	.set	reorder

.L65:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L67
	li	$12,12			
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	lbu	$13,9($17)
	lbu	$14,10($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L67:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L69
	li	$12,13			
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	lbu	$13,9($17)
	lbu	$14,10($17)
	lbu	$15,11($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$3,$12,$15
	.set	macro
	.set	reorder

.L69:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L71
	li	$12,14			
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	lbu	$13,9($17)
	lbu	$15,10($17)
	lbu	$14,11($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	or	$12,$12,$15
	lbu	$13,12($17)
	dsll	$14,$14,32
	or	$12,$12,$14
	.set	noreorder
	.set	nomacro
	b	.L90
	dsll	$13,$13,24
	.set	macro
	.set	reorder

.L71:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L73
	li	$12,15			
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	lbu	$13,9($17)
	lbu	$14,10($17)
	lbu	$15,11($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,12($17)
	or	$12,$12,$15
	lbu	$14,13($17)
	dsll	$13,$13,24
	or	$12,$12,$13
	dsll	$14,$14,16
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L73:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L75
	li	$12,16			
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	lbu	$13,9($17)
	lbu	$15,10($17)
	lbu	$14,11($17)
	lbu	$16,12($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	or	$12,$12,$15
	dsll	$14,$14,32
	or	$12,$12,$14
	dsll	$16,$16,24
	lbu	$13,13($17)
	or	$12,$12,$16
	lbu	$14,14($17)
	dsll	$13,$13,16
	or	$12,$12,$13
	dsll	$14,$14,8
	.set	noreorder
	.set	nomacro
	b	.L45
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L75:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L91
	andi	$12,$18,0x2000
	.set	macro
	.set	reorder

	lbu	$12,8($17)
	lbu	$13,9($17)
	lbu	$14,10($17)
	lbu	$15,11($17)
	lbu	$16,13($17)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,12($17)
	or	$12,$12,$15
	dsll	$16,$16,16
	lbu	$14,14($17)
	dsll	$13,$13,24
	or	$12,$12,$13
	or	$12,$12,$16
	lbu	$13,15($17)
	dsll	$14,$14,8
	or	$12,$12,$14
.L90:
	or	$3,$12,$13
.L45:
	andi	$12,$18,0x2000
.L91:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L78
	li	$12,4			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L79
	li	$12,8			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L78
	l.s	$f0,0($12)
	.set	macro
	.set	reorder

.L79:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L92
	andi	$12,$18,0x4000
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	l.s	$f0,0($12)
	l.s	$f2,4($12)
.L78:
	andi	$12,$18,0x4000
.L92:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L3
	li	$12,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L83
	li	$12,16			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	l.d	$f0,0($12)
	b	.L3
.L83:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L3
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	
	l.d	$f0,0($12)
	l.d	$f2,8($12)
.L3:
	ld	$31,200($sp)
	ld	$28,192($sp)
	ld	$19,184($sp)
	ld	$18,176($sp)
	ld	$17,168($sp)
	ld	$16,160($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,272
	.set	macro
	.set	reorder

.LFE1:
	.end	vacall
