	#.file	1 "vacall-mipsn32.c"
	.set	nobopt
	.option pic2
	.section	.text
	.text
	.align	2
	.globl	__vacall_r
	.ent	__vacall_r
__vacall_r:
.LFB1:
	.frame	$sp,272,$31		# vars= 144, regs= 5/0, args= 0, extra= 16
	.mask	0x90070000,-16
	.fmask	0x00000000,0
	subu	$sp,$sp,272
.LCFI0:
	sd	$31,192($sp)
.LCFI1:
	sd	$28,184($sp)
.LCFI2:
	sd	$18,176($sp)
.LCFI3:
	sd	$17,168($sp)
.LCFI4:
	sd	$16,160($sp)
.LCFI5:
	.set	noat
	lui	$1,%hi(%neg(%gp_rel(__vacall_r)))
	addiu	$1,$1,%lo(%neg(%gp_rel(__vacall_r)))
	daddu	$gp,$1,$25
	.set	at
	addu	$18,$sp,208
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
	sw	$18,20($sp)
	sw	$0,24($sp)
	sw	$0,28($sp)
	sw	$12,56($sp)
	sw	$0,60($sp)
	lw	$25,0($2)
	sd	$4,208($sp)
	lw	$4,4($2)
	addu	$5,$sp,16
	jal	$31,$25
	lw	$13,28($sp)
	#nop
	sltu	$12,$13,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L2
	sll	$13,$13,2
	.set	macro
	.set	reorder

	la	$12,.L70
	addu	$13,$13,$12
	lw	$14,0($13)
	#nop
	j	$14
.section	.rodata
	.align	3
.L70:
	.word	.L2
	.word	.L6
	.word	.L5
	.word	.L6
	.word	.L7
	.word	.L8
	.word	.L17
	.word	.L12
	.word	.L17
	.word	.L12
	.word	.L13
	.word	.L14
	.word	.L15
	.word	.L16
	.word	.L17
	.word	.L18
	.text
.L5:
	.set	noreorder
	.set	nomacro
	b	.L2
	lb	$2,40($sp)
	.set	macro
	.set	reorder

.L6:
	.set	noreorder
	.set	nomacro
	b	.L2
	lbu	$2,40($sp)
	.set	macro
	.set	reorder

.L7:
	.set	noreorder
	.set	nomacro
	b	.L2
	lh	$2,40($sp)
	.set	macro
	.set	reorder

.L8:
	.set	noreorder
	.set	nomacro
	b	.L2
	lhu	$2,40($sp)
	.set	macro
	.set	reorder

.L12:
	.set	noreorder
	.set	nomacro
	b	.L2
	lwu	$2,40($sp)
	.set	macro
	.set	reorder

.L13:
.L14:
	.set	noreorder
	.set	nomacro
	b	.L2
	ld	$2,40($sp)
	.set	macro
	.set	reorder

.L15:
	.set	noreorder
	.set	nomacro
	b	.L2
	l.s	$f0,40($sp)
	.set	macro
	.set	reorder

.L16:
	#nop
	l.d	$f0,40($sp)
	b	.L2
.L17:
	.set	noreorder
	.set	nomacro
	b	.L2
	lw	$2,40($sp)
	.set	macro
	.set	reorder

.L18:
	lw	$13,16($sp)
	#nop
	andi	$12,$13,0x1
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L19
	andi	$12,$13,0x200
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L2
	lw	$2,24($sp)
	.set	macro
	.set	reorder

.L19:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L2
	andi	$12,$13,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L22
	lw	$24,32($sp)
	.set	macro
	.set	reorder

	li	$12,2			# 0x2
	.set	noreorder
	.set	nomacro
	beq	$24,$12,.L25
	sltu	$12,$24,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L30
	li	$12,1			# 0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$24,$12,.L24
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	b	.L2
.L30:
	li	$12,4			# 0x4
	.set	noreorder
	.set	nomacro
	beq	$24,$12,.L26
	li	$12,8			# 0x8
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$24,$12,.L27
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	b	.L2
.L24:
	.set	noreorder
	.set	nomacro
	b	.L2
	lbu	$2,0($12)
	.set	macro
	.set	reorder

.L25:
	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L2
	lhu	$2,0($12)
	.set	macro
	.set	reorder

.L26:
	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L2
	lwu	$2,0($12)
	.set	macro
	.set	reorder

.L27:
	.set	noreorder
	.set	nomacro
	b	.L2
	ld	$2,0($12)
	.set	macro
	.set	reorder

.L22:
	addu	$13,$24,-1
	sltu	$12,$13,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L32
	sll	$13,$13,2
	.set	macro
	.set	reorder

	la	$12,.L50
	addu	$13,$13,$12
	lw	$14,0($13)
	#nop
	j	$14
.section	.rodata
	.align	3
.L50:
	.word	.L33
	.word	.L34
	.word	.L35
	.word	.L36
	.word	.L37
	.word	.L38
	.word	.L39
	.word	.L48
	.word	.L48
	.word	.L48
	.word	.L48
	.word	.L48
	.word	.L48
	.word	.L48
	.word	.L48
	.word	.L48
	.text
.L33:
	lw	$12,24($sp)
	#nop
	lbu	$13,0($12)
	lw	$24,32($sp)
	.set	noreorder
	.set	nomacro
	b	.L32
	dsll	$2,$13,56
	.set	macro
	.set	reorder

.L34:
	lw	$14,24($sp)
	#nop
	lbu	$13,0($14)
	lbu	$12,1($14)
	lw	$24,32($sp)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	b	.L32
	or	$2,$13,$12
	.set	macro
	.set	reorder

.L35:
	lw	$14,24($sp)
	lw	$24,32($sp)
	lbu	$15,0($14)
	lbu	$12,1($14)
	lbu	$13,2($14)
	dsll	$15,$15,56
	dsll	$12,$12,48
	or	$15,$15,$12
	dsll	$13,$13,40
	.set	noreorder
	.set	nomacro
	b	.L32
	or	$2,$15,$13
	.set	macro
	.set	reorder

.L36:
	lw	$16,24($sp)
	lw	$24,32($sp)
	lbu	$12,0($16)
	lbu	$15,1($16)
	lbu	$13,2($16)
	lbu	$14,3($16)
	dsll	$12,$12,56
	dsll	$15,$15,48
	or	$12,$12,$15
	dsll	$13,$13,40
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	b	.L72
	dsll	$14,$14,32
	.set	macro
	.set	reorder

.L37:
	lw	$16,24($sp)
	lw	$24,32($sp)
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
	b	.L32
	or	$2,$12,$13
	.set	macro
	.set	reorder

.L38:
	lw	$16,24($sp)
	lw	$24,32($sp)
	lbu	$12,0($16)
	lbu	$14,1($16)
	lbu	$13,2($16)
	lbu	$15,3($16)
	dsll	$12,$12,56
	dsll	$14,$14,48
	or	$12,$12,$14
	dsll	$13,$13,40
	or	$12,$12,$13
	dsll	$15,$15,32
	lbu	$14,4($16)
	or	$12,$12,$15
	lbu	$13,5($16)
	dsll	$14,$14,24
	or	$12,$12,$14
	dsll	$13,$13,16
	.set	noreorder
	.set	nomacro
	b	.L32
	or	$2,$12,$13
	.set	macro
	.set	reorder

.L39:
	lw	$17,24($sp)
	lw	$24,32($sp)
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
	.set	noreorder
	.set	nomacro
	b	.L72
	dsll	$14,$14,8
	.set	macro
	.set	reorder

.L48:
	lw	$17,24($sp)
	lw	$24,32($sp)
	lbu	$12,0($17)
	lbu	$14,1($17)
	lbu	$13,2($17)
	lbu	$15,3($17)
	lbu	$16,5($17)
	dsll	$12,$12,56
	dsll	$14,$14,48
	or	$12,$12,$14
	dsll	$13,$13,40
	or	$12,$12,$13
	dsll	$15,$15,32
	lbu	$14,4($17)
	or	$12,$12,$15
	dsll	$16,$16,16
	lbu	$13,6($17)
	dsll	$14,$14,24
	or	$12,$12,$14
	or	$12,$12,$16
	lbu	$14,7($17)
	dsll	$13,$13,8
	or	$12,$12,$13
.L72:
	or	$2,$12,$14
.L32:
	addu	$13,$24,-9
	sltu	$12,$13,8
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L51
	sll	$13,$13,2
	.set	macro
	.set	reorder

	la	$12,.L61
	addu	$13,$13,$12
	lw	$14,0($13)
	#nop
	j	$14
.section	.rodata
	.align	3
.L61:
	.word	.L52
	.word	.L53
	.word	.L54
	.word	.L55
	.word	.L56
	.word	.L57
	.word	.L58
	.word	.L59
	.text
.L52:
	lw	$12,24($sp)
	#nop
	lbu	$13,8($12)
	.set	noreorder
	.set	nomacro
	b	.L51
	dsll	$3,$13,56
	.set	macro
	.set	reorder

.L53:
	lw	$14,24($sp)
	#nop
	lbu	$13,8($14)
	lbu	$12,9($14)
	dsll	$13,$13,56
	.set	noreorder
	.set	nomacro
	b	.L73
	dsll	$12,$12,48
	.set	macro
	.set	reorder

.L54:
	lw	$12,24($sp)
	#nop
	lbu	$15,8($12)
	lbu	$13,9($12)
	lbu	$14,10($12)
	dsll	$15,$15,56
	dsll	$13,$13,48
	or	$15,$15,$13
	dsll	$14,$14,40
	.set	noreorder
	.set	nomacro
	b	.L51
	or	$3,$15,$14
	.set	macro
	.set	reorder

.L55:
	lw	$16,24($sp)
	#nop
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
	.set	noreorder
	.set	nomacro
	b	.L51
	or	$3,$12,$15
	.set	macro
	.set	reorder

.L56:
	lw	$16,24($sp)
	#nop
	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$15,10($16)
	lbu	$14,11($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	or	$12,$12,$15
	lbu	$13,12($16)
	dsll	$14,$14,32
	or	$12,$12,$14
	.set	noreorder
	.set	nomacro
	b	.L73
	dsll	$13,$13,24
	.set	macro
	.set	reorder

.L57:
	lw	$16,24($sp)
	#nop
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
	.set	noreorder
	.set	nomacro
	b	.L51
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L58:
	lw	$17,24($sp)
	#nop
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
	b	.L51
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L59:
	lw	$17,24($sp)
	#nop
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
.L73:
	or	$3,$12,$13
.L51:
	lw	$14,16($sp)
	#nop
	andi	$12,$14,0x1000
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L62
	lw	$13,32($sp)
	.set	macro
	.set	reorder

	li	$12,4			# 0x4
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L63
	li	$12,8			# 0x8
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L62
	l.s	$f0,0($12)
	.set	macro
	.set	reorder

.L63:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L74
	andi	$12,$14,0x2000
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	#nop
	l.s	$f0,0($12)
	l.s	$f2,4($12)
.L62:
	andi	$12,$14,0x2000
.L74:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L2
	lw	$13,32($sp)
	.set	macro
	.set	reorder

	li	$12,8			# 0x8
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L67
	li	$12,16			# 0x10
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	#nop
	l.d	$f0,0($12)
	b	.L2
.L67:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L2
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	#nop
	l.d	$f0,0($12)
	l.d	$f2,8($12)
.L2:
	ld	$31,192($sp)
	ld	$28,184($sp)
	ld	$18,176($sp)
	ld	$17,168($sp)
	ld	$16,160($sp)
	#nop
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,272
	.set	macro
	.set	reorder

.LFE1:
	.end	__vacall_r
