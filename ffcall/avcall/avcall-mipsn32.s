	#.file	1 "avcall-mipsn32.c"
	.set	nobopt
	.option pic2
	.section	.text
	.text
	.align	2
	.globl	__builtin_avcall
	.ent	__builtin_avcall
__builtin_avcall:
.LFB1:
	.frame	$fp,2096,$31		# vars= 2048, regs= 4/0, args= 0, extra= 16
	.mask	0xd0010000,-8
	.fmask	0x00000000,0
	subu	$sp,$sp,2096
.LCFI0:
	sd	$fp,2080($sp)
.LCFI1:
	move	$fp,$sp
.LCFI2:
	sd	$31,2088($sp)
.LCFI3:
	sd	$28,2072($sp)
.LCFI4:
	sd	$16,2064($sp)
.LCFI5:
	.set	noat
	lui	$1,%hi(%neg(%gp_rel(__builtin_avcall)))
	addiu	$1,$1,%lo(%neg(%gp_rel(__builtin_avcall)))
	daddu	$gp,$1,$25
	.set	at
	move	$16,$4
	lw	$2,20($16)
	move	$8,$sp
	lw	$3,28($16)
	addu	$2,$2,-72
	subu	$2,$2,$16
	.set	noreorder
	.set	nomacro
	beq	$3,$0,.L2
	sra	$4,$2,3
	.set	macro
	.set	reorder

	andi	$2,$3,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L103
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 #APP
	lwc1 $f12,36($16)
 #NO_APP
	andi	$2,$3,0x2
.L103:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L104
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 #APP
	lwc1 $f13,40($16)
 #NO_APP
	andi	$2,$3,0x4
.L104:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L105
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 #APP
	lwc1 $f14,44($16)
 #NO_APP
	andi	$2,$3,0x8
.L105:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L106
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 #APP
	lwc1 $f15,48($16)
 #NO_APP
	andi	$2,$3,0x10
.L106:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L107
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 #APP
	lwc1 $f16,52($16)
 #NO_APP
	andi	$2,$3,0x20
.L107:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L108
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 #APP
	lwc1 $f17,56($16)
 #NO_APP
	andi	$2,$3,0x40
.L108:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L109
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 #APP
	lwc1 $f18,60($16)
 #NO_APP
	andi	$2,$3,0x80
.L109:
	.set	noreorder
	.set	nomacro
	beql	$2,$0,.L110
	lw	$3,32($16)
	.set	macro
	.set	reorder

 #APP
	lwc1 $f19,64($16)
 #NO_APP
.L2:
	#nop
	lw	$3,32($16)
.L110:
	.set	noreorder
	.set	nomacro
	beq	$3,$0,.L11
	andi	$2,$3,0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L111
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 #APP
	ldc1 $f12,72($16)
 #NO_APP
	andi	$2,$3,0x2
.L111:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L112
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 #APP
	ldc1 $f13,80($16)
 #NO_APP
	andi	$2,$3,0x4
.L112:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L113
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 #APP
	ldc1 $f14,88($16)
 #NO_APP
	andi	$2,$3,0x8
.L113:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L114
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 #APP
	ldc1 $f15,96($16)
 #NO_APP
	andi	$2,$3,0x10
.L114:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L115
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 #APP
	ldc1 $f16,104($16)
 #NO_APP
	andi	$2,$3,0x20
.L115:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L116
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 #APP
	ldc1 $f17,112($16)
 #NO_APP
	andi	$2,$3,0x40
.L116:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L117
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 #APP
	ldc1 $f18,120($16)
 #NO_APP
	andi	$2,$3,0x80
.L117:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L118
	dli	$7,0x8		# 8
	.set	macro
	.set	reorder

 #APP
	ldc1 $f19,128($16)
 #NO_APP
.L11:
	dli	$7,0x8		# 8
.L118:
	move	$5,$4
	slt	$2,$7,$5
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L21
	addu	$6,$16,72
	.set	macro
	.set	reorder

.L23:
	dsll	$3,$7,32
	dsra	$3,$3,32
	sll	$3,$3,3
	addu	$2,$6,$3
	ld	$4,0($2)
	daddu	$7,$7,1
	addu	$3,$3,$8
	slt	$2,$7,$5
	.set	noreorder
	.set	nomacro
	bne	$2,$0,.L23
	sd	$4,-64($3)
	.set	macro
	.set	reorder

.L21:
 #APP
	ld $4,72($16)
	ld $5,80($16)
	ld $6,88($16)
	ld $7,96($16)
	ld $8,104($16)
	ld $9,112($16)
	ld $10,120($16)
	ld $11,128($16)
 #NO_APP
	lw	$25,0($16)
	#nop
	jal	$31,$25
	lw	$4,12($16)
	#nop
	sltu	$3,$4,17
	.set	noreorder
	.set	nomacro
	beq	$3,$0,.L25
	move	$7,$2
	.set	macro
	.set	reorder

	la	$2,.L99
	sll	$4,$4,2
	addu	$4,$4,$2
	lw	$5,0($4)
	#nop
	j	$5
.section	.rodata
	.align	3
.L99:
	.word	.L63
	.word	.L25
	.word	.L60
	.word	.L60
	.word	.L60
	.word	.L61
	.word	.L61
	.word	.L62
	.word	.L62
	.word	.L62
	.word	.L62
	.word	.L63
	.word	.L63
	.word	.L39
	.word	.L40
	.word	.L62
	.word	.L42
	.text
.L39:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	b	.L25
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

.L40:
	lw	$2,8($16)
	#nop
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	b	.L101
	move	$2,$0
	.set	macro
	.set	reorder

.L42:
	lw	$5,4($16)
	#nop
	andi	$2,$5,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L43
	li	$2,2			# 0x2
	.set	macro
	.set	reorder

	lw	$4,16($16)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L46
	move	$3,$4
	.set	macro
	.set	reorder

	sltu	$2,$4,3
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L55
	li	$2,1			# 0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L45
	addu	$2,$3,7
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L120
	srl	$5,$2,3
	.set	macro
	.set	reorder

.L55:
	li	$2,4			# 0x4
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L47
	li	$2,8			# 0x8
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L48
	addu	$2,$3,7
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L102
	srl	$5,$2,3
	.set	macro
	.set	reorder

.L45:
	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lbu	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$4,0($2)
	.set	macro
	.set	reorder

.L46:
	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lhu	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L25
	sh	$4,0($2)
	.set	macro
	.set	reorder

.L47:
	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lw	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L25
	sw	$4,0($2)
	.set	macro
	.set	reorder

.L48:
	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	ld	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L25
	sd	$4,0($2)
	.set	macro
	.set	reorder

.L102:
.L120:
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,.L25
	sll	$2,$5,3
	.set	macro
	.set	reorder

	dsll	$3,$7,32
	dsra	$3,$3,32
	addu	$6,$2,$3
.L52:
	ld	$4,0($6)
	addu	$6,$6,-8
	lw	$3,8($16)
	sll	$2,$5,3
	addu	$5,$5,-1
	addu	$2,$2,$3
	.set	noreorder
	.set	nomacro
	bgez	$5,.L52
	sd	$4,0($2)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L101
	move	$2,$0
	.set	macro
	.set	reorder

.L43:
	andi	$2,$5,0x100
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L25
	andi	$2,$5,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L58
	lw	$4,16($16)
	.set	macro
	.set	reorder

	li	$2,2			# 0x2
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L61
	sltu	$2,$4,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L66
	li	$2,1			# 0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L60
	move	$2,$0
	.set	macro
	.set	reorder

	b	.L101
.L66:
	li	$2,4			# 0x4
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L62
	li	$2,8			# 0x8
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L63
	move	$2,$0
	.set	macro
	.set	reorder

	b	.L101
.L60:
	lw	$3,8($16)
	andi	$2,$7,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$2,0($3)
	.set	macro
	.set	reorder

.L61:
	lw	$3,8($16)
	andi	$2,$7,0xffff
	.set	noreorder
	.set	nomacro
	b	.L25
	sh	$2,0($3)
	.set	macro
	.set	reorder

.L62:
	lw	$3,8($16)
	dsll	$2,$7,32
	dsra	$2,$2,32
	.set	noreorder
	.set	nomacro
	b	.L25
	sw	$2,0($3)
	.set	macro
	.set	reorder

.L63:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	b	.L25
	sd	$7,0($2)
	.set	macro
	.set	reorder

.L58:
	addu	$5,$4,-1
	sltu	$2,$5,16
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L119
	addu	$4,$4,-9
	.set	macro
	.set	reorder

	la	$2,.L86
	sll	$4,$5,2
	addu	$4,$4,$2
	lw	$5,0($4)
	#nop
	j	$5
.section	.rodata
	.align	3
.L86:
	.word	.L69
	.word	.L70
	.word	.L71
	.word	.L72
	.word	.L73
	.word	.L74
	.word	.L75
	.word	.L84
	.word	.L84
	.word	.L84
	.word	.L84
	.word	.L84
	.word	.L84
	.word	.L84
	.word	.L84
	.word	.L84
	.text
.L69:
	lw	$4,8($16)
	dsra	$2,$7,56
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$2,0($4)
	.set	macro
	.set	reorder

.L70:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$4,1($6)
	.set	macro
	.set	reorder

.L71:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	sb	$4,1($6)
	lw	$5,8($16)
	dsra	$2,$7,40
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$2,2($5)
	.set	macro
	.set	reorder

.L72:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	sb	$4,1($6)
	lw	$5,8($16)
	dsra	$2,$7,40
	sb	$2,2($5)
	lw	$6,8($16)
	dsra	$4,$7,32
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$4,3($6)
	.set	macro
	.set	reorder

.L73:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	sb	$4,1($6)
	lw	$5,8($16)
	dsra	$2,$7,40
	sb	$2,2($5)
	lw	$6,8($16)
	dsra	$4,$7,32
	sb	$4,3($6)
	lw	$5,8($16)
	dsrl	$2,$7,24
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$2,4($5)
	.set	macro
	.set	reorder

.L74:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	sb	$4,1($6)
	lw	$5,8($16)
	dsra	$2,$7,40
	sb	$2,2($5)
	lw	$6,8($16)
	dsra	$4,$7,32
	sb	$4,3($6)
	lw	$5,8($16)
	dsrl	$2,$7,24
	sb	$2,4($5)
	lw	$6,8($16)
	dsrl	$4,$7,16
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$4,5($6)
	.set	macro
	.set	reorder

.L75:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	sb	$4,1($6)
	lw	$5,8($16)
	dsra	$2,$7,40
	sb	$2,2($5)
	lw	$6,8($16)
	dsra	$4,$7,32
	sb	$4,3($6)
	lw	$5,8($16)
	dsrl	$2,$7,24
	sb	$2,4($5)
	lw	$6,8($16)
	dsrl	$4,$7,16
	sb	$4,5($6)
	lw	$5,8($16)
	dsrl	$2,$7,8
	.set	noreorder
	.set	nomacro
	b	.L100
	sb	$2,6($5)
	.set	macro
	.set	reorder

.L84:
	lw	$5,8($16)
	dsra	$2,$7,56
	sb	$2,0($5)
	lw	$6,8($16)
	dsra	$4,$7,48
	sb	$4,1($6)
	lw	$5,8($16)
	dsra	$2,$7,40
	sb	$2,2($5)
	lw	$6,8($16)
	dsra	$4,$7,32
	sb	$4,3($6)
	lw	$5,8($16)
	dsrl	$2,$7,24
	sb	$2,4($5)
	lw	$6,8($16)
	dsrl	$4,$7,16
	sb	$4,5($6)
	lw	$5,8($16)
	dsrl	$2,$7,8
	sb	$2,6($5)
	lw	$6,8($16)
	andi	$4,$7,0x00ff
	sb	$4,7($6)
.L100:
	lw	$4,16($16)
	#nop
	addu	$4,$4,-9
.L119:
	sltu	$2,$4,8
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L25
	sll	$4,$4,2
	.set	macro
	.set	reorder

	la	$2,.L97
	addu	$4,$4,$2
	lw	$5,0($4)
	#nop
	j	$5
.section	.rodata
	.align	3
.L97:
	.word	.L88
	.word	.L89
	.word	.L90
	.word	.L91
	.word	.L92
	.word	.L93
	.word	.L94
	.word	.L95
	.text
.L88:
	lw	$2,8($16)
	dsra	$3,$3,56
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,8($2)
	.set	macro
	.set	reorder

.L89:
	lw	$4,8($16)
	dsra	$2,$3,56
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$3,$3,48
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,9($5)
	.set	macro
	.set	reorder

.L90:
	lw	$5,8($16)
	dsra	$2,$3,56
	sb	$2,8($5)
	lw	$6,8($16)
	dsra	$4,$3,48
	sb	$4,9($6)
	lw	$2,8($16)
	dsra	$3,$3,40
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,10($2)
	.set	macro
	.set	reorder

.L91:
	lw	$5,8($16)
	dsra	$2,$3,56
	sb	$2,8($5)
	lw	$6,8($16)
	dsra	$4,$3,48
	sb	$4,9($6)
	lw	$5,8($16)
	dsra	$2,$3,40
	sb	$2,10($5)
	lw	$4,8($16)
	dsra	$3,$3,32
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,11($4)
	.set	macro
	.set	reorder

.L92:
	lw	$5,8($16)
	dsra	$2,$3,56
	sb	$2,8($5)
	lw	$6,8($16)
	dsra	$4,$3,48
	sb	$4,9($6)
	lw	$5,8($16)
	dsra	$2,$3,40
	sb	$2,10($5)
	lw	$6,8($16)
	dsra	$4,$3,32
	sb	$4,11($6)
	lw	$2,8($16)
	dsrl	$3,$3,24
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,12($2)
	.set	macro
	.set	reorder

.L93:
	lw	$5,8($16)
	dsra	$2,$3,56
	sb	$2,8($5)
	lw	$6,8($16)
	dsra	$4,$3,48
	sb	$4,9($6)
	lw	$5,8($16)
	dsra	$2,$3,40
	sb	$2,10($5)
	lw	$6,8($16)
	dsra	$4,$3,32
	sb	$4,11($6)
	lw	$5,8($16)
	dsrl	$2,$3,24
	sb	$2,12($5)
	lw	$4,8($16)
	dsrl	$3,$3,16
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,13($4)
	.set	macro
	.set	reorder

.L94:
	lw	$5,8($16)
	dsra	$2,$3,56
	sb	$2,8($5)
	lw	$6,8($16)
	dsra	$4,$3,48
	sb	$4,9($6)
	lw	$5,8($16)
	dsra	$2,$3,40
	sb	$2,10($5)
	lw	$6,8($16)
	dsra	$4,$3,32
	sb	$4,11($6)
	lw	$5,8($16)
	dsrl	$2,$3,24
	sb	$2,12($5)
	lw	$6,8($16)
	dsrl	$4,$3,16
	sb	$4,13($6)
	lw	$2,8($16)
	dsrl	$3,$3,8
	.set	noreorder
	.set	nomacro
	b	.L25
	sb	$3,14($2)
	.set	macro
	.set	reorder

.L95:
	lw	$5,8($16)
	dsra	$2,$3,56
	sb	$2,8($5)
	lw	$6,8($16)
	dsra	$4,$3,48
	sb	$4,9($6)
	lw	$5,8($16)
	dsra	$2,$3,40
	sb	$2,10($5)
	lw	$6,8($16)
	dsra	$4,$3,32
	sb	$4,11($6)
	lw	$5,8($16)
	dsrl	$2,$3,24
	sb	$2,12($5)
	lw	$6,8($16)
	dsrl	$4,$3,16
	sb	$4,13($6)
	lw	$5,8($16)
	dsrl	$2,$3,8
	sb	$2,14($5)
	lw	$4,8($16)
	andi	$3,$3,0x00ff
	sb	$3,15($4)
.L25:
	move	$2,$0
.L101:
	move	$sp,$fp
	ld	$31,2088($sp)
	ld	$fp,2080($sp)
	ld	$28,2072($sp)
	ld	$16,2064($sp)
	#nop
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,2096
	.set	macro
	.set	reorder

.LFE1:
	.end	__builtin_avcall
