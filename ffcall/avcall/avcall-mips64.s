	.file	1 "avcall-mips64.c"
	.set	nobopt

 # GNU C 2.6.3 [AL 1.1, MM 40] Silicon Graphics Mips compiled by GNU C

 # Cc1 defaults:

 # Cc1 arguments (-G value = 8, Cpu = 4000, ISA = 3):
 # -mfp64 -mgp64 -quiet -dumpbase -mips3 -mlong64 -O2 -fomit-frame-pointer
 # -fno-omit-frame-pointer -o

	.text
	.align	2
	.globl	__builtin_avcall

	.text
	.ent	__builtin_avcall
__builtin_avcall:
	.frame	$fp,2104,$31		# vars= 2048, regs= 3/0, args= 32, extra= 0
	.mask	0xc0010000,-8
	.fmask	0x00000000,0
	dsubu	$sp,$sp,2104
	sd	$16,2080($sp)
	move	$16,$4
	sd	$31,2096($sp)
	sd	$fp,2088($sp)
	ld	$2,40($16)
	li	$3,8
	dsubu	$2,$2,96
	dsubu	$2,$2,$16
	ddiv	$2,$2,$3
	move	$fp,$sp
	lw	$3,52($16)
	dsll	$4,$2,32
	dsra	$4,$4,32
	.set	noreorder
	.set	nomacro
	beq	$3,$0,$L2
	move	$5,$sp
	.set	macro
	.set	reorder

	andi	$2,$3,0x0001
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L119
	andi	$2,$3,0x0002
	.set	macro
	.set	reorder

 #APP
	lwc1 $f12,60($16)
 #NO_APP
	andi	$2,$3,0x0002
$L119:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L120
	andi	$2,$3,0x0004
	.set	macro
	.set	reorder

 #APP
	lwc1 $f13,64($16)
 #NO_APP
	andi	$2,$3,0x0004
$L120:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L121
	andi	$2,$3,0x0008
	.set	macro
	.set	reorder

 #APP
	lwc1 $f14,68($16)
 #NO_APP
	andi	$2,$3,0x0008
$L121:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L122
	andi	$2,$3,0x0010
	.set	macro
	.set	reorder

 #APP
	lwc1 $f15,72($16)
 #NO_APP
	andi	$2,$3,0x0010
$L122:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L123
	andi	$2,$3,0x0020
	.set	macro
	.set	reorder

 #APP
	lwc1 $f16,76($16)
 #NO_APP
	andi	$2,$3,0x0020
$L123:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L124
	andi	$2,$3,0x0040
	.set	macro
	.set	reorder

 #APP
	lwc1 $f17,80($16)
 #NO_APP
	andi	$2,$3,0x0040
$L124:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L125
	andi	$2,$3,0x0080
	.set	macro
	.set	reorder

 #APP
	lwc1 $f18,84($16)
 #NO_APP
	andi	$2,$3,0x0080
$L125:
	beq	$2,$0,$L2
 #APP
	lwc1 $f19,88($16)
 #NO_APP
$L2:
	lw	$3,56($16)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$3,$0,$L11
	andi	$2,$3,0x0001
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L126
	andi	$2,$3,0x0002
	.set	macro
	.set	reorder

 #APP
	ldc1 $f12,96($16)
 #NO_APP
	andi	$2,$3,0x0002
$L126:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L127
	andi	$2,$3,0x0004
	.set	macro
	.set	reorder

 #APP
	ldc1 $f13,104($16)
 #NO_APP
	andi	$2,$3,0x0004
$L127:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L128
	andi	$2,$3,0x0008
	.set	macro
	.set	reorder

 #APP
	ldc1 $f14,112($16)
 #NO_APP
	andi	$2,$3,0x0008
$L128:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L129
	andi	$2,$3,0x0010
	.set	macro
	.set	reorder

 #APP
	ldc1 $f15,120($16)
 #NO_APP
	andi	$2,$3,0x0010
$L129:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L130
	andi	$2,$3,0x0020
	.set	macro
	.set	reorder

 #APP
	ldc1 $f16,128($16)
 #NO_APP
	andi	$2,$3,0x0020
$L130:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L131
	andi	$2,$3,0x0040
	.set	macro
	.set	reorder

 #APP
	ldc1 $f17,136($16)
 #NO_APP
	andi	$2,$3,0x0040
$L131:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L132
	andi	$2,$3,0x0080
	.set	macro
	.set	reorder

 #APP
	ldc1 $f18,144($16)
 #NO_APP
	andi	$2,$3,0x0080
$L132:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L133
	li	$6,8
	.set	macro
	.set	reorder

 #APP
	ldc1 $f19,152($16)
 #NO_APP
$L11:
	li	$6,8
$L133:
	dsll	$2,$4,32
	dsra	$3,$2,32
	slt	$2,$6,$3
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L21
	move	$4,$3
	.set	macro
	.set	reorder

	dsll	$3,$6,3
$L134:
	daddu	$2,$16,$3
	ld	$2,96($2)
	daddu	$6,$6,1
	daddu	$3,$3,$5
	sd	$2,-64($3)
	slt	$2,$6,$4
	.set	noreorder
	.set	nomacro
	bne	$2,$0,$L134
	dsll	$3,$6,3
	.set	macro
	.set	reorder

$L21:
 #APP
	ld $4,96($16)
	ld $5,104($16)
	ld $6,112($16)
	ld $7,120($16)
	ld $8,128($16)
	ld $9,136($16)
	ld $10,144($16)
	ld $11,152($16)
 #NO_APP
	ld	$25,0($16)
	#nop
	jal	$31,$25
	lw	$4,24($16)
	move	$6,$2
	li	$2,0x00000001		# 1
	.set	noreorder
	.set	nomacro
	beql	$4,$2,$L135
	move	$2,$0
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L115
	li	$2,0x00000002		# 2
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,0x00000003		# 3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,0x00000004		# 4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,0x00000005		# 5
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L117
	li	$2,0x00000006		# 6
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L117
	li	$2,0x00000007		# 7
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L118
	li	$2,0x00000008		# 8
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L118
	li	$2,0x00000009		# 9
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L115
	li	$2,0x0000000a		# 10
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L115
	li	$2,0x0000000b		# 11
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L115
	li	$2,0x0000000c		# 12
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L115
	li	$2,0x0000000d		# 13
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L51
	li	$2,0x0000000e		# 14
	.set	macro
	.set	reorder

	ld	$2,16($16)
	.set	noreorder
	.set	nomacro
	j	$L26
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

$L51:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L53
	li	$2,0x0000000f		# 15
	.set	macro
	.set	reorder

	ld	$2,16($16)
	#nop
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	j	$L135
	move	$2,$0
	.set	macro
	.set	reorder

$L53:
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L115
	li	$2,0x00000010		# 16
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L135
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$4,8($16)
	#nop
	andi	$2,$4,0x0001
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L58
	li	$2,1
	.set	macro
	.set	reorder

	ld	$3,32($16)
	#nop
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L59
	li	$2,2
	.set	macro
	.set	reorder

	ld	$3,16($16)
	lbu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L59:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L61
	li	$2,4
	.set	macro
	.set	reorder

	ld	$3,16($16)
	lhu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L26
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L61:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L63
	li	$4,8
	.set	macro
	.set	reorder

	ld	$3,16($16)
	lw	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L26
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L63:
	.set	noreorder
	.set	nomacro
	bne	$3,$4,$L65
	daddu	$2,$3,7
	.set	macro
	.set	reorder

	ld	$3,16($16)
	ld	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L26
	sd	$2,0($3)
	.set	macro
	.set	reorder

$L65:
	ddivu	$2,$2,$4
	dsll	$5,$2,32
	dsra	$5,$5,32
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,$L135
	move	$2,$0
	.set	macro
	.set	reorder

$L69:
	dsll	$3,$5,32
	dsra	$3,$3,32
	dsll	$3,$3,3
	ld	$4,16($16)
	daddu	$2,$6,$3
	ld	$2,0($2)
	addu	$5,$5,-1
	daddu	$4,$4,$3
	.set	noreorder
	.set	nomacro
	bgez	$5,$L69
	sd	$2,0($4)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L135
	move	$2,$0
	.set	macro
	.set	reorder

$L58:
	andi	$2,$4,0x0200
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L26
	andi	$2,$4,0x0004
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L73
	li	$2,1
	.set	macro
	.set	reorder

	ld	$3,32($16)
	#nop
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L74
	li	$2,2
	.set	macro
	.set	reorder

$L116:
	ld	$3,16($16)
	andi	$2,$6,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L74:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L76
	li	$2,4
	.set	macro
	.set	reorder

$L117:
	ld	$3,16($16)
	andi	$2,$6,0xffff
	.set	noreorder
	.set	nomacro
	j	$L26
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L76:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L78
	li	$2,8
	.set	macro
	.set	reorder

$L118:
	ld	$3,16($16)
	dsll	$2,$6,32
	dsra	$2,$2,32
	.set	noreorder
	.set	nomacro
	j	$L26
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L78:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L135
	move	$2,$0
	.set	macro
	.set	reorder

$L115:
	ld	$2,16($16)
	.set	noreorder
	.set	nomacro
	j	$L26
	sd	$6,0($2)
	.set	macro
	.set	reorder

$L73:
	ld	$5,32($16)
	#nop
	sltu	$4,$0,$5
	dsll	$4,$4,32
	dsra	$4,$4,32
	sltu	$2,$5,17
	dsll	$2,$2,32
	dsra	$2,$2,32
	and	$4,$4,$2
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L26
	li	$2,1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L83
	li	$2,2
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L83:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L85
	li	$2,3
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$3,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,1($3)
	.set	macro
	.set	reorder

$L85:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L87
	li	$2,4
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$3,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($3)
	ld	$3,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,2($3)
	.set	macro
	.set	reorder

$L87:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L89
	li	$2,5
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$3,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($3)
	ld	$3,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$3,16($16)
	dsra	$2,$6,32
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,3($3)
	.set	macro
	.set	reorder

$L89:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L91
	li	$2,6
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$3,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($3)
	ld	$3,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$3,16($16)
	dsra	$2,$6,32
	andi	$2,$2,0x00ff
	sb	$2,3($3)
	ld	$3,16($16)
	dsra	$2,$6,24
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,4($3)
	.set	macro
	.set	reorder

$L91:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L93
	li	$2,7
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$3,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($3)
	ld	$3,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$3,16($16)
	dsra	$2,$6,32
	andi	$2,$2,0x00ff
	sb	$2,3($3)
	ld	$3,16($16)
	dsra	$2,$6,24
	andi	$2,$2,0x00ff
	sb	$2,4($3)
	ld	$3,16($16)
	dsra	$2,$6,16
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,5($3)
	.set	macro
	.set	reorder

$L93:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,$L95
	dsubu	$2,$5,8
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$3,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($3)
	ld	$3,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$3,16($16)
	dsra	$2,$6,32
	andi	$2,$2,0x00ff
	sb	$2,3($3)
	ld	$3,16($16)
	dsra	$2,$6,24
	andi	$2,$2,0x00ff
	sb	$2,4($3)
	ld	$3,16($16)
	dsra	$2,$6,16
	andi	$2,$2,0x00ff
	sb	$2,5($3)
	ld	$3,16($16)
	dsra	$2,$6,8
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$2,6($3)
	.set	macro
	.set	reorder

$L95:
	sltu	$2,$2,9
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L26
	dsra	$2,$6,56
	.set	macro
	.set	reorder

	ld	$4,16($16)
	andi	$2,$2,0x00ff
	sb	$2,0($4)
	ld	$4,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$4,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	sb	$2,2($4)
	ld	$4,16($16)
	dsra	$2,$6,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	ld	$4,16($16)
	dsra	$2,$6,24
	andi	$2,$2,0x00ff
	sb	$2,4($4)
	ld	$4,16($16)
	dsra	$2,$6,16
	andi	$2,$2,0x00ff
	sb	$2,5($4)
	ld	$4,16($16)
	dsra	$2,$6,8
	andi	$2,$2,0x00ff
	sb	$2,6($4)
	ld	$4,16($16)
	andi	$2,$6,0x00ff
	sb	$2,7($4)
	ld	$4,32($16)
	li	$2,8
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L26
	li	$2,9
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L100
	li	$2,10
	.set	macro
	.set	reorder

	ld	$2,16($16)
	dsra	$3,$3,56
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,8($2)
	.set	macro
	.set	reorder

$L100:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L102
	li	$2,11
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$2,16($16)
	dsra	$3,$3,48
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,9($2)
	.set	macro
	.set	reorder

$L102:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L104
	li	$2,12
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$4,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($4)
	ld	$2,16($16)
	dsra	$3,$3,40
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,10($2)
	.set	macro
	.set	reorder

$L104:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L106
	li	$2,13
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$4,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($4)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$2,16($16)
	dsra	$3,$3,32
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,11($2)
	.set	macro
	.set	reorder

$L106:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L108
	li	$2,14
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$4,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($4)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$4,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($4)
	ld	$2,16($16)
	dsra	$3,$3,24
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,12($2)
	.set	macro
	.set	reorder

$L108:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L110
	li	$2,15
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$4,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($4)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$4,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($4)
	ld	$4,16($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	ld	$2,16($16)
	dsra	$3,$3,16
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,13($2)
	.set	macro
	.set	reorder

$L110:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L112
	li	$2,16
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$4,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($4)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$4,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($4)
	ld	$4,16($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	ld	$4,16($16)
	dsra	$2,$3,16
	andi	$2,$2,0x00ff
	sb	$2,13($4)
	ld	$2,16($16)
	dsra	$3,$3,8
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L26
	sb	$3,14($2)
	.set	macro
	.set	reorder

$L112:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L135
	move	$2,$0
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$4,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($4)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$4,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($4)
	ld	$4,16($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	ld	$4,16($16)
	dsra	$2,$3,16
	andi	$2,$2,0x00ff
	sb	$2,13($4)
	ld	$4,16($16)
	dsra	$2,$3,8
	andi	$2,$2,0x00ff
	sb	$2,14($4)
	ld	$2,16($16)
	andi	$3,$3,0x00ff
	sb	$3,15($2)
$L26:
	move	$2,$0
$L135:
	move	$sp,$fp			# sp not trusted here
	ld	$31,2096($sp)
	ld	$fp,2088($sp)
	ld	$16,2080($sp)
	daddu	$sp,$sp,2104
	j	$31
	.end	__builtin_avcall
