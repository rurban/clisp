	.file	1 "avcall-mips.c"
	.set	nobopt

 # GNU C 2.6.3 [AL 1.1, MM 40] Silicon Graphics Mips compiled by GNU C

 # Cc1 defaults:

 # Cc1 arguments (-G value = 8, Cpu = 3000, ISA = 1):
 # -quiet -dumpbase -O2 -fomit-frame-pointer -fno-omit-frame-pointer -o

gcc2_compiled.:
__gnu_compiled_c:
	.text
	.align	2
	.globl	__builtin_avcall

	.text
	.ent	__builtin_avcall
__builtin_avcall:
	.frame	$fp,32,$31		# vars= 0, regs= 3/0, args= 16, extra= 0
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
	#nop
	lw	$2,20($16)
	lw	$3,4($16)
	addu	$2,$2,-48
	subu	$2,$2,$16
	sra	$5,$2,2
	andi	$2,$3,0x0200
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L2
	li	$6,0x00000004		# 4
	.set	macro
	.set	reorder

 #APP
	l.d $f12,32($16)
 #NO_APP
	andi	$2,$3,0x0400
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L51
	slt	$2,$6,$5
	.set	macro
	.set	reorder

 #APP
	l.d $f14,40($16)
 #NO_APP
$L2:
	slt	$2,$6,$5
$L51:
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
	lw	$4,12($16)
	move	$6,$2
	sltu	$2,$4,17
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L9
	sll	$2,$4,2
	.set	macro
	.set	reorder

	lw	$2,$L50($2)
	#nop
	j	$2
	.rdata
	.align	3
$L50:
	.word	$L45
	.word	$L9
	.word	$L43
	.word	$L43
	.word	$L43
	.word	$L44
	.word	$L44
	.word	$L45
	.word	$L45
	.word	$L45
	.word	$L45
	.word	$L22
	.word	$L22
	.word	$L23
	.word	$L24
	.word	$L45
	.word	$L26
	.text
$L22:
	lw	$2,8($16)
	#nop
	sw	$6,0($2)
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L9
	sw	$3,4($2)
	.set	macro
	.set	reorder

$L23:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L9
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

$L24:
	lw	$2,8($16)
	#nop
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	j	$L52
	move	$2,$0
	.set	macro
	.set	reorder

$L26:
	lw	$3,4($16)
	#nop
	andi	$2,$3,0x0001
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L27
	li	$2,0x00000002		# 2
	.set	macro
	.set	reorder

	lw	$3,16($16)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L30
	sltu	$2,$3,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L39
	li	$2,0x00000001		# 1
	.set	macro
	.set	reorder

	beq	$3,$2,$L29
	j	$L33
$L39:
	li	$2,0x00000004		# 4
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L31
	li	$2,0x00000008		# 8
	.set	macro
	.set	reorder

	beq	$3,$2,$L32
	j	$L33
$L29:
	lw	$3,8($16)
	lbu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L9
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L30:
	lw	$3,8($16)
	lhu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L9
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L31:
	lw	$3,8($16)
	lw	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L9
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L32:
	lw	$3,8($16)
	lw	$2,0($6)
	#nop
	sw	$2,0($3)
	lw	$3,8($16)
	lw	$2,4($6)
	.set	noreorder
	.set	nomacro
	j	$L9
	sw	$2,4($3)
	.set	macro
	.set	reorder

$L33:
	lw	$2,16($16)
	#nop
	addu	$2,$2,3
	srl	$5,$2,2
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,$L9
	sll	$2,$5,2
	.set	macro
	.set	reorder

	addu	$6,$2,$6
$L36:
	lw	$2,0($6)
	addu	$6,$6,-4
	sll	$3,$5,2
	lw	$4,8($16)
	addu	$5,$5,-1
	addu	$3,$3,$4
	.set	noreorder
	.set	nomacro
	bgez	$5,$L36
	sw	$2,0($3)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L52
	move	$2,$0
	.set	macro
	.set	reorder

$L27:
	andi	$2,$3,0x0002
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L9
	li	$2,0x00000002		# 2
	.set	macro
	.set	reorder

	lw	$3,16($16)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L44
	sltu	$2,$3,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L48
	li	$2,0x00000001		# 1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L43
	move	$2,$0
	.set	macro
	.set	reorder

	j	$L52
$L48:
	li	$2,0x00000004		# 4
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L45
	move	$2,$0
	.set	macro
	.set	reorder

	j	$L52
$L43:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L9
	sb	$6,0($2)
	.set	macro
	.set	reorder

$L44:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L9
	sh	$6,0($2)
	.set	macro
	.set	reorder

$L45:
	lw	$2,8($16)
	#nop
	sw	$6,0($2)
$L9:
	move	$2,$0
$L52:
	move	$sp,$fp			# sp not trusted here
	lw	$31,24($sp)
	lw	$fp,20($sp)
	lw	$16,16($sp)
	addu	$sp,$sp,32
	j	$31
	.end	__builtin_avcall
