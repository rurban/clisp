	.file	1 "vacall-mips.c"
	.set	nobopt

 # GNU C 2.6.3 [AL 1.1, MM 40] Silicon Graphics Mips compiled by GNU C

 # Cc1 defaults:

 # Cc1 arguments (-G value = 8, Cpu = 3000, ISA = 1):
 # -quiet -dumpbase -O2 -fomit-frame-pointer -fno-omit-frame-pointer -o

gcc2_compiled.:
__gnu_compiled_c:
	.text
	.align	2
	.globl	__vacall_r

	.text
	.ent	__vacall_r
__vacall_r:
	.frame	$fp,88,$31		# vars= 64, regs= 2/0, args= 16, extra= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	subu	$sp,$sp,88
	sw	$fp,80($sp)
	move	$fp,$sp
	addu	$8,$fp,88
	sw	$31,84($sp)
	sw	$4,88($fp)
	addu	$4,$fp,104
	sw	$5,92($fp)
	sw	$6,96($fp)
	sw	$7,100($fp)
	s.d	$f12,64($fp)
	s.d	$f14,72($fp)
	s.s	$f12,56($fp)
	s.s	$f14,60($fp)
	sw	$0,16($fp)
	sw	$8,20($fp)
	sw	$0,24($fp)
	sw	$0,28($fp)
	sw	$4,48($fp)
	sw	$0,52($fp)
	lw	$25,0($2)
	lw	$4,4($2)
	.set	noreorder
	.set	nomacro
	jal	$31,$25
	addu	$5,$fp,16
	.set	macro
	.set	reorder

	lw	$5,28($fp)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$5,$0,$L3
	li	$4,0x00000001		# 1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L39
	li	$4,0x00000002		# 2
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L6
	li	$4,0x00000003		# 3
	.set	macro
	.set	reorder

	lb	$2,40($fp)
	j	$L3
$L6:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L8
	li	$4,0x00000004		# 4
	.set	macro
	.set	reorder

$L39:
	lbu	$2,40($fp)
	j	$L3
$L8:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L10
	li	$4,0x00000005		# 5
	.set	macro
	.set	reorder

	lh	$2,40($fp)
	j	$L3
$L10:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L12
	li	$4,0x00000006		# 6
	.set	macro
	.set	reorder

	lhu	$2,40($fp)
	j	$L3
$L12:
	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,0x00000007		# 7
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,0x00000008		# 8
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,0x00000009		# 9
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
	li	$4,0x0000000c		# 12
	.set	macro
	.set	reorder

	lw	$2,40($fp)
	lw	$3,44($fp)
	j	$L3
$L22:
	lw	$5,28($fp)
	#nop
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L24
	li	$4,0x0000000d		# 13
	.set	macro
	.set	reorder

	l.s	$f0,40($fp)
	j	$L3
$L24:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L26
	li	$4,0x0000000e		# 14
	.set	macro
	.set	reorder

	l.d	$f0,40($fp)
	j	$L3
$L26:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L28
	li	$4,0x0000000f		# 15
	.set	macro
	.set	reorder

$L40:
	lw	$2,40($fp)
	j	$L3
$L28:
	bne	$5,$4,$L3
	lw	$5,16($fp)
	#nop
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
	li	$4,0x00000001		# 1
	.set	macro
	.set	reorder

	lw	$5,32($fp)
	#nop
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L34
	li	$4,0x00000002		# 2
	.set	macro
	.set	reorder

	lw	$4,24($fp)
	#nop
	lbu	$2,0($4)
	j	$L3
$L34:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L36
	li	$4,0x00000004		# 4
	.set	macro
	.set	reorder

	lw	$4,24($fp)
	#nop
	lhu	$2,0($4)
	j	$L3
$L36:
	bne	$5,$4,$L3
	lw	$4,24($fp)
	#nop
	lw	$2,0($4)
$L3:
	move	$sp,$fp			# sp not trusted here
	lw	$31,84($sp)
	lw	$fp,80($sp)
	addu	$sp,$sp,88
	j	$31
	.end	__vacall_r
