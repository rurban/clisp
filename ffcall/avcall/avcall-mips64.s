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
	.frame	$fp,2112,$31		# vars= 2056, regs= 3/0, args= 32, extra= 0
	.mask	0xc0010000,-8
	.fmask	0x00000000,0
	dsubu	$sp,$sp,2112
	sd	$16,2088($sp)
	move	$16,$4
	sd	$31,2104($sp)
	sd	$fp,2096($sp)
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
	beq	$2,$0,$L100
	andi	$2,$3,0x0002
	.set	macro
	.set	reorder

 #APP
	lwc1 $f12,60($16)
 #NO_APP
	andi	$2,$3,0x0002
$L100:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L101
	andi	$2,$3,0x0004
	.set	macro
	.set	reorder

 #APP
	lwc1 $f13,64($16)
 #NO_APP
	andi	$2,$3,0x0004
$L101:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L102
	andi	$2,$3,0x0008
	.set	macro
	.set	reorder

 #APP
	lwc1 $f14,68($16)
 #NO_APP
	andi	$2,$3,0x0008
$L102:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L103
	andi	$2,$3,0x0010
	.set	macro
	.set	reorder

 #APP
	lwc1 $f15,72($16)
 #NO_APP
	andi	$2,$3,0x0010
$L103:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L104
	andi	$2,$3,0x0020
	.set	macro
	.set	reorder

 #APP
	lwc1 $f16,76($16)
 #NO_APP
	andi	$2,$3,0x0020
$L104:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L105
	andi	$2,$3,0x0040
	.set	macro
	.set	reorder

 #APP
	lwc1 $f17,80($16)
 #NO_APP
	andi	$2,$3,0x0040
$L105:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L106
	andi	$2,$3,0x0080
	.set	macro
	.set	reorder

 #APP
	lwc1 $f18,84($16)
 #NO_APP
	andi	$2,$3,0x0080
$L106:
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
	beq	$2,$0,$L107
	andi	$2,$3,0x0002
	.set	macro
	.set	reorder

 #APP
	ldc1 $f12,96($16)
 #NO_APP
	andi	$2,$3,0x0002
$L107:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L108
	andi	$2,$3,0x0004
	.set	macro
	.set	reorder

 #APP
	ldc1 $f13,104($16)
 #NO_APP
	andi	$2,$3,0x0004
$L108:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L109
	andi	$2,$3,0x0008
	.set	macro
	.set	reorder

 #APP
	ldc1 $f14,112($16)
 #NO_APP
	andi	$2,$3,0x0008
$L109:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L110
	andi	$2,$3,0x0010
	.set	macro
	.set	reorder

 #APP
	ldc1 $f15,120($16)
 #NO_APP
	andi	$2,$3,0x0010
$L110:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L111
	andi	$2,$3,0x0020
	.set	macro
	.set	reorder

 #APP
	ldc1 $f16,128($16)
 #NO_APP
	andi	$2,$3,0x0020
$L111:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L112
	andi	$2,$3,0x0040
	.set	macro
	.set	reorder

 #APP
	ldc1 $f17,136($16)
 #NO_APP
	andi	$2,$3,0x0040
$L112:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L113
	andi	$2,$3,0x0080
	.set	macro
	.set	reorder

 #APP
	ldc1 $f18,144($16)
 #NO_APP
	andi	$2,$3,0x0080
$L113:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L114
	li	$6,8
	.set	macro
	.set	reorder

 #APP
	ldc1 $f19,152($16)
 #NO_APP
$L11:
	li	$6,8
$L114:
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
$L115:
	daddu	$2,$16,$3
	ld	$2,96($2)
	daddu	$6,$6,1
	daddu	$3,$3,$5
	sd	$2,-64($3)
	slt	$2,$6,$4
	.set	noreorder
	.set	nomacro
	bne	$2,$0,$L115
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
	sltu	$2,$4,17
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L25
	dsll	$2,$4,32
	.set	macro
	.set	reorder

	dsrl	$2,$2,32
	dsll	$2,$2,3
	ld	$2,$L99($2)
	#nop
	j	$2
	.rdata
	.align	3
$L99:
	.dword	$L63
	.dword	$L25
	.dword	$L60
	.dword	$L60
	.dword	$L60
	.dword	$L61
	.dword	$L61
	.dword	$L62
	.dword	$L62
	.dword	$L63
	.dword	$L63
	.dword	$L63
	.dword	$L63
	.dword	$L39
	.dword	$L40
	.dword	$L63
	.dword	$L42
	.text
$L39:
	ld	$2,16($16)
	.set	noreorder
	.set	nomacro
	j	$L25
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

$L40:
	ld	$2,16($16)
	#nop
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	j	$L116
	move	$2,$0
	.set	macro
	.set	reorder

$L42:
	lw	$4,8($16)
	#nop
	andi	$2,$4,0x0001
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L43
	li	$2,2
	.set	macro
	.set	reorder

	ld	$3,32($16)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L46
	sltu	$2,$3,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L55
	li	$2,1
	.set	macro
	.set	reorder

	beq	$3,$2,$L45
	j	$L49
$L55:
	li	$2,4
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L47
	li	$2,8
	.set	macro
	.set	reorder

	beq	$3,$2,$L48
	j	$L49
$L45:
	ld	$3,16($16)
	lbu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L25
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L46:
	ld	$3,16($16)
	lhu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L25
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L47:
	ld	$3,16($16)
	lw	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L25
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L48:
	ld	$3,16($16)
	ld	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L25
	sd	$2,0($3)
	.set	macro
	.set	reorder

$L49:
	ld	$2,32($16)
	#nop
	daddu	$2,$2,7
	dsrl	$2,$2,3
	dsll	$5,$2,32
	dsra	$5,$5,32
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,$L116
	move	$2,$0
	.set	macro
	.set	reorder

$L52:
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
	bgez	$5,$L52
	sd	$2,0($4)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L116
	move	$2,$0
	.set	macro
	.set	reorder

$L43:
	andi	$2,$4,0x0200
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L25
	andi	$2,$4,0x0004
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L58
	li	$2,2
	.set	macro
	.set	reorder

	ld	$3,32($16)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L61
	sltu	$2,$3,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L66
	li	$2,1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L60
	move	$2,$0
	.set	macro
	.set	reorder

	j	$L116
$L66:
	li	$2,4
	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L62
	li	$2,8
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$3,$2,$L63
	move	$2,$0
	.set	macro
	.set	reorder

	j	$L116
$L60:
	ld	$3,16($16)
	andi	$2,$6,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L25
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L61:
	ld	$3,16($16)
	andi	$2,$6,0xffff
	.set	noreorder
	.set	nomacro
	j	$L25
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L62:
	ld	$3,16($16)
	dsll	$2,$6,32
	dsra	$2,$2,32
	.set	noreorder
	.set	nomacro
	j	$L25
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L63:
	ld	$2,16($16)
	.set	noreorder
	.set	nomacro
	j	$L25
	sd	$6,0($2)
	.set	macro
	.set	reorder

$L58:
	ld	$2,32($16)
	#nop
	dsubu	$4,$2,1
	sltu	$2,$4,16
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L68
	dsll	$2,$4,3
	.set	macro
	.set	reorder

	ld	$2,$L86($2)
	#nop
	j	$2
	.rdata
	.align	3
$L86:
	.dword	$L69
	.dword	$L70
	.dword	$L71
	.dword	$L72
	.dword	$L73
	.dword	$L74
	.dword	$L75
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.dword	$L84
	.text
$L69:
	ld	$4,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,0($4)
	.set	macro
	.set	reorder

$L70:
	ld	$4,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($4)
	ld	$4,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,1($4)
	.set	macro
	.set	reorder

$L71:
	ld	$4,16($16)
	dsra	$2,$6,56
	andi	$2,$2,0x00ff
	sb	$2,0($4)
	ld	$4,16($16)
	dsra	$2,$6,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$4,16($16)
	dsra	$2,$6,40
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,2($4)
	.set	macro
	.set	reorder

$L72:
	ld	$4,16($16)
	dsra	$2,$6,56
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
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,3($4)
	.set	macro
	.set	reorder

$L73:
	ld	$4,16($16)
	dsra	$2,$6,56
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
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,4($4)
	.set	macro
	.set	reorder

$L74:
	ld	$4,16($16)
	dsra	$2,$6,56
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
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,5($4)
	.set	macro
	.set	reorder

$L75:
	ld	$4,16($16)
	dsra	$2,$6,56
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
	.set	noreorder
	.set	nomacro
	j	$L68
	sb	$2,6($4)
	.set	macro
	.set	reorder

$L84:
	ld	$4,16($16)
	dsra	$2,$6,56
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
$L68:
	ld	$2,32($16)
	#nop
	dsubu	$4,$2,9
	sltu	$2,$4,8
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L25
	dsll	$2,$4,3
	.set	macro
	.set	reorder

	ld	$2,$L97($2)
	#nop
	j	$2
	.rdata
	.align	3
$L97:
	.dword	$L88
	.dword	$L89
	.dword	$L90
	.dword	$L91
	.dword	$L92
	.dword	$L93
	.dword	$L94
	.dword	$L95
	.text
$L88:
	ld	$2,16($16)
	dsra	$3,$3,56
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L25
	sb	$3,8($2)
	.set	macro
	.set	reorder

$L89:
	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$2,16($16)
	dsra	$3,$3,48
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L25
	sb	$3,9($2)
	.set	macro
	.set	reorder

$L90:
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
	j	$L25
	sb	$3,10($2)
	.set	macro
	.set	reorder

$L91:
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
	j	$L25
	sb	$3,11($2)
	.set	macro
	.set	reorder

$L92:
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
	j	$L25
	sb	$3,12($2)
	.set	macro
	.set	reorder

$L93:
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
	j	$L25
	sb	$3,13($2)
	.set	macro
	.set	reorder

$L94:
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
	j	$L25
	sb	$3,14($2)
	.set	macro
	.set	reorder

$L95:
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
$L25:
	move	$2,$0
$L116:
	move	$sp,$fp			# sp not trusted here
	ld	$31,2104($sp)
	ld	$fp,2096($sp)
	ld	$16,2088($sp)
	daddu	$sp,$sp,2112
	j	$31
	.end	__builtin_avcall
