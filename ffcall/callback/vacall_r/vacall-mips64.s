	.file	1 "vacall-mips64.c"
	.set	nobopt

 # GNU C 2.6.3 [AL 1.1, MM 40] Silicon Graphics Mips compiled by GNU C

 # Cc1 defaults:

 # Cc1 arguments (-G value = 8, Cpu = 4000, ISA = 3):
 # -mfp64 -mgp64 -quiet -dumpbase -mips3 -mlong64 -O2 -fomit-frame-pointer -o

	.text
	.align	2
	.globl	__vacall_r

	.text
	.ent	__vacall_r
__vacall_r:
	.frame	$sp,216,$31		# vars= 168, regs= 2/0, args= 32, extra= 0
	.mask	0x80010000,-8
	.fmask	0x00000000,0
	dsubu	$sp,$sp,216
	sd	$31,144($sp)
	sd	$16,136($sp)
	dsubu	$sp,$sp,64
 #APP
	
 #NO_APP
	daddu	$24,$sp,216
	daddu	$12,$sp,280
	sd	$4,216($sp)
	sd	$5,224($sp)
	sd	$6,232($sp)
	sd	$7,240($sp)
	sd	$8,248($sp)
	sd	$9,256($sp)
	sd	$10,264($sp)
	sd	$11,272($sp)
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
	sd	$24,40($sp)
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
	#nop
	sltu	$12,$13,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L2
	dsll	$12,$13,32
	.set	macro
	.set	reorder

	dsrl	$12,$12,32
	dsll	$12,$12,3
	ld	$12,$L70($12)
	#nop
	j	$12
	.rdata
	.align	3
$L70:
	.dword	$L2
	.dword	$L6
	.dword	$L5
	.dword	$L6
	.dword	$L7
	.dword	$L8
	.dword	$L9
	.dword	$L10
	.dword	$L17
	.dword	$L17
	.dword	$L17
	.dword	$L17
	.dword	$L15
	.dword	$L16
	.dword	$L17
	.dword	$L18
	.text
$L5:
	lb	$2,72($sp)
	j	$L2
$L6:
	lbu	$2,72($sp)
	j	$L2
$L7:
	lh	$2,72($sp)
	j	$L2
$L8:
	lhu	$2,72($sp)
	j	$L2
$L9:
	lw	$2,72($sp)
	j	$L2
$L10:
	lwu	$2,72($sp)
	j	$L2
$L15:
	l.s	$f0,72($sp)
	j	$L2
$L16:
	l.d	$f0,72($sp)
	j	$L2
$L17:
	ld	$2,72($sp)
	j	$L2
$L18:
	lw	$13,32($sp)
	#nop
	andi	$12,$13,0x0001
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L19
	andi	$12,$13,0x0200
	.set	macro
	.set	reorder

	ld	$2,48($sp)
	j	$L2
$L19:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L2
	andi	$12,$13,0x0004
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L22
	li	$12,2
	.set	macro
	.set	reorder

	ld	$13,64($sp)
	#nop
	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L25
	sltu	$12,$13,3
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L30
	li	$12,1
	.set	macro
	.set	reorder

	beq	$13,$12,$L24
	j	$L2
$L30:
	li	$12,4
	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L26
	li	$12,8
	.set	macro
	.set	reorder

	beq	$13,$12,$L27
	j	$L2
$L24:
	ld	$12,48($sp)
	#nop
	lbu	$2,0($12)
	j	$L2
$L25:
	ld	$12,48($sp)
	#nop
	lhu	$2,0($12)
	j	$L2
$L26:
	ld	$12,48($sp)
	#nop
	lwu	$2,0($12)
	j	$L2
$L27:
	ld	$12,48($sp)
	#nop
	ld	$2,0($12)
	j	$L2
$L22:
	ld	$12,64($sp)
	#nop
	dsubu	$13,$12,1
	sltu	$12,$13,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L32
	dsll	$12,$13,3
	.set	macro
	.set	reorder

	ld	$12,$L50($12)
	#nop
	j	$12
	.rdata
	.align	3
$L50:
	.dword	$L33
	.dword	$L34
	.dword	$L35
	.dword	$L36
	.dword	$L37
	.dword	$L38
	.dword	$L39
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.dword	$L48
	.text
$L33:
	ld	$12,48($sp)
	#nop
	lbu	$12,0($12)
	.set	noreorder
	.set	nomacro
	j	$L32
	dsll	$2,$12,56
	.set	macro
	.set	reorder

$L34:
	ld	$12,48($sp)
	#nop
	lbu	$13,0($12)
	lbu	$12,1($12)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	j	$L32
	or	$2,$13,$12
	.set	macro
	.set	reorder

$L35:
	ld	$12,48($sp)
	#nop
	lbu	$14,0($12)
	lbu	$13,1($12)
	lbu	$12,2($12)
	dsll	$14,$14,56
	dsll	$13,$13,48
	or	$14,$14,$13
	.set	noreorder
	.set	nomacro
	j	$L72
	dsll	$12,$12,40
	.set	macro
	.set	reorder

$L36:
	ld	$15,48($sp)
	#nop
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
	j	$L32
	or	$2,$12,$13
	.set	macro
	.set	reorder

$L37:
	ld	$15,48($sp)
	#nop
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
	.set	noreorder
	.set	nomacro
	j	$L72
	dsll	$14,$14,24
	.set	macro
	.set	reorder

$L38:
	ld	$16,48($sp)
	#nop
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
	.set	noreorder
	.set	nomacro
	j	$L72
	dsll	$14,$14,16
	.set	macro
	.set	reorder

$L39:
	ld	$15,48($sp)
	#nop
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
	.set	noreorder
	.set	nomacro
	j	$L72
	dsll	$14,$14,8
	.set	macro
	.set	reorder

$L48:
	ld	$16,48($sp)
	#nop
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
$L72:
	or	$2,$12,$14
$L32:
	ld	$12,64($sp)
	#nop
	dsubu	$13,$12,9
	sltu	$12,$13,8
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L51
	dsll	$12,$13,3
	.set	macro
	.set	reorder

	ld	$12,$L61($12)
	#nop
	j	$12
	.rdata
	.align	3
$L61:
	.dword	$L52
	.dword	$L53
	.dword	$L54
	.dword	$L55
	.dword	$L56
	.dword	$L57
	.dword	$L58
	.dword	$L59
	.text
$L52:
	ld	$12,48($sp)
	#nop
	lbu	$12,8($12)
	.set	noreorder
	.set	nomacro
	j	$L51
	dsll	$3,$12,56
	.set	macro
	.set	reorder

$L53:
	ld	$12,48($sp)
	#nop
	lbu	$13,8($12)
	lbu	$12,9($12)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	j	$L51
	or	$3,$13,$12
	.set	macro
	.set	reorder

$L54:
	ld	$12,48($sp)
	#nop
	lbu	$14,8($12)
	lbu	$13,9($12)
	lbu	$12,10($12)
	dsll	$14,$14,56
	dsll	$13,$13,48
	or	$14,$14,$13
	.set	noreorder
	.set	nomacro
	j	$L73
	dsll	$12,$12,40
	.set	macro
	.set	reorder

$L55:
	ld	$15,48($sp)
	#nop
	lbu	$12,8($15)
	lbu	$13,9($15)
	lbu	$14,10($15)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	lbu	$13,11($15)
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$13,$13,32
	.set	noreorder
	.set	nomacro
	j	$L51
	or	$3,$12,$13
	.set	macro
	.set	reorder

$L56:
	ld	$15,48($sp)
	#nop
	lbu	$12,8($15)
	lbu	$13,9($15)
	lbu	$14,10($15)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,11($15)
	or	$12,$12,$14
	lbu	$14,12($15)
	dsll	$13,$13,32
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L73
	dsll	$14,$14,24
	.set	macro
	.set	reorder

$L57:
	ld	$16,48($sp)
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
	.set	noreorder
	.set	nomacro
	j	$L73
	dsll	$14,$14,16
	.set	macro
	.set	reorder

$L58:
	ld	$15,48($sp)
	#nop
	lbu	$12,8($15)
	lbu	$13,9($15)
	lbu	$14,10($15)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,11($15)
	or	$12,$12,$14
	lbu	$14,12($15)
	dsll	$13,$13,32
	or	$12,$12,$13
	dsll	$14,$14,24
	lbu	$13,13($15)
	or	$12,$12,$14
	lbu	$14,14($15)
	dsll	$13,$13,16
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L73
	dsll	$14,$14,8
	.set	macro
	.set	reorder

$L59:
	ld	$16,48($sp)
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
	lbu	$13,14($16)
	or	$12,$12,$14
	lbu	$14,15($16)
	dsll	$13,$13,8
	or	$12,$12,$13
$L73:
	or	$3,$12,$14
$L51:
	lw	$12,32($sp)
	#nop
	andi	$12,$12,0x1000
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L62
	li	$12,4
	.set	macro
	.set	reorder

	ld	$13,64($sp)
	#nop
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L63
	li	$12,8
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	#nop
	l.s	$f0,0($12)
	j	$L62
$L63:
	bne	$13,$12,$L62
	ld	$12,48($sp)
	#nop
	l.s	$f0,0($12)
	l.s	$f2,4($12)
$L62:
	lw	$12,32($sp)
	#nop
	andi	$12,$12,0x2000
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L2
	li	$12,8
	.set	macro
	.set	reorder

	ld	$13,64($sp)
	#nop
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L67
	li	$12,16
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	#nop
	l.d	$f0,0($12)
	j	$L2
$L67:
	bne	$13,$12,$L2
	ld	$12,48($sp)
	#nop
	l.d	$f0,0($12)
	l.d	$f2,8($12)
$L2:
 #APP
	
 #NO_APP
	daddu	$sp,$sp,64
	ld	$31,144($sp)
	ld	$16,136($sp)
	daddu	$sp,$sp,216
	j	$31
	.end	__vacall_r
