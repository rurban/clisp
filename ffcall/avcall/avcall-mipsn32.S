#include "asmmips.h"
	
	.set	nobopt
	.option pic2
	.section	.text
	.text
	.align	2
	.globl	__builtin_avcall
	DECLARE_FUNCTION(__builtin_avcall)
	.ent	__builtin_avcall
__builtin_avcall:
.LFB1:
	.frame	$fp,2096,$31		
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
	beq	$2,$0,.L120
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 
	lwc1 $f12,36($16)
 
	andi	$2,$3,0x2
.L120:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L121
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 
	lwc1 $f13,40($16)
 
	andi	$2,$3,0x4
.L121:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L122
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 
	lwc1 $f14,44($16)
 
	andi	$2,$3,0x8
.L122:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L123
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 
	lwc1 $f15,48($16)
 
	andi	$2,$3,0x10
.L123:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L124
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 
	lwc1 $f16,52($16)
 
	andi	$2,$3,0x20
.L124:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L125
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 
	lwc1 $f17,56($16)
 
	andi	$2,$3,0x40
.L125:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L126
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 
	lwc1 $f18,60($16)
 
	andi	$2,$3,0x80
.L126:
	.set	noreorder
	.set	nomacro
	beql	$2,$0,.L127
	lw	$3,32($16)
	.set	macro
	.set	reorder

 
	lwc1 $f19,64($16)
 
.L2:
	
	lw	$3,32($16)
.L127:
	.set	noreorder
	.set	nomacro
	beq	$3,$0,.L11
	andi	$2,$3,0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L128
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 
	ldc1 $f12,72($16)
 
	andi	$2,$3,0x2
.L128:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L129
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 
	ldc1 $f13,80($16)
 
	andi	$2,$3,0x4
.L129:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L130
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 
	ldc1 $f14,88($16)
 
	andi	$2,$3,0x8
.L130:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L131
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 
	ldc1 $f15,96($16)
 
	andi	$2,$3,0x10
.L131:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L132
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 
	ldc1 $f16,104($16)
 
	andi	$2,$3,0x20
.L132:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L133
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 
	ldc1 $f17,112($16)
 
	andi	$2,$3,0x40
.L133:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L134
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 
	ldc1 $f18,120($16)
 
	andi	$2,$3,0x80
.L134:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L135
	dli	$7,0x8		
	.set	macro
	.set	reorder

 
	ldc1 $f19,128($16)
 
.L11:
	dli	$7,0x8		
.L135:
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
 
	ld $4,72($16)
	ld $5,80($16)
	ld $6,88($16)
	ld $7,96($16)
	ld $8,104($16)
	ld $9,112($16)
	ld $10,120($16)
	ld $11,128($16)
 
	lw	$25,0($16)
	
	jal	$31,$25
	lw	$4,12($16)
	li	$3,1			
	.set	noreorder
	.set	nomacro
	beq	$4,$3,.L26
	move	$7,$2
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$0,.L115
	li	$2,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L116
	li	$2,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L116
	li	$2,4			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L116
	li	$2,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L117
	li	$2,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L117
	li	$2,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L118
	li	$2,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L118
	li	$2,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L118
	li	$2,10			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L118
	move	$5,$4
	.set	macro
	.set	reorder

	li	$2,11			
	.set	noreorder
	.set	nomacro
	beq	$5,$2,.L115
	li	$2,12			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$2,.L115
	li	$2,13			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$5,$2,.L51
	li	$2,14			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	b	.L26
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

.L51:
	.set	noreorder
	.set	nomacro
	bne	$5,$2,.L53
	li	$2,15			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	b	.L119
	move	$2,$0
	.set	macro
	.set	reorder

.L53:
	.set	noreorder
	.set	nomacro
	beq	$5,$2,.L118
	li	$2,16			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L119
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$4,4($16)
	
	andi	$2,$4,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L58
	li	$2,1			
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L59
	li	$2,2			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lbu	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$4,0($2)
	.set	macro
	.set	reorder

.L59:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L61
	li	$2,4			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lhu	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L26
	sh	$4,0($2)
	.set	macro
	.set	reorder

.L61:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L63
	li	$2,8			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lw	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L26
	sw	$4,0($2)
	.set	macro
	.set	reorder

.L63:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L65
	addu	$2,$3,7
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lw	$4,4($3)
	.set	noreorder
	.set	nomacro
	b	.L26
	sw	$4,0($2)
	.set	macro
	.set	reorder

.L65:
	srl	$5,$2,3
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,.L26
	sll	$2,$5,3
	.set	macro
	.set	reorder

	dsll	$3,$7,32
	dsra	$3,$3,32
	addu	$6,$2,$3
.L69:
	ld	$4,0($6)
	addu	$6,$6,-8
	lw	$3,8($16)
	sll	$2,$5,3
	addu	$5,$5,-1
	addu	$2,$2,$3
	.set	noreorder
	.set	nomacro
	bgez	$5,.L69
	sd	$4,0($2)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L119
	move	$2,$0
	.set	macro
	.set	reorder

.L58:
	andi	$2,$4,0x200
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L26
	andi	$2,$4,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L73
	li	$2,1			
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L74
	li	$2,2			
	.set	macro
	.set	reorder

.L116:
	lw	$3,8($16)
	andi	$2,$7,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$2,0($3)
	.set	macro
	.set	reorder

.L74:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L76
	li	$2,4			
	.set	macro
	.set	reorder

.L117:
	lw	$3,8($16)
	andi	$2,$7,0xffff
	.set	noreorder
	.set	nomacro
	b	.L26
	sh	$2,0($3)
	.set	macro
	.set	reorder

.L76:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L78
	li	$2,8			
	.set	macro
	.set	reorder

.L118:
	lw	$3,8($16)
	dsll	$2,$7,32
	dsra	$2,$2,32
	.set	noreorder
	.set	nomacro
	b	.L26
	sw	$2,0($3)
	.set	macro
	.set	reorder

.L78:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L119
	move	$2,$0
	.set	macro
	.set	reorder

.L115:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	b	.L26
	sd	$7,0($2)
	.set	macro
	.set	reorder

.L73:
	lw	$4,16($16)
	
	addu	$2,$4,-1
	sltu	$2,$2,16
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L26
	li	$2,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L83
	li	$2,2			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$2,0($3)
	.set	macro
	.set	reorder

.L83:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L85
	li	$2,3			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$7,56
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$3,$7,48
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$3,1($5)
	.set	macro
	.set	reorder

.L85:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L87
	li	$2,4			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$7,56
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$3,$7,48
	sb	$3,1($5)
	lw	$4,8($16)
	dsra	$2,$7,40
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$2,2($4)
	.set	macro
	.set	reorder

.L87:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L89
	li	$2,5			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$7,56
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$3,$7,48
	sb	$3,1($5)
	lw	$4,8($16)
	dsra	$2,$7,40
	sb	$2,2($4)
	lw	$5,8($16)
	dsra	$3,$7,32
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$3,3($5)
	.set	macro
	.set	reorder

.L89:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L91
	li	$2,6			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$7,56
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$3,$7,48
	sb	$3,1($5)
	lw	$4,8($16)
	dsra	$2,$7,40
	sb	$2,2($4)
	lw	$5,8($16)
	dsra	$3,$7,32
	sb	$3,3($5)
	lw	$4,8($16)
	dsrl	$2,$7,24
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$2,4($4)
	.set	macro
	.set	reorder

.L91:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L93
	li	$2,7			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$7,56
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$3,$7,48
	sb	$3,1($5)
	lw	$4,8($16)
	dsra	$2,$7,40
	sb	$2,2($4)
	lw	$5,8($16)
	dsra	$3,$7,32
	sb	$3,3($5)
	lw	$4,8($16)
	dsrl	$2,$7,24
	sb	$2,4($4)
	lw	$5,8($16)
	dsrl	$3,$7,16
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$3,5($5)
	.set	macro
	.set	reorder

.L93:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L95
	addu	$2,$4,-8
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$7,56
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$3,$7,48
	sb	$3,1($5)
	lw	$4,8($16)
	dsra	$2,$7,40
	sb	$2,2($4)
	lw	$5,8($16)
	dsra	$3,$7,32
	sb	$3,3($5)
	lw	$4,8($16)
	dsrl	$2,$7,24
	sb	$2,4($4)
	lw	$5,8($16)
	dsrl	$3,$7,16
	sb	$3,5($5)
	lw	$4,8($16)
	dsrl	$2,$7,8
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$2,6($4)
	.set	macro
	.set	reorder

.L95:
	sltu	$2,$2,9
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L26
	dsra	$2,$7,56
	.set	macro
	.set	reorder

	lw	$5,8($16)
	
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
	lw	$4,16($16)
	li	$2,8			
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L26
	li	$2,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L100
	li	$2,10			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsra	$3,$3,56
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$3,8($2)
	.set	macro
	.set	reorder

.L100:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L102
	li	$2,11			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$3,$3,48
	.set	noreorder
	.set	nomacro
	b	.L26
	sb	$3,9($5)
	.set	macro
	.set	reorder

.L102:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L104
	li	$2,12			
	.set	macro
	.set	reorder

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
	b	.L26
	sb	$3,10($2)
	.set	macro
	.set	reorder

.L104:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L106
	li	$2,13			
	.set	macro
	.set	reorder

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
	b	.L26
	sb	$3,11($4)
	.set	macro
	.set	reorder

.L106:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L108
	li	$2,14			
	.set	macro
	.set	reorder

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
	b	.L26
	sb	$3,12($2)
	.set	macro
	.set	reorder

.L108:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L110
	li	$2,15			
	.set	macro
	.set	reorder

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
	b	.L26
	sb	$3,13($4)
	.set	macro
	.set	reorder

.L110:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L112
	li	$2,16			
	.set	macro
	.set	reorder

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
	b	.L26
	sb	$3,14($2)
	.set	macro
	.set	reorder

.L112:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L119
	move	$2,$0
	.set	macro
	.set	reorder

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
.L26:
	move	$2,$0
.L119:
	move	$sp,$fp
	ld	$31,2088($sp)
	ld	$fp,2080($sp)
	ld	$28,2072($sp)
	ld	$16,2064($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,2096
	.set	macro
	.set	reorder

.LFE1:
	.end	__builtin_avcall
