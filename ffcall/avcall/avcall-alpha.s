	.set noreorder
	.set volatile
	.set noat
	.file	1 "avcall-alpha.c"
gcc2_compiled.:
__gnu_compiled_c:
.text
	.align 3
	.globl __builtin_avcall
	.ent __builtin_avcall
__builtin_avcall:
	ldgp $29,0($27)
__builtin_avcall..ng:
	lda $30,-16($30)
	.frame $30,16,$26,0
	stq $26,0($30)
	stq $9,8($30)
	.mask 0x4000200,-16
	.prologue 1
	bis $16,$16,$9
	lda $30,-2048($30)
	bis $30,$30,$4
	ldq $1,40($9)
	subq $1,48,$1
	subq $1,$9,$1
	srl $1,3,$1
	addl $1,$31,$3
	bis $31,6,$0
	cmplt $0,$3,$1
	beq $1,$35
	.align 5
$37:
	s8addq $0,0,$1
	addq $1,$4,$2
	addq $9,$1,$1
	ldt $f1,48($1)
	stt $f1,-48($2)
	addq $0,1,$0
	cmplt $0,$3,$1
	bne $1,$37
$35:
	ldq $16,48($9)
	ldt $f16,48($9)
	ldq $17,56($9)
	ldt $f17,56($9)
	ldq $18,64($9)
	ldt $f18,64($9)
	ldq $19,72($9)
	ldt $f19,72($9)
	ldq $20,80($9)
	ldt $f20,80($9)
	ldq $21,88($9)
	ldt $f21,88($9)
	ldq $27,0($9)
	jsr $26,($27),0
	ldgp $29,0($26)
	bis $1,$1,$3
	lda $30,2048($30)
	ldl $1,24($9)
	zapnot $1,15,$2
	cmpule $2,16,$1
	beq $1,$39
	lda $1,$81
	s4addq $2,$1,$1
	ldl $1,0($1)
	addq $1,$29,$2
	jmp $31,($2),$76
.data
	.quad 0
	.align 3
	.align 2
$81:
	.gprel32 $76
	.gprel32 $39
	.gprel32 $73
	.gprel32 $73
	.gprel32 $73
	.gprel32 $74
	.gprel32 $74
	.gprel32 $75
	.gprel32 $75
	.gprel32 $76
	.gprel32 $76
	.gprel32 $76
	.gprel32 $76
	.gprel32 $53
	.gprel32 $54
	.gprel32 $76
	.gprel32 $56
.text
	.align 4
$53:
	ldq $1,16($9)
	cvtts $f0,$f0
	sts $f0,0($1)
	br $31,$39
	.align 4
$54:
	ldq $1,16($9)
	stt $f0,0($1)
	br $31,$39
	.align 4
$56:
	ldl $1,8($9)
	blbc $1,$57
	ldq $2,32($9)
	subq $2,2,$1
	beq $1,$60
	cmpule $2,2,$1
	beq $1,$69
	subq $2,1,$1
	beq $1,$59
	br $31,$63
	.align 4
$69:
	subq $2,4,$1
	beq $1,$61
	subq $2,8,$1
	beq $1,$62
	br $31,$63
	.align 4
$59:
	ldq $1,16($9)
	ldq_u $2,0($0)
	extbl $2,$0,$2
	ldq_u $3,0($1)
	mskbl $3,$1,$3
	insbl $2,$1,$2
	bis $2,$3,$2
	stq_u $2,0($1)
	br $31,$39
	.align 4
$60:
	ldq $3,16($9)
	bic $0,6,$1
	ldq $2,0($1)
	bic $0,1,$1
	extwl $2,$1,$2
	bic $3,6,$4
	ldq $1,0($4)
	bic $3,1,$3
	mskwl $1,$3,$1
	inswl $2,$3,$2
	bis $1,$2,$1
	stq $1,0($4)
	br $31,$39
	.align 4
$61:
	ldq $1,16($9)
	lds $f1,0($0)
	sts $f1,0($1)
	br $31,$39
	.align 4
$62:
	ldq $1,16($9)
	ldt $f1,0($0)
	stt $f1,0($1)
	br $31,$39
	.align 4
$63:
	ldq $24,32($9)
	addq $24,7,$24
	srl $24,3,$24
	subl $24,1,$3
	blt $3,$39
	.align 5
$66:
	s8addq $3,0,$1
	ldq $2,16($9)
	addq $2,$1,$2
	addq $0,$1,$1
	ldt $f1,0($1)
	stt $f1,0($2)
	subl $3,1,$3
	cmplt $3,0,$1
	beq $1,$66
	br $31,$39
	.align 4
$57:
	ldl $1,8($9)
	srl $1,8,$1
	blbc $1,$39
	ldq $1,32($9)
	subq $1,1,$2
	cmpule $2,15,$1
	beq $1,$39
	lda $1,$79
	s4addq $2,$1,$1
	ldl $1,0($1)
	addq $1,$29,$2
	jmp $31,($2),$39
.data
	.align 3
	.align 2
$79:
	.gprel32 $73
	.gprel32 $74
	.gprel32 $39
	.gprel32 $75
	.gprel32 $39
	.gprel32 $39
	.gprel32 $39
	.gprel32 $76
	.gprel32 $39
	.gprel32 $39
	.gprel32 $39
	.gprel32 $39
	.gprel32 $39
	.gprel32 $39
	.gprel32 $39
	.gprel32 $77
.text
	.align 4
$73:
	ldq $3,16($9)
	ldq_u $2,0($3)
	mskbl $2,$3,$2
	insbl $0,$3,$1
	bis $1,$2,$1
	stq_u $1,0($3)
	br $31,$39
	.align 4
$74:
	ldq $1,16($9)
	bic $1,6,$3
	ldq $2,0($3)
	bic $1,1,$1
	mskwl $2,$1,$2
	inswl $0,$1,$1
	bis $2,$1,$2
	stq $2,0($3)
	br $31,$39
	.align 4
$75:
	ldq $1,16($9)
	stl $0,0($1)
	br $31,$39
	.align 4
$76:
	ldq $1,16($9)
	stq $0,0($1)
	br $31,$39
	.align 4
$77:
	ldq $1,16($9)
	stq $0,0($1)
	ldq $1,16($9)
	stq $3,8($1)
$39:
	bis $31,$31,$0
	ldq $26,0($30)
	ldq $9,8($30)
	addq $30,16,$30
	ret $31,($26),1
	.end __builtin_avcall
