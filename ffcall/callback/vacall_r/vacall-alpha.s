	.set noreorder
	.set volatile
	.set noat
	.file	1 "vacall-alpha.c"
gcc2_compiled.:
__gnu_compiled_c:
.text
	.align 3
	.globl __vacall_r
	.ent __vacall_r
__vacall_r:
	ldgp $29,0($27)
__vacall_r..ng:
	lda $30,-176($30)
	.frame $30,176,$26,0
	stq $26,0($30)
	.mask 0x4000000,-176
	.prologue 1
	stq $16,128($30)
	stq $17,136($30)
	stq $18,144($30)
	stq $19,152($30)
	stq $20,160($30)
	stq $21,168($30)
	stt $f16,80($30)
	stt $f17,88($30)
	stt $f18,96($30)
	stt $f19,104($30)
	stt $f20,112($30)
	stt $f21,120($30)
	stl $31,16($30)
	addq $30,128,$2
	stq $2,24($30)
	stq $31,32($30)
	stl $31,40($30)
	addq $30,176,$4
	stq $4,72($30)
	ldq $16,8($1)
	ldq $27,0($1)
	addq $30,16,$17
	jsr $26,($27),0
	ldgp $29,0($26)
	ldl $2,40($30)
	zapnot $2,15,$3
	cmpule $3,15,$2
	beq $2,$34
	lda $2,$62
	s4addq $3,$2,$2
	ldl $2,0($2)
	addq $2,$29,$3
	jmp $31,($3),$49
.data
	.quad 0
	.align 3
	.align 2
$62:
	.gprel32 $34
	.gprel32 $36
	.gprel32 $37
	.gprel32 $38
	.gprel32 $39
	.gprel32 $40
	.gprel32 $41
	.gprel32 $42
	.gprel32 $49
	.gprel32 $49
	.gprel32 $49
	.gprel32 $49
	.gprel32 $47
	.gprel32 $48
	.gprel32 $49
	.gprel32 $50
.text
	.align 4
$36:
$37:
	ldl $2,56($30)
	insbl $2,7,$2
	sra $2,56,$0
	br $31,$34
	.align 4
$38:
	ldl $2,56($30)
	extbl $2,0,$2
	zapnot $2,1,$0
	br $31,$34
	.align 4
$39:
	ldl $2,56($30)
	inswl $2,6,$2
	sra $2,48,$0
	br $31,$34
	.align 4
$40:
	ldl $2,56($30)
	extwl $2,0,$2
	zapnot $2,3,$0
	br $31,$34
	.align 4
$41:
	ldl $0,56($30)
	br $31,$34
	.align 4
$42:
	ldl $2,56($30)
	zapnot $2,15,$0
	br $31,$34
	.align 4
$47:
	lds $f0,56($30)
	br $31,$34
	.align 4
$48:
	ldt $f0,56($30)
	br $31,$34
	.align 4
$49:
	ldq $0,56($30)
	br $31,$34
	.align 4
$50:
	ldl $2,16($30)
	blbc $2,$51
	ldq $0,32($30)
	br $31,$34
	.align 4
$51:
	srl $2,9,$2
	blbc $2,$34
	ldq $2,48($30)
	subq $2,1,$3
	cmpule $3,15,$2
	beq $2,$34
	lda $2,$61
	s4addq $3,$2,$2
	ldl $2,0($2)
	addq $2,$29,$3
	jmp $31,($3),$34
.data
	.align 3
	.align 2
$61:
	.gprel32 $55
	.gprel32 $56
	.gprel32 $34
	.gprel32 $57
	.gprel32 $34
	.gprel32 $34
	.gprel32 $34
	.gprel32 $58
	.gprel32 $34
	.gprel32 $34
	.gprel32 $34
	.gprel32 $34
	.gprel32 $34
	.gprel32 $34
	.gprel32 $34
	.gprel32 $59
.text
	.align 4
$55:
	ldq $3,32($30)
	ldq_u $2,0($3)
	extbl $2,$3,$2
	zapnot $2,1,$0
	br $31,$34
	.align 4
$56:
	ldq $2,32($30)
	bic $2,6,$3
	ldq $3,0($3)
	bic $2,1,$2
	extwl $3,$2,$3
	zapnot $3,3,$0
	br $31,$34
	.align 4
$57:
	ldq $2,32($30)
	ldl $2,0($2)
	zapnot $2,15,$0
	br $31,$34
	.align 4
$58:
	ldq $2,32($30)
	ldq $0,0($2)
	br $31,$34
	.align 4
$59:
	ldq $2,32($30)
	ldq $0,0($2)
	ldq $1,8($2)
$34:
	ldq $26,0($30)
	addq $30,176,$30
	ret $31,($26),1
	.end __vacall_r
