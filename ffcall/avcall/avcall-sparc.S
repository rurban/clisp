#ifdef ASM_UNDERSCORE
#ifdef __STDC__
#define C(entrypoint) _##entrypoint
#else
#define C(entrypoint) _/**/entrypoint
#endif
#else
#define C(entrypoint) entrypoint
#endif
gcc2_compiled.:
C(__gnu_compiled_c:)
.text
	.align 4
	.global C(__builtin_avcall)
	.proc	04
C(__builtin_avcall:)
	!#PROLOGUE# 0
	save %sp,-1152,%sp
	!#PROLOGUE# 1
	ld [%i0+12],%g3
	cmp %g3,16
	ld [%i0+20],%g3
	add %sp,68,%l0
	add %g3,-40,%g3
	sub %g3,%i0,%g3
	bne L2
	sra %g3,2,%g4
	ld [%i0+4],%g3
	andcc %g3,8,%g0
	bne L86
	cmp %g4,6
	ld [%i0+8],%g3
	st %g3,[%sp+64]
L2:
	cmp %g4,6
L86:
	ble L4
	sll %g4,2,%g3
	add %i0,24,%o7
	mov 24,%g1
	add %g3,%i0,%g4
L6:
	ld [%o7+40],%g3
	st %g3,[%g1+%l0]
	add %o7,4,%o7
	cmp %o7,%g4
	bl L6
	add %g1,4,%g1
L4:
	ld [%i0+12],%g3
	cmp %g3,16
	bne,a L87
	ld [%i0+40],%o0
	ld [%i0+4],%g3
	andcc %g3,16,%g0
	be L8
	sethi %hi(-1614774272),%g3
	st %g3,[%fp-32]
	sethi %hi(16777216),%g3
	st %g3,[%fp-28]
	ld [%i0+16],%g3
	add %fp,-32,%o7
	and %g3,4095,%g3
	st %g3,[%fp-24]
	sethi %hi(-1341120512),%g3
	st %g3,[%fp-20]
	sethi %hi(-2117607416),%g3
	or %g3,%lo(-2117607416),%g3
	st %g3,[%fp-16]
	sethi %hi(-2115502080),%g3
	st %g3,[%fp-12]
	iflush %o7
	add %fp,-24,%g3
	iflush %g3
	add %fp,-16,%g3
	iflush %g3
	add %fp,-12,%g3
	iflush %g3
	ld [%i0+40],%o0
	ld [%i0+44],%o1
	ld [%i0+48],%o2
	ld [%i0+52],%o3
	ld [%i0+56],%o4
	ld [%i0+60],%o5
	jmp %o7
	ld [%i0],%g2
L8:
	ld [%i0+40],%o0
L87:
	ld [%i0+44],%o1
	ld [%i0+48],%o2
	ld [%i0+52],%o3
	ld [%i0+56],%o4
	ld [%i0],%g3
	call %g3,0
	ld [%i0+60],%o5
	nop
	ld [%i0+12],%g3
	cmp %g3,1
	be L10
	mov %o0,%g4
	cmp %g3,0
	be L83
	cmp %g3,2
	be L84
	cmp %g3,3
	be L84
	cmp %g3,4
	be L84
	cmp %g3,5
	be L85
	cmp %g3,6
	be L85
	cmp %g3,7
	be L83
	cmp %g3,8
	be L83
	cmp %g3,9
	be L83
	cmp %g3,10
	be,a L88
	ld [%i0+8],%g3
	ld [%i0+12],%o7
	add %o7,-11,%g3
	cmp %g3,1
	bgu L31
	cmp %o7,13
	ld [%i0+8],%g3
	st %g4,[%g3]
	ld [%i0+8],%g3
	b L10
	st %o1,[%g3+4]
L31:
	bne L33
	cmp %o7,14
	ld [%i0+4],%g3
	andcc %g3,32,%g0
	bne,a L34
	fdtos %f0,%f0
L34:
	ld [%i0+8],%g3
	b L10
	st %f0,[%g3]
L33:
	bne L37
	cmp %o7,15
	ld [%i0+8],%g3
	b L10
	std %f0,[%g3]
L37:
	be L83
	cmp %o7,16
	bne,a L89
	mov 0,%i0
	ld [%i0+4],%g3
	andcc %g3,8,%g0
	be L42
	andcc %g3,1,%g0
	ld [%i0+16],%g3
	cmp %g3,1
	bne L43
	cmp %g3,2
	ld [%i0+8],%o7
	ldub [%sp-1],%g3
	b L10
	stb %g3,[%o7]
L43:
	bne L45
	cmp %g3,4
	ld [%i0+8],%o7
	lduh [%sp-2],%g3
	b L10
	sth %g3,[%o7]
L45:
	bne L47
	cmp %g3,8
	ld [%i0+8],%o7
	ld [%sp-4],%g3
	b L10
	st %g3,[%o7]
L47:
	bne L49
	andcc %g3,3,%g0
	ld [%i0+8],%o7
	ld [%sp-8],%g3
	st %g3,[%o7]
	ld [%i0+8],%o7
	ld [%sp-4],%g3
	b L10
	st %g3,[%o7+4]
L49:
	be L51
	sub %sp,%g3,%o7
	ld [%i0+8],%i0
	cmp %g3,4
	bleu L52
	mov %g3,%g1
	and %o7,-4,%o7
L52:
	cmp %g3,0
	be,a L89
	mov 0,%i0
L55:
	ldub [%o7],%g3
	addcc %g1,-1,%g1
	stb %g3,[%i0]
	add %o7,1,%o7
	bne L55
	add %i0,1,%i0
	b L89
	mov 0,%i0
L51:
	cmp %o7,%sp
	bgeu L10
	ld [%i0+8],%i0
L60:
	ld [%o7],%g3
	st %g3,[%i0]
	add %o7,4,%o7
	cmp %o7,%sp
	blu L60
	add %i0,4,%i0
	b L89
	mov 0,%i0
L42:
	be L63
	andcc %g3,2,%g0
	ld [%i0+16],%g3
	cmp %g3,1
	bne L64
	cmp %g3,2
	ld [%i0+8],%o7
	ldub [%g4],%g3
	b L10
	stb %g3,[%o7]
L64:
	bne L66
	cmp %g3,4
	ld [%i0+8],%o7
	lduh [%g4],%g3
	b L10
	sth %g3,[%o7]
L66:
	bne L68
	cmp %g3,8
	ld [%i0+8],%o7
	ld [%g4],%g3
	b L10
	st %g3,[%o7]
L68:
	bne L70
	add %g3,3,%g3
	ld [%i0+8],%o7
	ld [%g4],%g3
	st %g3,[%o7]
	ld [%i0+8],%o7
	ld [%g4+4],%g3
	b L10
	st %g3,[%o7+4]
L70:
	srl %g3,2,%g3
	addcc %g3,-1,%g3
	bneg L10
	sll %g3,2,%g1
	ld [%i0+8],%o7
L90:
	ld [%g1+%g4],%g3
	st %g3,[%o7+%g1]
	addcc %g1,-4,%g1
	bpos,a L90
	ld [%i0+8],%o7
	b L89
	mov 0,%i0
L63:
	be,a L89
	mov 0,%i0
	ld [%i0+16],%g3
	cmp %g3,1
	bne L78
	cmp %g3,2
L84:
	ld [%i0+8],%g3
	b L10
	stb %g4,[%g3]
L78:
	bne L80
	cmp %g3,4
L85:
	ld [%i0+8],%g3
	b L10
	sth %g4,[%g3]
L80:
	bne,a L89
	mov 0,%i0
L83:
	ld [%i0+8],%g3
L88:
	st %g4,[%g3]
L10:
	mov 0,%i0
L89:
	ret
	restore
