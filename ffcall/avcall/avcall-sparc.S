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
	bne L76
	cmp %g4,6
	ld [%i0+8],%g3
	st %g3,[%sp+64]
L2:
	cmp %g4,6
L76:
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
	bne,a L77
	ld [%i0+40],%o0
	ld [%i0+4],%g3
	andcc %g3,16,%g0
	be L8
	sethi %hi(-1614774272),%g3
	st %g3,[%fp-32]
	sethi %hi(16777216),%o7
	st %o7,[%fp-28]
	ld [%i0+16],%g3
	and %g3,4095,%g3
	st %g3,[%fp-24]
	sethi %hi(-2117746688),%g3
	st %g3,[%fp-20]
	st %o7,[%fp-16]
	add %fp,-32,%o7
	iflush %o7
	add %fp,-24,%g3
	iflush %g3
	add %fp,-16,%g3
	iflush %g3
	ld [%i0+40],%o0
	ld [%i0+44],%o1
	ld [%i0+48],%o2
	ld [%i0+52],%o3
	ld [%i0+56],%o4
	ld [%i0+60],%o5
	sethi %hi(L9),%g3
	ld [%i0],%g2
	jmp %o7
	or %g3,%lo(L9),%l7
L9:
	b L78
	mov 0,%i0
L8:
	ld [%i0+40],%o0
L77:
	ld [%i0+44],%o1
	ld [%i0+48],%o2
	ld [%i0+52],%o3
	ld [%i0+56],%o4
	ld [%i0],%g3
	call %g3,0
	ld [%i0+60],%o5
	nop
	ld [%i0+12],%o7
	cmp %o7,16
	bgu L10
	mov %o0,%g4
	sethi %hi(L74),%g3
	or %g3,%lo(L74),%g3
	sll %o7,2,%o7
	ld [%o7+%g3],%g3
	jmp %g3
	nop
L74:
	.word	L69
	.word	L10
	.word	L67
	.word	L67
	.word	L67
	.word	L68
	.word	L68
	.word	L69
	.word	L69
	.word	L69
	.word	L69
	.word	L23
	.word	L23
	.word	L24
	.word	L27
	.word	L69
	.word	L29
L23:
	ld [%i0+8],%g3
	st %g4,[%g3]
	ld [%i0+8],%g3
	b L10
	st %o1,[%g3+4]
L24:
	ld [%i0+4],%g3
	andcc %g3,32,%g0
	bne,a L25
	fdtos %f0,%f0
L25:
	ld [%i0+8],%g3
	b L10
	st %f0,[%g3]
L27:
	ld [%i0+8],%g3
	b L10
	std %f0,[%g3]
L29:
	ld [%i0+4],%g3
	andcc %g3,8,%g0
	be L30
	andcc %g3,1,%g0
	ld [%i0+16],%g3
	cmp %g3,2
	be,a L33
	ld [%i0+8],%o7
	bgu L49
	cmp %g3,1
	be,a L32
	ld [%i0+8],%o7
	b L79
	andcc %g3,3,%g0
L49:
	cmp %g3,4
	be L34
	cmp %g3,8
	be,a L35
	ld [%i0+8],%o7
	b L36
	ld [%i0+16],%g3
L32:
	ldub [%sp-1],%g3
	b L10
	stb %g3,[%o7]
L33:
	lduh [%sp-2],%g3
	b L10
	sth %g3,[%o7]
L34:
	ld [%i0+8],%o7
	ld [%sp-4],%g3
	b L10
	st %g3,[%o7]
L35:
	ld [%sp-8],%g3
	st %g3,[%o7]
	ld [%i0+8],%o7
	ld [%sp-4],%g3
	b L10
	st %g3,[%o7+4]
L36:
	andcc %g3,3,%g0
L79:
	be L37
	sub %sp,%g3,%o7
	ld [%i0+8],%i0
	cmp %g3,4
	bleu L38
	mov %g3,%g1
	and %o7,-4,%o7
L38:
	cmp %g3,0
	be,a L78
	mov 0,%i0
L41:
	ldub [%o7],%g3
	addcc %g1,-1,%g1
	stb %g3,[%i0]
	add %o7,1,%o7
	bne L41
	add %i0,1,%i0
	b L78
	mov 0,%i0
L37:
	cmp %o7,%sp
	bgeu L10
	ld [%i0+8],%i0
L46:
	ld [%o7],%g3
	st %g3,[%i0]
	add %o7,4,%o7
	cmp %o7,%sp
	blu L46
	add %i0,4,%i0
	b L78
	mov 0,%i0
L30:
	be L51
	andcc %g3,2,%g0
	ld [%i0+16],%g3
	cmp %g3,2
	be,a L54
	ld [%i0+8],%o7
	bgu L63
	cmp %g3,1
	be,a L53
	ld [%i0+8],%o7
	b L80
	add %g3,3,%g3
L63:
	cmp %g3,4
	be L55
	cmp %g3,8
	be,a L56
	ld [%i0+8],%o7
	b L57
	ld [%i0+16],%g3
L53:
	ldub [%g4],%g3
	b L10
	stb %g3,[%o7]
L54:
	lduh [%g4],%g3
	b L10
	sth %g3,[%o7]
L55:
	ld [%i0+8],%o7
	ld [%g4],%g3
	b L10
	st %g3,[%o7]
L56:
	ld [%g4],%g3
	st %g3,[%o7]
	ld [%i0+8],%o7
	ld [%g4+4],%g3
	b L10
	st %g3,[%o7+4]
L57:
	add %g3,3,%g3
L80:
	srl %g3,2,%g3
	addcc %g3,-1,%g3
	bneg L10
	sll %g3,2,%g1
	ld [%i0+8],%o7
L81:
	ld [%g1+%g4],%g3
	st %g3,[%o7+%g1]
	addcc %g1,-4,%g1
	bpos,a L81
	ld [%i0+8],%o7
	b L78
	mov 0,%i0
L51:
	be,a L78
	mov 0,%i0
	ld [%i0+16],%g3
	cmp %g3,2
	be,a L82
	ld [%i0+8],%g3
	bgu L72
	cmp %g3,1
	be,a L83
	ld [%i0+8],%g3
	b L78
	mov 0,%i0
L72:
	cmp %g3,4
	be,a L84
	ld [%i0+8],%g3
	b L78
	mov 0,%i0
L67:
	ld [%i0+8],%g3
L83:
	b L10
	stb %g4,[%g3]
L68:
	ld [%i0+8],%g3
L82:
	b L10
	sth %g4,[%g3]
L69:
	ld [%i0+8],%g3
L84:
	st %g4,[%g3]
L10:
	mov 0,%i0
L78:
	ret
	restore
