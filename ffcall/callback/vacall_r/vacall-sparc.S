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
	.global C(__vacall_r)
	.proc	020
C(__vacall_r:)
	!#PROLOGUE# 0
	save %sp,-144,%sp
	!#PROLOGUE# 1
	st %i0,[%fp+68]
	st %i1,[%fp+72]
	st %i2,[%fp+76]
	st %i3,[%fp+80]
	st %i4,[%fp+84]
	st %i5,[%fp+88]
	st %g0,[%fp-48]
	add %fp,68,%o0
	st %o0,[%fp-44]
	st %g0,[%fp-40]
	ld [%fp+64],%o0
	st %g0,[%fp-36]
	st %o0,[%fp-16]
	ld [%g2],%o2
	ld [%g2+4],%o0
	call %o2,0
	add %fp,-48,%o1
	ld [%fp-36],%o0
	cmp %o0,0
	be L3
	cmp %o0,1
	be L45
	cmp %o0,2
	bne L6
	cmp %o0,3
L45:
	b L3
	ldsb [%fp-24],%i0
L6:
	bne L8
	ld [%fp-36],%o0
	b L3
	ldub [%fp-24],%i0
L8:
	cmp %o0,4
	bne L10
	ld [%fp-36],%o0
	b L3
	ldsh [%fp-24],%i0
L10:
	cmp %o0,5
	bne L12
	ld [%fp-36],%o0
	b L3
	lduh [%fp-24],%i0
L12:
	cmp %o0,6
	be L46
	ld [%fp-36],%o0
	cmp %o0,7
	be L46
	cmp %o0,8
	be L46
	cmp %o0,9
	be L46
	add %o0,-10,%o0
	cmp %o0,1
	bgu L22
	ld [%fp-36],%o0
	ld [%fp-24],%i0
	b L3
	ld [%fp-20],%i1
L22:
	cmp %o0,12
	bne L24
	ld [%fp-36],%o0
	ld [%fp-48],%o0
	andcc %o0,32,%g0
	be L25
	ld [%fp-24],%f2
	b L3
	fstod %f2,%f0
L25:
	b L3
	ld [%fp-24],%f0
L24:
	cmp %o0,13
	bne L28
	ld [%fp-36],%o0
	b L3
	ldd [%fp-24],%f0
L28:
	cmp %o0,14
	bne L30
	ld [%fp-36],%o0
L46:
	b L3
	ld [%fp-24],%i0
L30:
	cmp %o0,15
	bne L3
	ld [%fp-48],%o0
	andcc %o0,24,%g0
	be L33
	ld [%fp-32],%o0
	ld [%i7+8],%o1
	and %o0,4095,%o0
	cmp %o0,%o1
	bne L3
	ld [%fp-40],%i0
	b L3
	add %i7,4,%i7
L33:
	ld [%fp-48],%o0
	andcc %o0,1,%g0
	be L36
	andcc %o0,2,%g0
	b L3
	ld [%fp-40],%i0
L36:
	be L38
	ld [%fp-32],%o0
	cmp %o0,1
	bne L39
	cmp %o0,2
	ld [%fp-40],%o0
	b L3
	ldub [%o0],%i0
L39:
	bne L42
	ld [%fp-32],%o0
	ld [%fp-40],%o0
	b L3
	lduh [%o0],%i0
L42:
	cmp %o0,4
	bne,a L3
	add %i7,4,%i7
	ld [%fp-40],%o0
	b L3
	ld [%o0],%i0
L38:
	add %i7,4,%i7
L3:
	ret
	restore
