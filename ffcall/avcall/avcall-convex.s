;NO_APP
.fpmode native
gcc2_compiled.:
___gnu_compiled_c:
.text
.text
.align 2
.globl ___builtin_avcall
	ds.b "g263"
	ds.b "+01\0"
___builtin_avcall:
	sub.w #1032,sp
	ld.w (ap),a4
	ld.w 20(a4),s1
	mov a4,s3
	add.w #-32,s1
	sub.w s3,s1
	cvtw.l s1,s2
	shf #-2,s2
	ld.w #0,s1
	lt.w s1,s2
	jbrs.f L2
	mov sp,a2
	mov a4,a3
	mov s2,a1
	shf #2,a1
	add.w sp,a1
L4:
	ld.w 32(a3),s1
	add.w #4,a3
	st.w s1,(a2)
	add.w #4,a2
	lt.w a2,a1
	jbra.t L4
L2:
	ld.w (a4),a1
	st.w a4,-1028(fp)
	mov sp,ap
	calls (a1)
	ld.w 12(fp),ap
	ld.w -1028(fp),a4
	ld.w 12(a4),a1
	mov s0,a3
	ltu.w #16,a1
	jbra.t L6
	shf #2,a1
	ld.w L48(a1),a1
	jmp (a1)
.text 2
L48:
	ds.w L42
	ds.w L6
	ds.w L40
	ds.w L40
	ds.w L40
	ds.w L41
	ds.w L41
	ds.w L42
	ds.w L42
	ds.w L42
	ds.w L42
	ds.w L43
	ds.w L43
	ds.w L20
	ds.w L21
	ds.w L42
	ds.w L23
.text
.align 2
L20:
	ld.w 8(a4),a1
	st.s s0,(a1)
	jbr L6
L21:
	ld.w 8(a4),a1
	st.d s0,(a1)
	jbr L6
L23:
	ld.w 4(a4),s2
	mov.w s2,s1
	and #1,s1
	eq.w #0,s1
	jbrs.t L24
	ld.w 16(a4),s1
	eq.w #2,s1
	jbrs.t L27
	ltu.w #2,s1
	jbrs.t L36
	eq.w #1,s1
	jbrs.t L26
	jbr L30
L36:
	eq.w #4,s1
	jbrs.t L28
	eq.w #8,s1
	jbrs.t L29
	jbr L30
L26:
	ld.w 8(a4),a1
	ld.b (a3),s1
	st.b s1,(a1)
	jbr L6
L27:
	ld.w 8(a4),a1
	ld.h (a3),s1
	st.h s1,(a1)
	jbr L6
L28:
	ld.w 8(a4),a1
	ld.w (a3),s1
	st.w s1,(a1)
	jbr L6
L29:
	ld.w 8(a4),a1
	ld.w (a3),s1
	st.w s1,(a1)
	ld.w 8(a4),a1
	ld.w 4(a3),s1
	st.w s1,4(a1)
	jbr L6
L30:
	ld.w 16(a4),s1
	add.w #3,s1
	shf.w #-2,s1
	add.w #-1,s1
	le.w #0,s1
	jbrs.f L6
	mov s1,a1
	shf #2,a1
	mov a1,a2
	add.w a3,a2
	mov.w s1,s2
	shf.w #2,s2
L33:
	ld.w 8(a4),a1
	ld.w (a2),s1
	add.w #-4,a2
	add.w s2,a1
	add.w #-4,s2
	st.w s1,(a1)
	lt.w a2,a3
	jbra.f L33
	jbr L6
L24:
	mov.w s2,s1
	and #256,s1
	eq.w #0,s1
	jbrs.t L6
	ld.w 16(a4),s1
	eq.w #2,s1
	jbrs.t L41
	ltu.w #2,s1
	jbrs.t L46
	eq.w #1,s1
	jbrs.t L40
	jbr L6
L46:
	eq.w #4,s1
	jbrs.t L42
	eq.w #8,s1
	jbrs.t L43
	jbr L6
L40:
	ld.w 8(a4),a1
	st.b a3,(a1)
	jbr L6
L41:
	ld.w 8(a4),a1
	st.h a3,(a1)
	jbr L6
L42:
	ld.w 8(a4),a1
	st.w a3,(a1)
	jbr L6
L43:
	ld.w 8(a4),a1
	st.l s0,(a1)
L6:
	ld.w #0,s0
	rtn
	ds.h 0
