;NO_APP
.fpmode native
gcc2_compiled.:
___gnu_compiled_c:
.text
.text
.align 2
.globl ___vacall_r
	ds.b "g263"
	ds.b "+01\0"
___vacall_r:
	sub.w #32,sp
	ld.w #0,s1
	st.w s1,-28(fp)
	st.w ap,-24(fp)
	st.w s1,-20(fp)
	st.w s1,-16(fp)
	pshea -28(fp)
	mov s0,a1
	ld.w 4(a1),s1
	psh.w s1
	ld.w (a1),a1
	mov sp,ap
	calls (a1)
	ld.w 12(fp),ap
	add.w #8,sp
	ld.w -16(fp),a1
	ltu.w #15,a1
	jbra.t L1
	shf #2,a1
	ld.w L29(a1),a1
	jmp (a1)
.text 2
L29:
	ds.w L2
	ds.w L3
	ds.w L4
	ds.w L5
	ds.w L6
	ds.w L7
	ds.w L16
	ds.w L16
	ds.w L16
	ds.w L16
	ds.w L12
	ds.w L13
	ds.w L14
	ds.w L15
	ds.w L16
	ds.w L17
.text
.align 2
L2:
	rtn
L3:
L4:
	ld.b -8(fp),s1
	cvtb.w s1,s0
	rtn
L5:
	ld.b -8(fp),s1
	jbr L31
L6:
	ld.h -8(fp),s1
	cvth.w s1,s0
	rtn
L7:
	ld.h -8(fp),s1
	jbr L32
L12:
L13:
	ld.l -8(fp),s0
	rtn
L14:
	ld.s -8(fp),s0
	rtn
L15:
	ld.d -8(fp),s0
	rtn
L16:
	ld.w -8(fp),s0
	rtn
L17:
	ld.w -28(fp),s2
	mov.w s2,s1
	and #1,s1
	eq.w #0,s1
	jbrs.t L18
	ld.w -20(fp),s0
	rtn
L18:
	mov.w s2,s1
	and #1024,s1
	eq.w #0,s1
	jbrs.t L19
	ld.w -12(fp),s1
	eq.w #2,s1
	jbrs.t L23
	ltu.w #2,s1
	jbrs.t L28
	eq.w #1,s1
	jbrs.t L22
	rtn
L28:
	eq.w #4,s1
	jbrs.t L24
	eq.w #8,s1
	jbrs.t L25
	rtn
L22:
	ld.w -20(fp),a1
	ld.b (a1),s1
L31:
	mov.w s1,s0
	and #0xff,s0
	rtn
L23:
	ld.w -20(fp),a1
	ld.h (a1),s1
L32:
	mov.w s1,s0
	and #0xffff,s0
	rtn
L24:
	ld.w -20(fp),a1
	ld.w (a1),s0
	rtn
L25:
	ld.w -20(fp),a1
	ld.l (a1),s0
L19:
	rtn
L1:
	rtn
	ds.h 0
