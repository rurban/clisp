	version	 "03.00"
	file	 "avcall-m88k.c"
data

; cc1 (2.6.3) arguments: -O -fdefer-pop -fomit-frame-pointer
; -fcse-follow-jumps -fcse-skip-blocks -fexpensive-optimizations
; -fthread-jumps -fstrength-reduce -fpeephole -ffunction-cse -finline
; -fcaller-saves -freg-struct-return -fdelayed-branch -frerun-cse-after-loop
; -fschedule-insns -fschedule-insns2 -fcommon -fgnu-linker -m88110 -m88100
; -m88000 -mocs-debug-info -mocs-frame-position -msvr4 -mcheck-zero-division
; -mstandard

gcc2_compiled.:
text
	align	 8
	global	 __builtin_avcall
	type	 __builtin_avcall,#function
__builtin_avcall:
	subu	 #r31,#r31,48
	st	 #r1,#r31,36
	st	 #r25,#r31,32
.Ltb0:
	or	 #r25,#r0,#r2
	ld	 #r13,#r25,20
	subu	 #r13,#r13,24
	or	 #r9,#r0,0
	subu	 #r13,#r13,#r25
	subu	 #r31,#r31,1024
	ext	 #r10,#r13,0<2>
	bcnd.n	 #le0,#r10,.L3
	or	 #r8,#r0,#r31
	or	 #r11,#r0,#r25
.L5:
	ld	 #r13,#r11,24
	st	 #r13,#r8[#r9]
	addu	 #r9,#r9,1
	cmp	 #r13,#r9,#r10
	bb1.n	 #lt,#r13,.L5
	addu	 #r11,#r11,4
.L3:
	ld	 #r13,#r25,12
	cmp	 #r13,#r13,16
	bb1	 #ne,#r13,.L7
	ld	 #r12,#r25,8
.L7:
	ld	 #r2,#r25,24
	ld	 #r13,#r0,#r25
	ld	 #r3,#r25,28
	ld	 #r4,#r25,32
	ld	 #r5,#r25,36
	ld	 #r6,#r25,40
	ld	 #r7,#r25,44
	ld	 #r8,#r25,48
	jsr.n	 #r13
	ld	 #r9,#r25,52
	ld	 #r10,#r25,12
	or	 #r9,#r0,#r2
	or.u	 #r13,#r0,#hi16(.L49)
	cmp	 #r11,#r10,16
	bb0.n	 #ls,#r11,.L8
	or	 #r13,#r13,#lo16(.L49)
	ld	 #r13,#r13[#r10]
	jmp	 #r13
section	 .rodata,"a"
	align	 4
.L49:
	word	 .L44
	word	 .L8
	word	 .L42
	word	 .L42
	word	 .L42
	word	 .L43
	word	 .L43
	word	 .L44
	word	 .L44
	word	 .L44
	word	 .L44
	word	 .L21
	word	 .L21
	word	 .L22
	word	 .L23
	word	 .L44
	word	 .L25
text
	align	 4
.L21:
	ld	 #r13,#r25,8
	st	 #r9,#r0,#r13
	ld	 #r13,#r25,8
	br.n	 .L8
	st	 #r3,#r13,4
	align	 4
.L22:
	ld	 #r13,#r25,8
	br.n	 .L8
	st	 #r2,#r0,#r13
	align	 4
.L23:
	ld	 #r13,#r25,8
	br.n	 .L8
	st.d	 #r2,#r0,#r13
	align	 4
.L25:
	ld	 #r13,#r25,4
	bb0	 (31-31),#r13,.L26
	ld	 #r11,#r25,16
	cmp	 #r13,#r11,2
	bb0	 #ne,#r13,.L29
	bb0.n	 #ls,#r13,.L38
	cmp	 #r13,#r11,1
	bb0	 #ne,#r13,.L28
	br	 .L32
	align	 4
.L38:
	cmp	 #r13,#r11,4
	bb0.n	 #ne,#r13,.L30
	cmp	 #r13,#r11,8
	bb0	 #ne,#r13,.L31
	br	 .L32
	align	 4
.L28:
	ld	 #r11,#r25,8
	ld.bu	 #r13,#r0,#r9
	br.n	 .L8
	st.b	 #r13,#r0,#r11
	align	 4
.L29:
	ld	 #r11,#r25,8
	ld.hu	 #r13,#r0,#r9
	br.n	 .L8
	st.h	 #r13,#r0,#r11
	align	 4
.L30:
	ld	 #r11,#r25,8
	ld	 #r13,#r0,#r9
	br.n	 .L8
	st	 #r13,#r0,#r11
	align	 4
.L31:
	ld	 #r11,#r25,8
	ld	 #r13,#r0,#r9
	st	 #r13,#r0,#r11
	ld	 #r11,#r25,8
	ld	 #r13,#r9,4
	br.n	 .L8
	st	 #r13,#r11,4
	align	 4
.L32:
	ld	 #r13,#r25,16
	addu	 #r13,#r13,3
	extu	 #r10,#r13,0<2>
	subu	 #r10,#r10,1
	bcnd	 #lt0,#r10,.L8
.L35:
	ld	 #r11,#r25,8
	ld	 #r13,#r9[#r10]
	st	 #r13,#r11[#r10]
	subu	 #r10,#r10,1
	bcnd	 #ge0,#r10,.L35
	addu	 #r31,#r31,1024
	br	 .L50
	align	 4
.L26:
	bb0	 (31-30),#r13,.L8
	ld	 #r13,#r25,16
	cmp	 #r11,#r13,2
	bb0	 #ne,#r11,.L43
	bb0	 #ls,#r11,.L47
	cmp	 #r13,#r13,1
	bb0	 #ne,#r13,.L42
	addu	 #r31,#r31,1024
	br	 .L50
	align	 4
.L47:
	cmp	 #r13,#r13,4
	bb0	 #ne,#r13,.L44
	addu	 #r31,#r31,1024
	br	 .L50
	align	 4
.L42:
	ld	 #r13,#r25,8
	br.n	 .L8
	st.b	 #r9,#r0,#r13
	align	 4
.L43:
	ld	 #r13,#r25,8
	br.n	 .L8
	st.h	 #r9,#r0,#r13
	align	 4
.L44:
	ld	 #r13,#r25,8
	st	 #r9,#r0,#r13
.L8:
	addu	 #r31,#r31,1024
.L50:
.Lte0:
	ld	 #r1,#r31,36
	or	 #r2,#r0,0
	ld	 #r25,#r31,32
	jmp.n	 #r1
	addu	 #r31,#r31,48

section	 .tdesc,"a"
	word	 66,1,.Ltb0,.Lte0,0x100103f,0x30,0xfffffff4,0xfffffff0
text
.Lfe1:
	size	 __builtin_avcall,.Lfe1-__builtin_avcall
