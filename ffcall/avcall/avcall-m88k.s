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
	ld	 #r11,#r25,12
	cmp	 #r13,#r11,1
	bb0.n	 #ne,#r13,.L9
	or	 #r9,#r0,#r2
	bcnd.n	 #eq0,#r11,.L59
	cmp	 #r13,#r11,2
	bb0.n	 #ne,#r13,.L60
	cmp	 #r13,#r11,3
	bb0.n	 #ne,#r13,.L60
	cmp	 #r13,#r11,4
	bb0.n	 #ne,#r13,.L60
	cmp	 #r13,#r11,5
	bb0.n	 #ne,#r13,.L61
	cmp	 #r13,#r11,6
	bb0.n	 #ne,#r13,.L61
	cmp	 #r13,#r11,7
	bb0.n	 #ne,#r13,.L59
	cmp	 #r13,#r11,8
	bb0.n	 #ne,#r13,.L59
	cmp	 #r13,#r11,9
	bb0.n	 #ne,#r13,.L59
	cmp	 #r13,#r11,10
	bb0.n	 #ne,#r13,.L59
	subu	 #r13,#r11,11
	cmp	 #r13,#r13,1
	bb0	 #ls,#r13,.L30
	ld	 #r13,#r25,8
	st	 #r9,#r0,#r13
	ld	 #r13,#r25,8
	br.n	 .L9
	st	 #r3,#r13,4
	align	 4
.L30:
	cmp	 #r13,#r11,13
	bb1.n	 #ne,#r13,.L32
	cmp	 #r13,#r11,14
	ld	 #r13,#r25,8
	br.n	 .L9
	st	 #r2,#r0,#r13
	align	 4
.L32:
	bb1.n	 #ne,#r13,.L34
	cmp	 #r13,#r11,15
	ld	 #r13,#r25,8
	br.n	 .L9
	st.d	 #r2,#r0,#r13
	align	 4
.L34:
	bb0.n	 #ne,#r13,.L59
	cmp	 #r13,#r11,16
	bb0	 #eq,#r13,.L9
	ld	 #r13,#r25,4
	bb0	 (31-31),#r13,.L39
	ld	 #r11,#r25,16
	cmp	 #r13,#r11,1
	bb1.n	 #ne,#r13,.L40
	cmp	 #r13,#r11,2
	ld	 #r11,#r25,8
	ld.bu	 #r13,#r0,#r9
	br.n	 .L9
	st.b	 #r13,#r0,#r11
	align	 4
.L40:
	bb1.n	 #ne,#r13,.L42
	cmp	 #r13,#r11,4
	ld	 #r11,#r25,8
	ld.hu	 #r13,#r0,#r9
	br.n	 .L9
	st.h	 #r13,#r0,#r11
	align	 4
.L42:
	bb1.n	 #ne,#r13,.L44
	cmp	 #r13,#r11,8
	ld	 #r11,#r25,8
	ld	 #r13,#r0,#r9
	br.n	 .L9
	st	 #r13,#r0,#r11
	align	 4
.L44:
	bb1.n	 #ne,#r13,.L46
	addu	 #r13,#r11,3
	ld	 #r11,#r25,8
	ld	 #r13,#r0,#r9
	st	 #r13,#r0,#r11
	ld	 #r11,#r25,8
	ld	 #r13,#r9,4
	br.n	 .L9
	st	 #r13,#r11,4
	align	 4
.L46:
	extu	 #r10,#r13,0<2>
	subu	 #r10,#r10,1
	bcnd	 #lt0,#r10,.L9
.L50:
	ld	 #r11,#r25,8
	ld	 #r13,#r9[#r10]
	st	 #r13,#r11[#r10]
	subu	 #r10,#r10,1
	bcnd	 #ge0,#r10,.L50
	addu	 #r31,#r31,1024
	br	 .L62
	align	 4
.L39:
	bb0	 (31-30),#r13,.L9
	ld	 #r11,#r25,16
	cmp	 #r13,#r11,1
	bb1.n	 #ne,#r13,.L54
	cmp	 #r13,#r11,2
.L60:
	ld	 #r13,#r25,8
	br.n	 .L9
	st.b	 #r9,#r0,#r13
	align	 4
.L54:
	bb1.n	 #ne,#r13,.L56
	cmp	 #r13,#r11,4
.L61:
	ld	 #r13,#r25,8
	br.n	 .L9
	st.h	 #r9,#r0,#r13
	align	 4
.L56:
	bb1	 #ne,#r13,.L9
.L59:
	ld	 #r13,#r25,8
	st	 #r9,#r0,#r13
.L9:
	addu	 #r31,#r31,1024
.L62:
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
