	version	 "03.00"
	file	 "vacall-m88k.c"
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
	global	 __vacall_r
	type	 __vacall_r,#function
__vacall_r:
	subu	 #r31,#r31,80
	st	 #r1,#r31,36
.Ltb0:
	st	 #r2,#r31,80
	st	 #r3,#r31,84
	st	 #r4,#r31,88
	st	 #r5,#r31,92
	st	 #r6,#r31,96
	st	 #r7,#r31,100
	st	 #r8,#r31,104
	st	 #r9,#r31,108
	st	 #r0,#r31,40
	addu	 #r13,#r31,112
	st	 #r13,#r31,44
	st	 #r0,#r31,48
	st	 #r0,#r31,52
	st	 #r12,#r31,72
	ld	 #r2,#r11,4
	ld	 #r13,#r0,#r11
	jsr.n	 #r13
	addu	 #r3,#r31,40
	ld	 #r9,#r31,52
	or.u	 #r13,#r0,#hi16(.L29)
	cmp	 #r10,#r9,15
	bb0.n	 #ls,#r10,.L2
	or	 #r13,#r13,#lo16(.L29)
	ld	 #r13,#r13[#r9]
	jmp	 #r13
section	 .rodata,"a"
	align	 4
.L29:
	word	 .L2
	word	 .L4
	word	 .L5
	word	 .L6
	word	 .L7
	word	 .L8
	word	 .L17
	word	 .L17
	word	 .L17
	word	 .L17
	word	 .L14
	word	 .L14
	word	 .L15
	word	 .L16
	word	 .L17
	word	 .L18
text
	align	 4
.L4:
.L5:
	ld.b	 #r2,#r31,64
	br	 .L2
	align	 4
.L6:
	ld.bu	 #r2,#r31,64
	br	 .L2
	align	 4
.L7:
	ld.h	 #r2,#r31,64
	br	 .L2
	align	 4
.L8:
	ld.hu	 #r2,#r31,64
	br	 .L2
	align	 4
.L14:
	ld	 #r2,#r31,64
	ld	 #r3,#r31,68
	br	 .L2
	align	 4
.L15:
	ld	 #r2,#r31,64
	br	 .L2
	align	 4
.L16:
	ld.d	 #r2,#r31,64
	br	 .L2
	align	 4
.L17:
	ld	 #r2,#r31,64
	br	 .L2
	align	 4
.L18:
	ld	 #r13,#r31,40
	bb0	 (31-31),#r13,.L19
	ld	 #r2,#r31,48
	br	 .L2
	align	 4
.L19:
	bb0	 (31-30),#r13,.L2
	ld	 #r13,#r31,56
	cmp	 #r10,#r13,2
	bb0	 #ne,#r10,.L24
	bb0	 #ls,#r10,.L28
	cmp	 #r13,#r13,1
	bb0	 #ne,#r13,.L23
	br	 .L2
	align	 4
.L28:
	cmp	 #r13,#r13,4
	bb0	 #ne,#r13,.L25
	br	 .L2
	align	 4
.L23:
	ld	 #r13,#r31,48
	ld.bu	 #r2,#r0,#r13
	br	 .L2
	align	 4
.L24:
	ld	 #r13,#r31,48
	ld.hu	 #r2,#r0,#r13
	br	 .L2
	align	 4
.L25:
	ld	 #r13,#r31,48
	ld	 #r2,#r0,#r13
.L2:
.Lte0:
	ld	 #r1,#r31,36
	jmp.n	 #r1
	addu	 #r31,#r31,80

section	 .tdesc,"a"
	word	 66,1,.Ltb0,.Lte0,0x100003f,0x50,0xffffffd4,0xffffffd4
text
.Lfe1:
	size	 __vacall_r,.Lfe1-__vacall_r
