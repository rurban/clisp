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
	global	 vacall
	type	 vacall,#function
vacall:
	subu	 #r31,#r31,80
	st	 #r2,#r31,80
	st	 #r3,#r31,84
	st	 #r4,#r31,88
	st	 #r5,#r31,92
	st	 #r6,#r31,96
	st	 #r7,#r31,100
	st	 #r8,#r31,104
	or.u	 #r13,#r0,#hi16(vacall_function)
	st	 #r9,#r31,108
	ld	 #r11,#r13,#lo16(vacall_function)
	st	 #r1,#r31,36
.Ltb0:
	st	 #r0,#r31,40
	addu	 #r13,#r31,112
	st	 #r13,#r31,44
	st	 #r0,#r31,48
	st	 #r0,#r31,52
	addu	 #r2,#r31,40
	jsr.n	 #r11
	st	 #r12,#r31,72
	ld	 #r11,#r31,52
	bcnd.n	 #eq0,#r11,.L3
	cmp	 #r13,#r11,1
	bb0.n	 #ne,#r13,.L39
	cmp	 #r13,#r11,2
	bb1.n	 #ne,#r13,.L6
	cmp	 #r13,#r11,3
.L39:
	ld.b	 #r2,#r31,64
	br	 .L3
	align	 4
.L6:
	bb1.n	 #ne,#r13,.L8
	cmp	 #r13,#r11,4
	ld.bu	 #r2,#r31,64
	br	 .L3
	align	 4
.L8:
	bb1.n	 #ne,#r13,.L10
	cmp	 #r13,#r11,5
	ld.h	 #r2,#r31,64
	br	 .L3
	align	 4
.L10:
	bb1.n	 #ne,#r13,.L12
	cmp	 #r13,#r11,6
	ld.hu	 #r2,#r31,64
	br	 .L3
	align	 4
.L12:
	bb0.n	 #ne,#r13,.L40
	cmp	 #r13,#r11,7
	bb0.n	 #ne,#r13,.L40
	cmp	 #r13,#r11,8
	bb0.n	 #ne,#r13,.L40
	cmp	 #r13,#r11,9
	bb0.n	 #ne,#r13,.L40
	subu	 #r13,#r11,10
	cmp	 #r13,#r13,1
	bb0	 #ls,#r13,.L22
	ld	 #r2,#r31,64
	ld	 #r3,#r31,68
	br	 .L3
	align	 4
.L22:
	ld	 #r11,#r31,52
	cmp	 #r13,#r11,12
	bb1.n	 #ne,#r13,.L24
	cmp	 #r13,#r11,13
	ld	 #r2,#r31,64
	br	 .L3
	align	 4
.L24:
	bb1.n	 #ne,#r13,.L26
	cmp	 #r13,#r11,14
	ld.d	 #r2,#r31,64
	br	 .L3
	align	 4
.L26:
	bb1.n	 #ne,#r13,.L28
	cmp	 #r13,#r11,15
.L40:
	ld	 #r2,#r31,64
	br	 .L3
	align	 4
.L28:
	bb0	 #eq,#r13,.L3
	ld	 #r13,#r31,40
	bb0	 (31-31),#r13,.L31
	ld	 #r2,#r31,48
	br	 .L3
	align	 4
.L31:
	bb0	 (31-30),#r13,.L3
	ld	 #r11,#r31,56
	cmp	 #r13,#r11,1
	bb1.n	 #ne,#r13,.L34
	cmp	 #r13,#r11,2
	ld	 #r13,#r31,48
	ld.bu	 #r2,#r0,#r13
	br	 .L3
	align	 4
.L34:
	bb1.n	 #ne,#r13,.L36
	cmp	 #r13,#r11,4
	ld	 #r13,#r31,48
	ld.hu	 #r2,#r0,#r13
	br	 .L3
	align	 4
.L36:
	bb1	 #ne,#r13,.L3
	ld	 #r13,#r31,48
	ld	 #r2,#r0,#r13
.L3:
.Lte0:
	ld	 #r1,#r31,36
	jmp.n	 #r1
	addu	 #r31,#r31,80

section	 .tdesc,"a"
	word	 66,1,.Ltb0,.Lte0,0x100003f,0x50,0xffffffd4,0xffffffd4
text
.Lfe1:
	size	 vacall,.Lfe1-vacall
