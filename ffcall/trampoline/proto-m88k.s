	version	 "03.00"
	file	 "proto.c"
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
	global	 tramp
	type	 tramp,#function
tramp:
	or.u	 #r13,#r0,0x1234
	or	 #r13,#r13,0x5678
	subu	 #r31,#r31,48
	or.u	 #r2,#r0,0xbabe
	or	 #r2,#r2,0xbec0
	st	 #r1,#r31,36
.Ltb0:
	or.u	 #r12,#r0,0x7355
	or	 #r12,#r12,0x4711
	jsr.n	 #r2
	st	 #r12,#r0,#r13
.Lte0:
	ld	 #r1,#r31,36
	jmp.n	 #r1
	addu	 #r31,#r31,48

section	 .tdesc,"a"
	word	 66,1,.Ltb0,.Lte0,0x100003f,0x30,0xfffffff4,0xfffffff4
text
.Lfe1:
	size	 tramp,.Lfe1-tramp
	align	 8
	global	 jump
	type	 jump,#function
jump:
.Ltb1:
	or.u	 #r9,#r0,0xbabe
	or	 #r9,#r9,0xbec0
	jmp	 #r9
	align	 4
.Lte1:

section	 .tdesc,"a"
	word	 66,1,.Ltb1,.Lte1,0x100001f,0x0,0x1,0x0
text
.Lfe2:
	size	 jump,.Lfe2-jump
