| Tiny GCC Library
| Jörg Höhle, 9-Jul-96

|A 254 byte buffer is allocated and chunks max that large written
|Write() results are not checked! We suppose Write() writes all data!

|Bugs: (at least in 2.04), RawDoFmt can't output a %lc of '\0', it's
| simply skipped. This is certainly caused by the choice that the PutChProc
| routine is passed a 0 at the end of the format string.

.text
	.even
.globl _BPTRfprintf

| BPTRfprintf(BPTR, char * C-format, ... C-varargs);

_BPTRfprintf:

| Buffer size is 254 bytes

	link a3,#-260		| a3 serves as PutChData
			| a3@(16) C-varargs
			| a3@(12) C-format
			| a3@( 8) BPTR handle
			| a3@( 4) RTS
			| a3@( 0) a3 orig
			| a3@(-4) total
			| a3@(-6) counter WORD
			| a3@(-260) buffer
	moveml #0x0022,sp@-	| a2/a6
	clrw a3@(-6)
	clrl a3@(-4)
	movel a3@(12),a0	| C-format
	lea a3@(16),a1		| C-varargs
	lea pc@(stuffit),a2	| PutChProc, ATTENTION! old gas needed stuffit+2
				| PutChData in a3
	movel _SysBase,a6
	jsr a6@(-0x20a)		| RawDoFmt()

| flushit call superfluous as end is recognized by 0 char by stuffit
|	movew a3@(-6),d1
|	lea a3@(-260),a0
|	jbsr flushit
|L101:
	movel a3@(-4),d0
|	moveml a3@(-268),#0x4400	| a2/a6
	moveml sp@+,#0x4400		| a2/a6
	unlk a3
	rts

stuffit:
	|	d0.b char
	|	a3 remains accross RawDoFmt
	movew a3@(-6),d1
	lea a3@(-260),a0
	tstb d0			| 0 is end
	jeq flushit		| flush (without that 0)
	moveb d0,a0@(d1:w)
	addqw #1,d1
	cmpw #254,d1
	jeq L103
	movew d1,a3@(-6)
L102:
	rts
	
flushit:
	|	d1.w contains counter
	|	a0 contains buffer
	tstw d1
	jeq L102
L103:
	moveml #0x3002,sp@-	| d2-d3/a6
	moveq #0,d3		| necessary?
	movew d1,d3		| length
	movel a0,d2		| buffer
	movel a3@(8),d1		| handle
	movel _DOSBase,a6
	jsr a6@(-0x30)		| Write()

	movel d3,d1
	addl d1,a3@(-4)		| total += counter
	moveq #0,d1		| zero counter
	moveml sp@+,#0x400c	| d2-d3/a6
	movew d1,a3@(-6)
	rts

