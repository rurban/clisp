#APP
| GCC Library
| Jörg Höhle, 29-Nov-92
| I wanted _exit() a fall-through of _main()
| which is not possible in C, so I wrote it in assembly.
.text
	.even
.globl _ENTRY
_ENTRY:
	| save all but scratch registers, why not?
	moveml #0x3f3e,sp@-
	movel sp,___ExitSP
	movel 4:w,_SysBase
	| call _main(arglen,argline);
	movel a0,sp@-
	movel d0,sp@-
	jbsr __main
	jra L101
.globl __exit
__exit:
	movel sp@(4),d0
L101:
	movel ___ExitSP,sp
	moveml sp@+,#0x7cfc
	rts
.data
	.even
.comm ___ExitSP,4
.comm _SysBase,4
