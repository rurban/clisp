	.file	"proto64.c"
	.section	".toc","aw"
	.section	".text"
	.section	".toc","aw"
.LC0:
	.tc ID_73554711_43622155[TC],0x7355471143622155
.LC1:
	.tc ID_babebec0_dea0ffab[TC],0xbabebec0dea0ffab
	.section	".text"
	.align 2
	.p2align 4,,15
	.globl tramp
	.section	".opd","aw"
	.align 3
tramp:
	.quad	.L.tramp,.TOC.@tocbase,0
	.previous
	.type	tramp, @function
.L.tramp:
	mflr 0
	ld 9,.LC1@toc(2)
	ld 11,.LC0@toc(2)
	std 0,16(1)
	stdu 1,-112(1)
	nop
	nop
	ld 0,0(9)
	std 2,40(1)
	mtctr 0
	ld 11,16(9)
	ld 2,8(9)
	bctrl
	ld 2,40(1)
	addi 1,1,112
	ld 0,16(1)
	mtlr 0
	blr
	.long 0
	.byte 0,0,0,1,128,0,0,0
	.size	tramp,.-.L.tramp
	.section	".toc","aw"
	.set .LC2,.LC1
	.section	".text"
	.align 2
	.p2align 4,,15
	.globl jump
	.section	".opd","aw"
	.align 3
jump:
	.quad	.L.jump,.TOC.@tocbase,0
	.previous
	.type	jump, @function
.L.jump:
	ld 0,.LC2@toc(2)
	mtctr 0
	bctr
	.long 0
	.byte 0,0,0,0,0,0,0,0
	.size	jump,.-.L.jump
	.ident	"GCC: (GNU) 4.0.2"
