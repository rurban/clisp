#include "asm-hppa.h"
        .LEVEL 1.1
	IMPORT_MILLICODE($$dyncall)




        TEXT1()
	TEXT2()
        .align 4
        GLOBL(asm_mulu32_64)
        DECLARE_FUNCTION(asm_mulu32_64)


DEF(asm_mulu32_64)
        .PROC
        .CALLINFO
        .ENTRY
        STW %arg0,-16(%sp)
        FLDWS -16(%sp),%fr4
        STW %arg1,-16(%sp)
        FLDWS -16(%sp),%fr5
        XMPYU %fr4,%fr5,%fr6
        FSTDS %fr6,-16(%sp)
        LDW -16(%sp),%ret0
        LDW -12(%sp),%ret1
        BV 0(%r2)
        NOP
        .EXIT
        .PROCEND
DEF(L(endof_asm_mulu32_64))
        FUNEND(asm_mulu32_64)


        TEXT1()
	TEXT2()
        .align 4
        GLOBL(asm_length32)
        DECLARE_FUNCTION(asm_length32)

DEF(asm_length32)
        .PROC
        .CALLINFO
        .ENTRY

        LDI 1,%ret0

        EXTRU,<> %arg0,15,16,%r0
        SHD,TR %arg0,%r0,16,%arg0
        ADDI 16,%ret0,%ret0

        EXTRU,<> %arg0,7,8,%r0
        SHD,TR %arg0,%r0,24,%arg0
        ADDI 8,%ret0,%ret0

        EXTRU,<> %arg0,3,4,%r0
        SHD,TR %arg0,%r0,28,%arg0
        ADDI 4,%ret0,%ret0

        EXTRU,<> %arg0,1,2,%r0
        SHD,TR %arg0,%r0,30,%arg0
        ADDI 2,%ret0,%ret0

        EXTRU,= %arg0,0,1,%r0
        ADDI 1,%ret0,%ret0
        BV 0(%r2)
        NOP
        .EXIT
        .PROCEND
DEF(L(endof_asm_length32))
        FUNEND(asm_length32)
#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
