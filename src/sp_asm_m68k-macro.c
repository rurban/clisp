#include "asm-m68k.h"


        .text

        .align 2
        .globl C(asm_getSP)
        DECLARE_FUNCTION(asm_getSP)

FUNBEGIN(asm_getSP)
        lea %sp@(4),%a0
        movel %a0,%d0
        rts
L(endof_asm_getSP):
        FUNEND(asm_getSP)

        .align 2
        .globl C(asm_setSP)
        DECLARE_FUNCTION(asm_setSP)

FUNBEGIN(asm_setSP)
        movel %sp@+,%a0
        movel %sp@,%sp
        jmp %a0@
L(endof_asm_setSP):
        FUNEND(asm_setSP)
#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
