#include "asm-sparc.h"
        .section ".text"

        .global C(asm_getSP)


        .align 4
        DECLARE_FUNCTION(asm_getSP)
FUNBEGIN(asm_getSP)
        jmp %o7+8
       mov %sp,%o0
L(endof_asm_getSP):
        FUNEND(asm_getSP)
#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
