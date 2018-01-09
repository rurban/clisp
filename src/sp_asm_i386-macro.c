#include "asm-i386.h"


        TEXT()

        GLOBL(C(asm_getSP))


        P2ALIGN(2,3)
        DECLARE_FUNCTION(asm_getSP)
FUNBEGIN(asm_getSP)
        INSN2(lea,l	,X4 MEM_DISP(esp,4),R(eax))
        ret
L(endof_asm_getSP):
        FUNEND(asm_getSP,L(endof_asm_getSP)-asm_getSP)

#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
