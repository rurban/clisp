#include "asm-mips.h"


        .text


        .globl asm_getSP
        .align 2
        .ent asm_getSP
        DECLARE_FUNCTION(asm_getSP)
asm_getSP:
        move $2,$sp
        j $31
        .end asm_getSP
