# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

        .text

#    extern void* asm_getSP (void);
        .globl asm_getSP
        .align 2
        .ent asm_getSP
asm_getSP:
        move $2,$sp   # get stack pointer
        j $31         # return
        .end asm_getSP

