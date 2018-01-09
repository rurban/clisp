# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

        .text

        .globl asm_getSP

#    extern void* asm_getSP (void);
        .p2align 2,,3
        .type asm_getSP,@function
asm_getSP:
        leal    4(%esp),%eax    # aktueller Wert von ESP + 4 wegen Unterprogrammaufruf
        ret
.Lendof_asm_getSP:
        .size asm_getSP,.Lendof_asm_getSP-asm_getSP

