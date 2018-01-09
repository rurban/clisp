# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

  # (diese werden VOR der vorigen Instruktion ausgeführt):
  #define _             # Instruktion, die stets ausgeführt wird
  #define __            # Instruktion, die nur im Sprung-Fall ausgeführt wird
  # Abkürzungen für Anweisungen:
  #define ret   jmp %i7+8    # return from subroutine
  #define retl  jmp %o7+8    # return from leaf subroutine (no save/restore)

        .section ".text"

        .global asm_getSP

#    extern void* asm_getSP (void);
        .align 4
        .type asm_getSP,#function
asm_getSP:
        retl
       _ mov %sp,%o0
.Lendof_asm_getSP:
        .size asm_getSP,.Lendof_asm_getSP-asm_getSP

