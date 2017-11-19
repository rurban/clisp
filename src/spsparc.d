# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

  # (diese werden VOR der vorigen Instruktion ausgeführt):
  #define _             # Instruktion, die stets ausgeführt wird
  #define __            # Instruktion, die nur im Sprung-Fall ausgeführt wird
  # Abkürzungen für Anweisungen:
  #define ret   jmp %i7+8    # return from subroutine
  #define retl  jmp %o7+8    # return from leaf subroutine (no save/restore)

        .seg "text"

        .global asm_getSP
        .global _asm_getSP

#    extern void* asm_getSP (void);
asm_getSP:
_asm_getSP: retl
       _ mov %sp,%o0

#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
        .section .note.GNU-stack,"",@progbits
#endif

