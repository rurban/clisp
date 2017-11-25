! Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

           .text

           .globl asm_getSP
           .globl asm_setSP

!    extern void* asm_getSP (void);
asm_getSP: lea sp@(4),a0   ! aktueller Wert von SP + 4 wegen Unterprogrammaufruf
           movel a0,d0     ! in D0 = Ergebnisregister
           rts

!    extern void asm_setSP (void* sp_init_address);
asm_setSP: movel sp@+,a0   ! Returnadresse nach A0
           movel sp@,sp    ! SP auf den übergebenen Wert setzen
           jmp a0@         ! zurückspringen

#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
           .section .note.GNU-stack,"",@progbits
#endif

