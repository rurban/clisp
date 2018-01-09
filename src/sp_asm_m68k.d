! Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

        .text

        .align 2
        .globl asm_getSP
        .type asm_getSP,@function
!    extern void* asm_getSP (void);
asm_getSP:
        lea %sp@(4),%a0 ! aktueller Wert von SP + 4 wegen Unterprogrammaufruf
        movel %a0,%d0   ! in D0 = Ergebnisregister
        rts
.Lendof_asm_getSP:
        .size asm_getSP,.Lendof_asm_getSP-asm_getSP

        .align 2
        .globl asm_setSP
        .type asm_setSP,@function
!    extern void asm_setSP (void* sp_init_address);
asm_setSP:
        movel %sp@+,%a0 ! Returnadresse nach A0
        movel %sp@,%sp  ! SP auf den übergebenen Wert setzen
        jmp %a0@        ! zurückspringen
.Lendof_asm_setSP:
        .size asm_setSP,.Lendof_asm_setSP-asm_setSP

