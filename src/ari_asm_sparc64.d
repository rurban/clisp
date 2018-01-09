# Externe Routinen zu ARILEV1.D
# Prozessor: SPARC 64-bit
# Compiler: GNU-C oder ...
# Parameter passing conventions:
#   Argument registers:
#     %o0..%o5
#   Arguments of type uint8, uint16, uint32 have already been converted to
#   uint64 by the caller (zero-extend, "srl reg,0,reg").
#   Arguments of type sint8, sint16, sint32 have already been converted to
#   sint64 by the caller (sign-extend, "sra reg,0,reg").
#   Return value register:
#     %o0
#   Return values of type uint8, uint16, uint32 must be converted to uint64
#   before returning (zero-extend, "srl %o0,0,%o0").
#   Return values of type sint8, sint16, sint32 must be converted to sint64
#   before returning (sign-extend, "sra %o0,0,%o0").
#   Call-used registers (do not have to be preserved across function calls):
#     %o0..%o5, %g1..%g5
#   Global register usage:
#     see gcc-5.4.0/gcc/config/sparc/sparc.h comment before CALL_USED_REGISTERS.
# Settings: intCsize=32, intDsize=32.

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS
  #define SHIFT_LOOPS
  #define MUL_LOOPS
  #define DIV_LOOPS

#else

  # Indikatoren für Anweisungen (Instruktionen) in Delay-Slots
  # (diese werden VOR der vorigen Instruktion ausgeführt):
  #define _             # Instruktion, die stets ausgeführt wird
  #define __            # Instruktion, die nur im Sprung-Fall ausgeführt wird
  # Abkürzungen für Anweisungen:
  #define ret   return %i7+8  # return from subroutine
  #define retl  jmp %o7+8     # return from leaf subroutine (no save/restore)

  # Avoid "detect global register use not covered .register pseudo-op" error
  .register %g2,#scratch

        .section ".text"

        .global asm_mulu16_
        .global asm_mulu32_64
        .global asm_mulu32_unchecked
        .global asm_divu_6432_3232_
        .global asm_divu_3216_1616_
        .global asm_copy_loop_up
        .global asm_copy_loop_down
        .global asm_fill_loop_up
        .global asm_fill_loop_down
        .global asm_clear_loop_up
        .global asm_clear_loop_down
        .global asm_or_loop_up
        .global asm_xor_loop_up
        .global asm_and_loop_up
        .global asm_eqv_loop_up
        .global asm_nand_loop_up
        .global asm_nor_loop_up
        .global asm_andc2_loop_up
        .global asm_orc2_loop_up
        .global asm_not_loop_up
        .global asm_and_test_loop_up
        .global asm_test_loop_up
        .global asm_compare_loop_up
        .global asm_add_loop_down
        .global asm_addto_loop_down
        .global asm_inc_loop_down
        .global asm_sub_loop_down
        .global asm_subx_loop_down
        .global asm_subfrom_loop_down
        .global asm_dec_loop_down
        .global asm_neg_loop_down
        .global asm_shift1left_loop_down
        .global asm_shiftleft_loop_down
        .global asm_shiftleftcopy_loop_down
        .global asm_shift1right_loop_up
        .global asm_shiftright_loop_up
        .global asm_shiftrightsigned_loop_up
        .global asm_shiftrightcopy_loop_up
        .global asm_mulusmall_loop_down
        .global asm_mulu_loop_down
        .global asm_muluadd_loop_down
        .global asm_mulusub_loop_down
        .global asm_divu_loop_up
        .global asm_divucopy_loop_up

#define LOOP_TYPE  1    # 1: Standard-Schleifen
                        # 2: Schleifen ohne Pointer, nur mit Zähler
#define STANDARD_LOOPS  (LOOP_TYPE==1)
#define COUNTER_LOOPS  (LOOP_TYPE==2)

# extern uint32 asm_mulu16_ (uint16 arg1, uint16 arg2);
# result := arg1*arg2.
        .align 4
        .type asm_mulu16_,#function
asm_mulu16_: # Input in %o0,%o1, verändert %g1, Output in %o0
        umul %o0,%o1,%g1
        retl
       _ srl %g1,0,%o0
.Lendof_asm_mulu16_:
        .size asm_mulu16_,.Lendof_asm_mulu16_-asm_mulu16_

# extern uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2);
# result := arg1*arg2.
        .align 4
        .type asm_mulu32_64,#function
asm_mulu32_64: # Input in %o0,%o1, verändert %g1, Output in %o0
        umul %o0,%o1,%g1
        rd %y,%o1
        srl %g1,0,%o0
        sllx %o1,32,%o1
        retl
       _ or %o0,%o1,%o0
.Lendof_asm_mulu32_64:
        .size asm_mulu32_64,.Lendof_asm_mulu32_64-asm_mulu32_64

# extern uint32 asm_mulu32_unchecked (uint32 x, uint32 y);
# result := arg1*arg2 < 2^32.
        .align 4
        .type asm_mulu32_unchecked,#function
asm_mulu32_unchecked: # Input in %o0,%o1, Output in %o0
        umul %o0,%o1,%o2
        retl
       _ srl %o2,0,%o0
.Lendof_asm_mulu32_unchecked:
        .size asm_mulu32_unchecked,.Lendof_asm_mulu32_unchecked-asm_mulu32_unchecked

# extern uint64 [struct { uint32 q; uint32 r; }] asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y); -> 2^32*r+q
# x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^32*y .
        .align 4
        .type asm_divu_6432_3232_,#function
asm_divu_6432_3232_: # Input in %o0,%o1,%o2, Output in %o0
        wr %o0,%g0,%y
        udiv %o1,%o2,%o0        # x durch y dividieren, %o0 := q
        umul %o0,%o2,%g1        # %g1 := (q*y) mod 2^32
        sub %o1,%g1,%o1         # %o1 := (xlo-q*y) mod 2^32 = r
        srl %o0,0,%o0
        sllx %o1,32,%o1
        retl
       _ or %o0,%o1,%o0
.Lendof_asm_divu_6432_3232_:
        .size asm_divu_6432_3232_,.Lendof_asm_divu_6432_3232_-asm_divu_6432_3232_

# extern uint32 [struct { uint16 q; uint16 r; }] asm_divu_3216_1616_ (uint32 x, uint16 y); -> 2^16*r+q
# x = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^16*y .
        .align 4
        .type asm_divu_3216_1616_,#function
asm_divu_3216_1616_: # Input in %o0,%o1, Output in %o0 (Rest und Quotient).
        wr %g0,%g0,%y
        udiv %o0,%o1,%o2        # dividieren, Quotient nach %o2
#if 0 # Who says that %y has some meaningful contents after 'udiv' ??
        rd %y,%g1               # Rest aus %y
#else
        umul %o2,%o1,%g1        # %g1 := (q*y) mod 2^32
        sub %o0,%g1,%g1         # %g1 := (x-q*y) mod 2^32 = r
#endif
        sll %g1,16,%g1          # in die oberen 16 Bit schieben
        or %o2,%g1,%o0
        retl
       _ srl %o0,0,%o0
.Lendof_asm_divu_3216_1616_:
        .size asm_divu_3216_1616_,.Lendof_asm_divu_3216_1616_-asm_divu_3216_1616_

# extern uintD* asm_copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
        .align 4
        .type asm_copy_loop_up,#function
asm_copy_loop_up: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll09
       _ nop
.Ll08:    ld [%o0],%o3
          add %o0,4,%o0
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll08
         _ add %o1,4,%o1
.Ll09:  retl
       _ mov %o1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll09
       _ sub %o1,4,%o1
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[count]
        sub %o1,%o2,%o1         # %o1 = &destptr[count-1]
.Ll08:    ld [%o0+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          bne,pt %xcc,.Ll08
         _ st %o3,[%o1+%o2]     # Digit ablegen
.Ll09:  retl
       _ add %o1,4,%o0
#endif
.Lendof_asm_copy_loop_up:
        .size asm_copy_loop_up,.Lendof_asm_copy_loop_up-asm_copy_loop_up

# extern uintD* asm_copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 4
        .type asm_copy_loop_down,#function
asm_copy_loop_down: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll11
       _ sub %o0,4,%o0
.Ll10:    ld [%o0],%o3
          sub %o1,4,%o1
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll10
         _ sub %o0,4,%o0
.Ll11:  retl
       _ mov %o1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll11
       _ sub %o0,4,%o0
        sllx %o2,2,%o2          # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count]
.Ll10:    ld [%o0+%o2],%o3      # nächstes Digit holen
          subcc %o2,4,%o2       # Zähler erniedrigen, Pointer erniedrigen
          bne,pt %xcc,.Ll10
         _ st %o3,[%o1+%o2]     # Digit ablegen
.Ll11:  retl
       _ mov %o1,%o0
#endif
.Lendof_asm_copy_loop_down:
        .size asm_copy_loop_down,.Lendof_asm_copy_loop_down-asm_copy_loop_down

# extern uintD* asm_fill_loop_up (uintD* destptr, uintC count, uintD filler);
        .align 4
        .type asm_fill_loop_up,#function
asm_fill_loop_up: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll13
       _ nop
.Ll12:    st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,.Ll12
         _ add %o0,4,%o0
.Ll13:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll13
       _ sub %o0,4,%o0
        sub %g0,%o1,%o1         # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
.Ll12:    addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          bne,pt %xcc,.Ll12
         _ st %o2,[%o0+%o1]     # Digit ablegen
.Ll13:  retl
       _ add %o0,4,%o0
#endif
.Lendof_asm_fill_loop_up:
        .size asm_fill_loop_up,.Lendof_asm_fill_loop_up-asm_fill_loop_up

# extern uintD* asm_fill_loop_down (uintD* destptr, uintC count, uintD filler);
        .align 4
        .type asm_fill_loop_down,#function
asm_fill_loop_down: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll15
       _ sub %o0,4,%o0
.Ll14:    st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,.Ll14
         _ sub %o0,4,%o0
.Ll15:  retl
       _ add %o0,4,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll15
       _ sllx %o1,2,%o1         # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[-count]
.Ll14:    subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,pt %xcc,.Ll14
         _ st %o2,[%o0+%o1]     # Digit ablegen
.Ll15:  retl
       _ nop
#endif
.Lendof_asm_fill_loop_down:
        .size asm_fill_loop_down,.Lendof_asm_fill_loop_down-asm_fill_loop_down

# extern uintD* asm_clear_loop_up (uintD* destptr, uintC count);
        .align 4
        .type asm_clear_loop_up,#function
asm_clear_loop_up: # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll17
       _ nop
.Ll16:    st %g0,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,.Ll16
         _ add %o0,4,%o0
.Ll17:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll17
       _ sub %o0,4,%o0
        sub %g0,%o1,%o1         # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
.Ll16:    addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          bne,pt %xcc,.Ll16
         _ st %g0,[%o0+%o1]     # Digit 0 ablegen
.Ll17:  retl
       _ add %o0,4,%o0
#endif
.Lendof_asm_clear_loop_up:
        .size asm_clear_loop_up,.Lendof_asm_clear_loop_up-asm_clear_loop_up

# extern uintD* asm_clear_loop_down (uintD* destptr, uintC count);
        .align 4
        .type asm_clear_loop_down,#function
asm_clear_loop_down: # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll19
       _ sub %o0,4,%o0
.Ll18:    st %g0,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,.Ll18
         _ sub %o0,4,%o0
.Ll19:  retl
       _ add %o0,4,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll19
       _ sllx %o1,2,%o1         # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[-count]
.Ll18:    subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,pt %xcc,.Ll18
         _ st %g0,[%o0+%o1]     # Digit 0 ablegen
.Ll19:  retl
       _ nop
#endif
.Lendof_asm_clear_loop_down:
        .size asm_clear_loop_down,.Lendof_asm_clear_loop_down-asm_clear_loop_down

# extern void asm_or_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_or_loop_up,#function
asm_or_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll21
       _ nop
.Ll20:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll20
         _ add %o0,4,%o0
.Ll21:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll21
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll20:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          or %o4,%o3,%o3        # beide verknüpfen
          bne,pt %xcc,.Ll20
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll21:  retl
       _ nop
#endif
.Lendof_asm_or_loop_up:
        .size asm_or_loop_up,.Lendof_asm_or_loop_up-asm_or_loop_up

# extern void asm_xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_xor_loop_up,#function
asm_xor_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll23
       _ nop
.Ll22:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll22
         _ add %o0,4,%o0
.Ll23:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll23
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll22:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          xor %o4,%o3,%o3       # beide verknüpfen
          bne,pt %xcc,.Ll22
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll23:  retl
       _ nop
#endif
.Lendof_asm_xor_loop_up:
        .size asm_xor_loop_up,.Lendof_asm_xor_loop_up-asm_xor_loop_up

# extern void asm_and_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_and_loop_up,#function
asm_and_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll25
       _ nop
.Ll24:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll24
         _ add %o0,4,%o0
.Ll25:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll25
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll24:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          and %o4,%o3,%o3       # beide verknüpfen
          bne,pt %xcc,.Ll24
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll25:  retl
       _ nop
#endif
.Lendof_asm_and_loop_up:
        .size asm_and_loop_up,.Lendof_asm_and_loop_up-asm_and_loop_up

# extern void asm_eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_eqv_loop_up,#function
asm_eqv_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll27
       _ nop
.Ll26:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xnor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll26
         _ add %o0,4,%o0
.Ll27:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll27
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll26:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          xnor %o4,%o3,%o3      # beide verknüpfen
          bne,pt %xcc,.Ll26
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll27:  retl
       _ nop
#endif
.Lendof_asm_eqv_loop_up:
        .size asm_eqv_loop_up,.Lendof_asm_eqv_loop_up-asm_eqv_loop_up

# extern void asm_nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_nand_loop_up,#function
asm_nand_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll29
       _ nop
.Ll28:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll28
         _ add %o0,4,%o0
.Ll29:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll29
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll28:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          and %o4,%o3,%o3       # beide verknüpfen
          xor %o3,-1,%o3
          bne,pt %xcc,.Ll28
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll29:  retl
       _ nop
#endif
.Lendof_asm_nand_loop_up:
        .size asm_nand_loop_up,.Lendof_asm_nand_loop_up-asm_nand_loop_up

# extern void asm_nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_nor_loop_up,#function
asm_nor_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll31
       _ nop
.Ll30:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll30
         _ add %o0,4,%o0
.Ll31:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll31
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll30:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          or %o4,%o3,%o3        # beide verknüpfen
          xor %o3,-1,%o3
          bne,pt %xcc,.Ll30
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll31:  retl
       _ nop
#endif
.Lendof_asm_nor_loop_up:
        .size asm_nor_loop_up,.Lendof_asm_nor_loop_up-asm_nor_loop_up

# extern void asm_andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_andc2_loop_up,#function
asm_andc2_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll33
       _ nop
.Ll32:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          andn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll32
         _ add %o0,4,%o0
.Ll33:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll33
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll32:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          andn %o4,%o3,%o3      # beide verknüpfen
          bne,pt %xcc,.Ll32
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll33:  retl
       _ nop
#endif
.Lendof_asm_andc2_loop_up:
        .size asm_andc2_loop_up,.Lendof_asm_andc2_loop_up-asm_andc2_loop_up

# extern void asm_orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_orc2_loop_up,#function
asm_orc2_loop_up: # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll35
       _ nop
.Ll34:    ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          orn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll34
         _ add %o0,4,%o0
.Ll35:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll35
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
.Ll34:    ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          orn %o4,%o3,%o3       # beide verknüpfen
          bne,pt %xcc,.Ll34
         _ st %o3,[%o0+%o2]     # Digit ablegen
.Ll35:  retl
       _ nop
#endif
.Lendof_asm_orc2_loop_up:
        .size asm_orc2_loop_up,.Lendof_asm_orc2_loop_up-asm_orc2_loop_up

# extern void asm_not_loop_up (uintD* xptr, uintC count);
        .align 4
        .type asm_not_loop_up,#function
asm_not_loop_up: # Input in %o0,%o1
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll37
       _ nop
.Ll36:    ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne,pt %xcc,.Ll36
         _ add %o0,4,%o0
.Ll37:  retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll37
       _ sub %o0,4,%o0
        sub %g0,%o1,%o1         # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
.Ll36:    addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o1],%o2      # nächstes Digit holen
          xor %o2,-1,%o2
          bne,pt %xcc,.Ll36
         _ st %o2,[%o0+%o1]     # Digit ablegen
.Ll37:  retl
       _ nop
#endif
.Lendof_asm_not_loop_up:
        .size asm_not_loop_up,.Lendof_asm_not_loop_up-asm_not_loop_up

# extern bool asm_and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_and_test_loop_up,#function
asm_and_test_loop_up: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll39
       _ nop
.Ll38:    ld [%o0],%o3
          ld [%o1],%o4
          add %o0,4,%o0
          andcc %o3,%o4,%g0
          bne,pn %icc,.Ll40
         _ subcc %o2,1,%o2
          bne,pt %xcc,.Ll38
         _ add %o1,4,%o1
.Ll39:  retl
       _ mov 0,%o0
.Ll40:  retl
       _ mov 1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll39
       _ sub %g0,%o2,%o2        # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
          ld [%o0+%o2],%o3      # nächstes Digit holen
.Ll38:    ld [%o1+%o2],%o4      # noch ein Digit holen
          andcc %o3,%o4,%g0     # beide verknüpfen
          bne,pn %icc,.Ll40
         _ addcc %o2,4,%o2      # Zähler "erniedrigen", Pointer erhöhen
          bne,a,pt %xcc,.Ll38
         __ ld [%o0+%o2],%o3    # nächstes Digit holen
.Ll39:  retl
       _ mov 0,%o0
.Ll40:  retl
       _ mov 1,%o0
#endif
.Lendof_asm_and_test_loop_up:
        .size asm_and_test_loop_up,.Lendof_asm_and_test_loop_up-asm_and_test_loop_up

# extern bool asm_test_loop_up (uintD* ptr, uintC count);
        .align 4
        .type asm_test_loop_up,#function
asm_test_loop_up: # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll42
       _ nop
          lduw [%o0],%o2
.Ll41:    add %o0,4,%o0
          brnz,pn %o2,.Ll43
         _ subcc %o1,1,%o1
          bne,a,pt %xcc,.Ll41
         __ lduw [%o0],%o2
.Ll42:  retl
       _ mov 0,%o0
.Ll43:  retl
       _ mov 1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll42
       _ sub %g0,%o1,%o1        # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[count]
          lduw [%o0+%o1],%o2    # nächstes Digit holen
.Ll41:    brnz,pn %o2,.Ll43     # testen
         _ addcc %o1,4,%o1      # Zähler "erniedrigen", Pointer erhöhen
          bne,a,pt %xcc,.Ll41
         __ lduw [%o0+%o1],%o2  # nächstes Digit holen
.Ll42:  retl
       _ mov 0,%o0
.Ll43:  retl
       _ mov 1,%o0
#endif
.Lendof_asm_test_loop_up:
        .size asm_test_loop_up,.Lendof_asm_test_loop_up-asm_test_loop_up

# extern signean asm_compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 4
        .type asm_compare_loop_up,#function
asm_compare_loop_up: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll45
       _ nop
          ld [%o0],%o3
.Ll44:    ld [%o1],%o4
          add %o0,4,%o0
          subcc %o3,%o4,%g0
          bne,pn %icc,.Ll46
         _ add %o1,4,%o1
          subcc %o2,1,%o2
          bne,a,pt %xcc,.Ll44
         __ ld [%o0],%o3
.Ll45:  retl
       _ mov 0,%o0
.Ll46:  mov 1,%o0
        movlu %icc,-1,%o0
        retl
       _ sra %o0,0,%o0          # sign-extend %o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll45
       _ sub %g0,%o2,%o2        # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
          ld [%o0+%o2],%o3      # nächstes Digit holen
.Ll44:    ld [%o1+%o2],%o4      # noch ein Digit holen
          subcc %o3,%o4,%g0     # vergleichen
          bne,pn %icc,.Ll46
         _ addcc %o2,4,%o2      # Zähler "erniedrigen", Pointer erhöhen
          bne,a,pt %xcc,.Ll44
         __ ld [%o0+%o2],%o3    # nächstes Digit holen
.Ll45:  retl
       _ mov 0,%o0
.Ll46:  subcc %o3,%o4,%g0       # nochmals vergleichen
        mov 1,%o0
        movlu %icc,-1,%o0
        retl
       _ sra %o0,0,%o0          # sign-extend %o0
#endif
.Lendof_asm_compare_loop_up:
        .size asm_compare_loop_up,.Lendof_asm_compare_loop_up-asm_compare_loop_up

# extern uintD asm_add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
        .align 4
        .type asm_add_loop_down,#function
asm_add_loop_down: # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#if STANDARD_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,.Ll49
       _ mov %g0,%g1            # Carry := 0
.Ll48:    sub %o0,4,%o0
          lduw [%o0],%o4        # source1-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o5        # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          add %g1,%o5,%g1       # zum Carry addieren
          sub %o2,4,%o2
          st %g1,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll48
         _ srlx %g1,32,%g1      # neuer Carry
.Ll49:  retl
       _ mov %g1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,.Ll49
       _ mov %g0,%g1            # Carry := 0
        sllx %o3,2,%o3          # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
.Ll48:    subcc %o3,4,%o3
          lduw [%o0+%o3],%o4    # source1-digit, zero-extend
          lduw [%o1+%o3],%o5    # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          add %g1,%o5,%g1       # zum Carry addieren
          st %g1,[%o2+%o3]      # Digit ablegen
          bne,pt %xcc,.Ll48
         _ srlx %g1,32,%g1      # neuer Carry
.Ll49:  retl
       _ mov %g1,%o0
#endif
.Lendof_asm_add_loop_down:
        .size asm_add_loop_down,.Lendof_asm_add_loop_down-asm_add_loop_down

# extern uintD asm_addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 4
        .type asm_addto_loop_down,#function
asm_addto_loop_down: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll51
       _ mov %g0,%o5            # Carry := 0
.Ll50:    sub %o0,4,%o0
          lduw [%o0],%o3        # source-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o4        # dest-digit, zero-extend
          add %o5,%o3,%o5       # zum Carry addieren
          add %o5,%o4,%o5       # zum Carry addieren
          st %o5,[%o1]          # Digit ablegen
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll50
         _ srlx %o5,32,%o5      # neuer Carry
.Ll51:  retl
       _ mov %o5,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll51
       _ mov %g0,%o5            # Carry := 0
        sllx %o2,2,%o2          # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count]
.Ll50:    subcc %o2,4,%o2
          lduw [%o0+%o2],%o3    # source-digit, zero-extend
          lduw [%o1+%o2],%o4    # dest-digit, zero-extend
          add %o5,%o3,%o5       # zum Carry addieren
          add %o5,%o4,%o5       # zum Carry addieren
          st %o5,[%o1+%o2]      # Digit ablegen
          bne,pt %xcc,.Ll50
         _ srlx %o5,32,%o5      # neuer Carry
.Ll51:  retl
       _ mov %o5,%o0
#endif
.Lendof_asm_addto_loop_down:
        .size asm_addto_loop_down,.Lendof_asm_addto_loop_down-asm_addto_loop_down

# extern uintD asm_inc_loop_down (uintD* ptr, uintC count);
        .align 4
        .type asm_inc_loop_down,#function
asm_inc_loop_down: # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll53
       _ sub %o0,4,%o0
.Ll52:    ld [%o0],%o2
          addcc %o2,1,%o2
          bne,pn %icc,.Ll54
         _ st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,.Ll52
         _ sub %o0,4,%o0
.Ll53:  retl
       _ mov 1,%o0
.Ll54:  retl
       _ mov 0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll53
       _ sub %o0,4,%o0
        sllx %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
.Ll52:    addcc %o2,1,%o2       # incrementieren
          bne,pn %icc,.Ll54
         _ st %o2,[%o0+%o1]     # ablegen
          subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,a,pt %xcc,.Ll52
         __ ld [%o0+%o1],%o2
.Ll53:  retl
       _ mov 1,%o0
.Ll54:  retl
       _ mov 0,%o0
#endif
.Lendof_asm_inc_loop_down:
        .size asm_inc_loop_down,.Lendof_asm_inc_loop_down-asm_inc_loop_down

# extern uintD asm_sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
        .align 4
        .type asm_sub_loop_down,#function
asm_sub_loop_down: # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#if STANDARD_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,.Ll56
       _ mov %g0,%g1            # Carry := 0
.Ll55:    sub %o0,4,%o0
          lduw [%o0],%o4        # source1-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o5        # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll55
         _ srax %g1,32,%g1      # neuer Carry
.Ll56:  retl
       _ srl %g1,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,.Ll56
       _ mov %g0,%g1            # Carry := 0
        sllx %o3,2,%o3          # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
.Ll55:    subcc %o3,4,%o3
          lduw [%o0+%o3],%o4    # source1-digit, zero-extend
          lduw [%o1+%o3],%o5    # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2+%o3]      # Digit ablegen
          bne,pt %xcc,.Ll55
         _ srax %g1,32,%g1      # neuer Carry
.Ll56:  retl
       _ srl %g1,0,%o0
#endif
.Lendof_asm_sub_loop_down:
        .size asm_sub_loop_down,.Lendof_asm_sub_loop_down-asm_sub_loop_down

# extern uintD asm_subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
        .align 4
        .type asm_subx_loop_down,#function
asm_subx_loop_down: # Input in %o0,%o1,%o2,%o3,%o4, verändert %g1, Output in %o0
#if STANDARD_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,.Ll58
       _ sra %o4,0,%g1          # Carry, sign-extend
.Ll57:    sub %o0,4,%o0
          lduw [%o0],%o4        # source1-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o5        # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          sub %o2,4,%o2
          st %g1,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll57
         _ srax %g1,32,%g1      # neuer Carry
.Ll58:  retl
       _ srl %g1,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,.Ll58
       _ sra %o4,0,%g1          # Carry, sign-extend
        sllx %o3,2,%o3          # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
.Ll57:    subcc %o3,4,%o3
          lduw [%o0+%o3],%o4    # source1-digit, zero-extend
          lduw [%o1+%o3],%o5    # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2+%o3]      # Digit ablegen
          bne,pt %xcc,.Ll57
         _ srax %g1,32,%g1      # neuer Carry
.Ll58:  retl
       _ srl %g1,0,%o0
#endif
.Lendof_asm_subx_loop_down:
        .size asm_subx_loop_down,.Lendof_asm_subx_loop_down-asm_subx_loop_down

# extern uintD asm_subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 4
        .type asm_subfrom_loop_down,#function
asm_subfrom_loop_down: # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll60
       _ mov %g0,%o5            # Carry := 0
.Ll59:    sub %o1,4,%o1
          lduw [%o1],%o4        # dest-digit, zero-extend
          sub %o0,4,%o0
          lduw [%o0],%o3        # source-digit, zero-extend
          add %o5,%o4,%o5       # zum Carry addieren
          sub %o5,%o3,%o5       # vom Carry subtrahieren
          st %o5,[%o1]          # Digit ablegen
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll59
         _ srax %o5,32,%o5      # neuer Carry
.Ll60:  retl
       _ srl %o5,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll60
       _ mov %g0,%o5            # Carry := 0
        sllx %o2,2,%o2          # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count]
.Ll59:    subcc %o2,4,%o2
          lduw [%o1+%o2],%o4    # dest-digit, zero-extend
          lduw [%o0+%o2],%o3    # source-digit, zero-extend
          add %o5,%o4,%o5       # zum Carry addieren
          sub %o5,%o3,%o5       # vom Carry subtrahieren
          st %o5,[%o1+%o2]      # Digit ablegen
          bne,pt %xcc,.Ll59
         _ srax %o5,32,%o5      # neuer Carry
.Ll60:  retl
       _ srl %o5,0,%o0
#endif
.Lendof_asm_subfrom_loop_down:
        .size asm_subfrom_loop_down,.Lendof_asm_subfrom_loop_down-asm_subfrom_loop_down

# extern uintD asm_dec_loop_down (uintD* ptr, uintC count);
        .align 4
        .type asm_dec_loop_down,#function
asm_dec_loop_down: # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll62
       _ sub %o0,4,%o0
.Ll61:    ld [%o0],%o2
          subcc %o2,1,%o2
          bcc,pn %icc,.Ll63
         _ st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,.Ll61
         _ sub %o0,4,%o0
.Ll62:  retl
       _ mov -1,%o0
.Ll63:  retl
       _ mov 0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll62
       _ sub %o0,4,%o0
        sllx %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
.Ll61:    subcc %o2,1,%o2       # decrementieren
          bcc,pn %icc,.Ll63
         _ st %o2,[%o0+%o1]     # ablegen
          subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,a,pt %xcc,.Ll61
         __ ld [%o0+%o1],%o2
.Ll62:  retl
       _ mov -1,%o0
.Ll63:  retl
       _ mov 0,%o0
#endif
.Lendof_asm_dec_loop_down:
        .size asm_dec_loop_down,.Lendof_asm_dec_loop_down-asm_dec_loop_down

# extern uintD asm_neg_loop_down (uintD* ptr, uintC count);
        .align 4
        .type asm_neg_loop_down,#function
asm_neg_loop_down: # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        # erstes Digit /=0 suchen:
        brz,pn %o1,.Ll65
       _ sub %o0,4,%o0
.Ll64:    ld [%o0],%o2
          subcc %g0,%o2,%o2
          bne,pn %icc,.Ll66
         _ subcc %o1,1,%o1
          bne,pt %xcc,.Ll64
         _ sub %o0,4,%o0
.Ll65:  retl
       _ mov 0,%o0
.Ll66:  # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        st %o2,[%o0]            # 1 Digit negieren
        # alle anderen Digits invertieren:
        be,pn %xcc,.Ll68
       _ sub %o0,4,%o0
.Ll67:    ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne,pt %xcc,.Ll67
         _ sub %o0,4,%o0
.Ll68:  mov -1,%o0
        retl
       _ srl %o0,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        # erstes Digit /=0 suchen:
        brz,pn %o1,.Ll65
       _ sub %o0,4,%o0
        sllx %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
.Ll64:    subcc %g0,%o2,%o2     # negieren, testen
          bne,pn %icc,.Ll66
         _ subcc %o1,4,%o1      # Zähler erniedrigen, Pointer erniedrigen
          bne,a,pt %xcc,.Ll64
         __ ld [%o0+%o1],%o2
.Ll65:  retl
       _ mov 0,%o0
.Ll66:  # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        # alle anderen Digits invertieren:
        add %o1,4,%o1
        st %o2,[%o0+%o1]        # ablegen
        subcc %o1,4,%o1
        be,pn %xcc,.Ll68
       _ nop
          ld [%o0+%o1],%o2
.Ll67:    subcc %o1,4,%o1
          xor %o2,-1,%o2
          st %o2,[%o0+%o1]
          bne,a,pt %xcc,.Ll67
         __ ld [%o0+%o1],%o2
.Ll68:  mov -1,%o0
        retl
       _ srl %o0,0,%o0
#endif
.Lendof_asm_neg_loop_down:
        .size asm_neg_loop_down,.Lendof_asm_neg_loop_down-asm_neg_loop_down

# extern uintD asm_shift1left_loop_down (uintD* ptr, uintC count);
        .align 4
        .type asm_shift1left_loop_down,#function
asm_shift1left_loop_down: # Input in %o0,%o1, Output in %o0
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll70
       _ mov 0,%o3              # Carry := 0
.Ll69:    sub %o0,4,%o0
          lduw [%o0],%o2        # Digit
          subcc %o1,1,%o1
          add %o2,%o2,%o2       # shiften
          or %o3,%o2,%o3        # zum Carry addieren
          st %o3,[%o0]          # Digit ablegen
          bne,pt %xcc,.Ll69
         _ srlx %o3,32,%o3      # neuer Carry
.Ll70:  retl
       _ mov %o3,%o0
.Lendof_asm_shift1left_loop_down:
        .size asm_shift1left_loop_down,.Lendof_asm_shift1left_loop_down-asm_shift1left_loop_down

# extern uintD asm_shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
        .align 4
        .type asm_shiftleft_loop_down,#function
asm_shiftleft_loop_down: # Input in %o0,%o1,%o2,%o3, Output in %o0
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll72
       _ srl %o3,0,%o3          # zero-extend carry
.Ll71:    sub %o0,4,%o0
          lduw [%o0],%o4        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o4,%o2,%o4      # shiften
          or %o3,%o4,%o3        # zum Carry addieren
          st %o3,[%o0]          # Digit ablegen
          bne,pt %xcc,.Ll71
         _ srlx %o3,32,%o3      # neuer Carry
.Ll72:  retl
       _ mov %o3,%o0
.Lendof_asm_shiftleft_loop_down:
        .size asm_shiftleft_loop_down,.Lendof_asm_shiftleft_loop_down-asm_shiftleft_loop_down

# extern uintD asm_shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
        .align 4
        .type asm_shiftleftcopy_loop_down,#function
asm_shiftleftcopy_loop_down: # Input in %o0,%o1,%o2,%o3, Output in %o0
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,.Ll74
       _ mov 0,%o4              # Carry := 0
.Ll73:    sub %o0,4,%o0
          lduw [%o0],%o5        # Digit, zero-extend
          subcc %o2,1,%o2
          sllx %o5,%o3,%o5      # shiften
          or %o4,%o5,%o4        # zum Carry addieren
          sub %o1,4,%o1
          st %o4,[%o1]          # Digit ablegen
          bne,pt %xcc,.Ll73
         _ srlx %o4,32,%o4      # neuer Carry
.Ll74:  retl
       _ mov %o4,%o0
.Lendof_asm_shiftleftcopy_loop_down:
        .size asm_shiftleftcopy_loop_down,.Lendof_asm_shiftleftcopy_loop_down-asm_shiftleftcopy_loop_down

# extern uintD asm_shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
        .align 4
        .type asm_shift1right_loop_up,#function
asm_shift1right_loop_up: # Input in %o0,%o1,%o2, Output in %o0
#ifdef SLOWER
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll76
       _ sllx %o2,63,%o2        # Carry
.Ll75:    lduw [%o0],%o3        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o3,31,%o3       # shiften
          or %o2,%o3,%o2        # und mit altem Carry kombinieren
          srlx %o2,32,%o3
          st %o3,[%o0]          # und ablegen
          sllx %o2,32,%o2       # neuer Carry
          bne,pt %xcc,.Ll75
         _ add %o0,4,%o0
.Ll76:  retl
       _ srlx %o2,32,%o0
#else
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll76
       _ sll %o2,31,%o2         # Carry
.Ll75:    ld [%o0],%o3          # Digit
          subcc %o1,1,%o1
          srl %o3,1,%o4         # shiften
          or %o2,%o4,%o4        # und mit altem Carry kombinieren
          st %o4,[%o0]          # und ablegen
          sll %o3,31,%o2        # neuer Carry
          bne,pt %xcc,.Ll75
         _ add %o0,4,%o0
.Ll76:  retl
       _ mov %o2,%o0
#endif
.Lendof_asm_shift1right_loop_up:
        .size asm_shift1right_loop_up,.Lendof_asm_shift1right_loop_up-asm_shift1right_loop_up

# extern uintD asm_shiftright_loop_up (uintD* ptr, uintC count, uintC i);
        .align 4
        .type asm_shiftright_loop_up,#function
asm_shiftright_loop_up: # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
#ifdef SLOWER
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll78
       _ or %g0,%g0,%o3         # Carry := 0
        mov 32,%g1
        sub %g1,%o2,%g1         # 32-i
.Ll77:    lduw [%o0],%o4        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o4,%g1,%o4      # shiften
          or %o3,%o4,%o3        # und mit altem Carry kombinieren
          srlx %o3,32,%o4
          st %o4,[%o0]          # und ablegen
          sllx %o3,32,%o3       # neuer Carry
          bne,pt %xcc,.Ll77
         _ add %o0,4,%o0
.Ll78:  retl
       _ srlx %o3,32,%o0
#else
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,.Ll78
       _ or %g0,%g0,%o3         # Carry := 0
        sub %g0,%o2,%g1         # 32-i (mod 32)
.Ll77:    ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          srl %o4,%o2,%o5       # shiften
          or %o3,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o0]          # und ablegen
          sll %o4,%g1,%o3       # neuer Carry
          bne,pt %xcc,.Ll77
         _ add %o0,4,%o0
.Ll78:  retl
       _ mov %o3,%o0
#endif
.Lendof_asm_shiftright_loop_up:
        .size asm_shiftright_loop_up,.Lendof_asm_shiftright_loop_up-asm_shiftright_loop_up

# extern uintD asm_shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
        .align 4
        .type asm_shiftrightsigned_loop_up,#function
asm_shiftrightsigned_loop_up: # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
#ifdef SLOWER
#       srl %o1,0,%o1           # zero-extend %o1 = count
        mov 32,%g1
        sub %g1,%o2,%g1         # 32-i
        ldsw [%o0],%o3          # erstes Digit, sign-extend
        subcc %o1,1,%o1
        sllx %o3,%g1,%o3        # shiften
        srlx %o3,32,%o4
        st %o4,[%o0]            # und ablegen
        sllx %o3,32,%o3         # neuer Carry
        be,pn %xcc,.Ll80
       _ add %o0,4,%o0
.Ll79:    lduw [%o0],%o4        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o4,%g1,%o4      # shiften
          or %o3,%o4,%o3        # und mit altem Carry kombinieren
          srlx %o3,32,%o4
          st %o4,[%o0]          # und ablegen
          sllx %o3,32,%o3       # neuer Carry
          bne,pt %xcc,.Ll79
         _ add %o0,4,%o0
.Ll80:  retl
       _ srlx %o3,32,%o0
#else
#       srl %o1,0,%o1           # zero-extend %o1 = count
        ld [%o0],%o4            # erstes Digit
        sub %g0,%o2,%g1         # 32-i (mod 32)
        sra %o4,%o2,%o5         # shiften
        st %o5,[%o0]            # und ablegen
        sll %o4,%g1,%o3         # neuer Carry
        subcc %o1,1,%o1
        be,pn %xcc,.Ll80
       _ add %o0,4,%o0
.Ll79:    ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          srl %o4,%o2,%o5       # shiften
          or %o3,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o0]          # und ablegen
          sll %o4,%g1,%o3       # neuer Carry
          bne,pt %xcc,.Ll79
         _ add %o0,4,%o0
.Ll80:  retl
       _ mov %o3,%o0
#endif
.Lendof_asm_shiftrightsigned_loop_up:
        .size asm_shiftrightsigned_loop_up,.Lendof_asm_shiftrightsigned_loop_up-asm_shiftrightsigned_loop_up

# extern uintD asm_shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
        .align 4
        .type asm_shiftrightcopy_loop_up,#function
asm_shiftrightcopy_loop_up: # Input in %o0,%o1,%o2,%o3,%o4, verändert %g1,%g2, Output in %o0
#ifdef SLOWER
#       srl %o2,0,%o2           # zero-extend %o2 = count
        sub %g0,%o3,%g1         # 64-i (mod 64)
        brz,pn %o2,.Ll82
       _ sllx %o4,%g1,%o4       # erster Carry
        add %g1,32,%g1          # 32-i
.Ll81:    lduw [%o0],%o5        # Digit, zero-extend
          add %o0,4,%o0
          sllx %o5,%g1,%o5      # shiften
          or %o4,%o5,%o4        # und mit altem Carry kombinieren
          srlx %o4,32,%o5
          st %o5,[%o1]          # und ablegen
          sllx %o4,32,%o4       # neuer Carry
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll81
         _ add %o1,4,%o1
.Ll82:  retl
       _ srlx %o4,32,%o0
#else
#       srl %o2,0,%o2           # zero-extend %o2 = count
        sub %g0,%o3,%g1         # 32-i (mod 32)
        brz,pn %o2,.Ll82
       _ sll %o4,%g1,%g2        # erster Carry
.Ll81:    ld [%o0],%o4          # Digit
          add %o0,4,%o0
          srl %o4,%o3,%o5       # shiften
          or %g2,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o1]          # und ablegen
          sll %o4,%g1,%g2       # neuer Carry
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll81
         _ add %o1,4,%o1
.Ll82:  retl
       _ mov %g2,%o0
#endif
.Lendof_asm_shiftrightcopy_loop_up:
        .size asm_shiftrightcopy_loop_up,.Lendof_asm_shiftrightcopy_loop_up-asm_shiftrightcopy_loop_up

# extern uintD asm_mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
        .align 4
        .type asm_mulusmall_loop_down,#function
asm_mulusmall_loop_down: # Input in %o0,%o1,%o2,%o3, Output in %o0
#       srl %o2,0,%o2           # zero-extend %o2 = len
        brz,pn %o2,.Ll85
       _ sub %o1,4,%o1
.Ll83:    # nächstes Digit [%o1] mit der 6-Bit-Zahl %o0 multiplizieren
          # und kleinen Carry %o3 dazu:
          mov %o0,%y
          ld [%o1],%o4          # Wartetakt!
          addcc %o3,%o3,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%g0,%o5
          # Die 26 unteren Bits von %o5 und die 6 oberen Bits von %y
          # ergeben das Resultat. (Die anderen Bits sind Null.)
          sra %o5,26,%o3        # 6 obere Bits von %o5 -> neuer Carry
          tst %o4               # Korrektur, falls %o4 negativ war
          add %o3,%o0,%o4
          movl %icc,%o4,%o3     # (falls %o4 negativ war, noch + %o0)
          rd %y,%o4
          srl %o4,26,%o4        # 6 obere Bits von %y
          sll %o5,6,%o5         # 26 untere Bits von %o5
          or %o5,%o4,%o4        # neues Digit
          st %o4,[%o1]          # ablegen
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll83
         _ sub %o1,4,%o1
.Ll85:  retl
       _ srl %o3,0,%o0
.Lendof_asm_mulusmall_loop_down:
        .size asm_mulusmall_loop_down,.Lendof_asm_mulusmall_loop_down-asm_mulusmall_loop_down

# extern void asm_mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
        .align 4
        .type asm_mulu_loop_down,#function
asm_mulu_loop_down: # Input in %o0,%o1,%o2,%o3, verändert %g1
#       srl %o3,0,%o3           # zero-extend %o3 = len
        mov 0,%o4               # Carry
.Ll86:    sub %o1,4,%o1
          ld [%o1],%g1          # nächstes Digit
          sub %o2,4,%o2
          # mit digit multiplizieren: %o0 * %g1 -> %o5|%g1
          umul %g1,%o0,%g1
          rd %y,%o5
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%o5,%o4      # High-Digit gibt neuen Carry
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll86
         _ st %g1,[%o2]         # Low-Digit ablegen
        retl
       _ st %o4,[%o2-4]         # letzten Carry ablegen
.Lendof_asm_mulu_loop_down:
        .size asm_mulu_loop_down,.Lendof_asm_mulu_loop_down-asm_mulu_loop_down

# extern uintD asm_muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
        .align 4
        .type asm_muluadd_loop_down,#function
asm_muluadd_loop_down: # Input in %o0,%o1,%o2,%o3, verändert %g1,%g2, Output in %o0
#       srl %o3,0,%o3           # zero-extend %o3 = len
        mov 0,%o4               # Carry
.Ll89:    sub %o1,4,%o1
          ld [%o1],%o5          # nächstes source-Digit
          sub %o2,4,%o2
          # mit digit multiplizieren: %o0 * %o5 -> %g2|%g1
          umul %o0,%o5,%g1
          rd %y,%g2
          ld [%o2],%o5          # nächstes dest-digit
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%g2,%o4      # High-Digit gibt neuen Carry
          addcc %o5,%g1,%g1     # addieren
          addx %g0,%o4,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll89
         _ st %g1,[%o2]         # Low-Digit ablegen
        retl
       _ srl %o4,0,%o0          # letzter Carry
.Lendof_asm_muluadd_loop_down:
        .size asm_muluadd_loop_down,.Lendof_asm_muluadd_loop_down-asm_muluadd_loop_down

# extern uintD asm_mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
        .align 4
        .type asm_mulusub_loop_down,#function
asm_mulusub_loop_down: # Input in %o0,%o1,%o2,%o3, verändert %g1,%g2, Output in %o0
#       srl %o3,0,%o3           # zero-extend %o3 = len
        mov 0,%o4               # Carry
.Ll90:    sub %o1,4,%o1
          ld [%o1],%o5          # nächstes source-Digit
          sub %o2,4,%o2
          # mit digit multiplizieren: %o0 * %o5 -> %g2|%g1
          umul %o0,%o5,%g1
          rd %y,%g2
          ld [%o2],%o5          # nächstes dest-digit
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%g2,%o4      # High-Digit gibt neuen Carry
          subcc %o5,%g1,%o5     # davon das Low-Digit subtrahieren
          addx %g0,%o4,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll90
         _ st %o5,[%o2]         # dest-Digit ablegen
        retl
       _ srl %o4,0,%o0          # letzter Carry
.Lendof_asm_mulusub_loop_down:
        .size asm_mulusub_loop_down,.Lendof_asm_mulusub_loop_down-asm_mulusub_loop_down

# extern uintD asm_divu_loop_up (uintD digit, uintD* ptr, uintC len);
        .align 4
        .type asm_divu_loop_up,#function
asm_divu_loop_up: # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
#       srl %o2,0,%o2           # zero-extend %o2 = len
        brz,pn %o2,.Ll92
       _ mov 0,%o3              # Rest
#       srl %o0,0,%o0           # zero-extend %o0 = digit
.Ll91:    lduw [%o1],%o4        # nächstes Digit
          sllx %o3,32,%o3       # Rest als High-Digit
          or %o3,%o4,%o3        # zusammen
          udivx %o3,%o0,%o4     # durch digit dividieren
          st %o4,[%o1]          # Quotient ablegen
          umul %o0,%o4,%g1
          sub %o3,%g1,%o3       # Rest in den unteren 32 Bit von %o3
          subcc %o2,1,%o2
          bne,pt %xcc,.Ll91
         _ add %o1,4,%o1
.Ll92:  retl
       _ srl %o3,0,%o0          # Rest als Ergebnis
.Lendof_asm_divu_loop_up:
        .size asm_divu_loop_up,.Lendof_asm_divu_loop_up-asm_divu_loop_up

# extern uintD asm_divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
        .align 4
        .type asm_divucopy_loop_up,#function
asm_divucopy_loop_up: # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#       srl %o3,0,%o3           # zero-extend %o3 = len
        brz,pn %o3,.Ll94
       _ mov 0,%o4              # Rest
#       srl %o0,0,%o0           # zero-extend %o0 = digit
.Ll93:    lduw [%o1],%o5        # nächstes Digit
          add %o1,4,%o1
          sllx %o4,32,%o4       # Rest als High-Digit
          or %o4,%o5,%o4        # zusammen
          udivx %o4,%o0,%o5     # durch digit dividieren
          st %o5,[%o2]          # Quotient ablegen
          umul %o0,%o5,%g1
          sub %o4,%g1,%o4       # Rest in den unteren 32 Bit von %o4
          subcc %o3,1,%o3
          bne,pt %xcc,.Ll93
         _ add %o2,4,%o2
.Ll94:  retl
       _ srl %o4,0,%o0          # Rest als Ergebnis
.Lendof_asm_divucopy_loop_up:
        .size asm_divucopy_loop_up,.Lendof_asm_divucopy_loop_up-asm_divucopy_loop_up

#endif

