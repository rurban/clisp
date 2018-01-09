# Externe Routinen zu ARILEV1.D
# Prozessor: 80386 im native mode
# Assembler-Syntax: GNU oder SUN, Moves von links nach rechts
# Compiler: GNU-C oder SUN-C
# Parameter passing conventions:
#   Argument registers:
#     none, all arguments are passed on the stack 4(%esp),8(%esp),...
#   Return value register:
#     %eax for a single word, %eax,%edx for a 'long long'.
#   Call-used registers (do not have to be preserved across function calls):
#     %eax,%edx,%ecx
# Settings: intCsize=32, intDsize=32.

# Bruno Haible 1992-2001, 2017-2018
# Zum Teil abgeschrieben von Bernhard Degels "v-i386.s"

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

  !!if defined(__EMX__)
    # Direction-Flag ist defaultmäßig gelöscht
    !!define dir0start
    !!define dir0end
    !!define dir1start  std
    !!define dir1end    cld
  !!elif 1
    # Wir gehen auf Nummer sicher.
    !!define dir0start  cld
    !!define dir0end
    !!define dir1start  std
    !!define dir1end    cld
  !!else
    # Direction-Flag darf nach Belieben modifiziert werden
    !!define dir0start  cld
    !!define dir0end
    !!define dir1start  std
    !!define dir1end
  !!endif

            .text

            .globl asm_mulu32_64
            .globl asm_divu_6432_3232_
            .globl asm_copy_loop_up
            .globl asm_copy_loop_down
            .globl asm_fill_loop_up
            .globl asm_fill_loop_down
            .globl asm_clear_loop_up
            .globl asm_clear_loop_down
            .globl asm_or_loop_up
            .globl asm_xor_loop_up
            .globl asm_and_loop_up
            .globl asm_eqv_loop_up
            .globl asm_nand_loop_up
            .globl asm_nor_loop_up
            .globl asm_andc2_loop_up
            .globl asm_orc2_loop_up
            .globl asm_not_loop_up
            .globl asm_and_test_loop_up
            .globl asm_test_loop_up
            .globl asm_compare_loop_up
            .globl asm_add_loop_down
            .globl asm_addto_loop_down
            .globl asm_inc_loop_down
            .globl asm_sub_loop_down
            .globl asm_subx_loop_down
            .globl asm_subfrom_loop_down
            .globl asm_dec_loop_down
            .globl asm_neg_loop_down
            .globl asm_shift1left_loop_down
            .globl asm_shiftleft_loop_down
            .globl asm_shiftleftcopy_loop_down
            .globl asm_shift1right_loop_up
            .globl asm_shiftright_loop_up
            .globl asm_shiftrightsigned_loop_up
            .globl asm_shiftrightcopy_loop_up
            .globl asm_mulusmall_loop_down
            .globl asm_mulu_loop_down
            .globl asm_muluadd_loop_down
            .globl asm_mulusub_loop_down
            .globl asm_divu_loop_up
            .globl asm_divucopy_loop_up

/* Note: When using GNU C or the INTEL compiler, this function is not used,
   because we implement mulu32() as a macro that multiplies inline.  */
# extern uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2);
# 2^32*hi+lo := arg1*arg2.
            .p2align 2,,3
            .type asm_mulu32_64,@function
asm_mulu32_64:
            movl    4(%esp),%eax    # arg1
            mull    8(%esp)         # %edx|%eax := arg1 * arg2
            ret                     # %edx,%eax = 2^32*hi+lo als Ergebnis
.Lendof_asm_mulu32_64:
            .size asm_mulu32_64,.Lendof_asm_mulu32_64-asm_mulu32_64

/* Note: When using GNU C or the INTEL compiler, this function is not used,
   because we implement divu_6432_3232() as a macro that divides inline.  */
# extern uint64 [struct { uint32 q; uint32 r; }] asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
# x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^32*y .
            .p2align 2,,3
            .type asm_divu_6432_3232_,@function
asm_divu_6432_3232_:               # Output in %eax=q, %edx=r
            movl    4(%esp),%edx
            movl    8(%esp),%eax
            divl    12(%esp)       # x = %edx|%eax durch dividieren
            ret                    # Quotient %eax = q, Rest %edx = r als Ergebnis
.Lendof_asm_divu_6432_3232_:
            .size asm_divu_6432_3232_,.Lendof_asm_divu_6432_3232_-asm_divu_6432_3232_

# extern uintD* asm_copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_copy_loop_up,@function
asm_copy_loop_up:
            movl    %edi,%edx       # %edi retten
            movl    %esi,%eax       # %esi retten
            movl    4(%esp),%esi    # %esi = sourceptr
            movl    8(%esp),%edi    # %edi = destptr
            movl    12(%esp),%ecx   # %ecx = count
            dir0start
            rep ; movsl             # %ecx mal aufwärts (%edi) := (%esi)
            dir0end
            movl    %eax,%esi       # %esi zurück
            movl    %edi,%eax       # %edi als Ergebnis
            movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_copy_loop_up:
            .size asm_copy_loop_up,.Lendof_asm_copy_loop_up-asm_copy_loop_up

# extern uintD* asm_copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_copy_loop_down,@function
asm_copy_loop_down:
            movl    %edi,%edx       # %edi retten
            movl    %esi,%eax       # %esi retten
            movl    4(%esp),%esi    # %esi = sourceptr
            movl    8(%esp),%edi    # %edi = destptr
            movl    12(%esp),%ecx   # %ecx = count
            leal    -4(%esi),%esi
            leal    -4(%edi),%edi
            dir1start
            rep ; movsl             # %ecx mal abwärts (%edi) := (%esi)
            dir1end
            movl    %eax,%esi       # %esi zurück
            leal    4(%edi),%eax    # %edi als Ergebnis
            movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_copy_loop_down:
            .size asm_copy_loop_down,.Lendof_asm_copy_loop_down-asm_copy_loop_down

# extern uintD* asm_fill_loop_up (uintD* destptr, uintC count, uintD filler);
            .p2align 2,,3
            .type asm_fill_loop_up,@function
asm_fill_loop_up:
            movl    %edi,%edx       # %edi retten
            movl    4(%esp),%edi    # %edi = destptr
            movl    8(%esp),%ecx    # %ecx = count
            movl    12(%esp),%eax   # %eax = filler
            dir0start
            rep ; stosl             # %ecx mal aufwärts (%edi) := %eax
            dir0end
            movl    %edi,%eax       # %edi als Ergebnis
            movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_fill_loop_up:
            .size asm_fill_loop_up,.Lendof_asm_fill_loop_up-asm_fill_loop_up

# extern uintD* asm_fill_loop_down (uintD* destptr, uintC count, uintD filler);
            .p2align 2,,3
            .type asm_fill_loop_down,@function
asm_fill_loop_down:
            movl    %edi,%edx       # %edi retten
            movl    4(%esp),%edi    # %edi = destptr
            movl    8(%esp),%ecx    # %ecx = count
            movl    12(%esp),%eax   # %eax = filler
            leal    -4(%edi),%edi
            dir1start
            rep ; stosl             # %ecx mal abwärts (%edi) := %eax
            dir1end
            leal    4(%edi),%eax    # %edi als Ergebnis
            movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_fill_loop_down:
            .size asm_fill_loop_down,.Lendof_asm_fill_loop_down-asm_fill_loop_down

# extern uintD* asm_clear_loop_up (uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_clear_loop_up,@function
asm_clear_loop_up:
            movl    %edi,%edx       # %edi retten
            movl    4(%esp),%edi    # %edi = destptr
            movl    8(%esp),%ecx    # %ecx = count
            xorl    %eax,%eax       # %eax = 0
            dir0start
            rep ; stosl             # %ecx mal aufwärts (%edi) := %eax
            dir0end
            movl    %edi,%eax       # %edi als Ergebnis
            movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_clear_loop_up:
            .size asm_clear_loop_up,.Lendof_asm_clear_loop_up-asm_clear_loop_up

# extern uintD* asm_clear_loop_down (uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_clear_loop_down,@function
asm_clear_loop_down:
            movl    %edi,%edx       # %edi retten
            movl    4(%esp),%edi    # %edi = destptr
            movl    8(%esp),%ecx    # %ecx = count
            leal    -4(%edi),%edi
            xorl    %eax,%eax       # %eax = 0
            dir1start
            rep ; stosl             # %ecx mal abwärts (%edi) := %eax
            dir1end
            leal    4(%edi),%eax    # %edi als Ergebnis
            movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_clear_loop_down:
            .size asm_clear_loop_down,.Lendof_asm_clear_loop_down-asm_clear_loop_down

# extern void asm_or_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_or_loop_up,@function
asm_or_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Lolu2          # %ecx = 0 ?
.Lolu1:       movl    (%edx,%esi),%eax # *yptr
              orl     %eax,(%edx)      # *xptr |= ...
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Lolu1
.Lolu2:     popl    %esi            # %esi zurück
            ret
.Lendof_asm_or_loop_up:
            .size asm_or_loop_up,.Lendof_asm_or_loop_up-asm_or_loop_up

# extern void asm_xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_xor_loop_up,@function
asm_xor_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Lxlu2          # %ecx = 0 ?
.Lxlu1:       movl    (%edx,%esi),%eax # *yptr
              xorl    %eax,(%edx)      # *xptr ^= ...
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Lxlu1
.Lxlu2:     popl    %esi            # %esi zurück
            ret
.Lendof_asm_xor_loop_up:
            .size asm_xor_loop_up,.Lendof_asm_xor_loop_up-asm_xor_loop_up

# extern void asm_and_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_and_loop_up,@function
asm_and_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Lalu2          # %ecx = 0 ?
.Lalu1:       movl    (%edx,%esi),%eax # *yptr
              andl    %eax,(%edx)      # *xptr &= ...
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Lalu1
.Lalu2:     popl    %esi            # %esi zurück
            ret
.Lendof_asm_and_loop_up:
            .size asm_and_loop_up,.Lendof_asm_and_loop_up-asm_and_loop_up

# extern void asm_eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_eqv_loop_up,@function
asm_eqv_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Lelu2          # %ecx = 0 ?
.Lelu1:       movl    (%edx),%eax      # *xptr
              xorl    (%edx,%esi),%eax # ^ *yptr
              notl    %eax             # ~(...)
              movl    %eax,(%edx)      # =: *xptr
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Lelu1
.Lelu2:     popl    %esi            # %esi zurück
            ret
.Lendof_asm_eqv_loop_up:
            .size asm_eqv_loop_up,.Lendof_asm_eqv_loop_up-asm_eqv_loop_up

# extern void asm_nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_nand_loop_up,@function
asm_nand_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Lnalu2         # %ecx = 0 ?
.Lnalu1:      movl    (%edx),%eax      # *xptr
              andl    (%edx,%esi),%eax # & *yptr
              notl    %eax             # ~(...)
              movl    %eax,(%edx)      # =: *xptr
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Lnalu1
.Lnalu2:    popl    %esi            # %esi zurück
            ret
.Lendof_asm_nand_loop_up:
            .size asm_nand_loop_up,.Lendof_asm_nand_loop_up-asm_nand_loop_up

# extern void asm_nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_nor_loop_up,@function
asm_nor_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Lnolu2         # %ecx = 0 ?
.Lnolu1:      movl    (%edx),%eax      # *xptr
              orl     (%edx,%esi),%eax # | *yptr
              notl    %eax             # ~(...)
              movl    %eax,(%edx)      # =: *xptr
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Lnolu1
.Lnolu2:    popl    %esi            # %esi zurück
            ret
.Lendof_asm_nor_loop_up:
            .size asm_nor_loop_up,.Lendof_asm_nor_loop_up-asm_nor_loop_up

# extern void asm_andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_andc2_loop_up,@function
asm_andc2_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Laclu2         # %ecx = 0 ?
.Laclu1:      movl    (%edx,%esi),%eax # *yptr
              notl    %eax             # ~ *yptr
              andl    %eax,(%edx)      # *xptr &= ...
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Laclu1
.Laclu2:    popl    %esi            # %esi zurück
            ret
.Lendof_asm_andc2_loop_up:
            .size asm_andc2_loop_up,.Lendof_asm_andc2_loop_up-asm_andc2_loop_up

# extern void asm_orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_orc2_loop_up,@function
asm_orc2_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edx,%esi
            jecxz   .Loclu2         # %ecx = 0 ?
.Loclu1:      movl    (%edx,%esi),%eax # *yptr
              notl    %eax             # ~ *yptr
              orl     %eax,(%edx)      # *xptr |= ...
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Loclu1
.Loclu2:    popl    %esi            # %esi zurück
            ret
.Lendof_asm_orc2_loop_up:
            .size asm_orc2_loop_up,.Lendof_asm_orc2_loop_up-asm_orc2_loop_up

# extern void asm_not_loop_up (uintD* xptr, uintC count);
            .p2align 2,,3
            .type asm_not_loop_up,@function
asm_not_loop_up:
            movl    4(%esp),%edx    # %edx = xptr
            movl    8(%esp),%ecx    # %ecx = count
            jecxz   .Lnlu2          # %ecx = 0 ?
            nop ; nop ; nop ; nop ; nop ; nop
.Lnlu1:       notl    (%edx)           # ~= *xptr
              leal    4(%edx),%edx     # xptr++
              decl    %ecx
              jnz     .Lnlu1
.Lnlu2:     ret
.Lendof_asm_not_loop_up:
            .size asm_not_loop_up,.Lendof_asm_not_loop_up-asm_not_loop_up

# extern bool asm_and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_and_test_loop_up,@function
asm_and_test_loop_up:
            pushl   %esi            # %esi retten
            movl    8(%esp),%edx    # %edx = xptr
            movl    12(%esp),%esi   # %esi = yptr
            movl    16(%esp),%ecx   # %ecx = count
            jecxz   .Latlu2         # %ecx = 0 ?
            subl    %edx,%esi
.Latlu1:      movl    (%edx,%esi),%eax # *yptr
              andl    (%edx),%eax      # *xptr & ...
              jnz     .Latlu3
              leal    4(%edx),%edx     # xptr++, yptr++
              decl    %ecx
              jnz     .Latlu1
.Latlu2:    xorl    %eax,%eax       # Ergebnis 0
.Latlu3:    popl    %esi            # %esi zurück
            ret
.Lendof_asm_and_test_loop_up:
            .size asm_and_test_loop_up,.Lendof_asm_and_test_loop_up-asm_and_test_loop_up

# extern bool asm_test_loop_up (uintD* ptr, uintC count);
            .p2align 2,,3
            .type asm_test_loop_up,@function
asm_test_loop_up:
            movl    %edi,%edx       # %edi retten
            movl    4(%esp),%edi    # %edi = ptr
            movl    8(%esp),%ecx    # %ecx = count
            xorl    %eax,%eax       # %eax = 0
            dir0start
            repz ; scasl            # Falls %ecx > 0:
                                    # %ecx mal aufwärts (%edi) testen
                                    # und weiterschleifen, falls Z, d.h. (%edi)=0.
            dir0end
            # Noch ist %eax = 0.
            jz      .Ltlu1          # alles =0 -> Ergebnis 0
            incl    %eax            # Ergebnis 1
.Ltlu1:     movl    %edx,%edi       # %edi zurück
            ret
.Lendof_asm_test_loop_up:
            .size asm_test_loop_up,.Lendof_asm_test_loop_up-asm_test_loop_up

# extern signean asm_compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
            .p2align 2,,3
            .type asm_compare_loop_up,@function
asm_compare_loop_up:
            movl    %esi,%edx       # %esi retten
            movl    %edi,%eax       # %edi retten
            movl    4(%esp),%esi    # %esi = xptr
            movl    8(%esp),%edi    # %edi = yptr
            movl    12(%esp),%ecx   # %ecx = count
            dir0start
            repz ; cmpsl            # Falls %ecx > 0:
                                    # %ecx mal aufwärts (%edi) und (%esi) vergleichen
                                    # und weiterschleifen, falls Z, d.h. (%edi)=(%esi).
            dir0end
            # Flags -> Ergebnis:
            # Z,NC -> bis zum Schluss (%esi)-(%edi) = 0 -> x=y -> Ergebnis 0
            # NZ,C -> schließlich (%esi)-(%edi) < 0 -> x<y -> Ergebnis -1
            # NZ,NC -> schließlich (%esi)-(%edi) > 0 -> x>y -> Ergebnis +1
            movl    %eax,%edi       # %edi zurück
            movl    %edx,%esi       # %esi zurück
            jbe     .Lcmlu1         # "be" = Z oder C
            movl    $1,%eax         # Ergebnis +1
            ret
.Lcmlu1:    sbbl    %eax,%eax       # Ergebnis -1 (falls C) oder 0 (falls NC)
            ret
.Lendof_asm_compare_loop_up:
            .size asm_compare_loop_up,.Lendof_asm_compare_loop_up-asm_compare_loop_up

# extern uintD asm_add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_add_loop_down,@function
asm_add_loop_down:
            pushl   %esi            # %esi retten
            pushl   %edi            # %edi retten
            movl    12(%esp),%edx   # %edx = sourceptr1
            movl    16(%esp),%esi   # %esi = sourceptr2
            movl    20(%esp),%edi   # %edi = destptr
            movl    24(%esp),%ecx   # %ecx = count
            subl    %edi,%edx
            subl    %edi,%esi
            orl     %ecx,%ecx       # %ecx = 0 ?, Carry löschen
            jz      .Lald2
.Lald1:       leal    -4(%edi),%edi   # sourceptr1--, sourceptr2--, destptr--
              movl    (%edx,%edi),%eax # *sourceptr1
              adcl    (%esi,%edi),%eax # + *sourceptr2 + carry
              movl    %eax,(%edi)     # =: *destptr, neuen Carry behalten
              decl    %ecx
              jnz     .Lald1
.Lald2:     sbbl    %eax,%eax      # Ergebnis := - Carry
            popl    %edi           # %edi zurück
            popl    %esi           # %esi zurück
            ret
.Lendof_asm_add_loop_down:
            .size asm_add_loop_down,.Lendof_asm_add_loop_down-asm_add_loop_down

# extern uintD asm_addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_addto_loop_down,@function
asm_addto_loop_down:
            pushl   %edi            # %edi retten
            movl    8(%esp),%edx    # %edx = sourceptr
            movl    12(%esp),%edi   # %edi = destptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edi,%edx
            orl     %ecx,%ecx       # %ecx = 0 ?, Carry löschen
            jz      .Latld2
.Latld1:      leal    -4(%edi),%edi   # sourceptr--, destptr--
              movl    (%edx,%edi),%eax # *sourceptr
              adcl    %eax,(%edi)     # + *destptr + carry =: *destptr, neuer Carry
              decl    %ecx
              jnz     .Latld1
.Latld2:    sbbl    %eax,%eax       # Ergebnis := - Carry
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_addto_loop_down:
            .size asm_addto_loop_down,.Lendof_asm_addto_loop_down-asm_addto_loop_down

# extern uintD asm_inc_loop_down (uintD* ptr, uintC count);
            .p2align 2,,3
            .type asm_inc_loop_down,@function
asm_inc_loop_down:
            movl    4(%esp),%edx    # %edx = ptr
            movl    8(%esp),%ecx    # %ecx = count
            jecxz   .Lild2          # %ecx = 0 ?
.Lild1:       leal    -4(%edx),%edx
              addl    $1,(%edx)       # (*ptr)++
              jnc     .Lild3          # kein Carry -> fertig
              decl    %ecx
              jnz     .Lild1
.Lild2:     movl    $1,%eax         # Ergebnis := 1
            ret
.Lild3:     xorl    %eax,%eax       # Ergebnis := 0
            ret
.Lendof_asm_inc_loop_down:
            .size asm_inc_loop_down,.Lendof_asm_inc_loop_down-asm_inc_loop_down

# extern uintD asm_sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_sub_loop_down,@function
asm_sub_loop_down:
            pushl   %esi            # %esi retten
            pushl   %edi            # %edi retten
            movl    12(%esp),%edx   # %edx = sourceptr1
            movl    16(%esp),%esi   # %esi = sourceptr2
            movl    20(%esp),%edi   # %edi = destptr
            movl    24(%esp),%ecx   # %ecx = count
            subl    %edi,%edx
            subl    %edi,%esi
            orl     %ecx,%ecx       # %ecx = 0 ?, Carry löschen
            jz      .Lsld2
.Lsld1:       leal    -4(%edi),%edi   # sourceptr1--, sourceptr2--, destptr--
              movl    (%edx,%edi),%eax # *sourceptr1
              sbbl    (%esi,%edi),%eax # - *sourceptr2 - carry
              movl    %eax,(%edi)     # =: *destptr, neuen Carry behalten
              decl    %ecx
              jnz     .Lsld1
.Lsld2:     sbbl    %eax,%eax      # Ergebnis := - Carry
            popl    %edi           # %edi zurück
            popl    %esi           # %esi zurück
            ret
.Lendof_asm_sub_loop_down:
            .size asm_sub_loop_down,.Lendof_asm_sub_loop_down-asm_sub_loop_down

# extern uintD asm_subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
            .p2align 2,,3
            .type asm_subx_loop_down,@function
asm_subx_loop_down:
            pushl   %esi            # %esi retten
            pushl   %edi            # %edi retten
            movl    12(%esp),%edx   # %edx = sourceptr1
            movl    16(%esp),%esi   # %esi = sourceptr2
            movl    20(%esp),%edi   # %edi = destptr
            movl    24(%esp),%ecx   # %ecx = count
            jecxz   .Lsxld2         # %ecx = 0 ?
            subl    %edi,%edx
            subl    %edi,%esi
            movl    28(%esp),%eax   # carry, 0 oder -1
            addl    %eax,%eax       # Bit 31 davon in den Carry
            nop ; nop
.Lsxld1:      leal    -4(%edi),%edi   # sourceptr1--, sourceptr2--, destptr--
              movl    (%edx,%edi),%eax # *sourceptr1
              sbbl    (%esi,%edi),%eax # - *sourceptr2 - carry
              movl    %eax,(%edi)     # =: *destptr, neuen Carry behalten
              decl    %ecx
              jnz     .Lsxld1
            sbbl    %eax,%eax      # Ergebnis := - Carry
            popl    %edi           # %edi zurück
            popl    %esi           # %esi zurück
            ret
.Lsxld2:    movl    28(%esp),%eax  # Ergebnis := carry
            popl    %edi           # %edi zurück
            popl    %esi           # %esi zurück
            ret
.Lendof_asm_subx_loop_down:
            .size asm_subx_loop_down,.Lendof_asm_subx_loop_down-asm_subx_loop_down

# extern uintD asm_subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
            .p2align 2,,3
            .type asm_subfrom_loop_down,@function
asm_subfrom_loop_down:
            pushl   %edi            # %edi retten
            movl    8(%esp),%edx    # %edx = sourceptr
            movl    12(%esp),%edi   # %edi = destptr
            movl    16(%esp),%ecx   # %ecx = count
            subl    %edi,%edx
            orl     %ecx,%ecx       # %ecx = 0 ?, Carry löschen
            jz      .Lsfld2
.Lsfld1:      leal    -4(%edi),%edi   # sourceptr--, destptr--
              movl    (%edx,%edi),%eax # *sourceptr
              sbbl    %eax,(%edi)     # *destptr - *sourceptr - carry =: *destptr, neuer Carry
              decl    %ecx
              jnz     .Lsfld1
.Lsfld2:    sbbl    %eax,%eax       # Ergebnis := - Carry
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_subfrom_loop_down:
            .size asm_subfrom_loop_down,.Lendof_asm_subfrom_loop_down-asm_subfrom_loop_down

# extern uintD asm_dec_loop_down (uintD* ptr, uintC count);
            .p2align 2,,3
            .type asm_dec_loop_down,@function
asm_dec_loop_down:
            movl    4(%esp),%edx    # %edx = ptr
            movl    8(%esp),%ecx    # %ecx = count
            jecxz   .Ldld2          # %ecx = 0 ?
.Ldld1:       leal    -4(%edx),%edx
              subl    $1,(%edx)       # (*ptr)--
              jnc     .Ldld3          # kein Carry -> fertig
              decl    %ecx
              jnz     .Ldld1
.Ldld2:     movl    $-1,%eax        # Ergebnis := -1
            ret
.Ldld3:     xorl    %eax,%eax       # Ergebnis := 0
            ret
.Lendof_asm_dec_loop_down:
            .size asm_dec_loop_down,.Lendof_asm_dec_loop_down-asm_dec_loop_down

# extern uintD asm_neg_loop_down (uintD* ptr, uintC count);
            .p2align 2,,3
            .type asm_neg_loop_down,@function
asm_neg_loop_down:
            movl    4(%esp),%edx    # %edx = ptr
            movl    8(%esp),%ecx    # %ecx = count
            # erstes Digit /=0 suchen:
            jecxz   .Lnld2          # %ecx = 0 ?
.Lnld1:       leal    -4(%edx),%edx
              negl    (%edx)
              jnz     .Lnld3
              decl    %ecx
              jnz     .Lnld1
.Lnld2:     xorl    %eax,%eax       # Ergebnis := 0
            ret
            nop ; nop ; nop ; nop ; nop ; nop
.Lnld3:     # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
            # alle anderen Digits invertieren:
            decl    %ecx
            jz      .Lnld5
.Lnld4:       leal    -4(%edx),%edx
              notl    (%edx)
              decl    %ecx
              jnz     .Lnld4
.Lnld5:     movl    $-1,%eax        # Ergebnis := -1
            ret
.Lendof_asm_neg_loop_down:
            .size asm_neg_loop_down,.Lendof_asm_neg_loop_down-asm_neg_loop_down

# extern uintD asm_shift1left_loop_down (uintD* ptr, uintC count);
            .p2align 2,,3
            .type asm_shift1left_loop_down,@function
asm_shift1left_loop_down:
            movl    4(%esp),%edx    # %edx = ptr
            movl    8(%esp),%ecx    # %ecx = count
            orl     %ecx,%ecx       # %ecx = 0 ?, Carry löschen
            jz      .Ls1lld2
            nop ; nop ; nop ; nop
.Ls1lld1:     leal    -4(%edx),%edx   # ptr--
              rcll    $1,(%edx)       # *ptr und Carry um 1 Bit links rotieren
              decl    %ecx
              jnz     .Ls1lld1
.Ls1lld2:   sbbl    %eax,%eax       # Ergebnis := - Carry
            ret
.Lendof_asm_shift1left_loop_down:
            .size asm_shift1left_loop_down,.Lendof_asm_shift1left_loop_down-asm_shift1left_loop_down

# extern uintD asm_shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
            .p2align 2,,3
            .type asm_shiftleft_loop_down,@function
asm_shiftleft_loop_down:
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    12(%esp),%edi   # %edi = ptr
            movl    16(%esp),%edx   # %edx = count
            movl    20(%esp),%ecx   # %cl = i
            orl     %edx,%edx       # count = 0 ?
            jz      .Lslld4
            # erstes Digit shiften:
            leal    -4(%edi),%edi
            movl    (%edi),%eax     # Digit in %eax halten
            movl    %eax,%ebx       # und in %ebx rechnen:
            shll    %cl,%ebx        # um i Bits links shiften
            orl     24(%esp),%ebx   # und die unteren i Bits eintragen
            movl    %ebx,(%edi)     # und wieder ablegen
            # Letztes Digit in %eax.
            decl    %edx
            jz      .Lslld2
            nop ; nop ; nop ; nop
.Lslld1:      # weiteres Digit shiften:
              leal    -4(%edi),%edi
              movl    (%edi),%ebx
              shldl   %cl,%eax,(%edi) # (%edi) um %cl=i Bits links shiften, %eax von rechts reinshiften
              # Letztes Digit in %ebx.
              decl    %edx
              jz      .Lslld3
              # weiteres Digit shiften:
              leal    -4(%edi),%edi
              movl    (%edi),%eax
              shldl   %cl,%ebx,(%edi) # (%edi) um %cl=i Bits links shiften, %ebx von rechts reinshiften
              # Letztes Digit in %eax.
              decl    %edx
              jnz     .Lslld1
.Lslld2:    movl    %eax,%ebx
.Lslld3:    xorl    %eax,%eax       # %eax := 0
            shldl   %cl,%ebx,%eax   # %eax := höchste %cl=i Bits von %ebx
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            ret
.Lslld4:    movl    24(%esp),%eax   # %eax := carry
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_shiftleft_loop_down:
            .size asm_shiftleft_loop_down,.Lendof_asm_shiftleft_loop_down-asm_shiftleft_loop_down

# extern uintD asm_shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
            .p2align 2,,3
            .type asm_shiftleftcopy_loop_down,@function
asm_shiftleftcopy_loop_down:
            pushl   %esi            # %esi retten
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    16(%esp),%esi   # %esi = sourceptr
            movl    20(%esp),%edi   # %edi = destptr
            movl    24(%esp),%edx   # count
            movl    28(%esp),%ecx   # %cl = i
            orl     %edx,%edx       # count = 0 ?
            jz      .Lslcld4
            subl    %edi,%esi
            # erstes Digit shiften:
            leal    -4(%edi),%edi   # sourceptr--, destptr--
            movl    (%edi,%esi),%ebx # *sourceptr in %ebx halten
            movl    %ebx,%eax       # und in %eax rechnen:
            shll    %cl,%eax        # um i Bits links shiften, rechts Nullen rein
            movl    %eax,(%edi)     # und als *destptr ablegen
            # Letztes Digit in %ebx.
            negb    %cl             # %cl = 32-i
            decl    %edx
            jz      .Lslcld2
.Lslcld1:     # weiteres Digit shiften:
              leal    -4(%edi),%edi   # sourceptr--, destptr--
              movl    (%edi,%esi),%eax # nächstes Digit nach %eax
              shrdl   %cl,%eax,%ebx   # %ebx um %cl=32-i Bits rechts shiften, %eax von links reinshiften
              movl    %ebx,(%edi)     # %ebx als *destptr ablegen
              # Letztes Digit in %eax.
              decl    %edx
              jz      .Lslcld3
              # weiteres Digit shiften:
              leal    -4(%edi),%edi   # sourceptr--, destptr--
              movl    (%edi,%esi),%ebx # nächstes Digit nach %ebx
              shrdl   %cl,%ebx,%eax   # %eax um %cl=32-i Bits rechts shiften, %ebx von links reinshiften
              movl    %eax,(%edi)     # %eax als *destptr ablegen
              # Letztes Digit in %ebx.
              decl    %edx
              jnz     .Lslcld1
.Lslcld2:   movl    %ebx,%eax
.Lslcld3:   shrl    %cl,%eax        # %eax um 32-i Bits nach rechts shiften
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            popl    %esi            # %esi zurück
            ret
.Lslcld4:   xorl    %eax,%eax       # %eax := 0
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            popl    %esi            # %esi zurück
            ret
.Lendof_asm_shiftleftcopy_loop_down:
            .size asm_shiftleftcopy_loop_down,.Lendof_asm_shiftleftcopy_loop_down-asm_shiftleftcopy_loop_down

# extern uintD asm_shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
            .p2align 2,,3
            .type asm_shift1right_loop_up,@function
asm_shift1right_loop_up:
            movl    4(%esp),%edx    # %edx = ptr
            movl    8(%esp),%ecx    # %ecx = count
            movl    12(%esp),%eax   # %eax = carry (0 oder -1)
            jecxz   .Ls1rld3        # %ecx = 0 ?
            addl    %eax,%eax       # Carry := Bit 31 von carry
.Ls1rld1:     rcrl    $1,(%edx)       # *ptr und Carry um 1 Bit rechts rotieren
              leal    4(%edx),%edx    # ptr++
              decl    %ecx
              jnz     .Ls1rld1
.Ls1rld2:   sbbl    %eax,%eax       # Ergebnis := - Carry
.Ls1rld3:   ret
.Lendof_asm_shift1right_loop_up:
            .size asm_shift1right_loop_up,.Lendof_asm_shift1right_loop_up-asm_shift1right_loop_up

# extern uintD asm_shiftright_loop_up (uintD* ptr, uintC count, uintC i);
            .p2align 2,,3
            .type asm_shiftright_loop_up,@function
asm_shiftright_loop_up:
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    12(%esp),%edi   # %edi = ptr
            movl    16(%esp),%edx   # %edx = count
            movl    20(%esp),%ecx   # %cl = i
            orl     %edx,%edx       # count = 0 ?
            jz      .Lsrlu4
            # erstes Digit shiften:
            movl    (%edi),%eax     # Digit in %eax halten
            movl    %eax,%ebx       # und in %ebx rechnen:
            shrl    %cl,%ebx        # um i Bits rechts shiften
            movl    %ebx,(%edi)     # und wieder ablegen
            # Letztes Digit in %eax.
            decl    %edx
            jz      .Lsrlu2
            nop ; nop ; nop
.Lsrlu1:      # weiteres Digit shiften:
              leal    4(%edi),%edi
              movl    (%edi),%ebx
              shrdl   %cl,%eax,(%edi) # (%edi) um %cl=i Bits rechts shiften, %eax von links reinshiften
              # Letztes Digit in %ebx.
              decl    %edx
              jz      .Lsrlu3
              # weiteres Digit shiften:
              leal    4(%edi),%edi
              movl    (%edi),%eax
              shrdl   %cl,%ebx,(%edi) # (%edi) um %cl=i Bits rechts shiften, %ebx von links reinshiften
              # Letztes Digit in %eax.
              decl    %edx
              jnz     .Lsrlu1
.Lsrlu2:    movl    %eax,%ebx
.Lsrlu3:    xorl    %eax,%eax       # %eax := 0
            shrdl   %cl,%ebx,%eax   # %eax := niedrigste %cl=i Bits von %ebx, als Bits 31..32-i
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            ret
.Lsrlu4:    xorl    %eax,%eax       # %eax := 0
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_shiftright_loop_up:
            .size asm_shiftright_loop_up,.Lendof_asm_shiftright_loop_up-asm_shiftright_loop_up

# extern uintD asm_shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
            .p2align 2,,3
            .type asm_shiftrightsigned_loop_up,@function
asm_shiftrightsigned_loop_up:
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    12(%esp),%edi   # %edi = ptr
            movl    16(%esp),%edx   # %edx = count
            movl    20(%esp),%ecx   # %cl = i
            # erstes Digit shiften:
            movl    (%edi),%eax     # Digit in %eax halten
            movl    %eax,%ebx       # und in %ebx rechnen:
            sarl    %cl,%ebx        # um i Bits rechts shiften, Vorzeichen vervielfachen
            movl    %ebx,(%edi)     # und wieder ablegen
            # Letztes Digit in %eax.
            decl    %edx
            jz      .Lsrslu2
.Lsrslu1:     # weiteres Digit shiften:
              leal    4(%edi),%edi
              movl    (%edi),%ebx
              shrdl   %cl,%eax,(%edi) # (%edi) um %cl=i Bits rechts shiften, %eax von links reinshiften
              # Letztes Digit in %ebx.
              decl    %edx
              jz      .Lsrslu3
              # weiteres Digit shiften:
              leal    4(%edi),%edi
              movl    (%edi),%eax
              shrdl   %cl,%ebx,(%edi) # (%edi) um %cl=i Bits rechts shiften, %ebx von links reinshiften
              # Letztes Digit in %eax.
              decl    %edx
              jnz     .Lsrslu1
.Lsrslu2:   movl    %eax,%ebx
.Lsrslu3:   xorl    %eax,%eax       # %eax := 0
            shrdl   %cl,%ebx,%eax   # %eax := niedrigste %cl=i Bits von %ebx, als Bits 31..32-i
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_shiftrightsigned_loop_up:
            .size asm_shiftrightsigned_loop_up,.Lendof_asm_shiftrightsigned_loop_up-asm_shiftrightsigned_loop_up

# extern uintD asm_shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
            .p2align 2,,3
            .type asm_shiftrightcopy_loop_up,@function
asm_shiftrightcopy_loop_up:
            pushl   %esi            # %esi retten
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    16(%esp),%esi   # %esi = sourceptr
            movl    20(%esp),%edi   # %edi = destptr
            movl    24(%esp),%edx   # count
            movl    28(%esp),%ecx   # %cl = i
            negb    %cl             # 32-i
            movl    32(%esp),%eax   # %eax = carry
            orl     %edx,%edx       # count = 0 ?
            jz      .Lsrcld3
            subl    %edi,%esi
            # erstes Digit shiften:
            movl    (%edi,%esi),%ebx # *sourceptr in %ebx halten
            shldl   %cl,%ebx,%eax   # carry um %cl=32-i Bits links shiften, dabei *sourceptr rein
            movl    %eax,(%edi)     # und als *destptr ablegen
            # Letztes Digit in %ebx.
            decl    %edx
            jz      .Lsrcld2
.Lsrcld1:     # weiteres Digit shiften:
              leal    4(%edi),%edi    # sourceptr++, destptr++
              movl    (%edi,%esi),%eax # nächstes Digit nach %eax
              shldl   %cl,%eax,%ebx   # %ebx um %cl=32-i Bits links shiften, %eax von rechts reinshiften
              movl    %ebx,(%edi)     # %ebx als *destptr ablegen
              # Letztes Digit in %eax.
              decl    %edx
              jz      .Lsrcld3
              # weiteres Digit shiften:
              leal    4(%edi),%edi    # sourceptr++, destptr++
              movl    (%edi,%esi),%ebx # nächstes Digit nach %ebx
              shldl   %cl,%ebx,%eax   # %eax um %cl=32-i Bits links shiften, %ebx von rechts reinshiften
              movl    %eax,(%edi)     # %eax als *destptr ablegen
              # Letztes Digit in %ebx.
              decl    %edx
              jnz     .Lsrcld1
.Lsrcld2:   movl    %ebx,%eax
.Lsrcld3:   shll    %cl,%eax        # %eax um 32-i Bits nach links shiften
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            popl    %esi            # %esi zurück
            ret
.Lendof_asm_shiftrightcopy_loop_up:
            .size asm_shiftrightcopy_loop_up,.Lendof_asm_shiftrightcopy_loop_up-asm_shiftrightcopy_loop_up

# extern uintD asm_mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
            .p2align 2,,3
            .type asm_mulusmall_loop_down,@function
asm_mulusmall_loop_down:
            pushl   %ebp            # %ebp retten
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    16(%esp),%ebx   # %ebx = digit
            movl    20(%esp),%edi   # %edi = ptr
            movl    24(%esp),%ecx   # %ecx = len
            movl    28(%esp),%ebp   # %ebp = carry := newdigit
            movl    %ecx,%eax
            negl    %eax            # %eax = -len
            jz      .Lmsld2
            leal    -4(%edi,%eax,4),%edi # %edi = &ptr[-1-len]
            nop ; nop ; nop
.Lmsld1:      movl    (%edi,%ecx,4),%eax # *ptr
              mull    %ebx               # %edx|%eax := digit * *ptr
              addl    %ebp,%eax          # carry und Low-Teil des Produktes addieren
              movl    $0,%ebp
              adcl    %edx,%ebp          # Übertrag zum High-Teil %edx dazu, gibt neuen carry
              movl    %eax,(%edi,%ecx,4) # Low-Teil als *ptr ablegen
              decl    %ecx               # count--, ptr--
              jnz     .Lmsld1
.Lmsld2:    movl    %ebp,%eax       # Ergebnis := letzter Übertrag
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            popl    %ebp            # %ebp zurück
            ret
.Lendof_asm_mulusmall_loop_down:
            .size asm_mulusmall_loop_down,.Lendof_asm_mulusmall_loop_down-asm_mulusmall_loop_down

# extern void asm_mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
            .p2align 2,,3
            .type asm_mulu_loop_down,@function
asm_mulu_loop_down:
            pushl   %ebp            # %ebp retten
            pushl   %edi            # %edi retten
            pushl   %esi            # %esi retten
            pushl   %ebx            # %ebx retten
            movl    20(%esp),%ebx   # %ebx = digit
            movl    24(%esp),%esi   # %esi = sourceptr
            movl    28(%esp),%edi   # %edi = destptr
            movl    32(%esp),%ecx   # %ecx = len
            movl    %ecx,%eax
            notl    %eax            # %eax = -1-len
            leal    (%esi,%eax,4),%esi # %esi = &sourceptr[-1-len]
            leal    (%edi,%eax,4),%edi # %edi = &destptr[-1-len]
            xorl    %ebp,%ebp       # %epb = carry := 0
.Lmuld1:      movl    (%esi,%ecx,4),%eax # *sourceptr
              mull    %ebx               # %edx|%eax := digit * *sourceptr
              addl    %ebp,%eax          # carry und Low-Teil des Produktes addieren
              movl    $0,%ebp
              adcl    %edx,%ebp          # Übertrag zum High-Teil %edx dazu, gibt neuen carry
              movl    %eax,(%edi,%ecx,4) # Low-Teil als *destptr ablegen
              decl    %ecx               # count--, sourceptr--, destptr--
              jnz     .Lmuld1
            movl    %ebp,(%edi)     # letzten Übertrag ablegen
            popl    %ebx            # %ebx zurück
            popl    %esi            # %esi zurück
            popl    %edi            # %edi zurück
            popl    %ebp            # %ebp zurück
            ret
.Lendof_asm_mulu_loop_down:
            .size asm_mulu_loop_down,.Lendof_asm_mulu_loop_down-asm_mulu_loop_down

# extern uintD asm_muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
            .p2align 2,,3
            .type asm_muluadd_loop_down,@function
asm_muluadd_loop_down:
            pushl   %ebp            # %ebp retten
            pushl   %edi            # %edi retten
            pushl   %esi            # %esi retten
            pushl   %ebx            # %ebx retten
            movl    20(%esp),%ebx   # %ebx = digit
            movl    24(%esp),%esi   # %esi = sourceptr
            movl    28(%esp),%edi   # %edi = destptr
            movl    32(%esp),%ecx   # %ecx = len
            movl    %ecx,%eax
            notl    %eax            # %eax = -1-len
            leal    (%esi,%eax,4),%esi # %esi = &sourceptr[-1-len]
            leal    (%edi,%eax,4),%edi # %edi = &destptr[-1-len]
            xorl    %ebp,%ebp       # %epb = carry := 0
.Lmuald1:     movl    (%esi,%ecx,4),%eax # *sourceptr
              mull    %ebx               # %edx|%eax := digit * *sourceptr
              addl    %ebp,%eax          # carry und Low-Teil des Produktes addieren
              movl    $0,%ebp
              adcl    %ebp,%edx          # Übertrag zum High-Teil %edx dazu
              addl    %eax,(%edi,%ecx,4) # Low-Teil zu *destptr addieren
              adcl    %edx,%ebp          # zweiten Übertrag zu %edx addieren, gibt neuen carry
              decl    %ecx               # count--, sourceptr--, destptr--
              jnz     .Lmuald1
            movl    %ebp,%eax       # Ergebnis := letzter Übertrag
            popl    %ebx            # %ebx zurück
            popl    %esi            # %esi zurück
            popl    %edi            # %edi zurück
            popl    %ebp            # %ebp zurück
            ret
.Lendof_asm_muluadd_loop_down:
            .size asm_muluadd_loop_down,.Lendof_asm_muluadd_loop_down-asm_muluadd_loop_down

# extern uintD asm_mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
            .p2align 2,,3
            .type asm_mulusub_loop_down,@function
asm_mulusub_loop_down:
            pushl   %ebp            # %ebp retten
            pushl   %edi            # %edi retten
            pushl   %esi            # %esi retten
            pushl   %ebx            # %ebx retten
            movl    20(%esp),%ebx   # %ebx = digit
            movl    24(%esp),%esi   # %esi = sourceptr
            movl    28(%esp),%edi   # %edi = destptr
            movl    32(%esp),%ecx   # %ecx = len
            movl    %ecx,%eax
            notl    %eax            # %eax = -1-len
            leal    (%esi,%eax,4),%esi # %esi = &sourceptr[-1-len]
            leal    (%edi,%eax,4),%edi # %edi = &destptr[-1-len]
            xorl    %ebp,%ebp       # %epb = carry := 0
.Lmusld1:     movl    (%esi,%ecx,4),%eax # *sourceptr
              mull    %ebx               # %edx|%eax := digit * *sourceptr
              addl    %ebp,%eax          # carry und Low-Teil des Produktes addieren
              movl    $0,%ebp
              adcl    %ebp,%edx          # Übertrag zum High-Teil %edx dazu
              subl    %eax,(%edi,%ecx,4) # Low-Teil von *destptr subtrahieren
              adcl    %edx,%ebp          # zweiten Übertrag zu %edx addieren, gibt neuen carry
              decl    %ecx               # count--, sourceptr--, destptr--
              jnz     .Lmusld1
            movl    %ebp,%eax       # Ergebnis := letzter Übertrag
            popl    %ebx            # %ebx zurück
            popl    %esi            # %esi zurück
            popl    %edi            # %edi zurück
            popl    %ebp            # %ebp zurück
            ret
.Lendof_asm_mulusub_loop_down:
            .size asm_mulusub_loop_down,.Lendof_asm_mulusub_loop_down-asm_mulusub_loop_down

# extern uintD asm_divu_loop_up (uintD digit, uintD* ptr, uintC len);
            .p2align 2,,3
            .type asm_divu_loop_up,@function
asm_divu_loop_up:
            pushl   %edi            # %edi retten
            pushl   %ebx            # %ebx retten
            movl    12(%esp),%ebx   # %ebx = digit
            movl    16(%esp),%edi   # %edi = ptr
            movl    20(%esp),%ecx   # %ecx = len
            xorl    %edx,%edx       # %edx = Rest := 0
            jecxz   .Ldlu2          # %ecx = 0 ?
.Ldlu1:       movl    (%edi),%eax     # nächstes Digit *ptr
              divl    %ebx            # Division von %edx|%eax durch %ebx
              movl    %eax,(%edi)     # Quotient %eax ablegen, Rest in %edx behalten
              leal    4(%edi),%edi    # ptr++
              decl    %ecx
              jnz     .Ldlu1
.Ldlu2:     movl    %edx,%eax       # Ergebnis := letzter Rest
            popl    %ebx            # %ebx zurück
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_divu_loop_up:
            .size asm_divu_loop_up,.Lendof_asm_divu_loop_up-asm_divu_loop_up

# extern uintD asm_divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
            .p2align 2,,3
            .type asm_divucopy_loop_up,@function
asm_divucopy_loop_up:
            pushl   %edi            # %edi retten
            pushl   %esi            # %esi retten
            pushl   %ebx            # %ebx retten
            movl    16(%esp),%ebx   # %ebx = digit
            movl    20(%esp),%esi   # %esi = sourceptr
            movl    24(%esp),%edi   # %edi = destptr
            movl    28(%esp),%ecx   # %ecx = len
            xorl    %edx,%edx       # %edx = Rest := 0
            jecxz   .Ldclu2         # %ecx = 0 ?
            subl    %edi,%esi
.Ldclu1:      movl    (%esi,%edi),%eax # nächstes Digit *ptr
              divl    %ebx            # Division von %edx|%eax durch %ebx
              movl    %eax,(%edi)     # Quotient %eax ablegen, Rest in %edx behalten
              leal    4(%edi),%edi    # sourceptr++, destptr++
              decl    %ecx
              jnz     .Ldclu1
.Ldclu2:    movl    %edx,%eax       # Ergebnis := letzter Rest
            popl    %ebx            # %ebx zurück
            popl    %esi            # %esi zurück
            popl    %edi            # %edi zurück
            ret
.Lendof_asm_divucopy_loop_up:
            .size asm_divucopy_loop_up,.Lendof_asm_divucopy_loop_up-asm_divucopy_loop_up

#endif
