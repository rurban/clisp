#include "asm-sparc.h"
  .register %g2,$scratch

        .section ".text"

        .global C(asm_mulu16_)
        .global C(asm_mulu32_64)
        .global C(asm_mulu32_unchecked)
        .global C(asm_divu_6432_3232_)
        .global C(asm_divu_3216_1616_)
        .global C(asm_copy_loop_up)
        .global C(asm_copy_loop_down)
        .global C(asm_fill_loop_up)
        .global C(asm_fill_loop_down)
        .global C(asm_clear_loop_up)
        .global C(asm_clear_loop_down)
        .global C(asm_or_loop_up)
        .global C(asm_xor_loop_up)
        .global C(asm_and_loop_up)
        .global C(asm_eqv_loop_up)
        .global C(asm_nand_loop_up)
        .global C(asm_nor_loop_up)
        .global C(asm_andc2_loop_up)
        .global C(asm_orc2_loop_up)
        .global C(asm_not_loop_up)
        .global C(asm_and_test_loop_up)
        .global C(asm_test_loop_up)
        .global C(asm_compare_loop_up)
        .global C(asm_add_loop_down)
        .global C(asm_addto_loop_down)
        .global C(asm_inc_loop_down)
        .global C(asm_sub_loop_down)
        .global C(asm_subx_loop_down)
        .global C(asm_subfrom_loop_down)
        .global C(asm_dec_loop_down)
        .global C(asm_neg_loop_down)
        .global C(asm_shift1left_loop_down)
        .global C(asm_shiftleft_loop_down)
        .global C(asm_shiftleftcopy_loop_down)
        .global C(asm_shift1right_loop_up)
        .global C(asm_shiftright_loop_up)
        .global C(asm_shiftrightsigned_loop_up)
        .global C(asm_shiftrightcopy_loop_up)
        .global C(asm_mulusmall_loop_down)
        .global C(asm_mulu_loop_down)
        .global C(asm_muluadd_loop_down)
        .global C(asm_mulusub_loop_down)
        .global C(asm_divu_loop_up)
        .global C(asm_divucopy_loop_up)
        .align 4
        DECLARE_FUNCTION(asm_mulu16_)
FUNBEGIN(asm_mulu16_)
        umul %o0,%o1,%g1
        jmp %o7+8
       srl %g1,0,%o0
L(endof_asm_mulu16_):
        FUNEND(asm_mulu16_)



        .align 4
        DECLARE_FUNCTION(asm_mulu32_64)
FUNBEGIN(asm_mulu32_64)
        umul %o0,%o1,%g1
        rd %y,%o1
        srl %g1,0,%o0
        sllx %o1,32,%o1
        jmp %o7+8
       or %o0,%o1,%o0
L(endof_asm_mulu32_64):
        FUNEND(asm_mulu32_64)



        .align 4
        DECLARE_FUNCTION(asm_mulu32_unchecked)
FUNBEGIN(asm_mulu32_unchecked)
        umul %o0,%o1,%o2
        jmp %o7+8
       srl %o2,0,%o0
L(endof_asm_mulu32_unchecked):
        FUNEND(asm_mulu32_unchecked)



        .align 4
        DECLARE_FUNCTION(asm_divu_6432_3232_)
FUNBEGIN(asm_divu_6432_3232_)
        wr %o0,%g0,%y
        udiv %o1,%o2,%o0
        umul %o0,%o2,%g1
        sub %o1,%g1,%o1
        srl %o0,0,%o0
        sllx %o1,32,%o1
        jmp %o7+8
       or %o0,%o1,%o0
L(endof_asm_divu_6432_3232_):
        FUNEND(asm_divu_6432_3232_)



        .align 4
        DECLARE_FUNCTION(asm_divu_3216_1616_)
FUNBEGIN(asm_divu_3216_1616_)
        wr %g0,%g0,%y
        udiv %o0,%o1,%o2



        umul %o2,%o1,%g1
        sub %o0,%g1,%g1

        sll %g1,16,%g1
        or %o2,%g1,%o0
        jmp %o7+8
       srl %o0,0,%o0
L(endof_asm_divu_3216_1616_):
        FUNEND(asm_divu_3216_1616_)


        .align 4
        DECLARE_FUNCTION(asm_copy_loop_up)
FUNBEGIN(asm_copy_loop_up)


        brz,pn %o2,L(l09)
       nop
L(l08): ld [%o0],%o3
          add %o0,4,%o0
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l08)
         add %o1,4,%o1
L(l09): jmp %o7+8
       mov %o1,%o0
L(endof_asm_copy_loop_up):
        FUNEND(asm_copy_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_copy_loop_down)
FUNBEGIN(asm_copy_loop_down)


        brz,pn %o2,L(l11)
       sub %o0,4,%o0
L(l10): ld [%o0],%o3
          sub %o1,4,%o1
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l10)
         sub %o0,4,%o0
L(l11): jmp %o7+8
       mov %o1,%o0
L(endof_asm_copy_loop_down):
        FUNEND(asm_copy_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_fill_loop_up)
FUNBEGIN(asm_fill_loop_up)


        brz,pn %o1,L(l13)
       nop
L(l12): st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,L(l12)
         add %o0,4,%o0
L(l13): jmp %o7+8
       nop
L(endof_asm_fill_loop_up):
        FUNEND(asm_fill_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_fill_loop_down)
FUNBEGIN(asm_fill_loop_down)


        brz,pn %o1,L(l15)
       sub %o0,4,%o0
L(l14): st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,L(l14)
         sub %o0,4,%o0
L(l15): jmp %o7+8
       add %o0,4,%o0
L(endof_asm_fill_loop_down):
        FUNEND(asm_fill_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_clear_loop_up)
FUNBEGIN(asm_clear_loop_up)


        brz,pn %o1,L(l17)
       nop
L(l16): st %g0,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,L(l16)
         add %o0,4,%o0
L(l17): jmp %o7+8
       nop
L(endof_asm_clear_loop_up):
        FUNEND(asm_clear_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_clear_loop_down)
FUNBEGIN(asm_clear_loop_down)


        brz,pn %o1,L(l19)
       sub %o0,4,%o0
L(l18): st %g0,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,L(l18)
         sub %o0,4,%o0
L(l19): jmp %o7+8
       add %o0,4,%o0
L(endof_asm_clear_loop_down):
        FUNEND(asm_clear_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_or_loop_up)
FUNBEGIN(asm_or_loop_up)


        brz,pn %o2,L(l21)
       nop
L(l20): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l20)
         add %o0,4,%o0
L(l21): jmp %o7+8
       nop
L(endof_asm_or_loop_up):
        FUNEND(asm_or_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_xor_loop_up)
FUNBEGIN(asm_xor_loop_up)


        brz,pn %o2,L(l23)
       nop
L(l22): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l22)
         add %o0,4,%o0
L(l23): jmp %o7+8
       nop
L(endof_asm_xor_loop_up):
        FUNEND(asm_xor_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_and_loop_up)
FUNBEGIN(asm_and_loop_up)


        brz,pn %o2,L(l25)
       nop
L(l24): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l24)
         add %o0,4,%o0
L(l25): jmp %o7+8
       nop
L(endof_asm_and_loop_up):
        FUNEND(asm_and_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_eqv_loop_up)
FUNBEGIN(asm_eqv_loop_up)


        brz,pn %o2,L(l27)
       nop
L(l26): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xnor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l26)
         add %o0,4,%o0
L(l27): jmp %o7+8
       nop
L(endof_asm_eqv_loop_up):
        FUNEND(asm_eqv_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_nand_loop_up)
FUNBEGIN(asm_nand_loop_up)


        brz,pn %o2,L(l29)
       nop
L(l28): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l28)
         add %o0,4,%o0
L(l29): jmp %o7+8
       nop
L(endof_asm_nand_loop_up):
        FUNEND(asm_nand_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_nor_loop_up)
FUNBEGIN(asm_nor_loop_up)


        brz,pn %o2,L(l31)
       nop
L(l30): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l30)
         add %o0,4,%o0
L(l31): jmp %o7+8
       nop
L(endof_asm_nor_loop_up):
        FUNEND(asm_nor_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_andc2_loop_up)
FUNBEGIN(asm_andc2_loop_up)


        brz,pn %o2,L(l33)
       nop
L(l32): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          andn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l32)
         add %o0,4,%o0
L(l33): jmp %o7+8
       nop
L(endof_asm_andc2_loop_up):
        FUNEND(asm_andc2_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_orc2_loop_up)
FUNBEGIN(asm_orc2_loop_up)


        brz,pn %o2,L(l35)
       nop
L(l34): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          orn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l34)
         add %o0,4,%o0
L(l35): jmp %o7+8
       nop
L(endof_asm_orc2_loop_up):
        FUNEND(asm_orc2_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_not_loop_up)
FUNBEGIN(asm_not_loop_up)


        brz,pn %o1,L(l37)
       nop
L(l36): ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne,pt %xcc,L(l36)
         add %o0,4,%o0
L(l37): jmp %o7+8
       nop
L(endof_asm_not_loop_up):
        FUNEND(asm_not_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_and_test_loop_up)
FUNBEGIN(asm_and_test_loop_up)


        brz,pn %o2,L(l39)
       nop
L(l38): ld [%o0],%o3
          ld [%o1],%o4
          add %o0,4,%o0
          andcc %o3,%o4,%g0
          bne,pn %icc,L(l40)
         subcc %o2,1,%o2
          bne,pt %xcc,L(l38)
         add %o1,4,%o1
L(l39): jmp %o7+8
       mov 0,%o0
L(l40): jmp %o7+8
       mov 1,%o0
L(endof_asm_and_test_loop_up):
        FUNEND(asm_and_test_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_test_loop_up)
FUNBEGIN(asm_test_loop_up)


        brz,pn %o1,L(l42)
       nop
          lduw [%o0],%o2
L(l41): add %o0,4,%o0
          brnz,pn %o2,L(l43)
         subcc %o1,1,%o1
          bne,a,pt %xcc,L(l41)
         lduw [%o0],%o2
L(l42): jmp %o7+8
       mov 0,%o0
L(l43): jmp %o7+8
       mov 1,%o0
L(endof_asm_test_loop_up):
        FUNEND(asm_test_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_compare_loop_up)
FUNBEGIN(asm_compare_loop_up)


        brz,pn %o2,L(l45)
       nop
          ld [%o0],%o3
L(l44): ld [%o1],%o4
          add %o0,4,%o0
          subcc %o3,%o4,%g0
          bne,pn %icc,L(l46)
         add %o1,4,%o1
          subcc %o2,1,%o2
          bne,a,pt %xcc,L(l44)
         ld [%o0],%o3
L(l45): jmp %o7+8
       mov 0,%o0
L(l46): mov 1,%o0
        movlu %icc,-1,%o0
        jmp %o7+8
       sra %o0,0,%o0
L(endof_asm_compare_loop_up):
        FUNEND(asm_compare_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_add_loop_down)
FUNBEGIN(asm_add_loop_down)


        brz,pn %o3,L(l49)
       mov %g0,%g1
L(l48): sub %o0,4,%o0
          lduw [%o0],%o4
          sub %o1,4,%o1
          lduw [%o1],%o5
          add %g1,%o4,%g1
          add %g1,%o5,%g1
          sub %o2,4,%o2
          st %g1,[%o2]
          subcc %o3,1,%o3
          bne,pt %xcc,L(l48)
         srlx %g1,32,%g1
L(l49): jmp %o7+8
       mov %g1,%o0
L(endof_asm_add_loop_down):
        FUNEND(asm_add_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_addto_loop_down)
FUNBEGIN(asm_addto_loop_down)


        brz,pn %o2,L(l51)
       mov %g0,%o5
L(l50): sub %o0,4,%o0
          lduw [%o0],%o3
          sub %o1,4,%o1
          lduw [%o1],%o4
          add %o5,%o3,%o5
          add %o5,%o4,%o5
          st %o5,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l50)
         srlx %o5,32,%o5
L(l51): jmp %o7+8
       mov %o5,%o0
L(endof_asm_addto_loop_down):
        FUNEND(asm_addto_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_inc_loop_down)
FUNBEGIN(asm_inc_loop_down)


        brz,pn %o1,L(l53)
       sub %o0,4,%o0
L(l52): ld [%o0],%o2
          addcc %o2,1,%o2
          bne,pn %icc,L(l54)
         st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,L(l52)
         sub %o0,4,%o0
L(l53): jmp %o7+8
       mov 1,%o0
L(l54): jmp %o7+8
       mov 0,%o0
L(endof_asm_inc_loop_down):
        FUNEND(asm_inc_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_sub_loop_down)
FUNBEGIN(asm_sub_loop_down)


        brz,pn %o3,L(l56)
       mov %g0,%g1
L(l55): sub %o0,4,%o0
          lduw [%o0],%o4
          sub %o1,4,%o1
          lduw [%o1],%o5
          add %g1,%o4,%g1
          sub %g1,%o5,%g1
          st %g1,[%o2]
          subcc %o3,1,%o3
          bne,pt %xcc,L(l55)
         srax %g1,32,%g1
L(l56): jmp %o7+8
       srl %g1,0,%o0
L(endof_asm_sub_loop_down):
        FUNEND(asm_sub_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_subx_loop_down)
FUNBEGIN(asm_subx_loop_down)


        brz,pn %o3,L(l58)
       sra %o4,0,%g1
L(l57): sub %o0,4,%o0
          lduw [%o0],%o4
          sub %o1,4,%o1
          lduw [%o1],%o5
          add %g1,%o4,%g1
          sub %g1,%o5,%g1
          sub %o2,4,%o2
          st %g1,[%o2]
          subcc %o3,1,%o3
          bne,pt %xcc,L(l57)
         srax %g1,32,%g1
L(l58): jmp %o7+8
       srl %g1,0,%o0
L(endof_asm_subx_loop_down):
        FUNEND(asm_subx_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_subfrom_loop_down)
FUNBEGIN(asm_subfrom_loop_down)


        brz,pn %o2,L(l60)
       mov %g0,%o5
L(l59): sub %o1,4,%o1
          lduw [%o1],%o4
          sub %o0,4,%o0
          lduw [%o0],%o3
          add %o5,%o4,%o5
          sub %o5,%o3,%o5
          st %o5,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l59)
         srax %o5,32,%o5
L(l60): jmp %o7+8
       srl %o5,0,%o0
L(endof_asm_subfrom_loop_down):
        FUNEND(asm_subfrom_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_dec_loop_down)
FUNBEGIN(asm_dec_loop_down)


        brz,pn %o1,L(l62)
       sub %o0,4,%o0
L(l61): ld [%o0],%o2
          subcc %o2,1,%o2
          bcc,pn %icc,L(l63)
         st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,L(l61)
         sub %o0,4,%o0
L(l62): jmp %o7+8
       mov -1,%o0
L(l63): jmp %o7+8
       mov 0,%o0
L(endof_asm_dec_loop_down):
        FUNEND(asm_dec_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_neg_loop_down)
FUNBEGIN(asm_neg_loop_down)



        brz,pn %o1,L(l65)
       sub %o0,4,%o0
L(l64): ld [%o0],%o2
          subcc %g0,%o2,%o2
          bne,pn %icc,L(l66)
         subcc %o1,1,%o1
          bne,pt %xcc,L(l64)
         sub %o0,4,%o0
L(l65): jmp %o7+8
       mov 0,%o0
L(l66):
        st %o2,[%o0]

        be,pn %xcc,L(l68)
       sub %o0,4,%o0
L(l67): ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne,pt %xcc,L(l67)
         sub %o0,4,%o0
L(l68): mov -1,%o0
        jmp %o7+8
       srl %o0,0,%o0
L(endof_asm_neg_loop_down):
        FUNEND(asm_neg_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shift1left_loop_down)
FUNBEGIN(asm_shift1left_loop_down)

        brz,pn %o1,L(l70)
       mov 0,%o3
L(l69): sub %o0,4,%o0
          lduw [%o0],%o2
          subcc %o1,1,%o1
          add %o2,%o2,%o2
          or %o3,%o2,%o3
          st %o3,[%o0]
          bne,pt %xcc,L(l69)
         srlx %o3,32,%o3
L(l70): jmp %o7+8
       mov %o3,%o0
L(endof_asm_shift1left_loop_down):
        FUNEND(asm_shift1left_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shiftleft_loop_down)
FUNBEGIN(asm_shiftleft_loop_down)

        brz,pn %o1,L(l72)
       srl %o3,0,%o3
L(l71): sub %o0,4,%o0
          lduw [%o0],%o4
          subcc %o1,1,%o1
          sllx %o4,%o2,%o4
          or %o3,%o4,%o3
          st %o3,[%o0]
          bne,pt %xcc,L(l71)
         srlx %o3,32,%o3
L(l72): jmp %o7+8
       mov %o3,%o0
L(endof_asm_shiftleft_loop_down):
        FUNEND(asm_shiftleft_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shiftleftcopy_loop_down)
FUNBEGIN(asm_shiftleftcopy_loop_down)

        brz,pn %o2,L(l74)
       mov 0,%o4
L(l73): sub %o0,4,%o0
          lduw [%o0],%o5
          subcc %o2,1,%o2
          sllx %o5,%o3,%o5
          or %o4,%o5,%o4
          sub %o1,4,%o1
          st %o4,[%o1]
          bne,pt %xcc,L(l73)
         srlx %o4,32,%o4
L(l74): jmp %o7+8
       mov %o4,%o0
L(endof_asm_shiftleftcopy_loop_down):
        FUNEND(asm_shiftleftcopy_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shift1right_loop_up)
FUNBEGIN(asm_shift1right_loop_up)
        brz,pn %o1,L(l76)
       sll %o2,31,%o2
L(l75): ld [%o0],%o3
          subcc %o1,1,%o1
          srl %o3,1,%o4
          or %o2,%o4,%o4
          st %o4,[%o0]
          sll %o3,31,%o2
          bne,pt %xcc,L(l75)
         add %o0,4,%o0
L(l76): jmp %o7+8
       mov %o2,%o0

L(endof_asm_shift1right_loop_up):
        FUNEND(asm_shift1right_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_shiftright_loop_up)
FUNBEGIN(asm_shiftright_loop_up)
        brz,pn %o1,L(l78)
       or %g0,%g0,%o3
        sub %g0,%o2,%g1
L(l77): ld [%o0],%o4
          subcc %o1,1,%o1
          srl %o4,%o2,%o5
          or %o3,%o5,%o5
          st %o5,[%o0]
          sll %o4,%g1,%o3
          bne,pt %xcc,L(l77)
         add %o0,4,%o0
L(l78): jmp %o7+8
       mov %o3,%o0

L(endof_asm_shiftright_loop_up):
        FUNEND(asm_shiftright_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_shiftrightsigned_loop_up)
FUNBEGIN(asm_shiftrightsigned_loop_up)
        ld [%o0],%o4
        sub %g0,%o2,%g1
        sra %o4,%o2,%o5
        st %o5,[%o0]
        sll %o4,%g1,%o3
        subcc %o1,1,%o1
        be,pn %xcc,L(l80)
       add %o0,4,%o0
L(l79): ld [%o0],%o4
          subcc %o1,1,%o1
          srl %o4,%o2,%o5
          or %o3,%o5,%o5
          st %o5,[%o0]
          sll %o4,%g1,%o3
          bne,pt %xcc,L(l79)
         add %o0,4,%o0
L(l80): jmp %o7+8
       mov %o3,%o0

L(endof_asm_shiftrightsigned_loop_up):
        FUNEND(asm_shiftrightsigned_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_shiftrightcopy_loop_up)
FUNBEGIN(asm_shiftrightcopy_loop_up)
        sub %g0,%o3,%g1
        brz,pn %o2,L(l82)
       sll %o4,%g1,%g2
L(l81): ld [%o0],%o4
          add %o0,4,%o0
          srl %o4,%o3,%o5
          or %g2,%o5,%o5
          st %o5,[%o1]
          sll %o4,%g1,%g2
          subcc %o2,1,%o2
          bne,pt %xcc,L(l81)
         add %o1,4,%o1
L(l82): jmp %o7+8
       mov %g2,%o0

L(endof_asm_shiftrightcopy_loop_up):
        FUNEND(asm_shiftrightcopy_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_mulusmall_loop_down)
FUNBEGIN(asm_mulusmall_loop_down)

        brz,pn %o2,L(l85)
       sub %o1,4,%o1
L(l83):

          mov %o0,%y
          ld [%o1],%o4
          addcc %o3,%o3,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%g0,%o5


          sra %o5,26,%o3
          tst %o4
          add %o3,%o0,%o4
          movl %icc,%o4,%o3
          rd %y,%o4
          srl %o4,26,%o4
          sll %o5,6,%o5
          or %o5,%o4,%o4
          st %o4,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,L(l83)
         sub %o1,4,%o1
L(l85): jmp %o7+8
       srl %o3,0,%o0
L(endof_asm_mulusmall_loop_down):
        FUNEND(asm_mulusmall_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_mulu_loop_down)
FUNBEGIN(asm_mulu_loop_down)

        mov 0,%o4
L(l86): sub %o1,4,%o1
          ld [%o1],%g1
          sub %o2,4,%o2

          umul %g1,%o0,%g1
          rd %y,%o5
          addcc %o4,%g1,%g1
          addx %g0,%o5,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,L(l86)
         st %g1,[%o2]
        jmp %o7+8
       st %o4,[%o2-4]
L(endof_asm_mulu_loop_down):
        FUNEND(asm_mulu_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_muluadd_loop_down)
FUNBEGIN(asm_muluadd_loop_down)

        mov 0,%o4
L(l89): sub %o1,4,%o1
          ld [%o1],%o5
          sub %o2,4,%o2

          umul %o0,%o5,%g1
          rd %y,%g2
          ld [%o2],%o5
          addcc %o4,%g1,%g1
          addx %g0,%g2,%o4
          addcc %o5,%g1,%g1
          addx %g0,%o4,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,L(l89)
         st %g1,[%o2]
        jmp %o7+8
       srl %o4,0,%o0
L(endof_asm_muluadd_loop_down):
        FUNEND(asm_muluadd_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_mulusub_loop_down)
FUNBEGIN(asm_mulusub_loop_down)

        mov 0,%o4
L(l90): sub %o1,4,%o1
          ld [%o1],%o5
          sub %o2,4,%o2

          umul %o0,%o5,%g1
          rd %y,%g2
          ld [%o2],%o5
          addcc %o4,%g1,%g1
          addx %g0,%g2,%o4
          subcc %o5,%g1,%o5
          addx %g0,%o4,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,L(l90)
         st %o5,[%o2]
        jmp %o7+8
       srl %o4,0,%o0
L(endof_asm_mulusub_loop_down):
        FUNEND(asm_mulusub_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_divu_loop_up)
FUNBEGIN(asm_divu_loop_up)

        brz,pn %o2,L(l92)
       mov 0,%o3

L(l91): lduw [%o1],%o4
          sllx %o3,32,%o3
          or %o3,%o4,%o3
          udivx %o3,%o0,%o4
          st %o4,[%o1]
          umul %o0,%o4,%g1
          sub %o3,%g1,%o3
          subcc %o2,1,%o2
          bne,pt %xcc,L(l91)
         add %o1,4,%o1
L(l92): jmp %o7+8
       srl %o3,0,%o0
L(endof_asm_divu_loop_up):
        FUNEND(asm_divu_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_divucopy_loop_up)
FUNBEGIN(asm_divucopy_loop_up)

        brz,pn %o3,L(l94)
       mov 0,%o4

L(l93): lduw [%o1],%o5
          add %o1,4,%o1
          sllx %o4,32,%o4
          or %o4,%o5,%o4
          udivx %o4,%o0,%o5
          st %o5,[%o2]
          umul %o0,%o5,%g1
          sub %o4,%g1,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,L(l93)
         add %o2,4,%o2
L(l94): jmp %o7+8
       srl %o4,0,%o0
L(endof_asm_divucopy_loop_up):
        FUNEND(asm_divucopy_loop_up)
#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
