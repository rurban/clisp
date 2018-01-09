#include "asm-sparc.h"
#if defined(__sparcv8) || defined(__sparc_v8__) || defined(__sparcv9) || defined(__sparc_v9__)
  #define sparcv8
#endif
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
#ifdef sparcv8
        umul %o0,%o1,%o0
        jmp %o7+8
       nop
#else
        mov %o1,%y
        nop
        andcc %g0,%g0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2


        rd %y,%o0
        srl %o0,17,%o0
        sll %o2,15,%o2
        jmp %o7+8
       or %o2,%o0,%o0
#endif
L(endof_asm_mulu16_):
        FUNEND(asm_mulu16_)



        .align 4
        DECLARE_FUNCTION(asm_mulu32_64)
FUNBEGIN(asm_mulu32_64)
#ifdef sparcv8
        umul %o0,%o1,%o1
        jmp %o7+8
       rd %y,%o0
#else
        mov %o1,%y
        sra %o0,31,%o3
        andcc %g0,%g0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%g0,%o2
        and %o3,%o1,%o3
        add %o2,%o3,%o0
        jmp %o7+8
       rd %y,%o1
#endif
L(endof_asm_mulu32_64):
        FUNEND(asm_mulu32_64)



        .align 4
        DECLARE_FUNCTION(asm_mulu32_unchecked)
FUNBEGIN(asm_mulu32_unchecked)
#ifdef sparcv8
        umul %o0,%o1,%o0
        jmp %o7+8
       nop
#else
        subcc %o0,%o1,%g0
        bcc,a L(l01)
       mov %o1,%y

        mov %o0,%y
        nop
        andcc %g0,%g0,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2
        mulscc %o2,%o1,%o2


        rd %y,%o0
        srl %o0,17,%o0
        sll %o2,15,%o2
        jmp %o7+8
       or %o2,%o0,%o0
L(l01):
        nop
        andcc %g0,%g0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2
        mulscc %o2,%o0,%o2


        rd %y,%o0
        srl %o0,17,%o0
        sll %o2,15,%o2
        jmp %o7+8
       or %o2,%o0,%o0
#endif
L(endof_asm_mulu32_unchecked):
        FUNEND(asm_mulu32_unchecked)



        .align 4
        DECLARE_FUNCTION(asm_divu_6432_3232_)
FUNBEGIN(asm_divu_6432_3232_)
#ifdef sparcv8

        wr %o0,%g0,%y
        nop
        nop
        mov %o1,%o0
        udiv %o1,%o2,%o1
        umul %o1,%o2,%o2
        jmp %o7+8
       sub %o0,%o2,%o0
#else
        addcc %o2,%o2,%g0
        bcc L(smalldiv)
       andcc %o2,1,%g0
        be L(evendiv)
       srl %o2,1,%o2




        add %o2,1,%o2



        subcc %o0,%o2,%o3; bcc L(b01); addxcc %o1,%o1,%o1
L(a01): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb02; addxcc %o1,%o1,%o1
L(a02): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb03; addxcc %o1,%o1,%o1
L(a03): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb04; addxcc %o1,%o1,%o1
L(a04): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb05; addxcc %o1,%o1,%o1
L(a05): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb06; addxcc %o1,%o1,%o1
L(a06): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb07; addxcc %o1,%o1,%o1
L(a07): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb08; addxcc %o1,%o1,%o1
L(a08): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb09; addxcc %o1,%o1,%o1
L(a09): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb10; addxcc %o1,%o1,%o1
L(a10): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb11; addxcc %o1,%o1,%o1
L(a11): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb12; addxcc %o1,%o1,%o1
L(a12): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb13; addxcc %o1,%o1,%o1
L(a13): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb14; addxcc %o1,%o1,%o1
L(a14): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb15; addxcc %o1,%o1,%o1
L(a15): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb16; addxcc %o1,%o1,%o1
L(a16): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb17; addxcc %o1,%o1,%o1
L(a17): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb18; addxcc %o1,%o1,%o1
L(a18): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb19; addxcc %o1,%o1,%o1
L(a19): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb20; addxcc %o1,%o1,%o1
L(a20): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb21; addxcc %o1,%o1,%o1
L(a21): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb22; addxcc %o1,%o1,%o1
L(a22): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb23; addxcc %o1,%o1,%o1
L(a23): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb24; addxcc %o1,%o1,%o1
L(a24): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb25; addxcc %o1,%o1,%o1
L(a25): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb26; addxcc %o1,%o1,%o1
L(a26): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb27; addxcc %o1,%o1,%o1
L(a27): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb28; addxcc %o1,%o1,%o1
L(a28): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb29; addxcc %o1,%o1,%o1
L(a29): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb30; addxcc %o1,%o1,%o1
L(a30): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb31; addxcc %o1,%o1,%o1
L(a31): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lb32; addxcc %o1,%o1,%o1
L(a32): addx %o0,%o0,%o0
        xor %o1,-1,%o1
        add %o2,%o2,%o2
        sub %o2,1,%o2




        addcc %o1,%o0,%o0
        bcc L(l02)
       subcc %o0,%o2,%o3
        subcc %o3,%o2,%o0
        bcs L(l03)
       nop

        jmp %o7+8
       add %o1,2,%o1
L(l02):

        bcs L(l04)
       nop
L(l03):
        add %o1,1,%o1
        jmp %o7+8
       mov %o3,%o0
L(l04):
        jmp %o7+8
       nop

L(b01): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La02; addxcc %o1,%o1,%o1
L(b02): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La03; addxcc %o1,%o1,%o1
L(b03): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La04; addxcc %o1,%o1,%o1
L(b04): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La05; addxcc %o1,%o1,%o1
L(b05): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La06; addxcc %o1,%o1,%o1
L(b06): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La07; addxcc %o1,%o1,%o1
L(b07): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La08; addxcc %o1,%o1,%o1
L(b08): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La09; addxcc %o1,%o1,%o1
L(b09): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La10; addxcc %o1,%o1,%o1
L(b10): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La11; addxcc %o1,%o1,%o1
L(b11): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La12; addxcc %o1,%o1,%o1
L(b12): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La13; addxcc %o1,%o1,%o1
L(b13): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La14; addxcc %o1,%o1,%o1
L(b14): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La15; addxcc %o1,%o1,%o1
L(b15): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La16; addxcc %o1,%o1,%o1
L(b16): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La17; addxcc %o1,%o1,%o1
L(b17): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La18; addxcc %o1,%o1,%o1
L(b18): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La19; addxcc %o1,%o1,%o1
L(b19): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La20; addxcc %o1,%o1,%o1
L(b20): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La21; addxcc %o1,%o1,%o1
L(b21): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La22; addxcc %o1,%o1,%o1
L(b22): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La23; addxcc %o1,%o1,%o1
L(b23): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La24; addxcc %o1,%o1,%o1
L(b24): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La25; addxcc %o1,%o1,%o1
L(b25): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La26; addxcc %o1,%o1,%o1
L(b26): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La27; addxcc %o1,%o1,%o1
L(b27): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La28; addxcc %o1,%o1,%o1
L(b28): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La29; addxcc %o1,%o1,%o1
L(b29): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La30; addxcc %o1,%o1,%o1
L(b30): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La31; addxcc %o1,%o1,%o1
L(b31): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .La32; addxcc %o1,%o1,%o1
L(b32): addx %o3,%o3,%o3
        xor %o1,-1,%o1
        add %o2,%o2,%o2
        sub %o2,1,%o2




        addcc %o1,%o3,%o3
        bcc L(l05)
       subcc %o3,%o2,%o0
        subcc %o0,%o2,%o3
        bcs L(l06)
       nop

        add %o1,2,%o1
        jmp %o7+8
       mov %o3,%o0
L(l05):

        bcs L(l07)
       nop
L(l06):
        jmp %o7+8
       add %o1,1,%o1
L(l07):
        jmp %o7+8
       mov %o3,%o0
L(smalldiv):
        addcc %o1,%o1,%o1
L(c00): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld01; addxcc %o1,%o1,%o1
L(c01): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld02; addxcc %o1,%o1,%o1
L(c02): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld03; addxcc %o1,%o1,%o1
L(c03): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld04; addxcc %o1,%o1,%o1
L(c04): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld05; addxcc %o1,%o1,%o1
L(c05): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld06; addxcc %o1,%o1,%o1
L(c06): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld07; addxcc %o1,%o1,%o1
L(c07): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld08; addxcc %o1,%o1,%o1
L(c08): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld09; addxcc %o1,%o1,%o1
L(c09): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld10; addxcc %o1,%o1,%o1
L(c10): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld11; addxcc %o1,%o1,%o1
L(c11): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld12; addxcc %o1,%o1,%o1
L(c12): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld13; addxcc %o1,%o1,%o1
L(c13): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld14; addxcc %o1,%o1,%o1
L(c14): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld15; addxcc %o1,%o1,%o1
L(c15): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld16; addxcc %o1,%o1,%o1
L(c16): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld17; addxcc %o1,%o1,%o1
L(c17): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld18; addxcc %o1,%o1,%o1
L(c18): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld19; addxcc %o1,%o1,%o1
L(c19): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld20; addxcc %o1,%o1,%o1
L(c20): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld21; addxcc %o1,%o1,%o1
L(c21): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld22; addxcc %o1,%o1,%o1
L(c22): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld23; addxcc %o1,%o1,%o1
L(c23): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld24; addxcc %o1,%o1,%o1
L(c24): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld25; addxcc %o1,%o1,%o1
L(c25): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld26; addxcc %o1,%o1,%o1
L(c26): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld27; addxcc %o1,%o1,%o1
L(c27): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld28; addxcc %o1,%o1,%o1
L(c28): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld29; addxcc %o1,%o1,%o1
L(c29): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld30; addxcc %o1,%o1,%o1
L(c30): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld31; addxcc %o1,%o1,%o1
L(c31): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Ld32; addxcc %o1,%o1,%o1
L(c32): jmp %o7+8
       xor %o1,-1,%o1

L(d01): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc02; addxcc %o1,%o1,%o1
L(d02): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc03; addxcc %o1,%o1,%o1
L(d03): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc04; addxcc %o1,%o1,%o1
L(d04): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc05; addxcc %o1,%o1,%o1
L(d05): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc06; addxcc %o1,%o1,%o1
L(d06): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc07; addxcc %o1,%o1,%o1
L(d07): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc08; addxcc %o1,%o1,%o1
L(d08): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc09; addxcc %o1,%o1,%o1
L(d09): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc10; addxcc %o1,%o1,%o1
L(d10): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc11; addxcc %o1,%o1,%o1
L(d11): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc12; addxcc %o1,%o1,%o1
L(d12): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc13; addxcc %o1,%o1,%o1
L(d13): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc14; addxcc %o1,%o1,%o1
L(d14): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc15; addxcc %o1,%o1,%o1
L(d15): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc16; addxcc %o1,%o1,%o1
L(d16): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc17; addxcc %o1,%o1,%o1
L(d17): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc18; addxcc %o1,%o1,%o1
L(d18): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc19; addxcc %o1,%o1,%o1
L(d19): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc20; addxcc %o1,%o1,%o1
L(d20): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc21; addxcc %o1,%o1,%o1
L(d21): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc22; addxcc %o1,%o1,%o1
L(d22): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc23; addxcc %o1,%o1,%o1
L(d23): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc24; addxcc %o1,%o1,%o1
L(d24): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc25; addxcc %o1,%o1,%o1
L(d25): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc26; addxcc %o1,%o1,%o1
L(d26): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc27; addxcc %o1,%o1,%o1
L(d27): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc28; addxcc %o1,%o1,%o1
L(d28): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc29; addxcc %o1,%o1,%o1
L(d29): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc30; addxcc %o1,%o1,%o1
L(d30): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc31; addxcc %o1,%o1,%o1
L(d31): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Lc32; addxcc %o1,%o1,%o1
L(d32): mov %o3,%o0
        jmp %o7+8
       xor %o1,-1,%o1
L(evendiv):





        subcc %o0,%o2,%o3; bcc L(f01); addxcc %o1,%o1,%o1
L(e01): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf02; addxcc %o1,%o1,%o1
L(e02): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf03; addxcc %o1,%o1,%o1
L(e03): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf04; addxcc %o1,%o1,%o1
L(e04): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf05; addxcc %o1,%o1,%o1
L(e05): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf06; addxcc %o1,%o1,%o1
L(e06): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf07; addxcc %o1,%o1,%o1
L(e07): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf08; addxcc %o1,%o1,%o1
L(e08): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf09; addxcc %o1,%o1,%o1
L(e09): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf10; addxcc %o1,%o1,%o1
L(e10): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf11; addxcc %o1,%o1,%o1
L(e11): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf12; addxcc %o1,%o1,%o1
L(e12): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf13; addxcc %o1,%o1,%o1
L(e13): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf14; addxcc %o1,%o1,%o1
L(e14): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf15; addxcc %o1,%o1,%o1
L(e15): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf16; addxcc %o1,%o1,%o1
L(e16): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf17; addxcc %o1,%o1,%o1
L(e17): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf18; addxcc %o1,%o1,%o1
L(e18): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf19; addxcc %o1,%o1,%o1
L(e19): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf20; addxcc %o1,%o1,%o1
L(e20): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf21; addxcc %o1,%o1,%o1
L(e21): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf22; addxcc %o1,%o1,%o1
L(e22): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf23; addxcc %o1,%o1,%o1
L(e23): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf24; addxcc %o1,%o1,%o1
L(e24): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf25; addxcc %o1,%o1,%o1
L(e25): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf26; addxcc %o1,%o1,%o1
L(e26): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf27; addxcc %o1,%o1,%o1
L(e27): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf28; addxcc %o1,%o1,%o1
L(e28): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf29; addxcc %o1,%o1,%o1
L(e29): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf30; addxcc %o1,%o1,%o1
L(e30): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf31; addxcc %o1,%o1,%o1
L(e31): addx %o0,%o0,%o0; subcc %o0,%o2,%o3; bcc .Lf32; addxcc %o1,%o1,%o1
L(e32): addx %o0,%o0,%o0
        jmp %o7+8
       xor %o1,-1,%o1

L(f01): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le02; addxcc %o1,%o1,%o1
L(f02): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le03; addxcc %o1,%o1,%o1
L(f03): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le04; addxcc %o1,%o1,%o1
L(f04): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le05; addxcc %o1,%o1,%o1
L(f05): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le06; addxcc %o1,%o1,%o1
L(f06): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le07; addxcc %o1,%o1,%o1
L(f07): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le08; addxcc %o1,%o1,%o1
L(f08): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le09; addxcc %o1,%o1,%o1
L(f09): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le10; addxcc %o1,%o1,%o1
L(f10): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le11; addxcc %o1,%o1,%o1
L(f11): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le12; addxcc %o1,%o1,%o1
L(f12): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le13; addxcc %o1,%o1,%o1
L(f13): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le14; addxcc %o1,%o1,%o1
L(f14): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le15; addxcc %o1,%o1,%o1
L(f15): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le16; addxcc %o1,%o1,%o1
L(f16): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le17; addxcc %o1,%o1,%o1
L(f17): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le18; addxcc %o1,%o1,%o1
L(f18): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le19; addxcc %o1,%o1,%o1
L(f19): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le20; addxcc %o1,%o1,%o1
L(f20): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le21; addxcc %o1,%o1,%o1
L(f21): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le22; addxcc %o1,%o1,%o1
L(f22): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le23; addxcc %o1,%o1,%o1
L(f23): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le24; addxcc %o1,%o1,%o1
L(f24): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le25; addxcc %o1,%o1,%o1
L(f25): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le26; addxcc %o1,%o1,%o1
L(f26): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le27; addxcc %o1,%o1,%o1
L(f27): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le28; addxcc %o1,%o1,%o1
L(f28): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le29; addxcc %o1,%o1,%o1
L(f29): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le30; addxcc %o1,%o1,%o1
L(f30): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le31; addxcc %o1,%o1,%o1
L(f31): addx %o3,%o3,%o3; subcc %o3,%o2,%o0; bcc .Le32; addxcc %o1,%o1,%o1
L(f32): addx %o3,%o3,%o3
        mov %o3,%o0
        jmp %o7+8
       xor %o1,-1,%o1
#endif
L(endof_asm_divu_6432_3232_):
        FUNEND(asm_divu_6432_3232_)



        .align 4
        DECLARE_FUNCTION(asm_divu_3216_1616_)
FUNBEGIN(asm_divu_3216_1616_)
#ifdef sparcv8

        wr %g0,%g0,%y
        nop
        nop
        nop
        udiv %o0,%o1,%o0
        rd %y,%o1
        sll %o1,16,%o1
        jmp %o7+8
       or %o0,%o1,%o0
#else






        sll %o1,16,%o1
        srl %o1,1,%o1
        sub %g0,%o1,%o2
        addcc %o0,%o2,%o0; bcc L(h01); addx %o0,%o0,%o0
L(g01): addcc %o0,%o2,%o0; bcc .Lh02; addx %o0,%o0,%o0
L(g02): addcc %o0,%o2,%o0; bcc .Lh03; addx %o0,%o0,%o0
L(g03): addcc %o0,%o2,%o0; bcc .Lh04; addx %o0,%o0,%o0
L(g04): addcc %o0,%o2,%o0; bcc .Lh05; addx %o0,%o0,%o0
L(g05): addcc %o0,%o2,%o0; bcc .Lh06; addx %o0,%o0,%o0
L(g06): addcc %o0,%o2,%o0; bcc .Lh07; addx %o0,%o0,%o0
L(g07): addcc %o0,%o2,%o0; bcc .Lh08; addx %o0,%o0,%o0
L(g08): addcc %o0,%o2,%o0; bcc .Lh09; addx %o0,%o0,%o0
L(g09): addcc %o0,%o2,%o0; bcc .Lh10; addx %o0,%o0,%o0
L(g10): addcc %o0,%o2,%o0; bcc .Lh11; addx %o0,%o0,%o0
L(g11): addcc %o0,%o2,%o0; bcc .Lh12; addx %o0,%o0,%o0
L(g12): addcc %o0,%o2,%o0; bcc .Lh13; addx %o0,%o0,%o0
L(g13): addcc %o0,%o2,%o0; bcc .Lh14; addx %o0,%o0,%o0
L(g14): addcc %o0,%o2,%o0; bcc .Lh15; addx %o0,%o0,%o0
L(g15): addcc %o0,%o2,%o0; bcc .Lh16; addx %o0,%o0,%o0
L(g16):

        jmp %o7+8
       nop
L(h01): addcc %o0,%o1,%o0; bcs .Lg02; addx %o0,%o0,%o0
L(h02): addcc %o0,%o1,%o0; bcs .Lg03; addx %o0,%o0,%o0
L(h03): addcc %o0,%o1,%o0; bcs .Lg04; addx %o0,%o0,%o0
L(h04): addcc %o0,%o1,%o0; bcs .Lg05; addx %o0,%o0,%o0
L(h05): addcc %o0,%o1,%o0; bcs .Lg06; addx %o0,%o0,%o0
L(h06): addcc %o0,%o1,%o0; bcs .Lg07; addx %o0,%o0,%o0
L(h07): addcc %o0,%o1,%o0; bcs .Lg08; addx %o0,%o0,%o0
L(h08): addcc %o0,%o1,%o0; bcs .Lg09; addx %o0,%o0,%o0
L(h09): addcc %o0,%o1,%o0; bcs .Lg10; addx %o0,%o0,%o0
L(h10): addcc %o0,%o1,%o0; bcs .Lg11; addx %o0,%o0,%o0
L(h11): addcc %o0,%o1,%o0; bcs .Lg12; addx %o0,%o0,%o0
L(h12): addcc %o0,%o1,%o0; bcs .Lg13; addx %o0,%o0,%o0
L(h13): addcc %o0,%o1,%o0; bcs .Lg14; addx %o0,%o0,%o0
L(h14): addcc %o0,%o1,%o0; bcs .Lg15; addx %o0,%o0,%o0
L(h15): addcc %o0,%o1,%o0; bcs .Lg16; addx %o0,%o0,%o0
L(h16):
        add %o0,%o1,%o0
        jmp %o7+8
       add %o0,%o1,%o0
#endif
L(endof_asm_divu_3216_1616_):
        FUNEND(asm_divu_3216_1616_)


        .align 4
        DECLARE_FUNCTION(asm_copy_loop_up)
FUNBEGIN(asm_copy_loop_up)

        andcc %o2,%o2,%g0
        be L(l09)
       nop
L(l08): ld [%o0],%o3
          add %o0,4,%o0
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne L(l08)
         add %o1,4,%o1
L(l09): jmp %o7+8
       mov %o1,%o0
L(endof_asm_copy_loop_up):
        FUNEND(asm_copy_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_copy_loop_down)
FUNBEGIN(asm_copy_loop_down)

        andcc %o2,%o2,%g0
        be L(l11)
       sub %o0,4,%o0
L(l10): ld [%o0],%o3
          sub %o1,4,%o1
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne L(l10)
         sub %o0,4,%o0
L(l11): jmp %o7+8
       mov %o1,%o0
L(endof_asm_copy_loop_down):
        FUNEND(asm_copy_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_fill_loop_up)
FUNBEGIN(asm_fill_loop_up)

        andcc %o1,%o1,%g0
        be L(l13)
       nop
L(l12): st %o2,[%o0]
          subcc %o1,1,%o1
          bne L(l12)
         add %o0,4,%o0
L(l13): jmp %o7+8
       nop
L(endof_asm_fill_loop_up):
        FUNEND(asm_fill_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_fill_loop_down)
FUNBEGIN(asm_fill_loop_down)

        andcc %o1,%o1,%g0
        be L(l15)
       sub %o0,4,%o0
L(l14): st %o2,[%o0]
          subcc %o1,1,%o1
          bne L(l14)
         sub %o0,4,%o0
L(l15): jmp %o7+8
       add %o0,4,%o0
L(endof_asm_fill_loop_down):
        FUNEND(asm_fill_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_clear_loop_up)
FUNBEGIN(asm_clear_loop_up)

        andcc %o1,%o1,%g0
        be L(l17)
       nop
L(l16): st %g0,[%o0]
          subcc %o1,1,%o1
          bne L(l16)
         add %o0,4,%o0
L(l17): jmp %o7+8
       nop
L(endof_asm_clear_loop_up):
        FUNEND(asm_clear_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_clear_loop_down)
FUNBEGIN(asm_clear_loop_down)

        andcc %o1,%o1,%g0
        be L(l19)
       sub %o0,4,%o0
L(l18): st %g0,[%o0]
          subcc %o1,1,%o1
          bne L(l18)
         sub %o0,4,%o0
L(l19): jmp %o7+8
       add %o0,4,%o0
L(endof_asm_clear_loop_down):
        FUNEND(asm_clear_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_or_loop_up)
FUNBEGIN(asm_or_loop_up)

        andcc %o2,%o2,%g0
        be L(l21)
       nop
L(l20): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l20)
         add %o0,4,%o0
L(l21): jmp %o7+8
       nop
L(endof_asm_or_loop_up):
        FUNEND(asm_or_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_xor_loop_up)
FUNBEGIN(asm_xor_loop_up)

        andcc %o2,%o2,%g0
        be L(l23)
       nop
L(l22): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l22)
         add %o0,4,%o0
L(l23): jmp %o7+8
       nop
L(endof_asm_xor_loop_up):
        FUNEND(asm_xor_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_and_loop_up)
FUNBEGIN(asm_and_loop_up)

        andcc %o2,%o2,%g0
        be L(l25)
       nop
L(l24): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l24)
         add %o0,4,%o0
L(l25): jmp %o7+8
       nop
L(endof_asm_and_loop_up):
        FUNEND(asm_and_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_eqv_loop_up)
FUNBEGIN(asm_eqv_loop_up)

        andcc %o2,%o2,%g0
        be L(l27)
       nop
L(l26): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xnor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l26)
         add %o0,4,%o0
L(l27): jmp %o7+8
       nop
L(endof_asm_eqv_loop_up):
        FUNEND(asm_eqv_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_nand_loop_up)
FUNBEGIN(asm_nand_loop_up)

        andcc %o2,%o2,%g0
        be L(l29)
       nop
L(l28): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l28)
         add %o0,4,%o0
L(l29): jmp %o7+8
       nop
L(endof_asm_nand_loop_up):
        FUNEND(asm_nand_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_nor_loop_up)
FUNBEGIN(asm_nor_loop_up)

        andcc %o2,%o2,%g0
        be L(l31)
       nop
L(l30): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l30)
         add %o0,4,%o0
L(l31): jmp %o7+8
       nop
L(endof_asm_nor_loop_up):
        FUNEND(asm_nor_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_andc2_loop_up)
FUNBEGIN(asm_andc2_loop_up)

        andcc %o2,%o2,%g0
        be L(l33)
       nop
L(l32): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          andn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l32)
         add %o0,4,%o0
L(l33): jmp %o7+8
       nop
L(endof_asm_andc2_loop_up):
        FUNEND(asm_andc2_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_orc2_loop_up)
FUNBEGIN(asm_orc2_loop_up)

        andcc %o2,%o2,%g0
        be L(l35)
       nop
L(l34): ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          orn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne L(l34)
         add %o0,4,%o0
L(l35): jmp %o7+8
       nop
L(endof_asm_orc2_loop_up):
        FUNEND(asm_orc2_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_not_loop_up)
FUNBEGIN(asm_not_loop_up)

        andcc %o1,%o1,%g0
        be L(l37)
       nop
L(l36): ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne L(l36)
         add %o0,4,%o0
L(l37): jmp %o7+8
       nop
L(endof_asm_not_loop_up):
        FUNEND(asm_not_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_and_test_loop_up)
FUNBEGIN(asm_and_test_loop_up)

        andcc %o2,%o2,%g0
        be L(l39)
       nop
L(l38): ld [%o0],%o3
          ld [%o1],%o4
          add %o0,4,%o0
          andcc %o3,%o4,%g0
          bne L(l40)
         subcc %o2,1,%o2
          bne L(l38)
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

        andcc %o1,%o1,%g0
        be L(l42)
       nop
          ld [%o0],%o2
L(l41): add %o0,4,%o0
          andcc %o2,%o2,%g0
          bne L(l43)
         subcc %o1,1,%o1
          bne,a L(l41)
         ld [%o0],%o2
L(l42): jmp %o7+8
       mov 0,%o0
L(l43): jmp %o7+8
       mov 1,%o0
L(endof_asm_test_loop_up):
        FUNEND(asm_test_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_compare_loop_up)
FUNBEGIN(asm_compare_loop_up)

        andcc %o2,%o2,%g0
        be L(l45)
       nop
          ld [%o0],%o3
L(l44): ld [%o1],%o4
          add %o0,4,%o0
          subcc %o3,%o4,%g0
          bne L(l46)
         add %o1,4,%o1
          subcc %o2,1,%o2
          bne,a L(l44)
         ld [%o0],%o3
L(l45): jmp %o7+8
       mov 0,%o0
L(l46): blu .Ll47
       nop
        jmp %o7+8
       mov 1,%o0
L(l47): jmp %o7+8
       mov -1,%o0
L(endof_asm_compare_loop_up):
        FUNEND(asm_compare_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_add_loop_down)
FUNBEGIN(asm_add_loop_down)

        andcc %o3,%o3,%g0
        be L(l49)
       mov %g0,%g1
        sub %o0,4,%o0
L(l48): ld [%o0],%o4
          sub %o1,4,%o1
          ld [%o1],%o5
          subcc %g0,%g1,%g0
          addxcc %o4,%o5,%o4
          addx %g0,%g0,%g1
          sub %o2,4,%o2
          st %o4,[%o2]
          subcc %o3,1,%o3
          bne L(l48)
         sub %o0,4,%o0
L(l49): jmp %o7+8
       mov %g1,%o0
L(endof_asm_add_loop_down):
        FUNEND(asm_add_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_addto_loop_down)
FUNBEGIN(asm_addto_loop_down)

        andcc %o2,%o2,%g0
        be L(l51)
       mov %g0,%o5
        sub %o0,4,%o0
L(l50): ld [%o0],%o3
          sub %o1,4,%o1
          ld [%o1],%o4
          subcc %g0,%o5,%g0
          addxcc %o4,%o3,%o4
          addx %g0,%g0,%o5
          st %o4,[%o1]
          subcc %o2,1,%o2
          bne L(l50)
         sub %o0,4,%o0
L(l51): jmp %o7+8
       mov %o5,%o0
L(endof_asm_addto_loop_down):
        FUNEND(asm_addto_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_inc_loop_down)
FUNBEGIN(asm_inc_loop_down)

        andcc %o1,%o1,%g0
        be L(l53)
       sub %o0,4,%o0
L(l52): ld [%o0],%o2
          addcc %o2,1,%o2
          bne L(l54)
         st %o2,[%o0]
          subcc %o1,1,%o1
          bne L(l52)
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

        andcc %o3,%o3,%g0
        be L(l56)
       mov %g0,%g1
        sub %o0,4,%o0
L(l55): ld [%o0],%o4
          sub %o1,4,%o1
          ld [%o1],%o5
          subcc %g0,%g1,%g0
          subxcc %o4,%o5,%o4
          addx %g0,%g0,%g1
          sub %o2,4,%o2
          st %o4,[%o2]
          subcc %o3,1,%o3
          bne L(l55)
         sub %o0,4,%o0
L(l56): jmp %o7+8
       mov %g1,%o0
L(endof_asm_sub_loop_down):
        FUNEND(asm_sub_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_subx_loop_down)
FUNBEGIN(asm_subx_loop_down)

        andcc %o3,%o3,%g0
        be L(l58)
       mov %o4,%g1
        sub %o0,4,%o0
L(l57): ld [%o0],%o4
          sub %o1,4,%o1
          ld [%o1],%o5
          subcc %g0,%g1,%g0
          subxcc %o4,%o5,%o4
          addx %g0,%g0,%g1
          sub %o2,4,%o2
          st %o4,[%o2]
          subcc %o3,1,%o3
          bne L(l57)
         sub %o0,4,%o0
L(l58): jmp %o7+8
       mov %g1,%o0
L(endof_asm_subx_loop_down):
        FUNEND(asm_subx_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_subfrom_loop_down)
FUNBEGIN(asm_subfrom_loop_down)

        andcc %o2,%o2,%g0
        be L(l60)
       mov %g0,%o5
        sub %o0,4,%o0
L(l59): ld [%o0],%o3
          sub %o1,4,%o1
          ld [%o1],%o4
          subcc %g0,%o5,%g0
          subxcc %o4,%o3,%o4
          addx %g0,%g0,%o5
          st %o4,[%o1]
          subcc %o2,1,%o2
          bne L(l59)
         sub %o0,4,%o0
L(l60): jmp %o7+8
       mov %o5,%o0
L(endof_asm_subfrom_loop_down):
        FUNEND(asm_subfrom_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_dec_loop_down)
FUNBEGIN(asm_dec_loop_down)

        andcc %o1,%o1,%g0
        be L(l62)
       sub %o0,4,%o0
L(l61): ld [%o0],%o2
          subcc %o2,1,%o2
          bcc L(l63)
         st %o2,[%o0]
          subcc %o1,1,%o1
          bne L(l61)
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


        andcc %o1,%o1,%g0
        be L(l65)
       sub %o0,4,%o0
L(l64): ld [%o0],%o2
          subcc %g0,%o2,%o2
          bne L(l66)
         subcc %o1,1,%o1
          bne L(l64)
         sub %o0,4,%o0
L(l65): jmp %o7+8
       mov 0,%o0
L(l66):
        st %o2,[%o0]

        be L(l68)
       sub %o0,4,%o0
L(l67): ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne L(l67)
         sub %o0,4,%o0
L(l68): jmp %o7+8
       mov -1,%o0
L(endof_asm_neg_loop_down):
        FUNEND(asm_neg_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shift1left_loop_down)
FUNBEGIN(asm_shift1left_loop_down)
        andcc %o1,%o1,%g0
        be L(l70)
       mov 0,%o3
        sub %o0,4,%o0
L(l69): ld [%o0],%o2
          subcc %g0,%o3,%g0
          addxcc %o2,%o2,%o2
          addx %g0,%g0,%o3
          st %o2,[%o0]
          subcc %o1,1,%o1
          bne L(l69)
         sub %o0,4,%o0
L(l70): jmp %o7+8
       mov %o3,%o0
L(endof_asm_shift1left_loop_down):
        FUNEND(asm_shift1left_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shiftleft_loop_down)
FUNBEGIN(asm_shiftleft_loop_down)
        andcc %o1,%o1,%g0
        be L(l72)
       sub %g0,%o2,%g1
        sub %o0,4,%o0
L(l71): ld [%o0],%o4
          subcc %o1,1,%o1
          sll %o4,%o2,%o5
          or %o3,%o5,%o5
          st %o5,[%o0]
          srl %o4,%g1,%o3
          bne L(l71)
         sub %o0,4,%o0
L(l72): jmp %o7+8
       mov %o3,%o0
L(endof_asm_shiftleft_loop_down):
        FUNEND(asm_shiftleft_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shiftleftcopy_loop_down)
FUNBEGIN(asm_shiftleftcopy_loop_down)
        andcc %o2,%o2,%g0
        be L(l74)
       mov 0,%o4
        sub %g0,%o3,%g1
        sub %o0,4,%o0
L(l73): ld [%o0],%o5
          subcc %o2,1,%o2
          sll %o5,%o3,%g2
          or %o4,%g2,%g2
          sub %o1,4,%o1
          st %g2,[%o1]
          srl %o5,%g1,%o4
          bne L(l73)
         sub %o0,4,%o0
L(l74): jmp %o7+8
       mov %o4,%o0
L(endof_asm_shiftleftcopy_loop_down):
        FUNEND(asm_shiftleftcopy_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_shift1right_loop_up)
FUNBEGIN(asm_shift1right_loop_up)
        andcc %o1,%o1,%g0
        be L(l76)
       sll %o2,31,%o2
L(l75): ld [%o0],%o3
          subcc %o1,1,%o1
          srl %o3,1,%o4
          or %o2,%o4,%o4
          st %o4,[%o0]
          sll %o3,31,%o2
          bne L(l75)
         add %o0,4,%o0
L(l76): jmp %o7+8
       mov %o2,%o0
L(endof_asm_shift1right_loop_up):
        FUNEND(asm_shift1right_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_shiftright_loop_up)
FUNBEGIN(asm_shiftright_loop_up)
        andcc %o1,%o1,%g0
        be L(l78)
       or %g0,%g0,%o3
        sub %g0,%o2,%g1
L(l77): ld [%o0],%o4
          subcc %o1,1,%o1
          srl %o4,%o2,%o5
          or %o3,%o5,%o5
          st %o5,[%o0]
          sll %o4,%g1,%o3
          bne L(l77)
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
        be L(l80)
       add %o0,4,%o0
L(l79): ld [%o0],%o4
          subcc %o1,1,%o1
          srl %o4,%o2,%o5
          or %o3,%o5,%o5
          st %o5,[%o0]
          sll %o4,%g1,%o3
          bne L(l79)
         add %o0,4,%o0
L(l80): jmp %o7+8
       mov %o3,%o0
L(endof_asm_shiftrightsigned_loop_up):
        FUNEND(asm_shiftrightsigned_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_shiftrightcopy_loop_up)
FUNBEGIN(asm_shiftrightcopy_loop_up)
        sub %g0,%o3,%g1
        andcc %o2,%o2,%g0
        be L(l82)
       sll %o4,%g1,%g2
L(l81): ld [%o0],%o4
          add %o0,4,%o0
          srl %o4,%o3,%o5
          or %g2,%o5,%o5
          st %o5,[%o1]
          sll %o4,%g1,%g2
          subcc %o2,1,%o2
          bne L(l81)
         add %o1,4,%o1
L(l82): jmp %o7+8
       mov %g2,%o0
L(endof_asm_shiftrightcopy_loop_up):
        FUNEND(asm_shiftrightcopy_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_mulusmall_loop_down)
FUNBEGIN(asm_mulusmall_loop_down)
        andcc %o2,%o2,%g0
        be L(l85)
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


          tst %o4
          bge L(l84)
         sra %o5,26,%o3
          add %o3,%o0,%o3
L(l84): rd %y,%o4
          srl %o4,26,%o4
          sll %o5,6,%o5
          or %o5,%o4,%o4
          st %o4,[%o1]
          subcc %o2,1,%o2
          bne L(l83)
         sub %o1,4,%o1
L(l85): jmp %o7+8
       mov %o3,%o0
L(endof_asm_mulusmall_loop_down):
        FUNEND(asm_mulusmall_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_mulu_loop_down)
FUNBEGIN(asm_mulu_loop_down)
        mov 0,%o4
L(l87): ld [%o1-4],%g1

#ifdef sparcv8
          sub %o1,4,%o1
          umul %g1,%o0,%g1
          rd %y,%o5
#else
          mov %g1,%y
          sub %o1,4,%o1
          andcc %g0,%g0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%o0,%o5
          mulscc %o5,%g0,%o5
          tst %o0
          bl,a L(l88)
         add %o5,%g1,%o5
L(l88): rd %y,%g1
#endif
          addcc %o4,%g1,%g1
          addx %g0,%o5,%o4
          sub %o2,4,%o2
          subcc %o3,1,%o3
          bne L(l87)
         st %g1,[%o2]
        jmp %o7+8
       st %o4,[%o2-4]

L(endof_asm_mulu_loop_down):
        FUNEND(asm_mulu_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_muluadd_loop_down)
FUNBEGIN(asm_muluadd_loop_down)
        save %sp,-96,%sp
        mov 0,%l0
#ifndef sparcv8
        sra %i0,31,%l1
#endif
L(l89): ld [%i1-4],%o1
          sub %i1,4,%i1

#ifdef sparcv8
          umul %i0,%o1,%o0
          rd %y,%o2
#else
          mov %o1,%y
          and %o1,%l1,%o3
          andcc %g0,%g0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%g0,%o2
          add %o2,%o3,%o2
          rd %y,%o0
#endif
          sub %i2,4,%i2
          ld [%i2],%o1
          addcc %l0,%o0,%o0
          addx %g0,%o2,%l0
          addcc %o1,%o0,%o0
          addx %g0,%l0,%l0
          subcc %i3,1,%i3
          bne L(l89)
         st %o0,[%i2]
        mov %l0,%i0
        jmp %i7+8
       restore

L(endof_asm_muluadd_loop_down):
        FUNEND(asm_muluadd_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_mulusub_loop_down)
FUNBEGIN(asm_mulusub_loop_down)
        save %sp,-96,%sp
        mov 0,%l0
#ifndef sparcv8
        sra %i0,31,%l1
#endif
L(l90): ld [%i1-4],%o1
          sub %i1,4,%i1

#ifdef sparcv8
          umul %i0,%o1,%o0
          rd %y,%o2
#else
          mov %o1,%y
          and %o1,%l1,%o3
          andcc %g0,%g0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%i0,%o2
          mulscc %o2,%g0,%o2
          add %o2,%o3,%o2
          rd %y,%o0
#endif
          sub %i2,4,%i2
          ld [%i2],%o1
          addcc %l0,%o0,%o0
          addx %g0,%o2,%l0
          subcc %o1,%o0,%o1
          addx %g0,%l0,%l0
          subcc %i3,1,%i3
          bne L(l90)
         st %o1,[%i2]
        mov %l0,%i0
        jmp %i7+8
       restore

L(endof_asm_mulusub_loop_down):
        FUNEND(asm_mulusub_loop_down)


        .align 4
        DECLARE_FUNCTION(asm_divu_loop_up)
FUNBEGIN(asm_divu_loop_up)
        save %sp,-96,%sp
        andcc %i2,%i2,%g0
        be L(l92)
       mov 0,%o0
L(l91): ld [%i1],%o1
          call asm_divu_6432_3232_
         mov %i0,%o2
          st %o1,[%i1]
          subcc %i2,1,%i2
          bne L(l91)
         add %i1,4,%i1
L(l92): mov %o0,%i0
        jmp %i7+8
       restore
L(endof_asm_divu_loop_up):
        FUNEND(asm_divu_loop_up)


        .align 4
        DECLARE_FUNCTION(asm_divucopy_loop_up)
FUNBEGIN(asm_divucopy_loop_up)
        save %sp,-96,%sp
        andcc %i3,%i3,%g0
        be L(l94)
       mov 0,%o0
L(l93): ld [%i1],%o1
          call asm_divu_6432_3232_
         mov %i0,%o2
          st %o1,[%i2]
          add %i1,4,%i1
          subcc %i3,1,%i3
          bne L(l93)
         add %i2,4,%i2
L(l94): mov %o0,%i0
        jmp %i7+8
       restore
L(endof_asm_divucopy_loop_up):
        FUNEND(asm_divucopy_loop_up)
#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
