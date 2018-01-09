#include "asm-i386.h"
  #if defined(__EMX__)

    #define dir0start
    #define dir0end
    #define dir1start std
    #define dir1end cld
  #elif 1

    #define dir0start cld
    #define dir0end
    #define dir1start std
    #define dir1end cld
  #else

    #define dir0start cld
    #define dir0end
    #define dir1start std
    #define dir1end
  #endif

            TEXT()

            GLOBL(C(asm_mulu32_64))
            GLOBL(C(asm_divu_6432_3232_))
            GLOBL(C(asm_copy_loop_up))
            GLOBL(C(asm_copy_loop_down))
            GLOBL(C(asm_fill_loop_up))
            GLOBL(C(asm_fill_loop_down))
            GLOBL(C(asm_clear_loop_up))
            GLOBL(C(asm_clear_loop_down))
            GLOBL(C(asm_or_loop_up))
            GLOBL(C(asm_xor_loop_up))
            GLOBL(C(asm_and_loop_up))
            GLOBL(C(asm_eqv_loop_up))
            GLOBL(C(asm_nand_loop_up))
            GLOBL(C(asm_nor_loop_up))
            GLOBL(C(asm_andc2_loop_up))
            GLOBL(C(asm_orc2_loop_up))
            GLOBL(C(asm_not_loop_up))
            GLOBL(C(asm_and_test_loop_up))
            GLOBL(C(asm_test_loop_up))
            GLOBL(C(asm_compare_loop_up))
            GLOBL(C(asm_add_loop_down))
            GLOBL(C(asm_addto_loop_down))
            GLOBL(C(asm_inc_loop_down))
            GLOBL(C(asm_sub_loop_down))
            GLOBL(C(asm_subx_loop_down))
            GLOBL(C(asm_subfrom_loop_down))
            GLOBL(C(asm_dec_loop_down))
            GLOBL(C(asm_neg_loop_down))
            GLOBL(C(asm_shift1left_loop_down))
            GLOBL(C(asm_shiftleft_loop_down))
            GLOBL(C(asm_shiftleftcopy_loop_down))
            GLOBL(C(asm_shift1right_loop_up))
            GLOBL(C(asm_shiftright_loop_up))
            GLOBL(C(asm_shiftrightsigned_loop_up))
            GLOBL(C(asm_shiftrightcopy_loop_up))
            GLOBL(C(asm_mulusmall_loop_down))
            GLOBL(C(asm_mulu_loop_down))
            GLOBL(C(asm_muluadd_loop_down))
            GLOBL(C(asm_mulusub_loop_down))
            GLOBL(C(asm_divu_loop_up))
            GLOBL(C(asm_divucopy_loop_up))





            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_mulu32_64)
FUNBEGIN(asm_mulu32_64)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(eax))
            INSN1(mul,l	,X4 MEM_DISP(esp,8))
            ret
L(endof_asm_mulu32_64):
            FUNEND(asm_mulu32_64,L(endof_asm_mulu32_64)-asm_mulu32_64)





            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_divu_6432_3232_)
FUNBEGIN(asm_divu_6432_3232_)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(eax))
            INSN1(div,l	,X4 MEM_DISP(esp,12))
            ret
L(endof_asm_divu_6432_3232_):
            FUNEND(asm_divu_6432_3232_,L(endof_asm_divu_6432_3232_)-asm_divu_6432_3232_)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_copy_loop_up)
FUNBEGIN(asm_copy_loop_up)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,R(esi),R(eax))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(ecx))
            dir0start
            REP movsl
            dir0end
            INSN2(mov,l	,R(eax),R(esi))
            INSN2(mov,l	,R(edi),R(eax))
            INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_copy_loop_up):
            FUNEND(asm_copy_loop_up,L(endof_asm_copy_loop_up)-asm_copy_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_copy_loop_down)
FUNBEGIN(asm_copy_loop_down)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,R(esi),R(eax))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(ecx))
            INSN2(lea,l	,X4 MEM_DISP(esi,-4),R(esi))
            INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
            dir1start
            REP movsl
            dir1end
            INSN2(mov,l	,R(eax),R(esi))
            INSN2(lea,l	,X4 MEM_DISP(edi,4),R(eax))
            INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_copy_loop_down):
            FUNEND(asm_copy_loop_down,L(endof_asm_copy_loop_down)-asm_copy_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_fill_loop_up)
FUNBEGIN(asm_fill_loop_up)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(eax))
            dir0start
            REP stosl
            dir0end
            INSN2(mov,l	,R(edi),R(eax))
            INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_fill_loop_up):
            FUNEND(asm_fill_loop_up,L(endof_asm_fill_loop_up)-asm_fill_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_fill_loop_down)
FUNBEGIN(asm_fill_loop_down)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(eax))
            INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
            dir1start
            REP stosl
            dir1end
            INSN2(lea,l	,X4 MEM_DISP(edi,4),R(eax))
            INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_fill_loop_down):
            FUNEND(asm_fill_loop_down,L(endof_asm_fill_loop_down)-asm_fill_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_clear_loop_up)
FUNBEGIN(asm_clear_loop_up)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(xor,l	,R(eax),R(eax))
            dir0start
            REP stosl
            dir0end
            INSN2(mov,l	,R(edi),R(eax))
            INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_clear_loop_up):
            FUNEND(asm_clear_loop_up,L(endof_asm_clear_loop_up)-asm_clear_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_clear_loop_down)
FUNBEGIN(asm_clear_loop_down)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
            INSN2(xor,l	,R(eax),R(eax))
            dir1start
            REP stosl
            dir1end
            INSN2(lea,l	,X4 MEM_DISP(edi,4),R(eax))
            INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_clear_loop_down):
            FUNEND(asm_clear_loop_down,L(endof_asm_clear_loop_down)-asm_clear_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_or_loop_up)
FUNBEGIN(asm_or_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(olu2)
L(olu1): INSN2(mov,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN2(or,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(olu1))
L(olu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_or_loop_up):
            FUNEND(asm_or_loop_up,L(endof_asm_or_loop_up)-asm_or_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_xor_loop_up)
FUNBEGIN(asm_xor_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(xlu2)
L(xlu1): INSN2(mov,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN2(xor,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(xlu1))
L(xlu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_xor_loop_up):
            FUNEND(asm_xor_loop_up,L(endof_asm_xor_loop_up)-asm_xor_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_and_loop_up)
FUNBEGIN(asm_and_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(alu2)
L(alu1): INSN2(mov,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN2(and,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(alu1))
L(alu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_and_loop_up):
            FUNEND(asm_and_loop_up,L(endof_asm_and_loop_up)-asm_and_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_eqv_loop_up)
FUNBEGIN(asm_eqv_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(elu2)
L(elu1): INSN2(mov,l	,X4 MEM(edx),R(eax))
              INSN2(xor,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN1(not,l	,R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(elu1))
L(elu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_eqv_loop_up):
            FUNEND(asm_eqv_loop_up,L(endof_asm_eqv_loop_up)-asm_eqv_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_nand_loop_up)
FUNBEGIN(asm_nand_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(nalu2)
L(nalu1): INSN2(mov,l	,X4 MEM(edx),R(eax))
              INSN2(and,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN1(not,l	,R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(nalu1))
L(nalu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_nand_loop_up):
            FUNEND(asm_nand_loop_up,L(endof_asm_nand_loop_up)-asm_nand_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_nor_loop_up)
FUNBEGIN(asm_nor_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(nolu2)
L(nolu1): INSN2(mov,l	,X4 MEM(edx),R(eax))
              INSN2(or,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN1(not,l	,R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(nolu1))
L(nolu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_nor_loop_up):
            FUNEND(asm_nor_loop_up,L(endof_asm_nor_loop_up)-asm_nor_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_andc2_loop_up)
FUNBEGIN(asm_andc2_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(aclu2)
L(aclu1): INSN2(mov,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN1(not,l	,R(eax))
              INSN2(and,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(aclu1))
L(aclu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_andc2_loop_up):
            FUNEND(asm_andc2_loop_up,L(endof_asm_andc2_loop_up)-asm_andc2_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_orc2_loop_up)
FUNBEGIN(asm_orc2_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edx),R(esi))
            JECXZ L(oclu2)
L(oclu1): INSN2(mov,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN1(not,l	,R(eax))
              INSN2(or,l	,R(eax),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(oclu1))
L(oclu2): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_orc2_loop_up):
            FUNEND(asm_orc2_loop_up,L(endof_asm_orc2_loop_up)-asm_orc2_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_not_loop_up)
FUNBEGIN(asm_not_loop_up)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            JECXZ L(nlu2)
            nop ; nop ; nop ; nop ; nop ; nop
L(nlu1): INSN1(not,l	,X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(nlu1))
L(nlu2): ret
L(endof_asm_not_loop_up):
            FUNEND(asm_not_loop_up,L(endof_asm_not_loop_up)-asm_not_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_and_test_loop_up)
FUNBEGIN(asm_and_test_loop_up)
            INSN1(push,l	,R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            JECXZ L(atlu2)
            INSN2(sub,l	,R(edx),R(esi))
L(atlu1): INSN2(mov,l	,X4 MEM_INDEX(edx,esi),R(eax))
              INSN2(and,l	,X4 MEM(edx),R(eax))
              INSN1(jnz,_	,L(atlu3))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(atlu1))
L(atlu2): INSN2(xor,l	,R(eax),R(eax))
L(atlu3): INSN1(pop,l	,R(esi))
            ret
L(endof_asm_and_test_loop_up):
            FUNEND(asm_and_test_loop_up,L(endof_asm_and_test_loop_up)-asm_and_test_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_test_loop_up)
FUNBEGIN(asm_test_loop_up)
            INSN2(mov,l	,R(edi),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(xor,l	,R(eax),R(eax))
            dir0start
            REPZ scasl


            dir0end

            INSN1(jz,_	,L(tlu1))
            INSN1(inc,l	,R(eax))
L(tlu1): INSN2(mov,l	,R(edx),R(edi))
            ret
L(endof_asm_test_loop_up):
            FUNEND(asm_test_loop_up,L(endof_asm_test_loop_up)-asm_test_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_compare_loop_up)
FUNBEGIN(asm_compare_loop_up)
            INSN2(mov,l	,R(esi),R(edx))
            INSN2(mov,l	,R(edi),R(eax))
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(ecx))
            dir0start
            REPZ cmpsl


            dir0end




            INSN2(mov,l	,R(eax),R(edi))
            INSN2(mov,l	,R(edx),R(esi))
            INSN1(jbe,_	,L(cmlu1))
            INSN2(mov,l	,NUM(1),R(eax))
            ret
L(cmlu1): INSN2(sbb,l	,R(eax),R(eax))
            ret
L(endof_asm_compare_loop_up):
            FUNEND(asm_compare_loop_up,L(endof_asm_compare_loop_up)-asm_compare_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_add_loop_down)
FUNBEGIN(asm_add_loop_down)
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(ecx))
            INSN2(sub,l	,R(edi),R(edx))
            INSN2(sub,l	,R(edi),R(esi))
            INSN2(or,l	,R(ecx),R(ecx))
            INSN1(jz,_	,L(ald2))
L(ald1): INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edx,edi),R(eax))
              INSN2(adc,l	,X4 MEM_INDEX(esi,edi),R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(ald1))
L(ald2): INSN2(sbb,l	,R(eax),R(eax))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(endof_asm_add_loop_down):
            FUNEND(asm_add_loop_down,L(endof_asm_add_loop_down)-asm_add_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_addto_loop_down)
FUNBEGIN(asm_addto_loop_down)
            INSN1(push,l	,R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edi),R(edx))
            INSN2(or,l	,R(ecx),R(ecx))
            INSN1(jz,_	,L(atld2))
L(atld1): INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edx,edi),R(eax))
              INSN2(adc,l	,R(eax),X4 MEM(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(atld1))
L(atld2): INSN2(sbb,l	,R(eax),R(eax))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_addto_loop_down):
            FUNEND(asm_addto_loop_down,L(endof_asm_addto_loop_down)-asm_addto_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_inc_loop_down)
FUNBEGIN(asm_inc_loop_down)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            JECXZ L(ild2)
L(ild1): INSN2(lea,l	,X4 MEM_DISP(edx,-4),R(edx))
              INSN2(add,l	,NUM(1),X4 MEM(edx))
              INSN1(jnc,_	,L(ild3))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(ild1))
L(ild2): INSN2(mov,l	,NUM(1),R(eax))
            ret
L(ild3): INSN2(xor,l	,R(eax),R(eax))
            ret
L(endof_asm_inc_loop_down):
            FUNEND(asm_inc_loop_down,L(endof_asm_inc_loop_down)-asm_inc_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_sub_loop_down)
FUNBEGIN(asm_sub_loop_down)
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(ecx))
            INSN2(sub,l	,R(edi),R(edx))
            INSN2(sub,l	,R(edi),R(esi))
            INSN2(or,l	,R(ecx),R(ecx))
            INSN1(jz,_	,L(sld2))
L(sld1): INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edx,edi),R(eax))
              INSN2(sbb,l	,X4 MEM_INDEX(esi,edi),R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(sld1))
L(sld2): INSN2(sbb,l	,R(eax),R(eax))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(endof_asm_sub_loop_down):
            FUNEND(asm_sub_loop_down,L(endof_asm_sub_loop_down)-asm_sub_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_subx_loop_down)
FUNBEGIN(asm_subx_loop_down)
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(ecx))
            JECXZ L(sxld2)
            INSN2(sub,l	,R(edi),R(edx))
            INSN2(sub,l	,R(edi),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(eax))
            INSN2(add,l	,R(eax),R(eax))
            nop ; nop
L(sxld1): INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edx,edi),R(eax))
              INSN2(sbb,l	,X4 MEM_INDEX(esi,edi),R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(sxld1))
            INSN2(sbb,l	,R(eax),R(eax))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(sxld2): INSN2(mov,l	,X4 MEM_DISP(esp,28),R(eax))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(endof_asm_subx_loop_down):
            FUNEND(asm_subx_loop_down,L(endof_asm_subx_loop_down)-asm_subx_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_subfrom_loop_down)
FUNBEGIN(asm_subfrom_loop_down)
            INSN1(push,l	,R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ecx))
            INSN2(sub,l	,R(edi),R(edx))
            INSN2(or,l	,R(ecx),R(ecx))
            INSN1(jz,_	,L(sfld2))
L(sfld1): INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edx,edi),R(eax))
              INSN2(sbb,l	,R(eax),X4 MEM(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(sfld1))
L(sfld2): INSN2(sbb,l	,R(eax),R(eax))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_subfrom_loop_down):
            FUNEND(asm_subfrom_loop_down,L(endof_asm_subfrom_loop_down)-asm_subfrom_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_dec_loop_down)
FUNBEGIN(asm_dec_loop_down)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            JECXZ L(dld2)
L(dld1): INSN2(lea,l	,X4 MEM_DISP(edx,-4),R(edx))
              INSN2(sub,l	,NUM(1),X4 MEM(edx))
              INSN1(jnc,_	,L(dld3))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(dld1))
L(dld2): INSN2(mov,l	,NUM(-1),R(eax))
            ret
L(dld3): INSN2(xor,l	,R(eax),R(eax))
            ret
L(endof_asm_dec_loop_down):
            FUNEND(asm_dec_loop_down,L(endof_asm_dec_loop_down)-asm_dec_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_neg_loop_down)
FUNBEGIN(asm_neg_loop_down)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))

            JECXZ L(nld2)
L(nld1): INSN2(lea,l	,X4 MEM_DISP(edx,-4),R(edx))
              INSN1(neg,l	,X4 MEM(edx))
              INSN1(jnz,_	,L(nld3))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(nld1))
L(nld2): INSN2(xor,l	,R(eax),R(eax))
            ret
            nop ; nop ; nop ; nop ; nop ; nop
L(nld3):

            INSN1(dec,l	,R(ecx))
            INSN1(jz,_	,L(nld5))
L(nld4): INSN2(lea,l	,X4 MEM_DISP(edx,-4),R(edx))
              INSN1(not,l	,X4 MEM(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(nld4))
L(nld5): INSN2(mov,l	,NUM(-1),R(eax))
            ret
L(endof_asm_neg_loop_down):
            FUNEND(asm_neg_loop_down,L(endof_asm_neg_loop_down)-asm_neg_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shift1left_loop_down)
FUNBEGIN(asm_shift1left_loop_down)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(or,l	,R(ecx),R(ecx))
            INSN1(jz,_	,L(s1lld2))
            nop ; nop ; nop ; nop
L(s1lld1): INSN2(lea,l	,X4 MEM_DISP(edx,-4),R(edx))
              INSN2(rcl,l	,NUM(1),X4 MEM(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(s1lld1))
L(s1lld2): INSN2(sbb,l	,R(eax),R(eax))
            ret
L(endof_asm_shift1left_loop_down):
            FUNEND(asm_shift1left_loop_down,L(endof_asm_shift1left_loop_down)-asm_shift1left_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shiftleft_loop_down)
FUNBEGIN(asm_shiftleft_loop_down)
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ecx))
            INSN2(or,l	,R(edx),R(edx))
            INSN1(jz,_	,L(slld4))

            INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
            INSN2(mov,l	,X4 MEM(edi),R(eax))
            INSN2(mov,l	,R(eax),R(ebx))
            INSN2(shl,l	,R(cl),R(ebx))
            INSN2(or,l	,X4 MEM_DISP(esp,24),R(ebx))
            INSN2(mov,l	,R(ebx),X4 MEM(edi))

            INSN1(dec,l	,R(edx))
            INSN1(jz,_	,L(slld2))
            nop ; nop ; nop ; nop
L(slld1):
              INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM(edi),R(ebx))
              INSN2SHCL(shld,l	,R(eax),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jz,_	,L(slld3))

              INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM(edi),R(eax))
              INSN2SHCL(shld,l	,R(ebx),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jnz,_	,L(slld1))
L(slld2): INSN2(mov,l	,R(eax),R(ebx))
L(slld3): INSN2(xor,l	,R(eax),R(eax))
            INSN2SHCL(shld,l	,R(ebx),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            ret
L(slld4): INSN2(mov,l	,X4 MEM_DISP(esp,24),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_shiftleft_loop_down):
            FUNEND(asm_shiftleft_loop_down,L(endof_asm_shiftleft_loop_down)-asm_shiftleft_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shiftleftcopy_loop_down)
FUNBEGIN(asm_shiftleftcopy_loop_down)
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(ecx))
            INSN2(or,l	,R(edx),R(edx))
            INSN1(jz,_	,L(slcld4))
            INSN2(sub,l	,R(edi),R(esi))

            INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
            INSN2(mov,l	,X4 MEM_INDEX(edi,esi),R(ebx))
            INSN2(mov,l	,R(ebx),R(eax))
            INSN2(shl,l	,R(cl),R(eax))
            INSN2(mov,l	,R(eax),X4 MEM(edi))

            INSN1(neg,b	,R(cl))
            INSN1(dec,l	,R(edx))
            INSN1(jz,_	,L(slcld2))
L(slcld1):
              INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edi,esi),R(eax))
              INSN2SHCL(shrd,l	,R(eax),R(ebx))
              INSN2(mov,l	,R(ebx),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jz,_	,L(slcld3))

              INSN2(lea,l	,X4 MEM_DISP(edi,-4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edi,esi),R(ebx))
              INSN2SHCL(shrd,l	,R(ebx),R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jnz,_	,L(slcld1))
L(slcld2): INSN2(mov,l	,R(ebx),R(eax))
L(slcld3): INSN2(shr,l	,R(cl),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(slcld4): INSN2(xor,l	,R(eax),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(endof_asm_shiftleftcopy_loop_down):
            FUNEND(asm_shiftleftcopy_loop_down,L(endof_asm_shiftleftcopy_loop_down)-asm_shiftleftcopy_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shift1right_loop_up)
FUNBEGIN(asm_shift1right_loop_up)
            INSN2(mov,l	,X4 MEM_DISP(esp,4),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,8),R(ecx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(eax))
            JECXZ L(s1rld3)
            INSN2(add,l	,R(eax),R(eax))
L(s1rld1): INSN2(rcr,l	,NUM(1),X4 MEM(edx))
              INSN2(lea,l	,X4 MEM_DISP(edx,4),R(edx))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(s1rld1))
L(s1rld2): INSN2(sbb,l	,R(eax),R(eax))
L(s1rld3): ret
L(endof_asm_shift1right_loop_up):
            FUNEND(asm_shift1right_loop_up,L(endof_asm_shift1right_loop_up)-asm_shift1right_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shiftright_loop_up)
FUNBEGIN(asm_shiftright_loop_up)
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ecx))
            INSN2(or,l	,R(edx),R(edx))
            INSN1(jz,_	,L(srlu4))

            INSN2(mov,l	,X4 MEM(edi),R(eax))
            INSN2(mov,l	,R(eax),R(ebx))
            INSN2(shr,l	,R(cl),R(ebx))
            INSN2(mov,l	,R(ebx),X4 MEM(edi))

            INSN1(dec,l	,R(edx))
            INSN1(jz,_	,L(srlu2))
            nop ; nop ; nop
L(srlu1):
              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN2(mov,l	,X4 MEM(edi),R(ebx))
              INSN2SHCL(shrd,l	,R(eax),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jz,_	,L(srlu3))

              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN2(mov,l	,X4 MEM(edi),R(eax))
              INSN2SHCL(shrd,l	,R(ebx),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jnz,_	,L(srlu1))
L(srlu2): INSN2(mov,l	,R(eax),R(ebx))
L(srlu3): INSN2(xor,l	,R(eax),R(eax))
            INSN2SHCL(shrd,l	,R(ebx),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            ret
L(srlu4): INSN2(xor,l	,R(eax),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_shiftright_loop_up):
            FUNEND(asm_shiftright_loop_up,L(endof_asm_shiftright_loop_up)-asm_shiftright_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shiftrightsigned_loop_up)
FUNBEGIN(asm_shiftrightsigned_loop_up)
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ecx))

            INSN2(mov,l	,X4 MEM(edi),R(eax))
            INSN2(mov,l	,R(eax),R(ebx))
            INSN2(sar,l	,R(cl),R(ebx))
            INSN2(mov,l	,R(ebx),X4 MEM(edi))

            INSN1(dec,l	,R(edx))
            INSN1(jz,_	,L(srslu2))
L(srslu1):
              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN2(mov,l	,X4 MEM(edi),R(ebx))
              INSN2SHCL(shrd,l	,R(eax),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jz,_	,L(srslu3))

              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN2(mov,l	,X4 MEM(edi),R(eax))
              INSN2SHCL(shrd,l	,R(ebx),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jnz,_	,L(srslu1))
L(srslu2): INSN2(mov,l	,R(eax),R(ebx))
L(srslu3): INSN2(xor,l	,R(eax),R(eax))
            INSN2SHCL(shrd,l	,R(ebx),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_shiftrightsigned_loop_up):
            FUNEND(asm_shiftrightsigned_loop_up,L(endof_asm_shiftrightsigned_loop_up)-asm_shiftrightsigned_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_shiftrightcopy_loop_up)
FUNBEGIN(asm_shiftrightcopy_loop_up)
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(edx))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(ecx))
            INSN1(neg,b	,R(cl))
            INSN2(mov,l	,X4 MEM_DISP(esp,32),R(eax))
            INSN2(or,l	,R(edx),R(edx))
            INSN1(jz,_	,L(srcld3))
            INSN2(sub,l	,R(edi),R(esi))

            INSN2(mov,l	,X4 MEM_INDEX(edi,esi),R(ebx))
            INSN2SHCL(shld,l	,R(ebx),R(eax))
            INSN2(mov,l	,R(eax),X4 MEM(edi))

            INSN1(dec,l	,R(edx))
            INSN1(jz,_	,L(srcld2))
L(srcld1):
              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edi,esi),R(eax))
              INSN2SHCL(shld,l	,R(eax),R(ebx))
              INSN2(mov,l	,R(ebx),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jz,_	,L(srcld3))

              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN2(mov,l	,X4 MEM_INDEX(edi,esi),R(ebx))
              INSN2SHCL(shld,l	,R(ebx),R(eax))
              INSN2(mov,l	,R(eax),X4 MEM(edi))

              INSN1(dec,l	,R(edx))
              INSN1(jnz,_	,L(srcld1))
L(srcld2): INSN2(mov,l	,R(ebx),R(eax))
L(srcld3): INSN2(shl,l	,R(cl),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(esi))
            ret
L(endof_asm_shiftrightcopy_loop_up):
            FUNEND(asm_shiftrightcopy_loop_up,L(endof_asm_shiftrightcopy_loop_up)-asm_shiftrightcopy_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_mulusmall_loop_down)
FUNBEGIN(asm_mulusmall_loop_down)
            INSN1(push,l	,R(ebp))
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(ecx))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(ebp))
            INSN2(mov,l	,R(ecx),R(eax))
            INSN1(neg,l	,R(eax))
            INSN1(jz,_	,L(msld2))
            INSN2(lea,l	,X4 MEM_DISP_SHINDEX(edi,-4,eax,4),R(edi))
            nop ; nop ; nop
L(msld1): INSN2(mov,l	,X4 MEM_SHINDEX(edi,ecx,4),R(eax))
              INSN1(mul,l	,R(ebx))
              INSN2(add,l	,R(ebp),R(eax))
              INSN2(mov,l	,NUM(0),R(ebp))
              INSN2(adc,l	,R(edx),R(ebp))
              INSN2(mov,l	,R(eax),X4 MEM_SHINDEX(edi,ecx,4))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(msld1))
L(msld2): INSN2(mov,l	,R(ebp),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(ebp))
            ret
L(endof_asm_mulusmall_loop_down):
            FUNEND(asm_mulusmall_loop_down,L(endof_asm_mulusmall_loop_down)-asm_mulusmall_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_mulu_loop_down)
FUNBEGIN(asm_mulu_loop_down)
            INSN1(push,l	,R(ebp))
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,32),R(ecx))
            INSN2(mov,l	,R(ecx),R(eax))
            INSN1(not,l	,R(eax))
            INSN2(lea,l	,X4 MEM_SHINDEX(esi,eax,4),R(esi))
            INSN2(lea,l	,X4 MEM_SHINDEX(edi,eax,4),R(edi))
            INSN2(xor,l	,R(ebp),R(ebp))
L(muld1): INSN2(mov,l	,X4 MEM_SHINDEX(esi,ecx,4),R(eax))
              INSN1(mul,l	,R(ebx))
              INSN2(add,l	,R(ebp),R(eax))
              INSN2(mov,l	,NUM(0),R(ebp))
              INSN2(adc,l	,R(edx),R(ebp))
              INSN2(mov,l	,R(eax),X4 MEM_SHINDEX(edi,ecx,4))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(muld1))
            INSN2(mov,l	,R(ebp),X4 MEM(edi))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(esi))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(ebp))
            ret
L(endof_asm_mulu_loop_down):
            FUNEND(asm_mulu_loop_down,L(endof_asm_mulu_loop_down)-asm_mulu_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_muluadd_loop_down)
FUNBEGIN(asm_muluadd_loop_down)
            INSN1(push,l	,R(ebp))
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,32),R(ecx))
            INSN2(mov,l	,R(ecx),R(eax))
            INSN1(not,l	,R(eax))
            INSN2(lea,l	,X4 MEM_SHINDEX(esi,eax,4),R(esi))
            INSN2(lea,l	,X4 MEM_SHINDEX(edi,eax,4),R(edi))
            INSN2(xor,l	,R(ebp),R(ebp))
L(muald1): INSN2(mov,l	,X4 MEM_SHINDEX(esi,ecx,4),R(eax))
              INSN1(mul,l	,R(ebx))
              INSN2(add,l	,R(ebp),R(eax))
              INSN2(mov,l	,NUM(0),R(ebp))
              INSN2(adc,l	,R(ebp),R(edx))
              INSN2(add,l	,R(eax),X4 MEM_SHINDEX(edi,ecx,4))
              INSN2(adc,l	,R(edx),R(ebp))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(muald1))
            INSN2(mov,l	,R(ebp),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(esi))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(ebp))
            ret
L(endof_asm_muluadd_loop_down):
            FUNEND(asm_muluadd_loop_down,L(endof_asm_muluadd_loop_down)-asm_muluadd_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_mulusub_loop_down)
FUNBEGIN(asm_mulusub_loop_down)
            INSN1(push,l	,R(ebp))
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,32),R(ecx))
            INSN2(mov,l	,R(ecx),R(eax))
            INSN1(not,l	,R(eax))
            INSN2(lea,l	,X4 MEM_SHINDEX(esi,eax,4),R(esi))
            INSN2(lea,l	,X4 MEM_SHINDEX(edi,eax,4),R(edi))
            INSN2(xor,l	,R(ebp),R(ebp))
L(musld1): INSN2(mov,l	,X4 MEM_SHINDEX(esi,ecx,4),R(eax))
              INSN1(mul,l	,R(ebx))
              INSN2(add,l	,R(ebp),R(eax))
              INSN2(mov,l	,NUM(0),R(ebp))
              INSN2(adc,l	,R(ebp),R(edx))
              INSN2(sub,l	,R(eax),X4 MEM_SHINDEX(edi,ecx,4))
              INSN2(adc,l	,R(edx),R(ebp))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(musld1))
            INSN2(mov,l	,R(ebp),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(esi))
            INSN1(pop,l	,R(edi))
            INSN1(pop,l	,R(ebp))
            ret
L(endof_asm_mulusub_loop_down):
            FUNEND(asm_mulusub_loop_down,L(endof_asm_mulusub_loop_down)-asm_mulusub_loop_down)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_divu_loop_up)
FUNBEGIN(asm_divu_loop_up)
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,12),R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(ecx))
            INSN2(xor,l	,R(edx),R(edx))
            JECXZ L(dlu2)
L(dlu1): INSN2(mov,l	,X4 MEM(edi),R(eax))
              INSN1(div,l	,R(ebx))
              INSN2(mov,l	,R(eax),X4 MEM(edi))
              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(dlu1))
L(dlu2): INSN2(mov,l	,R(edx),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_divu_loop_up):
            FUNEND(asm_divu_loop_up,L(endof_asm_divu_loop_up)-asm_divu_loop_up)


            P2ALIGN(2,3)
            DECLARE_FUNCTION(asm_divucopy_loop_up)
FUNBEGIN(asm_divucopy_loop_up)
            INSN1(push,l	,R(edi))
            INSN1(push,l	,R(esi))
            INSN1(push,l	,R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,16),R(ebx))
            INSN2(mov,l	,X4 MEM_DISP(esp,20),R(esi))
            INSN2(mov,l	,X4 MEM_DISP(esp,24),R(edi))
            INSN2(mov,l	,X4 MEM_DISP(esp,28),R(ecx))
            INSN2(xor,l	,R(edx),R(edx))
            JECXZ L(dclu2)
            INSN2(sub,l	,R(edi),R(esi))
L(dclu1): INSN2(mov,l	,X4 MEM_INDEX(esi,edi),R(eax))
              INSN1(div,l	,R(ebx))
              INSN2(mov,l	,R(eax),X4 MEM(edi))
              INSN2(lea,l	,X4 MEM_DISP(edi,4),R(edi))
              INSN1(dec,l	,R(ecx))
              INSN1(jnz,_	,L(dclu1))
L(dclu2): INSN2(mov,l	,R(edx),R(eax))
            INSN1(pop,l	,R(ebx))
            INSN1(pop,l	,R(esi))
            INSN1(pop,l	,R(edi))
            ret
L(endof_asm_divucopy_loop_up):
            FUNEND(asm_divucopy_loop_up,L(endof_asm_divucopy_loop_up)-asm_divucopy_loop_up)

#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",@progbits
#endif
