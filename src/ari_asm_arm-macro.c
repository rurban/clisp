#include "asm-arm.h"
        .text


#if defined(__arm7m__) || defined(__arm8__) || defined(__arm9__) || defined(__strongarm__)

  #define HAVE_umull
#endif
        .global C(asm_mulu32_64)
        .align 2
        .type asm_mulu32_64,%function
FUNBEGIN(asm_mulu32_64)
#ifdef HAVE_umull
        MOV a3,a2
        UMULL a1,a2,a3,a1
#else
        MOV ip,a1,LSR $16
        MOV a3,a2,LSR $16
        BIC a1,a1,ip,LSL $16
        BIC a2,a2,a3,LSL $16
        MUL a4,a1,a2
        MUL a2,ip,a2
        MUL a1,a3,a1
        MUL a3,ip,a3
        ADDS a2,a2,a1

        ADDCS a3,a3,$0x10000
        ADDS a1,a4,a2,LSL $16
        ADC a2,a3,a2,LSR $16
#endif
        MOVS pc,lr
        FUNEND(asm_mulu32_64)
        .global C(asm_divu_3216_1616_)
        .align 2
        .type asm_divu_3216_1616_,%function
FUNBEGIN(asm_divu_3216_1616_)
        MOV a2,a2,LSL$15
        RSB a2,a2,$0
        ADDS a1,a2,a1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1

        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        ADCS a1,a2,a1,LSL$1
        SUBCC a1,a1,a2
        MOV a2,a1,LSR$15
        ADC a1,a1,a1
        MOV a1,a1,LSL$16
        MOV a1,a1,LSR$16
        MOVS pc, lr
        FUNEND(asm_divu_3216_1616_)
        .global C(asm_divu_6432_3232_)
        .align 2
        .type asm_divu_6432_3232_,%function
FUNBEGIN(asm_divu_6432_3232_)
        STMFD sp!, {v1,v2,v3,v4,v5,v6,lr}
        MOV v2, a2
        MOV v1, a3
        CMP a3, $0x10000
        BCS L(divu_6432_3232_l1)
        MOV a2, v2, LSR $16
        ORR a1, a2, a1, ASL $16
        MOV a2, v1
        BL asm_divu_3216_1616_
        MOV v3, a1
        MOV a1, v2, ASL $16
        MOV a1, a1, LSR $16
        ORR a1, a1, a2, ASL $16
        MOV a2, v1
        BL asm_divu_3216_1616_
        ORR a1, a1, v3, ASL $16
        LDMFD sp!, {v1,v2,v3,v4,v5,v6,pc}^

L(divu_6432_3232_l1):
        MOV v3, $0
        MOVS a4, v1, LSR $16
        ADDEQ v3, v3, $16
        MOVEQ v1, v1, ASL $16
        MOVS a4, v1, LSR $24
        ADDEQ v3, v3, $8
        MOVEQ v1, v1, ASL $8
        MOVS a4, v1, LSR $28
        ADDEQ v3, v3, $4
        MOVEQ v1, v1, ASL $4
        MOVS a4, v1, LSR $30
        ADDEQ v3, v3, $2
        MOVEQ v1, v1, ASL $2
        MOVS a4, v1, LSR $31
        ADDEQ v3, v3, $1
        MOVEQ v1, v1, ASL $1

        CMPS v3, $0
        MOVNE a2, a1, ASL v3
        RSBNE a1, v3, $32
        ORRNE a1, a2, v2, LSR a1
        MOVNE v2, v2, ASL v3
        ADD a2, v1, $0x10000
        MOVS v5, a2, LSR $16
        MOVEQ v4, a1, ASL $16
        MOVEQ a1, a1, LSR $16
        MOVNE a2, v5
        BLNE asm_divu_3216_1616_
        MOVNE v4, a2, ASL $16
        ORR v4, v4, v2, LSR $16
        MOV a4, v1, ASL $16
        MOV a4, a4, LSR $16
        MUL a3, a4, a1
        RSB a3, a3, a1, ASL $16
        MOV v6, a1
        ADDS a1, v4, a3
        ADDCS v6, v6, $1
        SUBCS a1, a1, v1
        CMP a1, v1
        ADDCS v6, v6, $1
        SUBCS a1, a1, v1
        CMP v5, $0
        MOVEQ v4, a1, ASL $16
        MOVEQ a1, a1, LSR $16
        MOVNE a2, v5
        BLNE asm_divu_3216_1616_
        MOVNE v4, a2, ASL $16
        MOV v2, v2, ASL $16
        ORR v4, v4, v2, LSR $16
        MOV a4, v1, ASL $16
        MOV a4, a4, LSR $16
        MUL a3, a4, a1
        RSB a3, a3, a1, ASL $16
        ADDS v4, v4, a3
        ADDCS a1, a1, $1
        SUBCS v4, v4, v1
        CMP v4, v1
        ADDCS a1, a1, $1
        SUBCS v4, v4, v1
        MOV a2, v4, LSR v3
        ORR a1, a1, v6, ASL $16
        LDMFD sp!, {v1,v2,v3,v4,v5,v6,pc}^
        FUNEND(asm_divu_6432_3232_)
        .global C(asm_copy_loop_up)
        .align 2
        .type asm_copy_loop_up,%function
FUNBEGIN(asm_copy_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_copy_loop_up_l1)
        CMP a4,$2
        LDR a4,[a1],$4
        STR a4,[a2],$4
        LDRGE a4,[a1],$4
        STRGE a4,[a2],$4
        LDRGT a4,[a1],$4
        STRGT a4,[a2],$4
L(asm_copy_loop_up_l1):
        BICS a4,a3,$3
        MOVEQ a1,a2
        MOVEQS pc,lr
        STMFD sp!,{v1,lr}
L(asm_copy_loop_up_l2):
        LDMIA a1!,{a3,v1,ip,lr}
        STMIA a2!,{a3,v1,ip,lr}
        SUBS a4,a4,$8
        LDMGEIA a1!,{a3,v1,ip,lr}
        STMGEIA a2!,{a3,v1,ip,lr}
        BGT L(asm_copy_loop_up_l2)
        MOV a1,a2
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_copy_loop_up)
        .global C(asm_copy_loop_down)
        .align 2
        .type asm_copy_loop_down,%function
FUNBEGIN(asm_copy_loop_down)
        ANDS a4,a3,$3
        BEQ L(asm_copy_loop_down_l1)
        CMP a4,$2
        LDR a4,[a1,$-4]!
        STR a4,[a2,$-4]!
        LDRGE a4,[a1,$-4]!
        STRGE a4,[a2,$-4]!
        LDRGT a4,[a1,$-4]!
        STRGT a4,[a2,$-4]!
L(asm_copy_loop_down_l1):
        BICS a4,a3,$3
        MOVEQ a1,a2
        MOVEQS pc,lr
        STMFD sp!,{v1,lr}
L(asm_copy_loop_down_l2):
        LDMDB a1!,{a3,v1,ip,lr}
        STMDB a2!,{a3,v1,ip,lr}
        SUBS a4,a4,$8
        LDMGEDB a1!,{a3,v1,ip,lr}
        STMGEDB a2!,{a3,v1,ip,lr}
        BGT L(asm_copy_loop_down_l2)
        MOV a1,a2
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_copy_loop_down)
        .global C(asm_clear_loop_up)
        .align 2
        .type asm_clear_loop_up,%function
FUNBEGIN(asm_clear_loop_up)
        MOV a3,$0
        .global C(asm_fill_loop_up)
        .align 2
        .type asm_fill_loop_up,%function
FUNBEGIN(asm_fill_loop_up)
        ANDS a4,a2,$3
        BEQ L(asm_fill_loop_up_l1)
        CMP a4,$2
        STR a3,[a1],$4
        STRGE a3,[a1],$4
        STRGT a3,[a1],$4
L(asm_fill_loop_up_l1):
        BICS a4,a2,$3
        MOVEQS pc,lr
        STMFD sp!,{v1,lr}
        MOV v1,a3
        MOV ip,a3
        MOV lr,a3
L(asm_fill_loop_up_l2):
        STMIA a1!,{a3,v1,ip,lr}
        SUBS a4,a4,$8
        STMGEIA a1!,{a3,v1,ip,lr}
        BGT L(asm_fill_loop_up_l2)
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_clear_loop_up)
        .global C(asm_clear_loop_down)
        .align 2
        .type asm_clear_loop_down,%function
FUNBEGIN(asm_clear_loop_down)
        MOV a3,$0
        .global C(asm_fill_loop_down)
        .align 2
        .type asm_fill_loop_down,%function
FUNBEGIN(asm_fill_loop_down)
        ANDS a4,a2,$3
        BEQ L(asm_fill_loop_down_l1)
        CMP a4,$2
        STR a3,[a1,$-4]!
        STRGE a3,[a1,$-4]!
        STRGT a3,[a1,$-4]!
L(asm_fill_loop_down_l1):
        BICS a4,a2,$3
        MOVEQS pc,lr
        STMFD sp!,{v1,lr}
        MOV v1,a3
        MOV ip,a3
        MOV lr,a3
L(asm_fill_loop_down_l2):
        STMDB a1!,{a3,v1,ip,lr}
        SUBS a4,a4,$8
        STMGEDB a1!,{a3,v1,ip,lr}
        BGT L(asm_fill_loop_down_l2)
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_clear_loop_down)
        .global C(asm_or_loop_up)
        .align 2
        .type asm_or_loop_up,%function
FUNBEGIN(asm_or_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_or_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        ORR ip,ip,a4
        STR ip,[a1],$4
        BLT L(asm_or_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        ORRGE ip,ip,a4
        STRGE ip,[a1],$4
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        ORRGT ip,ip,a4
        STRGT ip,[a1],$4
L(asm_or_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_or_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        ORR v3,v3,a3
        ORR v4,v4,v1
        ORR v5,v5,v2
        ORR lr,lr,ip
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_or_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_or_loop_up)
        .global C(asm_xor_loop_up)
        .align 2
        .type asm_xor_loop_up,%function
FUNBEGIN(asm_xor_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_xor_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        EOR ip,ip,a4
        STR ip,[a1],$4
        BLT L(asm_xor_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        EORGE ip,ip,a4
        STRGE ip,[a1],$4
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        EORGT ip,ip,a4
        STRGT ip,[a1],$4
L(asm_xor_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_xor_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        EOR v3,v3,a3
        EOR v4,v4,v1
        EOR v5,v5,v2
        EOR lr,lr,ip
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_xor_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_xor_loop_up)
        .global C(asm_and_loop_up)
        .align 2
        .type asm_and_loop_up,%function
FUNBEGIN(asm_and_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_and_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        AND ip,ip,a4
        STR ip,[a1],$4
        BLT L(asm_and_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        ANDGE ip,ip,a4
        STRGE ip,[a1],$4
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        ANDGT ip,ip,a4
        STRGT ip,[a1],$4
L(asm_and_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_and_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        AND v3,v3,a3
        AND v4,v4,v1
        AND v5,v5,v2
        AND lr,lr,ip
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_and_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_and_loop_up)
        .global C(asm_eqv_loop_up)
        .align 2
        .type asm_eqv_loop_up,%function
FUNBEGIN(asm_eqv_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_eqv_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        EOR ip,ip,a4
        MVN ip,ip
        STR ip,[a1],$4
        BLT L(asm_eqv_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        EORGE ip,ip,a4
        MVNGE ip,ip
        STRGE ip,[a1],$4
        BLE L(asm_eqv_loop_up_l1)
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        EORGT ip,ip,a4
        MVNGT ip,ip
        STRGT ip,[a1],$4
L(asm_eqv_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_eqv_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        EOR v3,v3,a3
        MVN v3,v3
        EOR v4,v4,v1
        MVN v4,v4
        EOR v5,v5,v2
        MVN v5,v5
        EOR lr,lr,ip
        MVN lr,lr
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_eqv_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_eqv_loop_up)
        .global C(asm_nand_loop_up)
        .align 2
        .type asm_nand_loop_up,%function
FUNBEGIN(asm_nand_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_nand_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        AND ip,ip,a4
        MVN ip,ip
        STR ip,[a1],$4
        BLT L(asm_nand_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        ANDGE ip,ip,a4
        MVNGE ip,ip
        STRGE ip,[a1],$4
        BLE L(asm_nand_loop_up_l1)
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        ANDGT ip,ip,a4
        MVNGT ip,ip
        STRGT ip,[a1],$4
L(asm_nand_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_nand_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        AND v3,v3,a3
        MVN v3,v3
        AND v4,v4,v1
        MVN v4,v4
        AND v5,v5,v2
        MVN v5,v5
        AND lr,lr,ip
        MVN lr,lr
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_nand_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_nand_loop_up)
        .global C(asm_nor_loop_up)
        .align 2
        .type asm_nor_loop_up,%function
FUNBEGIN(asm_nor_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_nor_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        ORR ip,ip,a4
        MVN ip,ip
        STR ip,[a1],$4
        BLT L(asm_nor_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        ORRGE ip,ip,a4
        MVNGE ip,ip
        STRGE ip,[a1],$4
        BLE L(asm_nor_loop_up_l1)
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        ORRGT ip,ip,a4
        MVNGT ip,ip
        STRGT ip,[a1],$4
L(asm_nor_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_nor_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        ORR v3,v3,a3
        MVN v3,v3
        ORR v4,v4,v1
        MVN v4,v4
        ORR v5,v5,v2
        MVN v5,v5
        ORR lr,lr,ip
        MVN lr,lr
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_nor_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_nor_loop_up)
        .global C(asm_andc2_loop_up)
        .align 2
        .type asm_andc2_loop_up,%function
FUNBEGIN(asm_andc2_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_andc2_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        BIC ip,ip,a4
        STR ip,[a1],$4
        BLT L(asm_andc2_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        BICGE ip,ip,a4
        STRGE ip,[a1],$4
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        BICGT ip,ip,a4
        STRGT ip,[a1],$4
L(asm_andc2_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_andc2_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        BIC v3,v3,a3
        BIC v4,v4,v1
        BIC v5,v5,v2
        BIC lr,lr,ip
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_andc2_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_andc2_loop_up)
        .global C(asm_orc2_loop_up)
        .align 2
        .type asm_orc2_loop_up,%function
FUNBEGIN(asm_orc2_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_orc2_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1]
        MVN a4,a4
        ORR ip,ip,a4
        STR ip,[a1],$4
        BLT L(asm_orc2_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1]
        MVNGE a4,a4
        ORRGE ip,ip,a4
        STRGE ip,[a1],$4
        BLE L(asm_orc2_loop_up_l1)
        LDRGT a4,[a2],$4
        LDRGT ip,[a1]
        MVNGT a4,a4
        ORRGT ip,ip,a4
        STRGT ip,[a1],$4
L(asm_orc2_loop_up_l1):
        BICS a4,a3,$3
        MOVEQS pc,lr
        STMFD sp!,{v1-v5,lr}
L(asm_orc2_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA a1,{v3,v4,v5,lr}
        MVN a3,a3
        ORR v3,v3,a3
        MVN v1,v1
        ORR v4,v4,v1
        MVN v2,v2
        ORR v5,v5,v2
        MVN ip,ip
        ORR lr,lr,ip
        STMIA a1!,{v3,v4,v5,lr}
        SUBS a4,a4,$4
        BGT L(asm_orc2_loop_up_l2)
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_orc2_loop_up)
        .global C(asm_not_loop_up)
        .align 2
        .type asm_not_loop_up,%function
FUNBEGIN(asm_not_loop_up)
        ANDS a3,a2,$3
        BEQ L(asm_not_loop_up_l1)
        CMP a3,$2
        LDR a3,[a1]
        MVN a3,a3
        STR a3,[a1],$4
        BLT L(asm_not_loop_up_l1)
        LDRGE a3,[a1]
        MVNGE a3,a3
        STRGE a3,[a1],$4
        LDRGT a3,[a1]
        MVNGT a3,a3
        STRGT a3,[a1],$4
L(asm_not_loop_up_l1):
        BICS a4,a2,$3
        MOVEQS pc,lr
        STMFD sp!,{lr}
L(asm_not_loop_up_l2):
        LDMIA a1,{a2,a3,ip,lr}
        MVN a2,a2
        MVN a3,a3
        MVN ip,ip
        MVN lr,lr
        STMIA a1!,{a2,a3,ip,lr}
        SUBS a4,a4,$4
        BGT L(asm_not_loop_up_l2)
        LDMFD sp!,{pc}^
        FUNEND(asm_not_loop_up)
        .global C(asm_and_test_loop_up)
        .align 2
        .type asm_and_test_loop_up,%function
FUNBEGIN(asm_and_test_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_and_test_loop_up_l1)
        CMP a4,$2
        LDR a4,[a2],$4
        LDR ip,[a1],$4
        TST ip,a4
        MOVNE a1,$1
        MOVNES pc,lr
        BCC L(asm_and_test_loop_up_l1)
        LDRGE a4,[a2],$4
        LDRGE ip,[a1],$4
        TSTGE ip,a4
        MOVNE a1,$1
        MOVNES pc,lr
        ANDS a4,a3,$3
        CMP a4,$2
        BLE L(asm_and_test_loop_up_l1)
        LDRGT a4,[a2],$4
        LDRGT ip,[a1],$4
        TSTGT ip,a4
        MOVNE a1,$1
        MOVNES pc,lr
L(asm_and_test_loop_up_l1):
        BICS a4,a3,$3
        MOVEQ a1,$0
        MOVEQS pc,lr
        STMFD sp!,{v1-v6,lr}
        MOV v6,a1
        MOV a1,$1
L(asm_and_test_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA v6!,{v3,v4,v5,lr}
        TST v3,a3
        TSTEQ v4,v1
        TSTEQ v5,v2
        TSTEQ lr,ip
        LDMNEFD sp!,{v1-v6,pc}^
        SUBS a4,a4,$4
        BGT L(asm_and_test_loop_up_l2)
        MOV a1,$0
        LDMFD sp!,{v1-v6,pc}^
        FUNEND(asm_and_test_loop_up)
        .global C(asm_test_loop_up)
        .align 2
        .type asm_test_loop_up,%function
FUNBEGIN(asm_test_loop_up)
        MOV ip,a1
        MOV a1,$1
        ANDS a3,a2,$3
        BEQ L(asm_test_loop_up_l1)
        LDR a4,[ip],$4
        TEQ a4,$0
        MOVNES pc,lr
        CMP a3,$2
        BLT L(asm_test_loop_up_l1)
        LDRGE a4,[ip],$4
        TEQGE a4,$0
        MOVNES pc,lr
        CMP a3,$2
        BLE L(asm_test_loop_up_l1)
        LDRGT a4,[ip],$4
        TEQGT a4,$0
        MOVNES pc,lr
L(asm_test_loop_up_l1):
        BICS a4,a2,$3
        MOVEQ a1,$0
        MOVEQS pc,lr
        STMFD sp!,{v1,lr}
L(asm_test_loop_up_l2):
        LDMIA ip!,{a2,a3,v1,lr}
        TEQ a2,$0
        TEQEQ a3,$0
        TEQEQ v1,$0
        TEQEQ lr,$0
        LDMNEFD sp!,{v1,pc}^
        SUBS a4,a4,$4
        BGT L(asm_test_loop_up_l2)
        MOV a1,$0
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_test_loop_up)
        .global C(asm_compare_loop_up)
        .align 2
        .type asm_compare_loop_up,%function
FUNBEGIN(asm_compare_loop_up)
        ANDS a4,a3,$3
        BEQ L(asm_compare_loop_up_l1)
        LDR a4,[a2],$4
        LDR ip,[a1],$4
        CMP ip,a4
        MVNLO a1,$0
        MOVHI a1,$1
        MOVNES pc,lr
        ANDS a4,a3,$3
        CMP a4,$2
        BLT L(asm_compare_loop_up_l1)
        LDR a4,[a2],$4
        LDR ip,[a1],$4
        CMP ip,a4
        MVNLO a1,$0
        MOVHI a1,$1
        MOVNES pc,lr
        ANDS a4,a3,$3
        CMP a4,$2
        BLE L(asm_compare_loop_up_l1)
        LDR a4,[a2],$4
        LDR ip,[a1],$4
        CMP ip,a4
        MVNLO a1,$0
        MOVHI a1,$1
        MOVNES pc,lr
L(asm_compare_loop_up_l1):
        BICS a4,a3,$3
        MOVEQ a1,$0
        MOVEQS pc,lr
        STMFD sp!,{v1-v6,lr}
        MOV v6,a1
        MOV a1,$1
L(asm_compare_loop_up_l2):
        LDMIA a2!,{a3,v1,v2,ip}
        LDMIA v6!,{v3,v4,v5,lr}
        CMP v3,a3
        CMPEQ v4,v1
        CMPEQ v5,v2
        CMPEQ lr,ip
        MVNLO a1,$0
        LDMNEFD sp!,{v1-v6,pc}^
        SUBS a4,a4,$4
        BGT L(asm_compare_loop_up_l2)
        MOV a1,$0
        LDMFD sp!,{v1-v6,pc}^
        FUNEND(asm_compare_loop_up)
        .global C(asm_addto_loop_down)
        .align 2
        .type asm_addto_loop_down,%function
FUNBEGIN(asm_addto_loop_down)
                MOV a4,a3
                MOV a3,a2
        .global C(asm_add_loop_down)
        .align 2
        .type asm_add_loop_down,%function
FUNBEGIN(asm_add_loop_down)
        ANDS ip,a4,$3
        BEQ L(asm_add_loop_down_l1)
        STMFD sp!,{v6,lr}
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        ADDS lr,lr,v6
        STR lr,[a3,$-4]!
        TEQ ip,$1
        BEQ L(asm_add_loop_down_l0)
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        ADCS lr,lr,v6
        STR lr,[a3,$-4]!
        TEQ ip,$2
        BEQ L(asm_add_loop_down_l0)
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        ADCS lr,lr,v6
        STR lr,[a3,$-4]!
L(asm_add_loop_down_l0):
        BICS a4,a4,$3
        BNE L(asm_add_loop_down_l3)
        ADCEQ a1,a4,a4
        LDMEQFD sp!,{v6,pc}^
L(asm_add_loop_down_l1):
        BICS a4,a4,$3
        MOVEQ a1,$0
        MOVEQS pc,lr
        CMN a4,$0
        STMFD sp!,{v6,lr}
L(asm_add_loop_down_l3):
        STMFD sp!,{v1-v5}
L(asm_add_loop_down_l2):
        LDMDB a2!,{v1,v2,v3,ip}
        LDMDB a1!,{v4,v5,v6,lr}
        ADCS lr,lr,ip
        ADCS v6,v6,v3
        ADCS v5,v5,v2
        ADCS v4,v4,v1
        STMDB a3!,{v4,v5,v6,lr}
        SUB a4,a4,$4
        TEQ a4,$0
        BNE L(asm_add_loop_down_l2)
        ADC a1,a4,a4
        LDMFD sp!,{v1-v6,pc}^
        FUNEND(asm_addto_loop_down)
        .global C(asm_inc_loop_down)
        .align 2
        .type asm_inc_loop_down,%function
FUNBEGIN(asm_inc_loop_down)
        ANDS a3,a2,$1
        BEQ L(asm_inc_loop_down_l1)
        LDR a4,[a1,$-4]!
        ADDS a4,a4,$1
        STR a4,[a1]
        MOVNE a1,$0
        MOVNES pc,lr
L(asm_inc_loop_down_l1):
        BICS a4,a2,$1
        MOVEQ a1,$1
        MOVEQS pc,lr
        MOV ip,a1
        MOV a1,$0
        ANDS a3,a4,$3
        BEQ L(asm_inc_loop_down_l3)
        LDMDB ip,{a2,a3}
        ADDS a3,a3,$1
        ADDEQS a2,a2,$1
        STMDB ip!,{a2,a3}
        MOVNES pc,lr
        SUBS a4,a4,$2
        MOVEQ a1,$1
        MOVEQS pc,lr
L(asm_inc_loop_down_l3):
        STMFD sp!,{v1,lr}
L(asm_inc_loop_down_l2):
        LDMDB ip,{a2,a3,v1,lr}
        ADDS lr,lr,$1
        ADDEQS v1,v1,$1
        ADDEQS a3,a3,$1
        ADDEQS a2,a2,$1
        STMDB ip!,{a2,a3,v1,lr}
        LDMNEFD sp!,{v1,pc}^
        SUBS a4,a4,$4
        BGT L(asm_inc_loop_down_l2)
        MOV a1,$1
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_inc_loop_down)
        .global C(asm_sub_loop_down)
        .align 2
        .type asm_sub_loop_down,%function
FUNBEGIN(asm_sub_loop_down)
        ANDS ip,a4,$3
        BEQ L(asm_sub_loop_down_l1)
        STMFD sp!,{v6,lr}
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        SUBS lr,lr,v6
        STR lr,[a3,$-4]!
        TEQ ip,$1
        BNE L(asm_sub_loop_down_l0)
L(asm_sub_loop_down_l4):
        BICS a4,a4,$3
        SBCEQ a1,a4,a4
        LDMEQFD sp!,{v6,pc}^
        STMFD sp!,{v1-v5}
        B L(asm_sub_loop_down_l2)
L(asm_sub_loop_down_l0):
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        SBCS lr,lr,v6
        STR lr,[a3,$-4]!
        TEQ ip,$2
        BEQ L(asm_sub_loop_down_l4)
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        SBCS lr,lr,v6
        STR lr,[a3,$-4]!
        B L(asm_sub_loop_down_l4)
L(asm_sub_loop_down_l1):
        BICS a4,a4,$3
        MOVEQ a1,$0
        MOVEQS pc,lr
        CMP a4,$0
        STMFD sp!,{v1-v6,lr}
L(asm_sub_loop_down_l2):
        LDMDB a2!,{v1,v2,v3,ip}
        LDMDB a1!,{v4,v5,v6,lr}
        SBCS lr,lr,ip
        SBCS v6,v6,v3
        SBCS v5,v5,v2
        SBCS v4,v4,v1
        STMDB a3!,{v4,v5,v6,lr}
        SUB a4,a4,$4
        TEQ a4,$0
        BNE L(asm_sub_loop_down_l2)
        SBC a1,a4,a4
        LDMFD sp!,{v1-v6,pc}^
        FUNEND(asm_sub_loop_down)
        .global C(asm_subx_loop_down)
        .align 2
        .type asm_subx_loop_down,%function
FUNBEGIN(asm_subx_loop_down)
        LDR ip,[sp]
L(asm_subx_loop_down_lsub):
        RSBS ip,ip,$0
        ANDS ip,a4,$3
        BEQ L(asm_subx_loop_down_l1)
        STMFD sp!,{v6,lr}
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        SBCS lr,lr,v6
        STR lr,[a3,$-4]!
        TEQ ip,$1
        BNE L(asm_subx_loop_down_l0)
L(asm_subx_loop_down_l4):
        BICS a4,a4,$3
        SBCEQ a1,a4,a4
        LDMEQFD sp!,{v6,pc}^
        STMFD sp!,{v1-v5}
        B L(asm_subx_loop_down_l2)
L(asm_subx_loop_down_l0):
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        SBCS lr,lr,v6
        STR lr,[a3,$-4]!
        TEQ ip,$2
        BEQ L(asm_subx_loop_down_l4)
        LDR v6,[a2,$-4]!
        LDR lr,[a1,$-4]!
        SBCS lr,lr,v6
        STR lr,[a3,$-4]!
        B L(asm_subx_loop_down_l4)
L(asm_subx_loop_down_l1):
        BICS a4,a4,$3
        SBCEQ a1,a4,a4
        MOVEQS pc,lr
        STMFD sp!,{v1-v6,lr}
L(asm_subx_loop_down_l2):
        LDMDB a2!,{v1,v2,v3,ip}
        LDMDB a1!,{v4,v5,v6,lr}
        SBCS lr,lr,ip
        SBCS v6,v6,v3
        SBCS v5,v5,v2
        SBCS v4,v4,v1
        STMDB a3!,{v4,v5,v6,lr}
        SUB a4,a4,$4
        TEQ a4,$0
        BNE L(asm_subx_loop_down_l2)
        SBC a1,a4,a4
        LDMFD sp!,{v1-v6,pc}^
        FUNEND(asm_subx_loop_down)
        .global C(asm_subfrom_loop_down)
        .align 2
        .type asm_subfrom_loop_down,%function
FUNBEGIN(asm_subfrom_loop_down)
        ANDS ip,a3,$3
        BEQ L(asm_subfrom_loop_down_l1)
        STMFD sp!,{lr}
        LDR a4,[a1,$-4]!
        LDR lr,[a2,$-4]!
        SUBS lr,lr,a4
        STR lr,[a2]
        TEQ ip,$1
        BNE L(asm_subfrom_loop_down_l0)
L(asm_subfrom_loop_down_l4):
        BICS a4,a3,$3
        SBCEQ a1,a4,a4
        LDMEQFD sp!,{pc}^
        STMFD sp!,{v1-v5}
        B L(asm_subfrom_loop_down_l2)
L(asm_subfrom_loop_down_l0):
        LDR a4,[a1,$-4]!
        LDR lr,[a2,$-4]!
        SBCS lr,lr,a4
        STR lr,[a2]
        TEQ ip,$2
        BEQ L(asm_subfrom_loop_down_l4)
        LDR a4,[a1,$-4]!
        LDR lr,[a2,$-4]!
        SBCS lr,lr,a4
        STR lr,[a2]
        B L(asm_subfrom_loop_down_l4)
L(asm_subfrom_loop_down_l1):
        BICS a4,a3,$3
        MOVEQ a1,$0
        MOVEQS pc,lr
        CMP a4,$0
        STMFD sp!,{v1-v5,lr}
L(asm_subfrom_loop_down_l2):
        LDMDB a1!,{a3,v1,v2,ip}
        LDMDB a2,{v3,v4,v5,lr}
        SBCS lr,lr,ip
        SBCS v5,v5,v2
        SBCS v4,v4,v1
        SBCS v3,v3,a3
        STMDB a2!,{v3,v4,v5,lr}
        SUB a4,a4,$4
        TEQ a4,$0
        BNE L(asm_subfrom_loop_down_l2)
        SBC a1,a4,a4
        LDMFD sp!,{v1-v5,pc}^
        FUNEND(asm_subfrom_loop_down)
        .global C(asm_dec_loop_down)
        .align 2
        .type asm_dec_loop_down,%function
FUNBEGIN(asm_dec_loop_down)
        ANDS a3,a2,$1
        BEQ L(asm_dec_loop_down_l1)
        LDR a4,[a1,$-4]!
        SUBS a4,a4,$1
        STR a4,[a1]
        MOVCS a1,$0
        MOVCSS pc,lr
L(asm_dec_loop_down_l1):
        BICS a4,a2,$1
        MVNEQ a1,$0
        MOVEQS pc,lr
        MOV ip,a1
        MOV a1,$0
        ANDS a3,a4,$3
        BEQ L(asm_dec_loop_down_l3)
        LDMDB ip,{a2,a3}
        SUBS a3,a3,$1
        SUBCCS a2,a2,$1
        STMDB ip!,{a2,a3}
        MOVCSS pc,lr
        SUBS a4,a4,$2
        MVNEQ a1,$0
        MOVEQS pc,lr
L(asm_dec_loop_down_l3):
        STMFD sp!,{v1,lr}
L(asm_dec_loop_down_l2):
        LDMDB ip,{a2,a3,v1,lr}
        SUBS lr,lr,$1
        SUBCCS v1,v1,$1
        SUBCCS a3,a3,$1
        SUBCCS a2,a2,$1
        STMDB ip!,{a2,a3,v1,lr}
        LDMCSFD sp!,{v1,pc}^
        SUBS a4,a4,$4
        BGT L(asm_dec_loop_down_l2)
        MVN a1,$0
        LDMFD sp!,{v1,pc}^
        FUNEND(asm_dec_loop_down)
        .global C(asm_neg_loop_down)
        .align 2
        .type asm_neg_loop_down,%function
FUNBEGIN(asm_neg_loop_down)
        CMPS a2,$0
        MOVEQ a1,$0
        MOVEQS pc,lr
L(asm_neg_loop_down_l1):
        LDR a3,[a1,$-4]!
        CMPS a3,$0
        BNE L(asm_neg_loop_down_l2)
        SUBS a2,a2,$1
        BNE L(asm_neg_loop_down_l1)
        MOV a1,$0
        MOVS pc,lr
L(asm_neg_loop_down_l2):
        RSB a3,a3,$0
        STR a3,[a1]
        SUBS a2,a2,$1
        MVNEQ a1,$0
        MOVEQS pc,lr

        ANDS a3,a2,$3
        BEQ L(asm_neg_loop_down_l3)
        CMP a3,$2
        LDR a3,[a1,$-4]!
        MVN a3,a3
        STR a3,[a1]
        BLT L(asm_neg_loop_down_l3)
        LDRGE a3,[a1,$-4]!
        MVNGE a3,a3
        STRGE a3,[a1]
        LDRGT a3,[a1,$-4]!
        MVNGT a3,a3
        STRGT a3,[a1]
L(asm_neg_loop_down_l3):
        BICS a4,a2,$3
        MVNEQ a1,$0
        MOVEQS pc,lr
        STMFD sp!,{lr}
L(asm_neg_loop_down_l4):
        LDMDB a1,{a2,a3,ip,lr}
        MVN a2,a2
        MVN a3,a3
        MVN ip,ip
        MVN lr,lr
        STMDB a1!,{a2,a3,ip,lr}
        SUBS a4,a4,$4
        BGT L(asm_neg_loop_down_l4)
        MVN a1,$0
        LDMFD sp!,{pc}^
        FUNEND(asm_neg_loop_down)
        .global C(asm_shift1left_loop_down)
        .align 2
        .type asm_shift1left_loop_down,%function
FUNBEGIN(asm_shift1left_loop_down)
        CMN a1,$0
        ANDS a3,a2,$1
        BEQ L(asm_shift1left_loop_down_l1)
        LDR a4,[a1,$-4]!
        ADDS a4,a4,a4
        STR a4,[a1]
L(asm_shift1left_loop_down_l1):
        BICS a4,a2,$1
        ADCEQ a1,a4,a4
        MOVEQS pc,lr
        ANDS a3,a4,$3
        BEQ L(asm_shift1left_loop_down_l3)
        LDMDB a1,{a2,a3}
        ADCS a3,a3,a3
        ADCS a2,a2,a2
        STMDB a1!,{a2,a3}
        BICS a4,a4,$2
        ADCEQ a1,a4,a4
        MOVEQS pc,lr
L(asm_shift1left_loop_down_l3):
        STMFD sp!,{lr}
L(asm_shift1left_loop_down_l2):
        LDMDB a1,{a2,a3,ip,lr}
        ADCS lr,lr,lr
        ADCS ip,ip,ip
        ADCS a3,a3,a3
        ADCS a2,a2,a2
        STMDB a1!,{a2,a3,ip,lr}
        SUB a4,a4,$4
        TEQ a4,$0
        BNE L(asm_shift1left_loop_down_l2)
        ADC a1,a4,a4
        LDMFD sp!,{pc}^
        FUNEND(asm_shift1left_loop_down)
        .global C(asm_shiftleft_loop_down)
        .align 2
        .type asm_shiftleft_loop_down,%function
FUNBEGIN(asm_shiftleft_loop_down)
        STMFD sp!,{v6,lr}
        RSB v6,a3,$32
        ANDS ip,a2,$3
        BEQ L(asm_shiftleft_loop_down_l1)
        LDR lr,[a1,$-4]!
        ORR a4,a4,lr,ASL a3
        STR a4,[a1,$0]
        MOV a4,lr,LSR v6
        CMP ip,$2
        BLT L(asm_shiftleft_loop_down_l1)
        LDRGE lr,[a1,$-4]!
        ORRGE a4,a4,lr,ASL a3
        STRGE a4,[a1,$0]
        MOVGE a4,lr,LSR v6
        LDRGT lr,[a1,$-4]!
        ORRGT a4,a4,lr,ASL a3
        STRGT a4,[a1,$0]
        MOVGT a4,lr,LSR v6
L(asm_shiftleft_loop_down_l1):
        BICS ip,a2,$3
        MOVEQ a1,a4
        LDMEQFD sp!,{v6,pc}^
        STMFD sp!,{v1-v3}
L(asm_shiftleft_loop_down_l2):
        LDMDB a1,{a2,v1,v2,v3}
        ORR lr,a4,v3,ASL a3
        MOV a4,v3,LSR v6
        ORR v3,a4,v2,ASL a3
        MOV a4,v2,LSR v6
        ORR v2,a4,v1,ASL a3
        MOV a4,v1,LSR v6
        ORR v1,a4,a2,ASL a3
        MOV a4,a2,LSR v6
        STMDB a1!,{v1,v2,v3,lr}
        SUBS ip,ip,$4
        BGT L(asm_shiftleft_loop_down_l2)
        MOV a1,a4
        LDMFD sp!,{v1-v3,v6,pc}^
        FUNEND(asm_shiftleft_loop_down)
        .global C(asm_shiftleftcopy_loop_down)
        .align 2
        .type asm_shiftleftcopy_loop_down,%function
FUNBEGIN(asm_shiftleftcopy_loop_down)
        STMFD sp!,{v5,v6,lr}
        MOV v5,$0
        RSB v6,a4,$32
        ANDS ip,a3,$3
        BEQ L(asm_shiftleftcopy_loop_down_l1)
        LDR lr,[a1,$-4]!
        ORR v5,v5,lr,ASL a4
        STR v5,[a2,$-4]!
        MOV v5,lr,LSR v6
        CMP ip,$2
        BLT L(asm_shiftleftcopy_loop_down_l1)
        LDRGE lr,[a1,$-4]!
        ORRGE v5,v5,lr,ASL a4
        STRGE v5,[a2,$-4]!
        MOVGE v5,lr,LSR v6
        LDRGT lr,[a1,$-4]!
        ORRGT v5,v5,lr,ASL a4
        STRGT v5,[a2,$-4]!
        MOVGT v5,lr,LSR v6
L(asm_shiftleftcopy_loop_down_l1):
        BICS ip,a3,$3
        MOVEQ a1,v5
        LDMEQFD sp!,{v5,v6,pc}^
        STMFD sp!,{v1-v3}
L(asm_shiftleftcopy_loop_down_l2):
        LDMDB a1!,{a3,v1,v2,v3}
        ORR lr,v5,v3,ASL a4
        MOV v5,v3,LSR v6
        ORR v3,v5,v2,ASL a4
        MOV v5,v2,LSR v6
        ORR v2,v5,v1,ASL a4
        MOV v5,v1,LSR v6
        ORR v1,v5,a3,ASL a4
        MOV v5,a3,LSR v6
        STMDB a2!,{v1,v2,v3,lr}
        SUBS ip,ip,$4
        BGT L(asm_shiftleftcopy_loop_down_l2)
        MOV a1,v5
        LDMFD sp!,{v1-v3,v5,v6,pc}^
        FUNEND(asm_shiftleftcopy_loop_down)
        .global C(asm_shift1right_loop_up)
        .align 2
        .type asm_shift1right_loop_up,%function
FUNBEGIN(asm_shift1right_loop_up)
        MOVS a3,a3,LSR $1
        ANDS a3,a2,$1
        BEQ L(asm_shift1right_loop_up_l1)
        LDR a4,[a1]
        MOVS a4,a4,rrx
        STR a4,[a1],$4
L(asm_shift1right_loop_up_l1):
        BICS a4,a2,$1
        MOVEQ a1,a4,rrx
        MOVEQS pc,lr
        ANDS a3,a4,$3
        BEQ L(asm_shift1right_loop_up_l3)
        LDMIA a1,{a2,a3}
        MOVS a2,a2,rrx
        MOVS a3,a3,rrx
        STMIA a1!,{a2,a3}
        BICS a4,a4,$2
        ADCEQ a1,a4,a4
        MOVEQS pc,lr
L(asm_shift1right_loop_up_l3):
        STMFD sp!,{lr}
L(asm_shift1right_loop_up_l2):
        LDMIA a1,{a2,a3,ip,lr}
        MOVS a2,a2,rrx
        MOVS a3,a3,rrx
        MOVS ip,ip,rrx
        MOVS lr,lr,rrx
        STMIA a1!,{a2,a3,ip,lr}
        SUB a4,a4,$4
        TEQ a4,$0
        BNE L(asm_shift1right_loop_up_l2)
        MOV a1,a4,rrx
        LDMFD sp!,{pc}^
        FUNEND(asm_shift1right_loop_up)
        .global C(asm_shiftright_loop_up)
        .align 2
        .type asm_shiftright_loop_up,%function
FUNBEGIN(asm_shiftright_loop_up)
        STMFD sp!,{v6,lr}
        MOV a4,$0
        RSB v6,a3,$32
L(asm_shiftright_loop_up_l0):
        ANDS ip,a2,$3
        BEQ L(asm_shiftright_loop_up_l1)
        LDR lr,[a1]
        ORR a4,a4,lr,LSR a3
        STR a4,[a1],$4
        MOV a4,lr,ASL v6
        CMP ip,$2
        BLT L(asm_shiftright_loop_up_l1)
        LDRGE lr,[a1]
        ORRGE a4,a4,lr,LSR a3
        STRGE a4,[a1],$4
        MOVGE a4,lr,ASL v6
        LDRGT lr,[a1]
        ORRGT a4,a4,lr,LSR a3
        STRGT a4,[a1],$4
        MOVGT a4,lr,ASL v6
L(asm_shiftright_loop_up_l1):
        BICS ip,a2,$3
        MOVEQ a1,a4
        LDMEQFD sp!,{v6,pc}^
        STMFD sp!,{v1-v3}
L(asm_shiftright_loop_up_l2):
        LDMIA a1,{v1,v2,v3,lr}
        ORR a2,a4,v1,LSR a3
        MOV a4,v1,ASL v6
        ORR v1,a4,v2,LSR a3
        MOV a4,v2,ASL v6
        ORR v2,a4,v3,LSR a3
        MOV a4,v3,ASL v6
        ORR v3,a4,lr,LSR a3
        MOV a4,lr,ASL v6
        STMIA a1!,{a2,v1,v2,v3}
        SUBS ip,ip,$4
        BGT L(asm_shiftright_loop_up_l2)
        MOV a1,a4
        LDMFD sp!,{v1-v3,v6,pc}^
        .global C(asm_shiftrightsigned_loop_up)
        .align 2
        .type asm_shiftrightsigned_loop_up,%function
FUNBEGIN(asm_shiftrightsigned_loop_up)
        STMFD sp!,{v6,lr}
        RSB v6,a3,$32
        LDR lr,[a1]
        MOV a4,lr,ASR $31
        AND a4,a4,a4,LSL v6
        B L(asm_shiftright_loop_up_l0)
        FUNEND(asm_shiftright_loop_up)
        .global C(asm_shiftrightcopy_loop_up)
        .align 2
        .type asm_shiftrightcopy_loop_up,%function
FUNBEGIN(asm_shiftrightcopy_loop_up)
        STMFD sp!,{v5,v6,lr}
        LDR v5,[sp,$12]
        RSB v6,a4,$32
        MOV v5,v5,ASL v6
L(asm_shiftrightcopy_loop_up_l0):
        ANDS ip,a3,$3
        BEQ L(asm_shiftrightcopy_loop_up_l1)
        LDR lr,[a1],$4
        ORR v5,v5,lr,LSR a4
        STR v5,[a2],$4
        MOV v5,lr,ASL v6
        CMP ip,$2
        BLT L(asm_shiftrightcopy_loop_up_l1)
        LDRGE lr,[a1],$4
        ORRGE v5,v5,lr,LSR a4
        STRGE v5,[a2],$4
        MOVGE v5,lr,ASL v6
        LDRGT lr,[a1],$4
        ORRGT v5,v5,lr,LSR a4
        STRGT v5,[a2],$4
        MOVGT v5,lr,ASL v6
L(asm_shiftrightcopy_loop_up_l1):
        BICS ip,a3,$3
        MOVEQ a1,v5
        LDMEQFD sp!,{v5,v6,pc}^
        STMFD sp!,{v1-v3}
L(asm_shiftrightcopy_loop_up_l2):
        LDMIA a1!,{v1,v2,v3,lr}
        ORR a3,v5,v1,LSR a4
        MOV v5,v1,ASL v6
        ORR v1,v5,v2,LSR a4
        MOV v5,v2,ASL v6
        ORR v2,v5,v3,LSR a4
        MOV v5,v3,ASL v6
        ORR v3,v5,lr,LSR a4
        MOV v5,lr,ASL v6
        STMIA a2!,{a3,v1,v2,v3}
        SUBS ip,ip,$4
        BGT L(asm_shiftrightcopy_loop_up_l2)
        MOV a1,v5
        LDMFD sp!,{v1-v3,v5,v6,pc}^
        FUNEND(asm_shiftrightcopy_loop_up)

#ifndef HAVE_umull
L(mulu32_64_vregs):
        MOV v1,a1,LSR $16
        MOV v2,ip,LSR $16
        BIC v3,a1,v1,LSL $16
        BIC ip,ip,v2,LSL $16
        MUL v4,v3,ip
        MUL ip,v1,ip
        MUL v3,v2,v3
        MUL v2,v1,v2
        ADDS ip,ip,v3

        ADDCS v2,v2,$0x10000
        ADDS v1,v4,ip,LSL $16
        ADC ip,v2,ip,LSR $16
        MOVS pc,lr
#endif
        .global C(asm_mulusmall_loop_down)
        .align 2
        .type asm_mulusmall_loop_down,%function
FUNBEGIN(asm_mulusmall_loop_down)
        CMP a3,$0
        MOVEQ a1,a4
        MOVEQS pc,lr
#ifdef HAVE_umull
        STMFD sp!,{v1,lr}
L(asm_mulusmall_loop_down_l1):
        LDR ip,[a2,$-4]!
        UMULL v1,ip,a1,ip
        ADDS v1,v1,a4
        ADC a4,ip,$0
        STR v1,[a2,$0]
        SUBS a3,a3,$1
        BNE L(asm_mulusmall_loop_down_l1)
        MOV a1,a4
        LDMFD sp!,{v1,pc}^
#else
        STMFD sp!,{v1-v2,lr}
L(asm_mulusmall_loop_down_l1):
        LDR ip,[a2,$-4]!



        MOV v1,ip,LSR $16
        BIC ip,ip,v1,LSL $16
        MUL v2,a1,v1
        MUL v1,a1,ip
        MOV ip,$0
        ADDS v1,v1,v2,LSL $16
        ADC ip,ip,v2,LSR $16

        ADDS v1,v1,a4
        ADC a4,ip,$0
        STR v1,[a2,$0]
        SUBS a3,a3,$1
        BNE L(asm_mulusmall_loop_down_l1)
        MOV a1,a4
        LDMFD sp!,{v1-v2,pc}^
#endif
        FUNEND(asm_mulusmall_loop_down)
        .global C(asm_mulu_loop_down)
        .align 2
        .type asm_mulu_loop_down,%function
FUNBEGIN(asm_mulu_loop_down)
#ifdef HAVE_umull
        STMFD sp!,{v1,v5,lr}
        MOV v5,$0
L(asm_mulu_loop_down_l1):
        LDR ip,[a2,$-4]!
        UMULL v1,ip,a1,ip
        ADDS v1,v1,v5
        ADC v5,ip,$0
        STR v1,[a3,$-4]!
        SUBS a4,a4,$1
        BNE L(asm_mulu_loop_down_l1)
        STR v5,[a3,$-4]!
        LDMFD sp!,{v1,v5,pc}^
#else
        STMFD sp!,{v1-v5,lr}
        MOV v5,$0
L(asm_mulu_loop_down_l1):
        LDR ip,[a2,$-4]!
        BL L(mulu32_64_vregs)
        ADDS v1,v1,v5
        ADC v5,ip,$0
        STR v1,[a3,$-4]!
        SUBS a4,a4,$1
        BNE L(asm_mulu_loop_down_l1)
        STR v5,[a3,$-4]!
        LDMFD sp!,{v1-v5,pc}^
#endif
        FUNEND(asm_mulu_loop_down)
        .global C(asm_muluadd_loop_down)
        .align 2
        .type asm_muluadd_loop_down,%function
FUNBEGIN(asm_muluadd_loop_down)
#ifdef HAVE_umull
        STMFD sp!,{v1,v5,lr}
        MOV v5,$0
L(asm_muluadd_loop_down_l1):
        LDR ip,[a2,$-4]!
        UMULL v1,ip,a1,ip
        ADDS v1,v1,v5
        ADCCS ip,ip,$0
        LDR v5,[a3,$-4]!
        ADDS v1,v1,v5
        ADC v5,ip,$0
        STR v1,[a3,$0]
        SUBS a4,a4,$1
        BNE L(asm_muluadd_loop_down_l1)
        MOV a1,v5
        LDMFD sp!,{v1,v5,pc}^
#else
        STMFD sp!,{v1-v5,lr}
        MOV v5,$0
L(asm_muluadd_loop_down_l1):
        LDR ip,[a2,$-4]!
        BL L(mulu32_64_vregs)
        ADDS v1,v1,v5
        ADCCS ip,ip,$0
        LDR v5,[a3,$-4]!
        ADDS v1,v1,v5
        ADC v5,ip,$0
        STR v1,[a3,$0]
        SUBS a4,a4,$1
        BNE L(asm_muluadd_loop_down_l1)
        MOV a1,v5
        LDMFD sp!,{v1-v5,pc}^
#endif
        FUNEND(asm_muluadd_loop_down)
        .global C(asm_mulusub_loop_down)
        .align 2
        .type asm_mulusub_loop_down,%function
FUNBEGIN(asm_mulusub_loop_down)
#ifdef HAVE_umull
        STMFD sp!,{v1,v5,lr}
        MOV v5,$0
L(asm_mulusub_loop_down_l1):
        LDR ip,[a2,$-4]!
        UMULL v1,ip,a1,ip
        ADDS v1,v1,v5
        ADC v5,ip,$0
        LDR ip,[a3,$-4]!
        SUBS ip,ip,v1
        STR ip,[a3,$0]
        ADDCC v5,v5,$1
        SUBS a4,a4,$1
        BNE L(asm_mulusub_loop_down_l1)
        MOV a1,v5
        LDMFD sp!,{v1,v5,pc}^
#else
        STMFD sp!,{v1-v5,lr}
        MOV v5,$0
L(asm_mulusub_loop_down_l1):
        LDR ip,[a2,$-4]!
        BL L(mulu32_64_vregs)
        ADDS v1,v1,v5
        ADC v5,ip,$0
        LDR ip,[a3,$-4]!
        SUBS ip,ip,v1
        STR ip,[a3,$0]
        ADDCC v5,v5,$1
        SUBS a4,a4,$1
        BNE L(asm_mulusub_loop_down_l1)
        MOV a1,v5
        LDMFD sp!,{v1-v5,pc}^
#endif
        FUNEND(asm_mulusub_loop_down)
#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
	.section .note.GNU-stack,"",%progbits
#endif
