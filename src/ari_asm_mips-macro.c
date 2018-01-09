#include "asm-mips.h"
        .text

        .globl asm_mulu32_
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





        .align 2
        .ent asm_mulu32_
        DECLARE_FUNCTION(asm_mulu32_)
asm_mulu32_:
        multu $5,$4
        mfhi $3
        mflo $2
        sw $3,($6)
        j $31
        .end asm_mulu32_


        .align 2
        .ent asm_copy_loop_up
        DECLARE_FUNCTION(asm_copy_loop_up)
$Lcolu1: lw $12,($4)
          addu $4,4
          sw $12,($5)
          addu $5,4
          subu $6,1
asm_copy_loop_up:
          bnez $6,$Lcolu1
        move $2,$5
        j $31
        .end asm_copy_loop_up


        .align 2
        .ent asm_copy_loop_down
        DECLARE_FUNCTION(asm_copy_loop_down)
$Lcold1: subu $4,4
          lw $12,($4)
          subu $5,4
          sw $12,($5)
          subu $6,1
asm_copy_loop_down:
          bnez $6,$Lcold1
        move $2,$5
        j $31
        .end asm_copy_loop_down


        .align 2
        .ent asm_fill_loop_up
        DECLARE_FUNCTION(asm_fill_loop_up)
$Lflu1: sw $6,($4)
          addu $4,4
          subu $5,1
asm_fill_loop_up:
          bnez $5,$Lflu1
        move $2,$4
        j $31
        .end asm_fill_loop_up


        .align 2
        .ent asm_fill_loop_down
        DECLARE_FUNCTION(asm_fill_loop_down)
$Lfld1: subu $4,4
          sw $6,($4)
          subu $5,1
asm_fill_loop_down:
          bnez $5,$Lfld1
        move $2,$4
        j $31
        .end asm_fill_loop_down


        .align 2
        .ent asm_clear_loop_up
        DECLARE_FUNCTION(asm_clear_loop_up)
$Lcllu1: sw $0,($4)
          addu $4,4
          subu $5,1
asm_clear_loop_up:
          bnez $5,$Lcllu1
        move $2,$4
        j $31
        .end asm_clear_loop_up


        .align 2
        .ent asm_clear_loop_down
        DECLARE_FUNCTION(asm_clear_loop_down)
$Lclld1: subu $4,4
          sw $0,($4)
          subu $5,1
asm_clear_loop_down:
          bnez $5,$Lclld1
        move $2,$4
        j $31
        .end asm_clear_loop_down


        .align 2
        .ent asm_or_loop_up
        DECLARE_FUNCTION(asm_or_loop_up)
$Lolu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          or $12,$13
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_or_loop_up:
          bnez $6,$Lolu1
        j $31
        .end asm_or_loop_up


        .align 2
        .ent asm_xor_loop_up
        DECLARE_FUNCTION(asm_xor_loop_up)
$Lxlu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          xor $12,$13
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_xor_loop_up:
          bnez $6,$Lxlu1
        j $31
        .end asm_xor_loop_up


        .align 2
        .ent asm_and_loop_up
        DECLARE_FUNCTION(asm_and_loop_up)
$Lalu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          and $12,$13
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_and_loop_up:
          bnez $6,$Lalu1
        j $31
        .end asm_and_loop_up


        .align 2
        .ent asm_eqv_loop_up
        DECLARE_FUNCTION(asm_eqv_loop_up)
$Lnxlu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          xor $12,$13
          nor $12,$0
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_eqv_loop_up:
          bnez $6,$Lnxlu1
        j $31
        .end asm_eqv_loop_up


        .align 2
        .ent asm_nand_loop_up
        DECLARE_FUNCTION(asm_nand_loop_up)
$Lnalu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          and $12,$13
          nor $12,$0
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_nand_loop_up:
          bnez $6,$Lnalu1
        j $31
        .end asm_nand_loop_up


        .align 2
        .ent asm_nor_loop_up
        DECLARE_FUNCTION(asm_nor_loop_up)
$Lnolu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          nor $12,$13
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_nor_loop_up:
          bnez $6,$Lnolu1
        j $31
        .end asm_nor_loop_up


        .align 2
        .ent asm_andc2_loop_up
        DECLARE_FUNCTION(asm_andc2_loop_up)
$Laclu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          nor $13,$0
          and $12,$13
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_andc2_loop_up:
          bnez $6,$Laclu1
        j $31
        .end asm_andc2_loop_up


        .align 2
        .ent asm_orc2_loop_up
        DECLARE_FUNCTION(asm_orc2_loop_up)
$Loclu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          nor $13,$0
          or $12,$13
          sw $12,($4)
          addu $4,4
          subu $6,1
asm_orc2_loop_up:
          bnez $6,$Loclu1
        j $31
        .end asm_orc2_loop_up


        .align 2
        .ent asm_not_loop_up
        DECLARE_FUNCTION(asm_not_loop_up)
$Lnlu1: lw $12,($4)
          subu $5,1
          nor $12,$0
          sw $12,($4)
          addu $4,4
asm_not_loop_up:
          bnez $5,$Lnlu1
        j $31
        .end asm_not_loop_up


        .align 2
        .ent asm_and_test_loop_up
        DECLARE_FUNCTION(asm_and_test_loop_up)
$Latlu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          and $12,$13
          bnez $12,$Latlu3
          addu $4,4
          subu $6,1
asm_and_test_loop_up:
          bnez $6,$Latlu1
        move $2,$0
        j $31
$Latlu3:li $2,1
        j $31
        .end asm_and_test_loop_up


        .align 2
        .ent asm_test_loop_up
        DECLARE_FUNCTION(asm_test_loop_up)
$Ltlu1: lw $12,($4)
          addu $4,4
          bnez $12,$Ltlu3
          subu $5,1
asm_test_loop_up:
          bnez $5,$Ltlu1
        move $2,$0
        j $31
$Ltlu3: li $2,1
        j $31
        .end asm_test_loop_up


        .align 2
        .ent asm_compare_loop_up
        DECLARE_FUNCTION(asm_compare_loop_up)
$Lcmlu1: lw $12,($4)
          lw $13,($5)
          addu $5,4
          bne $12,$13,$Lcmlu3
          addu $4,4
          subu $6,1
asm_compare_loop_up:
          bnez $6,$Lcmlu1
        move $2,$0
        j $31
$Lcmlu3:bltu $12,$13,$Lcmlu4
        li $2,1
        j $31
$Lcmlu4:li $2,-1
        j $31
        .end asm_compare_loop_up


        .align 2
        .ent asm_add_loop_down
        DECLARE_FUNCTION(asm_add_loop_down)
$Lald1:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,4
          addu $12,$13
          sw $12,($6)
          bltu $12,$13,$Lald4
$Lald2:
          subu $7,1
asm_add_loop_down:
          bnez $7,$Lald1
        move $2,$0
        j $31
$Lald3:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,4
          addu $12,$13
          addu $12,1
          sw $12,($6)
          bgtu $12,$13,$Lald2
$Lald4: subu $7,1
          bnez $7,$Lald3
        li $2,1
        j $31
        .end asm_add_loop_down


        .align 2
        .ent asm_addto_loop_down
        DECLARE_FUNCTION(asm_addto_loop_down)
$Latld1:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,1
          addu $12,$13
          sw $12,($5)
          bltu $12,$13,$Latld4
asm_addto_loop_down:
$Latld2: bnez $6,$Latld1
        move $2,$0
        j $31
$Latld3:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,1
          addu $12,$13
          addu $12,1
          sw $12,($5)
          bgtu $12,$13,$Latld2
$Latld4: bnez $6,$Latld3
        li $2,1
        j $31
        .end asm_addto_loop_down


        .align 2
        .ent asm_inc_loop_down
        DECLARE_FUNCTION(asm_inc_loop_down)
$Lild1: subu $4,4
          lw $12,($4)
          subu $5,1
          addu $12,1
          sw $12,($4)
          bnez $12,$Lild3
asm_inc_loop_down:
          bnez $5,$Lild1
        li $2,1
        j $31
$Lild3: move $2,$0
        j $31
        .end asm_inc_loop_down


        .align 2
        .ent asm_sub_loop_down
        DECLARE_FUNCTION(asm_sub_loop_down)
$Lsld1:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,4
          bltu $12,$13,$Lsld2
          subu $12,$13
          sw $12,($6)
          subu $7,1
asm_sub_loop_down:
          bnez $7,$Lsld1
        move $2,$0
        j $31
$Lsld2: subu $12,$13
          sw $12,($6)
          subu $7,1
          bnez $7,$Lsld3
        li $2,-1
        j $31
$Lsld3:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,4
          bgtu $12,$13,$Lsld4
          subu $12,$13
          subu $12,1
          sw $12,($6)
          subu $7,1
          bnez $7,$Lsld3
        li $2,-1
        j $31
$Lsld4: subu $12,$13
          subu $12,1
          sw $12,($6)
          subu $7,1
          bnez $7,$Lsld1
        move $2,$0
        j $31
        .end asm_sub_loop_down


        .align 2
        .ent asm_subx_loop_down
        DECLARE_FUNCTION(asm_subx_loop_down)
asm_subx_loop_down:
#if _MIPS_SIM == _ABIN32
        move $12,$8
#else
        lw $12,16($sp)
#endif
        bnez $12,$Lsxld5
        b $Lsxld2
$Lsxld1:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,4
          bltu $12,$13,$Lsxld3
          subu $12,$13
          sw $12,($6)
          subu $7,1
$Lsxld2: bnez $7,$Lsxld1
        move $2,$0
        j $31
$Lsxld3: subu $12,$13
          sw $12,($6)
          subu $7,1
          bnez $7,$Lsxld4
        li $2,-1
        j $31
$Lsxld4:
          subu $4,4
          subu $5,4
          lw $12,($4)
          lw $13,($5)
          subu $6,4
          bgtu $12,$13,$Lsxld6
          subu $12,$13
          subu $12,1
          sw $12,($6)
          subu $7,1
$Lsxld5: bnez $7,$Lsxld4
        li $2,-1
        j $31
$Lsxld6: subu $12,$13
          subu $12,1
          sw $12,($6)
          subu $7,1
          bnez $7,$Lsxld1
        move $2,$0
        j $31
        .end asm_subx_loop_down


        .align 2
        .ent asm_subfrom_loop_down
        DECLARE_FUNCTION(asm_subfrom_loop_down)
$Lsfld1:
          subu $4,4
          subu $5,4
          lw $12,($5)
          lw $13,($4)
          subu $6,1
          bltu $12,$13,$Lsfld2
          subu $12,$13
          sw $12,($5)
asm_subfrom_loop_down:
          bnez $6,$Lsfld1
        move $2,$0
        j $31
$Lsfld2: subu $12,$13
          sw $12,($5)
          bnez $6,$Lsfld3
        li $2,-1
        j $31
$Lsfld3:
          subu $4,4
          subu $5,4
          lw $12,($5)
          lw $13,($4)
          subu $6,1
          bgtu $12,$13,$Lsfld4
          subu $12,$13
          subu $12,1
          sw $12,($5)
          bnez $6,$Lsfld3
        li $2,-1
        j $31
$Lsfld4: subu $12,$13
          subu $12,1
          sw $12,($5)
          bnez $6,$Lsfld1
        move $2,$0
        j $31
        .end asm_subfrom_loop_down


        .align 2
        .ent asm_dec_loop_down
        DECLARE_FUNCTION(asm_dec_loop_down)
$Ldld1: subu $4,4
          lw $12,($4)
          subu $5,1
          bnez $12,$Ldld3
          subu $12,1
          sw $12,($4)
asm_dec_loop_down:
          bnez $5,$Ldld1
        li $2,-1
        j $31
$Ldld3: subu $12,1
        sw $12,($4)
        move $2,$0
        j $31
        .end asm_dec_loop_down


        .align 2
        .ent asm_neg_loop_down
        DECLARE_FUNCTION(asm_neg_loop_down)

$Lnld1: subu $4,4
          lw $12,($4)
          subu $5,1
          bnez $12,$Lnld3
asm_neg_loop_down:
          bnez $5,$Lnld1
        move $2,$0
        j $31
$Lnld3:

        subu $12,$0,$12
        sw $12,($4)

        b $Lnld5
$Lnld4: subu $4,4
          lw $12,($4)
          subu $5,1
          nor $12,$0
          sw $12,($4)
$Lnld5: bnez $5,$Lnld4
        li $2,-1
        j $31
        .end asm_neg_loop_down
