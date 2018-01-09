# Externe Routinen zu ARILEV1.D
# Prozessor: MIPS
# Endianness: irrelevant
# Compiler: GNU-C oder ...
# Parameter passing conventions:
#   Arguments:
#     o32: in registers $4,$5,$6,$7, and on the stack 16($sp),...
#     n32: in registers $4,$5,$6,$7,$8,$9,$10,$11, and on the stack 4($sp),...
#   Return value register:
#     o32: $2 for a single word, $2,$3 for a 'long long'.
#     n32: $2
#   Call-used registers (do not have to be preserved across function calls):
#     $2..$15, $24
# Settings: intCsize=32, intDsize=32.
# Particularities:
#   After every load instruction a wait cycle is necessary, before the
#   fetched values may be used.
#   After branches and jumps, there is a delay slot. The assembler fills
#   it by pulling some instruction from before the jump (unless we use
#   the pseudo-op '.set noreorder' to disable this instruction reordering).

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS

#else

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

/* Note: When using GNU C, this function is not used, because we implement
   mulu32() as a macro that multiplies inline.  */
# extern struct { uint32 lo; uint32 hi; } asm_mulu32_ (uint32 arg1, uint32 arg2, uint32* hi_ptr);
# 2^32*hi+lo := arg1*arg2.
        .align 2
        .ent asm_mulu32_ # Input in $4,$5,$6, Output in $2
        .type asm_mulu32_,@function
asm_mulu32_:
        multu $5,$4             # arg1 * arg2
        mfhi $3                 # hi
        mflo $2                 # lo
        sw $3,($6)              # hi abspeichern
        j $31                   # return
        .end asm_mulu32_

# extern uintD* asm_copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent asm_copy_loop_up # Input in $4,$5,$6, Output in $2
        .type asm_copy_loop_up,@function
$Lcolu1:  lw $12,($4)           # d = *sourceptr
          addu $4,4             # sourceptr++
          sw $12,($5)           # *destptr = d
          addu $5,4             # destptr++
          subu $6,1             # count--
asm_copy_loop_up:
          bnez $6,$Lcolu1       # until (count==0)
        move $2,$5              # destptr
        j $31                   # return
        .end asm_copy_loop_up

# extern uintD* asm_copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent asm_copy_loop_down # Input in $4,$5,$6, Output in $2
        .type asm_copy_loop_down,@function
$Lcold1:  subu $4,4             # sourceptr--
          lw $12,($4)           # d = *sourceptr
          subu $5,4             # destptr--
          sw $12,($5)           # *destptr = d
          subu $6,1             # count--
asm_copy_loop_down:
          bnez $6,$Lcold1       # until (count==0)
        move $2,$5              # destptr
        j $31                   # return
        .end asm_copy_loop_down

# extern uintD* asm_fill_loop_up (uintD* destptr, uintC count, uintD filler);
        .align 2
        .ent asm_fill_loop_up # Input in $4,$5,$6, Output in $2
        .type asm_fill_loop_up,@function
$Lflu1:   sw $6,($4)            # *destptr = filler
          addu $4,4             # destptr++
          subu $5,1             # count--
asm_fill_loop_up:
          bnez $5,$Lflu1        # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end asm_fill_loop_up

# extern uintD* asm_fill_loop_down (uintD* destptr, uintC count, uintD filler);
        .align 2
        .ent asm_fill_loop_down # Input in $4,$5,$6, Output in $2
        .type asm_fill_loop_down,@function
$Lfld1:   subu $4,4             # destptr--
          sw $6,($4)            # *destptr = filler
          subu $5,1             # count--
asm_fill_loop_down:
          bnez $5,$Lfld1        # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end asm_fill_loop_down

# extern uintD* asm_clear_loop_up (uintD* destptr, uintC count);
        .align 2
        .ent asm_clear_loop_up # Input in $4,$5, Output in $2
        .type asm_clear_loop_up,@function
$Lcllu1:  sw $0,($4)            # *destptr = 0
          addu $4,4             # destptr++
          subu $5,1             # count--
asm_clear_loop_up:
          bnez $5,$Lcllu1       # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end asm_clear_loop_up

# extern uintD* asm_clear_loop_down (uintD* destptr, uintC count);
        .align 2
        .ent asm_clear_loop_down # Input in $4,$5, Output in $2
        .type asm_clear_loop_down,@function
$Lclld1:  subu $4,4             # destptr--
          sw $0,($4)            # *destptr = 0
          subu $5,1             # count--
asm_clear_loop_down:
          bnez $5,$Lclld1       # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end asm_clear_loop_down

# extern void asm_or_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_or_loop_up # Input in $4,$5,$6
        .type asm_or_loop_up,@function
$Lolu1:   lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          or $12,$13            # x |= y
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_or_loop_up:
          bnez $6,$Lolu1        # until (count==0)
        j $31                   # return
        .end asm_or_loop_up

# extern void asm_xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_xor_loop_up # Input in $4,$5,$6
        .type asm_xor_loop_up,@function
$Lxlu1:   lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          xor $12,$13           # x ^= y
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_xor_loop_up:
          bnez $6,$Lxlu1        # until (count==0)
        j $31                   # return
        .end asm_xor_loop_up

# extern void asm_and_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_and_loop_up # Input in $4,$5,$6
        .type asm_and_loop_up,@function
$Lalu1:   lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          and $12,$13           # x &= y
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_and_loop_up:
          bnez $6,$Lalu1        # until (count==0)
        j $31                   # return
        .end asm_and_loop_up

# extern void asm_eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_eqv_loop_up # Input in $4,$5,$6
        .type asm_eqv_loop_up,@function
$Lnxlu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          xor $12,$13           # x ^= y
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_eqv_loop_up:
          bnez $6,$Lnxlu1       # until (count==0)
        j $31                   # return
        .end asm_eqv_loop_up

# extern void asm_nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_nand_loop_up # Input in $4,$5,$6
        .type asm_nand_loop_up,@function
$Lnalu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          and $12,$13           # x &= y        # Gibt es 'nand $12,$13' ??
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_nand_loop_up:
          bnez $6,$Lnalu1       # until (count==0)
        j $31                   # return
        .end asm_nand_loop_up

# extern void asm_nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_nor_loop_up # Input in $4,$5,$6
        .type asm_nor_loop_up,@function
$Lnolu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          nor $12,$13           # x = ~(x|y)
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_nor_loop_up:
          bnez $6,$Lnolu1       # until (count==0)
        j $31                   # return
        .end asm_nor_loop_up

# extern void asm_andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_andc2_loop_up # Input in $4,$5,$6
        .type asm_andc2_loop_up,@function
$Laclu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          nor $13,$0            # y = ~y
          and $12,$13           # x &= y
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_andc2_loop_up:
          bnez $6,$Laclu1       # until (count==0)
        j $31                   # return
        .end asm_andc2_loop_up

# extern void asm_orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_orc2_loop_up # Input in $4,$5,$6
        .type asm_orc2_loop_up,@function
$Loclu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          nor $13,$0            # y = ~y
          or $12,$13            # x |= y
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_orc2_loop_up:
          bnez $6,$Loclu1       # until (count==0)
        j $31                   # return
        .end asm_orc2_loop_up

# extern void asm_not_loop_up (uintD* xptr, uintC count);
        .align 2
        .ent asm_not_loop_up # Input in $4,$5
        .type asm_not_loop_up,@function
$Lnlu1:   lw $12,($4)           # x = *xptr
          subu $5,1             # count--
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
          addu $4,4             # xptr++
asm_not_loop_up:
          bnez $5,$Lnlu1        # until (count==0)
        j $31                   # return
        .end asm_not_loop_up

# extern bool asm_and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_and_test_loop_up # Input in $4,$5,$6
        .type asm_and_test_loop_up,@function
$Latlu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          and $12,$13           # x &= y
          bnez $12,$Latlu3      # if (x) ...
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_and_test_loop_up:
          bnez $6,$Latlu1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Latlu3:li $2,1                 # 1
        j $31                   # return
        .end asm_and_test_loop_up

# extern bool asm_test_loop_up (uintD* ptr, uintC count);
        .align 2
        .ent asm_test_loop_up # Input in $4,$5
        .type asm_test_loop_up,@function
$Ltlu1:   lw $12,($4)           # x = *ptr
          addu $4,4             # ptr++
          bnez $12,$Ltlu3
          subu $5,1             # count--
asm_test_loop_up:
          bnez $5,$Ltlu1        # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Ltlu3: li $2,1                 # 1
        j $31                   # return
        .end asm_test_loop_up

# extern signean asm_compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent asm_compare_loop_up # Input in $4,$5,$6
        .type asm_compare_loop_up,@function
$Lcmlu1:  lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          addu $5,4             # yptr++
          bne $12,$13,$Lcmlu3   # if (!(x==y)) ...
          addu $4,4             # xptr++
          subu $6,1             # count--
asm_compare_loop_up:
          bnez $6,$Lcmlu1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Lcmlu3:bltu $12,$13,$Lcmlu4    # if (x<y) ...
        li $2,1                 # 1
        j $31                   # return
$Lcmlu4:li $2,-1                # -1
        j $31                   # return
        .end asm_compare_loop_up

# extern uintD asm_add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
        .align 2
        .ent asm_add_loop_down # Input in $4,$5,$6,$7, Output in $2
        .type asm_add_loop_down,@function
$Lald1:   # kein Carry
          subu $4,4             # sourceptr1--
          subu $5,4             # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          subu $6,4             # destptr--
          addu $12,$13          # dest = source1 + source2
          sw $12,($6)           # *destptr = dest
          bltu $12,$13,$Lald4   # if (dest < source2) [also Carry] ...
$Lald2:
          subu $7,1             # count--
asm_add_loop_down:
          bnez $7,$Lald1        # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Lald3: # Hier Carry
          subu $4,4             # sourceptr1--
          subu $5,4             # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          subu $6,4             # destptr--
          addu $12,$13          # dest = source1 + source2
          addu $12,1            #        + 1
          sw $12,($6)           # *destptr = dest
          bgtu $12,$13,$Lald2   # if (dest > source2) [also kein Carry] ...
$Lald4:   subu $7,1             # count--
          bnez $7,$Lald3        # until (count==0)
        li $2,1                 # 1
        j $31                   # return
        .end asm_add_loop_down

# extern uintD asm_addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent asm_addto_loop_down # Input in $4,$5,$6, Output in $2
        .type asm_addto_loop_down,@function
$Latld1:  # kein Carry
          subu $4,4             # sourceptr--
          subu $5,4             # destptr--
          lw $12,($4)           # source1 = *sourceptr
          lw $13,($5)           # source2 = *destptr
          subu $6,1             # count--
          addu $12,$13          # dest = source1 + source2
          sw $12,($5)           # *destptr = dest
          bltu $12,$13,$Latld4  # if (dest < source2) [also Carry] ...
asm_addto_loop_down:
$Latld2:  bnez $6,$Latld1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Latld3:# Hier Carry
          subu $4,4             # sourceptr--
          subu $5,4             # destptr--
          lw $12,($4)           # source1 = *sourceptr
          lw $13,($5)           # source2 = *destptr
          subu $6,1             # count--
          addu $12,$13          # dest = source1 + source2
          addu $12,1            #        + 1
          sw $12,($5)           # *destptr = dest
          bgtu $12,$13,$Latld2  # if (dest > source2) [also kein Carry] ...
$Latld4:  bnez $6,$Latld3       # until (count==0)
        li $2,1                 # 1
        j $31                   # return
        .end asm_addto_loop_down

# extern uintD asm_inc_loop_down (uintD* ptr, uintC count);
        .align 2
        .ent asm_inc_loop_down # Input in $4,$5, Output in $2
        .type asm_inc_loop_down,@function
$Lild1:   subu $4,4             # ptr--
          lw $12,($4)           # x = *ptr
          subu $5,1             # count--
          addu $12,1            # x++;
          sw $12,($4)           # *ptr = x
          bnez $12,$Lild3       # if (!(x==0)) ...
asm_inc_loop_down:
          bnez $5,$Lild1        # until (count==0)
        li $2,1                 # 1
        j $31                   # return
$Lild3: move $2,$0              # 0
        j $31                   # return
        .end asm_inc_loop_down

# extern uintD asm_sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
        .align 2
        .ent asm_sub_loop_down # Input in $4,$5,$6,$7, Output in $2
        .type asm_sub_loop_down,@function
$Lsld1:   # kein Carry
          subu $4,4             # sourceptr1--
          subu $5,4             # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          subu $6,4             # destptr--
          bltu $12,$13,$Lsld2   # if (source1 < source2) [also Carry] ...
          subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
asm_sub_loop_down:
          bnez $7,$Lsld1        # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Lsld2:   subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,$Lsld3        # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Lsld3: # Hier Carry
          subu $4,4             # sourceptr1--
          subu $5,4             # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          subu $6,4             # destptr--
          bgtu $12,$13,$Lsld4   # if (source1 > source2) [also kein Carry] ...
          subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,$Lsld3        # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Lsld4:   subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,$Lsld1        # until (count==0)
        move $2,$0              # 0
        j $31                   # return
        .end asm_sub_loop_down

# extern uintD asm_subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
        .align 2
        .ent asm_subx_loop_down # Input in $4,$5,$6,$7,$8, Output in $2
        .type asm_subx_loop_down,@function
asm_subx_loop_down:
%%if _MIPS_SIM == _ABIN32
        move $12,$8             # carry
%%else
        lw $12,16($sp)          # carry
%%endif
        bnez $12,$Lsxld5        # !(carry==0) ?
        b $Lsxld2
$Lsxld1:  # kein Carry
          subu $4,4             # sourceptr1--
          subu $5,4             # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          subu $6,4             # destptr--
          bltu $12,$13,$Lsxld3  # if (source1 < source2) [also Carry] ...
          subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
$Lsxld2:  bnez $7,$Lsxld1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Lsxld3:  subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,$Lsxld4       # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Lsxld4:# Hier Carry
          subu $4,4             # sourceptr1--
          subu $5,4             # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          subu $6,4             # destptr--
          bgtu $12,$13,$Lsxld6  # if (source1 > source2) [also kein Carry] ...
          subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
$Lsxld5:  bnez $7,$Lsxld4       # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Lsxld6:  subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,$Lsxld1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
        .end asm_subx_loop_down

# extern uintD asm_subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent asm_subfrom_loop_down # Input in $4,$5,$6,$7, Output in $2
        .type asm_subfrom_loop_down,@function
$Lsfld1:  # kein Carry
          subu $4,4             # sourceptr--
          subu $5,4             # destptr--
          lw $12,($5)           # source1 = *destptr
          lw $13,($4)           # source2 = *sourceptr
          subu $6,1             # count--
          bltu $12,$13,$Lsfld2  # if (source1 < source2) [also Carry] ...
          subu $12,$13          # dest = source1 - source2
          sw $12,($5)           # *destptr = dest
asm_subfrom_loop_down:
          bnez $6,$Lsfld1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Lsfld2:  subu $12,$13          # dest = source1 - source2
          sw $12,($5)           # *destptr = dest
          bnez $6,$Lsfld3       # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Lsfld3:# Hier Carry
          subu $4,4             # sourceptr--
          subu $5,4             # destptr--
          lw $12,($5)           # source1 = *destptr
          lw $13,($4)           # source2 = *sourceptr
          subu $6,1             # count--
          bgtu $12,$13,$Lsfld4  # if (source1 > source2) [also kein Carry] ...
          subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($5)           # *destptr = dest
          bnez $6,$Lsfld3       # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Lsfld4:  subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($5)           # *destptr = dest
          bnez $6,$Lsfld1       # until (count==0)
        move $2,$0              # 0
        j $31                   # return
        .end asm_subfrom_loop_down

# extern uintD asm_dec_loop_down (uintD* ptr, uintC count);
        .align 2
        .ent asm_dec_loop_down # Input in $4,$5, Output in $2
        .type asm_dec_loop_down,@function
$Ldld1:   subu $4,4             # ptr--
          lw $12,($4)           # x = *ptr
          subu $5,1             # count--
          bnez $12,$Ldld3       # if (!(x==0)) ...
          subu $12,1            # x--;
          sw $12,($4)           # *ptr = x
asm_dec_loop_down:
          bnez $5,$Ldld1        # until (count==0)
        li $2,-1                # -1
        j $31                   # return
$Ldld3: subu $12,1              # x--;
        sw $12,($4)             # *ptr = x
        move $2,$0              # 0
        j $31                   # return
        .end asm_dec_loop_down

# extern uintD asm_neg_loop_down (uintD* ptr, uintC count);
        .align 2
        .ent asm_neg_loop_down # Input in $4,$5, Output in $2
        .type asm_neg_loop_down,@function
        # erstes Digit /=0 suchen:
$Lnld1:   subu $4,4             # ptr--
          lw $12,($4)           # x = *ptr
          subu $5,1             # count--
          bnez $12,$Lnld3       # if (!(x==0)) ...
asm_neg_loop_down:
          bnez $5,$Lnld1        # until (count==0)
        move $2,$0              # 0
        j $31                   # return
$Lnld3: # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        # 1 Digit negieren:
        subu $12,$0,$12         # x = -x
        sw $12,($4)             # *ptr = x
        # alle anderen Digits invertieren:
        b $Lnld5
$Lnld4:   subu $4,4             # xptr--
          lw $12,($4)           # x = *xptr
          subu $5,1             # count--
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
$Lnld5:   bnez $5,$Lnld4        # until (count==0)
        li $2,-1                # -1
        j $31                   # return
        .end asm_neg_loop_down

#endif

