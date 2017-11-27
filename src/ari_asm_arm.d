/* ariarm.d (c) Copyright 1994, 1997 P.J.Burwood
 bugfixes (c) Copyright 1996 B. Haible
 external routines for arilev1.d
 Processor: ARM in APCS mode
 Assembler-Syntax: GAS
 Parameter passing conventions, per APCS:
   Argument registers:
     r0..r3
   Return value register:
     r0 for a single word, r0,r1 for a 'long long'.
   Call-used registers (do not have to be preserved across function calls):
     r0..r3, r12 (= a1..a4, ip)
 Settings: intCsize=32, intDsize=32.
 Note: A sequence of up to 4 conditional instructions is used in preference
   to a branch. */

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS
  #define SHIFT_LOOPS
  #define MUL_LOOPS

#else

/* GAS syntax */

a1      .req    r0
a2      .req    r1
a3      .req    r2
a4      .req    r3
v1      .req    r4
v2      .req    r5
v3      .req    r6
v4      .req    r7
v5      .req    r8
v6      .req    r9
rfp     .req    r9
sl      .req    r10
fp      .req    r11
ip      .req    r12
sp      .req    r13
lr      .req    r14
pc      .req    r15

#define C(x) x
#define EXPORT(x) .global x
#define GLABEL(x)  x:
#define LABEL(x)  x:
#define END
	.text


#if defined(__arm7m__) || defined(__arm8__) || defined(__arm9__) || defined(__strongarm__)
  /* ARM7M and later have 32x32 -> 64 multiplies which execute in 2-4 clocks. */
  #define HAVE_umull
#endif


/* extern uint64 asm_mulu32_64 (uint32 x, uint32 y);
       entry
               a1 = x
               a2 = y
       exit
               a1 = low32(x*y)
               a2 = high32(x*y)
               a3,a4,ip destroyed */
        EXPORT(asm_mulu32_64)
GLABEL(asm_mulu32_64)
#ifdef HAVE_umull
        MOV     a3,a2
        UMULL   a1,a2,a3,a1
#else
        MOV     ip,a1,LSR #16    /* temp := top half of x */
        MOV     a3,a2,LSR #16    /* hi := top half of y */
        BIC     a1,a1,ip,LSL #16 /* x  := bottom half of x */
        BIC     a2,a2,a3,LSL #16 /* y  := bottom half of y */
        MUL     a4,a1,a2         /* low section of result */
        MUL     a2,ip,a2         /* ) middle sections */
        MUL     a1,a3,a1         /* )   of result */
        MUL     a3,ip,a3         /* high section of result */
        ADDS    a2,a2,a1         /* add middle sections */
                                 /* (can't use mla as we need carry) */
        ADDCS   a3,a3,#0x10000   /* carry from above add */
        ADDS    a1,a4,a2,LSL #16 /* x is now bottom 32 bits of result */
        ADC     a2,a3,a2,LSR #16 /* hi is top 32 bits */
#endif
        MOVS    pc,lr

/* extern uint16 asm_divu_3216_1616_ (uint32 x, uint16 y);
       entry
               a1 = x
               a2 = y
       exit
               a1 = q = floor(x/y)
               a2 = r = x-q*y
               a3 destroyed */
        EXPORT(asm_divu_3216_1616_)
GLABEL(asm_divu_3216_1616_)
        MOV     a2,a2,LSL#15    /* multiply divisor by 2^15 */
        RSB     a2,a2,#0        /* negate divisor */
        ADDS    a1,a2,a1        /* dividend = dividend + -divisor/2 */
        SUBCC   a1,a1,a2        /* dividend = dividend - -divisor/2 */
        ADCS    a1,a2,a1,LSL#1  /* dividend = dividend*2 + -divisor */
                                /* and shift quotient */
        SUBCC   a1,a1,a2        /* do this another 14 times */
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2
        ADCS    a1,a2,a1,LSL#1
        SUBCC   a1,a1,a2       /* do the last conditional subtraction */
        MOV     a2,a1,LSR#15   /* move remainder into a2 and shift */
        ADC     a1,a1,a1       /* move last bit of quotient in */
        MOV     a1,a1,LSL#16   /* AND out top 16 bits by shifting up */
        MOV     a1,a1,LSR#16   /* and back down again */
        MOVS    pc, lr

/* extern uint32 asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y); | -> Quotient q
 extern uint32 divu_32_rest;                                       | -> Rest r
       see arilev0 for algorithm
       entry
               a1 = xhi (dividend)
               a2 = xlo (dividend)
               a3 = y (divisor)
       exit
               a1 = 32 bit quotient
               a2 = 32 bit remainder
               a3, a4 destroyed */
        EXPORT(asm_divu_6432_3232_)
GLABEL(asm_divu_6432_3232_)
        STMFD   sp!, {v1,v2,v3,v4,v5,v6,lr}
        MOV     v2, a2          /* = xlo */
        MOV     v1, a3          /* = y */
        CMP     a3, #0x10000    /* y <= (uint32)(bit(16)-1) */
        BCS     divu_6432_3232_l1
        MOV     a2, v2, LSR #16
        ORR     a1, a2, a1, ASL #16 /* = highlow32(low16(xhi),high16(xlo)) */
        MOV     a2, v1
        BL      C(asm_divu_3216_1616_)
        MOV     v3, a1          /* = q1 */
        MOV     a1, v2, ASL #16
        MOV     a1, a1, LSR #16
        ORR     a1, a1, a2, ASL #16 /* = highlow32(r1,low16(xlo)) */
        MOV     a2, v1
        BL      C(asm_divu_3216_1616_)
        ORR     a1, a1, v3, ASL #16 /* = highlow32(q1,q0) */
        LDMFD   sp!, {v1,v2,v3,v4,v5,v6,pc}^

LABEL(divu_6432_3232_l1)
        MOV     v3, #0          /* s = 0 */
        MOVS    a4, v1, LSR #16 /* while ((sint32)y >= 0) */
        ADDEQ   v3, v3, #16     /*   { y = y<<1; s++; } */
        MOVEQ   v1, v1, ASL #16
        MOVS    a4, v1, LSR #24
        ADDEQ   v3, v3, #8
        MOVEQ   v1, v1, ASL #8
        MOVS    a4, v1, LSR #28
        ADDEQ   v3, v3, #4
        MOVEQ   v1, v1, ASL #4
        MOVS    a4, v1, LSR #30
        ADDEQ   v3, v3, #2
        MOVEQ   v1, v1, ASL #2
        MOVS    a4, v1, LSR #31
        ADDEQ   v3, v3, #1
        MOVEQ   v1, v1, ASL #1

        CMPS    v3, #0
        MOVNE   a2, a1, ASL v3     /* if (!(s==0)) */
        RSBNE   a1, v3, #32        /*   { xhi = (xhi << s) */
        ORRNE   a1, a2, v2, LSR a1 /*         | (xlo >> (32-s)); */
        MOVNE   v2, v2, ASL v3     /*     xlo = xlo << s; } */
        ADD     a2, v1, #0x10000   /* y1_1 = high16(y)+1 */
        MOVS    v5, a2, LSR #16    /* if (y1_1 = 0) */
        MOVEQ   v4, a1, ASL #16    /* r16 = low16(xhi) * 2^16 */
        MOVEQ   a1, a1, LSR #16    /* q1 = high16(xhi) */
        MOVNE   a2, v5
        BLNE    C(asm_divu_3216_1616_) /* divu_3216_1616(xhi,y1_1, q1=,r16=) */
        MOVNE   v4, a2, ASL #16    /* r16 = r16 * 2^16 */
        ORR     v4, v4, v2, LSR #16 /* r = highlow32(r16,high16(xlo)) */
        MOV     a4, v1, ASL #16     /* tmp = mulu16(low16(y),q1) */
        MOV     a4, a4, LSR #16
        MUL     a3, a4, a1
        RSB     a3, a3, a1, ASL #16 /* r2 = highlow32_0(q1) - tmp */
        MOV     v6, a1              /* = q1 */
        ADDS    a1, v4, a3          /* r += r2 */
        ADDCS   v6, v6, #1          /* if ( r < r2 ) { q1 += 1 */
        SUBCS   a1, a1, v1          /*                 r -= y } */
        CMP     a1, v1              /* if (r >= y) */
        ADDCS   v6, v6, #1          /*     { q1 += 1 */
        SUBCS   a1, a1, v1          /*       r -= y } */
        CMP     v5, #0              /* if (y1_1 = 0) */
        MOVEQ   v4, a1, ASL #16     /*    { r16 = low16(r) * 2^16 */
        MOVEQ   a1, a1, LSR #16     /*      q0  = high16(r) } */
        MOVNE   a2, v5
        BLNE    C(asm_divu_3216_1616_) /* divu_3216_1616(r,y1_1, q0=,r16=) */
        MOVNE   v4, a2, ASL #16     /* r16 = r16 * 2^16 */
        MOV     v2, v2, ASL #16
        ORR     v4, v4, v2, LSR #16 /* r = highlow32(r16,low16(xlo)) */
        MOV     a4, v1, ASL #16     /* tmp = mulu16(low16(y),q0) */
        MOV     a4, a4, LSR #16
        MUL     a3, a4, a1
        RSB     a3, a3, a1, ASL #16 /* r2 = highlow32_0(q0) - tmp */
        ADDS    v4, v4, a3          /* r += r2 */
        ADDCS   a1, a1, #1          /* if ( r < r2 ) { q0 += 1 */
        SUBCS   v4, v4, v1          /*                 r -= y } */
        CMP     v4, v1              /* if (r >= y) */
        ADDCS   a1, a1, #1          /*     { q0 += 1 */
        SUBCS   v4, v4, v1          /*       r -= y } */
        MOV     a2, v4, LSR v3      /* remainder = r >> s */
        ORR     a1, a1, v6, ASL #16 /* return highlow32(q1,q0) */
        LDMFD   sp!, {v1,v2,v3,v4,v5,v6,pc}^

/* extern uintD* asm_copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
       entry
               a1 = source pointer
               a2 = destination pointer
               a3 = count of words to store
       exit
               a1 = address of last word stored + 1
               a2 - a4, ip destroyed */
        EXPORT(asm_copy_loop_up)          /* word aligned copy loop up */
GLABEL(asm_copy_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_copy_loop_up_l1 /* yup, so branch */
        CMP     a4,#2           /* copy the first 1-3 words */
        LDR     a4,[a1],#4      /* to align the total to a multiple */
        STR     a4,[a2],#4      /* of 4 words */
        LDRGE   a4,[a1],#4
        STRGE   a4,[a2],#4
        LDRGT   a4,[a1],#4
        STRGT   a4,[a2],#4
LABEL(asm_copy_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,a2           /* return addr of last word stored */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1,lr}     /* save work regs */
LABEL(asm_copy_loop_up_l2)
        LDMIA   a1!,{a3,v1,ip,lr} /* copy 4 words in one go */
        STMIA   a2!,{a3,v1,ip,lr}
        SUBS    a4,a4,#8          /* decrement counter by 8 */
        LDMGEIA a1!,{a3,v1,ip,lr} /* if count still positive then copy */
        STMGEIA a2!,{a3,v1,ip,lr} /* 4 more words */
        BGT     asm_copy_loop_up_l2   /* and loop */
        MOV     a1,a2             /* return addr of last word stored */
        LDMFD   sp!,{v1,pc}^      /* restore work regs and return */

/* extern uintD* asm_copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
       entry
               a1 = source pointer
               a2 = destination pointer
               a3 = count of words to store
       exit
               a1 = address of last word stored
               a2 - a4, ip destroyed */
        EXPORT(asm_copy_loop_down)        /* word aligned copy loop down */
GLABEL(asm_copy_loop_down)
        ANDS    a4,a3,#3          /* multiple of 4 words ? */
        BEQ     asm_copy_loop_down_l1 /* yup, so branch */
        CMP     a4,#2             /* copy the first 1-3 words */
        LDR     a4,[a1,#-4]!      /* to align the total to a multiple */
        STR     a4,[a2,#-4]!      /* of 4 words */
        LDRGE   a4,[a1,#-4]!
        STRGE   a4,[a2,#-4]!
        LDRGT   a4,[a1,#-4]!
        STRGT   a4,[a2,#-4]!
LABEL(asm_copy_loop_down_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,a2           /* return addr of last word stored */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1,lr}     /* save work regs */
LABEL(asm_copy_loop_down_l2)
        LDMDB   a1!,{a3,v1,ip,lr} /* copy 4 words in one go */
        STMDB   a2!,{a3,v1,ip,lr}
        SUBS    a4,a4,#8          /* decrement counter by 8 */
        LDMGEDB a1!,{a3,v1,ip,lr} /* if count still positive then copy */
        STMGEDB a2!,{a3,v1,ip,lr} /* 4 more words */
        BGT     asm_copy_loop_down_l2 /* and loop */
        MOV     a1,a2             /* return addr of last word stored */
        LDMFD   sp!,{v1,pc}^      /* restore work regs and return */

/* extern uintD* asm_clear_loop_up (uintD* destptr, uintC count);
       entry
               a1 = destination pointer
               a2 = count of words to store
       exit
               a1 = address of last word stored + 1
               a2 - a4, ip destroyed */
        EXPORT(asm_clear_loop_up)         /* word aligned clear loop up */
GLABEL(asm_clear_loop_up)
        MOV     a3,#0           /* set filler to 0 */
                                        /* and drop into asm_fill_loop_up */

/* extern uintD* asm_fill_loop_up (uintD* destptr, uintC count, uintD filler);
       entry
               a1 = destination pointer
               a2 = count of words to store
               a3 = word to store
       exit
               a1 = address of last word stored + 1
               a2 - a4, ip destroyed */
        EXPORT(asm_fill_loop_up)          /* word aligned fill loop up */
GLABEL(asm_fill_loop_up)
        ANDS    a4,a2,#3        /* multiple of 4 words ? */
        BEQ     asm_fill_loop_up_l1 /* yup, so branch */
        CMP     a4,#2           /* store the first 1-3 words */
        STR     a3,[a1],#4      /* to align the total to a multiple */
        STRGE   a3,[a1],#4      /* of 4 words */
        STRGT   a3,[a1],#4
LABEL(asm_fill_loop_up_l1)
        BICS    a4,a2,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1,lr}     /* save work regs */
        MOV     v1,a3           /* copy filler to three other */
        MOV     ip,a3           /* registers */
        MOV     lr,a3
LABEL(asm_fill_loop_up_l2)
        STMIA   a1!,{a3,v1,ip,lr} /* store 4 fillers in one go */
        SUBS    a4,a4,#8          /* decrement counter by 8 */
        STMGEIA a1!,{a3,v1,ip,lr} /* if count still positive then store 4 */
        BGT     asm_fill_loop_up_l2   /* more and loop */
        LDMFD   sp!,{v1,pc}^      /* restore work regs and return */


/* extern uintD* asm_clear_loop_down (uintD* destptr, uintC count);
       entry
               a1 = destination pointer
               a2 = count of words to store
       exit
               a1 = address of last word stored + 1
               a2 - a4, ip destroyed */
        EXPORT(asm_clear_loop_down)       /* word aligned clear loop down */
GLABEL(asm_clear_loop_down)
        MOV     a3,#0                 /* set filler to 0 */
                                      /* and drop into asm_fill_loop_down */

/* extern uintD* asm_fill_loop_down (uintD* destptr, uintC count, uintD filler);
       entry
               a1 = destination pointer
               a2 = count of words to store
               a3 = word to store
       exit
               a1 = address of last word stored
               a2 - a4, ip destroyed */
        EXPORT(asm_fill_loop_down)        /* word aligned fill loop down */
GLABEL(asm_fill_loop_down)
        ANDS    a4,a2,#3          /* multiple of 4 words ? */
        BEQ     asm_fill_loop_down_l1 /* yup, so branch */
        CMP     a4,#2             /* store the first 1-3 words */
        STR     a3,[a1,#-4]!      /* to align the total to a multiple */
        STRGE   a3,[a1,#-4]!      /* of 4 words */
        STRGT   a3,[a1,#-4]!
LABEL(asm_fill_loop_down_l1)
        BICS    a4,a2,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1,lr}     /* save work regs */
        MOV     v1,a3           /* copy filler to three other */
        MOV     ip,a3           /* registers */
        MOV     lr,a3
LABEL(asm_fill_loop_down_l2)
        STMDB   a1!,{a3,v1,ip,lr} /* store 4 fillers in one go */
        SUBS    a4,a4,#8          /* decrement counter by 8 */
        STMGEDB a1!,{a3,v1,ip,lr} /* if count still positive then store 4 */
        BGT     asm_fill_loop_down_l2 /* more and loop */
        LDMFD   sp!,{v1,pc}^      /* restore work regs and return */

/* extern void asm_or_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be ORed
       exit
               xptr |= yptr for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_or_loop_up)            /* word aligned or loop up */
GLABEL(asm_or_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_or_loop_up_l1   /* yup, so branch */
        CMP     a4,#2           /* OR the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        ORR     ip,ip,a4
        STR     ip,[a1],#4
        BLT     asm_or_loop_up_l1   /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        ORRGE   ip,ip,a4
        STRGE   ip,[a1],#4
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        ORRGT   ip,ip,a4
        STRGT   ip,[a1],#4
LABEL(asm_or_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_or_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        ORR     v3,v3,a3          /* OR the four words */
        ORR     v4,v4,v1
        ORR     v5,v5,v2
        ORR     lr,lr,ip
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_or_loop_up_l2   /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be XORed
       exit
               xptr ^= yptr for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_xor_loop_up)           /* word aligned xor loop up */
GLABEL(asm_xor_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_xor_loop_up_l1  /* yup, so branch */
        CMP     a4,#2           /* XOR the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        EOR     ip,ip,a4
        STR     ip,[a1],#4
        BLT     asm_xor_loop_up_l1  /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        EORGE   ip,ip,a4
        STRGE   ip,[a1],#4
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        EORGT   ip,ip,a4
        STRGT   ip,[a1],#4
LABEL(asm_xor_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_xor_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        EOR     v3,v3,a3          /* XOR the four words */
        EOR     v4,v4,v1
        EOR     v5,v5,v2
        EOR     lr,lr,ip
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_xor_loop_up_l2  /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_and_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be ANDed
       exit
               xptr &= yptr for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_and_loop_up)           /* word aligned and loop up */
GLABEL(asm_and_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_and_loop_up_l1  /* yup, so branch */
        CMP     a4,#2           /* AND the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        AND     ip,ip,a4
        STR     ip,[a1],#4
        BLT     asm_and_loop_up_l1  /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        ANDGE   ip,ip,a4
        STRGE   ip,[a1],#4
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        ANDGT   ip,ip,a4
        STRGT   ip,[a1],#4
LABEL(asm_and_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_and_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        AND     v3,v3,a3          /* AND the four words */
        AND     v4,v4,v1
        AND     v5,v5,v2
        AND     lr,lr,ip
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_and_loop_up_l2  /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be XORed
       exit
               xptr = ~(xptr ^ yptr) for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_eqv_loop_up)           /* word aligned eqv loop up */
GLABEL(asm_eqv_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_eqv_loop_up_l1  /* yup, so branch */
        CMP     a4,#2           /* EQV the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        EOR     ip,ip,a4
        MVN     ip,ip
        STR     ip,[a1],#4
        BLT     asm_eqv_loop_up_l1  /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        EORGE   ip,ip,a4
        MVNGE   ip,ip
        STRGE   ip,[a1],#4
        BLE     asm_eqv_loop_up_l1  /* better to branch than skip instrs. */
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        EORGT   ip,ip,a4
        MVNGT   ip,ip
        STRGT   ip,[a1],#4
LABEL(asm_eqv_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_eqv_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        EOR     v3,v3,a3          /* EVQ the four words */
        MVN     v3,v3
        EOR     v4,v4,v1
        MVN     v4,v4
        EOR     v5,v5,v2
        MVN     v5,v5
        EOR     lr,lr,ip
        MVN     lr,lr
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_eqv_loop_up_l2  /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be NANDed
       exit
               xptr = ~(xptr & yptr) for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_nand_loop_up)          /* word aligned nand loop up */
GLABEL(asm_nand_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_nand_loop_up_l1 /* yup, so branch */
        CMP     a4,#2           /* NAND the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        AND     ip,ip,a4
        MVN     ip,ip
        STR     ip,[a1],#4
        BLT     asm_nand_loop_up_l1 /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        ANDGE   ip,ip,a4
        MVNGE   ip,ip
        STRGE   ip,[a1],#4
        BLE     asm_nand_loop_up_l1 /* better to branch than skip instrs. */
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        ANDGT   ip,ip,a4
        MVNGT   ip,ip
        STRGT   ip,[a1],#4
LABEL(asm_nand_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_nand_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        AND     v3,v3,a3          /* NAND the four words */
        MVN     v3,v3
        AND     v4,v4,v1
        MVN     v4,v4
        AND     v5,v5,v2
        MVN     v5,v5
        AND     lr,lr,ip
        MVN     lr,lr
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_nand_loop_up_l2 /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be NORed
       exit
               xptr = ~(xptr | yptr) for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_nor_loop_up)           /* word aligned nor loop up */
GLABEL(asm_nor_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_nor_loop_up_l1  /* yup, so branch */
        CMP     a4,#2           /* NOR the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        ORR     ip,ip,a4
        MVN     ip,ip
        STR     ip,[a1],#4
        BLT     asm_nor_loop_up_l1  /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        ORRGE   ip,ip,a4
        MVNGE   ip,ip
        STRGE   ip,[a1],#4
        BLE     asm_nor_loop_up_l1  /* better to branch than skip instrs. */
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        ORRGT   ip,ip,a4
        MVNGT   ip,ip
        STRGT   ip,[a1],#4
LABEL(asm_nor_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_nor_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        ORR     v3,v3,a3          /* NOR the four words */
        MVN     v3,v3
        ORR     v4,v4,v1
        MVN     v4,v4
        ORR     v5,v5,v2
        MVN     v5,v5
        ORR     lr,lr,ip
        MVN     lr,lr
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_nor_loop_up_l2  /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be ANDC2ed
       exit
               xptr = xptr & ~yptr for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_andc2_loop_up)         /* word aligned andc2 loop up */
GLABEL(asm_andc2_loop_up)
        ANDS    a4,a3,#3         /* multiple of 4 words ? */
        BEQ     asm_andc2_loop_up_l1 /* yup, so branch */
        CMP     a4,#2            /* ANDC2 the first 1-3 words */
        LDR     a4,[a2],#4       /* to align the total to a multiple */
        LDR     ip,[a1]          /* of 4 words */
        BIC     ip,ip,a4
        STR     ip,[a1],#4
        BLT     asm_andc2_loop_up_l1 /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        BICGE   ip,ip,a4
        STRGE   ip,[a1],#4
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        BICGT   ip,ip,a4
        STRGT   ip,[a1],#4
LABEL(asm_andc2_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_andc2_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        BIC     v3,v3,a3          /* ANDC2 the four words */
        BIC     v4,v4,v1
        BIC     v5,v5,v2
        BIC     lr,lr,ip
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_andc2_loop_up_l2 /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^  /* restore work regs and return */

/* extern void asm_orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be XORed
       exit
               xptr = xptr | ~yptr for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_orc2_loop_up)          /* word aligned orc2 loop up */
GLABEL(asm_orc2_loop_up)
        ANDS    a4,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_orc2_loop_up_l1 /* yup, so branch */
        CMP     a4,#2           /* ORC2 the first 1-3 words */
        LDR     a4,[a2],#4      /* to align the total to a multiple */
        LDR     ip,[a1]         /* of 4 words */
        MVN     a4,a4
        ORR     ip,ip,a4
        STR     ip,[a1],#4
        BLT     asm_orc2_loop_up_l1 /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1]
        MVNGE   a4,a4
        ORRGE   ip,ip,a4
        STRGE   ip,[a1],#4
        BLE     asm_orc2_loop_up_l1 /* better to branch than skip instrs. */
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1]
        MVNGT   a4,a4
        ORRGT   ip,ip,a4
        STRGT   ip,[a1],#4
LABEL(asm_orc2_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_orc2_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   a1,{v3,v4,v5,lr}  /* load target words */
        MVN     a3,a3             /* ORC2 the four words */
        ORR     v3,v3,a3
        MVN     v1,v1
        ORR     v4,v4,v1
        MVN     v2,v2
        ORR     v5,v5,v2
        MVN     ip,ip
        ORR     lr,lr,ip
        STMIA   a1!,{v3,v4,v5,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_orc2_loop_up_l2 /* if count still positive then loop */
        LDMFD   sp!,{v1-v5,pc}^ /* restore work regs and return */

/* extern void asm_not_loop_up (uintD* xptr, uintC count);
       entry
               a1 = xptr
               a2 = count of words to be NOTed
       exit
               xptr = ~xptr for count words
               a1 - a4, ip destroyed */
        EXPORT(asm_not_loop_up)           /* word aligned not loop up */
GLABEL(asm_not_loop_up)
        ANDS    a3,a2,#3        /* multiple of 4 words ? */
        BEQ     asm_not_loop_up_l1  /* yup, so branch */
        CMP     a3,#2           /* NOT the first 1-3 words */
        LDR     a3,[a1]         /* to align the total to a multiple */
        MVN     a3,a3           /* of 4 words */
        STR     a3,[a1],#4
        BLT     asm_not_loop_up_l1  /* better to branch than skip instrs. */
        LDRGE   a3,[a1]
        MVNGE   a3,a3
        STRGE   a3,[a1],#4
        LDRGT   a3,[a1]
        MVNGT   a3,a3
        STRGT   a3,[a1],#4
LABEL(asm_not_loop_up_l1)
        BICS    a4,a2,#3        /* set counter to multiple of 4 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{lr}        /* save work regs */
LABEL(asm_not_loop_up_l2)
        LDMIA   a1,{a2,a3,ip,lr} /* load 4 words in one go,NO writeback */
        MVN     a2,a2            /* NOT the four words */
        MVN     a3,a3
        MVN     ip,ip
        MVN     lr,lr
        STMIA   a1!,{a2,a3,ip,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_not_loop_up_l2  /* if count still positive then loop */
        LDMFD   sp!,{pc}^       /* restore work regs and return */

/* extern void asm_and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be AND_TESTed
       exit
               a1 = true if any words ANDed together are non-zero else false
               a2 - a4, ip destroyed */
        EXPORT(asm_and_test_loop_up) /* word aligned and_test loop up */
GLABEL(asm_and_test_loop_up)
        ANDS    a4,a3,#3            /* multiple of 4 words ? */
        BEQ     asm_and_test_loop_up_l1 /* yup, so branch */
        CMP     a4,#2
        LDR     a4,[a2],#4      /* AND_TEST the first 1-3 words */
        LDR     ip,[a1],#4      /* to align the total to a multiple */
        TST     ip,a4           /* of 4 words */
        MOVNE   a1,#1           /* return true if AND_TEST ok */
        MOVNES  pc,lr
        BCC     asm_and_test_loop_up_l1 /* better to branch than skip instrs. */
        LDRGE   a4,[a2],#4
        LDRGE   ip,[a1],#4
        TSTGE   ip,a4
        MOVNE   a1,#1
        MOVNES  pc,lr
        ANDS    a4,a3,#3
        CMP     a4,#2
        BLE     asm_and_test_loop_up_l1 /* better to branch than skip instrs. */
        LDRGT   a4,[a2],#4
        LDRGT   ip,[a1],#4
        TSTGT   ip,a4
        MOVNE   a1,#1
        MOVNES  pc,lr
LABEL(asm_and_test_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,#0           /* return false */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v6,lr}  /* save work regs */
        MOV     v6,a1           /* move xptr to v6 */
        MOV     a1,#1           /* set result to true */
LABEL(asm_and_test_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   v6!,{v3,v4,v5,lr} /* load target words */
        TST     v3,a3             /* AND_TEST the four words */
        TSTEQ   v4,v1
        TSTEQ   v5,v2
        TSTEQ   lr,ip
        LDMNEFD sp!,{v1-v6,pc}^
        SUBS    a4,a4,#4            /* decrement counter by 4 */
        BGT     asm_and_test_loop_up_l2 /* if count still positive then loop */
        MOV     a1,#0
        LDMFD   sp!,{v1-v6,pc}^ /* restore work regs and return */

/* extern void asm_test_loop_up (uintD* xptr, uintC count);
       entry
               a1 = xptr
               a2 = count of words to be TESTed
       exit
               a1 = true if any words are non-zero else false
               a2 - a4, ip destroyed */
        EXPORT(asm_test_loop_up)          /* word aligned test loop up */
GLABEL(asm_test_loop_up)
        MOV     ip,a1           /* move xptr to ip */
        MOV     a1,#1           /* set result to true */
        ANDS    a3,a2,#3        /* multiple of 4 words ? */
        BEQ     asm_test_loop_up_l1 /* yup, so branch */
        LDR     a4,[ip],#4      /* TEST the first 1-3 words */
        TEQ     a4,#0           /* align the total to a multiple of 4 */
        MOVNES  pc,lr           /* return true if AND_TEST ok */
        CMP     a3,#2
        BLT     asm_test_loop_up_l1 /* need to branch 'cos PSR set */
        LDRGE   a4,[ip],#4      /* when checking against zero */
        TEQGE   a4,#0
        MOVNES  pc,lr
        CMP     a3,#2
        BLE     asm_test_loop_up_l1 /* need to branch 'cos PSR set */
        LDRGT   a4,[ip],#4      /* when checking against zero */
        TEQGT   a4,#0
        MOVNES  pc,lr
LABEL(asm_test_loop_up_l1)
        BICS    a4,a2,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,#0           /* return false */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1,lr}     /* save work regs */
LABEL(asm_test_loop_up_l2)
        LDMIA   ip!,{a2,a3,v1,lr} /* load 4 words in one go */
        TEQ     a2,#0             /* TEST the four words */
        TEQEQ   a3,#0
        TEQEQ   v1,#0
        TEQEQ   lr,#0
        LDMNEFD sp!,{v1,pc}^
        SUBS    a4,a4,#4        /* decrement counter by 4 */
        BGT     asm_test_loop_up_l2 /* if count still positive then loop */
        MOV     a1,#0
        LDMFD   sp!,{v1,pc}^    /* restore work regs and return */

/* extern void asm_compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
       entry
               a1 = xptr
               a2 = yptr
               a3 = count of words to be COMPAREd
       exit
               a1 = +1 if first non-equal word in xptr[] and yptr[]
                       xptr[i] > yptr[i]
                    -1 if xptr[i] < yptr[i]
                     0 otherwise
               a2 - a4, ip destroyed */
        EXPORT(asm_compare_loop_up)       /* word aligned compare loop up */
GLABEL(asm_compare_loop_up)
        ANDS    a4,a3,#3           /* multiple of 4 words ? */
        BEQ     asm_compare_loop_up_l1 /* yup, so branch */
        LDR     a4,[a2],#4         /* COMPARE the first 1-3 words */
        LDR     ip,[a1],#4      /* to align the total to a multiple */
        CMP     ip,a4           /* of 4 words */
        MVNLO   a1,#0           /* x < y -> -1 */
        MOVHI   a1,#1           /* x > y -> +1 */
        MOVNES  pc,lr           /* and return result if not equal */
        ANDS    a4,a3,#3
        CMP     a4,#2
        BLT     asm_compare_loop_up_l1 /* need to branch 'cos PSR used */
        LDR     a4,[a2],#4
        LDR     ip,[a1],#4
        CMP     ip,a4
        MVNLO   a1,#0
        MOVHI   a1,#1
        MOVNES  pc,lr
        ANDS    a4,a3,#3
        CMP     a4,#2
        BLE     asm_compare_loop_up_l1 /* need to branch 'cos PSR used */
        LDR     a4,[a2],#4
        LDR     ip,[a1],#4
        CMP     ip,a4
        MVNLO   a1,#0
        MOVHI   a1,#1
        MOVNES  pc,lr
LABEL(asm_compare_loop_up_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,#0           /* xptr[] == yptr[] -> 0 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v6,lr}  /* save work regs */
        MOV     v6,a1           /* move xptr to v6 */
        MOV     a1,#1           /* set result to +1 */
LABEL(asm_compare_loop_up_l2)
        LDMIA   a2!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMIA   v6!,{v3,v4,v5,lr} /* load test words */
        CMP     v3,a3             /* COMPARE the four words */
        CMPEQ   v4,v1
        CMPEQ   v5,v2
        CMPEQ   lr,ip
        MVNLO   a1,#0           /* x < y -> -1 (a1 already holds +1) */
        LDMNEFD sp!,{v1-v6,pc}^
        SUBS    a4,a4,#4           /* decrement counter by 4 */
        BGT     asm_compare_loop_up_l2 /* if count still positive then loop */
        MOV     a1,#0
        LDMFD   sp!,{v1-v6,pc}^ /* restore work regs and return */

/* extern uintD asm_addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
       entry
               a1 = sourceptr
               a2 = destptr
               a3 = count of words to be added
       exit
               destptr[] = sourceptr[] + destptr[]
               a1 = last carry
               a2 - a4, ip destroyed */
        EXPORT(asm_addto_loop_down)       /* word aligned addto loop down */
GLABEL(asm_addto_loop_down)
                MOV     a4,a3   /* set regs for a call */
                MOV     a3,a2   /* to asm_add_loop_down */
                                        /* and drop into asm_add_loop_down */

/* extern uintD asm_add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
       entry
               a1 = sourceptr1
               a2 = sourceptr2
               a3 = destptr
               a4 = count of words to be added
       exit
               destptr[] = sourceptr1[] + sourceptr2[]
               a1 = last carry
               a2 - a4, ip destroyed */
        EXPORT(asm_add_loop_down)         /* word aligned add loop down */
GLABEL(asm_add_loop_down)
        ANDS    ip,a4,#3         /* multiple of 4 words ? */
        BEQ     asm_add_loop_down_l1 /* yup, so branch */
        STMFD   sp!,{v6,lr}
        LDR     v6,[a2,#-4]!    /* add the first 1-3 words */
        LDR     lr,[a1,#-4]!    /* to align the total to a multiple */
        ADDS    lr,lr,v6        /* of 4 words */
        STR     lr,[a3,#-4]!
        TEQ     ip,#1
        BEQ     asm_add_loop_down_l0 /* need to branch 'cos PSR used */
        LDR     v6,[a2,#-4]!
        LDR     lr,[a1,#-4]!
        ADCS    lr,lr,v6
        STR     lr,[a3,#-4]!
        TEQ     ip,#2
        BEQ     asm_add_loop_down_l0 /* need to branch 'cos PSR used */
        LDR     v6,[a2,#-4]!
        LDR     lr,[a1,#-4]!
        ADCS    lr,lr,v6
        STR     lr,[a3,#-4]!
LABEL(asm_add_loop_down_l0)          /* at least one add has happened */
        BICS    a4,a4,#3         /* set counter to multiple of 4 */
        BNE     asm_add_loop_down_l3 /* branch if more adds to do */
        ADCEQ   a1,a4,a4         /* set result to Carry (a4 is 0) */
        LDMEQFD sp!,{v6,pc}^     /* and return */
LABEL(asm_add_loop_down_l1)
        BICS    a4,a4,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,#0           /* no adds, so C = 0 */
        MOVEQS  pc,lr           /* if zero then we're done */
        CMN     a4,#0           /* clear carry bit */
        STMFD   sp!,{v6,lr}
LABEL(asm_add_loop_down_l3)
        STMFD   sp!,{v1-v5}     /* save work regs */
LABEL(asm_add_loop_down_l2)
        LDMDB   a2!,{v1,v2,v3,ip} /* load 4 words in one go */
        LDMDB   a1!,{v4,v5,v6,lr} /* and from source2 */
        ADCS    lr,lr,ip          /* add the four words with carry */
        ADCS    v6,v6,v3
        ADCS    v5,v5,v2
        ADCS    v4,v4,v1
        STMDB   a3!,{v4,v5,v6,lr} /* store 4 results */
        SUB     a4,a4,#4        /* decrement counter by 4, preserve C */
        TEQ     a4,#0           /* are we done ? */
        BNE     asm_add_loop_down_l2 /* if count non-zero then loop */
        ADC     a1,a4,a4         /* set result to Carry (a4 is 0) */
        LDMFD   sp!,{v1-v6,pc}^  /* restore work regs and return */

/* extern uintD asm_inc_loop_down (uintD* ptr, uintC count);
       entry
               a1 = ptr
               a2 = count of words to be INCed
       exit
               a1 = 0 if any words are non-zero after increment else 1
                      stop incrementing when first word becomes non-zero
               a2 - a4, ip destroyed */
        EXPORT(asm_inc_loop_down)         /* word aligned inc loop down */
GLABEL(asm_inc_loop_down)
        ANDS    a3,a2,#1         /* multiple of 2 words ? */
        BEQ     asm_inc_loop_down_l1 /* yup, so branch */
        LDR     a4,[a1,#-4]!     /* INC the first word */
        ADDS    a4,a4,#1        /* align the total to a multiple of 2 */
        STR     a4,[a1]
        MOVNE   a1,#0           /* set result to 0 */
        MOVNES  pc,lr           /* return 0 if non-zero result */
LABEL(asm_inc_loop_down_l1)
        BICS    a4,a2,#1        /* set counter to multiple of 2 */
        MOVEQ   a1,#1           /* return 1 */
        MOVEQS  pc,lr           /* if zero then we're done */
        MOV     ip,a1           /* move ptr to ip */
        MOV     a1,#0           /* set result to 0 */
        ANDS    a3,a4,#3
        BEQ     asm_inc_loop_down_l3
        LDMDB   ip,{a2,a3}      /* load 2 words in one go */
        ADDS    a3,a3,#1        /* INC the two words */
        ADDEQS  a2,a2,#1        /* stopping when first word non-zero */
        STMDB   ip!,{a2,a3}     /* store 2 results */
        MOVNES  pc,lr           /* return 0 if any result non-zero */
        SUBS    a4,a4,#2        /* decrement counter by 2 */
        MOVEQ   a1,#1           /* if finished loop then */
        MOVEQS  pc,lr           /* return 1 */
LABEL(asm_inc_loop_down_l3)         /* now a multiple of 4 words */
        STMFD   sp!,{v1,lr}     /* save work regs */
LABEL(asm_inc_loop_down_l2)
        LDMDB   ip,{a2,a3,v1,lr} /* load 4 words in one go */
        ADDS    lr,lr,#1         /* INC the four words */
        ADDEQS  v1,v1,#1         /* stopping when first word non-zero */
        ADDEQS  a3,a3,#1
        ADDEQS  a2,a2,#1
        STMDB   ip!,{a2,a3,v1,lr} /* store 4 results */
        LDMNEFD sp!,{v1,pc}^      /* return 0 if any result non-zero */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_inc_loop_down_l2 /* if count still positive then loop */
        MOV     a1,#1
        LDMFD   sp!,{v1,pc}^    /* restore work regs and return 1 */

/* extern uintD asm_sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
       entry
               a1 = sourceptr1
               a2 = sourceptr2
               a3 = destptr
               a4 = count of words to be subtracted
       exit
               destptr[] = sourceptr1[] -  sourceptr2[]
               a1 = last carry
               a2 - a4, ip destroyed */
        EXPORT(asm_sub_loop_down)         /* word aligned sub loop down */
GLABEL(asm_sub_loop_down)
        ANDS    ip,a4,#3         /* multiple of 4 words ? */
        BEQ     asm_sub_loop_down_l1 /* yup, so branch */
        STMFD   sp!,{v6,lr}
        LDR     v6,[a2,#-4]!    /* subtract the first 1-3 words */
        LDR     lr,[a1,#-4]!    /* to align the total to a multiple */
        SUBS    lr,lr,v6        /* of 4 words */
        STR     lr,[a3,#-4]!
        TEQ     ip,#1
        BNE     asm_sub_loop_down_l0 /* branch if more than one subtract */
LABEL(asm_sub_loop_down_l4)     /* drop through for better instr. timings */
        BICS    a4,a4,#3    /* set counter to multiple of 4 */
        SBCEQ   a1,a4,a4    /* set result to Carry (a4 is 0) */
        LDMEQFD sp!,{v6,pc}^     /* and return */
        STMFD   sp!,{v1-v5}      /* save work regs */
        B       asm_sub_loop_down_l2 /* branch if more subtracts to do */
LABEL(asm_sub_loop_down_l0)
        LDR     v6,[a2,#-4]!
        LDR     lr,[a1,#-4]!
        SBCS    lr,lr,v6
        STR     lr,[a3,#-4]!
        TEQ     ip,#2
        BEQ     asm_sub_loop_down_l4 /* need to branch 'cos PSR used */
        LDR     v6,[a2,#-4]!
        LDR     lr,[a1,#-4]!
        SBCS    lr,lr,v6
        STR     lr,[a3,#-4]!
        B       asm_sub_loop_down_l4
LABEL(asm_sub_loop_down_l1)
        BICS    a4,a4,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,#0           /* no subtracts, so C = 0 */
        MOVEQS  pc,lr           /* if zero then we're done */
        CMP     a4,#0           /* set carry bit, since a4 > 0 */
        STMFD   sp!,{v1-v6,lr}  /* save work regs */
LABEL(asm_sub_loop_down_l2)
        LDMDB   a2!,{v1,v2,v3,ip} /* load 4 words in one go */
        LDMDB   a1!,{v4,v5,v6,lr} /* and from source2 */
        SBCS    lr,lr,ip        /* subtract the four words with carry */
        SBCS    v6,v6,v3
        SBCS    v5,v5,v2
        SBCS    v4,v4,v1
        STMDB   a3!,{v4,v5,v6,lr} /* store 4 results */
        SUB     a4,a4,#4        /* decrement counter by 4, preserve C */
        TEQ     a4,#0           /* are we done ? */
        BNE     asm_sub_loop_down_l2 /* if count non-zero then loop */
        SBC     a1,a4,a4         /* set result to Carry (a4 is 0) */
        LDMFD   sp!,{v1-v6,pc}^  /* restore work regs and return */

/* extern uintD asm_subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
       entry
               a1 = sourceptr1
               a2 = sourceptr2
               a3 = destptr
               a4 = count of words to be subtracted
               [sp] = carry
       exit
               destptr[] = sourceptr1[] -  sourceptr2[]
               a1 = last carry
               a2 - a4, ip destroyed */
        EXPORT(asm_subx_loop_down)        /* word aligned xsub loop down */
GLABEL(asm_subx_loop_down)
        LDR     ip,[sp]         /* get starting value of carry */
LABEL(asm_subx_loop_down_lsub)
        RSBS    ip,ip,#0          /* set carry in PSR */
        ANDS    ip,a4,#3          /* multiple of 4 words ? */
        BEQ     asm_subx_loop_down_l1 /* yup, so branch */
        STMFD   sp!,{v6,lr}
        LDR     v6,[a2,#-4]!    /* subtract the first 1-3 words */
        LDR     lr,[a1,#-4]!    /* to align the total to a multiple */
        SBCS    lr,lr,v6        /* of 4 words */
        STR     lr,[a3,#-4]!
        TEQ     ip,#1
        BNE     asm_subx_loop_down_l0 /* branch if more than one subtract */
LABEL(asm_subx_loop_down_l4)    /* drop through for better instr. timings */
        BICS    a4,a4,#3    /* set counter to multiple of 4 */
        SBCEQ   a1,a4,a4    /* set result to Carry (a4 is 0) */
        LDMEQFD sp!,{v6,pc}^      /* and return */
        STMFD   sp!,{v1-v5}       /* save work regs */
        B       asm_subx_loop_down_l2 /* branch if more subtracts to do */
LABEL(asm_subx_loop_down_l0)
        LDR     v6,[a2,#-4]!
        LDR     lr,[a1,#-4]!
        SBCS    lr,lr,v6
        STR     lr,[a3,#-4]!
        TEQ     ip,#2
        BEQ     asm_subx_loop_down_l4 /* need to branch 'cos PSR used */
        LDR     v6,[a2,#-4]!
        LDR     lr,[a1,#-4]!
        SBCS    lr,lr,v6
        STR     lr,[a3,#-4]!
        B       asm_subx_loop_down_l4
LABEL(asm_subx_loop_down_l1)
        BICS    a4,a4,#3        /* set counter to multiple of 4 */
        SBCEQ   a1,a4,a4        /* set result to Carry (a4 is 0) */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{v1-v6,lr}  /* save work regs */
LABEL(asm_subx_loop_down_l2)
        LDMDB   a2!,{v1,v2,v3,ip} /* load 4 words in one go */
        LDMDB   a1!,{v4,v5,v6,lr} /* and from source2 */
        SBCS    lr,lr,ip        /* subtract the four words with carry */
        SBCS    v6,v6,v3
        SBCS    v5,v5,v2
        SBCS    v4,v4,v1
        STMDB   a3!,{v4,v5,v6,lr} /* store 4 results */
        SUB     a4,a4,#4        /* decrement counter by 4, preserve C */
        TEQ     a4,#0           /* are we done ? */
        BNE     asm_subx_loop_down_l2 /* if count non-zero then loop */
        SBC     a1,a4,a4          /* set result to Carry (a4 is 0) */
        LDMFD   sp!,{v1-v6,pc}^   /* restore work regs and return */

/* extern uintD asm_subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
       entry
               a1 = sourceptr
               a2 = destptr
               a3 = count of words to be subtracted
       exit
               destptr[] = destptr[] - sourceptr[]
               a1 = last carry
               a2 - a4, ip destroyed */
        EXPORT(asm_subfrom_loop_down) /* word aligned subfrom loop down */
GLABEL(asm_subfrom_loop_down)
        ANDS    ip,a3,#3             /* multiple of 4 words ? */
        BEQ     asm_subfrom_loop_down_l1 /* yup, so branch */
        STMFD   sp!,{lr}
        LDR     a4,[a1,#-4]!    /* subtract the first 1-3 words */
        LDR     lr,[a2,#-4]!    /* to align the total to a multiple */
        SUBS    lr,lr,a4        /* of 4 words */
        STR     lr,[a2]
        TEQ     ip,#1
        BNE     asm_subfrom_loop_down_l0 /* branch if more than one subtract */
LABEL(asm_subfrom_loop_down_l4) /* drop through for better instr. timings */
        BICS    a4,a3,#3    /* set counter to multiple of 4 */
        SBCEQ   a1,a4,a4    /* set result to Carry (a4 is 0) */
        LDMEQFD sp!,{pc}^   /* and return */
        STMFD   sp!,{v1-v5} /* save work regs */
        B       asm_subfrom_loop_down_l2 /* branch if more subtracts to do */
LABEL(asm_subfrom_loop_down_l0)
        LDR     a4,[a1,#-4]!
        LDR     lr,[a2,#-4]!
        SBCS    lr,lr,a4
        STR     lr,[a2]
        TEQ     ip,#2
        BEQ     asm_subfrom_loop_down_l4 /* need to branch 'cos PSR used */
        LDR     a4,[a1,#-4]!
        LDR     lr,[a2,#-4]!
        SBCS    lr,lr,a4
        STR     lr,[a2]
        B       asm_subfrom_loop_down_l4
LABEL(asm_subfrom_loop_down_l1)
        BICS    a4,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,#0           /* no subtracts, so C = 0 */
        MOVEQS  pc,lr           /* if zero then we're done */
        CMP     a4,#0           /* set carry bit, since a4 > 0 */
        STMFD   sp!,{v1-v5,lr}  /* save work regs */
LABEL(asm_subfrom_loop_down_l2)
        LDMDB   a1!,{a3,v1,v2,ip} /* load 4 words in one go */
        LDMDB   a2,{v3,v4,v5,lr}  /* and from destptr */
        SBCS    lr,lr,ip        /* subtract the four words with carry */
        SBCS    v5,v5,v2
        SBCS    v4,v4,v1
        SBCS    v3,v3,a3
        STMDB   a2!,{v3,v4,v5,lr} /* store 4 results */
        SUB     a4,a4,#4        /* decrement counter by 4, preserve C */
        TEQ     a4,#0           /* are we done ? */
        BNE     asm_subfrom_loop_down_l2 /* if count non-zero then loop */
        SBC     a1,a4,a4             /* set result to Carry (a4 is 0) */
        LDMFD   sp!,{v1-v5,pc}^      /* restore work regs and return */

/* extern uintD asm_dec_loop_down (uintD* ptr, uintC count);
       entry
               a1 = ptr
               a2 = count of words to be DECed
       exit
               a1 = 0 if any words are non-zero before decrement else -1
                      stop decrementing when first word is non-zero
               a2 - a4, ip destroyed */
        EXPORT(asm_dec_loop_down)         /* word aligned dec loop down */
GLABEL(asm_dec_loop_down)
        ANDS    a3,a2,#1         /* multiple of 2 words ? */
        BEQ     asm_dec_loop_down_l1 /* yup, so branch */
        LDR     a4,[a1,#-4]!     /* DEC the first word */
        SUBS    a4,a4,#1        /* align the total to a multiple of 2 */
        STR     a4,[a1]
        MOVCS   a1,#0           /* set result to 0 */
        MOVCSS  pc,lr           /* return 0 if non-zero result */
LABEL(asm_dec_loop_down_l1)
        BICS    a4,a2,#1        /* set counter to multiple of 2 */
        MVNEQ   a1,#0           /* return -1 */
        MOVEQS  pc,lr           /* if zero then we're done */
        MOV     ip,a1           /* move ptr to ip */
        MOV     a1,#0           /* set result to 0 */
        ANDS    a3,a4,#3
        BEQ     asm_dec_loop_down_l3
        LDMDB   ip,{a2,a3}      /* load 2 words in one go */
        SUBS    a3,a3,#1        /* DEC the two words */
        SUBCCS  a2,a2,#1        /* stopping when first word non-zero */
        STMDB   ip!,{a2,a3}     /* store 2 results */
        MOVCSS  pc,lr           /* return 0 if any result non-zero */
        SUBS    a4,a4,#2        /* decrement counter by 2 */
        MVNEQ   a1,#0           /* if finished loop then */
        MOVEQS  pc,lr           /* return -1 */
LABEL(asm_dec_loop_down_l3)         /* now a multiple of 4 words */
        STMFD   sp!,{v1,lr}     /* save work regs */
LABEL(asm_dec_loop_down_l2)
        LDMDB   ip,{a2,a3,v1,lr} /* load 4 words in one go */
        SUBS    lr,lr,#1         /* DEC the four words */
        SUBCCS  v1,v1,#1         /* stopping when first word non-zero */
        SUBCCS  a3,a3,#1
        SUBCCS  a2,a2,#1
        STMDB   ip!,{a2,a3,v1,lr} /* store 4 results */
        LDMCSFD sp!,{v1,pc}^      /* return 0 if any carry */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_dec_loop_down_l2 /* if count still positive then loop */
        MVN     a1,#0
        LDMFD   sp!,{v1,pc}^    /* restore work regs and return -1 */

/* extern void asm_neg_loop_down (uintD* ptr, uintC count);
       entry
               a1 = ptr
               a2 = count of words. The long integer is to be NEGated
       exit
               ptr[] = -ptr[] for count words
               a1 = last carry
               a2 - a4, ip destroyed */
        EXPORT(asm_neg_loop_down)         /* word aligned neg loop down */
GLABEL(asm_neg_loop_down)
        CMPS    a2,#0           /* count = 0 ? */
        MOVEQ   a1,#0           /* yup, so return 0 */
        MOVEQS  pc,lr
LABEL(asm_neg_loop_down_l1)          /* skip all the zero words first */
        LDR     a3,[a1,#-4]!     /* compare words against zero */
        CMPS    a3,#0            /* downwards in memory */
        BNE     asm_neg_loop_down_l2 /* non-zero, so negate rest of words */
        SUBS    a2,a2,#1         /* reduce count of words */
        BNE     asm_neg_loop_down_l1 /* more ?, so loop */
        MOV     a1,#0            /* return 0 */
        MOVS    pc,lr
LABEL(asm_neg_loop_down_l2)
        RSB     a3,a3,#0        /* first non-zero word = -word */
        STR     a3,[a1]
        SUBS    a2,a2,#1
        MVNEQ   a1,#0           /* done ? -> return -1 */
        MOVEQS  pc,lr
                                        /* now NOT rest of the words */
        ANDS    a3,a2,#3                /* multiple of 4 words ? */
        BEQ     asm_neg_loop_down_l3        /* yup, so branch */
        CMP     a3,#2                   /* NOT the first 1-3 words */
        LDR     a3,[a1,#-4]!    /* to align the total to a multiple */
        MVN     a3,a3           /* of 4 words */
        STR     a3,[a1]
        BLT     asm_neg_loop_down_l3 /* better to branch than skip instrs. */
        LDRGE   a3,[a1,#-4]!
        MVNGE   a3,a3
        STRGE   a3,[a1]
        LDRGT   a3,[a1,#-4]!
        MVNGT   a3,a3
        STRGT   a3,[a1]
LABEL(asm_neg_loop_down_l3)
        BICS    a4,a2,#3        /* set counter to multiple of 4 */
        MVNEQ   a1,#0           /* set result to -1 */
        MOVEQS  pc,lr           /* if zero then we're done */
        STMFD   sp!,{lr}        /* save work regs */
LABEL(asm_neg_loop_down_l4)
        LDMDB   a1,{a2,a3,ip,lr} /* load 4 words in one go,NO writeback */
        MVN     a2,a2            /* NOT the four words */
        MVN     a3,a3
        MVN     ip,ip
        MVN     lr,lr
        STMDB   a1!,{a2,a3,ip,lr} /* store 4 results */
        SUBS    a4,a4,#4          /* decrement counter by 4 */
        BGT     asm_neg_loop_down_l4 /* if count still positive then loop */
        MVN     a1,#0            /* set result to -1 */
        LDMFD   sp!,{pc}^        /* restore work regs and return -1 */

/* extern uintD asm_shift1left_loop_down (uintD* ptr, uintC count);
       entry
               a1 = ptr
               a2 = count of words to be shifted left
       exit
               a1 = carry out from last shift left
               a2 - a4, ip destroyed */
        EXPORT(asm_shift1left_loop_down) /* word aligned shift1left loop down */
GLABEL(asm_shift1left_loop_down)
        CMN     a1,#0           /* clear carry bit, since a1 > 0 */
        ANDS    a3,a2,#1        /* multiple of 2 words ? */
        BEQ     asm_shift1left_loop_down_l1 /* yup, so branch */
        LDR     a4,[a1,#-4]!            /* shift left the first word */
        ADDS    a4,a4,a4
        STR     a4,[a1]
LABEL(asm_shift1left_loop_down_l1)
        BICS    a4,a2,#1        /* set counter to multiple of 2 */
        ADCEQ   a1,a4,a4        /* if zero set result to C (a4 is 0) */
        MOVEQS  pc,lr           /* and return */
        ANDS    a3,a4,#3        /* multiple of 4 words ? */
        BEQ     asm_shift1left_loop_down_l3 /* yup, so branch */
        LDMDB   a1,{a2,a3}              /* load 2 words in one go */
        ADCS    a3,a3,a3                /* shift left the two words */
        ADCS    a2,a2,a2
        STMDB   a1!,{a2,a3}     /* store 2 results */
        BICS    a4,a4,#2        /* decrement counter by 2 */
        ADCEQ   a1,a4,a4        /* set result to Carry (a4 is 0) */
        MOVEQS  pc,lr           /* and return */
LABEL(asm_shift1left_loop_down_l3)  /* now a multiple of 4 words */
        STMFD   sp!,{lr}        /* save work regs */
LABEL(asm_shift1left_loop_down_l2)
        LDMDB   a1,{a2,a3,ip,lr} /* load 4 words in one go */
        ADCS    lr,lr,lr         /* shift left the four words */
        ADCS    ip,ip,ip
        ADCS    a3,a3,a3
        ADCS    a2,a2,a2
        STMDB   a1!,{a2,a3,ip,lr}       /* store 4 results */
        SUB     a4,a4,#4                /* decrement counter by 4 */
        TEQ     a4,#0                   /* are we done ? */
        BNE     asm_shift1left_loop_down_l2 /* if count non-zero then loop */
        ADC     a1,a4,a4        /* set result to Carry (a4 is 0) */
        LDMFD   sp!,{pc}^       /* restore work regs and return 1 */

/* extern uintD asm_shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
       entry
               a1 = ptr
               a2 = count of words to be shifted left
               a3 = size of left shift
               a4 = value to ORR in for first shift
       exit
               a1 = shift out from last shift left
               a2 - a4, ip destroyed */
        EXPORT(asm_shiftleft_loop_down) /* word aligned shiftleft loop down */
GLABEL(asm_shiftleft_loop_down)
        STMFD   sp!,{v6,lr}
        RSB     v6,a3,#32       /* size of complementary right shift */
        ANDS    ip,a2,#3        /* multiple of 4 words ? */
        BEQ     asm_shiftleft_loop_down_l1 /* yup, so branch */
        LDR     lr,[a1,#-4]!    /* shiftleft the first 1-3 words */
        ORR     a4,a4,lr,ASL a3 /* to align the total to a multiple */
        STR     a4,[a1,#0]      /* of 4 words */
        MOV     a4,lr,LSR v6
        CMP     ip,#2
        BLT     asm_shiftleft_loop_down_l1 /* better to branch than skip instrs. */
        LDRGE   lr,[a1,#-4]!
        ORRGE   a4,a4,lr,ASL a3
        STRGE   a4,[a1,#0]
        MOVGE   a4,lr,LSR v6
        LDRGT   lr,[a1,#-4]!
        ORRGT   a4,a4,lr,ASL a3
        STRGT   a4,[a1,#0]
        MOVGT   a4,lr,LSR v6
LABEL(asm_shiftleft_loop_down_l1)
        BICS    ip,a2,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,a4           /* if zero then we're done */
        LDMEQFD sp!,{v6,pc}^    /* so return last shift out */
        STMFD   sp!,{v1-v3}     /* save work regs */
LABEL(asm_shiftleft_loop_down_l2)
        LDMDB   a1,{a2,v1,v2,v3} /* load 4 words in one go */
        ORR     lr,a4,v3,ASL a3  /* shiftleft the four words */
        MOV     a4,v3,LSR v6     /* keep carry in a4 */
        ORR     v3,a4,v2,ASL a3  /* and store results up a register */
        MOV     a4,v2,LSR v6     /* to regs v1-v3,lr */
        ORR     v2,a4,v1,ASL a3
        MOV     a4,v1,LSR v6
        ORR     v1,a4,a2,ASL a3
        MOV     a4,a2,LSR v6
        STMDB   a1!,{v1,v2,v3,lr}      /* store 4 results */
        SUBS    ip,ip,#4               /* decrement counter by 4 */
        BGT     asm_shiftleft_loop_down_l2 /* if count still positive then loop */
        MOV     a1,a4                  /* result = last shift out */
        LDMFD   sp!,{v1-v3,v6,pc}^ /* restore work regs and return */

/* extern uintD asm_shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
       entry
               a1 = sourceptr
               a2 = destptr
               a3 = count of words to be shifted left
               a4 = size of left shift
       exit
               a1 = shift out from last shift left
               a2 - a4, ip destroyed */
        EXPORT(asm_shiftleftcopy_loop_down) /* word aligned shiftleftcopy loop down */
GLABEL(asm_shiftleftcopy_loop_down)
        STMFD   sp!,{v5,v6,lr}
        MOV     v5,#0           /* initial shift carry */
        RSB     v6,a4,#32       /* size of complementary right shift */
        ANDS    ip,a3,#3        /* multiple of 4 words ? */
        BEQ     asm_shiftleftcopy_loop_down_l1 /* yup, so branch */
        LDR     lr,[a1,#-4]!    /* shiftleft the first 1-3 words */
        ORR     v5,v5,lr,ASL a4 /* to align the total to a multiple */
        STR     v5,[a2,#-4]!    /* of 4 words */
        MOV     v5,lr,LSR v6
        CMP     ip,#2
        BLT     asm_shiftleftcopy_loop_down_l1 /* better to branch than skip instrs. */
        LDRGE   lr,[a1,#-4]!
        ORRGE   v5,v5,lr,ASL a4
        STRGE   v5,[a2,#-4]!
        MOVGE   v5,lr,LSR v6
        LDRGT   lr,[a1,#-4]!
        ORRGT   v5,v5,lr,ASL a4
        STRGT   v5,[a2,#-4]!
        MOVGT   v5,lr,LSR v6
LABEL(asm_shiftleftcopy_loop_down_l1)
        BICS    ip,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,v5           /* if zero then we're done */
        LDMEQFD sp!,{v5,v6,pc}^ /* so return last shift out */
        STMFD   sp!,{v1-v3}     /* save work regs */
LABEL(asm_shiftleftcopy_loop_down_l2)
        LDMDB   a1!,{a3,v1,v2,v3} /* load 4 words in one go */
        ORR     lr,v5,v3,ASL a4   /* shiftleft the four words */
        MOV     v5,v3,LSR v6      /* keep carry in v5 */
        ORR     v3,v5,v2,ASL a4   /* and store results up a register */
        MOV     v5,v2,LSR v6      /* to regs v1-v3,lr */
        ORR     v2,v5,v1,ASL a4
        MOV     v5,v1,LSR v6
        ORR     v1,v5,a3,ASL a4
        MOV     v5,a3,LSR v6
        STMDB   a2!,{v1,v2,v3,lr}          /* store 4 results */
        SUBS    ip,ip,#4                   /* decrement counter by 4 */
        BGT     asm_shiftleftcopy_loop_down_l2 /* if count still positive then loop */
        MOV     a1,v5                      /* result = last shift out */
        LDMFD   sp!,{v1-v3,v5,v6,pc}^ /* restore work regs and return */

/* extern uintD asm_shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
       entry
               a1 = ptr
               a2 = count of words to be shifted right
               a3 = carry
       exit
               a1 = carry out from last shift right
               a2 - a4, ip destroyed */
        EXPORT(asm_shift1right_loop_up) /* word aligned shift1right loop up */
GLABEL(asm_shift1right_loop_up)
        MOVS    a3,a3,LSR #1           /* set carry */
        ANDS    a3,a2,#1               /* multiple of 2 words ? */
        BEQ     asm_shift1right_loop_up_l1 /* yup, so branch */
        LDR     a4,[a1]                /* shift right the first word */
        MOVS    a4,a4,rrx
        STR     a4,[a1],#4
LABEL(asm_shift1right_loop_up_l1)
        BICS    a4,a2,#1        /* set counter to multiple of 2 */
        MOVEQ   a1,a4,rrx       /* if zero set result to C (a4 is 0) */
        MOVEQS  pc,lr           /* and return */
        ANDS    a3,a4,#3        /* multiple of 4 words ? */
        BEQ     asm_shift1right_loop_up_l3 /* yup, so branch */
        LDMIA   a1,{a2,a3}             /* load 2 words in one go */
        MOVS    a2,a2,rrx              /* shift right the two words */
        MOVS    a3,a3,rrx
        STMIA   a1!,{a2,a3}     /* store 2 results */
        BICS    a4,a4,#2        /* decrement counter by 2 */
        ADCEQ   a1,a4,a4        /* set result to Carry (a4 is 0) */
        MOVEQS  pc,lr           /* and return */
LABEL(asm_shift1right_loop_up_l3)   /* now a multiple of 4 words */
        STMFD   sp!,{lr}        /* save work regs */
LABEL(asm_shift1right_loop_up_l2)
        LDMIA   a1,{a2,a3,ip,lr} /* load 4 words in one go */
        MOVS    a2,a2,rrx        /* shift right the four words */
        MOVS    a3,a3,rrx
        MOVS    ip,ip,rrx
        MOVS    lr,lr,rrx
        STMIA   a1!,{a2,a3,ip,lr}      /* store 4 results */
        SUB     a4,a4,#4               /* decrement counter by 4 */
        TEQ     a4,#0                  /* are we done ? */
        BNE     asm_shift1right_loop_up_l2 /* if count non-zero then loop */
        MOV     a1,a4,rrx       /* set result to Carry (a4 is 0) */
        LDMFD   sp!,{pc}^       /* restore work regs and return 1 */

/* extern uintD asm_shiftright_loop_up (uintD* ptr, uintC count, uintC i);
       entry
               a1 = ptr
               a2 = count of words to be shifted right
               a3 = size of right shift
       exit
               a1 = shift out from last shift right
               a2 - a4, ip destroyed */
        EXPORT(asm_shiftright_loop_up) /* word aligned shiftright loop up */
GLABEL(asm_shiftright_loop_up)
        STMFD   sp!,{v6,lr}
        MOV     a4,#0           /* initial shift carry */
        RSB     v6,a3,#32       /* size of complementary left shift */
LABEL(asm_shiftright_loop_up_l0)
        ANDS    ip,a2,#3              /* multiple of 4 words ? */
        BEQ     asm_shiftright_loop_up_l1 /* yup, so branch */
        LDR     lr,[a1]         /* shiftright the first 1-3 words */
        ORR     a4,a4,lr,LSR a3 /* to align the total to a multiple */
        STR     a4,[a1],#4      /* of 4 words */
        MOV     a4,lr,ASL v6
        CMP     ip,#2
        BLT     asm_shiftright_loop_up_l1 /* better to branch than skip instrs. */
        LDRGE   lr,[a1]
        ORRGE   a4,a4,lr,LSR a3
        STRGE   a4,[a1],#4
        MOVGE   a4,lr,ASL v6
        LDRGT   lr,[a1]
        ORRGT   a4,a4,lr,LSR a3
        STRGT   a4,[a1],#4
        MOVGT   a4,lr,ASL v6
LABEL(asm_shiftright_loop_up_l1)
        BICS    ip,a2,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,a4           /* if zero then we're done */
        LDMEQFD sp!,{v6,pc}^    /* so return last shift out */
        STMFD   sp!,{v1-v3}     /* save work regs */
LABEL(asm_shiftright_loop_up_l2)
        LDMIA   a1,{v1,v2,v3,lr} /* load 4 words in one go */
        ORR     a2,a4,v1,LSR a3  /* shiftright the four words */
        MOV     a4,v1,ASL v6     /* keep carry in a4 */
        ORR     v1,a4,v2,LSR a3  /* and store results down a register */
        MOV     a4,v2,ASL v6     /* to regs a2,v1-v3 */
        ORR     v2,a4,v3,LSR a3
        MOV     a4,v3,ASL v6
        ORR     v3,a4,lr,LSR a3
        MOV     a4,lr,ASL v6
        STMIA   a1!,{a2,v1,v2,v3}     /* store 4 results */
        SUBS    ip,ip,#4              /* decrement counter by 4 */
        BGT     asm_shiftright_loop_up_l2 /* if count still positive then loop */
        MOV     a1,a4                 /* result = last shift out */
        LDMFD   sp!,{v1-v3,v6,pc}^    /* restore work regs and return */

/* extern uintD asm_shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
       entry
               a1 = ptr
               a2 = count of words to be shifted right signed
               a3 = size of right shift
       exit
               a1 = shift out from last shift right
               a2 - a4, ip destroyed */
        EXPORT(asm_shiftrightsigned_loop_up) /* word aligned shiftrightsigned loop up */
GLABEL(asm_shiftrightsigned_loop_up)
        STMFD   sp!,{v6,lr}
        RSB     v6,a3,#32       /* size of complementary left shift */
        LDR     lr,[a1]         /* setup carry for first shift. */
        MOV     a4,lr,ASR #31   /* this is the sign extended bits */
        AND     a4,a4,a4,LSL v6 /* 31->(32-i) of the first word */
        B       asm_shiftright_loop_up_l0 /* use right shift code now */

/* extern uintD asm_shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
       entry
               a1 = sourceptr
               a2 = destptr
               a3 = count of words to be shifted right
               a4 = size of right shift
               [sp] = carry for first shift
       exit
               a1 = shift out from last shift right
               a2 - a4, ip destroyed */
        EXPORT(asm_shiftrightcopy_loop_up) /* word aligned shiftrightcopy loop up */
GLABEL(asm_shiftrightcopy_loop_up)
        STMFD   sp!,{v5,v6,lr}
        LDR     v5,[sp,#12]     /* initial shift carry */
        RSB     v6,a4,#32       /* size of complementary left shift */
        MOV     v5,v5,ASL v6
LABEL(asm_shiftrightcopy_loop_up_l0)
        ANDS    ip,a3,#3                  /* multiple of 4 words ? */
        BEQ     asm_shiftrightcopy_loop_up_l1 /* yup, so branch */
        LDR     lr,[a1],#4      /* shiftright the first 1-3 words */
        ORR     v5,v5,lr,LSR a4 /* to align the total to a multiple */
        STR     v5,[a2],#4      /* of 4 words */
        MOV     v5,lr,ASL v6
        CMP     ip,#2
        BLT     asm_shiftrightcopy_loop_up_l1 /* better to branch than skip instrs. */
        LDRGE   lr,[a1],#4
        ORRGE   v5,v5,lr,LSR a4
        STRGE   v5,[a2],#4
        MOVGE   v5,lr,ASL v6
        LDRGT   lr,[a1],#4
        ORRGT   v5,v5,lr,LSR a4
        STRGT   v5,[a2],#4
        MOVGT   v5,lr,ASL v6
LABEL(asm_shiftrightcopy_loop_up_l1)
        BICS    ip,a3,#3        /* set counter to multiple of 4 */
        MOVEQ   a1,v5           /* if zero then we're done */
        LDMEQFD sp!,{v5,v6,pc}^ /* so return last shift out */
        STMFD   sp!,{v1-v3}     /* save work regs */
LABEL(asm_shiftrightcopy_loop_up_l2)
        LDMIA   a1!,{v1,v2,v3,lr} /* load 4 words in one go */
        ORR     a3,v5,v1,LSR a4   /* shiftright the four words */
        MOV     v5,v1,ASL v6      /* keep carry in v5 */
        ORR     v1,v5,v2,LSR a4 /* and store results down a register */
        MOV     v5,v2,ASL v6    /* to regs a2,v1-v3 */
        ORR     v2,v5,v3,LSR a4
        MOV     v5,v3,ASL v6
        ORR     v3,v5,lr,LSR a4
        MOV     v5,lr,ASL v6
        STMIA   a2!,{a3,v1,v2,v3}         /* store 4 results */
        SUBS    ip,ip,#4                  /* decrement counter by 4 */
        BGT     asm_shiftrightcopy_loop_up_l2 /* if count still positive then loop */
        MOV     a1,v5                     /* result = last shift out */
        LDMFD   sp!,{v1-v3,v5,v6,pc}^ /* restore work regs and return */

#ifndef HAVE_umull
/* mulu32_64_vregs
       entry
               a1 = x
               ip = y
       exit
               v1 = low32(x*y)
               ip = high32(x*y)
               v2,v3,v4 destroyed */
LABEL(mulu32_64_vregs)
        MOV     v1,a1,LSR #16    /* temp := top half of x */
        MOV     v2,ip,LSR #16    /* hi := top half of y */
        BIC     v3,a1,v1,LSL #16 /* x  := bottom half of x */
        BIC     ip,ip,v2,LSL #16 /* y  := bottom half of y */
        MUL     v4,v3,ip         /* low section of result */
        MUL     ip,v1,ip         /* ) middle sections */
        MUL     v3,v2,v3         /* )   of result */
        MUL     v2,v1,v2         /* high section of result */
        ADDS    ip,ip,v3         /* add middle sections */
                                 /* (can't use mla as we need carry) */
        ADDCS   v2,v2,#0x10000   /* carry from above add */
        ADDS    v1,v4,ip,LSL #16 /* x is now bottom 32 bits of result */
        ADC     ip,v2,ip,LSR #16 /* hi is top 32 bits */
        MOVS    pc,lr
#endif  /* HAVE_umull */

/* extern uintD asm_mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
       entry
               a1 = digit
               a2 = ptr
               a3 = count of words to be multiplied down
               a4 = new digit = carry
       exit
               a1 = final carry of multiply
               a2 - a4, ip destroyed */
        EXPORT(asm_mulusmall_loop_down)
GLABEL(asm_mulusmall_loop_down)
        CMP     a3,#0
        MOVEQ   a1,a4
        MOVEQS  pc,lr
#ifdef HAVE_umull
        STMFD   sp!,{v1,lr}
LABEL(asm_mulusmall_loop_down_l1)
        LDR     ip,[a2,#-4]!
        UMULL   v1,ip,a1,ip     /* muluD(digit,*--ptr,hi=,lo=) */
        ADDS    v1,v1,a4        /* lo += carry */
        ADC     a4,ip,#0       /* if (lo<carry) { hi += 1 }; carry=hi */
        STR     v1,[a2,#0]     /* *ptr = lo */
        SUBS    a3,a3,#1       /* len-- */
        BNE     asm_mulusmall_loop_down_l1 /* until len==0 */
        MOV     a1,a4                  /* return carry */
        LDMFD   sp!,{v1,pc}^
#else
        STMFD   sp!,{v1-v2,lr}
LABEL(asm_mulusmall_loop_down_l1)
        LDR     ip,[a2,#-4]!

/*       BL      mulu32_64_vregs         / * muluD(digit,*--ptr,hi=,lo=) */
/* replaced by multiplication of a small x = a1 and a big y = ip : */
        MOV     v1,ip,LSR #16    /* top half of y */
        BIC     ip,ip,v1,LSL #16 /* bottom half of y */
        MUL     v2,a1,v1         /* middle section of result */
        MUL     v1,a1,ip         /* low section of result */
        MOV     ip,#0            /* high section of result */
        ADDS    v1,v1,v2,LSL #16 /* bottom 32 bits of result */
        ADC     ip,ip,v2,LSR #16 /* top 32 bits of result */

        ADDS    v1,v1,a4        /* lo += carry */
        ADC     a4,ip,#0       /* if (lo<carry) { hi += 1 }; carry=hi */
        STR     v1,[a2,#0]     /* *ptr = lo */
        SUBS    a3,a3,#1       /* len-- */
        BNE     asm_mulusmall_loop_down_l1 /* until len==0 */
        MOV     a1,a4                  /* return carry */
        LDMFD   sp!,{v1-v2,pc}^
#endif

/* extern void asm_mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
       entry
               a1 = digit
               a2 = sourceptr
               a3 = destptr
               a4 = count of words to be multiplied down
       exit
               a1 - a4, ip destroyed */
        EXPORT(asm_mulu_loop_down)
GLABEL(asm_mulu_loop_down)
#ifdef HAVE_umull
        STMFD   sp!,{v1,v5,lr}
        MOV     v5,#0
LABEL(asm_mulu_loop_down_l1)
        LDR     ip,[a2,#-4]!
        UMULL   v1,ip,a1,ip     /* muluD(digit,*--sourceptr,hi=,lo=) */
        ADDS    v1,v1,v5        /* lo += carry */
        ADC     v5,ip,#0       /* if (lo<carry) { hi += 1 }; carry=hi */
        STR     v1,[a3,#-4]!   /* *--destptr = lo */
        SUBS    a4,a4,#1       /* len-- */
        BNE     asm_mulu_loop_down_l1 /* until len==0 */
        STR     v5,[a3,#-4]!      /* *--destptr = carry */
        LDMFD   sp!,{v1,v5,pc}^
#else
        STMFD   sp!,{v1-v5,lr}
        MOV     v5,#0
LABEL(asm_mulu_loop_down_l1)
        LDR     ip,[a2,#-4]!
        BL      mulu32_64_vregs /* muluD(digit,*--sourceptr,hi=,lo=) */
        ADDS    v1,v1,v5        /* lo += carry */
        ADC     v5,ip,#0       /* if (lo<carry) { hi += 1 }; carry=hi */
        STR     v1,[a3,#-4]!   /* *--destptr = lo */
        SUBS    a4,a4,#1       /* len-- */
        BNE     asm_mulu_loop_down_l1 /* until len==0 */
        STR     v5,[a3,#-4]!      /* *--destptr = carry */
        LDMFD   sp!,{v1-v5,pc}^
#endif

/* extern void asm_muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
       entry
               a1 = digit
               a2 = sourceptr
               a3 = destptr
               a4 = count of words to be multiplied added down
       exit
               a1 - a4, ip destroyed */
        EXPORT(asm_muluadd_loop_down)
GLABEL(asm_muluadd_loop_down)
#ifdef HAVE_umull
        STMFD   sp!,{v1,v5,lr}
        MOV     v5,#0
LABEL(asm_muluadd_loop_down_l1)
        LDR     ip,[a2,#-4]!
        UMULL   v1,ip,a1,ip     /* muluD(digit,*--sourceptr,hi=,lo=) */
        ADDS    v1,v1,v5        /* lo += carry */
        ADCCS   ip,ip,#0        /* if (lo<carry) { hi += 1 }; */
        LDR     v5,[a3,#-4]!    /* carry = *--destptr */
        ADDS    v1,v1,v5        /* lo += carry */
        ADC     v5,ip,#0       /* if (lo<carry) { hi += 1 }; carry=hi */
        STR     v1,[a3,#0]     /* *destptr = lo */
        SUBS    a4,a4,#1       /* len-- */
        BNE     asm_muluadd_loop_down_l1 /* until len==0 */
        MOV     a1,v5                /* return carry */
        LDMFD   sp!,{v1,v5,pc}^
#else
        STMFD   sp!,{v1-v5,lr}
        MOV     v5,#0
LABEL(asm_muluadd_loop_down_l1)
        LDR     ip,[a2,#-4]!
        BL      mulu32_64_vregs /* muluD(digit,*--sourceptr,hi=,lo=) */
        ADDS    v1,v1,v5        /* lo += carry */
        ADCCS   ip,ip,#0        /* if (lo<carry) { hi += 1 }; */
        LDR     v5,[a3,#-4]!    /* carry = *--destptr */
        ADDS    v1,v1,v5        /* lo += carry */
        ADC     v5,ip,#0       /* if (lo<carry) { hi += 1 }; carry=hi */
        STR     v1,[a3,#0]     /* *destptr = lo */
        SUBS    a4,a4,#1       /* len-- */
        BNE     asm_muluadd_loop_down_l1 /* until len==0 */
        MOV     a1,v5                /* return carry */
        LDMFD   sp!,{v1-v5,pc}^
#endif

/* extern void asm_mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
       entry
               a1 = digit
               a2 = sourceptr
               a3 = destptr
               a4 = count of words to be multiplied subtracted down
       exit
               a1 - a4, ip destroyed */
        EXPORT(asm_mulusub_loop_down)
GLABEL(asm_mulusub_loop_down)
#ifdef HAVE_umull
        STMFD   sp!,{v1,v5,lr}
        MOV     v5,#0
LABEL(asm_mulusub_loop_down_l1)
        LDR     ip,[a2,#-4]!
        UMULL   v1,ip,a1,ip     /* muluD(digit,*--sourceptr,hi=,lo=) */
        ADDS    v1,v1,v5        /* lo += carry */
        ADC     v5,ip,#0        /* if (lo<carry) { hi += 1 }; */
        LDR     ip,[a3,#-4]!    /* carry = *--destptr */
        SUBS    ip,ip,v1
        STR     ip,[a3,#0]      /* *destptr = carry - lo */
        ADDCC   v5,v5,#1       /* if (carry<lo) { hi += 1 }; carry=hi */
        SUBS    a4,a4,#1       /* len-- */
        BNE     asm_mulusub_loop_down_l1 /* until len==0 */
        MOV     a1,v5                /* return carry */
        LDMFD   sp!,{v1,v5,pc}^
#else
        STMFD   sp!,{v1-v5,lr}
        MOV     v5,#0
LABEL(asm_mulusub_loop_down_l1)
        LDR     ip,[a2,#-4]!
        BL      mulu32_64_vregs /* muluD(digit,*--sourceptr,hi=,lo=) */
        ADDS    v1,v1,v5        /* lo += carry */
        ADC     v5,ip,#0        /* if (lo<carry) { hi += 1 }; */
        LDR     ip,[a3,#-4]!    /* carry = *--destptr */
        SUBS    ip,ip,v1
        STR     ip,[a3,#0]      /* *destptr = carry - lo */
        ADDCC   v5,v5,#1       /* if (carry<lo) { hi += 1 }; carry=hi */
        SUBS    a4,a4,#1       /* len-- */
        BNE     asm_mulusub_loop_down_l1 /* until len==0 */
        MOV     a1,v5                /* return carry */
        LDMFD   sp!,{v1-v5,pc}^
#endif

#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
        .section .note.GNU-stack,"",%progbits
#endif

        END

#endif
