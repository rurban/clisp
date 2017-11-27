/*
 * External routines for ARILEV1.D
 * Processor: HPPA, because of XMPYU only on HPPA 1.1 (like HP9000/720)
 * Compiler: GNU-C or HP-C
 * Parameter passing conventions:
 *   Argument registers:
 *     %arg0,%arg1,%arg2,%arg3 (= %r26,%r25,%r24,%r23)
 *   Return value register:
 *     %ret0 = %r28 for a single word, %r28,%r29 for a 'long long'.
 *   Call-used registers (do not have to be preserved across function calls):
 *     %r1, %r19..%r26, %r28..%r29, %r31
 * Settings: intCsize=32, intDsize=32.
 */

#ifdef INCLUDED_FROM_C

#else

/* mostly copied out of hppa.s from the PARI/GP-distribution. */

#ifndef __GNUC__ /* with GNU-C we do mulu32() as macro, that multiplies inline */

                .SHORTDATA
                .IMPORT $global$,DATA
                .EXPORT mulu32_high
                .ALIGN 8
                .LABEL mulu32_high
                .WORD           /* 8 byte room */
                .WORD

                .CODE
                .EXPORT asm_mulu32_
/* extern struct { uint32 lo; uint32 hi; } asm_mulu32_ (uint32 arg1, uint32 arg2);
   2^32*hi+lo := arg1*arg2. */
                .LABEL asm_mulu32_
                .PROC
                .CALLINFO
                .ENTRY  /* input in %arg0,%arg1, Output in %ret0,mulu32_high */
                LDIL    L'mulu32_high-$global$,%r1
                LDO     R'mulu32_high-$global$(%r1),%r1
                                                /* %r1 = &x */
                STW     %arg0,0(%r1)            /* store x abspeichern */
                FLDWS   0(%r1),%fr4             /* and load into coprocessor */
                STW     %arg1,0(%r1)            /* store y */
                FLDWS   0(%r1),%fr5             /* and load into coprocessor */
                XMPYU   %fr4,%fr5,%fr6          /* multiply both */
                FSTDS   %fr6,0(%r1)             /* store result (64 bit) */
                LDWS    4(%r1),%ret0            /* low 32 bits as result */
                BV      0(%r2)                  /* Return */
                NOP
                .EXIT
                .PROCEND

#endif



                .CODE
                .EXPORT asm_length32
/* returns integer-size (>=1, <=32) of the argument /=0. */
                .LABEL asm_length32
                .PROC
                .CALLINFO
                .ENTRY          /* input in %arg0, output in %ret0 */
                /* y = 1; */
                LDI             1,%ret0
                /* if (x & (bit(31-15)*(bit(16)-1)) == 0) */
                EXTRU,<>        %arg0,15,16,%r0
                SHD,TR          %arg0,%r0,16,%arg0   /* x = x<<(32-16); else */
                ADDI            16,%ret0,%ret0       /* y = y+16; */
                /* if (x & (bit(31-7)*(bit(8)-1)) == 0) */
                EXTRU,<>        %arg0,7,8,%r0
                SHD,TR          %arg0,%r0,24,%arg0   /* x = x<<(32-24); else */
                ADDI            8,%ret0,%ret0        /* y = y+8; */
                /* if (x & (bit(31-3)*(bit(4)-1)) == 0) */
                EXTRU,<>        %arg0,3,4,%r0
                SHD,TR          %arg0,%r0,28,%arg0   /* x = x<<(32-28); else */
                ADDI            4,%ret0,%ret0        /* y = y+4; */
                /* if (x & (bit(31-1)*(bit(2)-1)) == 0) */
                EXTRU,<>        %arg0,1,2,%r0
                SHD,TR          %arg0,%r0,30,%arg0   /* x = x<<(32-30); else */
                ADDI            2,%ret0,%ret0        /* y = y+2; */
                /* if (x & (bit(31-0)*(bit(1)-1)) != 0) */
                EXTRU,=         %arg0,0,1,%r0
                ADDI            1,%ret0,%ret0        /* y = y+1; */
                BV              0(%r2)               /* Return */
                NOP
                .EXIT
                .PROCEND


#if defined __linux__ || defined __FreeBSD__ || defined __FreeBSD_kernel__ || defined __DragonFly__
                .section .note.GNU-stack,"",@progbits
#endif

                .END

#endif
