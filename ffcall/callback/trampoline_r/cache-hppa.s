	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.IMPORT $global$,DATA
	.IMPORT $$dyncall,MILLICODE
; gcc_compiled.:
	.SPACE $TEXT$
	.SUBSPA $CODE$

	.align 4
	.EXPORT __TR_clear_cache,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR
__TR_clear_cache
	.PROC
	.CALLINFO FRAME=0,NO_CALLS
	.ENTRY
	fdc 0(0,%r26)
	fdc 0(0,%r25)
	sync
	mfsp %sr0,%r19
	ldsid (0,%r26),%r20
	mtsp %r20,%sr0
	fic 0(%sr0,%r26)
	fic 0(%sr0,%r25)
	sync
	mtsp %r19,%sr0
	nop
	nop
	nop
	nop
	nop
	nop
	bv,n 0(%r2)
	.EXIT
	.PROCEND
