#include "asmsparc.h"
.text
	.align 4
	.global C(__TR_clear_cache)
	DECLARE_FUNCTION(__TR_clear_cache)
	.proc	020
C(__TR_clear_cache:)
	!$PROLOGUE$ 0
	!$PROLOGUE$ 1
	iflush %o0+0;iflush %o0+8;iflush %o0+16;iflush %o0+24
	retl
	nop
