	.file	"cache-rs6000.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
	.align 2
	.globl __TR_clear_cache
	.globl .__TR_clear_cache
.csect __TR_clear_cache[DS]
__TR_clear_cache:
	.long .__TR_clear_cache, TOC[tc0], 0
.csect .text[PR]
.__TR_clear_cache:
	br
LT..__TR_clear_cache:
	.long 0
	.byte 0,0,32,64,0,0,1,0
	.long 0
	.long LT..__TR_clear_cache-.__TR_clear_cache
	.short 16
	.byte "__TR_clear_cache"
_section_.text:
.csect .data[RW]
	.long _section_.text
