/* This file, when compiled to assembly-language, provides a starting point
   for developing the arilev1 part (the loops) of an ari_asm_${cpu}.d
   implementation. */

/* Tell arilev1c.d to declare functions with external linkage. */
#define ARILEV1_EXTERN

#include "lispbibl.c"

#include "aridecl.c"  /* declarations */
#include "arilev0.c"  /* machine-arithmetics */
#include "arilev1.c"  /* digit-sequences */
