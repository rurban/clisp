/* Instruction cache flushing for rs6000 */

/*
 * Copyright 1997 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

void __TR_clear_cache (char* first_addr)
{
  /* Taken from gforth-0.3.0. */
#if 0 /* This is not accepted by the AIX assembler in PWR or COM mode. */
  asm volatile ("icbi (%0); isync" : : "b" (first_addr));
#endif
}
