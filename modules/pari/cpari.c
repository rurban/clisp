/*
 * CLISP interface to PARI <http://pari.math.u-bordeaux.fr/>
 * Copyright (C) 1995 Michael Stoll
 * Copyright (C) 2004 Sam Steingold
 * This is free software, distributed under the GNU GPL
 */

#include <pari/pari.h>
#include "cpari.h"

void clispPutc(char c);
void clispPuts(const char *s);
void clispFlush(void);

void clispErrPutc(char c);
void clispErrPuts(const char *s);
void clispErrFlush(void);
void clispErrDie(void);

PariOUT clispOut = {clispPutc, clispPuts, clispFlush, NULL};
PariOUT clispErr = {clispErrPutc, clispErrPuts, clispErrFlush, clispErrDie};

void *clispTemp; /* a foreign place to use for casts and accesses from CLISP */

void init_for_clisp (long parisize, long maxprime)
{
  long v, n, *e;
  char *p;
  GEN p1;

  extern ulong init_opts;
  init_opts = 0;

  pari_init(parisize,maxprime);
  /*init_graph();*/

  pari_outfile = stdout;errfile = stderr;logfile = NULL;infile = stdin;
  pariOut = &clispOut; pariErr = &clispErr;
}

void fini_for_clisp (int leaving)
{
  /*free_graph();*/
  freeall();
  killallfiles(leaving);
}
