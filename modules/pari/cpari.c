/*
 * CLISP interface to PARI <http://pari.math.u-bordeaux.fr/>
 * Copyright (C) 1995 Michael Stoll
 * Copyright (C) 2004 Sam Steingold
 * This is free software, distributed under the GNU GPL
 */

#include "pari.h"

#if defined(USE_NEXT_VM)
#include <mach/mach.h>
#define PAGES_CHUNK 16   /* Number of memory pages allocated together */
#define CHUNK_SIZE (PAGES_CHUNK * vm_page_size)
#define PARI_STACK_TOP 0x38000000
    /* Upper end of some address range not used by CLISP,
       to use for the pari stack. */
#endif /* USE_NEXT_VM */

ulong base_avma;

extern ulong avloc;

void clispPutc(char c);
void clispPuts(char *s);
void clispFlush(void);

void clispErrPutc(char c);
void clispErrPuts(char *s);
void clispErrFlush(void);
void clispErrDie(void);

PariOUT clispOut = {clispPutc, clispPuts, clispFlush};
PariERR clispErr = {clispErrPutc, clispErrPuts, clispErrFlush, clispErrDie};

void *clispTemp; /* a foreign place to use for casts and accesses from CLISP */

#if defined(USE_NEXT_VM)
/* Use Mach vm_allocate() for allocating and extending the stack.
   Advantage: Stack can grow without being moved.
   Disadvantage: Must have some fixed address range for it.
 */

void extend_stack(ulong new_bot)
{
  long need = bot - new_bot;
  long need_chunks = (need + CHUNK_SIZE - 1) / CHUNK_SIZE;
  long new_bot_needed = bot - need_chunks * CHUNK_SIZE;

  if (vm_allocate(task_self(), (vm_address_t *)&new_bot_needed,
                   bot - new_bot_needed, FALSE)
       == KERN_SUCCESS)
    bot = new_bot_needed;
  else
    err(errpile);   /* err(errpile) doesn't call allocatemoremem */
}

/* Change cgetx to call extend_stack() instead of err(errpile) */

GEN cgetg(long x, long y)
{
  ulong p1;
  GEN z;

  p1=avma-(((ulong)x)<<TWOPOTBYTES_IN_LONG);
  if(p1<bot) extend_stack(p1);
  avma=p1;z=(GEN)p1;z[0]=evalpere(1)+evaltyp(y)+evallg(x);
  return z;
}

GEN cgeti(long x)
{
  ulong p1;
  GEN z;

  p1=avma-(((ulong)x)<<TWOPOTBYTES_IN_LONG);
  if(p1<bot) extend_stack(p1);
  avma=p1;z=(GEN)p1;z[0]=evaltyp(1)+evalpere(1)+evallg(x);
  return z;
}

GEN cgetr(long x)
{
  ulong p1;
  GEN z;

  p1=avma-(((ulong)x)<<TWOPOTBYTES_IN_LONG);
  if(p1<bot) extend_stack(p1);
  avma=p1;z=(GEN)p1;z[0]=evaltyp(2)+evalpere(1)+evallg(x);
  return z;
}
#endif /* USE_NEXT_VM */

void init_for_clisp(long parisize, long maxprime)
{
  long v, n, *e;
  char *p;
  GEN p1;

  printvariable=printvargp;

  outfile = stdout;errfile = stderr;logfile = NULL;infile = stdin;
  pariOut = &clispOut; pariErr = &clispErr;
  compact_arrays=1;
  pari_randseed=1;v=parisize&(BYTES_IN_LONG-1);
  if(v) parisize+=(BYTES_IN_LONG-v);            /* align to long boundary */
  if (!(diffptr=initprimes(maxprime))) err(memer);
#if __MWERKS__
  {
    OSErr resultCode; Handle newHand = MFTempNewHandle(parisize,&resultCode);
    if (!newHand) err(memer);
    HLock(newHand);
    bot=(long)*newHand;
  }
#else
#if defined(USE_NEXT_VM)
  top = avma = bot = PARI_STACK_TOP;
  extend_stack(bot-1);
     /* Allocate a first chunk of memory.
        Note: parisize is effectively ignored. */
#else
  if (!(bot=(long)malloc(parisize))) err(memer);
#endif /* USE_NEXT_VM */
#endif
#if !defined(USE_NEXT_VM)
  top=avma=bot+parisize;
#endif /* USE_NEXT_VM */
  if (!(varentries=(entree **)malloc(sizeof(entree*)*MAXVAR))) err(memer);
  if (!(hashtable=(entree **)malloc(sizeof(entree*)*TBLSZ))) err(memer);
  if (!(ordvar=(long *)malloc(sizeof(long)*MAXVAR))) err(memer);
  if (!(polun=(GEN *)malloc(sizeof(GEN)<<MAXSHIFTVAR))) err(memer);
  if (!(polx=(GEN *)malloc(sizeof(GEN)<<MAXSHIFTVAR))) err(memer);
  if (!(g=(GEN *)malloc(sizeof(GEN)*STACKSIZE))) err(memer);
  if (!(rectgraph=(long**)malloc(sizeof(long*)*16))) err(memer);
  for(n=0;n<16;n++)
  {
    if(!(e=rectgraph[n]=(long*)malloc(sizeof(long)*10))) err(memer);
    e[0]=e[1]=e[2]=e[3]=0;e[4]=lgetr(3);e[5]=lgetr(3);e[6]=lgetr(3);e[7]=lgetr(3);
    e[8]=lgetr(3);e[9]=lgetr(3);
  }
  for(n = 0; n < TBLSZ; n++) hashtable[n] = NULL;
  for(v = 0; v < NUMFUNC; v++)
  {
    for(n = 0, p = fonctions[v].name; *p; p++) n = n << 1 ^ *p;
    if (n < 0) n = -n; n %= TBLSZ;
    fonctions[v].next = hashtable[n];
    hashtable[n] = fonctions + v;
  }
  gnil = cgeti(2);gnil[1]=2; setpere(gnil,MAXUBYTE);
  gzero = cgeti(2);gzero[1]=2; setpere(gzero, MAXUBYTE);
  gun = stoi(1); setpere(gun, MAXUBYTE);
  gdeux = stoi(2); setpere(gdeux, MAXUBYTE);
  ghalf = cgetg(3,4);ghalf[1]=un;ghalf[2]=deux; setpere(ghalf, MAXUBYTE);
  gi = cgetg(3,6); gi[1] = zero; gi[2] = un; setpere(gi, MAXUBYTE);
  p1=cgetg(4,10);p1[1]=evalsigne(1)+evalvarn(MAXVARN)+evallgef(4);
  p1[2]=zero;p1[3]=un;polx[MAXVARN]=p1;
  p1=cgetg(3,10);p1[1]=evalsigne(1)+evalvarn(MAXVARN)+evallgef(3);
  p1[2]=un;polun[MAXVARN]=p1;
  for(v=0; v < MAXVAR; v++) ordvar[v] = v;
  polvar = cgetg(MAXVAR + 1,17); setlg(polvar,1); setpere(polvar, MAXUBYTE);
    /* a row vector of length MAXVAR, active length set to zero entries */
  for(v=1;v<=MAXVAR;v++) polvar[v]=evaltyp(17)+evalpere(MAXUBYTE)+evallg(1);
    /* fill polvar with something reasonable */
  primetab = cgetg(NUMPRTBELT+2,17);
  for(v = 1; v <= NUMPRTBELT+1; v++) primetab[v]=un;
    /* fill table of private primes with ones */
  for(v = 0; v < STACKSIZE; v++) g[v] = gzero;
    /* fill gp stack with zeros */
  lisseq("x");avloc=avma;
#if defined(USE_NEXT_VM)
  base_avma = avma;
#endif /* USE_NEXT_VM */
}

