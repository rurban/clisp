/* CLISP interface to GNU regex */
/* Bruno Haible 14.4.1995 */

#include <sys/types.h> /* regex.h needs this */
#include "regex.h"

#include <stdlib.h> /* declare malloc(), free() */

int mregcomp (ppreg,pattern,cflags)
  regex_t* * ppreg;
  const char * pattern;
  int cflags;
{
  regex_t* preg;

  *ppreg = preg = (regex_t*) malloc(sizeof(regex_t));
  if (!preg)
    return (int) REG_ESPACE;

  return regcomp(preg,pattern,cflags);
}

const char * mregerror (errcode,preg)
  int errcode;
  const regex_t * preg;
{
  size_t errbuf_size = 80; /* This will be enough. */
  char* errbuf = (char*) malloc(errbuf_size);
  if (!errbuf)
    return (const char *) 0;
  regerror(errcode,preg,errbuf,errbuf_size);
  return errbuf;
}

void mregfree (preg)
  regex_t* preg;
{
  regfree(preg);
  free(preg);
}
