/* CLISP interface to GNU regex */
/* Bruno Haible 14.4.1995 */

#include <sys/types.h> /* regex.h needs this */
#include <stdlib.h> /* declare malloc(), free() */
#include <regex.h>

int mregcomp (regex_t* * ppreg, const char * pattern, int cflags) {
  regex_t* preg;

  *ppreg = preg = (regex_t*) malloc(sizeof(regex_t));
  if (!preg)
    return (int) REG_ESPACE;

  return regcomp(preg,pattern,cflags);
}

regmatch_t* mregexec (const regex_t *preg, const char *string, int eflags)
{
  regmatch_t *ret = (regex_t*)calloc(preg->re_nsub+2, sizeof(regmatch_t));
  if (regexec(preg,string,preg->re_nsub+1,ret,eflags)) {
    free(ret);
    return NULL;
  } else return ret;
}

const char * mregerror (int errcode, const regex_t * preg) {
  size_t errbuf_size = 80; /* This will be enough. */
  char* errbuf = (char*) malloc(errbuf_size);
  if (!errbuf)
    return (const char *) 0;
  regerror(errcode,preg,errbuf,errbuf_size);
  return errbuf;
}

void mregfree (regex_t* preg) {
  regfree(preg);
  free(preg);
}
