/*
 * CLISP interface to GNU regex
 * originally by Bruno Haible 14.4.1995
 * rewritten by Sam Steingold 2003-08-06
 */

#include <clisp.h>
#include <sys/types.h>          /* regex.h needs this */
#include <stdlib.h>             /* declare malloc(), free() */
#include <stdio.h>              /* BUFSIZ */
#include <regex.h>

#ifndef FOREIGN
#error FOREIGN is not defined.
#error REGEXP needs a CLISP built with the foreign pointer datatype support.
#error Go into the main CLISP makefile and add a -DFOREIGN=void*
#error to CFLAGS make variable and rebuild CLISP before coming back here.
#endif

DEFMODULE(regexp,"REGEXP");

DEFUN(REGEXP::REGEXP-COMPILE, pattern &key EXTENDED IGNORE-CASE NEWLINE NOSUB)
{ /* compile the pattern into a regular expression */
  object pattern = check_string(STACK_4);
  int cflags = 0, status;
  regex_t* re;
  if (!missingp(STACK_0)) cflags |= REG_NOSUB;
  if (!missingp(STACK_1)) cflags |= REG_NEWLINE;
  if (!missingp(STACK_2)) cflags |= REG_ICASE;
  if (!missingp(STACK_3)) cflags |= REG_EXTENDED;
 restart_regcomp:
  begin_system_call();
  re = (regex_t*)malloc(sizeof(regex_t));
  end_system_call();
  if (re == NULL) OS_error();
  with_string_0(pattern,GLO(misc_encoding),patternz, {
    begin_system_call();
    status = regcomp(re,patternz,cflags);
    end_system_call();
  });
  if (status) {
    char buf[BUFSIZ];
    begin_system_call();
    regerror(status,re,buf,BUFSIZ);
    free(re);
    end_system_call();
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(asciz_to_string(buf,GLO(misc_encoding)));
    pushSTACK(pattern); pushSTACK(TheSubr(subr_self)->name);
    check_value(error,"~ (~): ~");
    pattern = value1;
    goto restart_regcomp;
  }
  STACK_2 = STACK_1 = allocate_fpointer((FOREIGN)re);
  STACK_0 = ``REGEXP::REGEXP-FREE``; funcall(L(finalize),2);
  VALUES1(STACK_0);
  skipSTACK(3);
}

DEFUN(REGEXP::REGEXP-FREE, compiled)
{ /* release the contents and the data of the compiled pattern */
  if (fpointerp(STACK_0) && fp_validp(TheFpointer(STACK_0))) {
    regex_t *re = (regex_t*)TheFpointer(STACK_0)->fp_pointer;
    regfree(re);
    free(re);
    VALUES1(T);
  } else
    VALUES1(NIL);
  skipSTACK(1);
}

DEFUN(REGEXP::REGEXP-EXEC, pattern string &key BOOLEAN START END NOTBOL NOTEOL)
{ /* match the compiled pattern against the string */
  object string = (STACK_5 = check_string(STACK_5));
  unsigned int length = vector_length(string);
  unsigned int start = missingp(STACK_3) ? 0
    : posfixnum_to_L(STACK_3 = check_posfixnum(STACK_3));
  unsigned int end = missingp(STACK_2) ? length
    : posfixnum_to_L(STACK_2 = check_posfixnum(STACK_2));
  int eflags = ((missingp(STACK_0) ? 0 : REG_NOTEOL) |
                (missingp(STACK_1) ? 0 : REG_NOTBOL));
  int status;
  bool bool_p = !missingp(STACK_4);
  regex_t *re;
  regmatch_t *ret;
  skipSTACK(5);                 /* drop all options */
  STACK_1 = check_fpointer(STACK_1,true);
  string = STACK_0;
  if (end != length || start != 0) {
    pushSTACK(sfixnum((int)(end-start)));
    pushSTACK(`:ELEMENT-TYPE`); pushSTACK(S(character));
    pushSTACK(`:DISPLACED-TO`); pushSTACK(string);
    pushSTACK(`:DISPLACED-INDEX-OFFSET`); pushSTACK(posfixnum(start));
    funcall(L(make_array),7);
    string = value1;
  }
  re = (regex_t*)TheFpointer(STACK_1)->fp_pointer;
  begin_system_call();
  ret = (regmatch_t*)alloca((re->re_nsub+1)*sizeof(regmatch_t));
  end_system_call();
  if (ret == NULL) OS_error();
  with_string_0(string,GLO(misc_encoding),stringz, {
    begin_system_call();
    status = regexec(re,stringz,re->re_nsub+1,ret,eflags);
    end_system_call();
  });
  if (status) {
    VALUES0;
  } else if (bool_p) {
    VALUES1(T);                 /* success indicator */
  } else {
    int count;
    for (count = 0; count <= re->re_nsub; count++)
      if (ret[count].rm_so >= 0 && ret[count].rm_eo >= 0) {
        pushSTACK(posfixnum(start+ret[count].rm_so));
        pushSTACK(posfixnum(start+ret[count].rm_eo));
        funcall(`REGEXP::MAKE-MATCH-BOA`,2); pushSTACK(value1);
      } else pushSTACK(NIL);
    funcall(L(values),re->re_nsub+1);
  }
  skipSTACK(2);                 /* drop pattern & string */
}
