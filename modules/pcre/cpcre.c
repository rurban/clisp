/*
 * PCRE - Perl Compatible Regular Expressions
 * <http://www.pcre.org/>
 * Copyright (C) 2003 Sam Steingold
 * GPL2
 */

#include "config.h"
#include <pcre.h>

#include "clisp.h"

#ifndef FOREIGN
#error FOREIGN is not defined.
#error REGEXP needs a CLISP built with the foreign pointer datatype support.
#error Go into the main CLISP makefile and add a -DFOREIGN=void*
#error to CFLAGS make variable and rebuild CLISP before coming back here.
#endif

DEFMODULE(pcre,"PCRE");

DEFUN(PCRE::PCRE-VERSION,)
{ VALUES3(asciz_to_string(pcre_version(),GLO(misc_encoding)),
          fixnum(PCRE_MAJOR),fixnum(PCRE_MINOR)); }
DEFUN(PCRE::PCRE-CONFIG,what)
{
  object arg = popSTACK();
 pcre_config_restart:
  if (eq(arg,`:UTF8`)) {
    int ret;
    pcre_config(PCRE_CONFIG_UTF8,&ret);
    VALUES_IF(ret==1);
  } else if (eq(arg,`:NEWLINE`)) {
    int ret;
    pcre_config(PCRE_CONFIG_NEWLINE,&ret);
    VALUES1(int_char(ret));
  } else if (eq(arg,`:LINK-SIZE`)) {
    int ret;
    pcre_config(PCRE_CONFIG_LINK_SIZE,&ret);
    VALUES1(fixnum(ret));
  } else if (eq(arg,`:POSIX-MALLOC-THRESHOLD`)) {
    int ret;
    pcre_config(PCRE_CONFIG_POSIX_MALLOC_THRESHOLD,&ret);
    VALUES1(fixnum(ret));
  } else if (eq(arg,`:MATCH-LIMIT`)) {
    int ret;
    pcre_config(PCRE_CONFIG_MATCH_LIMIT,&ret);
    VALUES1(fixnum(ret));
  } else {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(arg);             /* TYPE-ERROR slot DATUM */
    pushSTACK(`(MEMBER :UTF8 :NEWLINE :LINK-SIZE :POSIX-MALLOC-THRESHOLD :MATCH-LIMIT)`); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: invalid config option ~"));
    arg = value1;
    goto pcre_config_restart;
  }
}

DEFUN(PCRE::PCRE-FREE,fp)
{ /* free the pcre* or pcre_extra* object */
  object fp = popSTACK();
  if (fpointerp(fp) && fp_validp(TheFpointer(fp))) {
    void *datum = TheFpointer(fp)->fp_pointer;
    pcre_free(datum);
    VALUES1(T);
  } else VALUES1(NIL);
}

DEFUN(PCRE:PCRE-COMPILE,string &key :STUDY :IGNORE-CASE :MULTILINE :DOTALL \
      :EXTENDED :ANCHORED :DOLLAR-ENDONLY :EXTRA :NOTBOL :NOTEOL :UNGREADY \
      :NOTEMPTY :NO-AUTO-CAPTURE)
{ /* compile the pattern, return PATTERN struct */
  int options = PCRE_UTF8 |
    (missingp(STACK_(11)) ? 0 : PCRE_CASELESS) |
    (missingp(STACK_10)   ? 0 : PCRE_MULTILINE) |
    (missingp(STACK_9)    ? 0 : PCRE_DOTALL) |
    (missingp(STACK_8)    ? 0 : PCRE_EXTENDED) |
    (missingp(STACK_7)    ? 0 : PCRE_ANCHORED) |
    (missingp(STACK_6)    ? 0 : PCRE_DOLLAR_ENDONLY) |
    (missingp(STACK_5)    ? 0 : PCRE_EXTRA) |
    (missingp(STACK_4)    ? 0 : PCRE_NOTBOL) |
    (missingp(STACK_3)    ? 0 : PCRE_NOTEOL) |
    (missingp(STACK_2)    ? 0 : PCRE_UNGREEDY) |
    (missingp(STACK_1)    ? 0 : PCRE_NOTEMPTY) |
    (missingp(STACK_0)    ? 0 : PCRE_NO_AUTO_CAPTURE);
  bool study = !missingp(STACK_(12));
  const char *error_message;
  int error_offset;
  pcre *compiled_pattern;
  gcv_object_t *string = &STACK_(13), *cmp;
 pcre_compile_restart:
  with_string_0(check_string(*string),Symbol_value(S(utf_8)),pattern, {
      begin_system_call();
      compiled_pattern = pcre_compile(pattern,options,&error_message,
                                      &error_offset,NULL);
      end_system_call();
    });
  if (compiled_pattern == NULL) { /* error */
    pushSTACK(NIL);               /* no PLACE */
    pushSTACK(asciz_to_string(error_message,GLO(misc_encoding)));
    pushSTACK(fixnum(error_offset));
    pushSTACK(*string); pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~(~) at ~: ~"));
    *string = value1;
    goto pcre_compile_restart;
  }
  pushSTACK(allocate_fpointer(compiled_pattern)); cmp = &STACK_0;
  pushSTACK(*cmp); pushSTACK(``PCRE::PCRE-FREE``); funcall(L(finalize),2);
  if (study) {
    pcre_extra *pe;
    begin_system_call();
    pe = pcre_study(compiled_pattern,0,&error_message);
    end_system_call();
    if (error_message != NULL) { /* error */
      STACK_0 = NIL;           /* no PLACE - discard compiled_pattern */
      pushSTACK(asciz_to_string(error_message,GLO(misc_encoding)));
      pushSTACK(*string); pushSTACK(TheSubr(subr_self)->name);
      check_value(error,"~(~): ~");
      *string = value1;
      goto pcre_compile_restart;
    }
    pushSTACK(allocate_fpointer(pe));
  } else pushSTACK(NIL);
  funcall(`PCRE::MAKE-PAT`,2);
  skipSTACK(14);
}

/* can trigger GC */
static void check_pattern (object pat, pcre** compiled_pattern,
                           pcre_extra** study)
{ /* extract compiled pattern and the study results from the PATTERN */
  while(!structurep(pat) ||
        nullp(memq(`PCRE::PATTERN`,TheStructure(pat)->structure_types))) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(pat);             /* TYPE-ERROR slot DATUM */
    pushSTACK(`PCRE::PATTERN`); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(`PCRE::PATTERN`); pushSTACK(pat);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a ~"));
    pat = value1;
  }
  /* FIXME for derived structs! */
  *compiled_pattern = TheFpointer(TheStructure(pat)->recdata[1])->fp_pointer;
  if (nullp(TheStructure(pat)->recdata[2])) *study = NULL;
  else *study = TheFpointer(TheStructure(pat)->recdata[2])->fp_pointer;
}

/* two object should be on STACK for the error message */
nonreturning_function(static, pcre_error, (int status)) {
  pushSTACK(sfixnum(status)); pushSTACK(TheSubr(subr_self)->name);
  switch (status) {
    case PCRE_ERROR_NOMATCH:        fehler(error,"~/~ (~ ~): NOMATCH");
    case PCRE_ERROR_NULL:           fehler(error,"~/~ (~ ~): NULL");
    case PCRE_ERROR_BADOPTION:      fehler(error,"~/~ (~ ~): BADOPTION");
    case PCRE_ERROR_BADMAGIC:       fehler(error,"~/~ (~ ~): BADMAGIC");
    case PCRE_ERROR_UNKNOWN_NODE:   fehler(error,"~/~ (~ ~): UNKNOWN_NODE");
    case PCRE_ERROR_NOMEMORY:       fehler(error,"~/~ (~ ~): NOMEMORY");
    case PCRE_ERROR_NOSUBSTRING:    fehler(error,"~/~ (~ ~): NOSUBSTRING");
    case PCRE_ERROR_MATCHLIMIT:     fehler(error,"~/~ (~ ~): MATCHLIMIT");
    case PCRE_ERROR_CALLOUT:        fehler(error,"~/~ (~ ~): CALLOUT");
    case PCRE_ERROR_BADUTF8:        fehler(error,"~/~ (~ ~): BADUTF8");
    case PCRE_ERROR_BADUTF8_OFFSET: fehler(error,"~/~ (~ ~): BADUTF8_OFFSET");
    default: fehler(error,"~/~ (~ ~): ~");
  }
}


DEFUN(PCRE:PATTERN-INFO,pattern request)
{
  pcre *c_pat;
  pcre_extra *study;
  int status;
  check_pattern(STACK_1,&c_pat,&study);
 pcre_fullinfo_restart:
# define INFO(opt,val,bad)    do {                      \
    begin_system_call();                                \
    status = pcre_fullinfo(c_pat,study,opt,val);        \
    end_system_call();                                  \
    if (status < bad) pcre_error(status);               \
  } while(0)

  if (eq(STACK_0,`:OPTIONS`)) {
    unsigned long int options, nn = 0;
    INFO(PCRE_INFO_OPTIONS,&options,0);
    if (options & PCRE_CASELESS)  { nn++; pushSTACK(`:IGNORE-CASE`); }
    if (options & PCRE_MULTILINE) { nn++; pushSTACK(`:MULTILINE`); }
    if (options & PCRE_DOTALL)    { nn++; pushSTACK(`:DOTALL`); }
    if (options & PCRE_EXTENDED)  { nn++; pushSTACK(`:EXTENDED`); }
    if (options & PCRE_ANCHORED)  { nn++; pushSTACK(`:ANCHORED`); }
    if (options & PCRE_DOLLAR_ENDONLY) { nn++; pushSTACK(`:DOLLAR_ENDONLY`); }
    if (options & PCRE_EXTRA)     { nn++; pushSTACK(`:EXTRA`); }
    if (options & PCRE_NOTBOL)    { nn++; pushSTACK(`:NOTBOL`); }
    if (options & PCRE_NOTEOL)    { nn++; pushSTACK(`:NOTEOL`); }
    if (options & PCRE_UNGREEDY)  { nn++; pushSTACK(`:UNGREEDY`); }
    if (options & PCRE_NOTEMPTY)  { nn++; pushSTACK(`:NOTEMPTY`); }
    if (options & PCRE_UTF8)      { nn++; pushSTACK(`:UTF8`); }
    if (options & PCRE_NO_AUTO_CAPTURE) {nn++; pushSTACK(`:NO_AUTO_CAPTURE`);}
    if (options & PCRE_NO_UTF8_CHECK) { nn++; pushSTACK(`:NO_UTF8_CHECK`); }
    VALUES1(listof(nn));
  } else if (eq(STACK_0,`:SIZE`)) {
    unsigned long int length;
    INFO(PCRE_INFO_SIZE,&length,0);
    VALUES1(fixnum(length));
  } else if (eq(STACK_0,`:CAPTURECOUNT`)) {
    int count;
    INFO(PCRE_INFO_CAPTURECOUNT,&count,0);
    VALUES1(fixnum(count));
  } else if (eq(STACK_0,`:BACKREFMAX`)) {
    int ref;
    INFO(PCRE_INFO_BACKREFMAX,&ref,0);
    VALUES1(fixnum(ref));
  } else if (eq(STACK_0,`:FIRSTBYTE`)) {
    int value;
    INFO(PCRE_INFO_FIRSTBYTE,&value,-2);
    if (status == 0) VALUES1(int_char(value));
    else if (status == -1) VALUES1(`:BOL`);
    else if (status == -2) VALUES1(NIL);
  } else if (eq(STACK_0,`:FIRSTTABLE`)) {
    unsigned char table[256/sizeof(char)];
    VALUES1(allocate_bit_vector(Atype_Bit,256));
    begin_system_call();
    status = pcre_fullinfo(c_pat,study,PCRE_INFO_FIRSTTABLE,&table);
    if (status < 0) { end_system_call(); pcre_error(status); }
    memcpy(TheSbvector(value1)->data,table,sizeof(table));
    end_system_call();
  } else if (eq(STACK_0,`:LASTLITERAL`)) {
    int value;
    INFO(PCRE_INFO_LASTLITERAL,&value,0);
    if (status == 0) VALUES1(int_char(value));
    else VALUES1(NIL);
  } else if (eq(STACK_0,`:NAMEENTRYSIZE`)) {
    int value;
    INFO(PCRE_INFO_NAMEENTRYSIZE,&value,0);
    VALUES1(fixnum(value));
  } else if (eq(STACK_0,`:NAMECOUNT`)) {
    int value;
    INFO(PCRE_INFO_NAMECOUNT,&value,0);
    VALUES1(fixnum(value));
  } else if (eq(STACK_0,`:NAMETABLE`)) {
    int count, size, pos;
    char *table;
    begin_system_call();
    status = pcre_fullinfo(c_pat,study,PCRE_INFO_NAMECOUNT,&count);
    if (status < 0) { end_system_call(); pcre_error(status); }
    status = pcre_fullinfo(c_pat,study,PCRE_INFO_NAMEENTRYSIZE,&size);
    if (status < 0) { end_system_call(); pcre_error(status); }
    status = pcre_fullinfo(c_pat,study,PCRE_INFO_NAMETABLE,&table);
    end_system_call();
    if (status < 0) pcre_error(status);
    for (pos = 0; pos < count; pos++, table+=size) {
      pushSTACK(allocate_cons());
      Car(STACK_0) = asciz_to_string(table+2,GLO(misc_encoding));
      Cdr(STACK_0) = fixnum((table[0]<<1) + table[1]);
    }
    VALUES1(listof(count));
  } else if (eq(STACK_0,`:STUDYSIZE`)) {
    size_t value;
    INFO(PCRE_INFO_STUDYSIZE,&value,0);
    VALUES1(fixnum(value));
  } else {                      /* error */
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(STACK_1); pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~: ~ is not a valid option"));
    STACK_0 = value1;
    goto pcre_fullinfo_restart;
  }
  skipSTACK(2);
}

DEFUN(PCRE:PCRE-NAME-TO-INDEX,pattern name)
{ /* pcre_get_stringnumber() : named substring index in OVECTOR */
  pcre *c_pat;
  pcre_extra *study;
  int index;
  check_pattern(STACK_1,&c_pat,&study);
 restart_pcre_get_stringnumber:
  with_string_0(check_string(STACK_0),GLO(misc_encoding),name, {
      index = pcre_get_stringnumber(c_pat,name);
    });
  if (index>0) VALUES1(fixnum(index));
  else { /* error */
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(STACK_1);         /* name */
    pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~: ~ is not a valid pattern name"));
    STACK_0 = value1;
    goto restart_pcre_get_stringnumber;
  }
  skipSTACK(2);
}

DEFUN(PCRE:PCRE-EXEC,pattern subject &key :BOOLEAN                      \
      :OFFSET :ANCHORED :NOTBOL :NOTEOL :NOTEMPTY)
{ /* match the SUBJECT string against a pre-compiled PATTERN;
     return a vector of MATCH structures or NIL if no matches */
  int options =
    (missingp(STACK_3) ? 0 : PCRE_ANCHORED) |
    (missingp(STACK_2) ? 0 : PCRE_NOTBOL) |
    (missingp(STACK_1) ? 0 : PCRE_NOTEOL) |
    (missingp(STACK_0) ? 0 : PCRE_NOTEMPTY);
  int offset = missingp(STACK_4) ? 0
    : posfixnum_to_L(check_posfixnum(STACK_5));
  bool bool_p = !missingp(STACK_5);
  int *ovector;
  int capture_count, ovector_size, ret;
  pcre *c_pat;
  pcre_extra *study;
  skipSTACK(6); /* drop all options */
  check_pattern(STACK_1,&c_pat,&study);
  begin_system_call();
  ret = pcre_fullinfo(c_pat,study,PCRE_INFO_CAPTURECOUNT,&capture_count);
  end_system_call();
  if (ret < 0) pcre_error(ret);
  ovector_size = 3 * (capture_count + 1);
  ovector = alloca(ovector_size);
  with_string_0(check_string(STACK_0),Symbol_value(S(utf_8)),subject, {
      begin_system_call();
      ret = pcre_exec(c_pat,study,subject,subject_len,offset,options,
                      ovector,ovector_size);
      end_system_call();
    });
  if (ret == PCRE_ERROR_NOMATCH) VALUES1(NIL);
  else if (ret > 0) {
    if (bool_p) VALUES1(T); /* success indicator */
    else { /* return a vector */
      int pos, ov_pos;
      pushSTACK(allocate_vector(ret)); /* return value */
      for (pos = ov_pos = 0; pos < ret; pos++, ov_pos+=2)
        if (ovector[ov_pos] >= 0) {
          pushSTACK(fixnum(ovector[ov_pos]));
          pushSTACK(fixnum(ovector[ov_pos+1]));
          funcall(`PCRE::MAKE-MATCH-BOA`,2);
          TheSvector(STACK_0)->data[pos] = value1;
        }
      VALUES1(popSTACK());
    }
  } else pcre_error(ret);
  skipSTACK(2);                 /* drop pattern & subject */
}
