/*
 * GDBM - The GNU database manager
 * <http://www.gnu.org/software/gdbm/>
 * Copyright (C) 2007  Masayuki Onjo <onjo@lispuser.net>
 * Copyright (C) 2007  Sam Steingold <sds@gnu.org>
 * GPL2
 */

#include "clisp.h"
#include "config.h"

#if defined(HAVE_STRING_H)
# include <string.h>
#endif
#if defined(HAVE_UNISTD_H)
# include <unistd.h>            /* for lseek */
#endif

#if defined(HAVE_GDBM_H)
# include <gdbm.h>
#elif defined(HAVE_GDBM_GDBM_H)
# include <gdbm/gdbm.h>
#else
# error No GDBM headers!
#endif

DEFMODULE(gdbm,"GDBM");

DEFCHECKER(check-gdbm-errno, prefix=GDBM, NO-ERROR MALLOC-ERROR              \
           BLOCK-SIZE-ERROR FILE-OPEN-ERROR FILE-WRITE-ERROR FILE-SEEK-ERROR \
           FILE-READ-ERROR BAD-MAGIC-NUMBER EMPTY-DATABASE CANT-BE-READER    \
           CANT-BE-WRITER READER-CANT-DELETE READER-CANT-STORE               \
           READER-CANT-REORGANIZE UNKNOWN-UPDATE ITEM-NOT-FOUND              \
           REORGANIZE-FAILED CANNOT-REPLACE ILLEGAL-DATA OPT-ALREADY-SET     \
           OPT-ILLEGAL)
nonreturning_function(static, error_gdbm, (char *fatal_message)) {
  end_system_call(); /* in case we are called from _gdbm_fatal() */
  pushSTACK(`GDBM::GDBM-ERROR`);
  pushSTACK(`:MESSAGE`);
  if (fatal_message) {
    pushSTACK(asciz_to_string(fatal_message, GLO(misc_encoding)));
    pushSTACK(`:CODE`); pushSTACK(`:FATAL`);
  } else {
    pushSTACK(safe_to_string(gdbm_strerror(gdbm_errno)));
    pushSTACK(`:CODE`); pushSTACK(check_gdbm_errno_reverse(gdbm_errno));
  }
  pushSTACK(`"~A: ~A"`);
  pushSTACK(TheSubr(subr_self)->name);
  pushSTACK(STACK_4); /* message */
  funcall(L(error_of_type), 8);
  NOTREACHED;
}

DEFUN(GDBM::GDBM-VERSION,)
{ VALUES1(safe_to_string(gdbm_version)); }

#define SYSCALL(statement) begin_system_call(); statement; end_system_call()

DEFCHECKER(gdbm_open_read_write, prefix=GDBM, READER WRITER WRCREAT NEWDB)
DEFCHECKER(gdbm_open_option, prefix=GDBM, SYNC NOLOCK FAST)
#if defined(HAVE_GDBM_OPEN)
DEFUN(GDBM::GDBM-OPEN, name &key :BLOCKSIZE :READ-WRITE :OPTION :MODE)
{
  GDBM_FILE gdbm;
  int mode = check_uint_defaulted(STACK_0, 0644);
  int rw_opt1 = missingp(STACK_1) ? 0 : gdbm_open_option(STACK_1);
  int rw_opt2 = missingp(STACK_2) ? GDBM_WRCREAT
    : gdbm_open_read_write(STACK_2);
  int rw = rw_opt1 | rw_opt2;
  int bsize = check_uint_defaulted(STACK_3, 512);
  object path = STACK_4;

  skipSTACK(5);

  if (!stringp(path)) {
    VALUES1(NIL);
  } else {
    with_string_0(stringp(path) ? (object)path : physical_namestring(path),
                  GLO(pathname_encoding), name, {
        SYSCALL(gdbm = gdbm_open(name, bsize, rw, mode, error_gdbm));
      });
    if (gdbm != NULL) {
      pushSTACK(allocate_fpointer(gdbm));
      funcall(`GDBM::MAKE-GDBM`, 1);
      pushSTACK(value1);        /* save */
      pushSTACK(STACK_0); pushSTACK(``GDBM::GDBM-CLOSE``);
      funcall(L(finalize),2);
      VALUES1(popSTACK());      /* restore */
    } else
      error_gdbm(NULL);
  }
}
#endif  /* HAVE_GDBM_OPEN */

/* can trigger GC */
static GDBM_FILE check_gdbm (object gdbm)
{
  gdbm = check_classname(gdbm, `GDBM::GDBM`);
  return nullp(TheStructure(gdbm)->recdata[1]) ? NULL
    : (GDBM_FILE)TheFpointer(TheStructure(gdbm)->recdata[1])->fp_pointer;
}

#if defined(HAVE_GDBM_CLOSE)
DEFUN(GDBM:GDBM-CLOSE, dbf)
{
  GDBM_FILE dbf = check_gdbm(STACK_0);
  if (dbf) {
    SYSCALL(gdbm_close(dbf));
    TheStructure(STACK_0)->recdata[1] = NIL;
    VALUES1(T);
  } else {
    VALUES1(NIL);
  }
  skipSTACK(1);
}
#endif  /* HAVE_GDBM_CLOSE */

#if defined(HAVE_GDBM_FDESC)
DEFUN(GDBM:GDBM-FILE-SIZE, dbf)
{
  GDBM_FILE dbf = check_gdbm(popSTACK());
  off_t ret;
  begin_system_call();
  ret = handle_length(NIL,gdbm_fdesc(dbf));
  end_system_call();
  VALUES1(off_to_I(ret));
}
#endif  /* HAVE_GDBM_FDESC */

static object coerce_bitvector (object arg) {
  if (bit_vector_p(Atype_8Bit,arg)) return arg;
  else {
    pushSTACK(arg); pushSTACK(GLO(type_uint8_vector));
    funcall(L(coerce),2);
    if (!bit_vector_p(Atype_8Bit,value1)) { NOTREACHED; }
    return value1;
  }
}

#define with_datum(lisp_obj, datum_var, statement)  do {                \
  if (stringp(lisp_obj)) {                                              \
    with_string_0(lisp_obj, GLO(misc_encoding), datum_var##string, {    \
      datum datum_var;                                                  \
      datum_var.dptr = datum_var##string;                               \
      datum_var.dsize = datum_var##string_len;                          \
      SYSCALL(statement);                                               \
    });                                                                 \
  } else if (vectorp(lisp_obj)) {                                       \
    datum datum_var;                                                    \
    lisp_obj = coerce_bitvector(lisp_obj);                              \
    datum_var.dptr = (char*)TheSbvector(lisp_obj)->data;                \
    datum_var.dsize = vector_length(lisp_obj);                          \
    SYSCALL(statement);                                                 \
  } else NOTREACHED;                                                    \
 } while(0)


DEFCHECKER(gdbm_store_flag, prefix=GDBM, REPLACE INSERT);
#if defined(HAVE_GDBM_STORE)
DEFUN(GDBM:GDBM-STORE, dbf key content &key FLAG)
{
  GDBM_FILE dbf = check_gdbm(STACK_3);
  int flag = missingp(STACK_0) ? GDBM_REPLACE : gdbm_store_flag(STACK_0);
  object content_obj = STACK_1;
  object key_obj = STACK_2;
  int binary_p=0, string_p = stringp(content_obj);
  skipSTACK(4);

  if (!string_p) {
    content_obj = coerce_bitvector(check_vector(content_obj));
    binary_p = 1;
  }

  if (dbf && (string_p || binary_p)) {
    int status;
    with_datum(key_obj, key,
               with_datum(content_obj, content,
                          status = gdbm_store(dbf, key, content, flag)));
    VALUES_IF(!status);
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_STORE */

/* convert datum to Lisp string and release memory in datum
 can trigger GC */
static object datum_to_object (datum d, int binary) {
  if (d.dptr == NULL) return NIL;
  else if (binary==0) {
    object o = n_char_to_string(d.dptr, d.dsize, GLO(misc_encoding));
    free(d.dptr);
    return o;
  } else {
    object o = allocate_bit_vector(Atype_8Bit,d.dsize);
    int i = 0;
    for (i=0;i<d.dsize;i++)
      TheSbvector(o)->data[i] = d.dptr[i];
    free(d.dptr);
    return o;
  }
}

#if defined(HAVE_GDBM_FETCH)
DEFUN(GDBM:GDBM-FETCH, dbf key &key BINARY)
{
  GDBM_FILE dbf = check_gdbm(STACK_2);
  object binary = popSTACK();
  int binary_p = !missingp(binary);
  object key_obj = popSTACK();
  skipSTACK(1);                 /* drop dbf */

  if (dbf) {
    datum res;
    with_datum(key_obj, key, res = gdbm_fetch(dbf,key));
    VALUES1(datum_to_object(res,binary_p));
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_FETCH */

#if defined(HAVE_GDBM_DELETE)
DEFUN(GDBM:GDBM-DELETE, dbf key)
{
  GDBM_FILE dbf = check_gdbm(STACK_1);
  object key_obj = popSTACK();
  skipSTACK(1);                 /* drop dbf */

  if (dbf) {
    int status;
    with_datum(key_obj, key, status = gdbm_delete(dbf,key));
    VALUES_IF(status != -1);
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_DELETE */

#if defined(HAVE_GDBM_FIRSTKEY)
DEFUN(GDBM:GDBM-FIRSTKEY, dbf &key BINARY)
{
  int binary_p = !missingp(STACK_0);
  GDBM_FILE dbf = check_gdbm(STACK_1);

  skipSTACK(2);

  if (dbf) {
    datum res;
    SYSCALL(res = gdbm_firstkey(dbf));
    VALUES1(datum_to_object(res,binary_p));
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_FIRSTKEY */

#if defined(HAVE_GDBM_NEXTKEY)
DEFUN(GDBM:GDBM-NEXTKEY, dbf key &key BINARY)
{
  int binary_p = !missingp(STACK_0);
  object key_obj = STACK_1;
  GDBM_FILE dbf = check_gdbm(STACK_2);
  skipSTACK(3);                 /* drop dbf */

  if (dbf) {
    datum res;
    with_datum(key_obj, key, res = gdbm_nextkey(dbf,key));
    VALUES1(datum_to_object(res,binary_p));
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_NEXTKEY */

#define CHECK_RUN(statement)  do {              \
    int status;                                 \
    SYSCALL(status = statement);                \
    if (status == -1) error_gdbm(NULL);         \
    else VALUES1(T);                            \
  } while(0)

#if defined(HAVE_GDBM_REORGANIZE)
DEFUN(GDBM:GDBM-REORGANIZE, dbf)
{
  GDBM_FILE dbf = check_gdbm(popSTACK());

  if (dbf) {
    CHECK_RUN(gdbm_reorganize(dbf));
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_REORGANIZE */

#if defined(HAVE_GDBM_SYNC)
DEFUN(GDBM:GDBM-SYNC, dbf)
{
  GDBM_FILE dbf = check_gdbm(popSTACK());

  if (dbf) {
    SYSCALL(gdbm_sync(dbf));
    VALUES1(T);
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_SYNC */

#if defined(HAVE_GDBM_EXISTS)
DEFUN(GDBM:GDBM-EXISTS, dbf key)
{
  GDBM_FILE dbf = check_gdbm(STACK_1);
  object key_obj = popSTACK();
  skipSTACK(1);                 /* drop dbf */

  if (dbf) {
    int status;
    with_datum(key_obj, key, status = gdbm_exists(dbf, key));
    VALUES_IF(status);
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_EXISTS */

DEFCHECKER(gdbm_setopt_option, prefix=GDBM, CACHESIZE FASTMODE SYNCMODE \
           CENTFREE COALESCEBLKS)
#if defined(HAVE_GDBM_SETOPT)
DEFUN(GDBM:GDBM-SETOPT, dbf option value)
{
  GDBM_FILE dbf = check_gdbm(STACK_2);
  int option = gdbm_setopt_option(STACK_1);
  object value = STACK_0;
  skipSTACK(3);

  if (dbf) {
    int v;
    switch (option) {
      case GDBM_CACHESIZE:
        v = I_to_sint(check_sint(value)); break;
      case GDBM_FASTMODE: case GDBM_SYNCMODE:
      case GDBM_CENTFREE: case GDBM_COALESCEBLKS:
        v = nullp(value) ? 0 : 1; break;
      default: NOTREACHED;
    }
    CHECK_RUN(gdbm_setopt(dbf, option, &v, sizeof(int)));
  } else
    VALUES1(NIL);
}
#endif  /* HAVE_GDBM_SETOPT */
