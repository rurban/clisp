/*
 * CLISP: Berkeley-DB <http://www.sleepycat.com/docs/api_c/>
 * Copyright (C) 2003-2004 by Sam Steingold
 */

/* have to undefing UNICODE _here_ because clisp.h will #include <windows.h> */
#undef UNICODE
#include "clisp.h"

#ifndef FOREIGN
#error "Berkeley-DB requires CLISP FOREIGN CPP macro"
#endif

#include "config.h"

#if defined(__CYGWIN__)
# define UNIX_CYGWIN32
#endif

#if defined(_WIN32) || defined(UNIX_CYGWIN32)
# define WIN32_LEAN_AND_MEAN  /* avoid including junk */
# if defined(UNIX_CYGWIN32) || defined(__MINGW32__)
/* `unused' is used in function declarations. */
#  undef unused
#  define ULONGLONG OS_ULONGLONG
#  define ULONG OS_ULONG
#  include <windows.h>
#  undef ULONG
#  undef ULONGLONG
#  define unused (void)
# else
#  undef unused
#  include <windows.h>
#  define unused
# endif
#endif

#include <string.h>             /* for memset() */

/* #define DEBUG */
#if defined(DEBUG)
# include <stdio.h>
extern object nobject_out (FILE* stream, object obj);
# define XOUT(obj,label)                                                \
  (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),     \
   obj=nobject_out(stdout,obj), printf("\n"))
#else
# undef OBJECT_OUT
# define OBJECT_OUT(o,l)
# define XOUT(o,l)
#endif

#include <db.h>

DEFMODULE(bdb,"BDB")

DEFUN(BDB:DB-VERSION,)
{ /* Berkeley-DB version */
  int major, minor, patch;
  char * version;
  begin_system_call();
  version = db_version(&major,&minor,&patch);
  end_system_call();
  value1 = asciz_to_string(version,GLO(misc_encoding));
  value2 = fixnum(major);
  value3 = fixnum(minor);
  value4 = fixnum(patch);
  mv_count = 4;
}

nonreturning_function(static, error_bdb, (int status, char *caller)) {
  end_system_call();
  pushSTACK(asciz_to_string(db_strerror(status),GLO(misc_encoding)));
  pushSTACK(asciz_to_string(caller,GLO(misc_encoding)));
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,"~S (~S): ~S");
}
#define SYSCALL(caller,args)     do {                           \
    int db_error_code;                                          \
    begin_system_call();                                        \
    db_error_code = caller args;                                \
    if (db_error_code) error_bdb(db_error_code,#caller);        \
    end_system_call();                                          \
  } while(0)

/* check whether the OBJ has type TYPE and return its handle
 can trigger GC */
static void** object_handle_ (object obj, object type, bool null_on_error) {
  while (!typep_classname(obj,type)) {
    if (null_on_error) return NULL;
    pushSTACK(type);            /* save */
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(type);            /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(type); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1; type = popSTACK(); /* restore */
  }
  return &(TheFpointer(*(TheStructure(obj)->recdata+1))->fp_pointer); /* FIXME for derived structs! */
}
static inline void* object_handle (object obj, object type, bool null_on_error)
{
  void ** ret = object_handle_(obj,type,null_on_error);
  if (ret == NULL) return NULL;
  return *ret;
}

/* allocate a wrapper for the pointer and add a finalizer to it
 can trigger GC */
static void wrap_finalize (void* pointer, gcv_object_t* maker,
                           gcv_object_t* closer) {
  pushSTACK(allocate_fpointer(pointer)); funcall(*maker,1);
  pushSTACK(value1);            /* save for VALUES */
  pushSTACK(value1); pushSTACK(*closer); funcall(L(finalize),2);
  VALUES1(popSTACK());
}

/* ===== Database Environment ===== */
/* not exported:
 DB_ENV->err	Error message with error string
 DB_ENV->errx	Error message
*/

DEFCHECKER(env_encryption_check, DB_ENCRYPT_AES)
/* set the password to perform encryption and decryption.
 can trigger GC */
static void env_set_encryption (DB_ENV *dbe, gcv_object_t *o_flags_,
                                gcv_object_t *o_password_) {
  u_int32_t flags = env_encryption_check(*o_flags_);
  *o_password_ = check_string(*o_password_);
  with_string_0(o_password_,GLO(misc_encoding),password,
                { SYSCALL(dbe->set_encrypt,(dbe,password,flags)); });
}

DEFUN(BDB:ENV-CREATE,&key :PASSWORD :ENCRYPT    \
      :HOST :CLIENT_TIMEOUT :SERVER_TIMEOUT)
{ /* Create an environment handle */
  DB_ENV *dbe, *dbe_cl;
  bool remote_p = boundp(STACK_2); /* host ==> remote */
  int status, cl_timeout = 0, sv_timeout = 0;
 #if defined(DB_RPCCLIENT)      /* 4.2 and later */
  SYSCALL(db_env_create,(&dbe,remote_p ? DB_RPCCLIENT : 0));
 #elif defined(DB_CLIENT)       /* 4.1 and before */
  SYSCALL(db_env_create,(&dbe,remote_p ? DB_CLIENT : 0));
 #else
  #error "how does your Berkeley DB create a remote client?"
 #endif
  if (remote_p) {
    if (posfixnump(STACK_0)) sv_timeout = posfixnum_to_L(STACK_0);
    if (posfixnump(STACK_1)) cl_timeout = posfixnum_to_L(STACK_1);
   host_restart:
    if (stringp(STACK_2)) {     /* string host */
      with_string_0(STACK_2,GLO(misc_encoding),hostz, {
          begin_system_call();
          status = dbe->set_rpc_server(dbe,NULL,hostz,cl_timeout,sv_timeout,0);
          end_system_call();
        });
    } else if ((dbe_cl = object_handle(STACK_2,`BDB::ENV`,true))) {
      /* reuse client */
      begin_system_call();
      status = dbe->set_rpc_server(dbe,dbe_cl->cl_handle,NULL,
                                   cl_timeout,sv_timeout,0);
      end_system_call();
    } else {                    /* bad host */
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(STACK_(2+1));   /* TYPE-ERROR slot DATUM */
      pushSTACK(`(OR STRING BDB::ENV)`); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(STACK_2);                /* host */
      pushSTACK(`BDB::ENV`); pushSTACK(S(string)); pushSTACK(`:HOST`);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,GETTEXT("~S: ~S should be a ~S or a ~S, not ~S"));
      STACK_2 = value1;
      goto host_restart;
    }
    if (status) error_bdb(status,"set_rpc_server");
  }
  if (!missingp(STACK_4))       /* :PASSWD */
    env_set_encryption(dbe,&STACK_3,&STACK_4);
  skipSTACK(5);
  wrap_finalize(dbe,&`BDB::MKENV`,&``BDB::ENV-CLOSE``);
}

DEFUN(BDB:ENV-CLOSE, dbe)
{ /* close DB environment */
  DB_ENV **dbe = (DB_ENV**)object_handle_(popSTACK(),`BDB::ENV`,false);
  if (*dbe) {
    SYSCALL((*dbe)->close,(*dbe,0));
    *dbe = NULL;
    VALUES1(T);
  } else VALUES1(NIL);
}

DEFUN(BDB:ENV-DBREMOVE, dbe file database &key :TRANSACTION :AUTO_COMMIT)
{ /* remove DATABASE from FILE or the whole FILE */
  u_int32_t flags = (missingp(STACK_0) ? 0 : DB_AUTO_COMMIT);
  DB_TXN *txn = object_handle(STACK_1,`BDB::TXN`,true);
  DB_ENV *dbe = object_handle(STACK_4,`BDB::ENV`,false);
  if (!nullp(STACK_2)) STACK_2 = check_string(STACK_2);
  STACK_3 = check_string(STACK_3);
  with_string_0(STACK_3,GLO(misc_encoding),file, {
      if (stringp(STACK_2)) {
        with_string_0(STACK_2,GLO(misc_encoding),database, {
            SYSCALL(dbe->dbremove,(dbe,txn,file,database,flags));
          });
      } else SYSCALL(dbe->dbremove,(dbe,txn,file,NULL,flags));
    });
  VALUES0; skipSTACK(5);
}

DEFUN(BDB:ENV-DBRENAME, dbe file database newname       \
      &key :TRANSACTION :AUTO_COMMIT)
{ /* rename DATABASE to NEWNAME in FILE */
  u_int32_t flags = (missingp(STACK_0) ? 0 : DB_AUTO_COMMIT);
  DB_TXN *txn = object_handle(STACK_1,`BDB::TXN`,true);
  DB_ENV *dbe = object_handle(STACK_5,`BDB::ENV`,false);
  with_string_0(check_string(STACK_4),GLO(misc_encoding),file, {
      with_string_0(check_string(STACK_3),GLO(misc_encoding),database, {
          with_string_0(check_string(STACK_2),GLO(misc_encoding),newname, {
              SYSCALL(dbe->dbrename,(dbe,txn,file,database,newname,flags));
            });
        });
    });
  VALUES0; skipSTACK(6);
}

DEFFLAGSET(env_open_flags, DB_JOINENV DB_INIT_CDB DB_INIT_LOCK DB_INIT_LOG \
           DB_INIT_MPOOL DB_INIT_TXN DB_RECOVER DB_RECOVER_FATAL        \
           DB_USE_ENVIRON DB_USE_ENVIRON_ROOT DB_CREATE DB_LOCKDOWN     \
           DB_PRIVATE DB_SYSTEM_MEM DB_THREAD)
DEFUN(BDB:ENV-OPEN, dbe &key :HOME :JOINENV :INIT_CDB :INIT_LOCK :INIT_LOG \
      :INIT_MPOOL :INIT_TXN :RECOVER :RECOVER_FATAL :USE_ENVIRON        \
      :USE_ENVIRON_ROOT :CREATE :LOCKDOWN :PRIVATE :SYSTEM_MEM :THREAD :MODE)
{ /* open DB environment */
  int mode = posfixnum_default(popSTACK());
  u_int32_t flags = env_open_flags();
  DB_ENV *dbe = object_handle(STACK_1,`BDB::ENV`,false);
  if (!missingp(STACK_0)) STACK_0 = check_string(STACK_0);
  if (stringp(STACK_0)) {
    with_string_0(STACK_0,GLO(misc_encoding),home,
                  { SYSCALL(dbe->open,(dbe,home,flags,mode)); });
  } else SYSCALL(dbe->open,(dbe,NULL,flags,mode));
  VALUES0; skipSTACK(2);
}

DEFFLAGSET(env_remove_flags, DB_FORCE DB_USE_ENVIRON DB_USE_ENVIRON_ROOT)
DEFUN(BDB:ENV-REMOVE, dbe &key :HOME :FORCE :USE_ENVIRON :USE_ENVIRON_ROOT)
{ /* destroy an environment */
  u_int32_t flags = env_remove_flags();
  DB_ENV *dbe = object_handle(STACK_1,`BDB::ENV`,false);
  if (!missingp(STACK_0)) STACK_0 = check_string(STACK_0);
  if (stringp(STACK_0)) {
    with_string_0(STACK_0,GLO(misc_encoding),home,
                  { SYSCALL(dbe->remove,(dbe,home,flags)); });
  } else SYSCALL(dbe->remove,(dbe,NULL,flags));
  VALUES0; skipSTACK(2);
}

/* ===== Environment Configuration ===== */

/* not exported:
 DB_ENV->set_app_dispatch	Configure application recovery interface
 DB_ENV->set_alloc	Set local space allocation functions
 DB_ENV->set_encrypt	Set the environment cryptographic key [See ENV-CREATE]
 DB_ENV->set_errcall	Set error message callback
 DB_ENV->set_errfile	Set error message FILE
 DB_ENV->set_errpfx	Set error message prefix
 DB_ENV->set_feedback	Set feedback callback
 DB_ENV->set_paniccall	Set panic callback
 DB_ENV->set_rpc_server	Establish an RPC server connection [See ENV-CREATE]
*/

static void set_flags (object arg, u_int32_t *flag_on, u_int32_t *flag_off,
                       u_int32_t values) {
  if (boundp(arg))
    *(nullp(arg) ? flag_off : flag_on) |= values;
}
static void set_verbose (DB_ENV *dbe, object arg, u_int32_t flag) {
  if (boundp(arg)) SYSCALL(dbe->set_verbose,(dbe,flag,!nullp(arg)));
}

DEFUN(BDB:ENV-SET-OPTIONS, dbe &key :PASSWORD :ENCRYPT                  \
      :LOCK_TIMEOUT :TXN_TIMEOUT                                        \
      :SHM_KEY :TAS_SPINS :TX_TIMESTAMP :TX_MAX :DATA_DIR :TMP_DIR      \
      :AUTO_COMMIT :CDB_ALLDB :DIRECT_DB :DIRECT_LOG :NOLOCKING         \
      :NOMMAP :NOPANIC :OVERWRITE :PANIC_ENVIRONMENT :REGION_INIT       \
      :TXN_NOSYNC :TXN_WRITE_NOSYNC :YIELDCPU                           \
      :VERB_CHKPOINT :VERB_DEADLOCK :VERB_RECOVERY :VERB_REPLICATION    \
      :VERB_WAITSFOR :VERBOSE)
{ /* set many options */
  DB_ENV *dbe = object_handle(STACK_(23),`BDB::ENV`,false);
  { /* verbose */
    object verbosep = popSTACK(); /* :VERBOSE - all */
    set_verbose(dbe,verbosep,DB_VERB_WAITSFOR);
    set_verbose(dbe,verbosep,DB_VERB_REPLICATION);
    set_verbose(dbe,verbosep,DB_VERB_RECOVERY);
    set_verbose(dbe,verbosep,DB_VERB_DEADLOCK);
    set_verbose(dbe,verbosep,DB_VERB_CHKPOINT);
    set_verbose(dbe,popSTACK(),DB_VERB_WAITSFOR);
    set_verbose(dbe,popSTACK(),DB_VERB_REPLICATION);
    set_verbose(dbe,popSTACK(),DB_VERB_RECOVERY);
    set_verbose(dbe,popSTACK(),DB_VERB_DEADLOCK);
    set_verbose(dbe,popSTACK(),DB_VERB_CHKPOINT);
  }
  { /* flags */
    u_int32_t flags_on = 0, flags_off = 0;
    set_flags(popSTACK(),&flags_on,&flags_off,DB_YIELDCPU);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_TXN_WRITE_NOSYNC);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_TXN_NOSYNC);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_REGION_INIT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_PANIC_ENVIRONMENT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_OVERWRITE);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_NOPANIC);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_NOMMAP);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_NOLOCKING);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DIRECT_LOG);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DIRECT_DB);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_CDB_ALLDB);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_AUTO_COMMIT);
    if (flags_off) SYSCALL(dbe->set_flags,(dbe,flags_off,0));
    if (flags_on)  SYSCALL(dbe->set_flags,(dbe,flags_on,1));
  }
  if (!missingp(STACK_0)) {     /* TMP_DIR */
    with_string_0(check_string(popSTACK()),GLO(misc_encoding),tmp_dir,
                  { SYSCALL(dbe->set_tmp_dir,(dbe,tmp_dir)); });
  } else skipSTACK(1);
  if (!missingp(STACK_0)) {     /* DATA_DIR */
    with_string_0(check_string(popSTACK()),GLO(misc_encoding),data_dir,
                  { SYSCALL(dbe->set_data_dir,(dbe,data_dir)); });
  } else skipSTACK(1);
  if (!missingp(STACK_0)) {     /* TX_MAX */
    u_int32_t tx_max = posfixnum_to_L(check_posfixnum(STACK_0));
    SYSCALL(dbe->set_tx_max,(dbe,tx_max));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* TX_TIMESTAMP */
    time_t timestamp;
    convert_time_from_universal(STACK_0,&timestamp);
    SYSCALL(dbe->set_tx_timestamp,(dbe,&timestamp));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* TAS_SPINS */
    u_int32_t tas_spins = posfixnum_to_L(check_posfixnum(STACK_0));
    SYSCALL(dbe->set_tas_spins,(dbe,tas_spins));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* SHM_KEY */
    long shm_key = posfixnum_to_L(check_posfixnum(STACK_0));
    SYSCALL(dbe->set_shm_key,(dbe,shm_key));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* TXN_TIMEOUT */
    db_timeout_t timeout = posfixnum_to_L(check_posfixnum(STACK_0));
    SYSCALL(dbe->set_timeout,(dbe,timeout,DB_SET_TXN_TIMEOUT));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* LOCK_TIMEOUT */
    db_timeout_t timeout = posfixnum_to_L(check_posfixnum(STACK_0));
    SYSCALL(dbe->set_timeout,(dbe,timeout,DB_SET_LOCK_TIMEOUT));
  }
  skipSTACK(1);
  if (!missingp(STACK_1))       /* PASSWORD */
    env_set_encryption(dbe,&STACK_0,&STACK_1);
  skipSTACK(2);
  VALUES0; skipSTACK(1);        /* skip dbe */
}

/* get the list of verbosity options
 can trigger GC */
static object env_get_verbose (DB_ENV *dbe) {
  int count = 0, onoffp;
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_WAITSFOR,&onoffp));
  if (onoffp) { pushSTACK(`:VERB_WAITSFOR`); count++; }
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_REPLICATION,&onoffp));
  if (onoffp) { pushSTACK(`:VERB_REPLICATION`); count++;}
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_RECOVERY,&onoffp));
  if (onoffp) { pushSTACK(`:VERB_RECOVERY`); count++; }
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_DEADLOCK,&onoffp));
  if (onoffp) { pushSTACK(`:VERB_DEADLOCK`); count++; }
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_CHKPOINT,&onoffp));
  if (onoffp) { pushSTACK(`:VERB_CHKPOINT`); count++; }
  return listof(count);
}
/* get the tmp directory
 can trigger GC */
static object env_get_tmp_dir (DB_ENV *dbe) {
  const char *dir;
  SYSCALL(dbe->get_tmp_dir,(dbe,&dir));
  return dir ? asciz_to_string(dir,GLO(pathname_encoding)) : NIL;
}
/* get the data directory list
 can trigger GC */
static object env_get_data_dirs (DB_ENV *dbe) {
  const char **dirs; int ii;
  SYSCALL(dbe->get_data_dirs,(dbe,&dirs));
  if (dirs) {
    for (ii=0; dirs[ii]; ii++)
      pushSTACK(asciz_to_string(dirs[ii],GLO(pathname_encoding)));
    return listof(ii);
  } else return NIL;
}
/* get the max number of transactions */
static object env_get_tx_max (DB_ENV *dbe) {
  u_int32_t tx_max;
  SYSCALL(dbe->get_tx_max,(dbe,&tx_max));
  return fixnum(tx_max);
}
/* get the transaction timestamp
 can trigger GC */
static object env_get_tx_timestamp (DB_ENV *dbe) {
  time_t tx_timestamp;
  SYSCALL(dbe->get_tx_timestamp,(dbe,&tx_timestamp));
  return convert_time_to_universal(&tx_timestamp);
}
/* get the home directory
   return T when DBE is not yet open and a list otherwise
 can trigger GC */
static object env_get_home_dir (DB_ENV *dbe) {
  const char *home;
  int status;
  begin_system_call();
  status = dbe->get_home(dbe,&home);
  end_system_call();
  if (status) return T;
  if (home == NULL) return NIL;
  return asciz_to_string(home,GLO(pathname_encoding));
}
/* get the open flags
   return T when DBE is not yet open and a list otherwise
 can trigger GC */
static object env_get_open_flags (DB_ENV *dbe) {
  u_int32_t flags, count=0, status;
  begin_system_call();
  status = dbe->get_open_flags(dbe,&flags);
  end_system_call();
  if (status) return T;
  if (flags & DB_JOINENV) { pushSTACK(`:JOINENV`); count++; }
  if (flags & DB_INIT_CDB) { pushSTACK(`:INIT_CDB`); count++; }
  if (flags & DB_INIT_LOCK) { pushSTACK(`:INIT_LOCK`); count++; }
  if (flags & DB_INIT_LOG) { pushSTACK(`:INIT_LOG`); count++; }
  if (flags & DB_INIT_MPOOL) { pushSTACK(`:INIT_MPOOL`); count++; }
  if (flags & DB_INIT_TXN) { pushSTACK(`:INIT_TXN`); count++; }
  if (flags & DB_RECOVER) { pushSTACK(`:RECOVER`); count++; }
  if (flags & DB_RECOVER_FATAL) { pushSTACK(`:RECOVER_FATAL`); count++; }
  if (flags & DB_USE_ENVIRON) { pushSTACK(`:USE_ENVIRON`); count++; }
  if (flags & DB_USE_ENVIRON_ROOT) { pushSTACK(`:USE_ENVIRON_ROOT`); count++; }
  if (flags & DB_CREATE) { pushSTACK(`:CREATE`); count++; }
  if (flags & DB_LOCKDOWN) { pushSTACK(`:LOCKDOWN`); count++; }
  if (flags & DB_PRIVATE) { pushSTACK(`:PRIVATE`); count++; }
  if (flags & DB_SYSTEM_MEM) { pushSTACK(`:SYSTEM_MEM`); count++; }
  if (flags & DB_THREAD) { pushSTACK(`:THREAD`); count++; }
  return listof(count);
}
/* get the flags
 can trigger GC */
static object env_get_flags (DB_ENV *dbe) {
  u_int32_t count = 0, flags;
  SYSCALL(dbe->get_flags,(dbe,&flags));
  if (flags & DB_YIELDCPU) { pushSTACK(`:YIELDCPU`); count++; }
  if (flags & DB_TXN_WRITE_NOSYNC) { pushSTACK(`:TXN_WRITE_NOSYNC`);count++; }
  if (flags & DB_TXN_NOSYNC) { pushSTACK(`:TXN_NOSYNC`); count++; }
  if (flags & DB_REGION_INIT) { pushSTACK(`:REGION_INIT`); count++; }
  if (flags & DB_PANIC_ENVIRONMENT) {pushSTACK(`:PANIC_ENVIRONMENT`);count++;}
  if (flags & DB_OVERWRITE) { pushSTACK(`:OVERWRITE`); count++; }
  if (flags & DB_NOPANIC) { pushSTACK(`:NOPANIC`); count++; }
  if (flags & DB_NOMMAP) { pushSTACK(`:NOMMAP`); count++; }
  if (flags & DB_NOLOCKING) { pushSTACK(`:NOLOCKING`); count++; }
  if (flags & DB_DIRECT_LOG) { pushSTACK(`:DIRECT_LOG`); count++; }
  if (flags & DB_CDB_ALLDB) { pushSTACK(`:CDB_ALLDB`); count++; }
  if (flags & DB_AUTO_COMMIT) { pushSTACK(`:AUTO_COMMIT`); count++; }
  SYSCALL(dbe->get_encrypt_flags,(dbe,&flags));
  switch (flags) {
    case DB_ENCRYPT_AES: pushSTACK(`:ENCRYPT_AES`); count++; break;
    case 0: break;
    default: NOTREACHED;
  }
  return listof(count);
}
/* get test-and-set spin count */
static object env_get_tas_spins (DB_ENV *dbe) {
  u_int32_t tas_spins;
  SYSCALL(dbe->get_tas_spins,(dbe,&tas_spins));
  return fixnum(tas_spins);
}
/* get base segment ID for shared memory regions */
static object env_get_shm_key (DB_ENV *dbe) {
  long shm_key;
  SYSCALL(dbe->get_shm_key,(dbe,&shm_key));
  return fixnum(shm_key);
}
/* get timeout values for locks or transactions in the database environment */
static object env_get_timeout (DB_ENV *dbe, u_int32_t which) {
  db_timeout_t timeout;
  SYSCALL(dbe->get_timeout,(dbe,&timeout,which));
  return UL_to_I(timeout);
}
/* both timeouts as a list
 can trigger GC */
static object env_get_timeouts (DB_ENV *dbe) {
  pushSTACK(env_get_timeout(dbe,DB_SET_LOCK_TIMEOUT));
  pushSTACK(env_get_timeout(dbe,DB_SET_TXN_TIMEOUT));
  return listof(2);
}
DEFUNR(BDB:ENV-GET-OPTIONS, dbe &optional what) {
  object what = popSTACK();
  /* dbe may be NULL only for DB_XIDDATASIZE */
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,eq(what,`:DB_XIDDATASIZE`));
 restart_ENV_GET_OPTIONS:
  if (missingp(what)) {         /* get everything */
    value1 = env_get_verbose(dbe); pushSTACK(value1);
    value1 = env_get_flags(dbe); pushSTACK(value1);
    pushSTACK(env_get_tx_timestamp(dbe));
    pushSTACK(env_get_tx_max(dbe));
    pushSTACK(env_get_tmp_dir(dbe));
    value1 = env_get_data_dirs(dbe); pushSTACK(value1);
    pushSTACK(env_get_tas_spins(dbe));
    pushSTACK(env_get_shm_key(dbe));
    value1 = env_get_timeouts(dbe); pushSTACK(value1);
    pushSTACK(env_get_home_dir(dbe));
    value1 = env_get_open_flags(dbe); pushSTACK(value1);
    funcall(L(values),11);
  } else if (eq(what,S(Kverbose))) {
    VALUES1(env_get_verbose(dbe));
  } else if (eq(what,`:FLAGS`)) {
    VALUES1(env_get_flags(dbe));
  } else if (eq(what,`:VERB_WAITSFOR`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_WAITSFOR,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB_REPLICATION`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_REPLICATION,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB_RECOVERY`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_RECOVERY,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB_DEADLOCK`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_DEADLOCK,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB_CHKPOINT`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_CHKPOINT,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:YIELDCPU`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_YIELDCPU);
  } else if (eq(what,`:TXN_WRITE_NOSYNC`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_TXN_WRITE_NOSYNC);
  } else if (eq(what,`:TXN_NOSYNC`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_TXN_NOSYNC);
  } else if (eq(what,`:REGION_INIT`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_REGION_INIT);
  } else if (eq(what,`:PANIC_ENVIRONMENT`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_PANIC_ENVIRONMENT);
  } else if (eq(what,`:OVERWRITE`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_OVERWRITE);
  } else if (eq(what,`:NOPANIC`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_NOPANIC);
  } else if (eq(what,`:NOMMAP`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_NOMMAP);
  } else if (eq(what,`:NOLOCKING`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_NOLOCKING);
  } else if (eq(what,`:DIRECT_LOG`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_DIRECT_LOG);
  } else if (eq(what,`:CDB_ALLDB`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_CDB_ALLDB);
  } else if (eq(what,`:AUTO_COMMIT`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_flags,(dbe,&flags));
    VALUES_IF(flags & DB_AUTO_COMMIT);
  } else if (eq(what,`:TX_TIMESTAMP`)) {
    VALUES1(env_get_tx_timestamp(dbe));
  } else if (eq(what,`:TX_MAX`)) {
    VALUES1(env_get_tx_max(dbe));
  } else if (eq(what,`:DATA_DIR`)) {
    VALUES1(env_get_data_dirs(dbe));
  } else if (eq(what,`:TMP_DIR`)) {
    VALUES1(env_get_tmp_dir(dbe));
  } else if (eq(what,`:TAS_SPINS`)) {
    VALUES1(env_get_tas_spins(dbe));
  } else if (eq(what,`:SHM_KEY`)) {
    VALUES1(env_get_shm_key(dbe));
  } else if (eq(what,`:LOCK_TIMEOUT`)) {
    VALUES1(env_get_timeout(dbe,DB_SET_LOCK_TIMEOUT));
  } else if (eq(what,`:TXN_TIMEOUT`)) {
    VALUES1(env_get_timeout(dbe,DB_SET_TXN_TIMEOUT));
  } else if (eq(what,`:TIMEOUT`)) {
    VALUES1(env_get_timeouts(dbe));
  } else if (eq(what,`:ENCRYPT`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_encrypt_flags,(dbe,&flags));
    switch (flags) {
      case DB_ENCRYPT_AES: VALUES1(`:ENCRYPT_AES`);
      case 0: VALUES1(NIL);
      default: NOTREACHED;
    }
  } else if (eq(what,`:DB_XIDDATASIZE`)) {
    VALUES1(fixnum(DB_XIDDATASIZE));
  } else if (eq(what,`:HOME`)) {
    VALUES1(env_get_home_dir(dbe));
  } else if (eq(what,`:OPEN`)) {
    VALUES1(env_get_open_flags(dbe));
  } else {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(what); pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~S: invalid argument ~S"));
    what = value1;
    goto restart_ENV_GET_OPTIONS;
  }
}

/* ===== Database Operations ===== */

/* not exported:
 DB->associate	Associate a secondary index
 DB->err	Error message with error string
 DB->errx	Error message
 DB->join	Perform a database join on cursors
 DB->key_range	Return estimate of key location
 DB->verify	Verify/salvage a database
*/

DEFUN(BDB:DB-CREATE, dbe &key :XA)
{ /* create database */
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_XA_CREATE;
  DB_ENV *dbe = object_handle(STACK_1,`BDB::ENV`,true);
  DB *db;
  SYSCALL(db_create,(&db,dbe,flags));
  skipSTACK(2);
  wrap_finalize(db,&`BDB::MKDB`,&``BDB::DB-CLOSE``);
}

DEFUN(BDB:DB-CLOSE, db &key :NOSYNC)
{ /* Close a database */
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_NOSYNC;
  DB **db = (DB**)object_handle_(STACK_1,`BDB::DB`,false);
  if (*db) {
    SYSCALL((*db)->close,(*db,flags));
    *db = NULL;
    VALUES1(T);
  } else VALUES1(NIL);
  skipSTACK(2);
}

/* zero-out DBT and set allocation */
static void init_dbt (DBT* p_dbt , u_int32_t flags) {
  begin_system_call(); memset(p_dbt,0,sizeof(DBT)); end_system_call();
  p_dbt->flags = flags;
}

/* ensure that the return value is a byte vector of specified length
 can trigger GC */
static object check_byte_vector (object obj, int length) {
  while (!bit_vector_p(Atype_8Bit,obj)
         && (length<0 || length!=vector_length(obj))) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(GLO(type_uint8_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(GLO(type_uint8_vector)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    if (length >= 0) {
      pushSTACK(fixnum(length));
      check_value(type_error,GETTEXT("~S: ~S is not a vector of type ~S and length ~S"));
    } else check_value(type_error,GETTEXT("~S: ~S is not a vector of type ~S"));
    obj = value1;
  }
  return obj;
}

/* fill a DBT with contents of obj (a byte vector)
 can trigger GC */
static void fill_dbt (object obj, DBT* key)
{
  unsigned long idx = 0;
  obj = check_byte_vector(obj,-1);
  init_dbt(key,DB_DBT_USERMEM);
  key->ulen = key->size = vector_length(obj);
  obj = array_displace_check(obj,key->size,&idx);
  key->data = TheSbvector(obj)->data + idx;
}

/* convert a DBT to a byte vector
 can trigger GC */
static object dbt_to_vector (DBT *p_dbt)
{
  object vec;
  if (p_dbt->data == NULL) return NIL;
  vec = allocate_bit_vector(Atype_8Bit,p_dbt->size);
  begin_system_call();
  memcpy(TheSbvector(vec)->data,p_dbt->data,p_dbt->size);
  free(p_dbt->data);
  end_system_call();
  return vec;
}


DEFUN(BDB:DB-DEL, dbe key &key :TRANSACTION :AUTO_COMMIT)
{ /* Delete items from a database */
  u_int32_t flags = (missingp(STACK_0) ? 0 : DB_AUTO_COMMIT);
  DB_TXN *txn = object_handle(STACK_1,`BDB::TXN`,true);
  DB *db = object_handle(STACK_3,`BDB::DB`,false);
  DBT key;
  fill_dbt(STACK_2,&key);
  SYSCALL(db->del,(db,txn,&key,flags));
  skipSTACK(4);
  VALUES0;
}

DEFUN(BDB:DB-FD, db)
{ /* Return a file descriptor from a database */
  DB *db = object_handle(popSTACK(),`BDB::DB`,false);
  int fd;
  SYSCALL(db->fd,(db,&fd));
  VALUES1(fixnum(fd));
}

DEFFLAGSET(db_get_flags,  DB_CONSUME DB_CONSUME_WAIT DB_DIRTY_READ DB_RMW)
DEFUN(BDB:DB-GET, db key &key :CONSUME :CONSUME_WAIT :DIRTY_READ :RMW \
      :TRANSACTION :ERROR)
{ /* Get items from a database */
  int no_error = nullp(popSTACK());
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = db_get_flags();
  DB *db = object_handle(STACK_1,`BDB::DB`,false);
  DBT key, val;
  int status;
  fill_dbt(STACK_0,&key);
  init_dbt(&val,DB_DBT_MALLOC);
  skipSTACK(2);
  begin_system_call();
  status = db->get(db,txn,&key,&val,flags);
  end_system_call();
  if (status) {
    if (no_error) {
      switch (status) {
        case DB_NOTFOUND: VALUES1(`:NOTFOUND`); return;
        case DB_KEYEMPTY: VALUES1(`:KEYEMPTY`); return;
      }
    }
    error_bdb(status,"db->get");
  }
  VALUES1(dbt_to_vector(&val));
}

DEFUN(BDB:DB-STAT, db &key :FAST_STAT)
{ /* Return database statistics */
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_FAST_STAT;
  DB *db = object_handle(STACK_1,`BDB::DB`,false);
  int swapped_p;
  DBTYPE db_type;
  unsigned int count = 0;
  SYSCALL(db->get_byteswapped,(db,&swapped_p));
  pushSTACK(swapped_p ? T : NIL); count++;
  SYSCALL(db->get_type,(db,&db_type));
  switch (db_type) {
#define STAT_SLOT(slot)        pushSTACK(UL_to_I(slot)); count++
#define STAT_SLOT_FAST(slot)   pushSTACK(flags ? NIL : UL_to_I(slot)); count++
    case DB_HASH: {
      DB_HASH_STAT *hash_stat;
      SYSCALL(db->stat,(db,&hash_stat,flags));
      STAT_SLOT(hash_stat->hash_magic);
      STAT_SLOT(hash_stat->hash_version);
      STAT_SLOT(hash_stat->hash_nkeys);
      STAT_SLOT(hash_stat->hash_ndata);
      STAT_SLOT(hash_stat->hash_pagesize);
      STAT_SLOT(hash_stat->hash_ffactor);
      STAT_SLOT(hash_stat->hash_buckets);
      STAT_SLOT_FAST(hash_stat->hash_free);
      STAT_SLOT_FAST(hash_stat->hash_bfree);
      STAT_SLOT_FAST(hash_stat->hash_bigpages);
      STAT_SLOT_FAST(hash_stat->hash_big_bfree);
      STAT_SLOT_FAST(hash_stat->hash_overflows);
      STAT_SLOT_FAST(hash_stat->hash_ovfl_free);
      STAT_SLOT_FAST(hash_stat->hash_dup);
      STAT_SLOT_FAST(hash_stat->hash_dup_free);
      funcall(`BDB::MKDBSTAT-HASH`,count);
      free(hash_stat);
    } break;
    case DB_BTREE: case DB_RECNO: {
      DB_BTREE_STAT *btree_stat;
      SYSCALL(db->stat,(db,&btree_stat,flags));
      STAT_SLOT(btree_stat->bt_magic);
      STAT_SLOT(btree_stat->bt_version);
      STAT_SLOT(btree_stat->bt_nkeys);
      STAT_SLOT(btree_stat->bt_ndata);
      STAT_SLOT(btree_stat->bt_pagesize);
      STAT_SLOT(btree_stat->bt_minkey);
      STAT_SLOT(btree_stat->bt_re_len);
      STAT_SLOT(btree_stat->bt_re_pad);
      STAT_SLOT_FAST(btree_stat->bt_levels);
      STAT_SLOT_FAST(btree_stat->bt_int_pg);
      STAT_SLOT_FAST(btree_stat->bt_leaf_pg);
      STAT_SLOT_FAST(btree_stat->bt_dup_pg);
      STAT_SLOT_FAST(btree_stat->bt_free);
      STAT_SLOT_FAST(btree_stat->bt_int_pgfree);
      STAT_SLOT_FAST(btree_stat->bt_leaf_pgfree);
      STAT_SLOT_FAST(btree_stat->bt_dup_pgfree);
      STAT_SLOT_FAST(btree_stat->bt_over_pgfree);
      funcall(`BDB::MKDBSTAT-BTREE`,count);
      free(btree_stat);
    } break;
    case DB_QUEUE: {
      DB_QUEUE_STAT *queue_stat;
      SYSCALL(db->stat,(db,&queue_stat,flags));
      STAT_SLOT(queue_stat->qs_magic);
      STAT_SLOT(queue_stat->qs_version);
      STAT_SLOT(queue_stat->qs_nkeys);
      STAT_SLOT(queue_stat->qs_ndata);
      STAT_SLOT(queue_stat->qs_pagesize);
      STAT_SLOT(queue_stat->qs_extentsize);
      STAT_SLOT_FAST(queue_stat->qs_pages);
      STAT_SLOT(queue_stat->qs_re_len);
      STAT_SLOT(queue_stat->qs_re_pad);
      STAT_SLOT_FAST(queue_stat->qs_pgfree);
      STAT_SLOT(queue_stat->qs_first_recno);
      STAT_SLOT(queue_stat->qs_cur_recno);
      funcall(`BDB::MKDBSTAT-QUEUE`,count);
      free(queue_stat);
    } break;
    default: NOTREACHED;
#undef STAT_SLOT
#undef STAT_SLOT_FAST
  }
  skipSTACK(2);
}

/* check that type is a valid DBTYPE
 can trigger GC */
static DBTYPE check_dbtype (object type) {
 restart_check_dbtype:
  if (eq(type,`:BTREE`)) return DB_BTREE;
  if (eq(type,`:HASH`)) return DB_HASH;
  if (eq(type,`:QUEUE`)) return DB_QUEUE;
  if (eq(type,`:RECNO`)) return DB_RECNO;
  if (missingp(type) || eq(type,`:UNKNOWN`)) return DB_UNKNOWN;
  pushSTACK(NIL);               /* no PLACE */
  pushSTACK(type);              /* TYPE-ERROR slot DATUM */
  pushSTACK(`(MEMBER :BTREE :HASH :QUEUE :RECNO :UNKNOWN)`); /*EXPECTED-TYPE*/
  pushSTACK(`:UNKNOWN`); pushSTACK(`:RECNO`); pushSTACK(`:QUEUE`);
  pushSTACK(`:HASH`); pushSTACK(`:BTREE`); pushSTACK(type);
  pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,GETTEXT("~S: ~S should be one of ~S, ~S, ~S, ~S or ~S"));
  type = value1;
  goto restart_check_dbtype;
}

DEFFLAGSET(db_open_flags, DB_CREATE DB_DIRTY_READ DB_EXCL DB_NOMMAP \
           DB_RDONLY DB_THREAD DB_TRUNCATE DB_AUTO_COMMIT)
DEFUN(BDB:DB-OPEN, db file &key :DATABASE :TYPE :MODE :CREATE :DIRTY_READ \
      :EXCL :NOMMAP :RDONLY :THREAD :TRUNCATE :AUTO_COMMIT :TRANSACTION)
{ /* Open a database */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = db_open_flags();
  int mode = posfixnum_default2(popSTACK(),0644);
  DBTYPE db_type = check_dbtype(popSTACK());
  DB *db = object_handle(STACK_2,`BDB::DB`,false);
  with_string_0(check_string(STACK_1),GLO(misc_encoding),file, {
      if (missingp(STACK_0)) {  /* no :DATABASE */
        SYSCALL(db->open,(db,txn,file,NULL,db_type,flags,mode));
      } else {                  /* multiple databases in one file */
        with_string_0(check_string(STACK_0),GLO(misc_encoding),databse, {
            SYSCALL(db->open,(db,txn,file,databse,db_type,flags,mode));
          });
      }
    });
  VALUES0;
  skipSTACK(3);
}

DEFUN(BDB:DB-SYNC, db)
{ /* Flush a database to stable storage */
  DB *db = object_handle(popSTACK(),`BDB::DB`,false);
  SYSCALL(db->sync,(db,0));
  VALUES0;
}

DEFUN(BDB:DB-TRUNCATE, db &key :TRANSACTION :AUTO_COMMIT)
{ /* Empty a database */
  u_int32_t flags = (missingp(STACK_0) ? 0 : DB_AUTO_COMMIT);
  DB_TXN *txn = object_handle(STACK_1,`BDB::TXN`,true);
  DB *db = object_handle(STACK_2,`BDB::DB`,false);
  u_int32_t count;
  SYSCALL(db->truncate,(db,txn,&count,flags));
  VALUES1(UL_to_I(count)); skipSTACK(3);
}

DEFUN(BDB:DB-UPGRADE, db file &key :DUPSORT)
{ /* Upgrade a database */
  u_int32_t flags = (missingp(STACK_0) ? 0 : DB_DUPSORT);
  DB *db = object_handle(STACK_2,`BDB::DB`,false);
  with_string_0(check_string(STACK_1),GLO(misc_encoding),file, {
      SYSCALL(db->upgrade,(db,file,flags));
    });
  VALUES0; skipSTACK(3);
}

DEFUN(BDB:DB-RENAME, db file database newname)
{ /* Rename a database */
  DB *db = object_handle(STACK_3,`BDB::DB`,false);
  with_string_0(check_string(STACK_2),GLO(misc_encoding),file, {
      with_string_0(check_string(STACK_1),GLO(misc_encoding),database, {
          with_string_0(check_string(STACK_0),GLO(misc_encoding),newname, {
              SYSCALL(db->rename,(db,file,database,newname,0));
            });
        });
    });
  VALUES0; skipSTACK(4);
}

DEFUN(BDB:DB-REMOVE, db file database)
{ /* Remove a database */
  DB *db = object_handle(STACK_2,`BDB::DB`,false);
  with_string_0(check_string(STACK_1),GLO(misc_encoding),file, {
      with_string_0(check_string(STACK_0),GLO(misc_encoding),database, {
          SYSCALL(db->remove,(db,file,database,0));
        });
    });
  VALUES0; skipSTACK(3);
}

DEFCHECKER(db_put_flag, DB_APPEND DB_NODUPDATA DB_NOOVERWRITE)
DEFUN(BDB:DB-PUT, db key val &key :AUTO_COMMIT :FLAG :TRANSACTION)
{ /* Store items into a database */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = db_put_flag(popSTACK());
  DB *db = object_handle(STACK_3,`BDB::DB`,false);
  DBT key, val;
  if (!missingp(STACK_0)) flags |= DB_AUTO_COMMIT;
  skipSTACK(1);
  fill_dbt(STACK_0,&val);
  fill_dbt(STACK_1,&key);
  SYSCALL(db->put,(db,txn,&key,&val,flags));
  VALUES0; skipSTACK(3);
}

/* ===== cursors ===== */
DEFFLAGSET(make_cursor_flags, DB_DIRTY_READ DB_WRITECURSOR)
DEFUN(BDB:MAKE-CURSOR,db &key :DIRTY_READ :WRITECURSOR :TRANSACTION)
{ /* create a cursor */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = make_cursor_flags();
  DB *db = object_handle(popSTACK(),`BDB::DB`,false);
  DBC *cursor;
  SYSCALL(db->cursor,(db,txn,&cursor,flags));
  wrap_finalize(cursor,&`BDB::MKCURSOR`,&``BDB::CURSOR-CLOSE``);
}

DEFUN(BDB:CURSOR-CLOSE, cursor)
{ /* close a cursor */
  DBC **cursor = (DBC**)object_handle_(STACK_1,`BDB::CURSOR`,false);
  if (*cursor) {
    SYSCALL((*cursor)->c_close,(*cursor));
    *cursor = NULL;
    VALUES1(T);
  } else VALUES1(NIL);
  skipSTACK(1);
}

DEFUN(BDB:CURSOR-COUNT, cursor)
{ /* return a count of the number of data items for the key to which
     the cursor refers */
  DBC *cursor = object_handle(popSTACK(),`BDB::CURSOR`,false);
  db_recno_t count;
  SYSCALL(cursor->c_count,(cursor,&count,0));
  VALUES1(UL_to_I(count));
}

DEFUN(BDB:CURSOR-DEL, cursor)
{ /* delete the key/data pair to which the cursor refers */
  DBC *cursor = object_handle(popSTACK(),`BDB::CURSOR`,false);
  SYSCALL(cursor->c_del,(cursor,0));
  VALUES0;
}

DEFFLAGSET(cursor_dup_flags, DB_POSITION)
DEFUN(BDB:CURSOR-DUP, cursor &key :POSITION)
{ /* create a new cursor that uses the same transaction and locker ID as
     the original cursor */
  u_int32_t flags = cursor_dup_flags();
  DBC *cursor = object_handle(popSTACK(),`BDB::CURSOR`,false);
  DBC *new_cursor;
  SYSCALL(cursor->c_dup,(cursor,&new_cursor,flags));
  wrap_finalize(cursor,&`BDB::MKCURSOR`,&``BDB::CURSOR-CLOSE``);
}

DEFCHECKER(cursor_get_flag, DB_CURRENT DB_FIRST DB_GET_BOTH            \
           DB_GET_BOTH_RANGE DB_GET_RECNO DB_JOIN_ITEM DB_LAST DB_NEXT \
           DB_NEXT_DUP DB_NEXT_NODUP DB_PREV DB_PREV_NODUP DB_SET      \
           DB_SET_RANGE DB_SET_RECNO DB_DIRTY_READ DB_MULTIPLE)
DEFUN(BDB:CURSOR-GET, cursor key data flag &key :ERROR)
{ /* retrieve key/data pairs from the database */
  int no_error = nullp(popSTACK());
  u_int32_t flag = cursor_get_flag(popSTACK());
  DBC *cursor = object_handle(STACK_2,`BDB::CURSOR`,false);
  DBT key, val;
  int status;
  if (!nullp(STACK_1)) fill_dbt(STACK_1,&key);
  else init_dbt(&key,DB_DBT_MALLOC);
  if (!nullp(STACK_0)) fill_dbt(STACK_0,&val);
  else init_dbt(&val,DB_DBT_MALLOC);
  skipSTACK(3);
  begin_system_call();
  status = cursor->c_get(cursor,&key,&val,flag);
  end_system_call();
  if (status) {
    if (no_error) {
      switch (status) {
        case DB_NOTFOUND: VALUES1(`:NOTFOUND`); return;
        case DB_KEYEMPTY: VALUES1(`:KEYEMPTY`); return;
      }
    }
    error_bdb(status,"cursor->c_get");
  }
  pushSTACK(dbt_to_vector(&key));
  value2 = dbt_to_vector(&val);
  value1 = popSTACK();
  mv_count = 2;
}

DEFCHECKER(cursor_put_flag, DB_AFTER DB_BEFORE DB_CURRENT DB_KEYFIRST \
           DB_KEYLAST DB_NODUPDATA)
DEFUN(BDB:CURSOR-PUT, cursor key data flag)
{ /* retrieve key/data pairs from the database */
  u_int32_t flag = cursor_put_flag(popSTACK());
  DBC *cursor = object_handle(STACK_2,`BDB::CURSOR`,false);
  DBT key, val;
  fill_dbt(STACK_1,&key);
  fill_dbt(STACK_0,&val);
  SYSCALL(cursor->c_put,(cursor,&key,&val,flag));
  skipSTACK(3);
  VALUES0;
}

/* ===== transactions ===== */

DEFFLAGSET(txn_begin_flags, DB_DIRTY_READ DB_TXN_NOSYNC \
           DB_TXN_NOWAIT DB_TXN_SYNC)
DEFUN(BDB:TXN-BEGIN, dbe &key :PARENT :DIRTY_READ :NOSYNC :NOWAIT :SYNC)
{ /* create a transaction */
  u_int32_t flags = txn_begin_flags();
  DB_TXN *parent = object_handle(popSTACK(),`BDB::TXN`,true), *ret;
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,false);
  SYSCALL(dbe->txn_begin,(dbe,parent,&ret,flags));
  pushSTACK(allocate_fpointer(ret));
  funcall(`BDB::MKTXN`,1);
}

DEFUN(BDB:TXN-ABORT, txn)
{ /* Abort a transaction */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,false);
  SYSCALL(txn->abort,(txn));
  VALUES0;
}

DEFFLAGSET(txn_commit_flags, DB_TXN_NOSYNC DB_TXN_SYNC)
DEFUN(BDB:TXN-COMMIT, txn &key :NOSYNC :SYNC)
{ /* Commit a transaction */
  u_int32_t flags = txn_commit_flags();
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,false);
  SYSCALL(txn->commit,(txn,flags));
  VALUES0;
}

DEFUN(BDB:TXN-DISCARD, txn)
{ /* Discard a transaction */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,false);
  SYSCALL(txn->discard,(txn,0));
  VALUES0;
}

DEFUN(BDB:TXN-ID, txn)
{ /* Return the transaction's ID */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,false);
  u_int32_t id;
  begin_system_call(); id = txn->id(txn); end_system_call();
  VALUES1(UL_to_I(id));
}

DEFFLAGSET(txn_checkpoint_flags, DB_FORCE)
DEFUN(BDB:TXN-CHECKPOINT, dbe &key :KBYTE :MIN :FORCE)
{ /* flush the underlying memory pool, write a checkpoint record to the
     log, and then flush the log. */
  u_int32_t flags = txn_checkpoint_flags();
  u_int32_t min = posfixnum_default(popSTACK());
  u_int32_t kbyte = posfixnum_default(popSTACK());
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,false);
  SYSCALL(dbe->txn_checkpoint,(dbe,kbyte,min,flags));
  VALUES0;
}

/* return the pointer into the obj (which must be
   a (vector (unsigned-byte 8) DB_XIDDATASIZE))
 can trigger GC, the return value is invalidated by GC */
static u_int8_t* check_gid (gcv_object_t *obj_) {
  unsigned long idx;
  object data_vector;
  *obj_ = check_byte_vector(*obj_,DB_XIDDATASIZE);
  data_vector = array_displace_check(*obj_,DB_XIDDATASIZE,&idx);
  return TheSbvector(data_vector)->data+idx;
}

DEFUN(BDB:TXN-PREPARE, txn gid)
{ /* initiate the beginning of a two-phase commit */
  DB_TXN *txn = object_handle(STACK_1,`BDB::TXN`,false);
  u_int8_t *gid = check_gid(&STACK_0);
  SYSCALL(txn->prepare,(txn,gid));
  VALUES0; skipSTACK(2);
}

/* allocate a (vector (unsigned-byte 8) DB_XIDDATASIZE) for this gid
 can trigger GC */
static object gid_to_vector (u_int8_t gid[DB_XIDDATASIZE]) {
  object vec = allocate_bit_vector(Atype_8Bit,DB_XIDDATASIZE);
  begin_system_call();
  memcpy(TheSbvector(vec)->data,gid,DB_XIDDATASIZE);
  end_system_call();
  return vec;
}

DEFFLAGSET(txn_recover_flags, DB_FIRST DB_NEXT)
DEFUN(BDB:TXN-RECOVER, dbe &key :FIRST :NEXT)
{ /* return a list of prepared but not yet resolved transactions */
  u_int32_t flags = txn_recover_flags();
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,false);
  u_int32_t tx_max;
  DB_PREPLIST *preplist;
  int status, ii;
  long retnum;
  SYSCALL(dbe->get_tx_max,(dbe,&tx_max));
  preplist = my_malloc(tx_max * sizeof(DB_PREPLIST));
  begin_system_call();
  status = dbe->txn_recover(dbe,preplist,tx_max,&retnum,flags);
  if (status) {
    free(preplist); end_system_call();
    error_bdb(status,"dbe->txn_recover");
  }
  end_system_call();
  for (ii=0; ii<retnum; ii++) {
    pushSTACK(allocate_fpointer(preplist[ii].txn));
    funcall(`BDB::MKTXN`,1); pushSTACK(value1);
    pushSTACK(gid_to_vector(preplist[ii].gid));
    value1 = allocate_cons();
    Cdr(value1) = popSTACK();   /* gid */
    Car(value1) = popSTACK();   /* txn */
    pushSTACK(value1);          /* (TXN . GID) */
  }
  VALUES1(listof(retnum));
}

DEFCHECKER(txn_timeout_check, DB_SET_LOCK_TIMEOUT DB_SET_TXN_TIMEOUT)
DEFUN(BDB:TXN-SET-TIMEOUT, txn timeout which)
{ /* set timeout values for locks or transactions for the specified
     transaction */
  u_int32_t which = txn_timeout_check(popSTACK());
  db_timeout_t timeout = I_to_UL(check_uint32(popSTACK()));
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,false);
  SYSCALL(txn->set_timeout,(txn,timeout,which));
  VALUES0;
}

DEFFLAGSET(txn_stat_flags, DB_STAT_CLEAR)
DEFUN(BDB:TXN-STAT, dbe &key :STAT_CLEAR)
{ /* transaction subsystem statistics */
  u_int32_t flags = txn_stat_flags();
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,false);
  DB_TXN_STAT *stat;
  SYSCALL(dbe->txn_stat,(dbe,&stat,flags));
  pushSTACK(UL_to_I(stat->st_last_ckp.file));
  pushSTACK(UL_to_I(stat->st_last_ckp.offset));
  funcall(`BDB::MKLSM`,2); pushSTACK(value1);
  pushSTACK(convert_time_to_universal(&(stat->st_time_ckp)));
  pushSTACK(UL_to_I(stat->st_last_txnid));
  pushSTACK(UL_to_I(stat->st_maxtxns));
  pushSTACK(UL_to_I(stat->st_nactive));
  pushSTACK(UL_to_I(stat->st_maxnactive));
  pushSTACK(UL_to_I(stat->st_nbegins));
  pushSTACK(UL_to_I(stat->st_naborts));
  pushSTACK(UL_to_I(stat->st_ncommits));
  pushSTACK(UL_to_I(stat->st_nrestores));
  pushSTACK(UL_to_I(stat->st_regsize));
  pushSTACK(UL_to_I(stat->st_region_wait));
  pushSTACK(UL_to_I(stat->st_region_nowait));
  { /* txnarray */
    int ii, size = stat->st_nactive;
    DB_TXN_ACTIVE *txn_active = stat->st_txnarray;
    for (ii=0; ii<size; ii++) {
      pushSTACK(UL_to_I(txn_active->txnid));
      pushSTACK(UL_to_I(txn_active->parentid));
      pushSTACK(UL_to_I(txn_active->lsn.file));
      pushSTACK(UL_to_I(txn_active->lsn.offset));
      funcall(`BDB::MKLSM`,2); pushSTACK(value1);
      pushSTACK(UL_to_I(txn_active->xa_status));
      pushSTACK(gid_to_vector(txn_active->xid));
      funcall(`BDB::MKTXNACTIVE`,5); pushSTACK(value1);
    }
    value1 = vectorof(size); pushSTACK(value1);
  }
  funcall(`BDB::MKTXNSTAT`,14);
  free(stat);
}
