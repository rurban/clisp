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
static void* object_handle (object obj, object type, bool null_on_error) {
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
  return TheFpointer(*(TheStructure(obj)->recdata+1))->fp_pointer; /* FIXME for derived structs! */
}

/* ===== Database Environment ===== */
/* not exported:
 DB_ENV->err	Error message with error string
 DB_ENV->errx	Error message
*/

DEFUN(BDB:ENV-CREATE,&key :PASSWORD :ENCRYPT    \
      :HOST :CLIENT-TIMEOUT :SERVER-TIMEOUT)
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
  if (!missingp(STACK_4)) { /* :PASSWD */
    u_int32_t flags = missingp(STACK_3) ? 0 : DB_ENCRYPT_AES;
    with_string_0(check_string(STACK_4),GLO(misc_encoding),password,
                  { SYSCALL(dbe->set_encrypt,(dbe,password,flags)); });
  }
  skipSTACK(5);
  pushSTACK(allocate_fpointer(dbe));
  funcall(`BDB::MKENV`,1);
}

DEFUN(BDB:ENV-CLOSE, dbe)
{ /* close DB environment */
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,false);
  SYSCALL(dbe->close,(dbe,0));
  VALUES0;
}

DEFUN(BDB:ENV-DBREMOVE, dbe file database &key :TRANSACTION :AUTO-COMMIT)
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
      &key :TRANSACTION :AUTO-COMMIT)
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
DEFUN(BDB:ENV-OPEN, dbe &key :HOME :JOIN :INIT-CDB :INIT-LOCK :INIT-LOG \
      :INIT-MPOOL :INIT-TXN :RECOVER :RECOVER-FATAL :USE-ENVIRON        \
      :USE-ENVIRON-ROOT :CREATE :LOCKDOWN :PRIVATE :SYSTEM-MEM :THREAD :MODE)
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
DEFUN(BDB:ENV-REMOVE, dbe &key :HOME :FORCE :USE-ENVIRON :USE-ENVIRON-ROOT)
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
 DB_ENV->set_shm_key	Set system memory shared segment ID
 DB_ENV->set_tas_spins	Set the number of test-and-set spins
*/

static void set_flags (object arg, u_int32_t *flag_on, u_int32_t *flag_off,
                       u_int32_t values) {
  if (boundp(arg))
    *(nullp(arg) ? flag_off : flag_on) |= values;
}

DEFUN(BDB:ENV-SET-OPTIONS, dbe &key :DATA_DIR :TMP_DIR                  \
      :AUTO_COMMIT :CDB_ALLDB :DIRECT_DB :DIRECT_LOG :NOLOCKING         \
      :NOMMAP :NOPANIC :OVERWRITE :PANIC_ENVIRONMENT :REGION_INIT       \
      :TXN_NOSYNC :TXN_WRITE_NOSYNC :YIELDCPU                           \
      :VERB_CHKPOINT :VERB_DEADLOCK :VERB_RECOVERY :VERB_REPLICATION    \
      :VERB_WAITSFOR :VERBOSE)
{ /* set many options */
  u_int32_t flags_on = 0, flags_off = 0;
  DB_ENV *dbe = object_handle(STACK_(21),`BDB::ENV`,false);
  /* verbose */
  set_flags(popSTACK(),&flags_on,&flags_off, /* :VERBOSE - all */
            DB_VERB_WAITSFOR | DB_VERB_REPLICATION | DB_VERB_RECOVERY
            | DB_VERB_DEADLOCK | DB_VERB_CHKPOINT);
  set_flags(popSTACK(),&flags_on,&flags_off,DB_VERB_WAITSFOR);
  set_flags(popSTACK(),&flags_on,&flags_off,DB_VERB_REPLICATION);
  set_flags(popSTACK(),&flags_on,&flags_off,DB_VERB_RECOVERY);
  set_flags(popSTACK(),&flags_on,&flags_off,DB_VERB_DEADLOCK);
  set_flags(popSTACK(),&flags_on,&flags_off,DB_VERB_CHKPOINT);
  if (flags_off) SYSCALL(dbe->set_verbose,(dbe,flags_off,0));
  if (flags_on)  SYSCALL(dbe->set_verbose,(dbe,flags_on,1));
  /* flags */
  flags_on = flags_off = 0;
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
  /* tmp-dir */
  if (!missingp(STACK_0)) {
    with_string_0(check_string(popSTACK()),GLO(misc_encoding),tmp_dir,
                  { SYSCALL(dbe->set_tmp_dir,(dbe,tmp_dir)); });
  } else skipSTACK(1);
  /* data-dir */
  if (!missingp(STACK_0)) {
    with_string_0(check_string(popSTACK()),GLO(misc_encoding),data_dir,
                  { SYSCALL(dbe->set_data_dir,(dbe,data_dir)); });
  } else skipSTACK(1);
  VALUES0; skipSTACK(1);        /* skip dbe */
}

/* get the list of verbosity options
 can trigger GC */
static object env_verbose (DB_ENV *dbe) {
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
static object env_tmp_dir (DB_ENV *dbe) {
  const char *dir;
  SYSCALL(dbe->get_tmp_dir,(dbe,&dir));
  return dir ? asciz_to_string(dir,GLO(pathname_encoding)) : NIL;
}
/* get the data directory list
 can trigger GC */
static object env_data_dirs (DB_ENV *dbe) {
  const char **dirs; int ii;
  SYSCALL(dbe->get_data_dirs,(dbe,&dirs));
  if (dirs) {
    for (ii=0; dirs[ii]; ii++)
      pushSTACK(asciz_to_string(dirs[ii],GLO(pathname_encoding)));
    return listof(ii);
  } else return NIL;
}

DEFUNR(BDB:ENV-GET-OPTIONS, dbe &optional what) {
  object what = popSTACK();
  DB_ENV *dbe = object_handle(popSTACK(),`BDB::ENV`,false);
 restart_ENV_GET_OPTIONS:
  if (missingp(what)) {         /* get everything */
    /* verbose */
    value1 = env_verbose(dbe); pushSTACK(value1); /* save */
    { /* flags */
      u_int32_t count = 0, flags;
      SYSCALL(dbe->get_flags,(dbe,&flags));
      if (flags & DB_YIELDCPU) { pushSTACK(`:YIELDCPU`); count++; }
      if (flags & DB_TXN_WRITE_NOSYNC) {pushSTACK(`:TXN_WRITE_NOSYNC`);count++;}
      if (flags & DB_TXN_NOSYNC) { pushSTACK(`:TXN_NOSYNC`); count++; }
      if (flags & DB_REGION_INIT) { pushSTACK(`:REGION_INIT`); count++; }
      if (flags &DB_PANIC_ENVIRONMENT){pushSTACK(`:PANIC_ENVIRONMENT`);count++;}
      if (flags & DB_OVERWRITE) { pushSTACK(`:OVERWRITE`); count++; }
      if (flags & DB_NOPANIC) { pushSTACK(`:NOPANIC`); count++; }
      if (flags & DB_NOMMAP) { pushSTACK(`:NOMMAP`); count++; }
      if (flags & DB_NOLOCKING) { pushSTACK(`:NOLOCKING`); count++; }
      if (flags & DB_DIRECT_LOG) { pushSTACK(`:DIRECT_LOG`); count++; }
      if (flags & DB_CDB_ALLDB) { pushSTACK(`:CDB_ALLDB`); count++; }
      if (flags & DB_AUTO_COMMIT) { pushSTACK(`:AUTO_COMMIT`); count++; }
      value1 = listof(count); pushSTACK(value1); /* save */
      pushSTACK(fixnum(flags));                  /* raw flags too! */
    }
    /* tmp-dir */
    pushSTACK(env_tmp_dir(dbe));
    /* data-dir */
    value1 = env_data_dirs(dbe); pushSTACK(value1);
    funcall(L(values),5);
  } else if (eq(what,S(Kverbose))) {
    VALUES1(env_verbose(dbe));
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
  } else if (eq(what,`:DATA_DIR`)) {
    VALUES1(env_data_dirs(dbe));
  } else if (eq(what,`:TMP_DIR`)) {
    VALUES1(env_tmp_dir(dbe));
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
  DB_ENV *dbe = object_handle(STACK_1,`BDB::ENV`,false);
  DB *db;
  SYSCALL(db_create,(&db,dbe,flags));
  skipSTACK(2);
  pushSTACK(allocate_fpointer(db));
  funcall(`MKDB`,1);
}

DEFUN(BDB:DB-CLOSE, dbe &key :NOSYNC)
{ /* Close a database */
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_NOSYNC;
  DB *db = object_handle(STACK_1,`BDB::DB`,false);
  SYSCALL(db->close,(db,flags));
  skipSTACK(2);
  VALUES0;
}

/* zero-out DBT and set allocation */
static void init_dbt (DBT* p_dbt , u_int32_t flags) {
  begin_system_call(); memset(p_dbt,0,sizeof(DBT)); end_system_call();
  p_dbt->flags = flags;
}

/* fill a DBT with contents of obj (a byte vector)
 can trigger GC */
static void fill_dbt (object obj, DBT* key)
{
  unsigned long idx = 0;
  while (!bit_vector_p(Atype_8Bit,obj)) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(GLO(type_uint8_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(GLO(type_uint8_vector)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a vector of type ~S"));
    obj = value1;
  }
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


DEFUN(BDB:DB-DEL, dbe key &key :TRANSACTION :AUTO-COMMIT)
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
  DB *db = object_handle(STACK_3,`BDB::DB`,false);
  int fd;
  SYSCALL(db->fd,(db,&fd));
  skipSTACK(1);
  VALUES1(fixnum(fd));
}

DEFFLAGSET(db_get_flags,  DB_CONSUME DB_CONSUME_WAIT DB_DIRTY_READ DB_RMW)
DEFUN(BDB:DB-GET, db key &key :CONSUME :CONSUME-WAIT :DIRTY-READ :RMW \
      :TRANSACTION)
{ /* Get items from a database */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = db_get_flags();
  DB *db = object_handle(STACK_1,`BDB::DB`,false);
  DBT key, val;
  fill_dbt(STACK_0,&key);
  init_dbt(&val,DB_DBT_MALLOC);
  SYSCALL(db->get,(db,txn,&key,&val,flags));
  VALUES1(dbt_to_vector(&val)); skipSTACK(2);
}

DEFUN(BDB:DB-STAT, db &key :FAST-STAT)
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
      DB_HASH_STAT hash_stat;
      SYSCALL(db->stat,(db,&hash_stat,flags));
      STAT_SLOT(hash_stat.hash_magic);
      STAT_SLOT(hash_stat.hash_version);
      STAT_SLOT(hash_stat.hash_nkeys);
      STAT_SLOT(hash_stat.hash_ndata);
      STAT_SLOT(hash_stat.hash_pagesize);
      STAT_SLOT(hash_stat.hash_ffactor);
      STAT_SLOT(hash_stat.hash_buckets);
      STAT_SLOT_FAST(hash_stat.hash_free);
      STAT_SLOT_FAST(hash_stat.hash_bfree);
      STAT_SLOT_FAST(hash_stat.hash_bigpages);
      STAT_SLOT_FAST(hash_stat.hash_big_bfree);
      STAT_SLOT_FAST(hash_stat.hash_overflows);
      STAT_SLOT_FAST(hash_stat.hash_ovfl_free);
      STAT_SLOT_FAST(hash_stat.hash_dup);
      STAT_SLOT_FAST(hash_stat.hash_dup_free);
      funcall(`BDB::MKDBSTAT-HASH`,count);
    } break;
    case DB_BTREE: case DB_RECNO: {
      DB_BTREE_STAT btree_stat;
      SYSCALL(db->stat,(db,&btree_stat,flags));
      STAT_SLOT(btree_stat.bt_magic);
      STAT_SLOT(btree_stat.bt_version);
      STAT_SLOT(btree_stat.bt_nkeys);
      STAT_SLOT(btree_stat.bt_ndata);
      STAT_SLOT(btree_stat.bt_pagesize);
      STAT_SLOT(btree_stat.bt_minkey);
      STAT_SLOT(btree_stat.bt_re_len);
      STAT_SLOT(btree_stat.bt_re_pad);
      STAT_SLOT_FAST(btree_stat.bt_levels);
      STAT_SLOT_FAST(btree_stat.bt_int_pg);
      STAT_SLOT_FAST(btree_stat.bt_leaf_pg);
      STAT_SLOT_FAST(btree_stat.bt_dup_pg);
      STAT_SLOT_FAST(btree_stat.bt_free);
      STAT_SLOT_FAST(btree_stat.bt_int_pgfree);
      STAT_SLOT_FAST(btree_stat.bt_leaf_pgfree);
      STAT_SLOT_FAST(btree_stat.bt_dup_pgfree);
      STAT_SLOT_FAST(btree_stat.bt_over_pgfree);
      funcall(`BDB::MKDBSTAT-BTREE`,count);
    } break;
    case DB_QUEUE: {
      DB_QUEUE_STAT queue_stat;
      SYSCALL(db->stat,(db,&queue_stat,flags));
      STAT_SLOT(queue_stat.qs_magic);
      STAT_SLOT(queue_stat.qs_version);
      STAT_SLOT(queue_stat.qs_nkeys);
      STAT_SLOT(queue_stat.qs_ndata);
      STAT_SLOT(queue_stat.qs_pagesize);
      STAT_SLOT(queue_stat.qs_extentsize);
      STAT_SLOT_FAST(queue_stat.qs_pages);
      STAT_SLOT(queue_stat.qs_re_len);
      STAT_SLOT(queue_stat.qs_re_pad);
      STAT_SLOT_FAST(queue_stat.qs_pgfree);
      STAT_SLOT(queue_stat.qs_first_recno);
      STAT_SLOT(queue_stat.qs_cur_recno);
      funcall(`BDB::MKDBSTAT-QUEUE`,count);
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
DEFUN(BDB:DB-OPEN, db file &key :DATABASE :TYPE :MODE :CREATE :DIRTY-READ \
      :EXCL :NOMMAP :RDONLY :THREAD :TRUNCATE :AUTO-COMMIT :TRANSACTION)
{ /* Open a database */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = db_open_flags();
  int mode = posfixnum_default(popSTACK());
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

DEFUN(BDB:DB-TRUNCATE, db &key :TRANSACTION :AUTO-COMMIT)
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

DEFFLAGSET(db_put_flags, DB_APPEND DB_NODUPDATA DB_NOOVERWRITE DB_AUTO_COMMIT)

DEFUN(BDB:DB-PUT, db key val &key :APPEND :NODUPDATA :NOOVERWRITE \
      :AUTO-COMMIT :TRANSACTION)
{ /* Store items into a database */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  u_int32_t flags = db_put_flags();
  DB *db = object_handle(STACK_2,`BDB::DB`,false);
  DBT key, val;
  fill_dbt(STACK_0,&val);
  fill_dbt(STACK_1,&key);
  SYSCALL(db->put,(db,txn,&key,&val,flags));
  VALUES0; skipSTACK(3);
}

/* ===== transactions ===== */
/* not exported:
 DB_TXN->id	Return a transaction's ID
 DB_TXN->prepare	Prepare a transaction for commit
 DB_TXN->set_timeout	Set transaction timeout
 */
DEFFLAGSET(txn_begin_flags, DB_DIRTY_READ DB_TXN_NOSYNC \
           DB_TXN_NOWAIT DB_TXN_SYNC)
DEFUN(BDB:TXN-BEGIN, dbe &key :PARENT :DIRTY-READ :NOSYNC :NOWAIT :SYNC)
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
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  SYSCALL(txn->abort,(txn));
  VALUES0;
}

DEFFLAGSET(txn_commit_flags, DB_TXN_NOSYNC DB_TXN_SYNC)
DEFUN(BDB:TXN-COMMIT, txn &key :NOSYNC :SYNC)
{ /* Commit a transaction */
  u_int32_t flags = txn_commit_flags();
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  SYSCALL(txn->commit,(txn,flags));
  VALUES0;
}

DEFUN(BDB:TXN-DISCARD, txn)
{ /* Discard a transaction */
  DB_TXN *txn = object_handle(popSTACK(),`BDB::TXN`,true);
  SYSCALL(txn->discard,(txn,0));
  VALUES0;
}


