# directory key: win32 registry, LDAP, Gnome-config

#ifdef DIR_KEY

#include "lispbibl.c"

#ifdef WIN32_NATIVE
#include <winreg.h>
#include <winldap.h>
#define LDAP
#endif
#if defined(LDAP) && !defined(WIN32_NATIVE)
#include <ldap.h>
#endif
#ifdef GNOME
#include <gnome.h>
#endif

# as per RFC1777 and RFC2255
#ifndef LDAP_PORT
#define LDAP_PORT 389
#endif

#ifdef WIN32_NATIVE
#define DIR_KEY_INPUT  KEY_READ
#define DIR_KEY_OUTPUT KEY_WRITE
#define DIR_KEY_IO     KEY_ALL_ACCESS
#else
#define DIR_KEY_INPUT  0
#define DIR_KEY_OUTPUT 1
#define DIR_KEY_IO     2
#endif
# :IF_DOES_NOT_EXIST
#define IDNE_ERROR  0
#define IDNE_NIL    1
#define IDNE_CREATE 2
#define IDNE_IGNORE 3 /* used only in `init_iteration_node' */
# :SCOPE
#define SCOPE_SELF  0
#define SCOPE_KIDS  1
#define SCOPE_TREE  2

#ifdef WIN32_NATIVE
#define SYSCALL_WIN32(call)                                            \
  do {                                                                 \
    var uintL status;                                                  \
    begin_system_call();                                               \
    status = call;                                                     \
    if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); } \
    end_system_call();                                                 \
  } while(0)
#endif

#ifdef LDAP
#ifdef WIN32_NATIVE
#define SYSCALL_LDAP(call) SYSCALL_WIN32(call)
#else
#define SYSCALL_LDAP(call)                                             \
  do {                                                                 \
    var uintL status;                                                  \
    begin_system_call();                                               \
    status = call;                                                     \
    if (status != LDAP_SUCCESS) { SetLastError(status); OS_error(); }  \
    end_system_call();                                                 \
  } while(0)
#endif
#endif

# check whether the OBJ is a DIR-KEY
local object test_dir_key (object obj, bool check_open)
{
  if (!dir_key_p(obj)) {
    pushSTACK(obj);        # slot DATUM         of TYPE-ERROR
    pushSTACK(S(dir_key)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(S(dir_key));
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: ~ is not a ~")
           );
  }
  if (check_open && TheDirKey(obj)->closed_p) {
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,
           GETTEXT("~ on ~ is illegal")
           );
  }
  return obj;
}

# convert an array of char to a (VECTOR (UNSIGNED-BYTE 8))
# can trigger GC
local object reg_val_to_vector (uintL size, const char* buffer)
{
  var object vec = allocate_bit_vector(Atype_8Bit,size);
  var uintB* dat = TheSbvector(vec)->data;
  while(size--) *dat++ = *buffer++;
  return vec;
}

#ifdef WIN32_NATIVE
# convert a registry value [type;size;buffer] to the appropriate Lisp object
# can trigger GC
local object registry_value_to_object (DWORD type, DWORD size,
                                       const char* buffer)
{
  switch (type) {
    case REG_NONE: return NIL;
    case REG_SZ:
    case REG_EXPAND_SZ: # should we actually expand the env vars?!
      return asciz_to_string(buffer,O(misc_encoding));
    case REG_DWORD_LITTLE_ENDIAN:
      if (REG_DWORD_LITTLE_ENDIAN != REG_DWORD)
        return UL_to_I(((unsigned char)buffer[3] << 24)+
                       ((unsigned char)buffer[2] << 16)+
                       ((unsigned char)buffer[1] <<  8)+
                       ((unsigned char)buffer[0]));
      else
        return UL_to_I(*(DWORD*)buffer);
    case REG_DWORD_BIG_ENDIAN:
      if (REG_DWORD_BIG_ENDIAN != REG_DWORD)
        return UL_to_I(((unsigned char)buffer[0] << 24)+
                       ((unsigned char)buffer[1] << 16)+
                       ((unsigned char)buffer[2] <<  8)+
                       ((unsigned char)buffer[3]));
      else
        return UL_to_I(*(DWORD*)buffer);
    case REG_MULTI_SZ: {
      # multiple strings, separated by '\0'.
      pushSTACK(NIL);
      var uintL ii;
      for (ii = 0; ii < size; ) {
        var uintL jj;
        for (jj = ii; (jj < size) && buffer[jj]; jj++);
        if (jj < size-1) { # avoid last empty string
          var object new_cons = allocate_cons();
          Cdr(new_cons) = STACK_0;
          STACK_0 = new_cons;
          Car(STACK_0) = n_char_to_string(buffer+ii,jj-ii,O(misc_encoding));
        }
        ii = jj + 1;
      }
      return nreverse(popSTACK());
    }
    # case REG_RESOURCE_LIST:
    # case REG_FULL_RESOURCE_DESCRIPTOR:
    # case REG_RESOURCE_REQUIREMENTS_LIST:
    # case REG_LINK:
    # case REG_BINARY:
    default:
      return reg_val_to_vector(size,buffer);
  }
}

#endif

LISPFUNN(dir_key_type,1)
# (LISP:DIR-KEY-TYPE dkey)
# return the type of the key (:win32 or :gnome-config or :ldap)
{
  var object dkey = test_dir_key(popSTACK(),false);
  value1 = TheDirKey(dkey)->type; mv_count = 1;
}

LISPFUNN(dir_key_path,1)
# (LISP:DIR-KEY-PATH dkey)
# return the path of the key (a string)
{
  var object dkey = test_dir_key(popSTACK(),false);
  value1 = TheDirKey(dkey)->path; mv_count = 1;
}

LISPFUNN(dir_key_direction,1)
# (LISP:DIR-KEY-DIRECTION dkey)
# return the direction of this key - :input :output or :io
{
  var object dkey = test_dir_key(popSTACK(),true);
  value1 = TheDirKey(dkey)->direction; mv_count = 1;
}

LISPFUNN(dir_key_open_p,1)
# (LISP:DIR-KEY-OPEN-P dkey)
# return T if the key is open
{
  var object dkey = test_dir_key(popSTACK(),false);
  value1 = (TheDirKey(dkey)->closed_p ? NIL : T); mv_count = 1;
}

LISPFUNN(dir_key_close,1)
# (LISP:DIR-KEY-CLOSE dkey)
# close the supplied DIR-KEY
{
  var object dkey = popSTACK();
  #ifdef WIN32_NATIVE
  if (fpointerp(dkey)) { # an HKEY in an iterator_state
    if (TheFpointer(dkey)->fp_pointer)
      SYSCALL_WIN32(RegCloseKey((HKEY)(TheFpointer(dkey)->fp_pointer)));
  } else
  #endif
  {
    test_dir_key(dkey,false);
    if (!TheDirKey(dkey)->closed_p) {
      #ifdef WIN32_NATIVE
      if (eq(TheDirKey(dkey)->type,S(Kwin32))) {
        SYSCALL_WIN32(RegCloseKey((HKEY)(TheDirKey(dkey)->handle)));
      } else
      #endif
      #ifdef LDAP
      if (eq(TheDirKey(dkey)->type,S(Kldap))) {
        SYSCALL_LDAP(ldap_unbind((struct ldap*)(TheDirKey(dkey)->handle)));
      } else
      #endif
      #ifdef GNOME
      if (eq(TheDirKey(dkey)->type,S(Kgnome))) {
        with_string_0(TheDirKey(dkey)->path,O(pathname_encoding),pathz,{
          gnome_config_drop_file(pathz);
          gnome_config_sync_file(pathz);
        });
      } else
      #endif
        /* noop */ ;
      TheDirKey(dkey)->closed_p = true;
    }
  }
  value1 = NIL; mv_count = 1;
}

#ifdef WIN32_NATIVE
struct root {
  const char *name;
  unsigned int namelen;
  HKEY hkey;
};
#define MKKEY(key)  { #key, sizeof(#key)-1, key }
static struct root roots[] = {
#ifdef  HKEY_CLASSES_ROOT
  MKKEY(HKEY_CLASSES_ROOT),
#endif
#ifdef  HKEY_CURRENT_USER
  MKKEY(HKEY_CURRENT_USER),
#endif
#ifdef  HKEY_LOCAL_MACHINE
  MKKEY(HKEY_LOCAL_MACHINE),
#endif
#ifdef  HKEY_USERS
  MKKEY(HKEY_USERS),
#endif
#ifdef  HKEY_PERFORMANCE_DATA
  MKKEY(HKEY_PERFORMANCE_DATA),
#endif
#ifdef  HKEY_CURRENT_CONFIG
  MKKEY(HKEY_CURRENT_CONFIG),
#endif
#ifdef  HKEY_DYN_DATA
  MKKEY(HKEY_DYN_DATA),
#endif
};
#undef MKKEY

local HKEY parse_registry_path (const char* path, const char** base_ret)
{
  var unsigned int ii;
  var unsigned int len;
  var HKEY hkey = NULL;
  var char* base;
  var char* host;
  # Path syntax HOSTNAME\\... denotes a remote registry access.
  host = NULL;
  base = strstr(path,"\\\\");
  if (base != NULL) {
    len = base-path;
    host = (char*)alloca(len+1);
    strncpy(host,path,len);
    host[len] = 0;
    path = base + 2;
  }
  # Now look for the topmost directory component.
  base = strchr(path,'\\');
  if (base==NULL)
    len = strlen(path);
  else {
    len = base-path;
    base++;
  }
  # Return the remainder.
  *base_ret = base;
  # Get the key for the topmost directory component.
  for (ii = 0; ii < sizeof(roots)/sizeof(*roots); ii++)
    if (roots[ii].namelen == len && memcmp(roots[ii].name,path,len) == 0) {
      hkey = roots[ii].hkey;
      break;
    }
  if (hkey == NULL) { SetLastError(ERROR_PATH_NOT_FOUND); OS_error(); }
  if (host == NULL)
    return hkey;
  else {
    HKEY res;
    SYSCALL_WIN32(RegConnectRegistry(host,hkey,&res));
    return res;
  }
}

local void open_reg_key (HKEY hkey, char* path, REGSAM perms,
                         int if_not_exists, HKEY* p_hkey)
{
  var DWORD status;
  begin_system_call();
  status = RegOpenKeyEx(hkey,path,0,perms,p_hkey);
  if (status != ERROR_SUCCESS) {
    if ((if_not_exists == IDNE_IGNORE) ||
        (status == ERROR_FILE_NOT_FOUND) && (if_not_exists != IDNE_ERROR)) {
      switch (if_not_exists) {
        case IDNE_NIL: case IDNE_IGNORE:
          *p_hkey = NULL; break;
        case IDNE_CREATE:
          status = RegCreateKey(hkey,path,p_hkey);
          if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }
          break;
        default: NOTREACHED
      }
    } else { SetLastError(status); OS_error(); }
  }
  end_system_call();
}
#endif

local uintL parse_if_not_exists (object if_not_exists_arg, uintL direction)
{
  var uintL res = IDNE_ERROR;
  if ((eq(if_not_exists_arg,unbound) && (direction == DIR_KEY_INPUT))
      || eq(if_not_exists_arg,S(Kerror))) {
    res = IDNE_ERROR;
  } else if (eq(if_not_exists_arg,NIL)) {
    res = IDNE_NIL;
  } else if (eq(if_not_exists_arg,S(Kcreate))
             || eq(if_not_exists_arg,unbound)) {
    res = IDNE_CREATE;
  } else {
    pushSTACK(if_not_exists_arg);         # slot DATUM of         TYPE-ERROR
    pushSTACK(O(type_if_does_not_exist)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(if_not_exists_arg);
    pushSTACK(S(Kif_does_not_exist));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: illegal ~ argument ~")
           );
  }
  return res;
}

local uintL parse_direction (object* direction_arg)
{
  var uintL res = DIR_KEY_INPUT;
  if (eq(*direction_arg,unbound) || eq(*direction_arg,S(Kinput))) {
    res = DIR_KEY_INPUT;
    *direction_arg = S(Kinput);
  } else if (eq(*direction_arg,S(Koutput))) {
    res = DIR_KEY_OUTPUT;
  } else if (eq(*direction_arg,S(Kio))) {
    res = DIR_KEY_IO;
  } else {
    pushSTACK(*direction_arg);    # slot DATUM         of TYPE-ERROR
    pushSTACK(O(type_direction)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(*direction_arg);
    pushSTACK(S(Kdirection));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: illegal ~ argument ~")
           );
  }
  return res;
}

LISPFUN(dir_key_open,2,0,norest,key,2,(kw(direction),kw(if_does_not_exist)))
# (LISP:DIR-KEY-OPEN key path [:direction] [:if-does-not-exist])
# return the DIR-KEY object corresponding to the PATH under KEY
# PATH should be a string, like "HKEY_LOCAL_MACHINE\\Something"
# KEY can be either a DIR-KEY or a (MEMBER :win32 :gnome :ldap),
# in which case PATH must be absolute.
# :direction and :if-does-not-exist has the same meaning as for OPEN.
{
  var object if_not_exists_arg = STACK_0;
  var object direction_arg = STACK_1;
  var object path = STACK_2;
  var object root = STACK_3;
  var void* ret_handle;
  var uintL status;
  var uintL direction = parse_direction(&direction_arg);
  var uintL if_not_exists = parse_if_not_exists(if_not_exists_arg,direction);
  if (!stringp(path)) fehler_string(path);
  # create the key handle
  var object type = (dir_key_p(root) ? TheDirKey(root)->type : root);
  #ifdef WIN32_NATIVE
  if (eq(type,S(Kwin32))) {
    if (dir_key_p(root)) {
      test_dir_key(root,true);
      with_string_0(path,O(misc_encoding),pathz,{
        open_reg_key((HKEY)(TheDirKey(root)->handle),pathz,direction,
                     if_not_exists,(HKEY*)&ret_handle);
      });
    } else {
      with_string_0(path,O(misc_encoding),pathz,{
        var char* base;
        HKEY hkey = parse_registry_path(pathz,&base);
        open_reg_key(hkey,base,direction,if_not_exists,(HKEY*)&ret_handle);
      });
    }
  } else
  #endif
  #ifdef LDAP
  if (eq(type,S(Kldap))) {
    if (dir_key_p(root)) {
      test_dir_key(root,true);
      with_string_0(path,O(misc_encoding),pathz,{
        char *host = NULL;
        begin_system_call();
        if (ldap_get_option(TheDirKey(root)->handle,LDAP_OPT_HOST_NAME,
                            (void*)&host))
          printf("no host!\n");
        ret_handle = ldap_init((unsigned short*)host,LDAP_PORT);
        # FIXME: chdir to pathz
      });
    } else {
      with_string_0(path,O(misc_encoding),pathz,{
        # FIXME: parse the URL `pathz'
        ret_handle = ldap_init((unsigned short*)pathz,LDAP_PORT);
      });
    }
  } else
  #endif
  #ifdef GNOME
  if (eq(root,S(Kgnome))) {
    # do nothing - gnome-conf is stateless
  } else
  #endif
  { # invalid type
    pushSTACK(type);            # slot DATUM of         TYPE-ERROR
    pushSTACK(O(type_dir_key)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(S(dir_key));
    pushSTACK(type);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: ~ is not a ~")
           );
  }
  # create the DIR-KEY
  pushSTACK(ret_handle);
  pushSTACK(direction_arg);
  if (dir_key_p(root)) {
    pushSTACK(TheDirKey(root)->path);
    #ifdef WIN32_NATIVE
    if (eq(TheDirKey(root)->type,S(Kwin32)))
      pushSTACK(O(backslash_string));
    else
    #endif
      pushSTACK(O(slash_string));
    pushSTACK(path);
    var object totalpath = string_concat(3);
    pushSTACK(totalpath);
  } else
    pushSTACK(path);
  var object dkey = allocate_dir_key();
  TheDirKey(dkey)->type = S(Kwin32);
  TheDirKey(dkey)->closed_p = false;
  TheDirKey(dkey)->path = popSTACK();
  TheDirKey(dkey)->direction = popSTACK();
  TheDirKey(dkey)->handle = popSTACK();
  pushSTACK(dkey);
  # Call (FINALIZE dir-key #'dir-key-close).
  pushSTACK(dkey);
  pushSTACK(L(dir_key_close));
  funcall(L(finalize),2);
  # Done.
  value1 = STACK_0; mv_count = 1;
  skipSTACK(5);
}

# The common code in DIR-KEY-SUBKEYS & DIR-KEY-ATTRIBUTES
#define MAKE_OBJECT_LIST(COUNT_EXPR,GET_NEXT_OBJ_EXPR)                     \
  var object dkey = test_dir_key(popSTACK(),TRUE);                         \
  var LONG status;                                                         \
  var DWORD n_obj;                                                         \
  var DWORD maxlen;                                                        \
  var HKEY hkey = (HKEY)TheDirKey(dkey)->handle;                           \
  begin_system_call();                                                     \
  status = (COUNT_EXPR);                                                   \
  if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }       \
  end_system_call();                                                       \
  if (n_obj > 0) {                                                         \
    var unsigned int ii;                                                   \
    var char* buf = (char*)alloca(maxlen+1); /* one extra char for '\0' */ \
    maxlen++; pushSTACK(NIL);                                              \
    for (ii = 0; ii < n_obj; ii++) {                                       \
      var DWORD len = maxlen;                                              \
      var DWORD status;                                                    \
      begin_system_call();                                                 \
      status = (GET_NEXT_OBJ_EXPR);                                        \
      if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }   \
      end_system_call();                                                   \
      { var object new_cons = allocate_cons();                             \
        Cdr(new_cons) = STACK_0;                                           \
        STACK_0 = new_cons; }                                              \
      { var object string = asciz_to_string(buf,O(misc_encoding));         \
        Car(STACK_0) = string; }                                           \
    }                                                                      \
    value1 = nreverse(popSTACK());                                         \
  } else                                                                   \
    value1 = NIL;                                                          \
  mv_count = 1

LISPFUNN(dir_key_subkeys,1)
# (LISP:DIR-KEY-SUBKEYS key)
# return the list of subkey names of the given KEY
{
  if (eq(STACK_0,S(Kwin32))) { # top-level keys
    skipSTACK(1);
    var int ii;
    var int len = sizeof(roots)/sizeof(*roots);
    for (ii=0; ii<len; ii++)
      pushSTACK(asciz_to_string(roots[ii].name,O(misc_encoding)));
    value1 = listof(len);
    mv_count = 1;
  } else {
    MAKE_OBJECT_LIST(RegQueryInfoKey(hkey,NULL,NULL,NULL,&n_obj,&maxlen,
                                     NULL,NULL,NULL,NULL,NULL,NULL),
                     RegEnumKey(hkey,ii,buf,len));
  }
}

LISPFUNN(dir_key_attributes,1)
# (LISP:DIR-KEY-ATTRIBUTES key)
# return the list of attribute names of the given KEY
{
  MAKE_OBJECT_LIST(RegQueryInfoKey(hkey,NULL,NULL,NULL,NULL,NULL,NULL,
                                   &n_obj,&maxlen,NULL,NULL,NULL),
                   RegEnumValue(hkey,ii,buf,&len,NULL,NULL,NULL,NULL));
}
#undef MAKE_OBJECT_LIST

# iteration state access
# STACK = (NODE1 NODE2 ...)
#define ITST_DKEY(state)   TheSvector(state)->data[0]
#define ITST_PATH(state)   TheSvector(state)->data[1]
#define ITST_SCOPE(state)  TheSvector(state)->data[2]
#define ITST_STACK(state)  TheSvector(state)->data[3]
#define ITST_NODE(state)   Car(ITST_STACK(state))
# node to process
#define NODE_HANDLE(node)  TheSvector(node)->data[0]
#define NODE_KEY(node)     TheSvector(node)->data[1]
#define NODE_KEY_S(node)   TheSvector(node)->data[2]
#define NODE_ATT(node)     TheSvector(node)->data[3]
#define NODE_ATT_S(node)   TheSvector(node)->data[4]
#define NODE_DAT_S(node)   TheSvector(node)->data[5]
#define NODE_NAME(node)    TheSvector(node)->data[6]

local object itst_current (object state)
# return the current path - concatenate the NAMEs of the NODEs of the STACK
# can trigger GC
{
  var object stack = ITST_STACK(state);
  var uintL depth = 0;
  for (stack = nreverse(stack); !eq(stack,NIL); stack = Cdr(stack)) {
    var object name = NODE_NAME(Car(stack));
    if (Sstring_length(name) > 0) {
      if (depth) {
        depth++;
        pushSTACK(O(backslash_string));
      }
      depth++;
      pushSTACK(name);
    }
  }
  ITST_STACK(state) = nreverse(ITST_STACK(state));
  return string_concat(depth);
}

local int parse_scope (object scope) {
  if (eq(scope,S(Kself)))  return SCOPE_SELF;
  if (eq(scope,S(Klevel))) return SCOPE_KIDS;
  if (eq(scope,S(Ktree)))  return SCOPE_TREE;
  pushSTACK(scope);         # slot DATUM of         TYPE-ERROR
  pushSTACK(O(type_scope)); # slot EXPECTED-TYPE of TYPE-ERROR
  pushSTACK(S(Kscope));
  pushSTACK(scope);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,
         GETTEXT("~: ~ is not a ~")
         );
  return -1; # just to pacify the MSVC compiler
}

LISPFUNN(dkey_search_iterator,3)
# (SYS::DKEY-SEARCH-ITERATOR key path scope)
# return a simple vector with the iteration state
# < dkey path scope
# > #(dkey init-path scope (node ...))
# can trigger GC
{
  if (!stringp(STACK_1)) fehler_string(STACK_1);
  parse_scope(STACK_0);
  value1 = allocate_vector(4);
  ITST_DKEY(value1)  = test_dir_key(STACK_2,TRUE);
  ITST_PATH(value1)  = STACK_1;
  ITST_SCOPE(value1) = STACK_0;
  ITST_STACK(value1) = T;
  mv_count = 1;
  skipSTACK(3);
}

local void init_iteration_node (object state,object subkey,
                                object *new_path,object *failed_p)
# open HANDLE to point to DKEY\\PATH
# compute KEY_S, ATT_S and DAT_S
# return the full current path (itst_current)
# and T/NIL indicating whether OPEN was successful
# can trigger GC
{
  pushSTACK(state);
  pushSTACK(subkey);
  pushSTACK(allocate_cons());         # stack
  pushSTACK(allocate_vector(7));      # node
  pushSTACK(allocate_fpointer(NULL)); # handle
  var object handle = STACK_0;
  var object node   = STACK_1;
  var object stack  = STACK_2;
  subkey = STACK_3;
  state  = STACK_4;
  var object path = ITST_PATH(state);
  if (!stringp(path)) fehler_string(path);
  # (push node stack)
  if (eq(T,ITST_STACK(state)))
    ITST_STACK(state) = NIL;
  Car(stack) = node;
  Cdr(stack) = ITST_STACK(state);
  ITST_STACK(state) = stack;
  # init node
  NODE_KEY(node) = fixnum(0);
  NODE_ATT(node) = fixnum(0);
  NODE_NAME(node) = (eq(subkey,NIL) ? path : subkey);
  # init handle
  NODE_HANDLE(node) = handle;
  pushSTACK(handle);
  pushSTACK(L(dir_key_close));
  funcall(L(finalize),2); # (FINALIZE handle #'dir-key-close)
  *new_path = itst_current(STACK_4); # state
  var Dir_Key dk = TheDirKey(test_dir_key(ITST_DKEY(STACK_4),TRUE)); # state
  var Fpointer fp = TheFpointer(STACK_0); # handle
  with_string_0(*new_path,O(misc_encoding),pathz,{
    open_reg_key((HKEY)(dk->handle),pathz,parse_direction(&(dk->direction)),
                 IDNE_IGNORE,(HKEY*)&(fp->fp_pointer));
  });
  if (fp->fp_pointer) {
    var DWORD k_size;
    var DWORD a_size;
    var DWORD d_size;
    SYSCALL_WIN32(RegQueryInfoKey((HKEY)(fp->fp_pointer),NULL,NULL,NULL,NULL,
                                  &k_size,NULL,NULL,&a_size,&d_size,
                                  NULL,NULL));
    NODE_KEY_S(STACK_1) = fixnum(k_size+1); # node
    NODE_ATT_S(STACK_1) = fixnum(a_size+1); # node
    NODE_DAT_S(STACK_1) = fixnum(d_size+1); # node
    *failed_p = NIL;
  } else {
    NODE_KEY_S(STACK_1) = Fixnum_0; # node
    NODE_ATT_S(STACK_1) = Fixnum_0; # node
    NODE_DAT_S(STACK_1) = Fixnum_0; # node
    *failed_p = T;
  }
  skipSTACK(5);
}

local object state_next_key (object state)
# return the next key of the current state or NIL
# if the key is NIL, the HKEY is closed and the stack is popped
# can trigger GC
{
  var object stack = ITST_STACK(state);
  if (!eq(stack,NIL)) {
    var object node = ITST_NODE(state);
    var uintL keynum = posfixnum_to_L(NODE_KEY(node));
    var uintL keylen = posfixnum_to_L(NODE_KEY_S(node));
    var char* buffer = (char*)alloca(keylen);
    var Fpointer fp  = TheFpointer(NODE_HANDLE(node));
    if (fp->fp_pointer) {
      var DWORD status = RegEnumKey((HKEY)(fp->fp_pointer),
                                    keynum,buffer,keylen);
      if (status == ERROR_SUCCESS) {
        NODE_KEY(node) = fixnum_inc(NODE_KEY(node),1);
        return asciz_to_string(buffer,O(misc_encoding));
      } else {
        SYSCALL_WIN32(RegCloseKey((HKEY)(fp->fp_pointer)));
        fp->fp_pointer = NULL;
        ITST_STACK(state) = Cdr(stack);
        return NIL;
      }
    } else {
      ITST_STACK(state) = Cdr(stack);
      return NIL;
    }
  } else
    return NIL;
}

LISPFUNN(dkey_search_next_key,1)
# (SYS::DKEY-SEARCH-NEXT-KEY state)
# return the next key of this iteration
# and T if open failed
# can trigger GC
{
  var object state = STACK_0;
  var object dkey  = test_dir_key(ITST_DKEY(state),TRUE);
  var int scope = parse_scope(ITST_SCOPE(state));
  var object stack = ITST_STACK(state);
  switch (scope) {
    case SCOPE_SELF:            # just the top
      if (eq(stack,T))
        init_iteration_node(state,NIL,&value1,&value2); # first call
      else
        value1 = value2 = NIL;
      break;
    case SCOPE_KIDS:            # the children
      if (eq(stack,T)) {
        init_iteration_node(state,NIL,&value1,&value2);
        if (nullp(value2))
          value1 = state_next_key(STACK_0); # STACK_0 == state
      } else {
        value1 = state_next_key(STACK_0);
        value2 = NIL;
      }
      break;
    case SCOPE_TREE:            # the whole subtree
      if (eq(stack,T))
        init_iteration_node(state,NIL,&value1,&value2); # first call
      else {                          # find the next node and return it
        pushSTACK(NIL); # STACK_0 == subkey; STACK_1 == state
        do STACK_0 = state_next_key(STACK_1); # subkey==NIL ==> stack popped
        while (eq(STACK_0,NIL) && !eq(ITST_STACK(STACK_1),NIL));
        if (eq(STACK_0,NIL)) value1 = NIL;
        else init_iteration_node(STACK_1,STACK_0,&value1,&value2);
        skipSTACK(1);
      }
      break;
    default: NOTREACHED
  }
  skipSTACK(1);
  mv_count = 2;
}

LISPFUNN(dkey_search_next_att,1)
# (SYS::DKEY-SEARCH-NEXT-ATT state)
# return the next attribute and its value of this iteration
# can trigger GC
{
  var object state = STACK_0;
  var object stack = ITST_STACK(state);
  if (!consp(stack)) {
    pushSTACK(S(dkey_search_next_key));
    pushSTACK(state);
    pushSTACK(S(dkey_search_next_att));
    fehler(error,
           GETTEXT("~ from ~ without ~ before it")
           );
  }
  var object node = Car(stack);
  var Fpointer fp = TheFpointer(NODE_HANDLE(node));
  var uintL attnum = posfixnum_to_L(NODE_ATT(node));
  var uintL attlen = posfixnum_to_L(NODE_ATT_S(node));
  var uintL datlen = posfixnum_to_L(NODE_DAT_S(node));
  var char* att = (char*)alloca(attlen);
  var char* dat = (char*)alloca(datlen);
  var DWORD type;
  var DWORD size;
  var DWORD status = RegEnumValue((HKEY)(fp->fp_pointer),
                                  attnum,att,&attlen,NULL,
                                  &type,dat,&size);
  if (status == ERROR_SUCCESS) {
    NODE_ATT(node) = fixnum_inc(NODE_ATT(node),1);
    pushSTACK(asciz_to_string(att,O(misc_encoding)));
    pushSTACK(registry_value_to_object(type,size,dat));
  } else {
    pushSTACK(NIL);
    pushSTACK(NIL);
  }
  mv_count = 2;
  value1 = STACK_1;
  value2 = STACK_0;
  skipSTACK(3);
}

#undef ITST_DKEY
#undef ITST_PATH
#undef ITST_SCOPE
#undef ITST_STACK
#undef ITST_NODE
#undef NODE_NAME
#undef NODE_KEY
#undef NODE_KEY_S
#undef NODE_ATT
#undef NODE_ATT_S
#undef NODE_DAT_S
#undef NODE_HANDLE

LISPFUN(dir_key_value,2,1,norest,nokey,0,NILL)
# (LISP:DIR-KEY-VALUE key name [default])
# return the value of the given NAME in the KEY
# KEY must be an open DIR-KEY, NAME - a string
# if the name does not exists, return the DEFAULT (or signal an error)
# setf-able
# can trigger GC
{
  var object default_value = popSTACK();
  var object name = popSTACK();
  var object dkey = test_dir_key(popSTACK(),true);
  if (!stringp(name)) fehler_string(name);
  with_string_0(name,O(misc_encoding),namez,{
    var DWORD status;
    var DWORD type;
    var DWORD size;
    begin_system_call();
    status = RegQueryValueEx((HKEY)(TheDirKey(dkey)->handle),namez,
                             NULL,NULL,NULL,&size);
    if (status != ERROR_SUCCESS) {
      if ((status == ERROR_FILE_NOT_FOUND) && !eq(default_value,unbound)) {
        value1 = default_value;
        end_system_call();
        goto end;
      }
      SetLastError(status); OS_error();
    }
    ++size;                     # one extra char for `\0'
    var char* buffer = (char*)alloca(size);
    status = RegQueryValueEx((HKEY)(TheDirKey(dkey)->handle),namez,
                             NULL,&type,buffer,&size);
    if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }
    end_system_call();
    value1 = registry_value_to_object(type,size,buffer);
  end:;
  });
  mv_count = 1;
}

LISPFUNN(set_dkey_value,3)
# (SYS::SET-DKEY-VALUE key name value)
# set the given NAME in the KEY to the VALUE
{
  var object value = popSTACK();
  var object name = popSTACK();
  var object dkey = test_dir_key(popSTACK(),true);
  if (!stringp(name)) fehler_string(name);
  with_string_0(name,O(misc_encoding),namez, {
    if (stringp(value)) {
      with_string_0(value,O(misc_encoding),valz, {
        SYSCALL_WIN32(RegSetValueEx((HKEY)(TheDirKey(dkey)->handle),namez,0,
                                    REG_SZ,valz,strlen(valz)));
      });
    } else if (integerp(value)) {
      var DWORD word = I_to_UL(value);
      SYSCALL_WIN32(RegSetValueEx((HKEY)(TheDirKey(dkey)->handle),namez,0,
                                  REG_DWORD,(char*)&word,sizeof(DWORD)));
    } else if (bit_vector_p(Atype_8Bit,value)) {
      var uintL idx = 0;
      var uintL len = vector_length(value);
      object arr = (array_simplep(value) ? value :
                    iarray_displace_check(value,len,&idx));
      SYSCALL_WIN32(RegSetValueEx((HKEY)(TheDirKey(dkey)->handle),namez,0,
                                  REG_BINARY,TheSbvector(arr)->data+idx,
                                  len-idx));
    } else {
      pushSTACK(value);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~ on ~ is illegal")
             );
    }
  });
  value1 = value;
  mv_count = 1;
}

#define REG_KEY_DEL(call)                                               \
  var object name = popSTACK();                                         \
  var object dkey = test_dir_key(popSTACK(),true);                      \
  if (!stringp(name)) fehler_string(name);                              \
  with_string_0(name,O(misc_encoding),namez,{                           \
    SYSCALL_WIN32(call((HKEY)(TheDirKey(dkey)->handle),namez));         \
  });                                                                   \
  value1 = NIL;                                                         \
  mv_count = 1

LISPFUNN(dir_key_subkey_delete,2)
# (LISP:DIR-KEY-SUBKEY-DELETE key name)
# delete the specified subkey (and all its subkeys)
{
  REG_KEY_DEL(RegDeleteKey);
}
LISPFUNN(dir_key_value_delete,2)
# (LISP:DIR-KEY-VALUE-DELETE key name)
# delete the specified value
{
  REG_KEY_DEL(RegDeleteValue);
}
#undef REG_KEY_DEL

LISPFUNN(dkey_info,1)
# (SYS::DKEY-INFO key)
# return all the info about the key, as 10 values:
# class; class_length;
# num_sub_keys; max_sub_key_length; max_class_length;
# num_values; max_value_name_length; max_value_length;
# security_descriptor; write_time
# can trigger GC
{
  object dkey = test_dir_key(popSTACK(),TRUE);
  var HKEY hkey = (HKEY)(TheDirKey(dkey)->handle);
  var char* class_name = NULL;
  var DWORD class_length;
  var DWORD num_sub_keys;
  var DWORD max_sub_key_length;
  var DWORD max_class_length;
  var DWORD num_values;
  var DWORD max_value_name_length;
  var DWORD max_value_length;
  var DWORD security_descriptor;
  var FILETIME write_time;
  SYSCALL_WIN32(RegQueryInfoKey(hkey,NULL,&class_length,NULL,NULL,
                                NULL,NULL,NULL,NULL,NULL,NULL,NULL));
  if (class_length > 0) {
    class_length++;
    class_name = (char*)alloca(class_length);
  }
  SYSCALL_WIN32(RegQueryInfoKey(hkey,class_name,&class_length,NULL,
                                &num_sub_keys,&max_sub_key_length,
                                &max_class_length,
                                &num_values,&max_value_name_length,
                                &max_value_length,
                                &security_descriptor,
                                &write_time));
  value1 = (class_name ? asciz_to_string(class_name,O(misc_encoding)) : NIL);
  value2 = L_to_I(num_sub_keys);
  value3 = L_to_I(max_sub_key_length);
  value4 = L_to_I(max_class_length);
  value5 = L_to_I(num_values);
  value6 = L_to_I(max_value_name_length);
  value7 = L_to_I(max_value_length);
  value8 = L_to_I(security_descriptor);
  value9 = convert_time_to_universal(&write_time);
  mv_count = 9;
}

#undef SYSCALL_WIN32
#undef SYSCALL_LDAP

#endif # DIR_KEY
