# directory key: win32 registry, LDAP, Gnome-config

#ifdef DIR_KEY

#include "lispbibl.c"

#ifdef WIN32_NATIVE
#include <winreg.h>
#endif
#ifdef LDAP
#include <ldap.h>
#endif
#ifdef GNOME
#include <gnome.h>
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
# IF_DOES_NOT_EXISTS
#define IDNE_ERROR  0
#define IDNE_NIL    1
#define IDNE_CREATE 2

#ifdef WIN32_NATIVE
#define SYSCALL_WIN32(call)                                             \
  do { uintL status;                                                    \
   begin_system_call();                                                 \
   status = call;                                                       \
   if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }   \
   end_system_call();                                                   \
  } while(0)
#endif

#ifdef LDAP
#define SYSCALL_LDAP(call)                                              \
  do { uintL status;                                                    \
   begin_system_call();                                                 \
   status = call;                                                       \
   if (status != LDAP_SUCCESS) { SetLastError(status); OS_error(); }    \
   end_system_call();                                                   \
  } while(0)
#endif

# check whether the OBJ is a DIR-KEY
local void test_dir_key (object obj, boolean check_open)
{
  if (!dir_key_p(obj)) {
    pushSTACK(obj);        # slot DATUM         of TYPE-ERROR
    pushSTACK(S(dir_key)); # slot EXPECTED-TYPE of TYPE-ERROR
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
}

# convert an array of char to a (VECTOR (UNSIGNED-BYTE 8))
local object reg_val_to_vector (uintL size,const char* buffer)
{
  var object vec = allocate_bit_vector(Atype_8Bit,size);
  var uintB* dat = TheSbvector(vec)->data;
  while(size--) *dat++ = *buffer++;
  return vec;
}

#ifdef WIN32_NATIVE
# convert a registry value [type;size;buffer] to the appropriate Lisp object
local object registry_value_to_object (DWORD type,DWORD size,const char*buffer)
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
      else return UL_to_I(*(DWORD*)buffer);
    case REG_DWORD_BIG_ENDIAN:
      if (REG_DWORD_BIG_ENDIAN != REG_DWORD)
        return UL_to_I(((unsigned char)buffer[0] << 24)+
                       ((unsigned char)buffer[1] << 16)+
                       ((unsigned char)buffer[2] <<  8)+
                       ((unsigned char)buffer[3]));
      else return UL_to_I(*(DWORD*)buffer);
    case REG_MULTI_SZ: {
      object ret = NIL, tail = NIL;
      unsigned int ii, len;
      for (ii=0; (ii<size) && buffer[ii]; ii+= len+1) {
        len = strlen(buffer+ii);
        if (len >= size) len = size;
        if (eq(tail,NIL)) tail = ret = allocate_cons();
        else tail = Cdr(tail) = allocate_cons();
        Car(tail) = n_char_to_string(buffer+ii,len,O(misc_encoding));
        Cdr(tail) = NIL;
      }
      return ret;
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
  object dkey = popSTACK(); test_dir_key(dkey,FALSE);
  value1 = TheDirKey(dkey)->type; mv_count = 1;
}

LISPFUNN(dir_key_path,1)
# (LISP:DIR-KEY-PATH dkey)
# return the path of the key (a string)
{
  object dkey = popSTACK(); test_dir_key(dkey,FALSE);
  value1 = TheDirKey(dkey)->path; mv_count = 1;
}

LISPFUNN(dir_key_direction,1)
# (LISP:DIR-KEY-DIRECTION dkey)
# return the direction of this key - :input :output or :io
{
  object dkey = popSTACK(); test_dir_key(dkey,TRUE);
  value1 = TheDirKey(dkey)->direction; mv_count = 1;
}

LISPFUNN(dir_key_open_p,1)
# (LISP:DIR-KEY-OPEN-P dkey)
# return T if the key is open
{
  object dkey = popSTACK(); test_dir_key(dkey,FALSE);
  value1 = (TheDirKey(dkey)->closed_p ? NIL : T); mv_count = 1;
}

LISPFUNN(dir_key_close,1)
# (LISP:DIR-KEY-CLOSE dkey)
# close the supplied DIR-KEY
{
  object dkey = popSTACK();
  #ifdef WIN32_NATIVE
  if (fpointerp(dkey)) { # an HKEY in an iterator_state
    printf("finalizing %u\n",TheFpointer(dkey)->fp_pointer);
    if (TheFpointer(dkey)->fp_pointer)
      RegCloseKey((HKEY)TheFpointer(dkey)->fp_pointer);
  } else
  #endif
  {
    test_dir_key(dkey,FALSE);
    if (!TheDirKey(dkey)->closed_p) {
      #ifdef WIN32_NATIVE
      if (eq(TheDirKey(dkey)->type,S(Kwin32))) {
        SYSCALL_WIN32(RegCloseKey((HKEY)TheDirKey(dkey)->handle));
      } else
      #endif
      #ifdef LDAP
      if (eq(TheDirKey(dkey)->type,S(Kldap))) {
        SYSCALL_LDAP(ldap_unbind((LDAP*)TheDirKey(dkey)->handle));
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
      TheDirKey(dkey)->closed_p = TRUE;
    }
  }
  value1 = NIL; mv_count = 1;
}

#ifdef WIN32_NATIVE
struct root {
  const char *name;
  HKEY hkey;
};
#define MKKEY(key)  {#key,key}
static struct root roots[] = {
  MKKEY(HKEY_CLASSES_ROOT),
  MKKEY(HKEY_CURRENT_USER),
  MKKEY(HKEY_LOCAL_MACHINE),
  MKKEY(HKEY_USERS),
  MKKEY(HKEY_PERFORMANCE_DATA),
  MKKEY(HKEY_CURRENT_CONFIG),
  MKKEY(HKEY_DYN_DATA),
};
#undef MKKEY

local void open_reg_key (HKEY hkey,char* path,REGSAM perms,
                         int if_not_exists, # 0-error 1-NIL 2-create
                         HKEY* p_hkey)
{
  DWORD status;
  begin_system_call();
  status = RegOpenKeyEx(hkey,path,0,perms,p_hkey);
  if (status != ERROR_SUCCESS) {
    if ((status == ERROR_FILE_NOT_FOUND) && if_not_exists) {
      switch (if_not_exists) {
        case 1:
          *p_hkey = NULL; break;
        case 2:
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

local uintL parse_if_not_exists (object if_not_exists_arg,uintL direction)
{
  uintL res = IDNE_ERROR;
  if ((eq(if_not_exists_arg,unbound) && (direction == DIR_KEY_INPUT)) ||
      eq(if_not_exists_arg,S(Kerror))) {
    res = IDNE_ERROR;
  } else if (eq(if_not_exists_arg,NIL)) {
    res = IDNE_NIL;
  } else if (eq(if_not_exists_arg,S(Kcreate)) ||
             eq(if_not_exists_arg,unbound)) {
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

local uintL parse_direction (object *direction_arg)
{
  uintL res = DIR_KEY_INPUT;
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
  uintL status;
  uintL direction = parse_direction(&direction_arg);
  uintL if_not_exists = parse_if_not_exists(if_not_exists_arg,direction);
  if (!stringp(path)) fehler_string(path);
  # create the key handle
  var object type = (dir_key_p(root) ? TheDirKey(root)->type : root);
  #ifdef WIN32_NATIVE
  if (eq(type,S(Kwin32))) {
    if (dir_key_p(root)) {
      test_dir_key(root,TRUE);
      with_string_0(path,O(misc_encoding),pathz,{
        open_reg_key((HKEY)TheDirKey(root)->handle,pathz,direction,
                     if_not_exists,(HKEY*)&ret_handle);
      });
    } else {
      with_string_0(path,O(misc_encoding),pathz,{
        unsigned int ii;
        unsigned int len;
        HKEY hkey = NULL;
        char *base = strpbrk(pathz,"\\/");
        if (base==NULL) len = strlen(pathz);
        else { len=base-pathz; base++; }
        for (ii=0; ii<sizeof(roots)/sizeof(*roots); ii++)
          if (0 == strncmp(roots[ii].name,pathz,len)) { # equal?
            hkey = roots[ii].hkey;
            break;
          }
        if (hkey==NULL) { SetLastError(ERROR_PATH_NOT_FOUND); OS_error(); }
        open_reg_key(hkey,base,direction,if_not_exists,(HKEY*)&ret_handle);
      });
    }
  } else
  #endif
  #ifdef OPEN_LDAP
  if (eq(type,S(Kldap))) {
    ldap_init();
  } else
  #endif
  #ifdef GNOME_CONF
  if (eq(root,S(Kgnome))) {
    # do nothing - gnome-conf is stateless
  } else
  #endif
  { # invalid type
    pushSTACK(type);            # slot DATUM of         TYPE-ERROR
    pushSTACK(O(type_dir_key)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(type);
    pushSTACK(S(dir_key));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: ~ is not a ~")
           );
  }
  # create the DIR-KEY
  var object dkey = NIL;
  dkey = allocate_dir_key();
  TheDirKey(dkey)->type = S(Kwin32);
  TheDirKey(dkey)->closed_p = FALSE;
  TheDirKey(dkey)->direction = direction_arg;
  TheDirKey(dkey)->handle = ret_handle;
  if (dir_key_p(root)) {
    pushSTACK(TheDirKey(root)->path);
    #ifdef WIN32_NATIVE
    if (eq(TheDirKey(root)->type,S(Kwin32))) pushSTACK(O(backslash_string));
    else
    #endif
      pushSTACK(O(slash_string));
    pushSTACK(path);
    TheDirKey(dkey)->path = string_concat(3);
  } else {
    TheDirKey(dkey)->path = path;
  }
  pushSTACK(dkey);
  pushSTACK(L(dir_key_close));
  funcall(L(finalize),2); # (FINALIZE dir-key #'dir-key-close)
  skipSTACK(4);
  value1 = dkey;
  mv_count = 1;
}

# The common code in DIR-KEY-SUBKEYS & DIR-KEY-ATTRIBUTES
#define MAKE_OBJECT_LIST(count,get_obj)                                   \
  object dkey = popSTACK();                                               \
  object ret = NIL;                                                       \
  LONG status;                                                            \
  DWORD n_obj;                                                            \
  DWORD maxlen;                                                           \
  HKEY hkey;                                                              \
  test_dir_key(dkey,TRUE);                                                \
  hkey = (HKEY)TheDirKey(dkey)->handle;                                   \
  begin_system_call();                                                    \
  status = count;                                                         \
  if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }      \
  if (n_obj>0) {                                                          \
    unsigned int ii;                                                      \
    char * buf = (char*)alloca(++maxlen); /* one extra char for #\Null */ \
    object tail = NIL;                                                    \
    for (ii=0; ii<n_obj; ii++) {                                          \
      DWORD len = maxlen,                                                 \
      status = get_obj;                                                   \
      if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }  \
      if (nullp(tail)) tail = ret = allocate_cons();                      \
      else tail = Cdr(tail) = allocate_cons();                            \
      Cdr(tail) = NIL;                                                    \
      Car(tail) = asciz_to_string(buf,O(misc_encoding));                  \
    }                                                                     \
  }                                                                       \
  end_system_call();                                                      \
  value1 = ret;                                                           \
  mv_count = 1

LISPFUNN(dir_key_subkeys,1)
# (LISP:DIR-KEY-SUBKEYS key)
# return the list of subkey names of the given KEY
{
  MAKE_OBJECT_LIST(RegQueryInfoKey(hkey,NULL,NULL,NULL,&n_obj,&maxlen,
                                   NULL,NULL,NULL,NULL,NULL,NULL),
                   RegEnumKey(hkey,ii,buf,len));
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
{
  object stack = ITST_STACK(state);
  uintL depth = 0;
  stack = nreverse(stack);
  while(!eq(stack,NIL)) {
    if (depth) {
      depth++;
      pushSTACK(O(backslash_string));
    }
    pushSTACK(NODE_NAME(Car(stack)));
    stack = Cdr(stack);
    depth++;
  }
  ITST_STACK(state) = nreverse(ITST_STACK(state));
  var object res = string_concat(depth);
  return res;
}

LISPFUNN(dkey_search_iterator,3)
# return a simple vector with the iteration state
# < dkey path scope
# > #(dkey init-path scope (node ...))
{
  object scope = popSTACK();
  object path = popSTACK();
  object dkey = popSTACK();
  test_dir_key(dkey,TRUE);
  if (!stringp(path)) fehler_string(path);
  if (!eq(scope,S(Kself)) && !eq(scope,S(Klevel)) && !eq(scope,S(Ktree))) {
    pushSTACK(scope);         # slot DATUM of         TYPE-ERROR
    pushSTACK(O(type_scope)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(scope);
    pushSTACK(S(Kscope));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: ~ is not a ~")
           );
  }
  value1 = allocate_vector(4);
  ITST_DKEY(value1) = dkey;
  ITST_PATH(value1) = path;
  ITST_SCOPE(value1) = scope;
  ITST_STACK(value1) = T;
  mv_count;
}

local object init_iteration_node (object state,object subkey)
# open HANDLE to point to DKEY\\PATH
# compute KEY_S, ATT_S and DAT_S
# return the full current path (itst_current)
{
  object dkey = ITST_DKEY(state);
  object path = ITST_PATH(state);
  object stack = allocate_cons();
  object node = allocate_vector(7);
  test_dir_key(dkey,TRUE);
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
  var object handle = allocate_fpointer(NULL);
  var Dir_Key dk = TheDirKey(dkey);
  var Fpointer fp = TheFpointer(handle);
  NODE_HANDLE(node) = handle;
  pushSTACK(handle);
  pushSTACK(L(dir_key_close));
  funcall(L(finalize),2); # (FINALIZE handle #'dir-key-close)
  var object full_path = itst_current(state);
  with_string_0(full_path,O(misc_encoding),pathz,{
    open_reg_key((HKEY)(dk->handle),pathz,parse_direction(&(dk->direction)),
                 IDNE_ERROR,(HKEY*)&(fp->fp_pointer));
  });
  var DWORD k_size;
  var DWORD a_size;
  var DWORD d_size;
  SYSCALL_WIN32(RegQueryInfoKey((HKEY)(fp->fp_pointer),NULL,NULL,NULL,NULL,
                                &k_size,NULL,NULL,&a_size,&d_size,NULL,NULL));
  NODE_KEY_S(node) = fixnum(k_size+1);
  NODE_ATT_S(node) = fixnum(a_size+1);
  NODE_DAT_S(node) = fixnum(d_size+1);
  return full_path;
}

local object state_next_key (object state)
# return the next key of the current state or NIL
# the stack doesn't change, but the HKEY is closed
{
  object stack = ITST_STACK(state);
  if (!eq(stack,NIL)) {
    object node = ITST_NODE(state);
    uintL keynum = posfixnum_to_L(NODE_KEY(node));
    uintL keylen = posfixnum_to_L(NODE_KEY_S(node));
    char* buffer = (char*)alloca(keylen);
    Fpointer fp  = TheFpointer(NODE_HANDLE(node));
    if (fp->fp_pointer) {
      DWORD status = RegEnumKey((HKEY)(fp->fp_pointer),keynum,buffer,keylen);
      if (status == ERROR_SUCCESS) {
        NODE_KEY(node) = fixnum_inc(NODE_KEY(node),1);
        return asciz_to_string(buffer,O(misc_encoding));
      } else {
        SYSCALL_WIN32(RegCloseKey((HKEY)(fp->fp_pointer)));
        fp->fp_pointer = NULL;
        return NIL;
      }
    } else return NIL;
  } else return NIL;
}

LISPFUNN(dkey_search_next_key,1)
# (SYS::DKEY-SEARCH-NEXT-KEY state)
# return the next key of this iteration
{
  object state = popSTACK();
  object dkey  = ITST_DKEY(state);
  object path  = ITST_PATH(state);
  object scope = ITST_SCOPE(state);
  object stack = ITST_STACK(state);
  test_dir_key(dkey,TRUE);
  if (!stringp(path)) fehler_string(path);
  if (eq(scope,S(Kself))) {         # just the top
    if (eq(stack,T)) value1 = init_iteration_node(state,NIL); # first call
    else value1 = NIL;
  } else if (eq(scope,S(Klevel))) { # the children
    if (eq(stack,T)) init_iteration_node(state,NIL);
    value1 = state_next_key(state);
  } else if (eq(scope,S(Ktree))) {  # the whole subtree
    if (eq(stack,T)) value1 = init_iteration_node(state,NIL); # first call
    else {                          # find the next node and return it
      object subkey;
      do {
        subkey = state_next_key(state); # subkey==NIL ==> HANDLE is closed
        stack = ITST_STACK(state);
      } while (eq(subkey,NIL) && !eq(stack,NIL) && # pop stack
               !eq(ITST_STACK(state) = Cdr(stack),NIL));
      if (eq(subkey,NIL)) value1 = NIL;
      else value1 = init_iteration_node(state,subkey);
    }
  } else {
    pushSTACK(scope);         # slot DATUM of         TYPE-ERROR
    pushSTACK(O(type_scope)); # slot EXPECTED-TYPE of TYPE-ERROR
    pushSTACK(scope);
    pushSTACK(S(Kscope));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: ~ is not a ~")
           );
  }
  mv_count = 1;
}

LISPFUNN(dkey_search_next_att,1)
# (SYS::DKEY-SEARCH-NEXT-ATT state)
# return the next attribute and its value of this iteration
{
  object state = popSTACK();
  object stack = ITST_STACK(state);
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
                                  attnum,att,&attlen,NULL,&type,dat,&size);
  if (status == ERROR_SUCCESS) {
    value1 = asciz_to_string(att,O(misc_encoding));
    value2 = registry_value_to_object(type,size,dat);
    NODE_ATT(node) = fixnum_inc(NODE_ATT(node),1);
  } else
    value1 = value2 = NIL;
  mv_count = 2;
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
{
  var object default_value = popSTACK();
  var object name = popSTACK();
  var object dkey = popSTACK();
  test_dir_key(dkey,TRUE);
  if (!stringp(name)) fehler_string(name);
  with_string_0(name,O(misc_encoding),namez,{
    DWORD status;
    DWORD type;
    DWORD size;
    begin_system_call();
    status = RegQueryValueEx((HKEY)TheDirKey(dkey)->handle,namez,
                             NULL,NULL,NULL,&size);
    if (status != ERROR_SUCCESS) {
      if ((status == ERROR_FILE_NOT_FOUND) && !eq(default_value,unbound)) {
        value1 = default_value;
        end_system_call();
        goto end;
      }
      SetLastError(status); OS_error();
    }
    var char* buffer = (char*)alloca(++size); # one extra char for #\Null
    status = RegQueryValueEx((HKEY)TheDirKey(dkey)->handle,namez,
                             NULL,&type,buffer,&size);
    if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }
    end_system_call();
    value1 = registry_value_to_object(type,size,buffer);
  });
 end:
  mv_count = 1;
}

LISPFUNN(set_dkey_value,3)
# (SYS::SET-DKEY-VALUE key name value)
# set the given NAME in the KEY to the VALUE
{
  var object value = popSTACK();
  var object name = popSTACK();
  var object dkey = popSTACK();
  test_dir_key(dkey,TRUE);
  if (!stringp(name)) fehler_string(name);
  with_string_0(name,O(misc_encoding),namez, {
    if (stringp(value)) {
      with_string_0(value,O(misc_encoding),valz, {
        SYSCALL_WIN32(RegSetValueEx((HKEY)TheDirKey(dkey)->handle,namez,0,
                                    REG_SZ,valz,strlen(valz)));
      });
    } else if (integerp(value)) {
      DWORD word = I_to_UL(value);
      SYSCALL_WIN32(RegSetValueEx((HKEY)TheDirKey(dkey)->handle,namez,0,
                                  REG_DWORD,(char*)&word,sizeof(DWORD)));
    } else if (bit_vector_p(Atype_8Bit,value)) {
      uintL idx = 0;
      uintL len = vector_length(value);
      object arr = (array_simplep(value) ? value :
                    iarray_displace_check(value,len,&idx));
      SYSCALL_WIN32(RegSetValueEx((HKEY)TheDirKey(dkey)->handle,namez,0,
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
  var object dkey = popSTACK();                                         \
  test_dir_key(dkey,TRUE);                                              \
  if (!stringp(name)) fehler_string(name);                              \
  with_string_0(name,O(misc_encoding),namez,{                           \
    SYSCALL_WIN32(call((HKEY)TheDirKey(dkey)->handle,namez));           \
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
{
  object dkey = popSTACK();
  test_dir_key(dkey,TRUE);
  var HKEY hkey = (HKEY)TheDirKey(dkey)->handle;
  var char* class_name;
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
  class_name = (class_length>0 ? (char*)alloca(++class_length) : NULL);
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
