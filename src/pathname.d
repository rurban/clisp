# Pathnames for CLISP
# Bruno Haible 1990-2002
# Logical Pathnames: Marcus Daniels 16.9.1994
# ANSI compliance, bugs: Sam Steingold 1998-2002
# German comments translated into English: Stefan Kain 2002-01-03

#include "lispbibl.c"

#ifdef HAVE_DISASSEMBLER
  #include <string.h> # declares strlen()
  #ifdef RETSTRLENTYPE # unless strlen() is a macro
    extern_C RETSTRLENTYPE strlen (STRLEN_CONST char* s);
  #endif
#endif

#ifdef UNIX_LINUX
  #include <stdio.h> # declares sprintf()
#endif

# enable the following #define to debug pathname translations
# setting DEBUG_TRANSLATE_PATHNAME to a larger value results in more output
# WARNING: PRIN1 can trigger GC! BEWARE!
# define DEBUG_TRANSLATE_PATHNAME 1
#if DEBUG_TRANSLATE_PATHNAME
#include <stdio.h>
#define string_concat(x) (asciz_out_2("[%d]string_concat(%d)\n",__LINE__,x),(string_concat)(x))
local object debug_output (const char* label,object obj,const int pos) {
  # fprintf(stdout,"[%d] %s: ",pos,label);fflush(stdout);
  asciz_out_1("[%d] ",pos); asciz_out_s("%s: ",label);
  pushSTACK(obj);pushSTACK(subr_self);
  # gar_col();fprintf(stdout,"[gc] ");fflush(stdout);
  object_out(STACK_1);
  subr_self = popSTACK();
  return popSTACK();
}
local void debug_printf (const char* label,object obj,const int pos) {
  fprintf(stdout,"[%d] %s: %s\n",pos,label,
          (eq(obj,unbound) ? "#<UNBOUND>" : eq(obj,NIL) ? "NIL" :
           eq(obj,T) ? "T" : stringp(obj) ? "string" :
           logpathnamep(obj) ? "logical pathname" : pathnamep(obj) ? "path" :
           eq(obj,S(Knewest)) ? ":NEWEST" : symbolp(obj) ? "a symbol" :
           consp(obj) ? "a list" : numberp(obj) ? "a number" : "???"));
}
# define DOUT(l,o) printf("[%d] %s %s\n",__LINE__,l,#o);gar_col()
#define DOUT0(label,object) debug_output(label #object,object,__LINE__)
#define DOUT(label,object) object=debug_output(label #object,object,__LINE__)
#define SDOUT(label,object) debug_printf(label #object,object,__LINE__)
#else
#define DOUT(l,o)
#define SDOUT(l,o)
#endif

# =============================================================================
#                       Low level functions

# UP: Tests whether a pathname is possibly a symlink.
# possible_symlink(path)
#ifdef UNIX_LINUX
local inline bool possible_symlink (const char* path) {
  # In Linux 2.0.35, /proc/<pid>/{cwd,exe,root} and /proc/<pid>/fd/<n>
  # are symlinks pointing to void. Treat them like non-symlinks, in order
  # to avoid errors.
  if (path[0]=='/'
      && path[1]=='p' && path[2]=='r' && path[3]=='o' && path[4]=='c'
      && path[5]=='/'
      && (path[6]>='0' && path[6]<='9'))
    return false;
  return true;
}
#else
  #define possible_symlink(path)  true
#endif

#ifdef UNIX_LINUX
# The Linux /proc filesystem has some symlinks whose readlink value is
# zero-terminated: /proc/self in Linux 2.0.35, /proc/<pid>/fd/<n> in
# Linux 2.2.2. Remove this extraneous trailing zero byte.
local inline int my_readlink (const char* path, char* buf, size_t bufsiz) {
  var int linklen = readlink(path,buf,bufsiz);
  if (linklen > 0 && buf[linklen-1] == '\0')
    linklen--;
  return linklen;
}
#define readlink  my_readlink
#endif

#ifdef UNIX
  # library-function realpath implementation:
  # [Copyright: SUN Microsystems, B. Haible]
  # TITLE
  #   REALPATH(3)
  # SYNOPSIS
  #   char* realpath (const char* path, char resolved_path[MAXPATHLEN]);
  # DESCRIPTION
  #   realpath() expands all symbolic links  and  resolves  refer-
  #   ences  to '/./', '/../' and extra '/' characters in the null
  #   terminated string named by path and stores the canonicalized
  #   absolute pathname in the buffer named by resolved_path.  The
  #   resulting path will have no symbolic links  components,  nor
  #   any '/./' or '/../' components.
  # RETURN VALUES
  #   realpath() returns a pointer to the  resolved_path  on  suc-
  #   cess.   On  failure, it returns NULL, sets errno to indicate
  #   the error, and places in resolved_path the absolute pathname
  #   of the path component which could not be resolved.
#define realpath my_realpath # avoid conflict with Consensys realpath declaration
local char* realpath (const char* path, char* resolved_path) {
  # Method: use getwd and readlink.
  var char mypath[MAXPATHLEN];
  var int symlinkcount = 0; # the number of symbolic links so far
  var char* resolved_limit = &resolved_path[MAXPATHLEN-1];
  # Valid pointers are those with resolved_path <= ptr <= resolved_limit.
  # in *resolved_limit at most one null byte.
  # (similarly with mypath.)
  var char* resolve_start;
  {
    var char* resolved_ptr = resolved_path; # always <= resolved_limit
    # poss. use Working-Directory:
    if (!(path[0]=='/')) { # not an absolute pathname?
      if (getwd(resolved_path) == NULL)
        return NULL;
      resolved_ptr = resolved_path;
      while (*resolved_ptr) {
        resolved_ptr++;
      }
      if (resolved_ptr < resolved_limit) {
        *resolved_ptr++ = '/';
      }
      resolve_start = resolved_ptr;
    } else {
      resolve_start = resolved_ptr = &resolved_path[0];
    }
    # copy the path:
    var const char* path_ptr = path;
    while ((resolved_ptr < resolved_limit) && *path_ptr) {
      *resolved_ptr++ = *path_ptr++;
    }
    # finish with '/' and a null:
    if (resolved_ptr < resolved_limit) {
      *resolved_ptr++ = '/';
    }
    *resolved_ptr = 0;
  }
  # Now start in resolved_path at resolve_start.
  var char* from_ptr = resolve_start;
  var char* to_ptr = resolve_start;
  while ((to_ptr < resolved_limit) && (*from_ptr)) {
    # so far the path in  resolved_path[0]...to_ptr[-1]
    # has the shape '/subdir1/subdir2/.../txt',
    # whereas 'txt' is poss. empty, but no subdir is empty.
    var char next = *from_ptr++; *to_ptr++ = next;
    if ((next == '/') && (to_ptr > resolved_path+1)) {
      # to_ptr[-1]='/'  ->  resolve Directory ...to_ptr[-2] :
      var char* last_subdir_end = &to_ptr[-2];
      switch (*last_subdir_end) {
        case '/':
          #ifdef PATHNAME_UNIX_UNC
          if (to_ptr > resolved_path+2)
          #endif
            # '//' is simplified to '/' :
            to_ptr--;
          break;
        case '.':
          {
            var char* last_subdir_ptr = &last_subdir_end[-1];
            if (to_ptr > resolved_path+2) {
              if (*last_subdir_ptr == '.') {
                if ((to_ptr > resolved_path+4)
                    && (*--last_subdir_ptr == '/')) {
                  # last subdir was '/../'
                  # Therefore remove the subdir in front of it:
                  while ((last_subdir_ptr > resolved_path)
                         && !(*--last_subdir_ptr == '/'));
                  to_ptr = last_subdir_ptr+1;
                }
              } else if (*last_subdir_ptr == '/') {
                # last subdir was '/./'
                # remove:
                to_ptr = last_subdir_end;
              }
            }
          }
          break;
        default:
          # after a normal subdir
          #ifdef HAVE_READLINK
          if (possible_symlink(resolved_path)) {
            # read symbolic link:
            to_ptr[-1]=0; # replace '/' with 0
            #ifdef UNIX_CYGWIN32
            # readlink() does not work right on NFS mounted directories
            # (it returns -1,ENOENT or -1,EIO).
            # So check for a directory first.
            var struct stat statbuf;
            if (lstat(resolved_path,&statbuf) < 0)
              return NULL; # error
            if (S_ISDIR(statbuf.st_mode)) {
              # directory, not a symbolic link
              to_ptr[-1] = '/'; # insert the '/' again
            } else if (!S_ISLNK(statbuf.st_mode)) {
              # something else, but not a directory or symbolic link.
              errno = ENOTDIR;
              return NULL;
            } else
            #endif
              {
                var int linklen =
                  readlink(resolved_path,mypath,sizeof(mypath)-1);
                if (linklen >=0) {
                  # was a symbolic link
                  if (++symlinkcount > MAXSYMLINKS) {
                    errno = ELOOP_VALUE; return NULL;
                  }
                  # append the still to be resolved part of path
                  # to the link-content:
                  {
                    var char* mypath_ptr = &mypath[linklen]; # here is room
                    var char* mypath_limit = &mypath[MAXPATHLEN-1]; # up to here
                    if (mypath_ptr < mypath_limit) { *mypath_ptr++ = '/'; } # first, append a '/'
                    # then the rest:
                    while ((mypath_ptr <= mypath_limit)
                           && (*mypath_ptr = *from_ptr++))
                      { mypath_ptr++; }
                    *mypath_ptr = 0; # and conclude wit 0
                  }
                  # this replaces resp. completes the path:
                  if (mypath[0] == '/') {
                    # replaces the path:
                    from_ptr = &mypath[0]; to_ptr = resolved_path;
                    while ((*to_ptr++ = *from_ptr++));
                    from_ptr = resolved_path;
                  } else {
                    # completes the path:
                    # disrcard link-name. Therefore search for the last '/':
                    {
                      var char* ptr = &to_ptr[-1];
                      while ((ptr > resolved_path) && !(ptr[-1] == '/')) { ptr--; }
                      from_ptr = ptr;
                    }
                    {
                      var char* mypath_ptr = &mypath[0]; to_ptr = from_ptr;
                      while ((to_ptr <= resolved_limit) && (*to_ptr++ = *mypath_ptr++));
                    }
                  }
                  to_ptr = from_ptr;
                } else {
                  #if defined(UNIX_IRIX)
                  if ((errno == EINVAL) || (errno == ENXIO))
                  #elif defined(UNIX_MINT)
                  if ((errno == EINVAL) || (errno == EACCESS))
                  #elif defined(UNIX_CYGWIN32)
                  if ((errno == EINVAL) || (errno == EACCES))
                  #else
                  if (errno == EINVAL)
                  #endif
                    # no symbolic link
                    to_ptr[-1] = '/'; # insert the '/' again
                  else
                    return NULL; # error
                }
              }
          }
          #endif
          break;
      }
    }
  } # go for the next subdir
  # discard a '/' at the tail:
  if ((to_ptr[-1] == '/')
      #ifdef PATHNAME_UNIX_UNC
      && (to_ptr > resolved_path+2)
      #else
      && (to_ptr > resolved_path+1)
      #endif
      )
    to_ptr--;
  to_ptr[0] = 0; # conclude with 0
  return resolved_path; # finished
}
#endif
#ifdef RISCOS
  # SYNOPSIS
  #   char* realpath (char* path, char resolved_path[MAXPATHLEN]);
  # RETURN VALUES
  #   realpath() returns a pointer to the resolved_path on success.
  #   On failure, it returns NULL and sets errno to indicate the error.
#define realpath my_realpath # there is no consensus on realpath declaration
#include <sys/os.h>
local char* realpath (char* path, char* resolved_path) {
  var int handle;
  var int r[10];
  #if 0 # Both of these implementations should work.
  if (os_fopen(0x40,path,&handle))
    return NULL;
  r[0] = 7; r[1] = handle; r[2] = (long)resolved_path; r[5] = MAXPATHLEN;
  os_swi(9,r);
  os_fclose(handle);
  #else
  var os_error* err;
  r[0] = 37; r[1] = (long)path; r[2] = (long)resolved_path;
  r[3] = 0; r[4] = 0; r[5] = MAXPATHLEN;
  err = os_swi(0x29,r);
  if (err) {
    __seterr(err); return NULL;
  }
  #endif
  if (r[5] <= 0) {
    errno = ENOMEM /* ENAMETOOLONG would be better, but does not yet exist */;
    return NULL;
  }
  return resolved_path;
}
#endif

# Creates a new subdirectory.
# make_directory(pathstring);
# > pathstring: result of shorter_directory(...)
# > STACK_0: pathname
local inline void make_directory (char* pathstring) {
 #ifdef AMIGAOS
  set_break_sem_4();
  begin_system_call();
  {
    var BPTR lock = CreateDir(pathstring); # create sub-directory
    if (lock==BPTR_NULL) {
      end_system_call(); OS_file_error(STACK_0);
    }
    UnLock(lock); # release lock
  }
  end_system_call();
  clr_break_sem_4();
 #endif
 #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
  begin_system_call();
  if (mkdir(pathstring,0777)) { # create sub-directory
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! CreateDirectory(pathstring,NULL) ) { # create sub-directory
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

# Deletes a subdirectory.
# delete_directory(pathstring);
# > pathstring: result of shorter_directory(...)
# > STACK_0: pathname
local inline void delete_directory (char* pathstring) {
 #ifdef AMIGAOS
  # yet test, if it is really a directory and not a file??
  begin_system_call();
  if (! DeleteFile(pathstring) ) { # delete sub-directory
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #if defined(UNIX) || defined(EMUNIX)
  begin_system_call();
  if (rmdir(pathstring)) { # delete sub-directory
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef RISCOS
  begin_system_call();
  if (unlink(pathstring)) { # delete sub-directory
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! RemoveDirectory(pathstring) ) { # delete sub-directory
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

#if defined(MSDOS) || defined(WIN32_NATIVE)
# Changes the operating system's current directory.
# change_directory(pathstring);
# > pathstring: directory, ASCIZ-String
# > STACK_0: pathname
local inline void change_current_directory (char* pathstring) {
  begin_system_call();
 #ifdef MSDOS
  if (!( chdir(pathstring) ==0)) {
    end_system_call(); OS_file_error(STACK_0);
  }
 #endif
 #ifdef WIN32_NATIVE
  if (!SetCurrentDirectory(pathstring)) {
    end_system_call(); OS_file_error(STACK_0);
  }
 #endif
  end_system_call();
}
#endif

# Delete a file.
# delete_existing_file(pathstring);
# It is known that the file exists.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
local inline void delete_existing_file (char* pathstring) {
 #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
  begin_system_call();
  if (!( unlink(pathstring) ==0)) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #if defined(AMIGAOS) || defined(WIN32_NATIVE)
  begin_system_call();
  if (! DeleteFile(pathstring) ) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

#ifdef WIN32_NATIVE
#define WIN32_ERROR_NOT_FOUND (GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND || GetLastError()==ERROR_BAD_NETPATH)
#endif

# Delete a file.
# delete_file_if_exists(pathstring);
# No error is signaled if the file does not exist.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
# < result: whether the file existed
local inline bool delete_file_if_exists (char* pathstring) {
  var bool exists = true;
 #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
  begin_system_call();
  if (!( unlink(pathstring) ==0)) {
    if (!(errno==ENOENT)) { # not found -> OK
      end_system_call(); OS_file_error(STACK_0); # report other error
    }
    exists = false;
  }
  end_system_call();
 #endif
 #ifdef AMIGAOS
  begin_system_call();
  if (! DeleteFile(pathstring) ) {
    if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) { # not found -> OK
      end_system_call(); OS_file_error(STACK_0); # report other error
    }
    exists = false;
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! DeleteFile(pathstring) ) {
    if (!WIN32_ERROR_NOT_FOUND) {
      end_system_call(); OS_file_error(STACK_0);
    }
    exists = false;
  }
  end_system_call();
 #endif
  return exists;
}

# Delete a file being the target of a subsequent rename.
# delete_file_before_rename(pathstring);
# No error is signaled if the file does not exist.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
local inline void delete_file_before_rename (char* pathstring) {
 #if !defined(UNIX) # rename() on Unix does it automatically
  delete_file_if_exists(pathstring);
 #endif
}

# Rename a file.
# rename_existing_file(old_pathstring,new_pathstring);
# It is known that the old_pathstring exists.
# On platforms except UNIX, it is known that new_pathstring does not exist.
# > old_pathstring: old file name, ASCIZ-String
# > new_pathstring: new file name, ASCIZ-String
# > STACK_0: pathname
local inline void rename_existing_file (char* old_pathstring,
                                        char* new_pathstring) {
 #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
  begin_system_call();
  if ( rename(old_pathstring,new_pathstring) <0) { # rename file
    end_system_call(); OS_file_error(STACK_0); # report error
  }
  end_system_call();
 #endif
 #ifdef AMIGAOS
  begin_system_call();
  if (! Rename(old_pathstring,new_pathstring) ) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! MoveFile(old_pathstring,new_pathstring) ) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

# Rename a file.
# rename_file_to_nonexisting(old_pathstring,new_pathstring);
# It is known that new_pathstring does not exist.
# > old_pathstring: old file name, ASCIZ-String
# > new_pathstring: new file name, ASCIZ-String
# > STACK_3: old pathname
# > STACK_1: new pathname
local inline void rename_file_to_nonexisting (char* old_pathstring,
                                              char* new_pathstring) {
 #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
  begin_system_call();
  if ( rename(old_pathstring,new_pathstring) <0) { # rename file
    if (errno==ENOENT) {
      end_system_call(); OS_file_error(STACK_3);
    } else {
      end_system_call(); OS_file_error(STACK_1);
    }
  }
  end_system_call();
 #endif
 #ifdef AMIGAOS
  begin_system_call();
  if (! Rename(old_pathstring,new_pathstring) ) {
    if (IoErr()==ERROR_OBJECT_NOT_FOUND) {
      end_system_call(); OS_file_error(STACK_3);
    } else {
      end_system_call(); OS_file_error(STACK_1);
    }
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! MoveFile(old_pathstring,new_pathstring) ) {
    if (WIN32_ERROR_NOT_FOUND) {
      end_system_call(); OS_file_error(STACK_3);
    } else {
      end_system_call(); OS_file_error(STACK_1);
    }
  }
  end_system_call();
 #endif
}


# =============================================================================
#                         P A T H N A M E S

# All simple-strings occurring in pathnames are in fact normal-simple-strings.

#ifdef PATHNAME_AMIGAOS
# components:
# HOST          always NIL
# DEVICE        NIL or Simple-String
# DIRECTORY     (Startpoint . Subdirs) with
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (means "**" or "...", all subdirectories) or
#                subdir = :PARENT (means "/" instead of "subdir/") or
#                subdir = Simple-String, poss. with Wildcard-Character ? and *
# NAME          NIL or
#               Simple-String, poss. wit Wildcard-Character ? and *
#               (also :WILD on input)
# TYPE          NIL or
#               Simple-String, poss. with Wildcard-Character ? and *
#               (also :WILD on input)
# VERSION       always NIL (also :WILD or :NEWEST on input)
# Constraint: Startpoint = :RELATIVE only, if Device = NIL;
#             for a specified device there are only absolute Pathnames!
# An AMIGAOS-Filename is split in Name and Type as follows:
#   if there is no '.' in filename: Name = everything, Type = NIL,
#   if there is a '.' in filename: Name = everything in front, Type = everything behind the last '.' .
# capital-/small letters within the Strings are ignored for comparison,
# but apart from that no conversion between capital and small letters takes place.
# If a pathname must be completely specified (no wildcards),
# :WILD, :WILD-INFERIORS is not permitted; no wildcard-characters in the
# Strings, no NIL at NAME poss. too.
# External Notation:  device:sub1.typ/sub2.typ/name.typ
# with Defaults:             sub1.typ/sub2.typ/name.typ
# or                                           name.typ
# or                         sub1.typ/ ** /sub3.typ/x*.lisp  (without Spaces!)
# or similar.
# Formal:
#   ch ::= any Character except ':','/' and '*','?'
#   name ::= {ch}+
#   device ::= [ <empty> | ':' | name ':' ]
#              ; empty = current device, relative from the current directory
#              ; ':'  = current device, absolute (from root for disks)
#              ; name ':' = specified device, absolute (from root for disks)
#   subdir ::= [ <empty> | name ]                ; empty = '..'
#   pathname ::= device { subdir '/' }* name
# Examples:
#   String        Device    Directory                unser Pathname
#   ------        ------    ---------                --------------
#   'c:foo'       'C',     device->foo               "c" (:ABSOLUTE "foo")
#   'c:foo/'      'C',     device->foo               "c" (:ABSOLUTE "foo")
#   'c:foo/bar'   'C',     device->foo->bar          "c" (:ABSOLUTE "foo" "bar")
#   'c:/foo'      'C',     device->up->foo           "c" (:ABSOLUTE :PARENT "foo")
#   'c:'          'C',     device                    "c" (:ABSOLUTE)
#   ':foo'        current, device->root->foo         NIL (:ABSOLUTE "foo")
#   'foo'         current, device->foo               NIL (:RELATIVE "foo")
#   '/foo'        current, device->up->foo           NIL (:RELATIVE :PARENT "foo")
#   '//foo/bar'   current, device->up->up->foo->bar  NIL (:RELATIVE :PARENT :PARENT "foo" "bar")
#   ''            current, device                    NIL (:RELATIVE)
# A '/' can be appended to a pathstring, that is non-empty and that does
#  not end with ':' or '/', without changing its semantics.
# This '/' must be appended, before a further non-empty
# component can be appended.
# Appending a '/' to a pathstring, that is empty or that ends with ':' or '/' ,
# means to ascend to the Parent Directory!
# We interprete each pathstring, that is empty or that ends with ':' or '/' ,
# as directory-pathname (with Name=NIL and Type=NIL) .
#endif

#ifdef PATHNAME_UNIX
# Components:
# HOST          always NIL
# DEVICE        always NIL
# DIRECTORY     (Startpoint . Subdirs) whereas
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (means "**" or "...", all subdirectories) or
#                subdir = Simple-String, poss. with wildcard-character ? and *
# NAME          NIL or
#               Simple-String, poss. with wildcard-character ? and *
#               (also :WILD on input)
# TYPE          NIL or
#               Simple-String, poss. with wildcard-character ? and *
#               (also :WILD on input)
# VERSION       always NIL (also :WILD or :NEWEST on input)
# A UNIX-filename is split in Name and Type as follows:
#   if there is no '.' in Filename: Name = everything, Type = NIL,
#   if there is '.' in Filename: Name = everything in front of it, Type = everything behind the last '.' .
# If a pathname must be completely specified (no wildcards),
#  :WILD, :WILD-INFERIORS are not allowed, no wildcard-characters in the
# Strings, at NAME poss. also not NIL.
# External Notation:  server:/sub1.typ/sub2.typ/name.typ
# with Defaults:             /sub1.typ/sub2.typ/name.typ
# or                                            name.typ
# or                         /sub1.typ/ ** /sub3.typ/x*.lisp  (without Spaces!)
# or similar.
# If NAME starts with a dot, (parse-namestring (namestring pathname)) will not
# be the same as pathname.
#endif

#ifdef PATHNAME_OS2
# Components:
# HOST          always NIL
# DEVICE        NIL or :WILD or "A"|...|"Z"
# DIRECTORY     (Startpoint . Subdirs) whereas
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (means "**" or "...", all Subdirectories) or
#                subdir = Simple-String, poss. with Wildcard-Characters ? and *
# NAME          NIL or
#               Simple-String, poss. with Wildcard-Character ? and *
#               (also :WILD on input)
# TYPE          NIL or
#               Simple-String, poss. with Wildcard-Character ? and *
#               (also :WILD on input)
# VERSION       always NIL (also :WILD or :NEWEST on input)
# An OS/2-Filename is split into Name and Type as follows:
#   if there is no '.' in filename: Name = everything, Type = NIL,
#   if there is a '.' in filename: Name = everything in front of, Type = everything behind the last '.' .
# If a Pathname must be completely specified (no Wildcards),
# then :WILD, :WILD-INFERIORS are not allowed, no Wildcard-Characters in the
# Strings, at NAME poss. also not NIL.
# External notation:       A:\sub1.typ\sub2.typ\name.typ
# with Defaults:             \sub1.typ\sub2.typ\name.typ
# or                                            name.typ
# or                       *:\sub1.typ\**\sub3.typ\x*.lisp
# or similar.
# Instead of '\'  - traditionally on DOS - also '/' is allowed.
#endif

#ifdef PATHNAME_WIN32
# Components:
# HOST          NIL or Simple-String (Wildcard-Characters are without meaning)
# DEVICE        NIL or :WILD or "A"|...|"Z"
# DIRECTORY     (Startpoint . Subdirs) whereas
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (means "**" or "...", all Subdirectories) or
#                subdir = Simple-String, poss. with Wildcard-Character ? and *
# NAME          NIL or
#               Simple-String, poss. with Wildcard-Character ? and *
#               (also :WILD on input)
# TYPE          NIL or
#               Simple-String, poss. with Wildcard-Character ? and *
#               (also :WILD on input)
# VERSION       always NIL (also :WILD or :NEWEST on input)
# If HOST is non-NIL, DEVICE must be NIL.
# A WIN32-Filename is split into Name and Type as follows:
#   if there is no '.' in Filename: Name = everything, Type = NIL,
#   if there is a '.' in Filename: Name = everything in front of, Type = everything behind the last '.' .
# If a Pathname must be completely specified (no Wildcards),
# then :WILD, :WILD-INFERIORS are not allowed, no Wildcard-Characters in the
# Strings, at NAME poss. also not NIL.
# External notation:       A:\sub1.typ\sub2.typ\name.typ
# with Defaults:             \sub1.typ\sub2.typ\name.typ
# or                                            name.typ
# or                       *:\sub1.typ\**\sub3.typ\x*.lisp
# or similar.
# Instead of '\'  - traditionally on DOS - also '/' is allowed.
# If HOST is non-NIL and the DIRECTORY's Startpoint is not :ABSOLUTE,
# (parse-namestring (namestring pathname)) will not be the same as pathname.
#endif

#ifdef PATHNAME_RISCOS
#
# Peter Burwood <clisp@arcangel.demon.co.uk> writes:
#
# RISC OS provides several filing systems as standard (ADFS, IDEFS, NetFS,
# RamFS, NetPrint) and support for extra filing systems (DOSFS, ResourceFS and
# DeviceFS).
#
# A module called FileSwitch is at the centre of all filing system operation
# in RISC OS. FileSwitch provides a common core of functions used by all
# filing systems. It only provides the parts of these services that are device
# independent. The device dependant services that control the hardware are
# provided by separate modules, which are the actual filing systems.
# FileSwitch keeps track of active filing systems and switches betwen them as
# necessary.
#
# One of the filing system modules that RISC OS provides is FileCore. It takes
# the normal calls that FileSwitch sends to a filing system module, and
# converts them to a simpler set of calls to modules that control the
# hardware. Unlike FileSwitch it creates a fresh instantiation of itself for
# each module that it supports. Using FileCore to build filing system modules
# imposes a more rigid structure on it, as more of the filing system is
# predefined.
#
# As well as standard filing systems, FileSwitch supports image filing
# systems. These provide facilities for RISC OS to handle media in foreign
# formats, and to support `image files' (or partitions) in those formats.
# Rather than accessing the hardware directly they rely on standard RISC OS
# filing systems to do so. DOSFS is an example of an image filing system used
# to handle DOS format discs.
#
# Terminology
#
# A pathname may include a filing system name, a special field, a media name
# (e.g., a disc name), directory name(s), and the name of the object itself;
# each of these parts of a pathname is known as an `element' of the pathname.
#
# Filenames
#
# Filename `elements' may be up to ten characters in length on FileCore-based
# filing systems and on NetFS. These characters may be digits or letters.
# FileSwitch makes no distinction between upper and lower case, although
# filing systems can do so. As a general rule, you should not use top-bit-set
# characters in filenames, although some filing systems (such as
# FileCore-based ones) support them. Other characters may be used provided
# they do not have a special significance. Those that do are listed below :
#
#    .   Separates directory specifications, e.g., $.fred
#    :   Introduces a drive or disc specification, e.g., :0, :bigdisc. It also
#        marks the end of a filing system name, e.g., adfs:
#    *   Acts as a `wildcard' to match zero or more characters.
#    #   Acts as a `wildcard' to match any single character.
#    $   is the name of the root directory of the disc.
#    &   is the user root directory (URD)
#    @   is the currently selected directory (CSD)
#    ^   is the `parent' directory
#    %   is the currently selected library (CSL)
#    \   is the previously selected directory (PSD)
#
# Directories
#
# The root directory, $, forms the top of the directory hierarchy
# of the media which contains the CSD. $ does not have a parent directory,
# trying to access its parent will just access $. Each directory name is
# separated by a '.' character. For example:
#
#    $.Documents.Memos
#    %.cc
#
# Filing Systems
#
# Files may also be accessed on filing systems other than the current one by
# prefixing the filename with a filing system specification. A filing system
# name may appear between '-' characters, or suffixed by a ':', though the
# latter is advised since '-' can also be used to introduce a parameter on a
# command line, or as part of a file name. For example:
#
#    -net-$.SystemMesg
#    adfs:%.aasm
#
# Special Fields
#
# Special fields are used to supply more information to the filing system than
# you can using standard path names; for example NetFS and NetPrint use them
# to specify server addresses or names. They are introduced by a '#'
# character; a variety of syntaxes are possible:
#
#    net#MJHardy::disc1.mike
#       #MJHardy::disc1.mike
#   -net#MJHardy-:disc1.mike
#      -#MJHardy-:disc1.mike
#
# The special fields here are all MJHardy, and give the name of the fileserver
# to use. Special fields may use any character except for control characters,
# double quote '"', solidus '|' and space. If a special field contains a hypen
# you may only use the first two syntaxes given above.
#
# File$Path and Run$Path
#
# These two special variables control exactly where a file will be looked for,
# according to the operation being performed on it.
#
#    File$Path   for read operations
#    Run$Path    for execute operations
#
# The contents of each variable should expand to a list or prefixes, separated
# by commas. When a read operation is performed then the prefixes in File$Path
# are used in the order in which they are listed. The first object that
# matches is used, whether it be a file or directory. Similarly any execute
# operation uses the prefixes in Run$Path. These search paths are only used
# when the pathname does not contain an explicit filing system reference,
# e.g., executing adfs:file will not use Run$Path.
#
# Other path variables
#
# You can set up other path variables and use them as pseudo filing systems.
# For example if you typed:
#
#    *Set Source$Path adfs:$.src.,adfs:$.public.src.
#
# you could then refer to the pseudo filing system as Source: or (less
# preferable) as -Source-. These path variables work in the same was as
# File$Path and Run$Path.
#
# NOTE: Path variables are not implemented in this version of CLISP. A
# workaround for this is to use "<Foo$Path>" instead of "Foo:" until they are
# made available.
#
#
# from Lisp-string notation to internal representation
# ----------------------------------------------------
# NO swapping. "foo.lisp" means file type "lisp" and file name "foo".
# This is pseudo-BNF:
#
# legal character ::= any ISO latin-1 graphic character >= ' ' except
#                     '.' ':' '*' '#' '$' '&' '@' '^' '%' '\' '?'
#
# extended legal character ::= any ISO latin-1 graphic character >= ' ' except
#                              ':' '"' '|'
#
# legal-wild char ::= legal char | '*' | '#' | '?'
#
# host ::=   '-' { extended legal char except '-' }+ '-'
#          | { extended legal char except '-' } { extended legal char }* ':'
#          | empty
#
# device ::=   ':' { legal char }+ '.'
#            | empty
#
# directory ::=   { '$' | '&' | '@' | '%' | '\' } '.' { subdirectory }*
#               | { subdirectory }+
#               | empty
#
# '$' -> :ABSOLUTE :ROOT, '&' -> :ABSOLUTE :HOME, '@' -> :ABSOLUTE :CURRENT,
# '%' -> :ABSOLUTE :LIBRARY, '\' -> :ABSOLUTE :PREVIOUS, else :RELATIVE.
#
# subdirectory ::= { '^' | { legal-wild char }+ } '.'
#                  '^' -> :PARENT
#
# filename ::= { { legal-wild char }+ | empty }
#
# filetype ::= { '.' { legal-wild char }+ | empty }
#
# pathname ::= host device directory filename filetype
#
# Examples:
# String                          Hostname Device  Directory            Name         Type
# -net-$.SystemMesg                "net"   NIL     (:ABSOLUTE :ROOT)    "SystemMesg" NIL
# net#MJHardy::disc1.mike    "net#MJHardy" "disc1" (:ABSOLUTE :ROOT)    "mike"       NIL
# #MJHardy::disc1.mike          "#MJHardy" "disc1" (:ABSOLUTE :ROOT)    "mike"       NIL
# -net#MJHardy-:disc1.mike   "net#MJHardy" "disc1" (:ABSOLUTE :ROOT)    "mike"       NIL
# -#MJHardy-:disc1.mike         "#MJHardy" "disc1" (:ABSOLUTE :ROOT)    "mike"       NIL
# @.foo                            NIL     NIL     (:ABSOLUTE :CURRENT) "foo"        NIL
# foo                              NIL     NIL     (:RELATIVE)          "foo"        NIL
# ^.                               NIL     NIL     (:RELATIVE :PARENT)  NIL          NIL
# @.^.                             NIL     NIL     (:ABSOLUTE :CURRENT :PARENT) NIL  NIL
# foo.bar                          NIL     NIL     (:RELATIVE)          "foo"        "bar"
# foo.bar.baz                      NIL     NIL     (:RELATIVE "foo")    "bar"        "baz"
# foo.bar.                         NIL     NIL     (:RELATIVE "foo" "bar") NIL       NIL
# foo.@.                       illegal
#
# from internal representation to RISCOS string
# ---------------------------------------------
#
# with swapping _only_ of name/type components.
#
# Hostname    Device  Directory                   Name    Type      RISCOS String
#
# "net"       "disc1" (:ABSOLUTE :ROOT)           "foo"   NIL       "net::disc1.$.foo"
# "net#MJ"    "disc1" (:ABSOLUTE :ROOT "foo")     "bar"   "baz"     "net#MJ::disc1.$.foo.baz.bar"
# "adfs"      "4"     (:ABSOLUTE :ROOT "foo" "bar") NIL   NIL       "adfs::4.$.foo.bar"
# NIL         "disc1" (:ABSOLUTE :ROOT "foo")     "bar"   NIL       ":disc1.$.foo.bar"
# NIL         "disc1" (:ABSOLUTE :CURRENT)        NIL     NIL       illegal here
# NIL         "disc1" (:RELATIVE)                 NIL     NIL       ":disc1."
# NIL         "disc1" NIL                         NIL     NIL       ":disc1."
# NIL         NIL     (:ABSOLUTE :ROOT)           "foo"   NIL       "$.foo"
# NIL         NIL     (:ABSOLUTE :CURRENT)        "foo"   NIL       "@.foo"
# NIL         NIL     (:RELATIVE)                 "foo"   "bar"     "bar.foo"
# NIL         NIL     (:RELATIVE "foo")           "bar"   "baz"     "foo.baz.bar"
# NIL         NIL     (:ABSOLUTE :LIBRARY)        "bar"   NIL       "%.bar"
# NIL         NIL     (:ABSOLUTE :LIBRARY "foo")  "bar"   NIL       "%.foo.bar"
# NIL         NIL     (:RELATIVE)                 "foo"   "bar"     "bar.foo"
# NIL         NIL     (:RELATIVE "foo")           "bar"   NIL       "foo.bar"
# NIL         NIL     (:RELATIVE "foo")           NIL     "bar"     illegal here
#
# That is, the RISCOS string is the flattenation-concatenation of
#   (append
#     (if (null hostname) "" (append hostname ":"))
#     (if (null device) "" (append ":" device "."))
#     (case (pop directory)
#       (:ABSOLUTE (case (pop directory)
#                          (:ROOT "$.")
#                          (:HOME "&.")
#                          (:CURRENT "@.")
#                          (:LIBRARY "%.")
#                          (:PREVIOUS "\\.")
#       )          )
#       (:RELATIVE "")
#     )
#     (mapcar (lambda (subdir) (append subdir ".")) directory)
#     (if (null name)
#       (if (null type) "" (error "type with name illegal here"))
#       (if (null type)
#         name
#         (append type "." name)
#   ) ) )
#
# internal representation
# -----------------------
#
# Pathname components:
# HOST          Simple-String or NIL
# DEVICE        Simple-String or NIL
# DIRECTORY     (Startpoint . Subdirs) where
#                Startpoint = :RELATIVE | :ABSOLUTE anchor
#                anchor = :ROOT | :HOME | :CURRENT | :LIBRARY | :PREVIOUS
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :PARENT or
#                subdir = simple string, may contain wildcard characters ?,# and *
# NAME          NIL or
#               simple string, may contain wildcard characters ?,# and *
#               (may also be specified as :WILD)
# TYPE          NIL or
#               simple string, may contain wildcard characters ?,# and *
#               (may also be specified as :WILD)
# VERSION       always NIL (may also be specified as :WILD or :NEWEST)
#
# Constraint: startpoint /= :ABSOLUTE :ROOT only if device = NIL. If the device
# is specified, the pathname must be :ABSOLUTE :ROOT.
#
# Components:
# HOST          Simple-String or NIL
# DEVICE        Simple-String or NIL
# DIRECTORY     (Startpoint . Subdirs) whereas
#                Startpoint = :RELATIVE | :ABSOLUTE Anker
#                Anker = :ROOT | :HOME | :CURRENT | :LIBRARY | :PREVIOUS
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :PARENT oder
#                subdir = Simple-String, poss. with Wildcard-Character ?,# and *
# NAME          NIL or
#               Simple-String, poss. with Wildcard-Character ?,# and *
#               (also :WILD on input)
# TYPE          NIL or
#               Simple-String, poss. with Wildcard-Character ?,# and *
#               (also :WILD on input)
# VERSION       always NIL (also :WILD or :NEWEST on input)
#
#endif

#ifdef LOGICAL_PATHNAMES
# Components of Logical Pathnames:
# HOST          Simple-String or NIL
# DEVICE        always NIL
# DIRECTORY     (Startpoint . Subdirs) whereas
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#               subdir = :WILD-INFERIORS (means "**", all Subdirectories) or
#               subdir = :WILD (means "*") or
#               subdir = Simple-String, poss. with Wildcard-Character *
# NAME          NIL or
#               :WILD (means "*") or
#               Simple-String, poss. with Wildcard-Character *
# TYPE          NIL or
#               :WILD (means "*") or
#               Simple-String, poss. with Wildcard-Character *
# VERSION       NIL or :NEWEST or :WILD or Integer
# External Notation: see CLtl2 p. 628-629.
#endif

# access functions without case transforms:
# xpathname_host(logical,pathname)
# xpathname_device(logical,pathname)
# xpathname_directory(logical,pathname)
# xpathname_name(logical,pathname)
# xpathname_type(logical,pathname)
# xpathname_version(logical,pathname)
# > pathname: pathname or logical pathname
# > logical: flag = logpathnamep(pathname)
# < result: the value of the requested component
#if HAS_HOST
#define pathname_host_maybe(obj) ThePathname(obj)->pathname_host
#else
#define pathname_host_maybe(obj) (unused(obj), NIL)
#endif
#if HAS_DEVICE
#define pathname_device_maybe(obj) ThePathname(obj)->pathname_device
#else
#define pathname_device_maybe(obj) (unused(obj), NIL)
#endif
#if HAS_VERSION
#define pathname_version_maybe(obj) ThePathname(obj)->pathname_version
#else
#define pathname_version_maybe(obj) (unused(obj), NIL)
#endif

#ifdef LOGICAL_PATHNAMES
#define xpathname_host(logical,pathname)                    \
  (logical ? TheLogpathname(pathname)->pathname_host :      \
             pathname_host_maybe(pathname))
#define xpathname_device(logical,pathname)                  \
  (logical ? NIL : pathname_device_maybe(pathname))
#define xpathname_directory(logical,pathname)               \
  (logical ? TheLogpathname(pathname)->pathname_directory : \
                ThePathname(pathname)->pathname_directory)
#define xpathname_name(logical,pathname)                    \
  (logical ? TheLogpathname(pathname)->pathname_name :      \
                ThePathname(pathname)->pathname_name)
#define xpathname_type(logical,pathname)                    \
  (logical ? TheLogpathname(pathname)->pathname_type :      \
                ThePathname(pathname)->pathname_type)
#define xpathname_version(logical,pathname)                 \
  (logical ? TheLogpathname(pathname)->pathname_version :   \
             pathname_version_maybe(pathname))
#else # no logical pathnames
#define xpathname_host(logical,pathname) pathname_host_maybe(pathname)
#define xpathname_device(logical,pathname) pathname_device_maybe(pathname)
#define xpathname_directory(logical,pathname) \
  ThePathname(pathname)->pathname_directory
#define xpathname_name(logical,pathname) ThePathname(pathname)->pathname_name
#define xpathname_type(logical,pathname) ThePathname(pathname)->pathname_type
#define xpathname_version(logical,pathname) pathname_version_maybe(pathname)
#endif

#define SUBST_RECURSE(atom_form,self_call)                      \
  if (atomp(obj)) return atom_form;                             \
  check_STACK(); check_SP();                                    \
  pushSTACK(obj);                                               \
  { /* recursive call for CAR: */                               \
    object new_car = self_call(Car(obj));                       \
    pushSTACK(new_car);                                         \
  }                                                             \
  { /* recursive call for CDR: */                               \
    object new_cdr = self_call(Cdr(STACK_1));                   \
    if (eq(new_cdr,Cdr(STACK_1)) && eq(STACK_0,Car(STACK_1))) { \
      obj = STACK_1; skipSTACK(2); return obj;                  \
    } else { /* (CONS new_car new_cdr) */                       \
      STACK_1 = new_cdr;                                        \
     {object new_cons = allocate_cons();                        \
      Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();   \
      return new_cons;                                          \
    }}                                                          \
  }

# Converts capital-/small letters between :LOCAL and :COMMON .
# common_case(string)
# > string: Normal-Simple-String or Symbol/Zahl
# < result: converted Normal-Simple-String or the same Symbol/Number
# can trigger GC
# Operating System with preference for small letters or Capitalize
local object common_case (object string) {
  if (!simple_string_p(string))
    return string;
  var uintL len = Sstring_length(string);
  # Search, if capital- or small letters (or both) occur:
  var bool all_upper = true;
  var bool all_lower = true;
  if (len > 0) {
    var const chart* ptr = &TheSstring(string)->data[0];
    var uintL count;
    dotimespL(count,len, {
      var chart ch = *ptr++;
      if (!chareq(ch,up_case(ch)))
        all_upper = false;
      if (!chareq(ch,down_case(ch)))
        all_lower = false;
      if (!all_upper && !all_lower)
        break;
    });
  }
  if (all_upper == all_lower)
    # all_upper = all_lower = true: Nothing to convert.
    # all_upper = all_lower = false: "Mixed case represents itself."
    return string;
  if (all_upper)
    # all_upper = true, all_lower = false: STRING-DOWNCASE
    return string_downcase(string);
  else
    # all_upper = false, all_lower = true: STRING-UPCASE
    return string_upcase(string);
}
# the same, recursive like with SUBST:
local object subst_common_case (object obj) {
  SUBST_RECURSE(common_case(obj),subst_common_case);
}

#ifdef LOGICAL_PATHNAMES

local bool legal_logical_word_char (chart ch) {
  ch = up_case(ch);
  var cint c = as_cint(ch);
  if (((c >= 'A') && (c <= 'Z'))
      || ((c >= '0') && (c <= '9'))
      || (c == '-'))
    return true;
  else
    return false;
}

#endif

#if HAS_HOST

# UP: Determines, if a character is allowed as character in the host-part
# of a namestring.
# legal_hostchar(ch)
# > chart ch: Character-Code
# < result: true if allowed, else false
# NB: legal_logical_word_char(ch) implies legal_hostchar(ch).
local bool legal_hostchar (chart ch) {
 #if defined(PATHNAME_RISCOS)
  return (graphic_char_p(ch)
          && !chareq(ch,ascii(':'))
          && !chareq(ch,ascii('"'))
          && !chareq(ch,ascii('|')));
 #elif defined(PATHNAME_WIN32)
  # This is just a guess. I do not know which characters are allowed in
  # Windows host names.
  var cint c = as_cint(ch);
  if ((c >= ' ') && (c <= '~')
      && (c != '"') && (c != '/') && (c != ':') && (c != '<') && (c != '>')
      && (c != '\\'))
    return true;
  else
    return false;
 #else
  return alphanumericp(ch) || chareq(ch,ascii('-'));
 #endif
}

# UP: check an optional HOST argument
# test_optional_host(host,convert)
# > host: Host-Argument
# > convert: Flag, if case-conversion is undesired
# > subr_self: Caller (a SUBR)
# < result: valid host-component
# can trigger GC
local object test_optional_host (object host, bool convert) {
  if (eq(host,unbound))
    return NIL;
  if (nullp(host))
    goto OK; # NIL is OK
  # Else, host must be a String, whose characters are alphanumeric:
  if (!stringp(host)) {
    pushSTACK(host);         # TYPE-ERROR slot DATUM
    pushSTACK(O(type_host)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(host);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~: host should be NIL or a string, not ~"));
  }
  host = coerce_normal_ss(host); # as Normal-Simple-String
  if (convert)
    host = common_case(host);
  {
    var uintL len = Sstring_length(host);
    if (len > 0) {
      var const chart* charptr = &TheSstring(host)->data[0];
      dotimespL(len,len, {
        var chart ch = *charptr++;
        if (!legal_hostchar(ch))
          goto badhost;
      });
    }
  }
 OK: return host;
 badhost:
  pushSTACK(host);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(parse_error,GETTEXT("~: illegal hostname ~"));
}

#else

#ifdef LOGICAL_PATHNAMES

# UP: check an optional HOST argument
# test_optional_host(host)
# > host: Host-Argument
# > subr_self: Aufrufer (ein SUBR)
# < result: valid host-component
# can trigger GC
local object test_optional_host (object host) {
  if (eq(host,unbound))
    return NIL; # not specified -> NIL
  if (nullp(host))
    goto OK; # NIL is OK
  # Else, host must be a String, whose characters are alphanumeric:
  if (!stringp(host)) {
    pushSTACK(host);         # TYPE-ERROR slot DATUM
    pushSTACK(O(type_host)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(host);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~: host should be NIL or a string, not ~"));
  }
  host = coerce_normal_ss(host); # as Normal-Simple-String
  {
    var uintL len = Sstring_length(host);
    if (len > 0) {
      var const chart* charptr = &TheSstring(host)->data[0];
      dotimespL(len,len, {
        var chart ch = *charptr++;
        if (!legal_logical_word_char(ch))
          goto badhost;
      });
    }
  }
 OK: return host;
 badhost:
  pushSTACK(host);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(parse_error,GETTEXT("~: illegal hostname ~"));
}

#else

# UP: check an optional HOST argument
# test_optional_host(host);
# > host: Host-Argument
# > subr_self: Caller (a SUBR)
# < result: valid host-component
local object test_optional_host (object host) {
  if (!eq(host,unbound)) { # not specified -> OK
    if (!nullp(host)) { # specified -> should be =NIL
      pushSTACK(host);    # TYPE-ERROR slot DATUM
      pushSTACK(S(null)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(host);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~: host should be NIL, not ~"));
    }
  }
  return NIL;
}

#endif

#endif

# Determines, if two characters count as equal characters in pathnames.
# equal_pathchar(ch1,ch2)
# > chart ch1,ch2: Character-Codes
# < result: true if equal, else false
  #if !(defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32))
    #define equal_pathchar(ch1,ch2)  chareq(ch1,ch2)
  #else # defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
    # Case-insensitive, but normally without conversion
    #define equal_pathchar(ch1,ch2)  chareq(up_case(ch1),up_case(ch2))
  #endif

# UP: check whether the character is a valid element of NAME or TYPE
# component in a Namestring
# legal_namechar(ch)
# > chart ch: character-code
# < return: true if legal, else false
local bool legal_namechar (chart ch) {
  var uintB c;
 #ifdef UNICODE
  # Check whether ch fits into a single byte in O(pathname_encoding).
  if (!(cslen(O(pathname_encoding),&ch,1) == 1)) return false;
  cstombs(O(pathname_encoding),&ch,1,&c,1); # causes error message if it does not fit
 #else
  c = as_cint(ch);
 #endif
 #ifdef VALID_FILENAME_CHAR # defined in unixconf.h
  #define ch c
  return VALID_FILENAME_CHAR || (ch=='*') || (ch=='?');
  #undef ch
 #else
  #ifdef PATHNAME_AMIGAOS
  return (graphic_char_p(ch) && !(c=='/') && !(c==':'));
  #endif
  #ifdef PATHNAME_UNIX
  return ((c>=' ') && (c<='~') && !(c=='/'));
  #endif
  #ifdef PATHNAME_OS2
  return (graphic_char_p(ch) && !(c=='\\') && !(c=='/') && !(c==':'));
  #endif
  #ifdef PATHNAME_WIN32
  return ((c >= 1) && (c <= 127)
          && (c != '"') /*&& (c != '*')*/
          && (c != '/') && (c != ':')
          && (c != '<') && (c != '>') /*&& (c != '?')*/
          && (c != '\\'))
         || (c == 131)
         || (c >= 160);
  #endif
  #ifdef PATHNAME_RISCOS
  return (graphic_char_p(ch) && !(c==':') && !(c=='.')
          && !(c=='$') && !(c=='&') && !(c=='@')
          && !(c=='^') && !(c=='%') && !(c=='\\')
          # Wild Characters '*' '#' '?' are allowed here.
          );
  #endif
 #endif
}

# Determines, if a character is a wildcard for a single
# character.
# singlewild_char_p(ch)
# > chart ch: Character-Code
# < result: true if yes, else false
#if !defined(PATHNAME_RISCOS)
  #define singlewild_char_p(ch)  chareq(ch,ascii('?'))
#else # defined(PATHNAME_RISCOS)
  #define singlewild_char_p(ch)  (chareq(ch,ascii('?')) || chareq(ch,ascii('#')))
#endif
#define multiwild_char_p(ch)  chareq(ch,ascii('*'))
#define wild_char_p(ch)   (multiwild_char_p(ch) || singlewild_char_p(ch))

# Converts an object into a pathname.
local object coerce_xpathname (object obj); # later

# Converts an object into a non-logical pathname.
local object coerce_pathname (object obj); # later
#if !defined(LOGICAL_PATHNAMES)
#define coerce_pathname(obj)  coerce_xpathname(obj)
#endif

# Returns a default-pathname.
local object defaults_pathname (void); # later

# checks a default-pathname.
# test_default_pathname(defaults)
# > defaults: defaults-argument
# < result: value of the defaults-argument, a pathname
# can trigger GC
local object test_default_pathname (object defaults) {
  if (eq(defaults,unbound))
    # not specified -> take value of *DEFAULT-PATHNAME-DEFAULTS* :
    return defaults_pathname();
  else
    # specified -> turn into a pathname:
    return coerce_xpathname(defaults);
}

# error-message because of illegal pathname-argument.
# fehler_pathname_designator(thing); ( fehler_... = error_... )
# > thing: (erroneous) argument
# > subr_self: caller (a SUBR)
nonreturning_function(global, fehler_pathname_designator, (object thing)) {
  pushSTACK(thing);                       # TYPE-ERROR slot DATUM
  pushSTACK(O(type_designator_pathname)); # TYPE-ERROR slot EXPECTED-TYPE
  pushSTACK(thing);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument should be a string, symbol, file stream or pathname, not ~"));
}

# Tracks a chain of Synonym-Streams, so long as a File-Stream
# is reached.
# as_file_stream(stream)
# > stream: Builtin-Stream
# < stream: File-Stream
# > subr_self: caller (a SUBR)
local object as_file_stream (object stream) {
  var object s = stream;
  loop {
    if (TheStream(s)->strmtype == strmtype_file)
      return s;
    if (!(TheStream(s)->strmtype == strmtype_synonym))
      break;
    s = Symbol_value(TheStream(stream)->strm_synonym_symbol);
    if (!builtin_stream_p(s))
      break;
  }
  fehler_pathname_designator(stream);
}

# Signal an error if a file-stream does not have
# a file-name associated with it.
# test_file_stream_named(stream)
# > stream: File-Stream
# > subr_self: caller (a SUBR)
#define test_file_stream_named(stream)  \
  do { if (nullp(TheStream(stream)->strm_file_truename)) \
         fehler_file_stream_unnamed(stream);             \
  } while(0)
nonreturning_function(local, fehler_file_stream_unnamed, (object stream)) {
  pushSTACK(stream); # FILE-ERROR slot PATHNAME
  pushSTACK(stream);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(file_error,GETTEXT("~: filename for ~ is unknown"));
}

#if defined(UNIX) || defined(MSDOS) || defined(RISCOS) || defined(WIN32_NATIVE)

#if defined(UNIX) || defined(MSDOS)
  #define slash  '/'
#endif
#ifdef WIN32_NATIVE
  #define slash  '\\'
#endif
#ifdef RISCOS
  #define slash  '.'
#endif
# physical slash
#if defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
 #define pslashp(c)  (chareq(c,ascii('\\')) || chareq(c,ascii('/')))
#endif
#if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
 #define pslashp(c)  chareq(c,ascii('/'))
#endif
#ifdef PATHNAME_RISCOS
 #define pslashp(c)  chareq(c,ascii('.'))
#endif
#define colonp(c)    chareq(c,ascii(':'))
#ifndef LOGICAL_PATHNAMES
#define slashp(c)    pslashp(c)
#endif

# UP: add a character to an ASCII string and return as a Lisp string
# can trigger GC
#ifdef UNICODE
local object asciz_add_char (const char* chars, uintL len, char ch,
                             object encoding)
#else
#define asciz_add_char(chars,len,ch,encoding)  asciz_add_char_(chars,len,ch)
local object asciz_add_char_ (const char* chars, uintL len, char ch)
#endif
{
  var DYNAMIC_ARRAY(buf,char,len+1);
  begin_system_call(); memcpy(buf,chars,len); end_system_call();
  buf[len] = ch;
  var object s = n_char_to_string(buf,len+1,encoding);
  FREE_DYNAMIC_ARRAY(buf);
  return s;
}

# UP: Converts a Unix-Directory-Specification into a pathname.
# asciz_dir_to_pathname(path,encoding)
# > const char* path: path as ASCIZ-String
# > encoding: Encoding
# < result: as a pathname without name and type
# can trigger GC
#ifdef UNICODE
local object asciz_dir_to_pathname(const char* path, object encoding)
#else
#define asciz_dir_to_pathname(path,encoding)  asciz_dir_to_pathname_(path)
local object asciz_dir_to_pathname_(const char* path)
#endif
{
  var object pathname;
  var const char* pathptr = path;
  var uintL len = 0; # string length
  # ASCIZ-Stringende suchen:
  until (*pathptr == 0) {
    pathptr++; len++;
  }
  # as long as the String does not end with '/' already, a '/' is added:
  if ((len>0) && (pathptr[-1] == slash))
    pathname = n_char_to_string(path,len,encoding);
  else
    pathname = asciz_add_char(path,len,slash,encoding);
  # and convert into a pathname:
  return coerce_pathname(pathname);
}

#endif

# Type for PARSE-NAMESTRING:
# State while the string is being parsed character by character.
typedef struct {
  uintL index;    # index (incl. offset)
  object FNindex; # index as a fixnum
  uintL count;    # number of the remaining characters
} zustand;        # "state"

# Skip s characters.
#define Z_SHIFT(z,s) \
 do { (z).index += (s); (z).FNindex = fixnum_inc((z).FNindex,(s)); (z).count -= (s); } while(0)

# Tests whether the current character at Z satisfies pred.
#define Z_AT_SLASH(z,pred,st) \
 (((z).count != 0) && pred(TheSstring(st)->data[(z).index]))

# Replace this string with a substring.
#define Z_SUB(z,s) ((s) = subsstring((s),(z).index,(z).index+(z).count), (z).index = 0)

#ifdef LOGICAL_PATHNAMES

# Parsing of logical pathnames.

# separator between subdirs
#define semicolonp(c)  (chareq(c,ascii(';')))
#define slashp(c)      (semicolonp(c) || pslashp(c))

# Parses the name/type/version part (if subdirp=false) or a subdir part
# (if subdirp=true) of a logical pathname.
# parse_logical_word(&z,subdirp)
# > STACK_2: storage vector, a normal-simple-string
# > zustand z: start state
# < zustand z: updated
# < result: a normal-simple-string or :WILD or :WILD-INFERIORS or NIL
# can trigger GC
local object parse_logical_word (zustand* z, bool subdirp) {
  ASSERT(sstring_normal_p(STACK_2));
  var zustand startz = *z; # start-state
  var chart ch;
  # Is there a sequence of alphanumeric characters or '*',
  # no two '*' adjacent (except "**", if subdirp),
  # and, if subdirp, a ';' ?
  var bool last_was_star = false;
  var bool seen_starstar = false;
  loop {
    if (z->count == 0)
      break;
    ch = TheSstring(STACK_2)->data[z->index]; # next character
    if (!legal_logical_word_char(ch)) {
      if (chareq(ch,ascii('*'))) {
        if (last_was_star) {
          if (subdirp && (z->index - startz.index == 1))
            seen_starstar = true;
          else
            break; # adjacent '*' are forbidden
        } else
          last_was_star = true;
      } else
        break;
    }
    # skip character:
    Z_SHIFT(*z,1);
  }
  var uintL len = z->index - startz.index;
  if (subdirp) {
    if ((z->count == 0) || !slashp(ch)) {
      *z = startz; return NIL; # no ';' -> no subdir
    }
    # skip character ';' :
    Z_SHIFT(*z,1);
  }
  if (len==0)
    return NIL;
  else if ((len==1)
           && chareq(TheSstring(STACK_2)->data[startz.index],ascii('*')))
    return S(Kwild);
  else if ((len==2) && seen_starstar)
    return S(Kwild_inferiors);
  else { # build String:
    var object result = allocate_string(len);
    # and fill:
    {
      var const chart* ptr1 = &TheSstring(STACK_2)->data[startz.index];
      var chart* ptr2 = &TheSstring(result)->data[0];
      dotimespL(len,len, { *ptr2++ = up_case(*ptr1++); });
    }
    return result;
  }
}

# Test whether a string is a digit sequence.
# all_digits(string)
# > string: a normal-simple-string
# < true if the string consists entirely of digits, else false
local bool all_digits (object string) {
  var uintL len = Sstring_length(string);
  if (len > 0) {
    var const chart* charptr = &TheSstring(string)->data[0];
    dotimespL(len,len, {
      var chart ch = *charptr++;
      var cint c = as_cint(ch);
      if (!((c >= '0') && (c <= '9')))
        return false;
    });
  }
  return true;
}

# test whether the string contains semicolons,
# thus appearing to be a logical pathname
# > string: storage vector, a normal-simple-string
# < result: true if the string contains semicolons
local bool looks_logical_p (object string) {
  var uintL len = Sstring_length(string);
  if (len > 0) {
    SstringDispatch(string,{
      var const chart* charptr = TheSstring(string)->data;
      while (len-- > 0) {
        var chart ch = *charptr++;
        if (semicolonp(ch))
          return true;
      }
    },{
      var const scint* charptr = TheSmallSstring(string)->data;
      while (len-- > 0) {
        var scint ch = *charptr++;
        if (semicolonp(as_chart(ch)))
          return true;
      }
    });
  }
  return false;
}

# Attempt to parse a logical host name string, starting at a given state.
# parse_logical_host_prefix(&z,string)
# > string: storage vector, a normal-simple-string
# > state z: start state
# < state z: updated to point past the colon after the logical host
# < result: logical host, or NIL
# can trigger GC
local object parse_logical_host_prefix (zustand* zp, object string) {
  ASSERT(sstring_normal_p(string));
  var object host;
  var uintL startindex = zp->index;
  var chart ch;
  # a sequence of alphanumeric characters and then ':'
  loop {
    if (zp->count==0)
      return NIL; # string already ended -> no host
    ch = TheSstring(string)->data[zp->index]; # next character
    if (!legal_logical_word_char(ch))
      break;
    # go past alphanumeric character:
    Z_SHIFT(*zp,1);
  }
  if (!colonp(ch))
    return NIL; # no ':' -> no host
  { # make host-string:
    var uintL len = zp->index - startindex;
    host = allocate_string(len);
    # and fill it:
    if (len > 0) {
      var const chart* ptr1 = &TheSstring(string)->data[startindex];
      var chart* ptr2 = &TheSstring(host)->data[0];
      dotimespL(len,len, { *ptr2++ = up_case(*ptr1++); });
    }
  }
  # skip ':'
  Z_SHIFT(*zp,1);
  return host;
}

# CLHS for MAKE-PATHNAME: "Whenever a pathname is constructed the
# components may be canonicalized if appropriate."
# simplify the subdirectory list
# strings are coerced to normal simple strings
# the list should start with a valid startpoint (not checked!)
# > dir : pathname directory list
# < dir : the same list, destructively modified:
#         ".." or :back         ==> :up
#         ... x "foo" :up y ... ==> ... x y ...
#         ... x     "."   y ... ==> ... x y ...
#         :absolute :up   y ... ==> :absolute y ...
# can trigger GC
local object simplify_directory (object dir) {
  if (!consp(dir)) return dir;
  DOUT("simplify_directory:< ",dir);
  # kill ".", ".."->:up, coerce to normal simple strings
  var object cur = dir;
  while (consp(cur) && consp(Cdr(cur))) {
    if (stringp(Car(Cdr(cur)))) {
      if (string_equal(Car(Cdr(cur)),O(dot_string)))
        Cdr(cur) = Cdr(Cdr(cur)); # drop "."
      if (!consp(Cdr(cur))) break;
      if (string_equal(Car(Cdr(cur)),O(dotdot_string)))
        Car(Cdr(cur)) = S(Kup); # ".." --> :UP
      else # coerce to normal
        Car(Cdr(cur)) = coerce_normal_ss(Car(Cdr(cur)));
    } else if (eq(Car(Cdr(cur)),S(Kback)))
      Car(Cdr(cur)) = S(Kup); # :BACK --> :UP (ANSI)
    cur = Cdr(cur);
  }
  # collapse "foo/../" (quadratic algorithm)
  var bool changed_p = true;
  while (changed_p) {
    changed_p = false;
    cur = dir;
    while (consp(cur) && consp(Cdr(cur))) {
      if (consp(Cdr(Cdr(cur))) && !eq(Car(Cdr(cur)),S(Kup))
          && !eq(Car(Cdr(cur)),S(Kwild_inferiors))
          && eq(Car(Cdr(Cdr(cur))),S(Kup))) {
        changed_p = true;
        Cdr(cur) = Cdr(Cdr(Cdr(cur))); # collapse "foo/../"
      } else cur = Cdr(cur);
    }
  }
  if (eq(Car(dir),S(Kabsolute))) { # drop initial :up after :absolute
    while (consp(Cdr(dir)) && eq(Car(Cdr(dir)),S(Kup)))
      Cdr(dir) = Cdr(Cdr(dir));
  }
  DOUT("simplify_directory:> ",dir);
  return dir;
}

# Parses a logical pathname.
# parse_logical_pathnamestring(z)
# > STACK_1: storage vector, a normal-simple-string
# > STACK_0: freshly allocated logical pathname
# > state z: start state
# < STACK_0: same logical pathname, filled
# < result: number of remaining characters
# can trigger GC
local uintL parse_logical_pathnamestring (zustand z) {
  DOUT("parse_logical_pathnamestring:<0",STACK_0);
  DOUT("parse_logical_pathnamestring:<1",STACK_1);
  { # parse Host-Specification:
    var zustand startz = z;
    var object host = parse_logical_host_prefix(&z,STACK_1);
    if (nullp(host)) {
      z = startz; # back to the start
      host = STACK_(3+2); # Default-Host
    } else { # enter host:
      TheLogpathname(STACK_0)->pathname_host = host;
    }
  }
  { # enter Directory-Start:
    var object new_cons = allocate_cons(); # new Cons for Startpoint
    TheLogpathname(STACK_0)->pathname_directory = new_cons;
    pushSTACK(new_cons); # new (last (pathname-directory Pathname))
  }
  # stack layout:
  # data-vector, pathname, (last (pathname-directory Pathname)).
  # parse subdirectories:
  # If ";" is the first char, it is turned into :RELATIVE
  # (otherwise :ABSOLUTE) as the first subdir
  # for a reason that escapes me, ANSI CL specifies that
  # "foo:;bar;baz.zot" is a  :RELATIVE logical pathname while
  # "foo:/bar/baz.zot" is an :ABSOLUTE physical pathname.
  # see "19.3.1.1.3 The Directory part of a Logical Pathname Namestring"
  # http://www.lisp.org/HyperSpec/Body/sec_19-3-1-1-3.html
  if (Z_AT_SLASH(z,slashp,STACK_2)) {
    Z_SHIFT(z,1);
    Car(STACK_0) = S(Krelative);
  } else {
    Car(STACK_0) = S(Kabsolute);
  }
  loop {
    # try to parse the next subdir
    var object subdir = parse_logical_word(&z,true);
    if (nullp(subdir))
      break;
    # lengthen (pathname-directory pathname) by Subdir:
    pushSTACK(subdir);
    var object new_cons = allocate_cons(); # new Cons
    Car(new_cons) = popSTACK(); # = (cons subdir NIL)
    Cdr(STACK_0) = new_cons; # lengthens (pathname-directory Pathname)
    STACK_0 = new_cons; # new (last (pathname-directory Pathname))
  }
  { # parse Name:
    var object name = parse_logical_word(&z,false);
    TheLogpathname(STACK_1)->pathname_name = name;
    if ((z.count > 0)
        && chareq(TheSstring(STACK_2)->data[z.index],ascii('.'))) {
      var zustand z_name = z;
      # skip Character '.' :
      Z_SHIFT(z,1);
      # parse Typ:
      var object type = parse_logical_word(&z,false);
      TheLogpathname(STACK_1)->pathname_type = type;
      if (!nullp(type)) {
        if ((z.count > 0)
            && chareq(TheSstring(STACK_2)->data[z.index],ascii('.'))) {
          var zustand z_type = z;
          # skip Character '.' :
          Z_SHIFT(z,1);
          # parse Version:
          var object version = parse_logical_word(&z,false);
          if (eq(version,S(Kwild))) {
          } else if (equal(version,Symbol_name(S(Knewest)))) {
            version = S(Knewest);
          } else if (all_digits(version)) { # convert version into Integer
            pushSTACK(version); funcall(L(parse_integer),1);
            version = value1;
          } else {
            version = NIL;
          }
          TheLogpathname(STACK_1)->pathname_version = version;
          if (nullp(version))
            z = z_type; # restore character '.'
        } else {
          TheLogpathname(STACK_1)->pathname_version = NIL;
        }
      } else {
        z = z_name; # restore character '.'
        TheLogpathname(STACK_1)->pathname_version = NIL;
      }
    } else {
      TheLogpathname(STACK_1)->pathname_type = NIL;
      TheLogpathname(STACK_1)->pathname_version = NIL;
    }
  }
  skipSTACK(1);
  TheLogpathname(STACK_0)->pathname_directory =
    simplify_directory(TheLogpathname(STACK_0)->pathname_directory);
  DOUT("parse_logical_pathnamestring:>0",STACK_0);
  DOUT("parse_logical_pathnamestring:>1",STACK_1);
  return z.count;
}

# recognition of a logical host, cf. CLtL2 p. 631
# (defun logical-host-p (host)
#   (and (simple-string-p host)
#        (gethash host sys::*logical-pathname-translations*) ; :test #'equalp !
#        t
# ) )
local bool logical_host_p (object host) {
  return (simple_string_p(host)
          # No need to string-upcase host, because it's tested via EQUALP.
          && !eq(gethash(host,Symbol_value(S(logpathname_translations))),
                 nullobj));
}

#endif

#define string2wild(str) (equal(str,O(wild_string)) ? S(Kwild) : str)
#define wild2string(obj)    (eq(obj,S(Kwild)) ? O(wild_string) : obj)

#ifdef PATHNAME_NOEXT
# auxiliary function for PARSE-NAMESTRING:
# splits a string (at the last dot) into Name and Type.
# split_name_type(skip);
# > STACK_0: Normal-Simple-String
# > skip: 1 if a dot at the beginning should not trigger the splitting, else 0
# < STACK_1: Name
# < STACK_0: Type
# decrements STACK by 1
# can trigger GC
local void split_name_type (uintL skip) {
  var object string = STACK_0;
  var uintL length = Sstring_length(string);
  # Search for the last dot:
  var uintL index = length;
  {
    var const chart* ptr = &TheSstring(string)->data[index];
    while (index>skip) {
      if (chareq(*--ptr,ascii('.'))) goto punkt;
      index--;
    }
  }
  # no dot found -> Type := NIL
  pushSTACK(NIL);
  goto name_type_ok;
 punkt: # dot found at index
  # type := (substring string index)
  pushSTACK(subsstring(string,index,length));
  # name := (substring string 0 (1- index))
  STACK_1 = subsstring(STACK_1,0,index-1);
 name_type_ok:
  STACK_0 = string2wild(STACK_0);
  STACK_1 = string2wild(STACK_1);
}
#endif

# (PARSE-NAMESTRING thing [host [defaults [:start] [:end] [:junk-allowed]]]),
# CLTL p. 414
LISPFUN(parse_namestring,1,2,norest,key,3,
        (kw(start),kw(end),kw(junk_allowed)) ) {
  # stack layout: thing, host, defaults, start, end, junk-allowed.
  var bool junk_allowed;
  var bool parse_logical = false;
  DOUT("parse-namestring:[thng]",STACK_5);
  DOUT("parse-namestring:[host]",STACK_4);
  DOUT("parse-namestring:[dflt]",STACK_3);
  DOUT("parse-namestring:[beg]",STACK_2);
  DOUT("parse-namestring:[end]",STACK_1);
  DOUT("parse-namestring:[junk]",STACK_0);
  { # 1. check junk-allowed:
    var object obj = popSTACK(); # junk-allowed-Argument
    if (eq(obj,unbound))
      junk_allowed = false;
    else
      if (nullp(obj))
        junk_allowed = false;
      else
        junk_allowed = true;
  }
  # stack layout: thing, host, defaults, start, end.
  # 2. default-value for start is 0:
  if (eq(STACK_1,unbound))
    STACK_1 = Fixnum_0;
  # 3. check host:
 #if HAS_HOST || defined(LOGICAL_PATHNAMES)
  {
    var object host = STACK_3;
   #if HAS_HOST
    host = test_optional_host(host,false);
   #else
    host = test_optional_host(host);
   #endif
    if (nullp(host)) {
      # host := (PATHNAME-HOST defaults)
      var object defaults = test_default_pathname(STACK_2);
     #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(defaults))
        parse_logical = true;
     #endif
      host = xpathname_host(parse_logical,defaults);
    } else {
     #ifdef LOGICAL_PATHNAMES
      if (logical_host_p(host)) {
        parse_logical = true; host = string_upcase(host);
      }
     #endif
    }
    STACK_3 = host;
  }
 #else
  test_optional_host(STACK_3);
 #endif
  # 4. thing must be a String:
  DOUT("parse-namestring:[thng]",STACK_4);
  DOUT("parse-namestring:[host]",STACK_3);
  DOUT("parse-namestring:[dflt]",STACK_2);
  var object thing = STACK_4;
  if (xpathnamep(thing)) { # Pathname?
    value1 = thing; # 1. value thing
  fertig:
    DOUT("parse-namestring:[fertig]",value1);
    value2 = STACK_1; mv_count=2; # 2. value start
    skipSTACK(5); return;
  }
  if (builtin_stream_p(thing)) { # Stream?
    thing = as_file_stream(thing);
    test_file_stream_named(thing);
    value1 = TheStream(thing)->strm_file_name; # 1. value: Filename
    goto fertig; # 2. value like above
  }
  # thing should now be at least a String or a Symbol:
  var bool thing_symbol = false;
  if (!stringp(thing)) {
    if (!symbolp(thing))
      fehler_pathname_designator(thing);
    thing = Symbol_name(thing); # Symbol -> use symbol name
    thing_symbol = true;
    STACK_4 = thing; # and write back into the Stack
  }
  # thing = STACK_4 is now a String.
  # it will be traversed.
  var zustand z; # running state
 #ifdef PATHNAME_RISCOS
  # auxiliary variables for the conversion of a new_thing-relative FNindex
  # into a thing-relative FNindex.
  var object FNindex_limit = Fixnum_0;
  var sintL FNindex_offset = 0;
  var object FNindex_fallback;
 #endif
  {
    var object string; # String thing
    # check boundaries, with thing, start, end as arguments:
    var stringarg arg;
    pushSTACK(thing); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
    test_string_limits_ro(&arg);
    string = arg.string;
    z.index = arg.index;         # z.index = value of the