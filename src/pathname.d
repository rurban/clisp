# Pathnames für CLISP
# Bruno Haible 1990-1999
# Logical Pathnames: Marcus Daniels 16.9.1994

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


# =============================================================================
#                       Low level functions

# UP: Tests whether a pathname is possibly a symlink.
# possible_symlink(path)
#ifdef UNIX_LINUX
  local inline boolean possible_symlink (const char* path);
  local inline boolean possible_symlink(path)
    var const char* path;
    { # In Linux 2.0.35, /proc/<pid>/{cwd,exe,root} and /proc/<pid>/fd/<n>
      # are symlinks pointing to void. Treat them like non-symlinks, in order
      # to avoid errors.
      if (path[0]=='/'
          && path[1]=='p' && path[2]=='r' && path[3]=='o' && path[4]=='c'
          && path[5]=='/'
          && (path[6]>='0' && path[6]<='9')
         )
        return FALSE;
      return TRUE;
    }
#else
  #define possible_symlink(path)  TRUE
#endif

#ifdef UNIX_LINUX
  # The Linux /proc filesystem has some symlinks whose readlink value is
  # zero-terminated: /proc/self in Linux 2.0.35, /proc/<pid>/fd/<n> in
  # Linux 2.2.2. Remove this extraneous trailing zero byte.
  local inline int my_readlink (const char* path, char* buf, size_t bufsiz);
  local inline int my_readlink(path,buf,bufsiz)
    var const char* path;
    var char* buf;
    var size_t bufsiz;
    { var int linklen = readlink(path,buf,bufsiz);
      if (linklen > 0 && buf[linklen-1] == '\0') { linklen--; }
      return linklen;
    }
  #define readlink  my_readlink
#endif

#ifdef UNIX
  # Library-Funktion realpath implementieren:
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
  #define realpath  my_realpath  # Consensys deklariert realpath() bereits...
  local char* realpath (const char* path, char* resolved_path);
  # Methode: benutze getwd und readlink.
  local char* realpath(path,resolved_path)
    const char* path;
    char* resolved_path;
    { char mypath[MAXPATHLEN];
      int symlinkcount = 0; # Anzahl bisher aufgetretener symbolischer Links
      char* resolved_limit = &resolved_path[MAXPATHLEN-1];
      # Gültige Pointer sind die mit resolved_path <= ptr <= resolved_limit.
      # In *resolved_limit darf höchstens noch ein Nullbyte stehen.
      # (Analog mit mypath.)
      char* resolve_start;
      { char* resolved_ptr = resolved_path; # (bleibt stets <= resolved_limit)
        # evtl. Working-Directory benutzen:
        if (!(path[0]=='/')) # kein absoluter Pathname ?
          { if (getwd(resolved_path) == NULL) { return NULL; }
            resolved_ptr = resolved_path;
            while (*resolved_ptr) { resolved_ptr++; }
            if (resolved_ptr < resolved_limit) { *resolved_ptr++ = '/'; }
            resolve_start = resolved_ptr;
          }
          else
          { resolve_start = resolved_ptr = &resolved_path[0]; }
        # Dann path selber einkopieren:
       {const char* path_ptr = path;
        while ((resolved_ptr < resolved_limit) && *path_ptr)
          { *resolved_ptr++ = *path_ptr++; }
        # Mit '/' und einem Nullbyte abschließen:
        if (resolved_ptr < resolved_limit) { *resolved_ptr++ = '/'; }
        *resolved_ptr = 0;
      }}
      # Los geht's nun in resolved_path ab resolve_start.
      { char* from_ptr = resolve_start;
        char* to_ptr = resolve_start;
        while ((to_ptr < resolved_limit) && (*from_ptr))
          # Bis hierher hat der Pfad in  resolved_path[0]...to_ptr[-1]
          # die Gestalt '/subdir1/subdir2/.../txt',
          # wobei 'txt' evtl. leer, aber kein subdir leer.
          { char next = *from_ptr++; *to_ptr++ = next;
            if ((next == '/') && (to_ptr > resolved_path+1))
              # to_ptr[-1]='/'  ->  Directory ...to_ptr[-2] auflösen:
              { char* last_subdir_end = &to_ptr[-2];
                switch (*last_subdir_end)
                  { case '/':
                      #ifdef PATHNAME_UNIX_UNC
                      if (to_ptr > resolved_path+2)
                      #endif
                        # '//' wird zu '/' vereinfacht:
                        to_ptr--;
                      break;
                    case '.':
                      { char* last_subdir_ptr = &last_subdir_end[-1];
                        if (to_ptr > resolved_path+2)
                          { if (*last_subdir_ptr == '.')
                              { if ((to_ptr > resolved_path+4) && (*--last_subdir_ptr == '/'))
                                  # letztes subdir war '/../'
                                  # Dafür das subdir davor entfernen:
                                  { while ((last_subdir_ptr > resolved_path) && !(*--last_subdir_ptr == '/'));
                                    to_ptr = last_subdir_ptr+1;
                              }   }
                            elif (*last_subdir_ptr == '/')
                              { # letztes subdir war '/./'
                                # entfernen:
                                to_ptr = last_subdir_end;
                      }   }   }
                      break;
                    default:
                      # nach einem normalen subdir
                      #ifdef HAVE_READLINK
                      if (possible_symlink(resolved_path))
                        { # symbolischen Link lesen:
                          to_ptr[-1]=0; # '/' durch 0 ersetzen
                          {
                            #ifdef UNIX_CYGWIN32 # needed for Win95 only
                            # readlink() doesn't work right on NFS mounted directories
                            # (it returns -1,ENOENT or -1,EIO).
                            # So check for a directory first.
                            struct stat statbuf;
                            if (lstat(resolved_path,&statbuf) < 0)
                              { return NULL; } # Fehler
                            if (S_ISDIR(statbuf.st_mode))
                              # Verzeichnis, kein symbolisches Link
                              { to_ptr[-1] = '/'; } # wieder den '/' eintragen
                            else
                            #endif
                            { int linklen = readlink(resolved_path,mypath,sizeof(mypath)-1);
                              if (linklen >=0)
                                # war ein symbolisches Link
                                { if (++symlinkcount > MAXSYMLINKS) { errno = ELOOP_VALUE; return NULL; }
                                  # noch aufzulösenden path-Anteil an den Link-Inhalt anhängen:
                                  { char* mypath_ptr = &mypath[linklen]; # ab hier ist Platz
                                    char* mypath_limit = &mypath[MAXPATHLEN-1]; # bis hierher
                                    if (mypath_ptr < mypath_limit) { *mypath_ptr++ = '/'; } # erst ein '/' anhängen
                                    # dann den Rest:
                                    while ((mypath_ptr <= mypath_limit) && (*mypath_ptr = *from_ptr++)) { mypath_ptr++; }
                                    *mypath_ptr = 0; # und mit 0 abschließen
                                  }
                                  # Dies ersetzt bzw. ergänzt den path:
                                  if (mypath[0] == '/')
                                    # ersetzt den path:
                                    { from_ptr = &mypath[0]; to_ptr = resolved_path;
                                      while ((*to_ptr++ = *from_ptr++));
                                      from_ptr = resolved_path;
                                    }
                                    else
                                    # ergänzt den path:
                                    { # Linknamen streichen. Dazu bis zum letzten '/' suchen:
                                      { char* ptr = &to_ptr[-1];
                                        while ((ptr > resolved_path) && !(ptr[-1] == '/')) { ptr--; }
                                        from_ptr = ptr;
                                      }
                                      { char* mypath_ptr = &mypath[0]; to_ptr = from_ptr;
                                        while ((to_ptr <= resolved_limit) && (*to_ptr++ = *mypath_ptr++));
                                    } }
                                  to_ptr = from_ptr;
                                }
                                else
                                #if defined(UNIX_IRIX)
                                if ((errno == EINVAL) || (errno == ENXIO))
                                #elif defined(UNIX_MINT)
                                if ((errno == EINVAL) || (errno == EACCESS))
                                #elif defined(UNIX_CYGWIN32)
                                if ((errno == EINVAL) || (errno == EACCES))
                                #else
                                if (errno == EINVAL)
                                #endif
                                  # kein symbolisches Link
                                  { to_ptr[-1] = '/'; } # wieder den '/' eintragen
                                else
                                  { return NULL; } # Fehler
                            }
                          }
                        }
                      #endif
                      break;
              }   }
          } # dann zum nächsten subdir
        # ein '/' am Ende streichen:
        if ((to_ptr[-1] == '/')
            #ifdef PATHNAME_UNIX_UNC
            && (to_ptr > resolved_path+2)
            #else
            && (to_ptr > resolved_path+1)
            #endif
           )
          { to_ptr--; }
        to_ptr[0] = 0; # durch 0 abschließen
        return resolved_path; # fertig
    } }
#endif
#ifdef RISCOS
  # SYNOPSIS
  #   char* realpath (char* path, char resolved_path[MAXPATHLEN]);
  # RETURN VALUES
  #   realpath() returns a pointer to the resolved_path on success.
  #   On failure, it returns NULL and sets errno to indicate the error.
  local char* realpath (char* path, char* resolved_path);
  #include <sys/os.h>
  local char* realpath(path,resolved_path)
    var char* path;
    var char* resolved_path;
    { var int handle;
      var int r[10];
      #if 0 # Both of these implementations should work.
      if (os_fopen(0x40,path,&handle)) { return NULL; }
      r[0] = 7; r[1] = handle; r[2] = (long)resolved_path; r[5] = MAXPATHLEN;
      os_swi(9,r);
      os_fclose(handle);
      #else
      var os_error* err;
      r[0] = 37; r[1] = (long)path; r[2] = (long)resolved_path;
      r[3] = 0; r[4] = 0; r[5] = MAXPATHLEN;
      err = os_swi(0x29,r);
      if (err) { __seterr(err); return NULL; }
      #endif
      if (r[5] <= 0)
        { errno = ENOMEM /* ENAMETOOLONG would be better, but does not yet exist */;
          return NULL;
        }
      return resolved_path;
    }
#endif

# Creates a new subdirectory.
# make_directory(pathstring);
# > pathstring: result of shorter_directory(...)
# > STACK_0: pathname
  local inline void make_directory (char* pathstring);
  local inline void make_directory(pathstring)
    var char* pathstring;
    {
      #ifdef AMIGAOS
      set_break_sem_4();
      begin_system_call();
      {var BPTR lock = CreateDir(pathstring); # Unterdirectory erzeugen
       if (lock==BPTR_NULL) { end_system_call(); OS_file_error(STACK_0); }
       UnLock(lock); # Lock freigeben
      }
      end_system_call();
      clr_break_sem_4();
      #endif
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
      begin_system_call();
      if (mkdir(pathstring,0777)) # Unterdirectory erzeugen
        { end_system_call(); OS_file_error(STACK_0); }
      end_system_call();
      #endif
      #ifdef WIN32_NATIVE
      begin_system_call();
      if (! CreateDirectory(pathstring,NULL) ) # Unterdirectory erzeugen
        { end_system_call(); OS_file_error(STACK_0); }
      end_system_call();
      #endif
    }

# Deletes a subdirectory.
# delete_directory(pathstring);
# > pathstring: result of shorter_directory(...)
# > STACK_0: pathname
  local inline void delete_directory (char* pathstring);
  local inline void delete_directory(pathstring)
    var char* pathstring;
    {
      #ifdef AMIGAOS
      # Noch Test, ob's auch ein Directory und kein File ist??
      begin_system_call();
      if (! DeleteFile(pathstring) ) # Unterdirectory löschen
        { end_system_call(); OS_file_error(STACK_0); }
      end_system_call();
      #endif
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM)
      begin_system_call();
      if (rmdir(pathstring)) # Unterdirectory löschen
        { end_system_call(); OS_file_error(STACK_0); }
      end_system_call();
      #endif
      #ifdef RISCOS
      begin_system_call();
      if (unlink(pathstring)) # Unterdirectory löschen
        { end_system_call(); OS_file_error(STACK_0); }
      end_system_call();
      #endif
      #ifdef WIN32_NATIVE
      begin_system_call();
      if (! RemoveDirectory(pathstring) ) # Unterdirectory löschen
        { end_system_call(); OS_file_error(STACK_0); }
      end_system_call();
      #endif
    }

#if defined(MSDOS) || defined(WIN32_NATIVE)
# Changes the operating system's current directory.
# change_directory(pathstring);
# > pathstring: directory, ASCIZ-String
# > STACK_0: pathname
  local inline void change_current_directory (char* pathstring);
  local inline void change_current_directory(pathstring)
    var char* pathstring;
    {
      begin_system_call();
      #ifdef MSDOS
        if (!( chdir(pathstring) ==0))
          { end_system_call(); OS_file_error(STACK_0); }
      #endif
      #ifdef WIN32_NATIVE
        if (!SetCurrentDirectory(pathstring))
          { end_system_call(); OS_file_error(STACK_0); }
      #endif
      end_system_call();
    }
#endif

# Delete a file.
# delete_existing_file(pathstring);
# It is known that the file exists.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
  local inline void delete_existing_file (char* pathstring);
  local inline void delete_existing_file(pathstring)
    var char* pathstring;
    {
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
        begin_system_call();
        if (!( unlink(pathstring) ==0))
          { end_system_call(); OS_file_error(STACK_0); }
        end_system_call();
      #endif
      #if defined(AMIGAOS) || defined(WIN32_NATIVE)
        begin_system_call();
        if (! DeleteFile(pathstring) )
          { end_system_call(); OS_file_error(STACK_0); }
        end_system_call();
      #endif
    }

# Delete a file.
# delete_file_if_exists(pathstring);
# No error is signalled if the file does not exist.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
# < result: whether the file existed
  local inline boolean delete_file_if_exists (char* pathstring);
  local inline boolean delete_file_if_exists(pathstring)
    var char* pathstring;
    { var boolean exists = TRUE;
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
        begin_system_call();
        if (!( unlink(pathstring) ==0))
          { if (!(errno==ENOENT)) # nicht gefunden -> OK
              { end_system_call(); OS_file_error(STACK_0); } # sonstigen Error melden
            exists = FALSE;
          }
        end_system_call();
      #endif
      #ifdef AMIGAOS
        begin_system_call();
        if (! DeleteFile(pathstring) )
          { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) # nicht gefunden -> OK
              { end_system_call(); OS_file_error(STACK_0); } # sonstigen Error melden
            exists = FALSE;
          }
        end_system_call();
      #endif
      #ifdef WIN32_NATIVE
        begin_system_call();
        if (! DeleteFile(pathstring) )
          { if (!(GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND))
              { end_system_call(); OS_file_error(STACK_0); }
            exists = FALSE;
          }
        end_system_call();
      #endif
      return exists;
    }

# Delete a file being the target of a subsequent rename.
# delete_file_before_rename(pathstring);
# No error is signalled if the file does not exist.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
  local inline void delete_file_before_rename (char* pathstring);
  local inline void delete_file_before_rename(pathstring)
    var char* pathstring;
    {
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
  local inline void rename_existing_file (char* old_pathstring, char* new_pathstring);
  local inline void rename_existing_file(old_pathstring,new_pathstring)
    var char* old_pathstring;
    var char* new_pathstring;
    {
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
        begin_system_call();
        if ( rename(old_pathstring,new_pathstring) <0) # Datei umbenennen
          { end_system_call(); OS_file_error(STACK_0); } # Error melden
        end_system_call();
      #endif
      #ifdef AMIGAOS
        begin_system_call();
        if (! Rename(old_pathstring,new_pathstring) )
          { end_system_call(); OS_file_error(STACK_0); }
        end_system_call();
      #endif
      #ifdef WIN32_NATIVE
        begin_system_call();
        if (! MoveFile(old_pathstring,new_pathstring) )
          { end_system_call(); OS_file_error(STACK_0); }
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
  local inline void rename_file_to_nonexisting (char* old_pathstring, char* new_pathstring);
  local inline void rename_file_to_nonexisting(old_pathstring,new_pathstring)
    var char* old_pathstring;
    var char* new_pathstring;
    {
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
        begin_system_call();
        if ( rename(old_pathstring,new_pathstring) <0) # Datei umbenennen
          { if (errno==ENOENT)
              { end_system_call(); OS_file_error(STACK_3); }
              else
              { end_system_call(); OS_file_error(STACK_1); }
          }
        end_system_call();
      #endif
      #ifdef AMIGAOS
        begin_system_call();
        if (! Rename(old_pathstring,new_pathstring) )
          { if (IoErr()==ERROR_OBJECT_NOT_FOUND)
              { end_system_call(); OS_file_error(STACK_3); }
              else
              { end_system_call(); OS_file_error(STACK_1); }
          }
        end_system_call();
      #endif
      #ifdef WIN32_NATIVE
        begin_system_call();
        if (! MoveFile(old_pathstring,new_pathstring) )
          { if (GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND)
              { end_system_call(); OS_file_error(STACK_3); }
              else
              { end_system_call(); OS_file_error(STACK_1); }
          }
        end_system_call();
      #endif
    }


# =============================================================================
#                         P A T H N A M E S

# All simple-strings occurring in pathnames are in fact normal-simple-strings.

#ifdef PATHNAME_MSDOS
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder :WILD oder "A"|...|"Z"
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :CURRENT (bedeutet ".") oder
#                subdir = :PARENT (bedeutet "..") oder
#                subdir = :WILD-INFERIORS (bedeutet "...", alle Subdirectories) oder
#                subdir = (name . type)
#                 name = :WILD oder Simple-String mit max. 8 Zeichen
#                 type = :WILD oder Simple-String mit max. 3 Zeichen
# NAME          NIL oder :WILD oder Simple-String mit max. 8 Zeichen
# TYPE          NIL oder :WILD oder Simple-String mit max. 3 Zeichen
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Wenn ein Pathname vollständig spezifiziert sein muss (keine Wildcards),
# ist :WILD, :WILD-INFERIORS nicht erlaubt, bei NAME evtl. auch nicht NIL.
# Externe Notation:       A:\sub1.typ\sub2.typ\name.typ
# mit Defaults:             \sub1.typ\sub2.typ\name.typ
# oder                                         name.typ
# oder                    *:\sub1.typ\*.*\name.*
# oder Ähnliches.
# Statt '\' ist - wie unter DOS üblich - auch '/' erlaubt.
#endif

#ifdef PATHNAME_AMIGAOS
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder Simple-String
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (bedeutet "**" oder "...", alle Subdirectories) oder
#                subdir = :PARENT (bedeutet "/" statt "subdir/") oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Constraint: Startpoint = :RELATIVE nur, falls Device = NIL;
#             bei angegebenem Device gibt es also nur absolute Pathnames!
# Ein AMIGAOS-Filename wird folgendermaßen in Name und Typ aufgespalten:
#   falls kein '.' im Filename: Name = alles, Typ = NIL,
#   falls '.' im Filename: Name = alles vor, Typ = alles nach dem letzten '.' .
# Groß-/Klein-Schreibung innerhalb der Strings wird bei Vergleichen ignoriert,
# aber ansonsten findet keine Groß/Klein-Umwandlung statt.
# Wenn ein Pathname vollständig spezifiziert sein muss (keine Wildcards),
# ist :WILD, :WILD-INFERIORS nicht erlaubt, keine Wildcard-Zeichen in den
# Strings, bei NAME evtl. auch nicht NIL.
# Externe Notation:  device:sub1.typ/sub2.typ/name.typ
# mit Defaults:             sub1.typ/sub2.typ/name.typ
# oder                                        name.typ
# oder                      sub1.typ/ ** /sub3.typ/x*.lsp  (ohne Spaces!)
# oder Ähnliches.
# Formal:
#   ch ::= beliebgiges Character außer ':','/' und '*','?'
#   name ::= {ch}+
#   device ::= [ <leer> | ':' | name ':' ]
#              ; leer = aktuelles Device, relativ ab aktuellem Directory
#              ; ':'  = aktuelles Device, absolut (ab root bei Disks)
#              ; name ':' = angegebenes Device, absolut (ab root bei Disks)
#   subdir ::= [ <leer> | name ]                ; leer = '..'
#   pathname ::= device { subdir '/' }* name
# Beispiele:
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
# An einen Pathstring, der nichtleer ist und der nicht mit ':' oder '/'
# endet, kann ein '/' angehängt werden, ohne seine Semantik zu verändern.
# Dieser '/' muss angehängt werden, bevor man eine weitere nichtleere
# Komponente anhängen kann.
# An einen Pathstring, der leer ist oder mit ':' oder '/' endet, ein '/'
# anzuhängen, bedeutet aber, zum Parent Directory aufzusteigen!
# Bei uns wird jeder Pathstring, der leer ist oder mit ':' oder '/' endet,
# als Directory-Pathname (mit Name=NIL und Type=NIL) interpretiert.
#endif

#ifdef PATHNAME_UNIX
# Komponenten:
# HOST          stets NIL
# DEVICE        stets NIL
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (bedeutet "**" oder "...", alle Subdirectories) oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Ein UNIX-Filename wird folgendermaßen in Name und Typ aufgespalten:
#   falls kein '.' im Filename: Name = alles, Typ = NIL,
#   falls '.' im Filename: Name = alles vor, Typ = alles nach dem letzten '.' .
# Wenn ein Pathname vollständig spezifiziert sein muss (keine Wildcards),
# ist :WILD, :WILD-INFERIORS nicht erlaubt, keine Wildcard-Zeichen in den
# Strings, bei NAME evtl. auch nicht NIL.
# Externe Notation:  server:/sub1.typ/sub2.typ/name.typ
# mit Defaults:             /sub1.typ/sub2.typ/name.typ
# oder                                         name.typ
# oder                      /sub1.typ/ ** /sub3.typ/x*.lsp  (ohne Spaces!)
# oder Ähnliches.
#endif

#if defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder :WILD oder "A"|...|"Z"
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD-INFERIORS (bedeutet "**" oder "...", alle Subdirectories) oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Ein OS/2-Filename wird folgendermaßen in Name und Typ aufgespalten:
#   falls kein '.' im Filename: Name = alles, Typ = NIL,
#   falls '.' im Filename: Name = alles vor, Typ = alles nach dem letzten '.' .
# Wenn ein Pathname vollständig spezifiziert sein muss (keine Wildcards),
# ist :WILD, :WILD-INFERIORS nicht erlaubt, keine Wildcard-Zeichen in den
# Strings, bei NAME evtl. auch nicht NIL.
# Externe Notation:       A:\sub1.typ\sub2.typ\name.typ
# mit Defaults:             \sub1.typ\sub2.typ\name.typ
# oder                                         name.typ
# oder                    *:\sub1.typ\**\sub3.typ\x*.lsp
# oder Ähnliches.
# Statt '\' ist - wie unter DOS üblich - auch '/' erlaubt.
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
# NO swapping. "foo.lsp" means file type "lsp" and file name "foo".
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
# Komponenten:
# HOST          Simple-String oder NIL
# DEVICE        Simple-String oder NIL
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE Anker
#                Anker = :ROOT | :HOME | :CURRENT | :LIBRARY | :PREVIOUS
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :PARENT oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ?,# und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ?,# und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ?,# und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
#
#endif

#ifdef LOGICAL_PATHNAMES
# Komponenten von Logical Pathnames:
# HOST          Simple-String oder NIL
# DEVICE        stets NIL
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#               subdir = :WILD-INFERIORS (bedeutet "**", alle Subdirectories) oder
#               subdir = :WILD (bedeutet "*") oder
#               subdir = Simple-String, evtl. mit Wildcard-Zeichen *
# NAME          NIL oder
#               :WILD (bedeutet "*") oder
#               Simple-String, evtl. mit Wildcard-Zeichen *
# TYPE          NIL oder
#               :WILD (bedeutet "*") oder
#               Simple-String, evtl. mit Wildcard-Zeichen *
# VERSION       NIL oder :NEWEST oder :WILD oder Integer
# Externe Notation: siehe CLtl2 S. 628-629.
#endif

# Wandelt Strings in Normal-Simple-Strings um.
# subst_coerce_normal_ss(obj)
# can trigger GC
  local object subst_coerce_normal_ss (object obj);
  local object subst_coerce_normal_ss(obj)
    var object obj;
    { if (atomp(obj))
        { if (stringp(obj))
            return coerce_normal_ss(obj);
            else
            return obj;
        }
      check_STACK(); check_SP();
      pushSTACK(obj);
      # rekursiv für den CAR aufrufen:
      { var object new_car = subst_coerce_normal_ss(Car(obj));
        pushSTACK(new_car);
      }
      # rekursiv für den CDR aufrufen:
      { var object new_cdr = subst_coerce_normal_ss(Cdr(STACK_1));
        if (eq(new_cdr,Cdr(STACK_1)) && eq(STACK_0,Car(STACK_1)))
          { obj = STACK_1; skipSTACK(2); return obj; }
          else
          # (CONS new_car new_cdr)
          { STACK_1 = new_cdr;
           {var object new_cons = allocate_cons();
            Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
            return new_cons;
    } }   }}

# Wandelt Groß-/Klein-Schreibung zwischen :LOCAL und :COMMON um.
# common_case(string)
# > string: Normal-Simple-String oder Symbol/Zahl
# < ergebnis: umgewandelter Normal-Simple-String oder dasselbe Symbol/Zahl
# can trigger GC
  local object common_case (object string);
# Dasselbe, rekursiv wie mit SUBST:
  local object subst_common_case (object obj);
#if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32) || defined(PATHNAME_RISCOS) || defined(PATHNAME_AMIGAOS)
  # Betriebssystem mit Vorzug für Kleinbuchstaben oder Capitalize
  local object common_case(string)
    var object string;
    { if (!simple_string_p(string))
        return string;
     {var uintL len = Sstring_length(string);
      # Suche, ob Groß- oder Kleinbuchstaben (oder beides) vorkommen:
      var boolean all_upper = TRUE;
      var boolean all_lower = TRUE;
      if (len > 0)
        { var const chart* ptr = &TheSstring(string)->data[0];
          var uintL count;
          dotimespL(count,len,
            { var chart ch = *ptr++;
              if (!chareq(ch,up_case(ch))) { all_upper = FALSE; }
              if (!chareq(ch,down_case(ch))) { all_lower = FALSE; }
              if (!all_upper && !all_lower) break;
            });
        }
      if (all_upper == all_lower)
        # all_upper = all_lower = TRUE: Nichts zu konvertieren.
        # all_upper = all_lower = FALSE: "Mixed case represents itself."
        return string;
      if (all_upper)
        # all_upper = TRUE, all_lower = FALSE: STRING-DOWNCASE
        return string_downcase(string);
        else
        # all_upper = FALSE, all_lower = TRUE: STRING-UPCASE
        return string_upcase(string);
    }}
  local object subst_common_case(obj)
    var object obj;
    { if (atomp(obj))
        { return common_case(obj); }
      check_STACK(); check_SP();
      pushSTACK(obj);
      # rekursiv für den CAR aufrufen:
      { var object new_car = subst_common_case(Car(obj));
        pushSTACK(new_car);
      }
      # rekursiv für den CDR aufrufen:
      { var object new_cdr = subst_common_case(Cdr(STACK_1));
        if (eq(new_cdr,Cdr(STACK_1)) && eq(STACK_0,Car(STACK_1)))
          { obj = STACK_1; skipSTACK(2); return obj; }
          else
          # (CONS new_car new_cdr)
          { STACK_1 = new_cdr;
           {var object new_cons = allocate_cons();
            Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
            return new_cons;
    } }   }}
#else # defined(PATHNAME_MSDOS)
  # Betriebssystem mit Vorzug für Großbuchstaben
  #define common_case(string)  string
  #define subst_common_case(obj)  obj
#endif

#ifdef LOGICAL_PATHNAMES

local boolean legal_logical_word_char (chart ch);
local boolean legal_logical_word_char(ch)
  var chart ch;
  { ch = up_case(ch);
   {var cint c = as_cint(ch);
    if (((c >= 'A') && (c <= 'Z'))
        || ((c >= '0') && (c <= '9'))
        || (c == '-')
       )
      return TRUE;
      else
      return FALSE;
  }}

#endif

#if HAS_HOST

# UP: Stellt fest, ob ein Character als Zeichen im Host-Teil eines Namestring
# erlaubt ist.
# legal_hostchar(ch)
# > chart ch: Character-Code
# < ergebnis: TRUE falls erlaubt, FALSE sonst
  local boolean legal_hostchar (chart ch);
# NB: legal_logical_word_char(ch) impliziert legal_hostchar(ch).
  local boolean legal_hostchar(ch)
    var chart ch;
    {
      #ifdef PATHNAME_RISCOS
      return (graphic_char_p(ch)
              && !chareq(ch,ascii(':'))
              && !chareq(ch,ascii('"'))
              && !chareq(ch,ascii('|')));
      #else
      return alphanumericp(ch) || chareq(c,ascii('-'));
      #endif
    }

# UP: Überprüft ein optionales Host-Argument.
# test_optional_host(host,convert)
# > host: Host-Argument
# > convert: Flag, ob Case-Konversion erwünscht ist
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: gültige Host-Komponente
# can trigger GC
  local object test_optional_host (object host, boolean convert);
  local object test_optional_host(host,convert)
    var object host;
    var boolean convert;
    { if (eq(host,unbound)) { return NIL; } # nicht angegeben -> NIL
      if (nullp(host)) goto OK; # NIL ist OK
      # Sonst muss host ein String sein, dessen Zeichen alphanumerisch sind:
      if (!stringp(host))
        { pushSTACK(host); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_host)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(host);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: host should be NIL or a string, not ~")
                );
        }
      host = coerce_normal_ss(host); # als Normal-Simple-String
      if (convert) { host = common_case(host); }
      { var uintL len = Sstring_length(host);
        if (len > 0)
          { var const chart* charptr = &TheSstring(host)->data[0];
            dotimespL(len,len,
              { var chart ch = *charptr++;
                if (!legal_hostchar(ch)) goto badhost;
              });
      }   }
      OK: return host;
      badhost:
        { pushSTACK(host);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(parse_error,
                 GETTEXT("~: illegal hostname ~")
                );
        }
    }

#else

#ifdef LOGICAL_PATHNAMES

# UP: Überprüft ein optionales Host-Argument.
# test_optional_host(host)
# > host: Host-Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: gültige Host-Komponente
# can trigger GC
  local object test_optional_host (object host);
  local object test_optional_host(host)
    var object host;
    { if (eq(host,unbound)) { return NIL; } # nicht angegeben -> NIL
      if (nullp(host)) goto OK; # NIL ist OK
      # Sonst muss host ein String sein, dessen Zeichen alphanumerisch sind:
      if (!stringp(host))
        { pushSTACK(host); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_host)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(host);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: host should be NIL or a string, not ~")
                );
        }
      host = coerce_normal_ss(host); # als Normal-Simple-String
      { var uintL len = Sstring_length(host);
        if (len > 0)
          { var const chart* charptr = &TheSstring(host)->data[0];
            dotimespL(len,len,
              { var chart ch = *charptr++;
                if (!legal_logical_word_char(ch)) goto badhost;
              });
      }   }
      OK: return host;
      badhost:
        { pushSTACK(host);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(parse_error,
                 GETTEXT("~: illegal hostname ~")
                );
        }
    }

#else

# UP: Überprüft ein optionales Host-Argument.
# test_optional_host(host);
# > host: Host-Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: gültige Host-Komponente
  local object test_optional_host (object host);
  local object test_optional_host(host)
    var object host;
    { if (!eq(host,unbound)) # nicht angegeben -> OK
        { if (!nullp(host)) # angegeben -> sollte =NIL sein
            { pushSTACK(host); # Wert für Slot DATUM von TYPE-ERROR
              pushSTACK(S(null)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
              pushSTACK(host);
              pushSTACK(TheSubr(subr_self)->name);
              fehler(type_error,
                     GETTEXT("~: host should be NIL, not ~")
                    );
            }
        }
      return NIL;
    }

#endif

#endif

# Stellt fest, ob zwei Characters als Zeichen in Pathnames als gleich gelten.
# equal_pathchar(ch1,ch2)
# > chart ch1,ch2: Character-Codes
# < ergebnis: TRUE falls gleich, FALSE sonst
  #if !(defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32))
    #define equal_pathchar(ch1,ch2)  chareq(ch1,ch2)
  #else # defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
    # Case-insensitive, aber normalerweise ohne Konversion
    #define equal_pathchar(ch1,ch2)  chareq(up_case(ch1),up_case(ch2))
  #endif

# UP: Stellt fest, ob ein Character als Zeichen im Namens-/Typ-Teil eines
# Namestring erlaubt ist.
# legal_namechar(ch)
# > chart ch: Character-Code
# < ergebnis: TRUE falls erlaubt, FALSE sonst
  local boolean legal_namechar (chart ch);
  local boolean legal_namechar(ch)
    var chart ch;
    { var uintB c;
      #ifdef UNICODE
      # Check whether ch fits into a single byte in O(pathname_encoding).
      if (!(cslen(O(pathname_encoding),&ch,1) == 1)) return FALSE;
      cstombs(O(pathname_encoding),&ch,1,&c,1); # causes error message if it doesn't fit
      #else
      c = as_cint(ch);
      #endif
      #ifdef VALID_FILENAME_CHAR # defined in unixconf.h
      #define ch c
      return VALID_FILENAME_CHAR || (ch=='*') || (ch=='?');
      #undef ch
      #else
      #ifdef PATHNAME_MSDOS
      # Leo Sarasua says that "%*+,;<=>[]| are invalid.
      return (alphanumericp(ch) || (c=='_')
              || (c=='!') || (c=='#') || (c=='$') || (c=='&') || (c=='\'')
              || (c=='(') || (c==')') || (c=='-') || (c=='?') || (c=='@')
              || (c=='^') || (c=='`') || (c=='{') || (c=='}') || (c=='~'));
      #endif
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
      return ((c >= 1) && (c <= 127) && (c != 34) /*&& (c != 42)*/ && (c != 47) && (c != 58) && (c != 60) && (c != 62) /*&& (c != 63)*/ && (c != 92)) || (c == 131) || ((c >= 160) && (c != 197) && (c != 206));
      #endif
      #ifdef PATHNAME_RISCOS
      return (graphic_char_p(ch) && !(c==':') && !(c=='.')
              && !(c=='$') && !(c=='&') && !(c=='@')
              && !(c=='^') && !(c=='%') && !(c=='\\')
              # Wild Characters '*' '#' '?' sind hier erlaubt.
             );
      #endif
      #endif
    }

# Stellt fest, ob ein Character ein Wildcard-Platzhalter für ein einzelnes
# Zeichen ist.
# singlewild_char_p(ch)
# > chart ch: Character-Code
# < ergebnis: TRUE falls ja, FALSE sonst
  #if !defined(PATHNAME_RISCOS)
    #define singlewild_char_p(ch)  chareq(ch,ascii('?'))
  #else # defined(PATHNAME_RISCOS)
    #define singlewild_char_p(ch)  (chareq(ch,ascii('?')) || chareq(ch,ascii('#')))
  #endif

# Wandelt ein Objekt in einen Pathname um.
  local object coerce_xpathname (object obj); # später

# Wandelt ein Objekt in einen nicht-Logical Pathname um.
  local object coerce_pathname (object obj); # später
#if !defined(LOGICAL_PATHNAMES)
  #define coerce_pathname(obj)  coerce_xpathname(obj)
#endif

# Liefert den Default-Pathname.
  local object defaults_pathname (void); # später

# Überprüft einen Default-Pathname.
# test_default_pathname(defaults)
# > defaults: Defaults-Argument
# < ergebnis: Wert des Defaults-Arguments, ein Pathname
# can trigger GC
  local object test_default_pathname (object defaults);
  local object test_default_pathname(defaults)
    var object defaults;
    { if (eq(defaults,unbound))
        # nicht angegeben -> Wert von *DEFAULT-PATHNAME-DEFAULTS* nehmen:
        { return defaults_pathname(); }
        else
        # angegeben -> zu einem Pathname machen:
        { return coerce_xpathname(defaults); }
    }

# Fehlermeldung wegen illegalem Pathname-Argument.
# fehler_pathname_designator(thing);
# > thing: (fehlerhaftes) Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_pathname_designator, (object thing));
  global void fehler_pathname_designator(thing)
    var object thing;
    { pushSTACK(thing); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_designator_pathname)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(thing);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument should be a string, symbol, file stream or pathname, not ~")
            );
    }

# Verfolgt eine Kette von Synonym-Streams, solange bis bei einem File-Stream
# angelangt.
# as_file_stream(stream)
# > stream: Builtin-Stream
# < stream: File-Stream
# > subr_self: Aufrufer (ein SUBR)
  local object as_file_stream (object stream);
  local object as_file_stream(stream)
    var object stream;
    { var object s = stream;
      loop
        { if (TheStream(s)->strmtype == strmtype_file) { return s; }
          if (!(TheStream(s)->strmtype == strmtype_synonym)) break;
          s = Symbol_value(TheStream(stream)->strm_synonym_symbol);
          if (!builtin_stream_p(s)) break;
        }
      fehler_pathname_designator(stream);
    }

# Signal an error if a file-stream does not have a file-name associated with it.
# test_file_stream_named(stream)
# > stream: File-Stream
# > subr_self: Aufrufer (ein SUBR)
  #define test_file_stream_named(stream)  \
    { if (nullp(TheStream(stream)->strm_file_truename)) \
        fehler_file_stream_unnamed(stream);             \
    }
  nonreturning_function(local, fehler_file_stream_unnamed, (object stream));
  local void fehler_file_stream_unnamed(stream)
    var object stream;
    { pushSTACK(stream); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(stream);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(file_error,
             GETTEXT("~: filename for ~ is unknown")
            );
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

# UP: Wandelt eine Unix-Directory-Angabe in ein Pathname um.
# asciz_dir_to_pathname(path,encoding)
# > const char* path: path als ASCIZ-String
# > encoding: Encoding
# < ergebnis: als Pathname ohne Name und Typ
 #ifdef UNICODE
  local object asciz_dir_to_pathname (const char* path, object encoding);
  local object asciz_dir_to_pathname(const char* path, object encoding)
 #else
  #define asciz_dir_to_pathname(path,encoding)  asciz_dir_to_pathname_(path)
  local object asciz_dir_to_pathname_ (const char* path);
  local object asciz_dir_to_pathname_(const char* path)
 #endif
     { var object pathname;
       var const char* pathptr = path;
       var uintL len = 0; # Stringlänge
       until (*pathptr == 0) { pathptr++; len++; } # ASCIZ-Stringende suchen
       # Sofern der String nicht schon mit '/' endet, wird ein '/' angefügt:
       if ((len>0) && (pathptr[-1] == slash))
         { pathname = n_char_to_string(path,len,encoding); }
         else
         { var DYNAMIC_ARRAY(pathbuf,char,len+1);
           begin_system_call(); memcpy(pathbuf,path,len); end_system_call();
           pathbuf[len] = slash;
           pathname = n_char_to_string(pathbuf,len+1,encoding);
           FREE_DYNAMIC_ARRAY(pathbuf);
         }
       # und in ein Pathname umwandeln:
       return coerce_pathname(pathname);
     }

#endif

# Typ für PARSE-NAMESTRING:
# Der String wird durchlaufen.
  typedef struct { uintL index; # Index (incl. Offset)
                   object FNindex; # Index als Fixnum
                   uintL count; # Anzahl der verbleibenden Characters
                 }
          zustand;

#ifdef LOGICAL_PATHNAMES

# Parst einen Logical-Pathname.
# parse_logical_pathnamestring(z)
# > STACK_1: Datenvektor, ein Normal-Simple-String
# > STACK_0: neuer Logical Pathname
# > zustand z: Start-Zustand
# < STACK_0: selber Logical Pathname, ausgefüllt
# < ergebnis: Anzahl der übriggebliebenen Zeichen
# can trigger GC
local uintL parse_logical_pathnamestring (zustand z);

# Trennzeichen zwischen subdirs
#define slashp(c)  chareq(c,ascii(';'))

# Parst Name/Type/Version-Teil (subdirp=FALSE) bzw. subdir-Teil (subdirp=TRUE).
# Liefert Normal-Simple-String oder :WILD oder :WILD-INFERIORS oder NIL.
local object parse_logical_word (zustand* z, boolean subdirp);
local object parse_logical_word(z,subdirp)
  var zustand* z;
  var boolean subdirp;
  { var zustand startz; startz = *z; # Start-Zustand
   {var chart ch;
    # Kommt eine Folge von alphanumerischen Zeichen oder '*',
    # keine zwei '*' adjazent (ausgenommen "**", falls subdirp),
    # und, falls subdirp, ein ';' ?
    var boolean last_was_star = FALSE;
    var boolean seen_starstar = FALSE;
    loop
      { if (z->count == 0) break;
        ch = TheSstring(STACK_2)->data[z->index]; # nächstes Character
        if (!legal_logical_word_char(ch))
          { if (chareq(ch,ascii('*')))
              { if (last_was_star)
                  { if (subdirp && (z->index - startz.index == 1))
                      seen_starstar = TRUE;
                      else
                      break; # adjazente '*' sind verboten
                  }
                  else
                  last_was_star = TRUE;
              }
              else
              break;
          }
        # Character übergehen:
        z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
      }
    {var uintL len = z->index - startz.index;
     if (subdirp)
       { if ((z->count == 0) || !slashp(ch))
           { *z = startz; return NIL; } # kein ';' -> kein subdir
         # Character ';' übergehen:
         z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
       }
     if (len==0)
       { return NIL; }
     elif ((len==1) && chareq(TheSstring(STACK_2)->data[startz.index],ascii('*')))
       { return S(Kwild); }
     elif ((len==2) && seen_starstar)
       { return S(Kwild_inferiors); }
     else # String bilden:
       { var object result = allocate_string(len);
         # und füllen:
         {var const chart* ptr1 = &TheSstring(STACK_2)->data[startz.index];
          var chart* ptr2 = &TheSstring(result)->data[0];
          dotimespL(len,len, { *ptr2++ = up_case(*ptr1++); });
         }
         return result;
       }
  }}}

# Test auf Ziffernfolge:
local boolean all_digits (object string);
local boolean all_digits(string)
  var object string;
  { var uintL len = Sstring_length(string);
    if (len > 0)
      { var const chart* charptr = &TheSstring(string)->data[0];
        dotimespL(len,len,
          { var chart ch = *charptr++;
            var cint c = as_cint(ch);
            if (!((c >= '0') && (c <= '9'))) return FALSE;
          });
      }
    return TRUE;
  }

local uintL parse_logical_pathnamestring(z)
  var zustand z;
  { # Host-Specification parsen:
    {  var object host;
       var zustand startz; startz = z; # Start-Zustand
     { var chart ch;
       # Kommt eine Folge von alphanumerischen Zeichen und dann ein ':' ?
       loop
         { if (z.count==0) goto no_hostspec; # String schon zu Ende -> kein Host
           ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
           if (!legal_logical_word_char(ch)) break;
           # alphanumerisches Character übergehen:
           z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
         }
       if (!chareq(ch,ascii(':'))) goto no_hostspec; # kein ':' -> kein Host
       # Host-String bilden:
       { var uintL len = z.index - startz.index;
         host = allocate_string(len);
         # und füllen:
         if (len > 0)
           { var const chart* ptr1 = &TheSstring(STACK_1)->data[startz.index];
             var chart* ptr2 = &TheSstring(host)->data[0];
             dotimespL(len,len, { *ptr2++ = up_case(*ptr1++); });
       }   }
       # Character ':' übergehen:
       z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
       goto hostspec_ok;
      no_hostspec: # keine Host-Specification
       z = startz; # zum Start zurück
       host = STACK_(3+2); # Default-Host
      hostspec_ok:
       # Host eintragen:
       TheLogpathname(STACK_0)->pathname_host = host;
    }}
    # Directory-Start eintragen:
    { var object new_cons = allocate_cons(); # neues Cons für Startpoint
      TheLogpathname(STACK_0)->pathname_directory = new_cons;
      pushSTACK(new_cons); # neues (last (pathname-directory Pathname))
    }
    # Stackaufbau: Datenvektor, Pathname, (last (pathname-directory Pathname)).
    # Subdirectories parsen:
    # Falls sofort ein ';' kommt, wird er übergangen, und es kommt :RELATIVE
    # (sonst :ABSOLUTE) als erstes subdir:
    if ((!(z.count == 0)) && slashp(TheSstring(STACK_2)->data[z.index]))
      # Es kommt sofort ein ';'.
      { # Character übergehen:
        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
        Car(STACK_0) = S(Krelative); # Startpoint = :RELATIVE
      }
      else
      # Es kommt nicht sofort ein ';'.
      { Car(STACK_0) = S(Kabsolute); } # Startpoint = :ABSOLUTE
    loop
      { # Versuche, ein weiteres Unterdirectory zu parsen.
        var object subdir = parse_logical_word(&z,TRUE);
        if (nullp(subdir)) break;
        # (pathname-directory pathname) um Subdir verlängern:
        pushSTACK(subdir);
       {var object new_cons = allocate_cons(); # neues Cons
        Car(new_cons) = popSTACK(); # = (cons subdir NIL)
        Cdr(STACK_0) = new_cons; # verlängert (pathname-directory Pathname)
        STACK_0 = new_cons; # neues (last (pathname-directory Pathname))
      }}
    # Name parsen:
    { var object name = parse_logical_word(&z,FALSE);
      TheLogpathname(STACK_1)->pathname_name = name;
      if ((z.count > 0) && chareq(TheSstring(STACK_2)->data[z.index],ascii('.')))
        { var zustand z_name; z_name = z;
          # Character '.' übergehen:
          z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
          # Typ parsen:
         {var object type = parse_logical_word(&z,FALSE);
          TheLogpathname(STACK_1)->pathname_type = type;
          if (!nullp(type))
            { if ((z.count > 0) && chareq(TheSstring(STACK_2)->data[z.index],ascii('.')))
                { var zustand z_type; z_type = z;
                  # Character '.' übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  # Version parsen:
                 {var object version = parse_logical_word(&z,FALSE);
                  if (eq(version,S(Kwild))) { }
                  elif (equal(version,Symbol_name(S(Knewest))))
                    { version = S(Knewest); }
                  elif (all_digits(version))
                    { pushSTACK(version); funcall(L(parse_integer),1); # version in Integer umwandeln
                      version = value1;
                    }
                  else
                    { version = NIL; }
                  TheLogpathname(STACK_1)->pathname_version = version;
                  if (nullp(version))
                    { z = z_type; } # Character '.' wieder zurück
                }}
                else
                { TheLogpathname(STACK_1)->pathname_version = NIL; }
            }
            else
            { z = z_name; # Character '.' wieder zurück
              TheLogpathname(STACK_1)->pathname_version = NIL;
            }
        }}
        else
        { TheLogpathname(STACK_1)->pathname_type = NIL;
          TheLogpathname(STACK_1)->pathname_version = NIL;
        }
    }
    skipSTACK(1);
    return z.count;
  }

#undef slashp

# Erkennung eines logischen Hosts, vgl. CLtL2 S. 631
# (defun logical-host-p (host)
#   (and (simple-string-p host)
#        (gethash host sys::*logical-pathname-translations*)
#        t
# ) )
  local boolean logical_host_p (object host);
  local boolean logical_host_p(host)
    var object host;
    { return (simple_string_p(host)
              # Fehlt host = string_upcase(host); ??
              && !eq(gethash(host,Symbol_value(S(logpathname_translations))),
                     nullobj
             )      );
    }

#endif

#ifdef PATHNAME_EXT83
# Hilfsfunktion für PARSE-NAMESTRING:
# Parst einen Namens- oder Typteil.
# parse_name_or_type(&z,stdlen,def)
# > stdlen: Standard-Länge des Teils
# > def: Defaultwert
# > STACK_3: Datenvektor des Strings, ein Normal-Simple-String
# > z: Zustand
# < z: Zustand
# < ergebnis: Namens- oder Typteil (=default, falls leer)
# can trigger GC
  local object parse_name_or_type (zustand* z, uintL stdlen, object def);
  local object parse_name_or_type(z,stdlen,def)
    var zustand* z;
    var uintL stdlen;
    var object def;
    { var uintL z_start_index = z->index; # Index beim Start des Namens
      loop
        { var chart ch;
          if (z->count == 0) break;
          ch = TheSstring(STACK_3)->data[z->index]; # nächstes Character
          ch = up_case(ch); # als Großbuchstabe
          if (chareq(ch,ascii('.'))) break;
          if (chareq(ch,ascii('*')))
            # '*' angetroffen.
            { # nicht am Anfang des Namens -> beendet den Namen:
              if (!(z->index == z_start_index)) break;
              # Character übergehen:
              z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
              return S(Kwild); # Name := :WILD
            }
          if (!legal_namechar(ch)) break; # gültiges Character ?
          # ja -> Teil des Namens
          # Character übergehen:
          z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
        }
      # Ende des Namens erreicht.
      # Name := Teilstring von STACK_3 von z_start_index (einschließlich)
      #                                bis z->index (ausschließlich).
     {var uintL len = z->index - z_start_index;
      # kein Name angegeben -> default zurück:
      if (len==0) { return def; }
      #ifndef EMUNIX_PORTABEL # unter OS/2 gilt die 8+3-Regel nicht mehr
      # bei len > stdlen setze len:=stdlen :
      if (len > stdlen) { len = stdlen; }
      #endif
      {var object string = allocate_string(len); # String der Länge len
       # füllen:
       var const chart* ptr1 = &TheSstring(STACK_3)->data[z_start_index];
       var chart* ptr2 = &TheSstring(string)->data[0];
       dotimespL(len,len, { *ptr2++ = up_case(*ptr1++); });
       # Name fertig.
       return string;
    }}}
#endif

#ifdef PATHNAME_NOEXT
# Hilfsfunktion für PARSE-NAMESTRING:
# Spaltet einen String (beim letzten Punkt) in Name und Typ auf.
# split_name_type(skip);
# > STACK_0: Normal-Simple-String
# > skip: 1 falls ein Punkt an erster Stelle nicht aufspaltend wirken soll, 0 sonst
# < STACK_1: Name
# < STACK_0: Typ
# Erniedrigt STACK um 1
# can trigger GC
  local void split_name_type (uintL skip);
  local void split_name_type(skip)
    var uintL skip;
    { var object string = STACK_0;
      var uintL length = Sstring_length(string);
      # Nach dem letzten Punkt suchen:
      var uintL index = length;
      { var const chart* ptr = &TheSstring(string)->data[index];
        while (index>skip)
          { if (chareq(*--ptr,ascii('.'))) goto punkt;
            index--;
      }   }
      # kein Punkt gefunden -> Typ := NIL
      pushSTACK(NIL);
      goto name_type_ok;
      punkt: # Punkt bei index gefunden
      # type := (substring string index)
      pushSTACK(subsstring(string,index,length));
      # name := (substring string 0 (1- index))
      STACK_1 = subsstring(STACK_1,0,index-1);
      name_type_ok: ;
    }
#endif

LISPFUN(parse_namestring,1,2,norest,key,3,\
        (kw(start),kw(end),kw(junk_allowed)) )
# (PARSE-NAMESTRING thing [host [defaults [:start] [:end] [:junk-allowed]]]),
# CLTL S. 414
  { # Stackaufbau: thing, host, defaults, start, end, junk-allowed.
    var boolean junk_allowed;
    var boolean parse_logical = FALSE;
    # 1. junk-allowed überprüfen:
    { var object obj = popSTACK(); # junk-allowed-Argument
      if (eq(obj,unbound))
        { junk_allowed = FALSE; }
        else
        if (nullp(obj)) { junk_allowed = FALSE; } else { junk_allowed = TRUE; }
    }
    # Stackaufbau: thing, host, defaults, start, end.
    # 2. Default-Wert für start ist 0:
    { if (eq(STACK_1,unbound)) { STACK_1 = Fixnum_0; }}
    # 3. host überprüfen:
    #if HAS_HOST || defined(LOGICAL_PATHNAMES)
    { var object host;
      #if HAS_HOST
      host = test_optional_host(STACK_3,FALSE);
      #else
      host = test_optional_host(STACK_3);
      #endif
      if (nullp(host))
        { # host := (PATHNAME-HOST defaults)
          var object defaults = test_default_pathname(STACK_2);
          #ifdef LOGICAL_PATHNAMES
          if (logpathnamep(defaults))
            { parse_logical = TRUE; host = TheLogpathname(defaults)->pathname_host; }
            else
          #endif
            {
              #if HAS_HOST
              host = ThePathname(defaults)->pathname_host;
              #else
              host = NIL;
              #endif
            }
        }
      #ifdef LOGICAL_PATHNAMES
      elif (logical_host_p(host))
        { parse_logical = TRUE; host = string_upcase(host); }
      #endif
      STACK_3 = host;
    }
    #else
    { test_optional_host(STACK_3); }
    #endif
    # 4. thing muss ein String sein:
    { var object thing = STACK_4;
      if (xpathnamep(thing)) # Pathname?
        { value1 = thing; # 1. Wert thing
          fertig:
          value2 = STACK_1; mv_count=2; # 2. Wert start
          skipSTACK(5); return;
        }
      if (builtin_stream_p(thing)) # Stream?
        { thing = as_file_stream(thing);
          test_file_stream_named(thing);
          value1 = TheStream(thing)->strm_file_name; # 1. Wert: Filename
          goto fertig; # 2. Wert wie oben
        }
      # thing sollte nun wenigstens ein String oder Symbol sein:
     {var boolean thing_symbol = FALSE;
      if (!stringp(thing))
        { if (!symbolp(thing)) { fehler_pathname_designator(thing); }
          thing = Symbol_name(thing); # Symbol -> Symbolname verwenden
          thing_symbol = TRUE;
          STACK_4 = thing; # und in den Stack zurückschreiben
        }
      # thing = STACK_4 ist jetzt ein String.
      { # Er wird durchlaufen.
        var zustand z; # laufender Zustand
        #ifdef PATHNAME_RISCOS
        # Hilfsvariablen zur Umsetzung eines new_thing-relativen FNindex
        # in einen thing-relativen FNindex.
        var object FNindex_limit = Fixnum_0;
        var sintL FNindex_offset = 0;
        var object FNindex_fallback;
        #endif
       {var object string; # String thing
        # Grenzen überprüfen, mit thing, start, end als Argumenten:
        var stringarg arg;
        pushSTACK(thing); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
        test_string_limits_ro(&arg);
        string = arg.string;
        z.index = arg.index;         # z.index = Wert des start-Arguments,
        z.count = arg.len;           # z.count = Anzahl der Characters.
        z.FNindex = fixnum(z.index); # z.FNindex = start-Index als Fixnum.
        z.index += arg.offset;
        # Coerce string to be a normal-simple-string.
        if (thing_symbol && !parse_logical)
          {
            #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32) || defined(PATHNAME_RISCOS)
            # Betriebssystem mit Vorzug für Kleinbuchstaben
            string = subsstring(string,z.index,z.index+z.count); z.index = 0; # ja -> mit STRING-DOWNCASE umwandeln
            nstring_downcase(&TheSstring(string)->data[0],Sstring_length(string));
            #endif
            #ifdef PATHNAME_AMIGAOS
            # Betriebssystem mit Vorzug für Capitalize
            string = subsstring(string,z.index,z.index+z.count); z.index = 0; # ja -> mit STRING-CAPITALIZE umwandeln
            nstring_capitalize(&TheSstring(string)->data[0],Sstring_length(string));
            #endif
          }
        SstringDispatch(string,
          {},
          { string = subsstring(string,z.index,z.index+z.count); z.index = 0; }
          );
        pushSTACK(string);
       }
        #ifdef LOGICAL_PATHNAMES
        if (parse_logical)
          { pushSTACK(allocate_logpathname());
            # Stackaufbau: ..., Datenvektor, Pathname.
           {var uintL remaining = parse_logical_pathnamestring(z);
            z.index += z.count-remaining; z.FNindex = fixnum_inc(z.FNindex,z.count-remaining); z.count = remaining;
          }}
          else
        #endif
          {
            #ifdef PATHNAME_RISCOS
              # If the string starts with a system variable in <...> syntax,
              # then perform the substitution
              # (string-concat "<" var ">" tail) --> (string-concat (sys::getenv var) tail).
              if ((!(z.count==0)) && chareq(TheSstring(STACK_0)->data[z.index],ascii('<')))
                { var zustand startz = z; # Start-Zustand
                  var chart ch;
                  # Character '<' übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  loop
                    { if (z.count==0) goto no_envvar;
                      ch = TheSstring(STACK_0)->data[z.index]; # nächstes Character
                      if (chareq(ch,ascii('>'))) break;
                      if (!(graphic_char_p(ch) && !chareq(ch,ascii('*')) && !chareq(ch,ascii('#')))) goto no_envvar;
                      # gültiges Character übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                    }
                  FNindex_fallback = z.FNindex;
                  # Character '>' übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  # Environment-Variable als Simple-String bauen:
                  if (z.index - startz.index - 2 == 0) goto no_envvar;
                  {var object envvar = subsstring(STACK_0,startz.index+1,z.index-1);
                   # Deren Wert holen:
                   with_sstring_0(envvar,O(misc_encoding),envvar_asciz,
                     { begin_system_call();
                      {var const char* envval = getenv(envvar_asciz);
                       end_system_call();
                       if (envval==NULL)
                         { pushSTACK(envvar);
                           pushSTACK(S(parse_namestring));
                           fehler(parse_error,
                                  GETTEXT("~: there is no environment variable ~")
                                 );
                         }
                       pushSTACK(asciz_to_string(envval,O(misc_encoding))); # Wert der Variablen als String
                     }});
                  }
                  # Reststück bilden:
                  pushSTACK(subsstring(STACK_1,z.index,z.index+z.count));
                  # Beides zusammenhängen, thing ersetzen:
                  { var uintL envval_len = Sstring_length(STACK_1);
                    var object new_thing = string_concat(2);
                    STACK_(4+1) = STACK_0 = new_thing;
                    # Der 2. Wert FNindex muss nachher noch modifiziert werden.
                    FNindex_limit = fixnum(envval_len);
                    FNindex_offset = (sintL)posfixnum_to_L(z.FNindex) - (sintL)envval_len;
                    z.index = 0; z.count = Sstring_length(new_thing); z.FNindex = Fixnum_0;
                  }
                  goto envvar_ok;
                 no_envvar: # keine Environment-Variable
                  z = startz; # zum Start zurück
                }
              envvar_ok: ;
            #endif
            pushSTACK(allocate_pathname());
            # Stackaufbau: ..., Datenvektor, Pathname.
            #if HAS_HOST
              # Host-Specification parsen:
              {var object host;
               { var zustand startz = z; # Start-Zustand
                 var chart ch;
                 #if defined(PATHNAME_RISCOS)
                   # Kommt eine Folge von Zeichen, eingeschlossen in '-',
                   # oder eine Folge von Zeichen und dann eine ':' ?
                   if (z.count==0) goto no_hostspec; # String schon zu Ende -> kein Host
                   ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                   if (chareq(ch,ascii('-')))
                     { # '-' übergehen:
                       z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                       loop
                         { if (z.count==0) goto no_hostspec; # String schon zu Ende -> kein Host
                           ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                           if (chareq(ch,ascii('-'))) break;
                           if (!legal_hostchar(ch)) goto no_hostspec;
                           # gültiges Character übergehen:
                           z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                         }
                       # Host-String bilden:
                       if (z.index - startz.index - 1 == 0) goto no_hostspec;
                       host = subsstring(STACK_1,startz.index+1,z.index);
                     }
                     else
                     { loop
                         { if (!legal_hostchar(ch)) goto no_hostspec;
                           # gültiges Character übergehen:
                           z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                           if (z.count==0) goto no_hostspec; # String schon zu Ende -> kein Host
                           ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                           if (chareq(ch,ascii(':'))) break;
                         }
                       # Host-String bilden:
                       host = subsstring(STACK_1,startz.index,z.index);
                     }
                   # Character '-' bzw. ':' übergehen:
                   z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                   goto hostspec_ok;
                 #else
                   # Kommt eine Folge von alphanumerischen Zeichen und dann ein ':' bzw. '::' ?
                   loop
                     { if (z.count==0) goto no_hostspec; # String schon zu Ende -> kein Host
                       ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                       if (!alphanumericp(ch)) break;
                       # alphanumerisches Character übergehen:
                       z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                     }
                   if (!chareq(ch,ascii(':'))) goto no_hostspec; # kein ':' -> kein Host
                   # Host-String bilden:
                   host = subsstring(STACK_1,startz.index,z.index);
                   # Character ':' übergehen:
                   z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                   goto hostspec_ok;
                 #endif
                 no_hostspec: # keine Host-Specification
                   z = startz; # zum Start zurück
                   host = STACK_(3+2); # Default-Host
               }
               hostspec_ok:
               # Host eintragen:
               ThePathname(STACK_0)->pathname_host = host;
              }
            #endif
            #if HAS_DEVICE
             #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
              # Einbuchstabige Device-Specification parsen:
              {var object device = NIL; # Device := NIL
               # Drive-Specification parsen:
               # Kommt ein Buchstabe ('*','A'-'Z','a'-'z') und dann ein ':' ?
               { var zustand startz = z; # Start-Zustand
                 var chart ch;
                 if (z.count==0) goto no_drivespec; # String schon zu Ende ?
                 ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                 ch = up_case(ch); # als Großbuchstabe
                 if (chareq(ch,ascii('*')))
                   # ch = '*' -> Device := :WILD
                   { device = S(Kwild); }
                 elif ((as_cint(ch) >= 'A') && (as_cint(ch) <= 'Z'))
                   # 'A' <= ch <= 'Z' -> Device := "ch"
                   { var object string = allocate_string(1); # String der Länge 1
                     TheSstring(string)->data[0] = ch; # mit ch als einzigem Buchstaben
                     device = string;
                   }
                 else goto no_device;
                 # Device OK, Character übergehen:
                 z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                 if (z.count==0) goto no_drivespec; # String schon zu Ende ?
                 ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                 ch = up_case(ch); # als Großbuchstabe
                 no_device:
                 # mit Doppelpunkt abgeschlossen?
                 if (!chareq(ch,ascii(':'))) goto no_drivespec;
                 # Character übergehen:
                 z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                 goto drivespec_ok;
                 no_drivespec:
                 # Es ist nicht gelungen, eine Drive-Specification zu parsen.
                 z = startz; # Start-Zustand wiederherstellen
                 device = NIL; # Device := NIL
               }
               drivespec_ok:
               ThePathname(STACK_0)->pathname_device = device; # Device eintragen
              }
             #endif
             #ifdef PATHNAME_AMIGAOS
              # Device-Specification parsen:
              {var object device;
               # Kommt eine nichtleere Folge von alphanumerischen Zeichen und dann ein ':' ?
               { var zustand startz = z; # Start-Zustand
                 var chart ch;
                 loop
                   { if (z.count==0) goto no_devicespec; # String schon zu Ende -> kein Device
                     ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                     if (!legal_namechar(ch)) break;
                     # alphanumerisches Character übergehen:
                     z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                   }
                 if (!chareq(ch,ascii(':'))) goto no_devicespec; # kein ':' -> kein Device
                 if (z.index==startz.index) goto no_devicespec; # ':' am Anfang ist kein Device
                 # Device-String bilden:
                 device = subsstring(STACK_1,startz.index,z.index);
                 # Character ':' nicht übergehen; das ergibt dann :ABSOLUTE.
                 goto devicespec_ok;
                 no_devicespec: # keine Device-Specification
                   z = startz; # zum Start zurück
                   device = NIL; # Device NIL
               }
               devicespec_ok:
               # Device eintragen:
               ThePathname(STACK_0)->pathname_device = device;
              }
             #endif
             #ifdef PATHNAME_RISCOS
              # Device-Specification parsen:
              {var object device;
               # Kommt ein ':', eine nichtleere Folge von Zeichen und dann ein '.' ?
               { var zustand startz = z; # Start-Zustand
                 var chart ch;
                 if (z.count==0) goto no_devicespec; # String schon zu Ende -> kein Device
                 ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                 if (!chareq(ch,ascii(':'))) goto no_devicespec; # kein ':' -> kein Device
                 # Character ':' übergehen:
                 z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                 loop
                   { if (z.count==0) goto no_devicespec; # String schon zu Ende -> kein Device
                     ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                     if (!(legal_namechar(ch) && !chareq(ch,ascii('*')) && !singlewild_char_p(ch))) break;
                     # gültiges Character übergehen:
                     z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                   }
                 if (!chareq(ch,ascii('.'))) goto no_devicespec; # kein '.' -> kein Device
                 # Device-String bilden:
                 if (z.index - startz.index - 1 == 0) goto no_devicespec;
                 device = subsstring(STACK_1,startz.index+1,z.index);
                 # Character '.' übergehen:
                 z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                 goto devicespec_ok;
                 no_devicespec: # keine Device-Specification
                   z = startz; # zum Start zurück
                   device = NIL; # Device NIL
               }
               devicespec_ok:
               # Device eintragen:
               ThePathname(STACK_0)->pathname_device = device;
              }
             #endif
            #endif
            # Directory-Start eintragen:
            { var object new_cons = allocate_cons(); # neues Cons für Startpoint
              ThePathname(STACK_0)->pathname_directory = new_cons;
              pushSTACK(new_cons); # neues (last (pathname-directory Pathname))
            }
            # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)).
            # Subdirectories parsen:
            # Trennzeichen zwischen subdirs ist unter MSDOS sowohl '\' als auch '/':
            #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
             #define slashp(c)  (chareq(c,ascii('\\')) || chareq(c,ascii('/')))
            #endif
            #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
             #define slashp(c)  chareq(c,ascii('/'))
            #endif
            #ifdef PATHNAME_RISCOS
             #define slashp(c)  chareq(c,ascii('.'))
            #endif
            {
              #if defined(USER_HOMEDIR) && defined(PATHNAME_UNIX)
              # Falls sofort ein '~' kommt, wird bis zum nächsten '/' oder Stringende
              # ein Username gelesen und das Home-Directory dieses Users eingesetzt:
              if ((!(z.count == 0)) && chareq(TheSstring(STACK_2)->data[z.index],ascii('~')))
                # Es kommt sofort ein '~'.
                { # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                 {var object userhomedir; # Pathname des User-Homedir
                  # nächsten '/' suchen:
                  var uintL charcount = 0;
                  { var const chart* charptr = &TheSstring(STACK_2)->data[z.index];
                    var uintL count;
                    dotimesL(count,z.count,
                      { if (chareq(*charptr++,ascii('/'))) break;
                        charcount++;
                      });
                  }
                  # Username hat charcount Zeichen
                  if (charcount==0)
                    { userhomedir = O(user_homedir); } # nur '~' -> User-Homedir
                    else
                    { # Username bauen:
                      var object username = subsstring(STACK_2,z.index,z.index+charcount);
                      # Dessen Home-Directory aus dem Passwort-File holen:
                      with_sstring_0(username,O(misc_encoding),username_asciz,
                        { begin_system_call();
                          errno = 0;
                         {var struct passwd * userpasswd = getpwnam(username_asciz);
                          if (userpasswd == (struct passwd *)NULL) # erfolglos?
                            { if (!(errno==0)) { OS_error(); } # Error melden
                              end_system_call();
                              # sonst: Fehler
                              pushSTACK(username);
                              pushSTACK(S(parse_namestring));
                              fehler(parse_error,
                                     GETTEXT("~: there is no user named ~")
                                    );
                            }
                          end_system_call();
                          userhomedir = asciz_dir_to_pathname(userpasswd->pw_dir,O(misc_encoding)); # Homedir als Pathname
                        }});
                    }
                  # Directory aus dem Pathname userhomedir kopieren:
                  # (copy-list dir) = (nreconc (reverse dir) nil),
                  # dabei dessen letztes Cons merken.
                  userhomedir = reverse(ThePathname(userhomedir)->pathname_directory);
                  STACK_0 = userhomedir; userhomedir = nreconc(userhomedir,NIL);
                  ThePathname(STACK_1)->pathname_directory = userhomedir;
                  # username-Characters übergehen:
                  z.index += charcount; z.FNindex = fixnum_inc(z.FNindex,charcount); z.count -= charcount;
                  # Falls der String zu Ende ist: fertig,
                  # sonst kommt sofort ein '/', es wird übergangen:
                  if (z.count==0)
                    { pushSTACK(NIL); pushSTACK(NIL); goto after_name_type; } # Name und Typ := NIL
                  # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                }}
              else
              #endif
              #if defined(PATHNAME_UNIX) && 0 # Wozu braucht man das, außer für $HOME ?
              # Falls sofort ein '$' kommt, wird bis zum nächsten '/' oder Stringende
              # eine Environment-Variable gelesen und ihr Wert eingesetzt:
              if ((!(z.count == 0)) && chareq(TheSstring(STACK_2)->data[z.index],ascii('$')))
                # Es kommt sofort ein '$'.
                { # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                 {var object envval_dir;
                  # nächsten '/' suchen:
                  var uintL charcount = 0;
                  { var const chart* charptr = &TheSstring(STACK_2)->data[z.index];
                    var uintL count;
                    dotimesL(count,z.count,
                      { if (chareq(*charptr++,ascii('/'))) break;
                        charcount++;
                      });
                  }
                  # Environment-Variable hat charcount Zeichen.
                  { var object envvar = subsstring(STACK_2,z.index,z.index+charcount);
                    # Deren Wert holen:
                    with_sstring_0(envvar,O(misc_encoding),envvar_asciz,
                      { begin_system_call();
                       {var const char* envval = getenv(envvar_asciz);
                        end_system_call();
                        if (envval==NULL)
                          { pushSTACK(envvar);
                            pushSTACK(S(parse_namestring));
                            fehler(parse_error,
                                   GETTEXT("~: there is no environment variable ~")
                                  );
                          }
                        envval_dir = asciz_dir_to_pathname(envval,O(misc_encoding)); # Wert der Variablen als Pathname
                      }});
                  }
                  # Directory aus dem Pathname envval_dir kopieren:
                  # (copy-list dir) = (nreconc (reverse dir) nil),
                  # dabei dessen letztes Cons merken.
                  envval_dir = reverse(ThePathname(envval_dir)->pathname_directory);
                  STACK_0 = envval_dir; envval_dir = nreconc(envval_dir,NIL);
                  ThePathname(STACK_1)->pathname_directory = envval_dir;
                  # envvar-Characters übergehen:
                  z.index += charcount; z.FNindex = fixnum_inc(z.FNindex,charcount); z.count -= charcount;
                  # Falls der String zu Ende ist: fertig,
                  # sonst kommt sofort ein '/', es wird übergangen:
                  if (z.count==0)
                    { pushSTACK(NIL); pushSTACK(NIL); goto after_name_type; } # Name und Typ := NIL
                  # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                }}
              else
              #endif
              #if defined(PATHNAME_UNIX) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
              # Falls sofort ein '\' bzw. '/' kommt, wird er übergangen, und es kommt
              # :ABSOLUTE (sonst :RELATIVE) als erstes subdir:
              if ((!(z.count == 0)) && slashp(TheSstring(STACK_2)->data[z.index]))
                # Es kommt sofort ein '\' bzw. '/'.
                { # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  Car(STACK_0) = S(Kabsolute); # Startpoint = :ABSOLUTE
                }
                else
                # Es kommt nicht sofort ein '\' bzw. '/'.
                { Car(STACK_0) = S(Krelative); } # Startpoint = :RELATIVE
              #endif
              #ifdef PATHNAME_AMIGAOS
              # Falls sofort ein ':' kommt, wird er übergangen, und es kommt
              # :ABSOLUTE (sonst :RELATIVE) als erstes subdir:
              if ((!(z.count == 0)) && chareq(TheSstring(STACK_2)->data[z.index],ascii(':')))
                # Es kommt sofort ein ':'.
                { # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  Car(STACK_0) = S(Kabsolute); # directory = (:ABSOLUTE)
                }
                else
                # Es kommt nicht sofort ein ':'.
                { Car(STACK_0) = S(Krelative); } # directory = (:RELATIVE)
              #endif
              #ifdef PATHNAME_RISCOS
              # Präfix '$.' oder '&.' oder '@.' oder '%.' oder '\.' parsen.
              if ((z.count >= 2) && chareq(TheSstring(STACK_2)->data[z.index+1],ascii('.')))
                { switch (as_cint(TheSstring(STACK_2)->data[z.index]))
                    { case '$': Car(STACK_0) = S(Kroot); break; # directory = (:ABSOLUTE :ROOT)
                      case '&': Car(STACK_0) = S(Khome); break; # directory = (:ABSOLUTE :HOME)
                      case '@': Car(STACK_0) = S(Kcurrent); break; # directory = (:ABSOLUTE :CURRENT)
                      case '%': Car(STACK_0) = S(Klibrary); break; # directory = (:ABSOLUTE :LIBRARY)
                      case '\\': Car(STACK_0) = S(Kprevious); break; # directory = (:ABSOLUTE :PREVIOUS)
                      default: goto prefix_relative;
                    }
                  # Präfix übergehen:
                  z.index+=2; z.FNindex = fixnum_inc(z.FNindex,2); z.count-=2;
                  # (pathname-directory pathname) um ein Cons (:ABSOLUTE) verlängern:
                 {var object new_cons = allocate_cons(); # neues Cons
                  Car(new_cons) = S(Kabsolute); Cdr(new_cons) = STACK_0;
                  ThePathname(STACK_1)->pathname_directory = new_cons;
                }}
                else
                prefix_relative:
                { Car(STACK_0) = S(Krelative); } # directory = (:RELATIVE)
              #endif
              #if !defined(PATHNAME_RISCOS)
              loop
                { # Versuche, ein weiteres Unterdirectory zu parsen.
                  #ifdef PATHNAME_EXT83
                    # Kommt '.\' oder '..\' oder '...\' ?
                    if ((!(z.count == 0)) && chareq(TheSstring(STACK_2)->data[z.index],ascii('.')))
                      { # nächstes Character ist ein '.'.
                        var zustand subdirz = z; # Zustand beim Start des Subdirectories
                        # Character übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        if (z.count == 0) goto no_dots; # String schon zu Ende ?
                       {var chart ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                        if (slashp(ch))
                          # '.\' angetroffen -> (cons :CURRENT NIL) bauen
                          { pushSTACK(S(Kcurrent)); goto dots; }
                        if (!chareq(ch,ascii('.'))) goto no_dots;
                        # zweites Character war auch ein '.'.
                        # Character übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        if (z.count == 0) goto no_dots; # String schon zu Ende ?
                        ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                        if (slashp(ch))
                          # '..\' angetroffen -> (cons :PARENT NIL) bauen
                          { pushSTACK(S(Kparent)); goto dots; }
                        if (!chareq(ch,ascii('.'))) goto no_dots;
                        # drittes Character war auch ein '.'.
                        # Character übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        if (z.count == 0) goto no_dots; # String schon zu Ende ?
                        ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                        if (slashp(ch))
                          # '...\' angetroffen -> (cons :WILD-INFERIORS NIL) bauen
                          { pushSTACK(S(Kwild_inferiors)); goto dots; }
                        goto no_dots;
                       }
                        dots:
                        # '.\' oder '..\' oder '...\' angetroffen, Keyword im Stack.
                        # Character '\' übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        goto subdir_ok;
                        no_dots:
                        z = subdirz; # Zustand wiederherstellen
                      }
                    # Versuche, normale 'name.typ'-Syntax zu parsen:
                    pushSTACK(NIL); # dummy
                    { # Name, hat max. 8 Buchstaben:
                      var object name = parse_name_or_type(&z,8,NIL);
                      STACK_0 = name;
                    }
                    # Versuche, '.typ'-Syntax zu parsen:
                    { var object type;
                      if ((!(z.count==0)) && chareq(TheSstring(STACK_3)->data[z.index],ascii('.')))
                        { # Es kommt ein '.'. Character übergehen:
                          z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                          # Typ, hat max. 3 Buchstaben:
                          type = parse_name_or_type(&z,3,O(leer_string));
                        }
                        else
                        { type = NIL; }
                      pushSTACK(type);
                    }
                    # Stackaufbau: ...,
                    #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
                    #   name, type.
                    # Kommt sofort ein '\', so war es ein Unterdirectory,
                    # sonst ist der Pathname beendet:
                    if ((z.count==0) || !slashp(TheSstring(STACK_4)->data[z.index])) break;
                    # Es kommt ein '\'. Character übergehen:
                    z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                    # name=NIL -> durch "" ersetzen:
                    if (eq(STACK_1,NIL)) { STACK_1 = O(leer_string); }
                    # type=NIL -> durch "" ersetzen:
                    if (eq(STACK_0,NIL)) { STACK_0 = O(leer_string); }
                    { var object new_cons = allocate_cons(); # neues Cons
                      Cdr(new_cons) = popSTACK(); # type
                      Car(new_cons) = popSTACK(); # name
                      # new_cons = (cons name type)
                      pushSTACK(new_cons);
                    }
                    subdir_ok:
                  #endif
                  #ifdef PATHNAME_NOEXT
                    { var uintL z_start_index = z.index; # Index beim Start
                      loop
                        { var chart ch;
                          if (z.count == 0) break;
                          ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                          if (!legal_namechar(ch)) break; # gültiges Character ?
                          # ja -> Teil des Namens
                          # Character übergehen:
                          z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        }
                      # Ende des Namens erreicht.
                      # Name := Teilstring von STACK_2 von z_start_index (einschließlich)
                      #                                bis z.index (ausschließlich).
                     {var object string = subsstring(STACK_2,z_start_index,z.index);
                      # Name fertig.
                      pushSTACK(string);
                    }}
                    # Kommt sofort ein '/' bzw. '\', so war es ein Unterdirectory,
                    # sonst ist der Pathname beendet:
                    if ((z.count==0) || !slashp(TheSstring(STACK_3)->data[z.index]))
                      # Nein -> war der Name und kein Subdir.
                      break;
                    # Es kommt ein '/' bzw. '\'. Character übergehen:
                    z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                    # Stackaufbau: ...,
                    #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
                    #   subdir.
                    #ifdef PATHNAME_AMIGAOS
                    # War es '' ?
                    if (equal(STACK_0,O(leer_string)))
                      { STACK_0 = S(Kparent); } # ja -> durch :PARENT ersetzen
                    else
                    #endif
                    # War es '**' oder '...' ?
                    if (equal(STACK_0,O(wildwild_string)) || equal(STACK_0,O(punktpunktpunkt_string)))
                      { STACK_0 = S(Kwild_inferiors); } # ja -> durch :WILD-INFERIORS ersetzen
                  #endif
                  # (pathname-directory pathname) um Subdir STACK_0 verlängern:
                  { var object new_cons = allocate_cons(); # neues Cons
                    Car(new_cons) = popSTACK(); # = (cons subdir NIL)
                    Cdr(STACK_0) = new_cons; # verlängert (pathname-directory Pathname)
                    STACK_0 = new_cons; # neues (last (pathname-directory Pathname))
                  }
                }
              #else # defined(PATHNAME_RISCOS)
              pushSTACK(unbound); # maybe-name
              # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)),
              #              maybe-name.
              loop
                { # Versuche, ein weiteres Unterdirectory zu parsen.
                  # Maybe-Name = die letzte gelesene Komponente in
                  # { { legal-wild char }+ | empty } '.'  Syntax.
                  # Ob ein weiteres subdir oder der Name, wird sich erst noch
                  # entscheiden.
                  # Kommt '^.' ?
                  if (!nullp(STACK_0)
                      && (z.count >= 2)
                      && chareq(TheSstring(STACK_3)->data[z.index],ascii('^'))
                      && slashp(TheSstring(STACK_3)->data[z.index+1])
                     )
                    { # beide Characters übergehen:
                      z.index+=2; z.FNindex = fixnum_inc(z.FNindex,2); z.count-=2;
                      pushSTACK(S(Kparent)); # :PARENT
                    }
                    else
                    # Versuche, normale  { legal-wild char }+  Syntax zu parsen:
                    { var uintL z_start_index = z.index; # Index beim Start des Namens
                      loop
                        { var chart ch;
                          if (z.count == 0) break;
                          ch = TheSstring(STACK_3)->data[z.index]; # nächstes Character
                          if (!legal_namechar(ch)) break; # gültiges Character ?
                          # ja -> Teil des Namens
                          # Character übergehen:
                          z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        }
                      # Ende des Namens erreicht.
                      # Name := Teilstring von STACK_3 von z_start_index (einschließlich)
                      #                                bis z.index (ausschließlich).
                     {var object string;
                      if (z.index - z_start_index == 0)
                        { string = NIL; } # "" wird zu NIL
                        else
                        { string = subsstring(STACK_3,z_start_index,z.index); }
                      # Name fertig.
                      if (nullp(STACK_0)
                          || (z.count==0)
                          || !slashp(TheSstring(STACK_3)->data[z.index])
                         )
                        { pushSTACK(string); break; }
                      # Character '.' übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      pushSTACK(string);
                    }}
                  if (!eq(STACK_1,unbound))
                    # (pathname-directory pathname) um Subdir STACK_1 verlängern:
                    { var object new_cons = allocate_cons(); # neues Cons
                      Car(new_cons) = STACK_1; # = (cons subdir NIL)
                      Cdr(STACK_2) = new_cons; # verlängert (pathname-directory Pathname)
                      STACK_2 = new_cons; # neues (last (pathname-directory Pathname))
                    }
                  STACK_1 = STACK_0; skipSTACK(1); # maybe-name := subdir
                }
              if (eq(STACK_1,unbound)) { STACK_1 = STACK_0; STACK_0 = NIL; }
              # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)),
              #              name, type.
              # In gewissen Fällen hört die Directory-Angabe nicht nach dem
              # vorletzten Punkt, sondern nach dem letzten Punkt auf:
              elif (eq(STACK_1,S(Kparent)) # z.B. "bar.^.foo"
                    || (nullp(STACK_0) && !nullp(STACK_1)) # z.B. "foo.bar."
                   )
                # (pathname-directory pathname) um Subdir STACK_1 verlängern:
                { var object new_cons = allocate_cons(); # neues Cons
                  Car(new_cons) = STACK_1; # = (cons subdir NIL)
                  Cdr(STACK_2) = new_cons; # verlängert (pathname-directory Pathname)
                  STACK_2 = new_cons; # neues (last (pathname-directory Pathname))
                  STACK_1 = STACK_0; # name := type
                  STACK_0 = NIL;     # type := NIL
                }
              #endif
              #if defined(PATHNAME_EXT83) || defined(PATHNAME_RISCOS)
              # Stackaufbau: ...,
              #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
              #   name, type.
              # Name und Typ in Pathname eintragen:
              { var object type = popSTACK();
                var object name = popSTACK();
                skipSTACK(1); # Directory ist schon eingetragen
               {var object pathname = STACK_0;
                ThePathname(pathname)->pathname_name = name;
                ThePathname(pathname)->pathname_type = type;
              }}
              #endif
              #ifdef PATHNAME_NOEXT
              # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)),
              #              string.
              split_name_type(0); # String STACK_0 in Name und Typ aufspalten
              after_name_type:
              # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)),
              #              name, type.
              # Name und Typ in Pathname eintragen:
              { var object type = popSTACK();
                var object name = popSTACK();
                skipSTACK(1); # Directory ist schon eingetragen
                # name="" durch Name=NIL ersetzen:
                if (equal(name,O(leer_string))) { name = NIL; }
               {var object pathname = STACK_0;
                ThePathname(pathname)->pathname_name = name;
                ThePathname(pathname)->pathname_type = type;
              }}
              #endif
            }
            #undef slashp
          }
        # Pathname fertig.
        # Stackaufbau: ..., Datenvektor, Pathname.
        if (!junk_allowed)
          # Überprüfen, ob keine Zeichen mehr übrig sind:
          if (!(z.count == 0))
            { pushSTACK(z.FNindex); # letzter Index
              pushSTACK(STACK_(4+2+1)); # thing
              pushSTACK(S(parse_namestring));
              fehler(parse_error,
                     GETTEXT("~: syntax error in filename ~ at position ~")
                    );
            }
        #ifdef LOGICAL_PATHNAMES
        if (parse_logical)
          { if (!nullp(STACK_(3+2)))
              # Hosts müssen übereinstimmen, vgl. CLtL2 S. 629
              if (!equal(STACK_(3+2),TheLogpathname(STACK_0)->pathname_host))
                { pushSTACK(STACK_0);
                  pushSTACK(TheLogpathname(STACK_(0+1))->pathname_host);
                  pushSTACK(STACK_(3+2+2));
                  pushSTACK(S(parse_namestring));
                  fehler(error,
                         GETTEXT("~: hosts ~ and ~ of ~ should coincide")
                        );
          }     }
        #endif
        value1 = STACK_0; # Pathname als 1. Wert
        #ifdef PATHNAME_RISCOS
        if (as_oint(z.FNindex) >= as_oint(FNindex_limit))
          # FNindex von new_thing nach thing umrechnen:
          { value2 = fixnum_inc(z.FNindex,FNindex_offset); }
          else
          # FNindex zeigt in den ersetzten (!) String envval. Was bleibt
          # uns als Index anderes übrig als der Start-Index?
          # (Nicht ganz korrekt freilich: Hätte das Parsen wirklich dort
          # aufgehört, würde value1 anders aussehen!)
          # Zum Beispiel ein Index in das Innere des <...>-Konstruktes.
          # (Auch das ist nicht ganz korrekt, kommt der Sache aber näher.)
          { value2 = FNindex_fallback; }
        #else
        value2 = z.FNindex; # Index als 2. Wert
        #endif
        mv_count=2; # 2 Werte
        skipSTACK(5+2); return;
  } }}}

# UP: Wandelt ein Objekt in einen Pathname um.
# coerce_xpathname(object)
# > object: Objekt
# < ergebnis: (PATHNAME Objekt)
# can trigger GC
  local object coerce_xpathname (object obj);
  local object coerce_xpathname(obj)
    var object obj;
    { if (xpathnamep(obj))
        # Bei Pathnames ist nichts zu tun.
        { return obj; }
        else
        # sonst: PARSE-NAMESTRING aufrufen:
        { pushSTACK(subr_self); # subr_self retten (für spätere Fehlermeldungen)
          pushSTACK(obj); funcall(L(parse_namestring),1);
          subr_self = popSTACK();
          return value1;
        }
    }

LISPFUNN(pathname,1)
# (PATHNAME pathname), CLTL S. 413
  { value1 = coerce_xpathname(popSTACK()); mv_count=1; }

LISPFUN(pathnamehost,1,0,norest,key,1, (kw(case)))
# (PATHNAME-HOST pathname), CLTL S. 417, CLtL2 S. 644
  { var object pathname = coerce_xpathname(STACK_1);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(pathname))
      { value1 = TheLogpathname(pathname)->pathname_host; mv_count=1; }
      else
    #endif
      {
        #if HAS_HOST
        var object erg = ThePathname(pathname)->pathname_host;
        value1 = (eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
        mv_count=1; # host als Wert
        #else
        value1 = NIL; mv_count=1; # NIL als Wert
        #endif
      }
    skipSTACK(2);
  }

LISPFUN(pathnamedevice,1,0,norest,key,1, (kw(case)))
# (PATHNAME-DEVICE pathname), CLTL S. 417, CLtL2 S. 644
  { var object pathname = coerce_xpathname(STACK_1);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(pathname))
      { value1 = NIL; mv_count=1; }
      else
    #endif
      {
        #if HAS_DEVICE
        var object erg = ThePathname(pathname)->pathname_device;
        value1 = (eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
        mv_count=1; # device als Wert
        #else
        value1 = NIL; mv_count=1; # NIL als Wert
        #endif
      }
    skipSTACK(2);
  }

LISPFUN(pathnamedirectory,1,0,norest,key,1, (kw(case)))
# (PATHNAME-DIRECTORY pathname), CLTL S. 417, CLtL2 S. 644
  { var object pathname = coerce_xpathname(STACK_1);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(pathname))
      { value1 = TheLogpathname(pathname)->pathname_directory; }
      else
    #endif
      { var object erg = ThePathname(pathname)->pathname_directory;
        value1 = (eq(STACK_0,S(Kcommon)) ? subst_common_case(erg) : erg);
      }
    mv_count=1; # directory als Wert
    skipSTACK(2);
  }

LISPFUN(pathnamename,1,0,norest,key,1, (kw(case)))
# (PATHNAME-NAME pathname), CLTL S. 417, CLtL2 S. 644
  { var object pathname = coerce_xpathname(STACK_1);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(pathname))
      { value1 = TheLogpathname(pathname)->pathname_name; }
      else
    #endif
      { var object erg = ThePathname(pathname)->pathname_name;
        value1 = (eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
      }
    mv_count=1; # name als Wert
    skipSTACK(2);
  }

LISPFUN(pathnametype,1,0,norest,key,1, (kw(case)))
# (PATHNAME-TYPE pathname), CLTL S. 417, CLtL2 S. 644
  { var object pathname = coerce_xpathname(STACK_1);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(pathname))
      { value1 = TheLogpathname(pathname)->pathname_type; }
      else
    #endif
      { var object erg = ThePathname(pathname)->pathname_type;
        value1 = (eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
      }
    mv_count=1; # type als Wert
    skipSTACK(2);
  }

LISPFUNN(pathnameversion,1)
# (PATHNAME-VERSION pathname), CLTL S. 417, CLtL2 S. 644
  { var object pathname = coerce_xpathname(popSTACK());
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(pathname))
      { value1 = TheLogpathname(pathname)->pathname_version; }
      else
    #endif
      {
        #if HAS_VERSION
        value1 = ThePathname(pathname)->pathname_version; # version als Wert
        #else
        value1 = NIL; # NIL als Wert
        #endif
      }
    mv_count=1;
  }

# Zugriffsfunktionen ohne Groß-/Klein-Umwandlung:
# xpathname_host(logical,pathname)
# xpathname_device(logical,pathname)
# xpathname_directory(logical,pathname)
# xpathname_name(logical,pathname)
# xpathname_type(logical,pathname)
# xpathname_version(logical,pathname)
# > pathname: Pathname oder Logical Pathname
# > logical: Flag, ob es sich um einen Logical Pathname handelt
# < ergebnis: Wert der entsprechenden Komponente von pathname
  #ifdef LOGICAL_PATHNAMES
    #if HAS_HOST
      #define xpathname_host(logical,pathname)  \
        (logical ? TheLogpathname(pathname)->pathname_host : ThePathname(pathname)->pathname_host)
    #else
      #define xpathname_host(logical,pathname)  \
        (logical ? TheLogpathname(pathname)->pathname_host : NIL)
    #endif
    #if HAS_DEVICE
      #define xpathname_device(logical,pathname)  \
        (logical ? NIL : ThePathname(pathname)->pathname_device)
    #else
      #define xpathname_device(logical,pathname)  NIL
    #endif
    #define xpathname_directory(logical,pathname)  \
      (logical ? TheLogpathname(pathname)->pathname_directory : ThePathname(pathname)->pathname_directory)
    #define xpathname_name(logical,pathname)  \
      (logical ? TheLogpathname(pathname)->pathname_name : ThePathname(pathname)->pathname_name)
    #define xpathname_type(logical,pathname)  \
      (logical ? TheLogpathname(pathname)->pathname_type : ThePathname(pathname)->pathname_type)
    #if HAS_VERSION
      #define xpathname_version(logical,pathname)  \
        (logical ? TheLogpathname(pathname)->pathname_version : ThePathname(pathname)->pathname_version)
    #else
      #define xpathname_version(logical,pathname)  \
        (logical ? TheLogpathname(pathname)->pathname_version : NIL)
    #endif
  #else
    # logical immer =FALSE
    #if HAS_HOST
      #define xpathname_host(logical,pathname)  ThePathname(pathname)->pathname_host
    #else
      #define xpathname_host(logical,pathname)  NIL
    #endif
    #if HAS_DEVICE
      #define xpathname_device(logical,pathname)  ThePathname(pathname)->pathname_device
    #else
      #define xpathname_device(logical,pathname)  NIL
    #endif
    #define xpathname_directory(logical,pathname)  ThePathname(pathname)->pathname_directory
    #define xpathname_name(logical,pathname)  ThePathname(pathname)->pathname_name
    #define xpathname_type(logical,pathname)  ThePathname(pathname)->pathname_type
    #if HAS_VERSION
      #define xpathname_version(logical,pathname)  ThePathname(pathname)->pathname_version
    #else
      #define xpathname_version(logical,pathname)  NIL
    #endif
  #endif

#ifdef LOGICAL_PATHNAMES

LISPFUNN(logical_pathname,1)
# (LOGICAL-PATHNAME thing), CLtL2 S. 631
  { var object thing = popSTACK();
    if (logpathnamep(thing))
      # Bei Logical Pathnames ist nichts zu tun.
      { value1 = thing; mv_count=1; }
    elif (pathnamep(thing))
      # Normale Pathnames können nicht in Logical Pathnames umgewandelt werden.
      { pushSTACK(thing); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_logical_pathname)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(thing);
        pushSTACK(S(logical_pathname));
        fehler(type_error,
               GETTEXT("~: argument ~ is not a logical pathname, string, stream or symbol")
              );
      }
    else
      # sonst: PARSE-NAMESTRING aufrufen:
      { pushSTACK(subr_self); # subr_self retten (für spätere Fehlermeldungen)
        # Das Ergebnis von (PARSE-NAMESTRING thing nil empty-logical-pathname)
        # ist garantiert ein logischer Pathname.
        pushSTACK(thing); pushSTACK(NIL); pushSTACK(O(empty_logical_pathname));
        funcall(L(parse_namestring),3);
        subr_self = popSTACK();
        mv_count=1;
      }
  }

LISPFUN(translate_logical_pathname,1,0,norest,key,0,_EMA_)
# (TRANSLATE-LOGICAL-PATHNAME pathname &key), CLtL2 S. 631
  { var object pathname;
    # It is not clear from the ANSI CL spec how the argument shall be coerced
    # to a pathname. But the examples in the spec indicate that if the
    # argument is a string, it should be converted to a logical pathname,
    # by calling LOGICAL-PATHNAME, not by calling PATHNAME.
    if (stringp(STACK_0))
      { funcall(L(logical_pathname),1); pathname = value1; }
      else
      { pathname = coerce_xpathname(popSTACK()); }
    if (logpathnamep(pathname))
      { # Umwandeln eines logischen in einen normalen Pathname:
        # (let ((ht (make-hash-table :test #'equal)))
        #   (loop
        #     (when (gethash pathname ht) (error "Translation loop"))
        #     (setf (gethash pathname ht) t)
        #     (let ((host (or (pathname-host pathname) "SYS")))
        #       (unless (logical-host-p host) (error "No translation for host"))
        #       (let* ((translations (gethash host sys::*logical-pathname-translations*))
        #              (translation (assoc pathname translations :test #'pathname-match-p)))
        #         (unless (and translation (consp translation) (consp (cdr translation)))
        #           (error "No translation for pathname")
        #         )
        #         (setq pathname (translate-pathname pathname (first translation) (second translation)))
        #     ) )
        #     (unless (sys::logical-pathname-p pathname) (return))
        #   )
        #   pathname
        # )
        pushSTACK(pathname);
        pushSTACK(S(Ktest)); pushSTACK(L(equal)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # Stackaufbau: pathname, ht.
        loop
          { if (!nullp(shifthash(STACK_0,STACK_1,T)))
              { # STACK_1 = pathname; # Wert für Slot PATHNAME von FILE-ERROR
                STACK_0 = STACK_1;
                pushSTACK(S(translate_logical_pathname));
                fehler(file_error,
                       GETTEXT("~: endless loop while resolving ~")
                      );
              }
            if (nullp(TheLogpathname(STACK_1)->pathname_host))
              # Host NIL durch Default-Host ersetzen:
              { var object newp = allocate_logpathname();
                var object oldp = STACK_1;
                TheLogpathname(newp)->pathname_host      = O(default_logical_pathname_host); # Default "SYS"
                TheLogpathname(newp)->pathname_directory = TheLogpathname(oldp)->pathname_directory;
                TheLogpathname(newp)->pathname_name      = TheLogpathname(oldp)->pathname_name;
                TheLogpathname(newp)->pathname_type      = TheLogpathname(oldp)->pathname_type;
                TheLogpathname(newp)->pathname_version   = TheLogpathname(oldp)->pathname_version;
                STACK_1 = newp;
              }
           {var object host = TheLogpathname(STACK_1)->pathname_host;
            var object translations = gethash(host,Symbol_value(S(logpathname_translations)));
            if (eq(translations,nullobj))
              { # STACK_1 = pathname; # Wert für Slot PATHNAME von FILE-ERROR
                STACK_0 = STACK_1;
                pushSTACK(host);
                pushSTACK(S(translate_logical_pathname));
                fehler(file_error,
                       GETTEXT("~: unknown logical host ~ in ~")
                      );
              }
            # (ASSOC pathname translations :test #'pathname-match-p):
            pushSTACK(STACK_1); pushSTACK(translations);
            pushSTACK(S(Ktest)); pushSTACK(L(pathname_match_p));
            funcall(L(assoc),4);
            if (atomp(value1) || matomp(Cdr(value1)))
              { # STACK_1 = pathname; # Wert für Slot PATHNAME von FILE-ERROR
                STACK_0 = STACK_1;
                pushSTACK(S(translate_logical_pathname));
                fehler(file_error,
                       GETTEXT("~: No replacement rule for ~ is known.")
                      );
              }
            # (TRANSLATE-PATHNAME pathname (first rule) (second rule) :MERGE NIL):
            pushSTACK(STACK_1); pushSTACK(Car(value1)); pushSTACK(Car(Cdr(value1)));
            pushSTACK(S(Kmerge)); pushSTACK(NIL);
            funcall(L(translate_pathname),5);
            STACK_1 = pathname = value1;
            if (!logpathnamep(pathname)) break;
          }}
        skipSTACK(2);
      }
    value1 = pathname; mv_count=1;
  }

# UP: Wandelt ein Objekt in einen nicht-Logical Pathname um.
# coerce_pathname(object)
# > object: Objekt
# < ergebnis: (TRANSLATE-LOGICAL-PATHNAME (PATHNAME Objekt))
# can trigger GC
  local object coerce_pathname (object obj);
  local object coerce_pathname(obj)
    var object obj;
    { obj = coerce_xpathname(obj);
      if (pathnamep(obj))
        # Bei Pathnames ist nichts zu tun.
        { return obj; }
      elif (logpathnamep(obj))
        # TRANSLATE-LOGICAL-PATHNAME aufrufen:
        { pushSTACK(subr_self); # subr_self retten (für spätere Fehlermeldungen)
          pushSTACK(obj); funcall(L(translate_logical_pathname),1);
          subr_self = popSTACK();
          return value1;
        }
      else
        { NOTREACHED }
    }

#endif

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für ein Subdirectory (car path) ergeben.
# subdir_namestring_parts(path)
# > path: ein Cons
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# verändert STACK
  local uintC subdir_namestring_parts (object path);
  local uintC subdir_namestring_parts(path)
    var object path;
    { var object subdir = Car(path);
      #if defined(PATHNAME_MSDOS)
      if (eq(subdir,S(Kcurrent))) # :CURRENT ?
        { pushSTACK(O(punkt_string)); return 1; }
      elif (eq(subdir,S(Kparent))) # :PARENT ?
        { pushSTACK(O(punktpunkt_string)); return 1; }
      elif (eq(subdir,S(Kwild_inferiors))) # :WILD-INFERIORS ?
        { pushSTACK(O(punktpunktpunkt_string)); return 1; }
      else
        # normales subdir (name . type)
        { var object name = Car(subdir);
          var object type = Cdr(subdir);
          # name = :WILD -> String "*"
          if (eq(name,S(Kwild))) { name = O(wild_string); }
          pushSTACK(name);
          # type = :WILD -> String "*"
          if (eq(type,S(Kwild))) { type = O(wild_string); }
          if (Sstring_length(type) == 0)
            # type = "" -> nicht auszugeben
            { return 1+0; }
            else
            { pushSTACK(O(punkt_string)); # "."
              pushSTACK(type);
              return 1+2;
            }
        }
      #endif
      #ifdef PATHNAME_AMIGAOS
      if (eq(subdir,S(Kparent))) # :PARENT ?
        { return 0; } # Leerstring
      elif (eq(subdir,S(Kwild_inferiors))) # :WILD-INFERIORS ?
        { pushSTACK(O(wildwild_string)); return 1; }
      else
        # normales subdir
        { pushSTACK(subdir); return 1; }
      #endif
      #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      if (eq(subdir,S(Kwild_inferiors))) # :WILD-INFERIORS ?
        { pushSTACK(O(wildwild_string)); return 1; }
        else
        # normales subdir
        { pushSTACK(subdir); return 1; }
      #endif
      #ifdef PATHNAME_RISCOS
      if (eq(subdir,S(Kparent))) # :PARENT ?
        { pushSTACK(O(parent_string)); return 1; }
        else
        # normales subdir
        { pushSTACK(subdir); return 1; }
      #endif
    }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für den Host des Pathname pathname ergeben.
# host_namestring_parts(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# verändert STACK
#if HAS_HOST
  local uintC host_namestring_parts (object pathname);
  local uintC host_namestring_parts(pathname)
    var object pathname;
    { var object host = ThePathname(pathname)->pathname_host;
      if (nullp(host))
        { return 0; } # kein String
        else
        { pushSTACK(host);
          pushSTACK(O(doppelpunkt_string)); # ":"
          return 2;
    }   }
#else
  #define host_namestring_parts(pathname)  (unused (pathname), 0)  # keine Strings
#endif

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String fürs Device und Directory des Pathname pathname ergeben.
# directory_namestring_parts(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# verändert STACK
  local uintC directory_namestring_parts (object pathname);
  local uintC directory_namestring_parts (pathname)
    var object pathname;
    { var uintC stringcount = 0; # bisherige Stringzahl = 0
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      # Device:
      { var object device = ThePathname(pathname)->pathname_device;
        if (!(nullp(device))) # NIL -> kein String
          { if (eq(device,S(Kwild))) { device = O(wild_string); } # :WILD -> String "*"
            pushSTACK(device); # Device auf den Stack
            stringcount++; # und mitzählen
      }   }
      #endif
      #ifdef PATHNAME_AMIGAOS
      # Device:
      { var object device = ThePathname(pathname)->pathname_device;
        if (!(nullp(device))) # NIL -> kein String
          { pushSTACK(device); # Device auf den Stack
            stringcount += 1; # und mitzählen
            # Wegen :ABSOLUTE kommt gleich danach ein ":" auf den Stack.
      }   }
      #endif
      #ifdef PATHNAME_RISCOS
      # Device:
      { var object device = ThePathname(pathname)->pathname_device;
        if (!(nullp(device))) # NIL -> kein String
          { pushSTACK(O(doppelpunkt_string)); # ":"
            pushSTACK(device); # Device auf den Stack
            pushSTACK(O(punkt_string)); # "."
            stringcount += 3; # und mitzählen
      }   }
      #endif
      # Directory:
      { var object directory = ThePathname(pathname)->pathname_directory;
        #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
        # evtl. Doppelpunkt:
        if (!(stringcount == 0)) # nur falls schon was auf dem Stack
          { pushSTACK(O(doppelpunkt_string)); stringcount++; } # ":" auf den Stack
        #endif
        # Ist das erste subdir = :ABSOLUTE oder = :RELATIVE ?
        if (eq(Car(directory),S(Kabsolute)))
          #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
          { pushSTACK(O(backslash_string)); stringcount++; } # "\\" auf den Stack
          #endif
          #ifdef PATHNAME_AMIGAOS
          { pushSTACK(O(doppelpunkt_string)); stringcount++; } # ":" auf den Stack
          #endif
          #ifdef PATHNAME_UNIX
          { pushSTACK(O(slash_string)); stringcount++; } # "/" auf den Stack
          #endif
          #ifdef PATHNAME_RISCOS
          { directory = Cdr(directory); # übergehen
           {var object firstdir = Car(directory);
            if (eq(firstdir,S(Kroot)))
              { pushSTACK(O(root_string)); stringcount++; } # "$." auf den Stack
            elif (eq(firstdir,S(Khome)))
              { pushSTACK(O(home_string)); stringcount++; } # "&." auf den Stack
            elif (eq(firstdir,S(Kcurrent)))
              { pushSTACK(O(current_string)); stringcount++; } # "@." auf den Stack
            elif (eq(firstdir,S(Klibrary)))
              { pushSTACK(O(library_string)); stringcount++; } # "%." auf den Stack
            elif (eq(firstdir,S(Kprevious)))
              { pushSTACK(O(previous_string)); stringcount++; } # "\\." auf den Stack
            else
              { NOTREACHED }
          }}
          #endif
        directory = Cdr(directory); # übergehen
        # weitere subdirs auf den Stack:
        while (consp(directory))
          { stringcount += subdir_namestring_parts(directory);
            #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
            pushSTACK(O(backslash_string)); stringcount++; # "\\" auf den Stack
            #endif
            #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
            pushSTACK(O(slash_string)); stringcount++; # "/" auf den Stack
            #endif
            #ifdef PATHNAME_RISCOS
            pushSTACK(O(punkt_string)); stringcount++; # "." auf den Stack
            #endif
            directory = Cdr(directory);
          }
      }
      return stringcount;
    }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für Name und Typ des Pathname ergeben.
# nametype_namestring_parts(name,type,version)
# > name, type, evtl. version: Komponenten des Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# can trigger GC
# verändert STACK
  #if HAS_VERSION
  local uintC nametype_namestring_parts (object name, object type, object version);
  local uintC nametype_namestring_parts (
    var object name,
    var object type,
    var object version)
  #else
  local uintC nametype_namestring_parts_ (object name, object type);
  local uintC nametype_namestring_parts_ (
    var object name,
    var object type)
  #define nametype_namestring_parts(n,t,v)  nametype_namestring_parts_(n,t)
  #endif
    { var uintC stringcount = 0;
      # Name:
      if (!nullp(name)) # name=NIL -> nicht ausgeben
        {
          #if defined(PATHNAME_MSDOS)
          if (eq(name,S(Kwild))) { name = O(wild_string); } # :WILD -> String "*"
          #endif
          pushSTACK(name); # Name auf den Stack
          stringcount++; # und mitzählen
        }
      # Typ:
      if (!nullp(type)) # type=NIL -> nicht ausgeben
        { pushSTACK(O(punkt_string)); # "." auf den Stack
          stringcount++; # und mitzählen
          #if defined(PATHNAME_MSDOS)
          if (eq(type,S(Kwild))) { type = O(wild_string); } # :WILD -> String "*"
          #endif
          pushSTACK(type); # Typ auf den Stack
          stringcount++; # und mitzählen
        }
      #if HAS_VERSION
      if (!nullp(version)) # version=NIL -> nicht ausgeben
        { pushSTACK(O(strichpunkt_string)); # ";" auf den Stack
          stringcount++; # und mitzählen
          if (eq(version,S(Knewest)))
            { pushSTACK(O(zero_string)); } # :NEWEST -> String "0"
            else
            # Version (Integer >0) in String umwandeln: (sys::decimal-string version)
            { pushSTACK(decimal_string(version)); }
          stringcount++; # und mitzählen
        }
      #endif
      return stringcount;
    }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für Name und Typ des Pathname ergeben.
# file_namestring_parts(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# can trigger GC
# verändert STACK
  local uintC file_namestring_parts (object pathname);
  local uintC file_namestring_parts(pathname)
    var object pathname;
    { return nametype_namestring_parts(ThePathname(pathname)->pathname_name,
                                       ThePathname(pathname)->pathname_type,
                                       ThePathname(pathname)->pathname_version);
    }

# UP: Wandelt Pathname in String um.
# whole_namestring(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: Normal-Simple-String
# can trigger GC
  local object whole_namestring (object pathname);
  local object whole_namestring(pathname)
    var object pathname;
    { var uintC stringcount;
      stringcount = host_namestring_parts(pathname); # Strings für den Host
      stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
      stringcount += file_namestring_parts(pathname); # Strings für den Filename
      subr_self = L(namestring); # ("aktuelles" SUBR für Fehlermeldung)
      return string_concat(stringcount); # zusammenhängen
    }

LISPFUNN(file_namestring,1)
# (FILE-NAMESTRING pathname), CLTL S. 417
  { var object pathname = coerce_pathname(popSTACK());
    var uintC stringcount = file_namestring_parts(pathname); # Strings für den Filename
    value1 = string_concat(stringcount); mv_count=1; # zusammenhängen
  }

# UP: Liefert den String zum Directory eines Pathname.
# directory_namestring(pathname)
# > pathname: nicht-Logical Pathname
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Normal-Simple-String
# can trigger GC
  local object directory_namestring (object pathname);
  local object directory_namestring(pathname)
    var object pathname;
    { var uintC stringcount =
        directory_namestring_parts(pathname); # Strings fürs Directory
      return string_concat(stringcount); # zusammenhängen
    }

LISPFUNN(directory_namestring,1)
# (DIRECTORY-NAMESTRING pathname), CLTL S. 417
  { var object pathname = coerce_pathname(popSTACK());
    value1 = directory_namestring(pathname); mv_count=1;
  }

LISPFUNN(host_namestring,1)
# (HOST-NAMESTRING pathname), CLTL S. 417
  { var object pathname = coerce_pathname(popSTACK());
    #if HAS_HOST
    var uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
    value1 = string_concat(stringcount); # zusammenhängen
    #else
    value1 = O(leer_string); # "" als Wert
    #endif
    mv_count=1;
  }

#if HAS_VERSION || defined(LOGICAL_PATHNAMES)
# UP: Überprüft ein optionales VERSION-Argument.
# test_optional_version(def);
# > STACK_0: VERSION-Argument
# > def: Defaultwert dafür
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: gültige Version-Komponente
  local object test_optional_version (object def);
  local object test_optional_version(def)
    var object def;
    { var object version = STACK_0;
      if (eq(version,unbound)) { return def; } # nicht angegeben -> Default
      elif (nullp(version)) {} # NIL ist OK
      elif (eq(version,S(Kwild))) {} # :WILD ist OK
      elif (eq(version,S(Knewest))) {} # :NEWEST ist OK
      elif (posfixnump(version) && !eq(version,Fixnum_0)) {} # Fixnum >0 ist OK
      elif (pathnamep(version)) # Pathname -> dessen Version
        { STACK_0 = xpathname_version(FALSE,version); }
      #ifdef LOGICAL_PATHNAMES
      elif (logpathnamep(version)) # Logical Pathname -> dessen Version
        { STACK_0 = TheLogpathname(version)->pathname_version; }
      #endif
      else # Keiner der gewünschten Fälle -> Fehler:
        { pushSTACK(version); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_version)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(version);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: :VERSION-argument should be NIL or a positive fixnum or :WILD or :NEWEST, not ~")
                );
        }
      return version;
    }
#else
# UP: Überprüft ein optionales VERSION-Argument.
# test_optional_version();
# > STACK_0: VERSION-Argument
# > subr_self: Aufrufer (ein SUBR)
  #define test_optional_version(def)  test_optional_version_()
  local void test_optional_version_ (void);
  local void test_optional_version_()
    { var object version = STACK_0;
      if (eq(version,unbound) # nicht angegeben?
          || nullp(version)         # oder NIL ?
          || eq(version,S(Kwild))   # oder :WILD ?
          || eq(version,S(Knewest)) # oder :NEWEST ?
         )
        { return; } # ja -> OK
        else
        { pushSTACK(version); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_version)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(version);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: :VERSION-argument should be NIL or :WILD or :NEWEST, not ~")
                );
    }   }
#endif

#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)

# Das Betriebssystem verwaltet ein Default-Drive.
# Das Betriebssystem verwaltet auf jedem Drive ein Default-Directory. Dieses
# kann sich allerdings ändern, wenn eine andere Diskette eingelegt wird.

# Es wird ein Default-Drive geführt: DEFAULT_DRIVE = O(default_drive).

# Die Variable *DEFAULT-PATHNAME-DEFAULTS* enthält (als Pathname) den
# Defaultwert für jede MERGE-Operation. Dies ist derjenige, den das System
# in vom Benutzer eingegebene Pathnames "hineininterpretiert".
# Er wird auf dem neuesten Stand des DEFAULT_DRIVE gehalten: bei der
# Initialisierung das aktuelle Device (im Sinne von DOS), bei der
# Änderung von DEFAULT_DRIVE mittels CD.

#endif # PATHNAME_MSDOS || PATHNAME_OS2 || PATHNAME_WIN32

#if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)

# Die Variable *DEFAULT-PATHNAME-DEFAULTS* enthält (als Pathname) den
# Defaultwert für jede MERGE-Operation. Dies ist derjenige, den das System
# in vom Benutzer eingegebene Pathnames "hineininterpretiert".

#endif

#ifdef UNIX

# Das Betriebssystem verwaltet ein Default-Directory ("working directory")
# für diesen Prozess. Es kann mit chdir verändert und mit getwd abgefragt
# werden. Siehe CHDIR(2) und GETWD(3).

#endif

#ifdef AMIGAOS

# Das Betriebssystem verwaltet ein Default-Directory ("current directory")
# für diesen Prozess. Es kann mit CurrentDir verändert und mit einer
# Kombination aus Examine und ParentDir abgefragt werden.

#endif

# UP: Neuberechnung von *DEFAULT-PATHNAME-DEFAULTS*
#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
# aus DEFAULT_DRIVE
#endif
# recalc_defaults_pathname();
# < ergebnis: Wert von *DEFAULT-PATHNAME-DEFAULTS*, ein Pathname
# can trigger GC
  local object recalc_defaults_pathname (void);
  local object recalc_defaults_pathname()
    {
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      # (MAKE-PATHNAME :DEVICE default-drive) ausführen:
      pushSTACK(S(Kdevice)); pushSTACK(O(default_drive));
      funcall(L(make_pathname),2);
      #endif
      #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_RISCOS)
      # (MAKE-PATHNAME) ausführen:
      funcall(L(make_pathname),0);
      #endif
      # und *DEFAULT-PATHNAME-DEFAULTS* zuweisen:
      return Symbol_value(S(default_pathname_defaults)) = value1;
    }

# UP: Liefert den Default-Pathname.
# defaults_pathname()
# < ergebnis: Wert von *DEFAULT-PATHNAME-DEFAULTS*, ein Pathname
# can trigger GC
  local object defaults_pathname (void);
  local object defaults_pathname()
    { var object pathname = Symbol_value(S(default_pathname_defaults)); # Wert von *DEFAULT-PATHNAME-DEFAULTS*
      if (pathnamep(pathname))
        # ist Pathname -> OK
        { return pathname; }
        else
        # sonst Warnung:
        { pushSTACK(subr_self); # subr_self retten (für spätere Fehlermeldungen)
          # (WARN "Der Wert von ~S war kein Pathname. ~:*~S wird zurückgesetzt." ...)
          pushSTACK(OLS(defaults_warn_string));
          pushSTACK(S(default_pathname_defaults));
          funcall(S(warn),2);
          # und neuberechnen:
          pathname = recalc_defaults_pathname();
          subr_self = popSTACK();
          return pathname;
    }   }

LISPFUN(merge_pathnames,1,2,norest,key,1, (kw(wild)))
# (MERGE-PATHNAMES pathname [defaults [default-version]] [:wild]), CLTL S. 415
# (defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) default-version)
#   (setq pathname (pathname pathname))
#   (setq defaults (pathname defaults))
#   (multiple-value-call #'make-pathname
#if HAS_HOST
#     (if (or (equal (pathname-host pathname) (pathname-host defaults))
#             (null (pathname-host pathname))
#         )
#       (values
#         :host (or (pathname-host pathname) (pathname-host defaults))
#endif
#if HAS_DEVICE
#     (if (or (equal (pathname-device pathname) (pathname-device defaults))
#             (null (pathname-device pathname))
#         )
#       (values
#         :device (or (pathname-device pathname) (pathname-device defaults))
#endif
#         :directory
#           (let ((pathname-dir (pathname-directory pathname))
#                 (defaults-dir (pathname-directory defaults)))
#             (if (eq (car pathname-dir) ':RELATIVE)
#               (cond ((null (cdr pathname-dir)) defaults-dir)
#                     ((or *merge-pathnames-ansi*
#                          (not (eq (car defaults-dir) ':RELATIVE)) ; <----
#                      )
#                      (append defaults-dir (cdr pathname-dir))
#                     )
#                     (t pathname-dir)
#               )
#               pathname-dir
#           ) )
#       )
#       (values
#if HAS_HOST
#         :host (pathname-host pathname)
#endif
#if HAS_DEVICE
#         :device (pathname-device pathname)
#endif
#         :directory (pathname-directory pathname)
#     ) )
#     :name (or (pathname-name pathname) (pathname-name defaults))
#     :type (or (pathname-type pathname) (pathname-type defaults))
# ) )
# Ist das :WILD-Argument angegeben, werden statt fehlenden Komponenten
# :WILD-Komponenten ersetzt.
#
# Explanation of the "<----" line:
#
# Roger Kehr <kehr@iti.informatik.th-darmstadt.de> asks why in CLISP
#
# (merge-pathnames (make-pathname :directory '(:relative "x"))
#                  (make-pathname :directory '(:relative "y")))
# => #"x/"
#
# where he expects to get #"y/x/".
#
# There are two reasons for this behaviour:
#
# 1. An informal one: I found the latter behaviour confusing and changed
#    CLISP to do it the former way. It seems to work better this way.
#
# 2. A formal one: MERGE-PATHNAMES is used to specify default components
#    for pathnames, so there is some analogy between (MERGE-PATHNAMES a b)
#    and (or a b). Obviously putting in the same default a second time
#    should do the same as putting it in once:
#
#      (or a b b) is the same as (or a b), so
#
#      (MERGE-PATHNAMES (MERGE-PATHNAMES a b) b) should be the same as
#      (MERGE-PATHNAMES a b).
#
#    (This question actually matters because in Common Lisp there is
#    no distinction between "pathnames with defaults merged-in" and
#    "pathnames with defaults not yet applied". For example, you don't
#    know whether COMPILE-FILE will merge in some defaults.)
#
#    Now, (MERGE-PATHNAMES (MERGE-PATHNAMES '#"x/" '#"y/") '#"y/")
#    and  (MERGE-PATHNAMES '#"x/" '#"y/")
#    are equal in CLISP's implementation, but not in implementations
#    that strictly follow the Common Lisp spec. In fact, the above
#    twice-default = once-default rule holds for all pathnames in CLISP.
#
  { var boolean wildp = !(eq(STACK_0,unbound) || nullp(STACK_0));
    skipSTACK(1);
    # default-version überprüfen:
    #if HAS_VERSION || defined(LOGICAL_PATHNAMES)
    {var object v = test_optional_version(S(Knewest)); # Default ist :NEWEST
     STACK_0 = STACK_1; STACK_1 = STACK_2; STACK_2 = v;
    }# Stackaufbau: default-version, pathname, defaults.
    #else
     test_optional_version(S(Knewest)); skipSTACK(1);
     # Stackaufbau: pathname, defaults.
    #endif
    # pathname und defaults überprüfen:
    # defaults zu einem Pathname machen:
    STACK_0 = test_default_pathname(STACK_0);
    # pathname zu einem Pathname machen:
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(STACK_0))
      { if (!xpathnamep(STACK_1))
          { pushSTACK(subr_self); # subr_self retten (für spätere Fehlermeldungen)
            # Das Ergebnis von (PARSE-NAMESTRING obj nil empty-logical-pathname)
            # ist garantiert ein logischer Pathname.
            pushSTACK(STACK_(1+1)); pushSTACK(NIL); pushSTACK(O(empty_logical_pathname));
            funcall(L(parse_namestring),3);
            subr_self = popSTACK();
            STACK_1 = value1;
      }   }
      else
    #endif
      { STACK_1 = coerce_xpathname(STACK_1); }
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(STACK_1) && logpathnamep(STACK_0))
      # MERGE-PATHNAMES für Logical Pathnames
      { var object newp = allocate_logpathname(); # neuen Pathname holen
        var object d = popSTACK(); # defaults
        var object p = popSTACK(); # pathname
        # Hosts matchen:
        { var object p_host = TheLogpathname(p)->pathname_host;
          var object d_host = TheLogpathname(d)->pathname_host;
          TheLogpathname(newp)->pathname_host = p_host; # erstmal new-host := pathname-host
          if (equal(p_host,d_host)) goto lmatch_directories;
          if (wildp ? FALSE : nullp(p_host))
            { # pathname-host nicht angegeben, aber defaults-host angegeben:
              TheLogpathname(newp)->pathname_host = d_host; # new-host := defaults-host
              goto lmatch_directories;
        }   }
        # Directories nicht matchen:
        { # new-directory := pathname-directory :
          TheLogpathname(newp)->pathname_directory = TheLogpathname(p)->pathname_directory;
        }
        goto ldirectories_OK;
        lmatch_directories:
        # Directories matchen:
        { var object p_directory = TheLogpathname(p)->pathname_directory; # pathname-directory
          var object d_directory = TheLogpathname(d)->pathname_directory; # defaults-directory
          var object new_subdirs = p_directory;
          # Fängt pathname-subdirs mit :RELATIVE an?
          if (!wildp && eq(Car(p_directory),S(Krelative)))
            # ja.
            { # Endet pathname-subdirs danach?
              if (matomp(Cdr(p_directory)))
                # ja -> verwende defaults-subdirs:
                { new_subdirs = d_directory; }
                else
                # nein.
                { # Fängt defaults-subdirs mit :RELATIVE an?
                  if (eq(Car(d_directory),S(Krelative)))
                    # ja -> Ersetzen von :RELATIVE in pathname-subdirs
                    # durch das gesamte defaults-subdirs ist nicht sinnvoll
                    # (da nicht klar ist, auf was das dabei entstehende
                    # Default-Directory sich beziehen soll). Daher nichts tun:
                    {}
                    else
                    # nein -> Um :RELATIVE aufzulösen: ersetze :RELATIVE
                    # in pathname-subdirs durch defaults-subdirs, d.h.
                    # bilde (append defaults-subdirs (cdr pathname-subdirs)) =
                    # (nreconc (reverse defaults-subdirs) (cdr pathname-subdirs)) :
                    { pushSTACK(p); pushSTACK(d); pushSTACK(newp);
                      pushSTACK(Cdr(p_directory));
                      {var object temp = reverse(d_directory);
                       new_subdirs = nreconc(temp,popSTACK());
                      }
                      newp = popSTACK(); d = popSTACK(); p = popSTACK();
                    }
            }   }
          TheLogpathname(newp)->pathname_directory = new_subdirs; # new-directory := new-subdirs
        }
        ldirectories_OK:
        # Nun sind die Directories OK.
        # Name matchen:
        # Verwende pathname-name, falls angegeben, und defaults-name sonst.
        { var object p_name = TheLogpathname(p)->pathname_name;
          TheLogpathname(newp)->pathname_name =
            (!(wildp ? eq(p_name,S(Kwild)) : nullp(p_name))
             ? p_name
             : TheLogpathname(d)->pathname_name
            );
        }
        # Typ matchen:
        # Verwende pathname-type, falls angegeben, und defaults-type sonst.
        { var object p_type = TheLogpathname(p)->pathname_type;
          TheLogpathname(newp)->pathname_type =
            (!(wildp ? eq(p_type,S(Kwild)) : nullp(p_type))
             ? p_type
             : TheLogpathname(d)->pathname_type
            );
        }
        # Version matchen:
        # Verwende pathname-version, falls angegeben, und default-version sonst.
        { var object p_version = TheLogpathname(p)->pathname_version;
          TheLogpathname(newp)->pathname_version =
            (!(wildp ? eq(p_version,S(Kwild)) : nullp(p_version))
             ? p_version
             : STACK_0
            );
          skipSTACK(1);
        }
        # new als Wert:
        value1 = newp; mv_count=1;
        return;
      }
    # nicht beides logische Pathnames -> erst in normale Pathnames umwandeln:
    STACK_1 = coerce_pathname(STACK_1);
    STACK_0 = coerce_pathname(STACK_0);
    #endif
   {var object newp = allocate_pathname(); # neuen Pathname holen
    var object d = popSTACK(); # defaults
    var object p = popSTACK(); # pathname
    #if HAS_HOST
    # Hosts matchen:
    { var object p_host = ThePathname(p)->pathname_host;
      var object d_host = ThePathname(d)->pathname_host;
      ThePathname(newp)->pathname_host = p_host; # erstmal new-host := pathname-host
      # beide Hosts gleich -> Devices matchen:
      if (equal(p_host,d_host)) goto match_devices;
      if (wildp ? FALSE : nullp(p_host))
        { # pathname-host nicht angegeben, aber defaults-host angegeben:
          ThePathname(newp)->pathname_host = d_host; # new-host := defaults-host
          goto match_devices;
        }
      goto notmatch_devices;
    }
    #endif
    match_devices:
    #if HAS_DEVICE
    # Devices matchen:
    { var object p_device = ThePathname(p)->pathname_device;
      var object d_device = ThePathname(d)->pathname_device;
      ThePathname(newp)->pathname_device = p_device; # erstmal new-device := pathname-device
      # beide Devices gleich -> Directories matchen:
      if (equal(p_device,d_device)) goto match_directories;
      if (wildp ? eq(p_device,S(Kwild)) : nullp(p_device))
        { # pathname-device nicht angegeben, aber defaults-device angegeben:
          ThePathname(newp)->pathname_device = d_device; # new-device := defaults-device
          goto match_directories;
        }
      goto notmatch_directories;
    }
    #endif
    # Directories matchen:
    match_directories:
    { var object p_directory = ThePathname(p)->pathname_directory; # pathname-directory
      var object d_directory = ThePathname(d)->pathname_directory; # defaults-directory
      var object new_subdirs = p_directory;
      # Fängt pathname-subdirs mit :RELATIVE an?
      if (!wildp && eq(Car(p_directory),S(Krelative)))
        # ja.
        { # Endet pathname-subdirs danach?
          if (matomp(Cdr(p_directory)))
            # ja -> verwende defaults-subdirs:
            { new_subdirs = d_directory; }
            else
            # nein.
            { # Fängt defaults-subdirs mit :RELATIVE an?
              if (nullp(Symbol_value(S(merge_pathnames_ansi)))
                  && eq(Car(d_directory),S(Krelative)))
                # ja -> Ersetzen von :RELATIVE in pathname-subdirs
                # durch das gesamte defaults-subdirs ist nicht sinnvoll
                # (da nicht klar ist, auf was das dabei entstehende
                # Default-Directory sich beziehen soll). Daher nichts tun:
                {}
                else
                # nein -> Um :RELATIVE aufzulösen: ersetze :RELATIVE
                # in pathname-subdirs durch defaults-subdirs, d.h.
                # bilde (append defaults-subdirs (cdr pathname-subdirs)) =
                # (nreconc (reverse defaults-subdirs) (cdr pathname-subdirs)) :
                { pushSTACK(p); pushSTACK(d); pushSTACK(newp);
                  pushSTACK(Cdr(p_directory));
                  {var object temp = reverse(d_directory);
                   new_subdirs = nreconc(temp,popSTACK());
                  }
                  newp = popSTACK(); d = popSTACK(); p = popSTACK();
                }
        }   }
      ThePathname(newp)->pathname_directory = new_subdirs; # new-directory := new-subdirs
    }
    goto directories_OK;
    # Devices nicht matchen:
    notmatch_devices:
    #if HAS_DEVICE
    { # new-device := pathname-device :
      ThePathname(newp)->pathname_device = ThePathname(p)->pathname_device;
    }
    #endif
    # Directories nicht matchen:
    notmatch_directories:
    { # new-directory := pathname-directory :
      ThePathname(newp)->pathname_directory = ThePathname(p)->pathname_directory;
    }
    directories_OK:
    # Nun sind die Directories OK.
    # Name matchen:
    # Verwende pathname-name, falls angegeben, und defaults-name sonst.
    { var object p_name = ThePathname(p)->pathname_name;
      ThePathname(newp)->pathname_name =
        (!(wildp ?
           #ifdef PATHNAME_EXT83
           eq(p_name,S(Kwild))
           #else # PATHNAME_NOEXT || PATHNAME_RISCOS
           equal(p_name,O(wild_string))
           #endif
           : nullp(p_name)
          )
         ? p_name
         : ThePathname(d)->pathname_name
        );
    }
    # Typ matchen:
    # Verwende pathname-type, falls angegeben, und defaults-type sonst.
    { var object p_type = ThePathname(p)->pathname_type;
      ThePathname(newp)->pathname_type =
        (!(wildp ?
           #ifdef PATHNAME_EXT83
           eq(p_type,S(Kwild))
           #else # PATHNAME_NOEXT || PATHNAME_RISCOS
           equal(p_type,O(wild_string))
           #endif
           : nullp(p_type)
          )
         ? p_type
         : ThePathname(d)->pathname_type
        );
    }
    #if HAS_VERSION
    # Version matchen:
    # Verwende pathname-version, falls angegeben, und default-version sonst.
    { var object p_version = ThePathname(p)->pathname_version;
      ThePathname(newp)->pathname_version =
        (!(wildp ? eq(p_version,S(Kwild)) : nullp(p_version))
         ? p_version
         : STACK_0
        );
    }
    #endif
    #if HAS_VERSION || defined(LOGICAL_PATHNAMES)
    skipSTACK(1);
    #endif
    # new als Wert:
    value1 = newp; mv_count=1;
  }}

LISPFUN(enough_namestring,1,1,norest,nokey,0,NIL)
# (ENOUGH-NAMESTRING pathname [defaults]), CLTL S. 417
# (defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
#   (setq pathname (pathname pathname))
#   (setq defaults (pathname defaults))
#   (namestring
#     (multiple-value-call #'make-pathname
#if HAS_HOST
#       (if (equal (pathname-host pathname) (pathname-host defaults))
#         (values
#           :host nil
#endif
#if HAS_DEVICE
#       (if (equal (pathname-device pathname) (pathname-device defaults))
#         (values
#           :device nil
#endif
#           :directory
#             (let ((pathname-dir (pathname-directory pathname))
#                   (defaults-dir (pathname-directory defaults)))
#               (if (equal pathname-dir defaults-dir)
#                 (list ':RELATIVE)
#                 (if (and (not (eq (car pathname-dir) ':RELATIVE))
#                          (not (eq (car defaults-dir) ':RELATIVE))
#                          (equal (subseq pathname-dir 0 (min (length pathname-dir) (length defaults-dir)))
#                                 defaults-dir
#                     )    )
#                   (cons ':RELATIVE (nthcdr (length defaults-dir) pathname-dir))
#                   pathname-dir
#             ) ) )
#         )
#         (values
#if HAS_HOST
#           :host (pathname-host pathname)
#endif
#if HAS_DEVICE
#           :device (pathname-device pathname)
#endif
#           :directory (pathname-directory pathname)
#       ) )
#       :name (if (equal (pathname-name pathname) (pathname-name defaults))
#               nil
#               (pathname-name pathname)
#             )
#       :type (if (equal (pathname-type pathname) (pathname-type defaults))
#               nil
#               (pathname-type pathname)
#             )
# ) ) )
  { # pathname und defaults überprüfen:
    # pathname zu einem Pathname machen:
    STACK_1 = coerce_pathname(STACK_1);
    # defaults zu einem Pathname machen:
    STACK_0 = coerce_pathname(test_default_pathname(STACK_0));
    # neuen Pathname holen:
   {var object newp = allocate_pathname();
    pushSTACK(newp);
    # Stackaufbau: pathname, defaults, new.
    #if HAS_HOST
    # Hosts vergleichen:
    { var object p_host = ThePathname(STACK_2)->pathname_host; # pathname-host
      var object d_host = ThePathname(STACK_1)->pathname_host; # defaults-host
      if (equal(p_host,d_host)) # beide Hosts gleich ?
        # ja.
        { ThePathname(newp)->pathname_host = NIL; # new-host := NIL
    #endif
    #if HAS_DEVICE
    # Devices vergleichen:
    { var object p_device = ThePathname(STACK_2)->pathname_device; # pathname-device
      var object d_device = ThePathname(STACK_1)->pathname_device; # defaults-device
      if (equal(p_device,d_device)) # beide Devices gleich ?
        # ja.
        { ThePathname(newp)->pathname_device = NIL; # new-device := NIL
    #endif
         {var object p_directory = ThePathname(STACK_2)->pathname_directory; # pathname-directory
          var object d_directory = ThePathname(STACK_1)->pathname_directory; # defaults-directory
          var object new_subdirs;
          # vergleiche pathname-subdirs und defaults-subdirs:
          if (equal(p_directory,d_directory))
            # gleich -> verwende (cons :RELATIVE nil) :
            { new_subdirs = NIL; goto insert_RELATIVE; }
            else
            { # Fängt weder pathname-subdirs noch defaults-subdirs
              # mit :RELATIVE an?
              if (   (!eq(Car(p_directory),S(Krelative)))
                  && (!eq(Car(d_directory),S(Krelative)))
                 )
                # ja -> testen, ob defaults-subdirs ein Anfangsstück
                # der Liste pathname-subdirs ist:
                { var object Lp = p_directory;
                  var object Ld = d_directory;
                  # Ist Ld ein Anfangsstück von Lp ?
                  loop
                    { if (atomp(Ld)) # Ld zu Ende -> ja
                        { new_subdirs = Lp; goto insert_RELATIVE; }
                      if (atomp(Lp)) break; # Lp zu Ende -> nein
                      if (!equal(Car(Ld),Car(Lp))) # verschiedene Listenelemente?
                        break; # -> nein
                      Ld = Cdr(Ld); Lp = Cdr(Lp); # Listen weiterrücken
                    }
                }
              new_subdirs = p_directory; # new-subdirs := pathname-subdirs
              goto subdirs_ok;
            }
          insert_RELATIVE:
          # new-subdirs := (cons :RELATIVE new-subdirs) :
          { pushSTACK(new_subdirs);
            new_subdirs = allocate_cons();
            Cdr(new_subdirs) = popSTACK(); Car(new_subdirs) = S(Krelative);
          }
          subdirs_ok: # new-subdirs ist die neue Subdir-Liste.
          # new-directory := new-subdirs :
          ThePathname(newp=STACK_0)->pathname_directory = new_subdirs;
         }
    #if HAS_DEVICE
        }
        else
        # verschiedene Devices
        { # new-device := pathname-device :
          ThePathname(newp)->pathname_device = p_device;
          # new-directory := pathname-directory :
          ThePathname(newp)->pathname_directory = ThePathname(STACK_2)->pathname_directory;
        }
    }
    #endif
    #if HAS_HOST
        }
        else
        # verschiedene Hosts
        { # new-host := pathname-host :
          ThePathname(newp)->pathname_host = p_host;
          #if HAS_DEVICE
          # new-device := pathname-device :
          ThePathname(newp)->pathname_device = ThePathname(STACK_2)->pathname_device;
          #endif
          # new-directory := pathname-directory :
          ThePathname(newp)->pathname_directory = ThePathname(STACK_2)->pathname_directory;
        }
    }
    #endif
    # name einfüllen:
    { var object p_name = ThePathname(STACK_2)->pathname_name; # pathname-name
      var object d_name = ThePathname(STACK_1)->pathname_name; # defaults-name
      ThePathname(newp)->pathname_name = (equal(p_name,d_name) ? NIL : p_name);
    }
    # type einfüllen:
    { var object p_type = ThePathname(STACK_2)->pathname_type; # pathname-type
      var object d_type = ThePathname(STACK_1)->pathname_type; # defaults-type
      ThePathname(newp)->pathname_type = (equal(p_type,d_type) ? NIL : p_type);
    }
    skipSTACK(3);
    # (namestring new) bilden:
    value1 = whole_namestring(newp); mv_count=1;
  }}

#ifdef LOGICAL_PATHNAMES

# UP: Überprüft, ob object ein zulässiger Name ist:
# :WILD oder ein Simple-String aus gültigen Zeichen, keine adjazenten '*'.
# legal_logical_word(object)
  local boolean legal_logical_word (object obj);
  local boolean legal_logical_word(obj)
    var object obj;
    { if (eq(obj,S(Kwild))) { return TRUE; }
      if (!simple_string_p(obj)) { return FALSE; }
     {var uintL len = Sstring_length(obj);
      if (len==0) { return FALSE; } # leeres Word ist verboten
      {var const chart* charptr = &TheSstring(obj)->data[0];
       var boolean last_was_star = FALSE;
       dotimespL(len,len,
         { var chart ch = *charptr++;
           if (!(legal_logical_word_char(ch) || chareq(ch,ascii('*')))) { return FALSE; }
           if (chareq(ch,ascii('*')))
             { if (last_was_star) return FALSE; # adjazente '*' sind verboten
               last_was_star = TRUE;
             }
             else
             { last_was_star = FALSE; }
         });
       return TRUE;
    }}}

#endif

#ifdef PATHNAME_EXT83

# UP: Überprüft, ob object ein zulässiger Name oder Typ ist: :WILD oder
# ein Simple-String mit max. stdlen Zeichen, alle alphabetisch und Up-case.
# legal_name_or_type(object,stdlen)
  local boolean legal_name_or_type (object obj, uintL stdlen);
  local boolean legal_name_or_type(obj,stdlen)
    var object obj;
    var uintL stdlen;
    { if (eq(obj,S(Kwild))) { return TRUE; } # :WILD ist OK
      if (!simple_string_p(obj)) { return FALSE; } # sonst: Simple-String ?
     {var uintL len = Sstring_length(obj);
      #ifndef EMUNIX_PORTABEL
      if (!(len <= stdlen)) { return FALSE; } # und Länge <=stdlen ?
      #endif
      # Jedes einzelne Zeichen überprüfen:
      if (len > 0)
        { var const chart* ptr = &TheSstring(obj)->data[0];
          dotimespL(len,len,
            { var chart ch = *ptr++;
              if (!(legal_namechar(ch) # zulässiges Zeichen ?
                    && chareq(up_case(ch),ch) # und Großbuchstabe ?
                 ) )
                { return FALSE; }
            });
        }
      return TRUE;
    }}

# UP: Überprüft, ob object ein zulässiger Name ist: :WILD oder
# ein Simple-String mit max. 8 Zeichen, alle alphabetisch und Up-case.
# legal_name(object)
  #define legal_name(obj)  legal_name_or_type(obj,8)

# UP: Überprüft, ob object ein zulässiger Typ ist: :WILD oder
# ein Simple-String mit max. 3 Zeichen, alle alphabetisch und Up-case.
# legal_type(object)
  #define legal_type(obj)  legal_name_or_type(obj,3)

#endif # PATHNAME_EXT83

#if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)

# UP: Überprüft, ob object ein zulässiger Name ist:
# ein Simple-String aus gültigen Zeichen
# legal_name(object)
  local boolean legal_name (object obj);
  local boolean legal_name(obj)
    var object obj;
    { if (!simple_string_p(obj)) { return FALSE; }
     {var uintL len = Sstring_length(obj);
      if (len > 0)
        { var const chart* charptr = &TheSstring(obj)->data[0];
          dotimespL(len,len, { if (!legal_namechar(*charptr++)) { return FALSE; } } );
        }
      return TRUE;
    }}

# UP: Überprüft, ob object ein zulässiger Name ist:
# ein Simple-String aus gültigen Zeichen, ohne '.'
# legal_type(object)
  local boolean legal_type (object obj);
#ifdef PATHNAME_NOEXT
  local boolean legal_type(obj)
    var object obj;
    { if (!simple_string_p(obj)) { return FALSE; }
     {var uintL len = Sstring_length(obj);
      if (len > 0)
        { var const chart* charptr = &TheSstring(obj)->data[0];
          dotimespL(len,len,
            { var chart ch = *charptr++;
              if (chareq(ch,ascii('.')) || !legal_namechar(ch)) { return FALSE; }
            });
        }
      return TRUE;
    }}
#endif
#ifdef PATHNAME_RISCOS
  #define legal_type(obj)  legal_name(obj)
#endif

#endif # PATHNAME_NOEXT || PATHNAME_RISCOS

LISPFUN(make_pathname,0,0,norest,key,8,\
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
# (MAKE-PATHNAME [:host] [:device] [:directory] [:name] [:type] [:version]
#                [:defaults] [:case]),
# CLTL S. 416, CLtL2 S. 643
  # Stackaufbau: defaults, case, host, device, directory, name, type, version.
  { var boolean logical = FALSE;
    var boolean convert = eq(STACK_6,S(Kcommon));
    # 1. host überprüfen:
    #ifdef LOGICAL_PATHNAMES
    # Damit TRANSLATE-PATHNAMES logische Pathnames erzeugen kann:
    if (logpathnamep(STACK_5))
      { STACK_5 = TheLogpathname(STACK_5)->pathname_host;
        logical = TRUE; convert = FALSE;
      }
    #endif
    #if HAS_HOST
    STACK_5 = test_optional_host(STACK_5,convert);
    #else
    STACK_5 = test_optional_host(STACK_5);
    #endif
    #ifdef LOGICAL_PATHNAMES
    if (!nullp(STACK_5) && logical_host_p(STACK_5))
      { logical = TRUE; convert = FALSE; STACK_5 = string_upcase(STACK_5); }
    #endif
    # 2. device überprüfen:
    #if HAS_DEVICE
    { var object device = STACK_4;
      if (eq(device,unbound)) # angegeben ?
        { STACK_4 = NIL; } # nein -> verwende NIL
        else
        { if (stringp(device)) { STACK_4 = device = coerce_normal_ss(device); }
          if (convert) { STACK_4 = device = common_case(device); }
          if (nullp(device)) goto device_ok; # = NIL ?
          #ifdef LOGICAL_PATHNAMES
          elif (logical)
            { if (logpathnamep(device)) # Pathname -> dessen Device
                { STACK_4 = NIL; goto device_ok; }
            }
          #endif
          #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
          elif (eq(device,S(Kwild))) goto device_ok; # = :WILD ?
          elif (simple_string_p(device)) # Simple-String ?
            { if (Sstring_length(device) == 1) # der Länge 1 ?
                { var chart ch = TheSstring(device)->data[0];
                  if ((as_cint(ch) >= 'A') && (as_cint(ch) <= 'Z')) # mit Buchstaben >='A' und <='Z' ?
                    goto device_ok;
            }   }
          #endif
          #ifdef PATHNAME_AMIGAOS
          elif (simple_string_p(device)) # Simple-String ?
            { var uintL count = Sstring_length(device);
              if (count > 0)
                { var const chart* ptr = &TheSstring(device)->data[0];
                  dotimespL(count,count,
                    { if (!legal_namechar(*ptr++)) goto device_not_ok; }
                    );
                }
              goto device_ok;
              device_not_ok: ;
            }
          #endif
          #ifdef PATHNAME_RISCOS
          elif (simple_string_p(device)) # Simple-String ?
            { var uintL count = Sstring_length(device);
              if (count > 0)
                { var const chart* ptr = &TheSstring(device)->data[0];
                  dotimespL(count,count,
                    { var chart ch = *ptr++;
                      if (!(legal_namechar(ch) && !chareq(ch,ascii('*')) && !singlewild_char_p(ch)))
                        goto device_not_ok;
                    });
                }
              goto device_ok;
              device_not_ok: ;
            }
          #endif
          elif (xpathnamep(device)) # Pathname -> dessen Device
            {
              #ifdef LOGICAL_PATHNAMES
              device = coerce_pathname(device);
              #endif
              STACK_4 = ThePathname(device)->pathname_device; goto device_ok;
            }
          # Keiner der gewünschten Fälle -> Fehler:
          pushSTACK(STACK_4); pushSTACK(S(Kdevice)); goto fehler_arg;
          device_ok: ;
    }   }
    #else
    { var object device = STACK_4;
      if (!eq(device,unbound)) # angegeben ?
        if (!(nullp(device) || xpathnamep(device))) # NIL oder Pathname -> OK
          # Keiner der gewünschten Fälle -> Fehler:
          { pushSTACK(STACK_4); pushSTACK(S(Kdevice)); goto fehler_arg; }
    }
    #endif
    # 3. directory überprüfen:
    { var object directory = STACK_3;
      if (eq(directory,unbound) || nullp(directory)) # nicht angegeben oder =NIL ?
        {
          #ifdef PATHNAME_AMIGAOS
          if (!nullp(STACK_4)) # Device angegeben (bei nicht-logical Pathname)?
            STACK_3 = O(directory_absolute); # Default ist (:ABSOLUTE)
          else
          #endif
            STACK_3 = O(directory_default); # Default ist (:RELATIVE)
          goto directory_ok;
        }
      elif (consp(directory)) # ein Cons?
        { STACK_3 = directory = subst_coerce_normal_ss(directory);
          if (convert) { STACK_3 = directory = subst_common_case(directory); }
          # Der CAR entweder :RELATIVE oder :ABSOLUTE ?
          if (!consp(directory)) goto directory_bad;
          { var object startpoint = Car(directory);
            if (!(eq(startpoint,S(Krelative)) || eq(startpoint,S(Kabsolute))))
              goto directory_bad;
            #ifdef PATHNAME_RISCOS
            if (!logical && eq(startpoint,S(Kabsolute)))
              { directory = Cdr(directory);
                startpoint = Car(directory);
                if (!(eq(startpoint,S(Kroot))
                      || eq(startpoint,S(Khome))
                      || eq(startpoint,S(Kcurrent))
                      || eq(startpoint,S(Klibrary))
                      || eq(startpoint,S(Kprevious))
                   ) )
                  goto directory_bad;
              }
            #endif
          }
          directory = Cdr(directory);
          # Subdir-Liste überprüfen:
          while (consp(directory))
            { # nächstes subdir überprüfen:
              var object subdir = Car(directory);
              #ifdef LOGICAL_PATHNAMES
              if (logical)
                { if (!(eq(subdir,S(Kwild_inferiors)) || legal_logical_word(subdir)))
                    goto directory_bad;
                }
                else
              #endif
                {
                  #ifdef PATHNAME_EXT83
                  if (consp(subdir))
                    { # subdir ist ein Cons
                      if (!(legal_name(Car(subdir)) && legal_type(Cdr(subdir))))
                        goto directory_bad;
                    }
                    else
                    { # subdir ist ein Atom
                      if (!(eq(subdir,S(Kcurrent)) # = :CURRENT ?
                            || eq(subdir,S(Kparent)) # = :PARENT ?
                            || eq(subdir,S(Kwild_inferiors)) # = :WILD-INFERIORS ?
                         ) )
                        goto directory_bad;
                    }
                  #endif
                  #ifdef PATHNAME_NOEXT
                  #ifdef PATHNAME_AMIGAOS
                  if (!(eq(subdir,S(Kwild_inferiors)) || eq(subdir,S(Kparent))
                        || legal_name(subdir)
                     ) )
                    goto directory_bad;
                  #endif
                  #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
                  if (!(eq(subdir,S(Kwild_inferiors)) || legal_name(subdir)))
                    goto directory_bad;
                  #endif
                  #endif
                  #ifdef PATHNAME_RISCOS
                  if (!(eq(subdir,S(Kparent)) || legal_name(subdir)))
                    goto directory_bad;
                  #endif
                }
              directory = Cdr(directory);
            }
          goto directory_ok;
        }
      #ifdef LOGICAL_PATHNAMES
      elif (logical)
        { if (logpathnamep(directory)) # Pathname -> dessen Directory
            { STACK_3 = TheLogpathname(directory)->pathname_directory; goto directory_ok; }
        }
      #endif
      elif (xpathnamep(directory)) # Pathname -> dessen Directory
        {
          #ifdef LOGICAL_PATHNAMES
          directory = coerce_pathname(directory);
          #endif
          STACK_3 = ThePathname(directory)->pathname_directory; goto directory_ok;
        }
      # Keiner der gewünschten Fälle -> Fehler:
      directory_bad:
      pushSTACK(STACK_3); pushSTACK(S(Kdirectory)); goto fehler_arg;
      directory_ok: ;
      #ifdef PATHNAME_AMIGAOS
      # Bei device /= NIL muss directory mit :ABSOLUTE anfangen:
      if (!nullp(STACK_4) && !eq(Car(STACK_3),S(Kabsolute))) goto directory_bad;
      #endif
    }
    # 4. name überprüfen:
    { var object name = STACK_2;
      if (stringp(name)) { STACK_2 = name = subst_coerce_normal_ss(name); }
      if (convert) { STACK_2 = name = common_case(name); }
      if (eq(name,unbound))
        { STACK_2 = NIL; } # nicht angegeben -> verwende NIL
      elif (nullp(name)) {} # NIL ist OK
      #ifdef LOGICAL_PATHNAMES
      elif (logical)
        { if (legal_logical_word(name)) {} # OK
          elif (logpathnamep(name)) # Pathname -> dessen Name
            { STACK_2 = TheLogpathname(name)->pathname_name; }
          else # Keiner der gewünschten Fälle -> Fehler:
            { pushSTACK(STACK_2); pushSTACK(S(Kname)); goto fehler_arg; }
        }
      #endif
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
      elif (eq(name,S(Kwild))) { STACK_2 = O(wild_string); } # aus :WILD mache "*"
      #endif
      elif (equal(name,O(leer_string))) # name = "" ?
        { STACK_2 = NIL; } # ja -> verwende NIL
      elif (legal_name(name)) {} # zulässiger Name ist OK
      elif (xpathnamep(name)) # Pathname -> dessen Name
        {
          #ifdef LOGICAL_PATHNAMES
          name = coerce_pathname(name);
          #endif
          STACK_2 = ThePathname(name)->pathname_name;
        }
      else # Keiner der gewünschten Fälle -> Fehler:
        { pushSTACK(STACK_2); pushSTACK(S(Kname)); goto fehler_arg; }
    }
    # 5. type überprüfen:
    { var object type = STACK_1;
      if (stringp(type)) { STACK_1 = type = subst_coerce_normal_ss(type); }
      if (convert) { STACK_1 = type = common_case(type); }
      if (eq(type,unbound))
        { STACK_1 = NIL; } # nicht angegeben -> verwende NIL
      elif (nullp(type)) {} # NIL ist OK
      #ifdef LOGICAL_PATHNAMES
      elif (logical)
        { if (legal_logical_word(type)) {} # OK
          elif (logpathnamep(type)) # Pathname -> dessen Typ
            { STACK_1 = TheLogpathname(type)->pathname_type; }
          else # Keiner der gewünschten Fälle -> Fehler:
            { pushSTACK(STACK_1); pushSTACK(S(Ktype)); goto fehler_arg; }
        }
      #endif
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
      elif (eq(type,S(Kwild))) { STACK_1 = O(wild_string); } # aus :WILD mache "*"
      #endif
      elif (legal_type(type)) {} # zulässiger Typ ist OK
      elif (xpathnamep(type)) # Pathname -> dessen Typ
        {
          #ifdef LOGICAL_PATHNAMES
          type = coerce_pathname(type);
          #endif
          STACK_1 = ThePathname(type)->pathname_type;
        }
      else # Keiner der gewünschten Fälle -> Fehler:
        { pushSTACK(STACK_1); pushSTACK(S(Ktype)); goto fehler_arg; }
    }
    # 6. version überprüfen:
    #if HAS_VERSION || defined(LOGICAL_PATHNAMES)
    STACK_0 = test_optional_version(NIL); # Default ist NIL
    #else
    test_optional_version(NIL);
    #endif
    # 7. Pathname bauen:
    {var object pathname;
     #ifdef LOGICAL_PATHNAMES
     if (logical)
       { pathname = allocate_logpathname(); # neuer Logical Pathname
         TheLogpathname(pathname)->pathname_version   = popSTACK();
         TheLogpathname(pathname)->pathname_type      = popSTACK();
         TheLogpathname(pathname)->pathname_name      = popSTACK();
         TheLogpathname(pathname)->pathname_directory = popSTACK();
         skipSTACK(1);
         TheLogpathname(pathname)->pathname_host      = popSTACK();
       }
       else
     #endif
       { pathname = allocate_pathname(); # neuer Pathname
         #if HAS_VERSION
         ThePathname(pathname)->pathname_version   = popSTACK();
         #else
         skipSTACK(1);
         #endif
         ThePathname(pathname)->pathname_type      = popSTACK();
         ThePathname(pathname)->pathname_name      = popSTACK();
         ThePathname(pathname)->pathname_directory = popSTACK();
         #if HAS_DEVICE
         ThePathname(pathname)->pathname_device    = popSTACK();
         #else
         skipSTACK(1);
         #endif
         #if HAS_HOST
         ThePathname(pathname)->pathname_host      = popSTACK();
         #else
         skipSTACK(1);
         #endif
       }
    skipSTACK(1); # case vergessen
    # 8. evtl. Defaults hineinmergen:
     {var object defaults = popSTACK();
      if (eq(defaults,unbound))
        # keine Defaults angegeben -> pathname als Wert
        { value1 = pathname; }
        else
        # (MERGE-PATHNAMES pathname defaults [nil]) aufrufen:
        { pushSTACK(pathname); pushSTACK(defaults); pushSTACK(NIL);
          funcall(L(merge_pathnames),3);
        }
      mv_count=1;
      return;
    }}
    # Fehlermeldung:
    fehler_arg:
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,
           GETTEXT("~: illegal ~ argument ~")
          );
  }

#ifdef LOGICAL_PATHNAMES

LISPFUN(make_logical_pathname,0,0,norest,key,8,\
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
# (MAKE-LOGICAL-PATHNAME [:host] [:device] [:directory] [:name] [:type] [:version]
#                        [:defaults] [:case]),
# wie MAKE-PATHNAME, nur dass ein Logical Pathname gebildet wird.
  { # Ein logischer Pathname als :HOST-Argument zu MAKE-PATHNAME
    # erzwingt einen logischen Pathname als Ergebnis.
    var object obj = allocate_logpathname();
    TheLogpathname(obj)->pathname_host = (!eq(STACK_5,unbound) ? STACK_5 : NIL);
    STACK_5 = obj;
    # weiter bei MAKE-PATHNAME.
    C_make_pathname();
  }

#endif

#ifdef USER_HOMEDIR
LISPFUN(user_homedir_pathname,0,1,norest,nokey,0,NIL)
# (USER-HOMEDIR-PATHNAME [host]), CLTL S. 418
  {
    #if HAS_HOST
    STACK_0 = test_optional_host(STACK_0,FALSE); # Host überprüfen
    #ifdef PATHNAME_RISCOS
    {var object pathname = allocate_pathname(); # neuer Pathname
     ThePathname(pathname)->pathname_host      = popSTACK();
     #if HAS_DEVICE
     ThePathname(pathname)->pathname_device    = NIL;
     #endif
     ThePathname(pathname)->pathname_directory = O(directory_homedir);
     ThePathname(pathname)->pathname_name      = NIL;
     ThePathname(pathname)->pathname_type      = NIL;
     #if HAS_VERSION
     ThePathname(pathname)->pathname_version   = NIL;
     #endif
     value1 = pathname;
    }
    #else
    ??
    #endif
    #else
    test_optional_host(popSTACK()); # Host überprüfen und ignorieren
    value1 = O(user_homedir); # User-Homedir-Pathname
    #endif
    mv_count=1; # als Wert
  }
#endif

# UP: Kopiert einen Pathname.
# copy_pathname(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: Kopie des Pathname, mit denselben Komponenten
# can trigger GC
  local object copy_pathname (object pathname);
  local object copy_pathname(pathname)
    var object pathname;
    { pushSTACK(pathname);
     {var object newp = allocate_pathname();
      pathname = popSTACK();
      #if HAS_HOST
      ThePathname(newp)->pathname_host      = ThePathname(pathname)->pathname_host     ;
      #endif
      #if HAS_DEVICE
      ThePathname(newp)->pathname_device    = ThePathname(pathname)->pathname_device   ;
      #endif
      ThePathname(newp)->pathname_directory = ThePathname(pathname)->pathname_directory;
      ThePathname(newp)->pathname_name      = ThePathname(pathname)->pathname_name     ;
      ThePathname(newp)->pathname_type      = ThePathname(pathname)->pathname_type     ;
      #if HAS_VERSION
      ThePathname(newp)->pathname_version   = ThePathname(pathname)->pathname_version  ;
      #endif
      return newp;
    }}

# Wildcards
# =========

#if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
# UP: Testet, ob ein Simple-String Wildcards enthält.
# has_wildcards(string)
# > string: Normal-Simple-String
# < ergebnis: TRUE wenn string Wildcard-Zeichen enthält
  local boolean has_wildcards (object string);
  local boolean has_wildcards(string)
    var object string;
    { var uintL len = Sstring_length(string);
      if (len > 0)
        { var const chart* charptr = &TheSstring(string)->data[0];
          dotimespL(len,len,
            { var chart ch = *charptr++;
              if (chareq(ch,ascii('*')) # Wildcard für beliebig viele Zeichen
                  || singlewild_char_p(ch) # Wildcard für genau ein Zeichen
                 )
                { return TRUE; }
            });
        }
      return FALSE;
    }
#endif

#ifdef LOGICAL_PATHNAMES
# UP: Testet, ob ein Simple-String Wildcards enthält.
# has_word_wildcards(string)
# > string: Normal-Simple-String
# < ergebnis: TRUE wenn string Wildcard-Zeichen enthält
  local boolean has_word_wildcards (object string);
  local boolean has_word_wildcards(string)
    var object string;
    { var uintL len = Sstring_length(string);
      if (len > 0)
        { var const chart* charptr = &TheSstring(string)->data[0];
          dotimespL(len,len, { if (chareq(*charptr++,ascii('*'))) { return TRUE; } } );
        }
      return FALSE;
    }
#endif

# UP: Testet, ob die Host-Komponente eines Pathname Wildcards enthält.
# has_host_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn (PATHNAME-HOST pathname) Wildcards enthält.
  local boolean has_host_wildcards (object pathname);
  # Host kann keine Wildcards enthalten.
  #define has_host_wildcards(pathname)  (unused (pathname), FALSE)

# UP: Testet, ob die Device-Komponente eines Pathname Wildcards enthält.
# has_device_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn (PATHNAME-DEVICE pathname) Wildcards enthält.
  local boolean has_device_wildcards (object pathname);
  local boolean has_device_wildcards(pathname)
    var object pathname;
    {
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(pathname))
        return FALSE;
      #endif
      # Device überprüfen: = :WILD ?
      return eq(ThePathname(pathname)->pathname_device,S(Kwild));
      #else
      return FALSE;
      #endif
    }

# UP: Testet, ob die Directory-Komponente eines Pathname Wildcards enthält.
# has_directory_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn (PATHNAME-DIRECTORY pathname) Wildcards enthält.
  local boolean has_directory_wildcards (object pathname);
  local boolean has_directory_wildcards(pathname)
    var object pathname;
    { # Directory überprüfen:
      #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(pathname))
        { var object directory = TheLogpathname(pathname)->pathname_directory;
          while (consp(directory = Cdr(directory)))
            { var object subdir = Car(directory);
              if (simple_string_p(subdir))
                { if (has_word_wildcards(subdir)) return TRUE; }
                else
                { if (eq(subdir,S(Kwild)) || eq(subdir,S(Kwild_inferiors)))
                    return TRUE;
            }   }
          return FALSE;
        }
      #endif
     {var object directory = ThePathname(pathname)->pathname_directory;
      while (consp(directory = Cdr(directory)))
        { var object subdir = Car(directory);
          #ifdef PATHNAME_EXT83
          if (consp(subdir))
            { # subdir ist ein Cons. name oder type = :WILD ?
              if (eq(Car(subdir),S(Kwild)) || eq(Cdr(subdir),S(Kwild)))
                return TRUE;
            }
            else
            { # subdir ist ein Atom. = :WILD-INFERIORS ?
              if (eq(subdir,S(Kwild_inferiors)))
                return TRUE;
            }
          #endif
          #ifdef PATHNAME_NOEXT
          if (simple_string_p(subdir))
            { if (has_wildcards(subdir)) return TRUE; }
            else
            { if (eq(subdir,S(Kwild_inferiors))) return TRUE; }
          #endif
          #ifdef PATHNAME_RISCOS
          if (simple_string_p(subdir))
            { if (has_wildcards(subdir)) return TRUE; }
          #endif
        }
      return FALSE;
    }}

# UP: Testet, ob die Name-Komponente eines Pathname Wildcards enthält.
# has_name_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn (PATHNAME-NAME pathname) Wildcards enthält.
  local boolean has_name_wildcards (object pathname);
  local boolean has_name_wildcards(pathname)
    var object pathname;
    { # Name überprüfen:
      #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(pathname))
        { var object name = TheLogpathname(pathname)->pathname_name;
          if (simple_string_p(name))
            { if (has_word_wildcards(name)) return TRUE; }
            else
            { if (eq(name,S(Kwild))) return TRUE; }
          return FALSE;
        }
      #endif
      #ifdef PATHNAME_EXT83
      if (eq(ThePathname(pathname)->pathname_name,S(Kwild))) # Name = :WILD ?
        return TRUE;
      #endif
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
      { var object name = ThePathname(pathname)->pathname_name;
        if (simple_string_p(name))
          { if (has_wildcards(name)) return TRUE; }
      }
      #endif
      return FALSE;
    }

# UP: Testet, ob die Type-Komponente eines Pathname Wildcards enthält.
# has_type_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn (PATHNAME-TYPE pathname) Wildcards enthält.
  local boolean has_type_wildcards (object pathname);
  local boolean has_type_wildcards(pathname)
    var object pathname;
    { # Typ überprüfen:
      #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(pathname))
        { var object type = TheLogpathname(pathname)->pathname_type;
          if (simple_string_p(type))
            { if (has_word_wildcards(type)) return TRUE; }
            else
            { if (eq(type,S(Kwild))) return TRUE; }
          return FALSE;
        }
      #endif
      #ifdef PATHNAME_EXT83
      if (eq(ThePathname(pathname)->pathname_type,S(Kwild))) # Typ = :WILD ?
        return TRUE;
      #endif
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
      { var object type = ThePathname(pathname)->pathname_type;
        if (simple_string_p(type))
          { if (has_wildcards(type)) return TRUE; }
      }
      #endif
      return FALSE;
    }

# UP: Testet, ob die Version-Komponente eines Pathname Wildcards enthält.
# has_version_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn (PATHNAME-VERSION pathname) Wildcards enthält.
  local boolean has_version_wildcards (object pathname);
  local boolean has_version_wildcards(pathname)
    var object pathname;
    { # Version überprüfen:
      #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(pathname))
        { if (eq(TheLogpathname(pathname)->pathname_version,S(Kwild)))
            return TRUE;
          return FALSE;
        }
      #endif
      return FALSE;
    }

# UP: Testet, ob irgendeine Komponente eines Pathname Wildcards enthält.
# has_some_wildcards(pathname)
# > pathname: Pathname
# < ergebnis: TRUE wenn pathname Wildcards enthält.
  local boolean has_some_wildcards (object pathname);
  local boolean has_some_wildcards(pathname)
    var object pathname;
    { if (has_host_wildcards(pathname)) return TRUE;
      if (has_device_wildcards(pathname)) return TRUE;
      if (has_directory_wildcards(pathname)) return TRUE;
      if (has_name_wildcards(pathname)) return TRUE;
      if (has_type_wildcards(pathname)) return TRUE;
      if (has_version_wildcards(pathname)) return TRUE;
      return FALSE;
    }

# UP: Überprüft, ob ein Pathname keine Wildcards enthält.
# check_no_wildcards(pathname);
# > pathname: Pathname
  local void check_no_wildcards (object pathname);
  local void check_no_wildcards(pathname)
    var object pathname;
    { if (!has_some_wildcards(pathname))
        # Keine Wildcards gefunden.
        return;
      # Fehlermeldung, wenn der Pathname Wildcards enthält:
      pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(pathname);
      fehler(file_error,
             GETTEXT("wildcards are not allowed here: ~")
            );
    }

LISPFUN(wild_pathname_p,1,1,norest,nokey,0,NIL)
# (WILD-PATHNAME-P pathname [field-key]), CLtL2 S. 623
  { var object pathname = coerce_xpathname(STACK_1);
    var object key = STACK_0;
    var boolean erg;
    if (eq(key,unbound) || nullp(key)) { erg = has_some_wildcards(pathname); }
    elif (eq(key,S(Khost))) { erg = has_host_wildcards(pathname); }
    elif (eq(key,S(Kdevice))) { erg = has_device_wildcards(pathname); }
    elif (eq(key,S(Kdirectory))) { erg = has_directory_wildcards(pathname); }
    elif (eq(key,S(Kname))) { erg = has_name_wildcards(pathname); }
    elif (eq(key,S(Ktype))) { erg = has_type_wildcards(pathname); }
    elif (eq(key,S(Kversion))) { erg = has_version_wildcards(pathname); }
    else
      { pushSTACK(key); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_pathname_field_key)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(NIL);
        pushSTACK(S(Kversion));
        pushSTACK(S(Ktype));
        pushSTACK(S(Kname));
        pushSTACK(S(Kdirectory));
        pushSTACK(S(Kdevice));
        pushSTACK(S(Khost));
        pushSTACK(key);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: argument ~ should be ~, ~, ~, ~, ~, ~ or ~")
              );
      }
    value1 = (erg ? T : NIL); mv_count=1; # boolescher Wert
    skipSTACK(2);
  }

# Wildcard Matching
# =================

# For the purposes of wildcard matching, according to CLHS, non-present
# components (i.e. NIL or a directory = (:RELATIVE)) are treated as wild.

#if defined(PATHNAME_NOEXT) || defined(LOGICAL_PATHNAMES)

  # UP: Matcht einen Wildcard-String ("Muster") mit einem "Beispiel".
  # > muster: Normal-Simple-String, mit Platzhaltern
  #           '?' für genau 1 Zeichen
  #           '*' für beliebig viele Zeichen
  # > beispiel: Normal-Simple-String, der damit zu matchen ist
  local boolean wildcard_match (object muster, object beispiel);
  # rekursive Implementation wegen Backtracking:
  local boolean wildcard_match_ab (uintL m_count, const chart* m_ptr, uintL b_count, const chart* b_ptr);
  local boolean wildcard_match(muster,beispiel)
    var object muster;
    var object beispiel;
    { return wildcard_match_ab(
                               /* m_count = */ Sstring_length(muster),
                               /* m_ptr   = */ &TheSstring(muster)->data[0],
                               /* b_count = */ Sstring_length(beispiel),
                               /* b_ptr   = */ &TheSstring(beispiel)->data[0]
                              );
    }
  local boolean wildcard_match_ab(m_count,m_ptr,b_count,b_ptr)
    var uintL m_count;
    var const chart* m_ptr;
    var uintL b_count;
    var const chart* b_ptr;
    { var chart c;
      loop
        { if (m_count==0)
            { return (b_count==0 ? TRUE : FALSE); } # "" matcht nur ""
          m_count--;
          c = *m_ptr++; # nächstes Match-Zeichen
          if (chareq(c,ascii('?'))) # Wildcard '?'
            { if (b_count==0) return FALSE; # mindestens ein Zeichen muss noch kommen
              b_count--; b_ptr++; # es wird ignoriert
            }
          elif (chareq(c,ascii('*'))) break; # Wildcard '*' später
          else # alles andere muss genau matchen:
            { if (b_count==0) return FALSE;
              b_count--; if (!equal_pathchar(*b_ptr++,c)) return FALSE;
            }
        }
      # Wildcard '*': Suche nächstes non-Wildcard-Zeichen und zähle die '?'
      # mit (denn eine Folge '*??*???***?' matcht alles, was mindestens so
      # lang ist, wie die Folge Fragezeichen enthält). Man kann die '?' auch
      # gleich verwerten, denn '*??*???***?' ist zu '??????*' äquivalent.
      loop
        { if (m_count==0) return TRUE; # Wildcard am Ende matcht den Rest.
          m_count--;
          c = *m_ptr++; # nächstes Match-Zeichen
          if (chareq(c,ascii('?'))) # Fragezeichen: nach vorne ziehen, sofort abarbeiten
            { if (b_count==0) return FALSE;
              b_count--; b_ptr++;
            }
          elif (!chareq(c,ascii('*'))) break;
        }
      # c = nächstes non-Wildcard-Zeichen. Suche es.
      loop
        { if (b_count==0) return FALSE; # c nicht gefunden
          b_count--;
          if (equal_pathchar(*b_ptr++,c))
            { if (wildcard_match_ab(m_count,m_ptr,b_count,b_ptr))
                return TRUE;
        }   }
    }

#endif

# UPs: Matcht jeweils eine Pathname-Komponente ("Beispiel") und
# eine Pathname-Komponente ("Muster").
  local boolean host_match (object muster, object beispiel, boolean logical);
  local boolean device_match (object muster, object beispiel, boolean logical);
  local boolean directory_match (object muster, object beispiel, boolean logical);
  local boolean nametype_match (object muster, object beispiel, boolean logical);
  local boolean version_match (object muster, object beispiel, boolean logical);
  local boolean host_match(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (nullp(muster)) return TRUE;
          return equal(muster,beispiel);
        }
      #endif
      #if HAS_HOST
      if (nullp(muster)) return TRUE;
      return equal(muster,beispiel);
      #else
      return TRUE;
      #endif
    }
  local boolean device_match(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    {
      #if HAS_DEVICE
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { return TRUE; }
      #endif
      if (nullp(muster)) return TRUE;
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      if (eq(muster,S(Kwild))) return TRUE;
      if (eq(beispiel,S(Kwild))) return FALSE;
      #endif
      #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      return equalp(muster,beispiel);
      #else
      return equal(muster,beispiel);
      #endif
      #else
      return TRUE;
      #endif
    }
  local boolean nametype_match_aux (object muster, object beispiel, boolean logical);
  local boolean nametype_match_aux(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (eq(muster,S(Kwild))) return TRUE;
          if (eq(beispiel,S(Kwild))) return FALSE;
          if (nullp(muster))
            { if (nullp(beispiel)) return TRUE; else return FALSE; }
          if (nullp(beispiel))
            { return FALSE; }
          return wildcard_match(muster,beispiel);
        }
      #endif
      #ifdef PATHNAME_EXT83
      if (eq(muster,S(Kwild))) return TRUE;
      if (eq(beispiel,S(Kwild))) return FALSE;
      return equal(muster,beispiel);
      #endif
      #ifdef PATHNAME_NOEXT
      if (nullp(muster))
        { if (nullp(beispiel)) return TRUE; else return FALSE; }
      if (nullp(beispiel))
        { return FALSE; }
      return wildcard_match(muster,beispiel);
      #endif
    }
  local boolean subdir_match(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    { if (eq(muster,beispiel)) return TRUE;
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (eq(muster,S(Kwild))) return TRUE;
          if (!simple_string_p(muster) || !simple_string_p(beispiel)) return FALSE;
          return wildcard_match(muster,beispiel);
        }
      #endif
      #ifdef PATHNAME_EXT83
      if (atomp(muster) || atomp(beispiel)) return FALSE;
      return (nametype_match_aux(Car(muster),Car(beispiel),FALSE)
              && nametype_match_aux(Cdr(muster),Cdr(beispiel),FALSE)
             );
      #endif
      #ifdef PATHNAME_NOEXT
      if (!simple_string_p(muster) || !simple_string_p(beispiel)) return FALSE;
      return wildcard_match(muster,beispiel);
      #endif
    }
  # rekursive Implementation wegen Backtracking:
  local boolean directory_match_ab (object m_list, object b_list, boolean logical);
  local boolean directory_match_ab(m_list,b_list,logical)
    var object m_list;
    var object b_list;
    var boolean logical;
    { # Algorithmus analog zu wildcard_match_ab.
      var object item;
      loop
        { if (atomp(m_list)) { return atomp(b_list); }
          item = Car(m_list); m_list = Cdr(m_list);
          if (eq(item,S(Kwild_inferiors))) break;
          if (atomp(b_list)) return FALSE;
          if (!subdir_match(item,Car(b_list),logical)) return FALSE;
          b_list = Cdr(b_list);
        }
      loop
        { if (atomp(m_list)) return TRUE;
          item = Car(m_list); m_list = Cdr(m_list);
          if (!eq(item,S(Kwild_inferiors))) break;
        }
      loop
        { if (atomp(b_list)) return FALSE;
          if (subdir_match(item,Car(b_list),logical))
            { b_list = Cdr(b_list);
              if (directory_match_ab(m_list,b_list,logical)) return TRUE;
            }
            else
            { b_list = Cdr(b_list); }
        }
    }
  local boolean directory_match(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    { # muster mit O(directory_default) vergleichen:
      if (eq(Car(muster),S(Krelative)) && nullp(Cdr(muster)))
        return TRUE;
      # Startpoint matchen:
      if (!eq(Car(muster),Car(beispiel)))
        return FALSE;
      muster = Cdr(muster); beispiel = Cdr(beispiel);
      # subdirs matchen:
      return directory_match_ab(muster,beispiel,logical);
    }
  local boolean nametype_match(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    { if (nullp(muster)) return TRUE;
      return nametype_match_aux(muster,beispiel,logical);
    }
  local boolean version_match(muster,beispiel,logical)
    var object muster;
    var object beispiel;
    var boolean logical;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (nullp(muster) || eq(muster,S(Kwild))) return TRUE;
          return eql(muster,beispiel);
        }
      #endif
      #if HAS_VERSION
      if (nullp(muster) || eq(muster,S(Kwild))) return TRUE;
      if (eq(beispiel,S(Kwild))) return FALSE;
      if (eql(muster,beispiel)) return TRUE;
      return FALSE;
      #else
      return TRUE;
      #endif
    }

LISPFUNN(pathname_match_p,2)
# (PATHNAME-MATCH-P pathname wildname), CLtL2 S. 623
  { # Stackaufbau: pathname, wildname.
    var boolean logical = FALSE;
    STACK_1 = coerce_xpathname(STACK_1);
    STACK_0 = coerce_xpathname(STACK_0);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(STACK_1) && logpathnamep(STACK_0))
      { logical = TRUE; }
      else
      # nicht beides logische Pathnames -> erst in normale Pathnames umwandeln:
      { STACK_1 = coerce_pathname(STACK_1);
        STACK_0 = coerce_pathname(STACK_0);
      }
    #endif
   {var object wildname = popSTACK();
    var object pathname = popSTACK();
    if (!host_match(xpathname_host(logical,wildname),
                    xpathname_host(logical,pathname),
                    logical
       )           )
      goto no;
    if (!device_match(xpathname_device(logical,wildname),
                      xpathname_device(logical,pathname),
                      logical
       )             )
      goto no;
    if (!directory_match(xpathname_directory(logical,wildname),
                         xpathname_directory(logical,pathname),
                         logical
       )                )
      goto no;
    if (!nametype_match(xpathname_name(logical,wildname),
                        xpathname_name(logical,pathname),
                        logical
       )               )
      goto no;
    if (!nametype_match(xpathname_type(logical,wildname),
                        xpathname_type(logical,pathname),
                        logical
       )               )
      goto no;
    if (!version_match(xpathname_version(logical,wildname),
                       xpathname_version(logical,pathname),
                       logical
       )              )
      goto no;
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }}

# (TRANSLATE-PATHNAME beispiel muster1 muster2) machen wir folgendermaßen:
# 1. (PATHNAME-MATCH-P beispiel muster1) nachrechnen, dabei aber die
#    Substitution aufheben, in Form von Textstücken (:WILD -> "*").
# 2. In muster2 die Textstücke einsetzen, bis muster2 voll ist oder die
#    Textstücke aufgebraucht sind.
# 3. Zum Schluss (MERGE-PATHNAMES modifiziertes_muster2 beispiel).

  # UP: Vergleicht einen Wildcard-String ("Muster") mit einem "Beispiel".
  # wildcard_diff(muster,beispiel,previous,solutions);
  # > muster: Normal-Simple-String, mit Platzhaltern
  #           '?' für genau 1 Zeichen
  #           '*' für beliebig viele Zeichen
  # > beispiel: Normal-Simple-String, der damit zu vergleichen ist
  # > previous: bisher bekanntes Vergleichsergebnis
  #             (umgedrehte Liste von Normal-Simple-Strings, NILs und Listen)
  # > solutions: Pointer auf eine Liste im STACK, auf die die
  #              Vergleichsergebnisse (umgedrehte Liste von
  #              Normal-Simple-Strings und Listen) zu consen sind
  # kann GC auslösen

  # Hier wünscht man sich nicht Lisp oder C, sondern PROLOG als Sprache!

  #define push_solution()  \
    { var object new_cons = allocate_cons(); \
      Car(new_cons) = *previous;             \
      Cdr(new_cons) = *solutions;            \
      *solutions = new_cons;                 \
    }
  #define push_solution_with(new_piece)  \
    { pushSTACK(new_piece);                                   \
     {var object new_cons = allocate_cons();                  \
      Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;     \
      STACK_0 = new_cons;                                     \
      new_cons = allocate_cons();                             \
      Car(new_cons) = popSTACK(); Cdr(new_cons) = *solutions; \
      *solutions = new_cons;                                  \
    }}

#if defined(PATHNAME_NOEXT) || defined(LOGICAL_PATHNAMES)

  local void wildcard_diff (object muster, object beispiel, const object* previous, object* solutions);

  # rekursive Implementation wegen Backtracking:
  local void wildcard_diff_ab (object muster, object beispiel, uintL m_index, uintL b_index, const object* previous, object* solutions);

  local void wildcard_diff(muster,beispiel,previous,solutions)
    var object muster;
    var object beispiel;
    var const object* previous;
    var object* solutions;
    { wildcard_diff_ab(muster,beispiel,0,0,previous,solutions); }

  local void wildcard_diff_ab(muster,beispiel,m_index,b_index,previous,solutions)
    var object muster;
    var object beispiel;
    var uintL m_index;
    var uintL b_index;
    var const object* previous;
    var object* solutions;
    { var chart c;
      loop
        { if (m_index == Sstring_length(muster))
            { if (b_index == Sstring_length(beispiel))
                { push_solution(); }
              return;
            }
          c = TheSstring(muster)->data[m_index++];
          if (chareq(c,ascii('*'))) break;
          if (b_index == Sstring_length(beispiel)) return;
          if (chareq(c,ascii('?')))
            { # wildcard_diff_ab() rekursiv aufrufen, mit erweitertem previous:
              c = TheSstring(beispiel)->data[b_index++];
              pushSTACK(muster); pushSTACK(beispiel);
              { var object new_string = allocate_string(1);
                TheSstring(new_string)->data[0] = c;
                pushSTACK(new_string);
              }
              { var object new_cons = allocate_cons();
                Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;
                STACK_0 = new_cons; # (CONS ... previous)
              }
              wildcard_diff_ab(STACK_2,STACK_1,m_index,b_index,&STACK_0,solutions);
              skipSTACK(3);
              return;
            }
            else
            { if (!equal_pathchar(TheSstring(beispiel)->data[b_index++],c))
                return;
            }
        }
     {var uintL b_start_index = b_index;
      loop
        { # Um weniger zu consen, die Fälle abfangen, wo wildcard_diff_ab()
          # gar nichts tut:
          if (m_index == Sstring_length(muster)
              ? b_index == Sstring_length(beispiel)
              : (c = TheSstring(muster)->data[m_index],
                 chareq(c,ascii('*')) || chareq(c,ascii('?'))
                 || (b_index < Sstring_length(beispiel)
                     && equal_pathchar(TheSstring(beispiel)->data[b_index],c)
             )  )   )
            # wildcard_diff_ab() rekursiv aufrufen, mit erweitertem previous:
            { pushSTACK(muster); pushSTACK(beispiel);
              pushSTACK(subsstring(beispiel,b_start_index,b_index)); # (SUBSTRING beispiel b_start_index b_index)
             {var object new_cons = allocate_cons();
              Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;
              STACK_0 = new_cons; # (CONS ... previous)
              wildcard_diff_ab(STACK_2,STACK_1,m_index,b_index,&STACK_0,solutions);
              skipSTACK(1);
              beispiel = popSTACK(); muster = popSTACK();
            }}
          if (b_index == Sstring_length(beispiel))
            break;
          b_index++;
        }
    }}

#endif

# UPs: Vergleicht jeweils eine Pathname-Komponente ("Beispiel") und
# eine Pathname-Komponente ("Muster").
# can trigger GC
  local void host_diff (object muster, object beispiel, boolean logical, const object* previous, object* solutions);
  local void device_diff (object muster, object beispiel, boolean logical, const object* previous, object* solutions);
  local void directory_diff (object muster, object beispiel, boolean logical, const object* previous, object* solutions);
  local void nametype_diff (object muster, object beispiel, boolean logical, const object* previous, object* solutions);
  local void version_diff (object muster, object beispiel, boolean logical, const object* previous, object* solutions);
  local void host_diff(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (nullp(muster))
            { push_solution_with(beispiel); return; }
          if (!equal(muster,beispiel)) return;
        }
        else
      #endif
        {
          #if HAS_HOST
          if (nullp(muster))
            { push_solution_with(beispiel); return; }
          if (!equal(muster,beispiel)) return;
          #endif
        }
      push_solution();
    }
  local void device_diff(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    {
      #if HAS_DEVICE
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { push_solution(); return; }
      #endif
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      if (nullp(muster) || eq(muster,S(Kwild)))
        { var object string =
            (eq(beispiel,S(Kwild)) ? O(wild_string) : beispiel);
          push_solution_with(string);
          return;
        }
      if (eq(beispiel,S(Kwild))) return;
      #endif
      #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      if (nullp(muster))
        { var object string =
            #if defined(PATHNAME_AMIGAOS)
              beispiel;
            #endif
            #if defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
              (eq(beispiel,S(Kwild)) ? O(wild_string) : beispiel);
            #endif
          push_solution_with(string);
          return;
        }
      if (!equalp(muster,beispiel)) return;
      #else
      if (!equal(muster,beispiel)) return;
      #endif
      #endif
      push_solution();
    }
  local void nametype_diff_aux (object muster, object beispiel, boolean logical, const object* previous, object* solutions);
  local void nametype_diff_aux(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (eq(muster,S(Kwild)))
            { var object string =
                (eq(beispiel,S(Kwild)) ? O(wild_string) : beispiel);
              push_solution_with(string);
              return;
            }
          if (eq(beispiel,S(Kwild))) return;
          if (nullp(muster))
            { if (nullp(beispiel))
                { push_solution(); }
              return;
            }
          if (nullp(beispiel))
            return;
          wildcard_diff(muster,beispiel,previous,solutions);
          return;
        }
      #endif
      #ifdef PATHNAME_EXT83
      if (eq(muster,S(Kwild)))
        { var object string =
            (eq(beispiel,S(Kwild)) ? O(wild_string) : beispiel);
          push_solution_with(string);
          return;
        }
      if (eq(beispiel,S(Kwild))) return;
      if (!equal(muster,beispiel)) return;
      push_solution();
      #endif
      #ifdef PATHNAME_NOEXT
      if (nullp(muster))
        { if (nullp(beispiel))
            { push_solution(); }
          return;
        }
      if (nullp(beispiel))
        return;
      wildcard_diff(muster,beispiel,previous,solutions);
      #endif
    }
  local void subdir_diff(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    { if (eq(muster,beispiel))
        { if (eq(beispiel,S(Kwild)))
            { push_solution_with(O(wild_string)); }
            else
            { push_solution(); }
          return;
        }
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (eq(muster,S(Kwild)))
            { var object string =
                (eq(beispiel,S(Kwild)) ? O(wild_string) : beispiel);
              push_solution_with(string);
              return;
            }
          if (eq(beispiel,S(Kwild))) return;
          if (!simple_string_p(muster) || !simple_string_p(beispiel)) return;
          wildcard_diff(muster,beispiel,previous,solutions);
          return;
        }
      #endif
      #ifdef PATHNAME_EXT83
      if (atomp(muster) || atomp(beispiel)) return;
      pushSTACK(NIL); pushSTACK(Cdr(muster)); pushSTACK(Cdr(beispiel));
      nametype_diff_aux(Car(muster),Car(beispiel),FALSE,previous,&STACK_2);
      while (mconsp(STACK_2))
        { pushSTACK(Car(STACK_2));
          nametype_diff_aux(STACK_(1+1),STACK_(0+1),FALSE,&STACK_0,solutions);
          skipSTACK(1);
          STACK_2 = Cdr(STACK_2);
        }
      skipSTACK(3);
      #endif
      #ifdef PATHNAME_NOEXT
      if (!simple_string_p(muster) || !simple_string_p(beispiel)) return;
      wildcard_diff(muster,beispiel,previous,solutions);
      #endif
    }
  # rekursive Implementation wegen Backtracking:
  local void directory_diff_ab (object m_list, object b_list, boolean logical, const object* previous, object* solutions);
  local void directory_diff_ab(m_list,b_list,logical,previous,solutions)
    var object m_list;
    var object b_list;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    { # Algorithmus analog zu wildcard_diff_ab.
      var object item;
      if (atomp(m_list))
        { if (atomp(b_list))
            { push_solution(); }
          return;
        }
      item = Car(m_list); m_list = Cdr(m_list);
      if (!eq(item,S(Kwild_inferiors)))
        { if (atomp(b_list)) return;
          pushSTACK(NIL); pushSTACK(m_list); pushSTACK(Cdr(b_list));
          subdir_diff(item,Car(b_list),logical,previous,&STACK_2);
          # directory_diff_ab() rekursiv aufrufen, mit erweitertem previous:
          while (mconsp(STACK_2))
            { pushSTACK(Car(STACK_2));
              directory_diff_ab(STACK_(1+1),STACK_(0+1),logical,&STACK_0,solutions);
              skipSTACK(1);
              STACK_2 = Cdr(STACK_2);
            }
          skipSTACK(3);
        }
        else
        { pushSTACK(b_list); # b_start_list := b_list
          loop
            { # Um weniger zu consen, die Fälle abfangen, wo directory_diff_ab()
              # gar nichts tut:
              if (atomp(m_list)
                  ? atomp(b_list)
                  : (eq(Car(m_list),S(Kwild_inferiors)) || !atomp(b_list))
                 )
                # directory_diff_ab() rekursiv aufrufen, mit erweitertem previous:
                { pushSTACK(m_list); pushSTACK(b_list);
                  pushSTACK(STACK_2); pushSTACK(b_list);
                  funcall(L(ldiff),2); # (LDIFF b_start_list b_list)
                  pushSTACK(S(Kdirectory)); pushSTACK(value1);
                 {var object new_piece = listof(2); # (:DIRECTORY subdir1 ... subdirn)
                  pushSTACK(new_piece);
                 }
                 {var object new_cons = allocate_cons();
                  Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;
                  STACK_0 = new_cons; # (CONS ... previous)
                  directory_diff_ab(STACK_2,STACK_1,logical,&STACK_0,solutions);
                  skipSTACK(1);
                  b_list = popSTACK(); m_list = popSTACK();
                }}
              if (atomp(b_list)) break;
              b_list = Cdr(b_list);
            }
          skipSTACK(1);
        }
    }
  local void directory_diff(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    { # muster mit O(directory_default) vergleichen:
      if (eq(Car(muster),S(Krelative)) && nullp(Cdr(muster)))
        { # Augment the solution with the beispiel list - starting
          # with :ABSOLUTE or :RELATIVE, it will not fit for "**".
          push_solution_with(beispiel);
          return;
        }
      # Startpoint vergleichen:
      if (!eq(Car(muster),Car(beispiel)))
        return;
      muster = Cdr(muster); beispiel = Cdr(beispiel);
      # subdirs vergleichen:
      directory_diff_ab(muster,beispiel,logical,previous,solutions);
    }
  local void nametype_diff(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    { if (nullp(muster))
        { var object string =
            (eq(beispiel,S(Kwild)) ? O(wild_string) : beispiel);
          push_solution_with(string);
          return;
        }
      nametype_diff_aux(muster,beispiel,logical,previous,solutions);
    }
  local void version_diff(muster,beispiel,logical,previous,solutions)
    var object muster;
    var object beispiel;
    var boolean logical;
    var const object* previous;
    var object* solutions;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (nullp(muster) || eq(muster,S(Kwild)))
            { var object string =
                (eq(beispiel,S(Kwild)) ? O(wild_string) :
                 integerp(beispiel) ? decimal_string(beispiel) : # (SYS::DECIMAL-STRING beispiel)
                 NIL
                );
              push_solution_with(string);
              return;
            }
          if (eq(beispiel,S(Kwild))) return;
          if (!eql(muster,beispiel)) return;
          push_solution();
          return;
        }
      #endif
      #if HAS_VERSION
      if (nullp(muster) || eq(muster,S(Kwild)))
        { var object string =
            (eq(beispiel,S(Kwild)) ? O(wild_string) :
             integerp(beispiel) ? decimal_string(beispiel) : # (SYS::DECIMAL-STRING beispiel)
             NIL;
            );
          push_solution_with(string);
          return;
        }
      if (eq(beispiel,S(Kwild))) return;
      if (!eql(muster,beispiel)) return;
      push_solution();
      #else
      push_solution_with(NIL);
      #endif
    }

  #undef push_solution_with
  #undef push_solution

# Jede Substitution ist eine Liste von Normal-Simple-Strings oder Listen.
# (Die Listen entstehen bei :WILD-INFERIORS in directory_diff().)
# Ein Normal-Simple-String passt nur auf '?' oder '*' oder :WILD,
# eine Liste passt nur auf :WILD-INFERIORS.

#ifdef LOGICAL_PATHNAMES

# Beim Einsetzen von Stücken normaler Pathnames in logische Pathnames:
# Umwandlung in Großbuchstaben.
# logical_case(string)
# > string: Normal-Simple-String oder Symbol/Zahl
# < ergebnis: umgewandelter Normal-Simple-String oder dasselbe Symbol/Zahl
# can trigger GC
  local object logical_case (object string);
# Dasselbe, rekursiv wie mit SUBST:
  local object subst_logical_case (object obj);
#if defined(PATHNAME_MSDOS)
  # sowieso schon alles Großbuchstaben
  #define logical_case(string)  string
  #define subst_logical_case(obj)  obj
#else
  local object logical_case(string)
    var object string;
    { if (!simple_string_p(string))
        return string;
      return string_upcase(string);
    }
  local object subst_logical_case(obj)
    var object obj;
    { if (atomp(obj))
        { return logical_case(obj); }
      check_STACK(); check_SP();
      pushSTACK(obj);
      # rekursiv für den CAR aufrufen:
      { var object new_car = subst_logical_case(Car(obj));
        pushSTACK(new_car);
      }
      # rekursiv für den CDR aufrufen:
      { var object new_cdr = subst_logical_case(Cdr(STACK_1));
        if (eq(new_cdr,Cdr(STACK_1)) && eq(STACK_0,Car(STACK_1)))
          { obj = STACK_1; skipSTACK(2); return obj; }
          else
          # (CONS new_car new_cdr)
          { STACK_1 = new_cdr;
           {var object new_cons = allocate_cons();
            Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
            return new_cons;
    } }   }}
#endif

# Beim Einsetzen von Stücken logischer Pathnames in normale Pathnames:
# Umwandlung in Großbuchstaben.
# customary_case(string)
# > string: Normal-Simple-String oder Symbol/Zahl
# < ergebnis: umgewandelter Normal-Simple-String oder dasselbe Symbol/Zahl
# can trigger GC
  local object customary_case (object string);
# Dasselbe, rekursiv wie mit SUBST:
  local object subst_customary_case (object obj);
#if defined(PATHNAME_MSDOS)
  # Betriebssystem mit Vorzug für Großbuchstaben
  #define customary_case(string)  string
  #define subst_customary_case(obj)  obj
#else
  local object customary_case(string)
    var object string;
    { if (!simple_string_p(string))
        return string;
      #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32) || defined(PATHNAME_RISCOS)
      # Betriebssystem mit Vorzug für Kleinbuchstaben
      return string_downcase(string);
      #endif
      #ifdef PATHNAME_AMIGAOS
      # Betriebssystem mit Vorzug für Capitalize
      string = copy_string(string);
      nstring_capitalize(&TheSstring(string)->data[0],Sstring_length(string));
      return string;
      #endif
    }
  local object subst_customary_case(obj)
    var object obj;
    { if (atomp(obj))
        { return customary_case(obj); }
      check_STACK(); check_SP();
      pushSTACK(obj);
      # rekursiv für den CAR aufrufen:
      { var object new_car = subst_customary_case(Car(obj));
        pushSTACK(new_car);
      }
      # rekursiv für den CDR aufrufen:
      { var object new_cdr = subst_customary_case(Cdr(STACK_1));
        if (eq(new_cdr,Cdr(STACK_1)) && eq(STACK_0,Car(STACK_1)))
          { obj = STACK_1; skipSTACK(2); return obj; }
          else
          # (CONS new_car new_cdr)
          { STACK_1 = new_cdr;
           {var object new_cons = allocate_cons();
            Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
            return new_cons;
    } }   }}
#endif

#endif

# UP: Eine Substitution auf ein Muster anwenden.
# translate_pathname(&subst,muster)
  local object translate_pathname (object* subst, object muster);
# translate_host(&subst,muster,logical) etc. liefert den host etc. mit Ersetzungen
# und verkürzen subst passend. Falls nicht passend, liefert es nullobj.
  local object translate_host (object* subst, object muster, boolean logical);
  local object translate_device (object* subst, object muster, boolean logical);
  local object translate_subdir (object* subst, object muster, boolean logical);
  local object translate_directory (object* subst, object muster, boolean logical);
  local object translate_nametype (object* subst, object muster, boolean logical);
  local object translate_version (object* subst, object muster, boolean logical);
  local object translate_host(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if (nullp(muster) && mconsp(*subst))
            { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
                { var object erg = Car(*subst); *subst = Cdr(*subst);
                  return erg;
                }
                else
                return nullobj;
            }
        }
        else
      #endif
        {
          #if HAS_HOST
          if (nullp(muster) && mconsp(*subst))
            { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
                { var object erg = Car(*subst); *subst = Cdr(*subst);
                  return erg;
                }
                else
                return nullobj;
            }
          #endif
        }
      return muster;
    }
  local object translate_device(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    {
      #if HAS_DEVICE
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { return muster; }
      #endif
      #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      if (nullp(muster) && mconsp(*subst))
      #else
      if ((nullp(muster) || eq(muster,S(Kwild))) && mconsp(*subst))
      #endif
        { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
            { var object erg = Car(*subst); *subst = Cdr(*subst);
              return erg;
            }
            else
            return nullobj;
        }
      #endif
      return muster;
    }
  local object translate_nametype_aux (object* subst, object muster, boolean logical);
  local object translate_nametype_aux(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    { if (eq(muster,S(Kwild)) && mconsp(*subst))
        { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
            { var object erg = Car(*subst); *subst = Cdr(*subst);
              return (nullp(erg) ? O(leer_string) : erg);
            }
            else
            return nullobj;
        }
      if (simple_string_p(muster))
        { pushSTACK(muster); # muster retten
         {var object* muster_ = &STACK_0;
          var uintL len = Sstring_length(muster);
          var uintL index = 0;
          var uintL stringcount = 0; # Anzahl der Strings auf dem Stack
          loop
            { var uintL last_index = index;
              var chart c;
              # Suche nächstes Wildcard-Zeichen:
              muster = *muster_;
              loop
                { if (index == len) break;
                  c = TheSstring(muster)->data[index];
                  if ((chareq(c,ascii('*')) # Wildcard für beliebig viele Zeichen
                       || (!logical && singlewild_char_p(c)) # Wildcard für genau ein Zeichen
                      )
                      && mconsp(*subst)
                     )
                    break;
                  index++;
                }
              # Nächsten Teilstring auf den Stack:
              pushSTACK(subsstring(muster,last_index,index)); # (SUBSTRING muster last_index index)
              stringcount++;
              # Fertig?
              if (index == len) break;
              # Wildcard ersetzen:
              if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
                { var object s = Car(*subst);
                  pushSTACK(nullp(s) ? O(leer_string) : s);
                  *subst = Cdr(*subst); stringcount++;
                }
                else
                { skipSTACK(stringcount+1); return nullobj; }
              index++;
            }
          funcall(L(string_concat),stringcount); # (STRING-CONCAT alle Strings)
          skipSTACK(1);
          return value1;
        }}
      return muster;
    }
  local object translate_subdir(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { return translate_nametype_aux(subst,muster,logical); }
      #endif
      #ifdef PATHNAME_EXT83
      if (atomp(muster)) return muster;
      pushSTACK(Car(muster)); pushSTACK(Cdr(muster));
      if (eq(STACK_1 = translate_nametype_aux(subst,STACK_1,FALSE),nullobj)
          || eq(STACK_0 = translate_nametype_aux(subst,STACK_0,FALSE),nullobj)
         )
        { skipSTACK(2); return nullobj; }
      {var object new_cons = allocate_cons();
       Car(new_cons) = STACK_1; Cdr(new_cons) = STACK_0; skipSTACK(2);
       return new_cons;
      }
      #endif
      #ifdef PATHNAME_NOEXT
      return translate_nametype_aux(subst,muster,FALSE);
      #endif
    }
  local object translate_directory(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    { # muster mit O(directory_default) vergleichen:
      if (eq(Car(muster),S(Krelative)) && nullp(Cdr(muster)) && mconsp(*subst))
        { var object list = Car(*subst); *subst = Cdr(*subst);
          if (consp(list)
              && (eq(Car(list),S(Kabsolute)) || eq(Car(list),S(Krelative)))
             )
            { return copy_list(list); }
            else
            return nullobj;
        }
     {var uintL itemcount = 0; # Anzahl der Elemente auf dem Stack
      # Startpoint:
      pushSTACK(Car(muster)); muster = Cdr(muster); itemcount++;
      # subdirs:
      while (consp(muster))
        { var object item = Car(muster);
          muster = Cdr(muster);
          if (eq(item,S(Kwild_inferiors)))
            { if (mconsp(*subst))
                { if (consp(Car(*subst)) && eq(Car(Car(*subst)),S(Kdirectory)))
                    { var object list = Cdr(Car(*subst)); *subst = Cdr(*subst);
                      while (consp(list))
                        { pushSTACK(Car(list)); list = Cdr(list); itemcount++; }
                    }
                    else
                    { skipSTACK(itemcount); return nullobj; }
                }
                else
                { pushSTACK(item); itemcount++; }
            }
            else
            { pushSTACK(muster); # muster retten
              item = translate_subdir(subst,item,logical);
              if (eq(item,nullobj)) { skipSTACK(itemcount+1); return nullobj; }
              muster = STACK_0; STACK_0 = item; itemcount++;
            }
        }
      return listof(itemcount);
    }}
  local object translate_nametype(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    { if (nullp(muster) && mconsp(*subst))
        { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
            { var object erg = Car(*subst); *subst = Cdr(*subst);
              return erg;
            }
            else
            return nullobj;
        }
      return translate_nametype_aux(subst,muster,logical);
    }
  local object translate_version(subst,muster,logical)
    var object* subst;
    var object muster;
    var boolean logical;
    {
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        { if ((nullp(muster) || eq(muster,S(Kwild))) && mconsp(*subst))
            { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
                { var object erg = Car(*subst); *subst = Cdr(*subst);
                  if (nullp(erg)) { return erg; }
                  pushSTACK(erg); funcall(L(parse_integer),1);
                  return value1;
                }
                else
                return nullobj;
            }
          return muster;
        }
      #endif
      #if HAS_VERSION
      if ((nullp(muster) || eq(muster,S(Kwild))) && mconsp(*subst))
        { if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
            { var object erg = Car(*subst); *subst = Cdr(*subst);
              if (nullp(erg)) { return erg; }
              pushSTACK(erg); funcall(L(parse_integer),1);
              return value1;
            }
            else
            return nullobj;
        }
      return muster;
      #else
      if (simple_string_p(Car(*subst)) || nullp(Car(*subst)))
        { *subst = Cdr(*subst); return NIL; }
        else
        return nullobj;
      #endif
    }
  local object translate_pathname(subst,muster)
    var object* subst;
    var object muster;
    { var boolean logical = FALSE;
      var object item;
      pushSTACK(*subst); # subst retten für Fehlermeldung
      pushSTACK(muster);
      #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(muster)) { logical = TRUE; }
      #endif
      # Argumente für MAKE-PATHNAME zusammenbauen:
      item = translate_host(subst,xpathname_host(logical,muster),logical);
      if (eq(item,nullobj)) { goto subst_error; }
      pushSTACK(S(Khost)); pushSTACK(item);
      #if HAS_DEVICE
      item = translate_device(subst,xpathname_device(logical,STACK_2),logical);
      if (eq(item,nullobj)) { skipSTACK(2); goto subst_error; }
      pushSTACK(S(Kdevice)); pushSTACK(item);
      #endif
      item = translate_directory(subst,xpathname_directory(logical,STACK_(2+2*HAS_DEVICE)),logical);
      if (eq(item,nullobj)) { skipSTACK(2+2*HAS_DEVICE); goto subst_error; }
      pushSTACK(S(Kdirectory)); pushSTACK(item);
      item = translate_nametype(subst,xpathname_name(logical,STACK_(2+2*HAS_DEVICE+2)),logical);
      if (eq(item,nullobj)) { skipSTACK(2+2*HAS_DEVICE+2); goto subst_error; }
      pushSTACK(S(Kname)); pushSTACK(item);
      item = translate_nametype(subst,xpathname_type(logical,STACK_(2+2*HAS_DEVICE+4)),logical);
      if (eq(item,nullobj)) { skipSTACK(2+2*HAS_DEVICE+4); goto subst_error; }
      pushSTACK(S(Ktype)); pushSTACK(item);
      item = translate_version(subst,xpathname_version(logical,STACK_(2+2*HAS_DEVICE+6)),logical);
      if (eq(item,nullobj)) { skipSTACK(2+2*HAS_DEVICE+6); goto subst_error; }
      pushSTACK(S(Kversion)); pushSTACK(item);
      # Alle Ersetzungsstücke müssen verbraucht werden!
      if (mconsp(*subst)) { skipSTACK(2+2*HAS_DEVICE+8); goto subst_error; }
      # (MAKE-PATHNAME ...) bzw. (SYS::MAKE-LOGICAL-PATHNAME ...) aufrufen:
      #ifdef LOGICAL_PATHNAMES
      if (logical)
        funcall(L(make_logical_pathname),2+2*HAS_DEVICE+8);
        else
      #endif
        funcall(L(make_pathname),2+2*HAS_DEVICE+8);
      skipSTACK(2);
      return value1;
     subst_error: # Error wegen nullobj.
      # Stackaufbau: subst, muster.
      pushSTACK(STACK_1);
      pushSTACK(S(translate_pathname));
      fehler(error,
             GETTEXT("~: replacement pieces ~ do not fit into ~")
            );
    }

LISPFUN(translate_pathname,3,0,norest,key,2, (kw(all),kw(merge)))
# (TRANSLATE-PATHNAME beispiel muster1 muster2 [:all] [:merge]), CLtL2 S. 624
# :all = T --> liefere eine Liste aller passenden Pathnames
# :all = NIL --> Error, falls mehr als ein Pathname passt
# :merge = NIL --> letzten MERGE-PATHNAMES Schritt überspringen
  { # Stackaufbau: beispiel, muster1, muster2, all, merge.
    var boolean logical = FALSE; # Flag, ob beispiel und muster logische Pathnames sind
    var boolean logical2 = FALSE; # Flag, ob muster2 ein logischer Pathname ist
    STACK_4 = coerce_xpathname(STACK_4);
    STACK_3 = coerce_xpathname(STACK_3);
    STACK_2 = coerce_xpathname(STACK_2);
    #ifdef LOGICAL_PATHNAMES
    if (logpathnamep(STACK_4) && logpathnamep(STACK_3))
      { logical = TRUE; }
      else
      # nicht beides logische Pathnames -> erst in normale Pathnames umwandeln:
      { STACK_4 = coerce_pathname(STACK_4);
        STACK_3 = coerce_pathname(STACK_3);
      }
    if (logpathnamep(STACK_2))
      { logical2 = TRUE; }
    #endif
    # 1. Schritt: Liste aller passenden Substitutionen bilden.
    pushSTACK(NIL); pushSTACK(NIL);
    host_diff(xpathname_host(logical,STACK_(3+2)),xpathname_host(logical,STACK_(4+2)),logical,&STACK_1,&STACK_0);
    while (mconsp(STACK_0))
      { pushSTACK(Car(STACK_0)); pushSTACK(NIL);
        device_diff(xpathname_device(logical,STACK_(3+4)),xpathname_device(logical,STACK_(4+4)),logical,&STACK_1,&STACK_0);
        while (mconsp(STACK_0))
          { pushSTACK(Car(STACK_0)); pushSTACK(NIL);
            directory_diff(xpathname_directory(logical,STACK_(3+6)),xpathname_directory(logical,STACK_(4+6)),logical,&STACK_1,&STACK_0);
            while (mconsp(STACK_0))
              { pushSTACK(Car(STACK_0)); pushSTACK(NIL);
                nametype_diff(xpathname_name(logical,STACK_(3+8)),xpathname_name(logical,STACK_(4+8)),logical,&STACK_1,&STACK_0);
                while (mconsp(STACK_0))
                  { pushSTACK(Car(STACK_0)); pushSTACK(NIL);
                    nametype_diff(xpathname_type(logical,STACK_(3+10)),xpathname_type(logical,STACK_(4+10)),logical,&STACK_1,&STACK_0);
                    while (mconsp(STACK_0))
                      { pushSTACK(Car(STACK_0));
                        version_diff(xpathname_version(logical,STACK_(3+11)),xpathname_version(logical,STACK_(4+11)),logical,&STACK_0,&STACK_10);
                        skipSTACK(1);
                        STACK_0 = Cdr(STACK_0);
                      }
                    skipSTACK(2);
                    STACK_0 = Cdr(STACK_0);
                  }
                skipSTACK(2);
                STACK_0 = Cdr(STACK_0);
              }
            skipSTACK(2);
            STACK_0 = Cdr(STACK_0);
          }
        skipSTACK(2);
        STACK_0 = Cdr(STACK_0);
      }
    skipSTACK(1);
    # Stackaufbau: ..., solutions.
    if (matomp(STACK_0))
      { pushSTACK(STACK_(3+1));
        pushSTACK(STACK_(4+1+1));
        pushSTACK(S(translate_pathname));
        fehler(error,
               GETTEXT("~: ~ is not a specialization of ~")
              );
      }
    # 2.,3. Schritt:
    pushSTACK(NIL); # pathnames := '()
    while (mconsp(STACK_1)) # solutions durchgehen
      { var object solutions = STACK_1;
        STACK_1 = Cdr(solutions);
       {var object solution = reverse(Car(solutions)); # Liste solution umdrehen
        # 2. Schritt: Substitution in muster2 einfügen.
        #ifdef LOGICAL_PATHNAMES
        # Groß-/Kleinschreibung passend konvertieren:
        if (!logical)
          { if (logical2)
              { solution = subst_logical_case(solution); }
          }
          else
          { if (!logical2)
              { solution = subst_customary_case(solution); }
          }
        #endif
        pushSTACK(solution);
        STACK_0 = translate_pathname(&STACK_0,STACK_(2+1+2));
       }
        # 3. Schritt: (MERGE-PATHNAMES modifiziertes_muster2 beispiel :WILD T)
        if (!nullp(STACK_(0+1+2))) # :MERGE-Argument abfragen
          if (has_some_wildcards(STACK_0)) # evtl. ist MERGE-PATHNAMES unnötig
            { pushSTACK(STACK_(4+1+2)); pushSTACK(unbound);
              pushSTACK(S(Kwild)); pushSTACK(T);
              funcall(L(merge_pathnames),5);
              pushSTACK(value1);
            }
        # (PUSH pathname pathnames)
       {var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
        STACK_0 = new_cons;
      }}
    # 4. Schritt: (DELETE-DUPLICATES pathnames :TEST #'EQUAL)
    pushSTACK(S(Ktest)); pushSTACK(L(equal));
    funcall(L(delete_duplicates),3);
    # Stackaufbau: ..., nil.
    if (eq(STACK_(1+1),unbound) || nullp(STACK_(1+1))) # :ALL-Argument abfragen
      { if (mconsp(Cdr(value1)))
          { pushSTACK(value1);
            pushSTACK(STACK_(2+2));
            pushSTACK(STACK_(3+3));
            pushSTACK(STACK_(4+4));
            pushSTACK(S(translate_pathname));
            fehler(error,
                   GETTEXT("(~ ~ ~ ~) is ambiguous: ~")
                  );
          }
        value1 = Car(value1);
      }
    mv_count=1;
    skipSTACK(5+1);
  }

# UP: Stellt fest, ob der Name eines Pathname =NIL ist.
# namenullp(pathname)
# > pathname: nicht-Logical Pathname
  # local boolean namenullp (object pathname);
  # local boolean namenullp(pathname)
  #   { return nullp(ThePathname(pathname)->pathname_name); }
  #define namenullp(path)  (nullp(ThePathname(path)->pathname_name))

# Fehler, wenn ein Directory nicht existiert
# > obj: Pathname oder (besser) fehlerhafte Komponente
  nonreturning_function(local, fehler_dir_not_exists, (object obj));
  local void fehler_dir_not_exists(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(obj);
      fehler(file_error,
             GETTEXT("nonexistent directory: ~")
            );
    }

# Fehler, wenn eine Datei bereits existiert
# > caller: Aufrufer (ein Symbol)
# > pathname: Pathname
  nonreturning_function(local, fehler_file_exists, (object caller, object pathname));
  local void fehler_file_exists(caller,pathname)
    var object caller;
    var object pathname;
    { pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(pathname);
      pushSTACK(caller);
      fehler(file_error,
             GETTEXT("~: File ~ already exists")
            );
    }

#ifdef LOGICAL_PATHNAMES
# Ein "absoluter Pathname" ist stets ein nicht-Logical Pathname, evtl.
# mit weiteren Einschränkungen.
#endif

#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)

# Ein "absoluter Pathname" ist ein Pathname, bei dem Device ein überprüfter
# String ist und Directory kein :RELATIVE, :CURRENT, :PARENT enthält.

# UP: Liefert den Namestring eines Pathname fürs Betriebssystem.
# OSnamestring(dir_namestring)
# > STACK_0: nicht-Logical Pathname
# > dir_namestring: Directory-Namestring (für DOS)
# < ergebnis: Namestring (für DOS)
# can trigger GC
  local object OSnamestring (object dir_namestring);
  local object OSnamestring(dir_namestring)
    var object dir_namestring;
    { var uintC stringcount;
      pushSTACK(dir_namestring); # Directory-Namestring als 1. String
      stringcount = file_namestring_parts(STACK_(0+1)); # Strings zum Filenamen
      return string_concat(1+stringcount); # zusammenhängen
    }

#if !defined(WIN32_NATIVE)

# Working Directory auf einem gegebenen Drive abfragen:
# getwdof(&buf,drive)
# > uintB* &buf: Adresse eines Path-Buffers
# > uintB drive: Laufwerk (0=A, 1=B, ...)
# < ergebnis: <0 falls Fehler
  #ifdef DJUNIX
    #define getwdof(buf,drive)  \
      ({__asm__ (# DOS Function 47H                                                         \
                 " movb $0x47,%%ah ; int $0x21 "                                            \
                 :                                                                # OUT     \
                 : "S" /* %esi */ ((uintB*)(buf)), "d" /* %dl */ ((uintB)(drive)) # IN      \
                 : "ax","bx","cx","di" /* %eax, %ebx, %ecx, %edi */               # CLOBBER \
                );                                                                          \
        0;                                                                                  \
       })
  #endif
  #ifdef EMUNIX
    #define getwdof(buf,drive)  _getcwd(buf,drive)
  #endif
  #ifdef WATCOM
    local int getwdof (uintB* buf, uintB drive);
    local int getwdof(buf,drive)
      var uintB* buf;
      var uintB drive;
      { var union REGS in;
        var union REGS out;
        in.regB.ah = 0x47; in.regB.dl = drive; in.regL.esi = (unsigned long) buf;
        intdos(&in,&out);
        return 0;
      }
  #endif

# Liefert das aktuelle Directory auf einem Laufwerk.
# getwd_of(path,drive)
# > uintB drive: Laufwerks-(groß-)buchstabe
# > uintB* path: Platz fürs aktuelle Directory
# < path: Pfad des aktuellen Directories, mit '/' als Trennzeichen und als Anfang
# < ergebnis: <0 falls Fehler
  #if defined(DJUNIX) || defined(WATCOM)
    #define getwd_of(path,drive)  ((path)[0] = '/', getwdof(&(path)[1],(drive)-'A'+1))
  #endif
  #ifdef EMUNIX
    #define getwd_of(path,drive)  _getcwd1(path,drive)
  #endif

#endif # !WIN32_NATIVE

# UP: Stellt fest, ob ein Laufwerk existiert.
# > uintB drive: Laufwerks-(groß-)buchstabe
# < boolean ergebnis: ob dieses Laufwerk existiert und ansprechbar ist
  local boolean good_drive (uintB drive);
  #ifdef EMUNIX
  local boolean good_drive(drive)
    var uintB drive;
    { # Methode (siehe HELPPC/misc.txt):
      # 1. save current drive  (INT 0x21,0x19)
      # 2. set current drive  (INT 0x21,0xE)
      # 3. get current drive  (INT 0x21,0x19)
      # 4. if current drive == drive requested
      #       then drive exists
      #       else drive doesn't exist
      # 5. reset original drive  (INT 0x21,0xE)
      var boolean result;
      begin_system_call();
     {var uintB orig_drive = _getdrive();
      _chdrive(drive);
      result = (_getdrive() == drive);
      _chdrive(orig_drive);
     }
      end_system_call();
      return result;
      # Alternative:
      # { var uintB drv[3];
      #   var uintB fsys[16];
      #   drv[0] = drive; drv[1] = ':'; drv[2] = '\0';
      #   begin_system_call();
      #  {var int result = _filesys(drv,&fsys,sizeof(fsys));
      #   end_system_call();
      #   return (result==0);
      # }}
    }
  #endif
  #if defined(DJUNIX) || defined(WATCOM)
  local boolean good_drive(drive)
    var uintB drive;
    { # Methode (siehe HELPPC/misc.txt):
      # 1. save current drive  (INT 0x21,0x19)
      # 2. set current drive  (INT 0x21,0xE)
      # 3. get current drive  (INT 0x21,0x19)
      # 4. if current drive == drive requested
      #       then drive exists
      #       else drive doesn't exist
      # 5. reset original drive  (INT 0x21,0xE)
      var union REGS in;
      var union REGS out;
      var uintB orig_drive;
      var boolean result;
      begin_system_call();
      in.regB.ah = 0x19; intdos(&in,&out); orig_drive = out.regB.al; # 1.
      in.regB.ah = 0x0E; in.regB.dl = drive; intdos(&in,&out);       # 2.
      in.regB.ah = 0x19; intdos(&in,&out);                           # 3.
      result = (out.regB.al == drive);                               # 4.
      in.regB.ah = 0x0E; in.regB.dl = orig_drive; intdos(&in,&out);  # 5.
      end_system_call();
      return result;
    }
  #endif
  #ifdef WIN32_NATIVE
  local boolean good_drive(drive)
    var uintB drive;
    { var char rootpath[4];
      var DWORD result;
      rootpath[0] = drive;
      rootpath[1] = ':';
      rootpath[2] = '\\';
      rootpath[3] = '\0';
      begin_system_call();
      result = GetDriveType(rootpath);
      switch (result)
        { case DRIVE_UNKNOWN:
            end_system_call();
            return FALSE;
          case DRIVE_NO_ROOT_DIR:
            # Distinguish NFS mounts from nonassigned drive letters:
            result = GetFileAttributes(rootpath);
            end_system_call();
            return !(result==0xFFFFFFFF);
          default:
            end_system_call();
            return TRUE;
        }
    }
  #if 0
  # The following fails to recognize some (but not all) NFS mounts on WinNT.
  local boolean good_drive_notsogood(drive)
    var uintB drive;
    { var DWORD drives_bitmask;
      begin_system_call();
      drives_bitmask = GetLogicalDrives();
      end_system_call();
      return ((drives_bitmask & ((DWORD)1 << (drive-'A'))) != 0);
    }
  #endif
  #endif

# UP: Liefert das aktuelle Drive.
# < char drive: Laufwerks-(groß-)buchstabe
  local char default_drive (void);
 #ifdef EMUNIX
  local char default_drive()
    { var uintB result;
      begin_system_call();
      result = _getdrive();
      end_system_call();
      return result;
    }
 #endif
 #if defined(DJUNIX) || defined(WATCOM)
  #if 1
    local char default_drive()
      { var union REGS in;
        var union REGS out;
        begin_system_call();
        in.regB.ah = 0x19;
        intdos(&in,&out);
        end_system_call();
        return 'A'+out.regB.al;
      }
  #else # nur defined(WATCOM)
    local char default_drive()
      { var unsigned int drive;
        begin_system_call();
        _dos_getdrive(&drive);
        end_system_call();
        return 'A'+drive-1;
      }
  #endif
 #endif
 #ifdef WIN32_NATIVE
  local char default_drive()
    { var DWORD path_buflen = _MAX_PATH;
      var char* path_buffer = (char*)alloca(path_buflen);
      var DWORD result;
      begin_system_call();
      result = GetCurrentDirectory(path_buflen,path_buffer);
      if (!result) { OS_error(); }
      if (result >= path_buflen)
        { path_buflen = result; path_buffer = (char*)alloca(path_buflen);
          result = GetCurrentDirectory(path_buflen,path_buffer);
          if (!result) { OS_error(); }
        }
      end_system_call();
      ASSERT(path_buffer[1]==':');
      ASSERT(path_buffer[2]=='\\');
      return up_case(path_buffer[0]);
    }
 #endif

# UP: Liefert das aktuelle Directory auf einem gegebenen Drive.
# > uintB drive: Laufwerks-(groß-)buchstabe
# > object pathname: Pathname (für Fehlermeldungszwecke)
# < ergebnis: aktuelles Directory (als Pathname)
# can trigger GC
  local object default_directory_of (uintB drive, object pathname);
  local object default_directory_of(drive,pathname)
    var uintB drive;
    var object pathname;
    # Working Directory (von DOS) ist das aktuelle Directory:
    {
      #if defined(WIN32_NATIVE)
        var char currpath[4];
        var DWORD path_buflen = _MAX_PATH;
        var char* path_buffer = (char*)alloca(path_buflen+1);
        var char* dummy;
        var DWORD result;
        currpath[0] = drive;
        currpath[1] = ':';
        currpath[2] = '.'; # this dot is actually not needed
        currpath[3] = '\0';
        begin_system_call();
        result = GetFullPathName(currpath,path_buflen,path_buffer,&dummy);
        if (!result) { end_system_call(); OS_file_error(pathname); }
        if (result >= path_buflen)
          { path_buflen = result; path_buffer = (char*)alloca(path_buflen+1);
            result = GetFullPathName(currpath,path_buflen,path_buffer,&dummy);
            if (!result) { end_system_call(); OS_file_error(pathname); }
          }
        end_system_call();
        # evtl. noch ein '\' am Schluss anfügen:
        { var char* path_end = &path_buffer[asciz_length(path_buffer)];
          if (!(path_end[-1]=='\\')) { path_end[0] = '\\'; path_end[1] = '\0'; }
        }
      #else
        var char path_buffer[3+MAXPATHLEN]; # vgl. GETWD(3)
        path_buffer[0] = drive; path_buffer[1] = ':';
        # Working Directory in path_buffer ablegen:
        begin_system_call();
        getwd_of(&path_buffer[2],drive);
        end_system_call();
      #endif
      # Hack von DJ (siehe GO32/EXPHDLR.C) und EM (siehe LIB/MISC/_GETCWD1.C):
      # wandelt alle '\' in '/' und alle Groß- in Kleinbuchstaben (nur Kosmetik,
      # da DOS und unser PARSE-NAMESTRING auch Filenamen mit '/' statt '\'
      # verstehen).
      # in Pathname umwandeln:
      return asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding));
    }

# UP: Füllt Default-Drive und Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: nicht-Logical Pathname mit Device /= :WILD
# < ergebnis: neuer absoluter Pathname
# can trigger GC
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var object pathname;
    { # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
      pushSTACK(pathname);
      # Stackaufbau: pathname.
      # Default fürs Device:
      if (nullp(ThePathname(pathname)->pathname_device)) # kein Device angegeben?
        # Nimm das Default-Drive stattdessen:
        { ThePathname(pathname)->pathname_device = O(default_drive); }
      # Default fürs Directory:
        { var object subdirs = ThePathname(pathname)->pathname_directory;
          # Fängt pathname-directory mit :RELATIVE an?
          if (eq(Car(subdirs),S(Krelative)))
            # ja -> Ersetze :RELATIVE durch das Default-Directory:
            { pushSTACK(Cdr(subdirs));
             {var uintB drive = as_cint(TheSstring(ThePathname(pathname)->pathname_device)->data[0]);
              var object default_dir = default_directory_of(drive,pathname);
              # default_dir (ein Pathname) ist fertig.
              # Ersetze :RELATIVE durch default-subdirs, d.h.
              # bilde  (append default-subdirs (cdr subdirs))
              #      = (nreconc (reverse default-subdirs) (cdr subdirs))
              var object temp = ThePathname(default_dir)->pathname_directory;
              temp = reverse(temp);
              subdirs = nreconc(temp,popSTACK());
            }}
          # Liste durchgehen und dabei neu aufconsen, dabei '.\' und '..\'
          # und '...\' verarbeiten (nicht dem DOS überlassen):
          pushSTACK(subdirs);
          pushSTACK(NIL);
          # Stackaufbau: Pathname, subdir-oldlist, subdir-newlist.
          while (mconsp(STACK_1)) # Bis oldlist am Ende ist:
            { var object subdir = Car(STACK_1); # nächstes subdir
              if
                 #if defined(PATHNAME_MSDOS)
                 (eq(subdir,S(Kcurrent)))
                 #else
                 (equal(subdir,O(punkt_string)))
                 #endif
                # = :CURRENT -> newlist unverändert lassen
                {}
              elif
                   #if defined(PATHNAME_MSDOS)
                   (eq(subdir,S(Kparent)))
                   #else
                   (equal(subdir,O(punktpunkt_string)))
                   #endif
                # = :PARENT -> newlist um eins verkürzen:
                { if (matomp(Cdr(STACK_0))) # newlist (bis auf :ABSOLUTE) leer ?
                    { # :PARENT von "\" aus liefert Error
                      pushSTACK(STACK_2); # Wert für Slot PATHNAME von FILE-ERROR
                      pushSTACK(O(backslash_string)); # "\\"
                      pushSTACK(directory_namestring(STACK_(2+2))); # Directory von pathname
                      fehler(file_error,
                             GETTEXT("no directory ~ above ~")
                            );
                    }
                  if (eq(Car(STACK_0),S(Kwild_inferiors))) # newlist fängt mit '...\' an ?
                    { # :PARENT von "...\" aus liefert Error
                      pushSTACK(STACK_2); # Wert für Slot PATHNAME von FILE-ERROR
                      pushSTACK(directory_namestring(STACK_(2+1))); # Directory von pathname
                      fehler(file_error, # '"..\\" nach "...\\" ist unzulässig: ~'
                             GETTEXT("\"..\\\\\" after \"...\\\\\" is invalid: ~")
                            );
                    }
                  STACK_0 = Cdr(STACK_0);
                }
              else # (auch wenn :ABSOLUTE !)
                { # newlist um eins verlängern:
                  pushSTACK(subdir);
                 {var object new_cons = allocate_cons();
                  Car(new_cons) = popSTACK();
                  Cdr(new_cons) = STACK_0;
                  STACK_0 = new_cons;
                }}
              STACK_1 = Cdr(STACK_1);
            }
          subdirs = nreverse(popSTACK()); # newlist, wieder umdrehen
          skipSTACK(1);
          # Stackaufbau: pathname.
          pathname = popSTACK();
          ThePathname(pathname)->pathname_directory = subdirs; # in den Pathname eintragen
        }
      return pathname;
    }

# UP: Stellt sicher, dass das Directory eines Pathname existiert.
# Sonst Fehlermeldung.
# assure_dir_exists(links_resolved,tolerantp)
# > STACK_0: absoluter Pathname ohne Wildcards im Directory
# > links_resolved: Flag, ob im Directory des Pathname schon alle Links
#     aufgelöst sind und es als existierend bekannt ist
# > tolerantp: Flag, ob ein Fehler vermieden werden soll
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für DOS)
#     falls Name/=NIL: Namestring (für DOS)
#     falls tolerantp evtl.: nullobj
# can trigger GC
  local object assure_dir_exists (boolean links_resolved, boolean tolerantp);
  local object assure_dir_exists(links_resolved,tolerantp)
    var boolean links_resolved;
    var boolean tolerantp;
    { var uintC stringcount = directory_namestring_parts(STACK_0); # Strings fürs Directory
      var object dir_namestring = string_concat(stringcount); # zusammenhängen
      if (!links_resolved)
        { # Existenztest:
          #ifdef MSDOS
            # 1. Subdir-List leer -> OK
            #    (Muss abgefangen werden, denn stat() auf Rootdir liefert Fehler.)
            # 2. OS/2: Subdir-List = ("PIPE") -> OK
            #    (Dieses Spezialverzeichnis "\\PIPE\\" ist in Wirklichkeit keines.)
            # 3. Sonst stat() probieren.
            if (!(nullp(Cdr(ThePathname(STACK_0)->pathname_directory))
                  #ifdef PATHNAME_OS2
                  || equal(Cdr(ThePathname(STACK_0)->pathname_directory),O(pipe_subdirs))
                  #endif
               ) )
              { var struct stat statbuf;
                with_sstring(dir_namestring,O(pathname_encoding),path,len,
                  { ASSERT((len > 0) && (path[len-1] == '\\'));
                    path[len-1] = '\0'; # '\' am Schluss durch Nullbyte ersetzen
                    begin_system_call();
                    if (stat(path,&statbuf) < 0)
                      { if (!(tolerantp && (errno==ENOENT)))
                          { end_system_call(); OS_file_error(STACK_0); }
                        end_system_call();
                        FREE_DYNAMIC_ARRAY(path);
                        return nullobj;
                      }
                    end_system_call();
                  });
                if (!S_ISDIR(statbuf.st_mode)) # gefundene Datei kein Unterdirectory ?
                  { if (tolerantp) { return nullobj; }
                    fehler_dir_not_exists(dir_namestring);
              }   }
          #endif
          #ifdef WIN32_NATIVE
            var DWORD fileattr;
            with_sstring_0(dir_namestring,O(pathname_encoding),path,
              { if (!nullp(Cdr(ThePathname(STACK_0)->pathname_directory)))
                  { var uintL len = Sstring_length(dir_namestring);
                    ASSERT((len > 0) && (path[len-1] == '\\'));
                    path[len-1] = '\0'; # '\' am Schluss durch Nullbyte ersetzen
                  }
                begin_system_call();
                fileattr = GetFileAttributes(path);
                if (fileattr == 0xFFFFFFFF)
                  { if (!(tolerantp && (GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND)))
                      { end_system_call(); OS_file_error(STACK_0); }
                    end_system_call();
                    FREE_DYNAMIC_ARRAY(path);
                    return nullobj;
                  }
                end_system_call();
              });
            if (!(fileattr & FILE_ATTRIBUTE_DIRECTORY)) # gefundene Datei kein Unterdirectory ?
              { if (tolerantp) { return nullobj; }
                fehler_dir_not_exists(dir_namestring);
              }
          #endif
        }
      if (namenullp(STACK_0))
        { return dir_namestring; }
        else
        { return OSnamestring(dir_namestring); }
    }

# UP: Liefert den Directory-Namestring eines Pathname unter der Annahme,
#     dass das Directory dieses Pathname existiert.
# assume_dir_exists()
# > STACK_0: absoluter Pathname ohne Wildcards im Directory
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für DOS)
#     falls Name/=NIL: Namestring (für DOS)
# can trigger GC
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { return assure_dir_exists(TRUE,FALSE); }

#endif

#ifdef PATHNAME_AMIGAOS

# UP: Liefert den Truename eines Directory-Locks.
# > set_break_sem_4(): schon ausgeführt
# > lock: Directory-Lock, wird freigegeben
# < ergebnis: Directory (als Pathname)
# can trigger GC
  local object directory_truename (BPTR lock);
  local object directory_truename(lock)
    var BPTR lock;
    { # Von hier aus hochhangeln:
      pushSTACK(NIL); # Subdir-Liste := NIL
      { var LONGALIGNTYPE(struct FileInfoBlock) fib;
        var struct FileInfoBlock * fibptr = LONGALIGN(&fib);
        loop
          { # Directory selbst ansehen:
            begin_system_call();
           {var LONG ergebnis = Examine(lock,fibptr);
            if (!ergebnis) { UnLock(lock); OS_error(); }
            end_system_call();
           }
            # seinen Namen verwenden:
           {var object name = asciz_to_string(&fibptr->fib_FileName[0],O(pathname_encoding));
            # zum Parent-Directory hochsteigen:
            var BPTR parentlock;
            begin_system_call();
            parentlock = ParentDir(lock);
            UnLock(lock);
            end_system_call();
            if (!(parentlock==BPTR_NULL))
              # name ist der Name eines Subdirectories
              { # vor die Subdir-Liste pushen:
                pushSTACK(name);
               {var object new_cons = allocate_cons();
                Car(new_cons) = popSTACK();
                Cdr(new_cons) = STACK_0;
                STACK_0 = new_cons;
               }
                lock = parentlock; # und vom Parent Directory aus weitermachen
              }
              else
              { begin_system_call();
                if (IoErr()) { OS_error(); } # Fehler aufgetreten?
                end_system_call();
                # name ist der Name eines DOS-Volumes.
                pushSTACK(name);
                break;
              }
      }   }}
      clr_break_sem_4(); # Unterbrechungen wieder zulassen
      # Stackaufbau: subdirs, devicename.
     {# subdirs mit :ABSOLUTE anfangen lassen:
      var object new_cons = allocate_cons();
      Car(new_cons) = S(Kabsolute); Cdr(new_cons) = STACK_1;
      STACK_1 = new_cons;
     }
     {var object default_dir = allocate_pathname(); # neuer Pathname mit Name=NIL und Typ=NIL
      ThePathname(default_dir)->pathname_device = popSTACK();
      ThePathname(default_dir)->pathname_directory = popSTACK();
      return default_dir;
    }}

# UP: Liefert das aktuelle Directory.
# < ergebnis: aktuelles Directory (als Pathname)
# can trigger GC
  local object default_directory (void);
  local object default_directory()
    { # Lock fürs aktuelle Directory holen:
      set_break_sem_4(); # Unterbrechungen währenddessen verhindern
      begin_system_call();
     {var BPTR lock = Lock("",ACCESS_READ);
      if (lock==BPTR_NULL)
        { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) { clr_break_sem_4(); OS_error(); }
          end_system_call(); clr_break_sem_4();
          pushSTACK(unbound); # "Wert" für Slot PATHNAME von FILE-ERROR
          fehler(file_error,
                 GETTEXT("Couldn't access current directory")
                );
        }
      end_system_call();
      return directory_truename(lock); # macht clr_break_sem_4(); und UnLock(lock);
    }}

# UP: Füllt Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: neuer absoluter Pathname
# can trigger GC
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var object pathname;
    { # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
      # Dann das Default-Directory in den Pathname einbauen:
      { var object subdirs = ThePathname(pathname)->pathname_directory;
        # Fängt pathname-directory mit :RELATIVE an?
        if (eq(Car(subdirs),S(Krelative)))
          { # ja -> Ersetze :RELATIVE durch default-subdirs, d.h.
            # bilde  (append default-subdirs (cdr subdirs))
            #      = (nreconc (reverse default-subdirs) (cdr subdirs))
            pushSTACK(pathname);
            pushSTACK(Cdr(subdirs));
           {var object temp = default_directory();
            temp = ThePathname(temp)->pathname_directory;
            temp = reverse(temp);
            subdirs = nreconc(temp,popSTACK());
            pathname = popSTACK();
            # in den Pathname eintragen:
            ThePathname(pathname)->pathname_directory = subdirs;
          }}
      }
      return pathname;
    }

# UP: Macht aus einem Directory-Namestring einen, der für AMIGAOS geeignet ist.
# OSdirnamestring(namestring)
# > namestring: neu erzeugter Directory-Namestring, mit '/' oder ':' am
#               Schluss, ein Normal-Simple-String
# < ergebnis: Namestring zu diesem Directory, im AmigaOS-Format: letzter '/'
#             gestrichen, falls überflüssig, ein Normal-Simple-String
# can trigger GC
  local object OSdirnamestring (object namestring);
  local object OSdirnamestring(namestring)
    var object namestring;
    { var uintL len = Sstring_length(namestring);
      if (len==0) goto ok; # Leerstring -> nichts streichen
     {var chart ch = TheSstring(namestring)->data[len-1];
      if (!chareq(ch,ascii('/'))) goto ok; # kein '/' am Schluss -> nichts streichen
      if (len==1) goto ok; # "/" bedeutet Parent -> nicht streichen
      ch = TheSstring(namestring)->data[len-2];
      if (chareq(ch,ascii('/')) || chareq(ch,ascii(':'))) # davor ein '/' oder ':'
        goto ok; # -> bedeutet Parent -> nicht streichen
      # '/' am Schluss streichen:
        namestring = subsstring(namestring,0,len-1);
      ok: # nichts streichen
        return namestring;
    }}

# UP: Stellt sicher, dass das Directory eines Pathname existiert.
# assure_dir_exists(tolerantp)
# > STACK_0: nicht-Logical Pathname, bei dem Directory kein :RELATIVE enthält.
# > links_resolved: Flag, ob im Directory des Pathname schon alle Links
#     aufgelöst sind und es als existierend bekannt ist
# > tolerantp: Flag, ob ein Fehler vermieden werden soll
# > subr_self: Aufrufer (ein SUBR)
# < STACK_0: (evtl. derselbe) Pathname, aber aufgelöst.
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für AMIGAOS, mit '/' am Schluss)
#     falls Name/=NIL: Namestring (für AMIGAOS)
#     falls tolerantp evtl.: nullobj
# < filestatus: Falls Name/=NIL: NULL falls das File nicht existiert,
#                                sonst ein Pointer auf eine STAT-Information.
# can trigger GC
  local var struct FileInfoBlock * filestatus;
  local object assure_dir_exists (boolean links_resolved, boolean tolerantp);
  local object assure_dir_exists(links_resolved,tolerantp)
    var boolean links_resolved;
    var boolean tolerantp;
    { if (!links_resolved)
        { # Zur Auflösung von :PARENTs, die über Root hinaussteigen,
          # müssen wir das Betriebssystem bemühen. Daher:
          var object dir_namestring;
          {var uintC stringcount = directory_namestring_parts(STACK_0); # Strings fürs Directory
           dir_namestring = string_concat(stringcount);
          }
          pushSTACK(dir_namestring);
          dir_namestring = OSdirnamestring(dir_namestring); # ohne überflüssigen '/' am Schluss
          with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz,
            { # Lock für dieses Directory holen:
              set_break_sem_4(); # Unterbrechungen währenddessen verhindern
              begin_system_call();
             {var BPTR lock = Lock(dir_namestring_asciz,ACCESS_READ);
              if (lock==BPTR_NULL)
                { var LONG errcode = IoErr();
                  end_system_call();
                  FREE_DYNAMIC_ARRAY(dir_namestring_asciz);
                  switch (errcode)
                    { case ERROR_OBJECT_NOT_FOUND:
                        clr_break_sem_4();
                        if (tolerantp) { skipSTACK(1); return nullobj; }
                        fehler_dir_not_exists(STACK_0);
                      case ERROR_ACTION_NOT_KNOWN:
                        # Ein Device, bei dem man keine Locks für Subdirectories holen
                        # kann! Hierbei muss es sich wohl um ein spezielles Device handeln
                        # (PIPE, CON, AUX, etc.).
                        # Wir stoppen die Subdirectory-Überprüfungen. Nicht einmal mehr
                        # Examine() rufen wir auf. Wir gehen im Gegenteil davon aus, dass
                        # das File im gewöhnlichen Sinne (noch) nicht existiert.
                        clr_break_sem_4(); # Unterbrechungen zulassen, da wir nun doch kein Lock belegt haben
                        if (namenullp(STACK_(0+1))) # kein File angesprochen?
                          { return popSTACK(); } # ja -> fertig
                          else
                          { var uintC stringcount = 1; # directory_namestring schon auf dem STACK
                            stringcount += file_namestring_parts(STACK_(0+1)); # Strings für den Filename
                           {var object namestring = string_concat(stringcount); # zusammenhängen
                            filestatus = (struct FileInfoBlock *)NULL; # File existiert nicht, sagen wir
                            return namestring;
                          }}
                      default:
                        OS_file_error(STACK_(0+1));
                }   }
              end_system_call();
              dir_namestring = popSTACK();
              # und überprüfen, ob's ein Directory ist:
              { var LONGALIGNTYPE(struct FileInfoBlock) fib;
                var struct FileInfoBlock * fibptr = LONGALIGN(&fib);
                begin_system_call();
               {var LONG ergebnis = Examine(lock,fibptr);
                if (!ergebnis)
                  { UnLock(lock);
                    end_system_call(); clr_break_sem_4();
                    FREE_DYNAMIC_ARRAY(dir_namestring_asciz);
                    OS_file_error(STACK_0);
                  }
                if (!(fibptr->fib_DirEntryType >= 0)) # etwa kein Directory?
                  { UnLock(lock);
                    end_system_call(); clr_break_sem_4();
                    FREE_DYNAMIC_ARRAY(dir_namestring_asciz);
                    if (tolerantp) { return nullobj; }
                    # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
                    pushSTACK(dir_namestring);
                    pushSTACK(TheSubr(subr_self)->name);
                    fehler(file_error,
                           GETTEXT("~: ~ names a file, not a directory")
                          );
                  }
                end_system_call();
              }}
              # Lock zum Truename machen:
              {var object new_pathname = directory_truename(lock); # macht clr_break_sem_4();
               var object old_pathname = STACK_0;
               ThePathname(new_pathname)->pathname_name = ThePathname(old_pathname)->pathname_name;
               ThePathname(new_pathname)->pathname_type = ThePathname(old_pathname)->pathname_type;
               STACK_0 = new_pathname;
            }}});
        }
     {var object pathname = STACK_0;
      # Information zum angesprochenen File holen:
      if (namenullp(pathname)) # kein File angesprochen?
        { return directory_namestring(pathname); } # ja -> fertig
      { var uintC stringcount = 0;
        stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
        stringcount += file_namestring_parts(pathname); # Strings für den Filename
       {var object namestring = string_concat(stringcount); # zusammenhängen
        with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
          {  # Lock für dieses File holen:
             set_break_sem_4(); # Unterbrechungen währenddessen verhindern
             begin_system_call();
           { var BPTR lock = Lock(namestring_asciz,ACCESS_READ);
             if (lock==BPTR_NULL)
               { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND))
                   { end_system_call(); clr_break_sem_4(); OS_file_error(STACK_0); }
                 end_system_call(); clr_break_sem_4();
                 # File existiert nicht.
                 filestatus = (struct FileInfoBlock *)NULL; return namestring;
               }
             end_system_call();
             # File existiert.
             # Information holen:
            {local var LONGALIGNTYPE(struct FileInfoBlock) status;
             var struct FileInfoBlock * statusptr = LONGALIGN(&status);
             begin_system_call();
             if (! Examine(lock,statusptr) )
               { UnLock(lock); end_system_call(); clr_break_sem_4(); OS_file_error(STACK_0); }
             UnLock(lock);
             end_system_call();
             clr_break_sem_4();
             if (statusptr->fib_DirEntryType >= 0) # Ist es ein Directory?
               { # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
                 pushSTACK(whole_namestring(STACK_0));
                 pushSTACK(TheSubr(subr_self)->name);
                 fehler(file_error,
                        GETTEXT("~: ~ names a directory, not a file")
                       );
               }
               else
               # normales File
               { pushSTACK(namestring);
                 # Die Groß-/Kleinschreibung des Truename wird bestimmt durch
                 # das bereits existierende File.
                 pushSTACK(asciz_to_string(&statusptr->fib_FileName[0],O(pathname_encoding)));
                 split_name_type(1);
                {var object pathname = STACK_(0+3); # der kopierte Pathname
                 ThePathname(pathname)->pathname_type = popSTACK();
                 ThePathname(pathname)->pathname_name = popSTACK();
                 # Fertig.
                 filestatus = statusptr;
                 return popSTACK(); # namestring
               }}
          }}});
    }}}}

# Dasselbe unter der Annahme, dass das Directory bereits existiert.
# (Keine Vereinfachung, da wir ja den Truename bestimmen müssen.??)
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { subr_self = L(open); return assure_dir_exists(TRUE,FALSE); }

#endif

#ifdef PATHNAME_UNIX

# UP: Liefert das aktuelle Directory.
# < ergebnis: aktuelles Directory (als Pathname)
# can trigger GC
  local object default_directory (void);
  local object default_directory()
    # Working Directory (von UNIX) ist das aktuelle Directory:
    { var char path_buffer[MAXPATHLEN]; # vgl. GETWD(3)
      # Working Directory in path_buffer ablegen:
      begin_system_call();
      if ( getwd(&path_buffer[0]) ==NULL)
        { end_system_call();
          pushSTACK(O(punkt_string)); # Wert für Slot PATHNAME von FILE-ERROR
          pushSTACK(asciz_to_string(&path_buffer[0],O(pathname_encoding))); # Meldung
          fehler(file_error,
                 GETTEXT("UNIX error while GETWD: ~")
                );
        }
      end_system_call();
      # Es muss mit '/' anfangen:
      if (!(path_buffer[0] == '/'))
        { pushSTACK(O(punkt_string)); # Wert für Slot PATHNAME von FILE-ERROR
          pushSTACK(asciz_to_string(&path_buffer[0],O(pathname_encoding)));
          fehler(file_error,
                 GETTEXT("UNIX GETWD returned ~")
                );
        }
      # in Pathname umwandeln:
      return asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding));
    }

# UP: Füllt Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: neuer Pathname, bei dem Directory kein :RELATIVE enthält.
#             (kurz: "absoluter Pathname")
# can trigger GC
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var object pathname;
    { # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
      # Dann das Default-Directory in den Pathname einbauen:
      { var object subdirs = ThePathname(pathname)->pathname_directory;
        # Fängt pathname-directory mit :RELATIVE an?
        if (eq(Car(subdirs),S(Krelative)))
          { # ja -> Ersetze :RELATIVE durch default-subdirs, d.h.
            # bilde  (append default-subdirs (cdr subdirs))
            #      = (nreconc (reverse default-subdirs) (cdr subdirs))
            pushSTACK(pathname);
            pushSTACK(Cdr(subdirs));
           {var object temp = default_directory();
            temp = ThePathname(temp)->pathname_directory;
            temp = reverse(temp);
            subdirs = nreconc(temp,popSTACK());
            pathname = popSTACK();
            # in den Pathname eintragen:
            ThePathname(pathname)->pathname_directory = subdirs;
          }}
      }
      return pathname;
    }

# UP: Stellt sicher, dass das Directory eines Pathname existiert, und löst
# dabei symbolische Links auf.
# assure_dir_exists(tolerantp)
# > STACK_0: nicht-Logical Pathname, bei dem Directory kein :RELATIVE enthält.
# > links_resolved: Flag, ob im Directory des Pathname schon alle Links
#     aufgelöst sind und es als existierend bekannt ist
# > tolerantp: Flag, ob ein Fehler vermieden werden soll
# > subr_self: Aufrufer (ein SUBR)
# < STACK_0: (evtl. derselbe) Pathname, wobei weder fürs Directory noch
#            für den Filenamen ein symbolisches Link zu verfolgen ist.
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für UNIX, mit '/' am Schluss)
#     falls Name/=NIL: Namestring (für UNIX)
#     falls tolerantp evtl.: nullobj
# < filestatus: Falls Name/=NIL: NULL falls das File nicht existiert,
#                                sonst ein Pointer auf eine STAT-Information.
# can trigger GC
  local var struct stat * filestatus;
  local object assure_dir_exists (boolean links_resolved, boolean tolerantp);
  #ifdef HAVE_LSTAT
    #define if_HAVE_LSTAT(statement)  statement
  #else
    #define if_HAVE_LSTAT(statement)
  #endif
  local object assure_dir_exists(links_resolved,tolerantp)
    var boolean links_resolved;
    var boolean tolerantp;
    { var uintC allowed_links = MAXSYMLINKS; # Anzahl der noch erlaubten symbolischen Links
      if (links_resolved) goto dir_exists;
      loop # Schleife über die aufzulösenden symbolischen Links
        { # Truepath des Directory bestimmen:
          { var char path_buffer[MAXPATHLEN]; # vgl. REALPATH(3)
            { var uintC stringcount = directory_namestring_parts(STACK_0); # Strings zum Directory
              pushSTACK(O(punkt_string)); # und "."
             {var object string = string_concat(stringcount+1); # zusammenhängen
              # symbolische Links darin auflösen:
              with_sstring_0(string,O(pathname_encoding),string_asciz,
                { begin_system_call();
                  if ( realpath(string_asciz,&path_buffer[0]) ==NULL)
                    { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
                      end_system_call();
                      if (!tolerantp)
                        { fehler_dir_not_exists(asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding))); } # fehlerhafte Komponente
                      end_system_call();
                      FREE_DYNAMIC_ARRAY(string_asciz);
                      return nullobj;
                    }
                  end_system_call();
                });
            }}
            # Neuer Directory-Path muss mit '/' anfangen:
            if (!(path_buffer[0] == '/'))
              { # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
                pushSTACK(asciz_to_string(&path_buffer[0],O(pathname_encoding)));
                fehler(file_error,
                       GETTEXT("UNIX REALPATH returned ~")
                      );
              }
            # Am Schluss evtl. ein '/' anfügen:
            {var char* pathptr = &path_buffer[0];
             var uintL len = 0; # Stringlänge
             until (*pathptr == 0) { pathptr++; len++; } # ASCIZ-Stringende suchen
             if (!((len>0) && (pathptr[-1]=='/')))
               { *pathptr = '/'; len++; } # ein '/' anfügen
            # und in einen String umwandeln:
             { var object new_string = n_char_to_string(&path_buffer[0],len,O(pathname_encoding));
            # Pathname draus machen und dessen Directory verwenden:
              {var object new_pathname = coerce_pathname(new_string);
               ThePathname(STACK_0)->pathname_directory
                 = ThePathname(new_pathname)->pathname_directory;
          } }}}
         dir_exists:
          # Information zum angesprochenen File holen:
          if (namenullp(STACK_0)) # kein File angesprochen?
            { return directory_namestring(STACK_0); } # ja -> fertig
          { var object pathname = STACK_0;
            var uintC stringcount = 0;
            stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
            stringcount += file_namestring_parts(pathname); # Strings für den Filename
           {var object namestring = string_concat(stringcount); # zusammenhängen
            # Information holen:
            local struct stat status;
            with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
              { begin_system_call();
                if (!( lstat(namestring_asciz,&status) ==0))
                  { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
                    # File existiert nicht.
                    end_system_call();
                    FREE_DYNAMIC_ARRAY(namestring_asciz);
                    filestatus = (struct stat *)NULL; return namestring;
                  }
                end_system_call();
                # File existiert.
                if (S_ISDIR(status.st_mode)) # Ist es ein Directory?
                  { # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
                    pushSTACK(whole_namestring(STACK_0));
                    pushSTACK(TheSubr(subr_self)->name);
                    fehler(file_error,
                           GETTEXT("~: ~ names a directory, not a file")
                          );
                  }
                if_HAVE_LSTAT(
                elif (possible_symlink(namestring_asciz) && S_ISLNK(status.st_mode)) # Ist es ein symbolisches Link?
                  # ja -> weiterverfolgen:
                  { if (allowed_links==0) # keine Links mehr erlaubt?
                      # ja -> UNIX-Error ELOOP simulieren
                      { begin_system_call();
                        errno = ELOOP_VALUE;
                        end_system_call();
                        OS_file_error(STACK_0);
                      }
                    allowed_links--; # danach ist ein Link weniger erlaubt
                   {var uintL linklen = status.st_size; # vermutliche Länge des Link-Inhalts
                    retry_readlink:
                    { var DYNAMIC_ARRAY(linkbuf,char,linklen+1); # Buffer für den Link-Inhalt
                      # Link-Inhalt lesen:
                      begin_system_call();
                     {var int result = readlink(namestring_asciz,linkbuf,linklen);
                      end_system_call();
                      if (result<0)
                        { OS_file_error(STACK_0); }
                      if (!(result == (int)linklen)) # manchmal (AIX, NFS) stimmt status.st_size nicht
                        { FREE_DYNAMIC_ARRAY(linkbuf); linklen = result; goto retry_readlink; }
                     }
                      # Daraus ein Pathname machen:
                      # (MERGE-PATHNAMES (PARSE-NAMESTRING linkbuf) pathname-without-name&type)
                      pushSTACK(subr_self);
                      pushSTACK(n_char_to_string(linkbuf,linklen,O(pathname_encoding)));
                      FREE_DYNAMIC_ARRAY(linkbuf);
                    }
                    funcall(L(parse_namestring),1);
                    pushSTACK(value1);
                    pathname = copy_pathname(STACK_(0+2));
                    ThePathname(pathname)->pathname_name = NIL;
                    ThePathname(pathname)->pathname_type = NIL;
                    pushSTACK(pathname);
                    funcall(L(merge_pathnames),2);
                    subr_self = popSTACK();
                    STACK_0 = value1;
                  }}
                ) # HAVE_LSTAT
                else
                  # normales File
                  { filestatus = &status; return namestring; }
              });
          }}
    }   }

# Dasselbe unter der Annahme, dass das Directory bereits existiert.
# (Nur wenig Vereinfachung, da das File ein symbolisches Link in ein anderes
# Directory sein kann, und dieses muss dann als existent überprüft werden.)
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { subr_self = L(open); return assure_dir_exists(TRUE,FALSE); }

#endif

#ifdef PATHNAME_RISCOS

# Ein "absoluter Pathname" ist ein Pathname, bei dem Directory mit
# (:ABSOLUTE :ROOT ...) beginnt.

# UP: Liefert das aktuelle Directory.
# < ergebnis: aktuelles Directory (als Pathname)
# can trigger GC
  local object default_directory (void);
  local object default_directory()
    # Working Directory (von RISCOS) ist das aufgelöste "@":
    { var char path_buffer[MAXPATHLEN];
      # Working Directory in path_buffer ablegen:
      begin_system_call();
      if ( realpath("@",&path_buffer[0]) ==NULL) { OS_error(); }
      end_system_call();
      # in Pathname umwandeln:
      return asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding));
    }

#if 0 # unbenutzt
# UP: Convert a valid RISCOS file namestring to an absolute pathname.
# canonicalise_filename(filename)
# > filename: Simple-Asciz-String
# < result: absolute pathname
  local object canonicalise_filename (object filename);
  local object canonicalise_filename(filename)
    var object filename;
    { var char path_buffer[MAXPATHLEN];
      with_string_0(filename,O(pathname_encoding),filename_asciz,
        { begin_system_call();
          if ( realpath(filename_asciz,&path_buffer[0]) ==NULL) { OS_error(); }
          end_system_call();
        });
      # in Pathname umwandeln:
      return coerce_pathname(asciz_to_string(&path_buffer[0],O(pathname_encoding)));
    }
#endif

# UP: Convert a valid RISCOS directory namestring to an absolute pathname.
# canonicalise_dirname(pathname,dirname)
# > pathname: Pathname whose host name and device is to be used
# > dirname: Normal-Simple-String, ends with '.'
# < result: absolute pathname
  local object canonicalise_dirname (object pathname, object dirname);
  local object canonicalise_dirname(pathname,dirname)
    var object pathname;
    var object dirname;
    { pushSTACK(pathname);
     {var uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
      # Device, vgl. directory_namestring_parts():
      { var object device = ThePathname(pathname)->pathname_device;
        if (!(nullp(device))) # NIL -> kein String
          { pushSTACK(O(doppelpunkt_string)); # ":"
            pushSTACK(device); # Device auf den Stack
            pushSTACK(O(punkt_string)); # "."
            stringcount += 3; # und mitzählen
      }   }
      pushSTACK(dirname);
      {var object dir_string = string_concat(stringcount+1);
       with_sstring(dir_string,O(pathname_encoding),dir_path,len,
         { # Punkt am Schluss durch Nullbyte ersetzen:
           ASSERT((len > 0) && (dir_path[len-1] == '.'));
           dir_path[len-1] = '\0';
           # absolut machen:
          {var char path_buffer[MAXPATHLEN];
           begin_system_call();
           if ( realpath(dir_path,&path_buffer[0]) ==NULL)
             { end_system_call(); OS_file_error(STACK_0); }
           end_system_call();
           skipSTACK(1);
           # in Pathname umwandeln:
           dir_string = asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding));
         }});
       return dir_string;
    }}}

# UP: Füllt Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: nicht-Logical Pathname
# < ergebnis: neuer Pathname, bei dem Directory kein :RELATIVE u.ä. enthält.
#             (kurz: "absoluter Pathname")
# can trigger GC
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var object pathname;
    { var boolean resolved_root = FALSE;
      retry:
      # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
     {var object subdirs = ThePathname(pathname)->pathname_directory;
      # Ist das Device angegeben, so muss das Directory mit (:ABSOLUTE :ROOT ...)
      # anfangen (oder mit (:RELATIVE ...) - das wird ersetzt).
      if (!nullp(ThePathname(pathname)->pathname_device))
        { if (eq(Car(subdirs),S(Krelative)))
            { pushSTACK(pathname); # pathname retten
              pushSTACK(allocate_cons());
             {var object new_cons = allocate_cons();
              subdirs = popSTACK();
              pathname = popSTACK(); # pathname zurück
              Car(subdirs) = S(Kabsolute); Cdr(subdirs) = new_cons;
              Car(new_cons) = S(Kroot); Cdr(new_cons) = Cdr(ThePathname(pathname)->pathname_directory);
              ThePathname(pathname)->pathname_directory = subdirs;
            }}
          elif (!(eq(Car(subdirs),S(Kabsolute)) && eq(Car(Cdr(subdirs)),S(Kroot))))
            { pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
              pushSTACK(pathname);
              pushSTACK(O(root_string));
              pushSTACK(TheSubr(subr_self)->name);
              fehler(file_error,
                     GETTEXT("~: If a device is specified, the directory must begin with ~: ~")
                    );
            }
        }
      pushSTACK(pathname); # pathname retten
      {var object defaults;
       if (eq(Car(subdirs),S(Krelative)))
         { pushSTACK(Cdr(subdirs)); defaults = default_directory(); }
       else # (eq(Car(subdirs),S(Kabsolute)))
         { var object next = Car(Cdr(subdirs));
           pushSTACK(Cdr(Cdr(subdirs)));
           if (eq(next,S(Kroot))) # :ROOT -> "$." auflösen oder fertig
             { # "$." wird nur dann aufgelöst, wenn Host oder Device noch
               # unbekannt sind, aber nur einmal (um eine Endlosschleife zu
               # verhindern). Ob Host oder Device =NIL sind, ist nämlich
               # nicht so wichtig.
               if (!resolved_root
                   && (nullp(ThePathname(pathname)->pathname_host)
                       || nullp(ThePathname(pathname)->pathname_device)
                  )   )
                 { defaults = canonicalise_dirname(pathname,O(root_string));
                   resolved_root = TRUE;
                 }
               else
                 { goto resolved; }
             }
           elif (eq(next,S(Khome))) # :HOME -> "&." auflösen
             { defaults = canonicalise_dirname(pathname,O(home_string)); }
           elif (eq(next,S(Kcurrent))) # :CURRENT -> "@." auflösen
             { defaults = canonicalise_dirname(pathname,O(current_string)); }
           elif (eq(next,S(Klibrary))) # :LIBRARY -> "%." auflösen
             { defaults = canonicalise_dirname(pathname,O(library_string)); }
           elif (eq(next,S(Kprevious))) # :PREVIOUS -> "\\." auflösen
             { defaults = canonicalise_dirname(pathname,O(previous_string)); }
           else
             { NOTREACHED }
         }
       # Stackaufbau: pathname, rest-subdirs.
       # Nicht ganz so wie bei MERGE-PATHNAMES verfahren:
       # bilde  (append default-subdirs rest-subdirs)
       #      = (nreconc (reverse default-subdirs) rest-subdirs)
       pathname = STACK_1;
       ThePathname(pathname)->pathname_host = ThePathname(defaults)->pathname_host;
       ThePathname(pathname)->pathname_device = ThePathname(defaults)->pathname_device;
       defaults = ThePathname(defaults)->pathname_directory;
       defaults = reverse(defaults); subdirs = nreconc(defaults,popSTACK());
       pathname = popSTACK();
       ThePathname(pathname)->pathname_directory = subdirs;
       # Es könnte sein, dass auch jetzt noch nicht alles aufgelöst ist.
       goto retry;
     }}
     resolved: # Stackaufbau: pathname, subdir-oldlist.
      # Liste durchgehen und dabei neu aufconsen, dabei "^." verarbeiten.
      # (Sonst müsste dies assure_dir_exists() machen.)
      pushSTACK(S(Kroot)); pushSTACK(S(Kabsolute));
      { var object newlist = listof(2); pushSTACK(newlist); }
      # Stackaufbau: pathname, subdir-oldlist, subdir-newlist.
      while (mconsp(STACK_1)) # Bis oldlist am Ende ist:
        { var object subdir = Car(STACK_1); # nächstes subdir
          if (eq(subdir,S(Kparent)))
            # = :PARENT -> newlist um eins verkürzen:
            { if (matomp(Cdr(Cdr(STACK_0)))) # newlist (bis auf :ABSOLUTE und :ROOT) leer ?
                { # :PARENT von "$." aus liefert Error
                  pushSTACK(STACK_2); # Wert für Slot PATHNAME von FILE-ERROR
                  pushSTACK(O(root_string)); # "$."
                  pushSTACK(directory_namestring(STACK_(2+2))); # Directory von pathname
                  fehler(file_error,
                         GETTEXT("no directory ~ above ~")
                        );
                }
              STACK_0 = Cdr(STACK_0);
            }
          else
            { # newlist um eins verlängern:
              pushSTACK(subdir);
             {var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
              Cdr(new_cons) = STACK_0;
              STACK_0 = new_cons;
            }}
          STACK_1 = Cdr(STACK_1);
        }
     {var object subdirs = nreverse(popSTACK()); # newlist, wieder umdrehen
      skipSTACK(1);
      pathname = popSTACK();
      ThePathname(pathname)->pathname_directory = subdirs; # in den Pathname eintragen
      return pathname;
    }}

# UP: Liefert den Namestring eines Pathname fürs Betriebssystem.
# OSnamestring(dir_namestring)
# > STACK_0: nicht-Logical Pathname
# > dir_namestring: Directory-Namestring
# < ergebnis: Namestring (für RISCOS, mit Name/Type vertauscht)
# can trigger GC
  local object OSnamestring (object dir_namestring);
  local object OSnamestring(dir_namestring)
    var object dir_namestring;
    { var object pathname = STACK_0;
      var uintC stringcount;
      pushSTACK(dir_namestring); # Directory-Namestring als 1. String
      stringcount = # und Strings zum Filenamen
        (nullp(ThePathname(pathname)->pathname_type)
         ? nametype_namestring_parts(ThePathname(pathname)->pathname_name,
                                     ThePathname(pathname)->pathname_type,
                                     ThePathname(pathname)->pathname_version)
         # Name und Typ vertauschen (der Typ wird zu einem Subdirectory-Namen):
         : nametype_namestring_parts(ThePathname(pathname)->pathname_type,
                                     ThePathname(pathname)->pathname_name,
                                     ThePathname(pathname)->pathname_version)
        );
      return string_concat(1+stringcount); # zusammenhängen
    }

# UP: Stellt sicher, dass das Directory eines Pathname existiert.
# Sonst Fehlermeldung.
# assure_dir_exists(links_resolved,tolerantp)
# assure_dir_exists_(links_resolved,tolerantp,allowdir)
# > STACK_0: absoluter Pathname ohne Wildcards im Directory
# > links_resolved: Flag, ob im Directory des Pathname schon alle Links
#     aufgelöst sind und es als existierend bekannt ist
# > tolerantp: Flag, ob ein Fehler vermieden werden soll
# > allowdir: Flag, ob bei Name/=NIL ein Directory statt eines File erlaubt ist
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für RISCOS, mit '.' am Schluss)
#     falls Name/=NIL: Namestring (für RISCOS)
#     falls tolerantp evtl.: nullobj
# < filestatus: Falls Name/=NIL: NULL falls das File nicht existiert,
#                                sonst ein Pointer auf eine STAT-Information.
# can trigger GC
  local var struct stat * filestatus;
  #define assure_dir_exists(links_resolved,tolerantp)  \
     assure_dir_exists_(links_resolved,tolerantp,FALSE)
  local object assure_dir_exists_ (boolean links_resolved, boolean tolerantp, boolean allowdir);
  local object assure_dir_exists_(links_resolved,tolerantp,allowdir)
    var boolean links_resolved;
    var boolean tolerantp;
    var boolean allowdir;
    { var object pathname = STACK_0;
      var uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
      stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
     {var object dir_namestring = string_concat(stringcount); # zusammenhängen
      if (!links_resolved)
        { # Existenztest:
          var struct stat statbuf;
          with_sstring(dir_namestring,O(pathname_encoding),dir_namestring_asciz,len,
            { ASSERT((len > 0) && (dir_namestring_asciz[len-1]=='.'));
              dir_namestring_asciz[len-1] = '\0'; # '.' am Schluss durch Nullbyte ersetzen
              begin_system_call();
              if (stat(dir_namestring_asciz,&statbuf) < 0)
                { if (!(tolerantp && (errno==ENOENT)))
                    { end_system_call(); OS_file_error(STACK_0); }
                  end_system_call();
                  FREE_DYNAMIC_ARRAY(dir_namestring_asciz);
                  return nullobj;
                }
              end_system_call();
            });
          if (!S_ISDIR(statbuf.st_mode)) # gefundene Datei kein Unterdirectory ?
            { if (tolerantp) { return nullobj; }
              fehler_dir_not_exists(dir_namestring);
        }   }
      # Information zum angesprochenen File holen:
      if (namenullp(STACK_0)) # kein File angesprochen?
        { return dir_namestring; } # ja -> fertig
        else
        { var object namestring = OSnamestring(dir_namestring);
          # Information holen:
          local struct stat status;
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { begin_system_call();
              if (stat(namestring_asciz,&status) < 0)
                { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
                  # File existiert nicht.
                  end_system_call();
                  FREE_DYNAMIC_ARRAY(namestring_asciz);
                  filestatus = (struct stat *)NULL; return namestring;
                }
              end_system_call();
            });
          # File existiert.
          if (!allowdir && S_ISDIR(status.st_mode)) # Ist es ein Directory?
            { # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
              pushSTACK(whole_namestring(STACK_0));
              pushSTACK(TheSubr(subr_self)->name);
              fehler(file_error,
                     GETTEXT("~: ~ names a directory, not a file")
                    );
            }
          else
            # normales File oder (erlaubterweise) Directory
            { filestatus = &status; return namestring; }
    }}  }

# Dasselbe unter der Annahme, dass das Directory bereits existiert.
# (Keine Vereinfachung, da wir ja den Truename bestimmen müssen.??)
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { subr_self = L(open); return assure_dir_exists(TRUE,FALSE); }

# Ein File "name.type" wird dem RISCOS als "type.name" vermittelt, dabei ist
# "type" der Name eines Unterverzeichnisses! Soll ein File "name.type" angelegt
# werden, muss daher zuerst das Unterverzeichnis "type" erzeugt werden.
# prepare_create(pathname);
# > pathname: ein Pathname
# can trigger GC
  local void prepare_create (object pathname);
  local object pathname_add_subdir (void);
  local void prepare_create(pathname)
    var object pathname;
    { if (!nullp(ThePathname(pathname)->pathname_type))
        { # call pathname_add_subdir:
          pushSTACK(pathname); pushSTACK(ThePathname(pathname)->pathname_type);
          pathname = pathname_add_subdir();
          ThePathname(pathname)->pathname_name = NIL;
          ThePathname(pathname)->pathname_type = NIL;
          # call MAKE-DIR if the directory does not exist:
          pushSTACK(subr_self); # subr_self retten
          pushSTACK(pathname);
          if (eq(assure_dir_exists(FALSE,TRUE),nullobj))
            { funcall(L(make_dir),1); }
          else
            { skipSTACK(1); }
          subr_self = popSTACK(); # subr_self zurück
    }   }

#else

# Normally nothing special to do before creating a file.
  #define prepare_create(pathname)

#endif

#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
#if 0 # unbenutzt
# UP: Macht aus einem Directory-Namestring einen, der für DOS geeignet ist.
# OSdirnamestring(namestring)
# > namestring: neu erzeugter Directory-Namestring, mit '\' am Schluss,
#               ein Normal-Simple-String
# < ergebnis: Namestring zu diesem Directory, im DOS-Format: letzter '\'
#             gestrichen, falls überflüssig, ein Normal-Simple-String
# can trigger GC
  local object OSdirnamestring (object namestring);
  local object OSdirnamestring(namestring)
    var object namestring;
    { var uintL len = Sstring_length(namestring);
      if (len==0) goto ok; # Leerstring -> nichts streichen
     {var chart ch = TheSstring(namestring)->data[len-1];
      if (!chareq(ch,ascii('\\'))) goto ok; # kein '\' am Schluss -> nichts streichen
      if (len==1) goto ok; # "\" bedeutet Root -> nicht streichen
      ch = TheSstring(namestring)->data[len-2];
      if (chareq(ch,ascii('\\')) || chareq(ch,ascii(':'))) # davor ein '\' oder ':'
        goto ok; # -> bedeutet Parent -> nicht streichen
      # '\' am Schluss streichen:
        namestring = subsstring(namestring,0,len-1);
      ok: # nichts streichen
        return namestring;
    }}
#endif
# UP: Setzt das Default-Drive und sein Default-Directory neu.
# change_default();
# > STACK_0: absoluter Pathname, bei dem Device ein String ist und Directory
#     kein :RELATIVE, :CURRENT, :PARENT enthält, und Name und Typ =NIL sind.
# can trigger GC
  local void change_default (void);
  local void change_default()
    { # Default-Directory zu diesem Drive neu setzen:
      { var object pathname = STACK_0;
        var uintC stringcount =
          directory_namestring_parts(pathname); # Strings fürs Directory
        # ohne überflüssiges '\' am Schluss
        if (mconsp(Cdr(ThePathname(pathname)->pathname_directory)))
          { skipSTACK(1); stringcount--; }
       {var object string = string_concat(stringcount); # zusammenhängen
        with_sstring_0(string,O(pathname_encoding),asciz,
          { # Default-Directory ändern:
            change_current_directory(asciz);
          });
      }}
      # Default-Drive neu setzen:
      O(default_drive) = ThePathname(STACK_0)->pathname_device;
      # *DEFAULT-PATHNAME-DEFAULTS* neu setzen:
      recalc_defaults_pathname();
    }
#endif
#ifdef PATHNAME_AMIGAOS
# UP: Setzt das Default-Directory neu.
# change_default();
# > STACK_0: absoluter Pathname, bei dem Directory kein :RELATIVE, :CURRENT,
#     :PARENT enthält, und Name und Typ =NIL sind.
# can trigger GC
  local void change_default (void);
  extern BPTR orig_dir_lock; # Lock auf das ursprüngliche Verzeichnis
                             # (das gehört nicht uns, nicht freigeben!)
  local void change_default()
    { var uintC stringcount =
        directory_namestring_parts(STACK_0); # Strings fürs Directory
      var object dir_namestring = string_concat(stringcount);
      dir_namestring = OSdirnamestring(dir_namestring); # ohne überflüssigen '/' am Schluss
      with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz,
        { # Default-Directory ändern:
          set_break_sem_4();
          begin_system_call();
          {var BPTR lock = Lock(dir_namestring_asciz,ACCESS_READ);
           if (lock==BPTR_NULL) { end_system_call(); OS_file_error(STACK_0); }
           lock = CurrentDir(lock); # current directory neu setzen
           # Lock zum alten current directory merken bzw. aufgeben:
           if (orig_dir_lock == BPTR_NONE)
             { orig_dir_lock = lock; }
             else
             { UnLock(lock); }
          }
          end_system_call();
          clr_break_sem_4();
        });
    }
#endif
#ifdef PATHNAME_UNIX
# UP: Setzt das Default-Directory neu.
# change_default();
# > STACK_0: absoluter Pathname, bei dem Directory kein :RELATIVE, :CURRENT,
#     :PARENT enthält, und Name und Typ =NIL sind.
# can trigger GC
  local void change_default (void);
  local void change_default()
    { var uintC stringcount = host_namestring_parts(STACK_0); # Strings für den Host
      stringcount += directory_namestring_parts(STACK_0); # Strings fürs Directory
     {var object string = string_concat(stringcount); # zusammenhängen
      with_sstring_0(string,O(pathname_encoding),asciz,
        { # Default-Directory ändern:
          begin_system_call();
          if (!( chdir(asciz) ==0)) { end_system_call(); OS_file_error(STACK_0); }
          end_system_call();
        });
    }}
#endif
#ifdef PATHNAME_RISCOS
# UP: Setzt das Default-Directory neu.
# change_default();
# > STACK_0: absoluter Pathname, bei dem Name und Typ =NIL sind.
# can trigger GC
  local void change_default (void);
  local void change_default()
    { var object pathname = STACK_0;
      var uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
      stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
     {var object dir_namestring = string_concat(stringcount); # zusammenhängen
      with_sstring(dir_namestring,O(pathname_encoding),dir_namestring_asciz,len,
        { ASSERT((len > 0) && (dir_namestring_asciz[len-1]=='.'));
          dir_namestring_asciz[len-1] = '\0'; # '.' am Schluss durch Nullbyte ersetzen
          begin_system_call();
          if (!( chdir(dir_namestring_asciz) ==0)) { end_system_call(); OS_file_error(STACK_0); }
          end_system_call();
        });
    }}
#endif

LISPFUN(namestring,1,1,norest,nokey,0,NIL)
# (NAMESTRING pathname), CLTL S. 417
# (NAMESTRING pathname t) -> Namestring im externen Format
#   Unix: mit Default-Directory
  { var object flag = popSTACK(); # optionales Argument flag
    var object pathname = coerce_pathname(popSTACK()); # Argument zu einem Pathname machen
    #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_RISCOS) || defined(PATHNAME_WIN32)
    if (!eq(flag,unbound) && !nullp(flag))
      # flag /= NIL -> fürs Betriebssystem:
      { check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        # (da Unix/AMIGAOS das Default-Directory von LISP nicht kennt
        # und Win32 multitasking ist)
        value1 = whole_namestring(pathname);
      }
      else
    #endif
      # normal
      { value1 = whole_namestring(pathname); }
    mv_count=1;
  }

# Fehlermeldung wegen fehlendem Dateinamen
# fehler_noname(pathname);
# > pathname: Pathname
  nonreturning_function(local, fehler_noname, (object pathname));
  local void fehler_noname(pathname)
    var object pathname;
    { pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(pathname);
      fehler(file_error,
             GETTEXT("no file name given: ~")
            );
    }

# Fehler wegen unzulässiger Name/Type-Angabe
# fehler_notdir(pathname);
# > pathname: Pathname
  nonreturning_function(local, fehler_notdir, (object pathname));
  local void fehler_notdir(pathname)
    var object pathname;
    { pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(pathname);
      fehler(file_error,
             GETTEXT("not a directory: ~")
            );
    }

# Test, ob ein File existiert:
# file_exists(namestring)
# > vorausgegangen: assure_dir_exists()
# > STACK_0: Pathname, wie nach Ausführung von assure_dir_exists(), Name/=NIL
# > namestring: dessen Namestring fürs Betriebssystem
  #if defined(MSDOS) || defined(WIN32_NATIVE)
    #ifdef MSDOS
      local inline int access0 (CONST char* path);
      local inline int access0(path)
        var CONST char* path;
        { var int erg;
          begin_system_call();
          erg = access(path,0);
          end_system_call();
          return erg;
        }
    #endif
    #ifdef WIN32_NATIVE
      local inline int access0 (const char* path);
      local inline int access0(path)
        var const char* path;
        { var DWORD fileattr;
          begin_system_call();
          fileattr = GetFileAttributes(path);
          if (fileattr == 0xFFFFFFFF)
            { if (GetLastError()==ERROR_FILE_NOT_FOUND)
                { end_system_call(); return -1; }
              end_system_call(); OS_file_error(STACK_0);
            }
          end_system_call();
          return 0;
        }
    #endif
    local boolean file_exists (object namestring);
    local boolean file_exists(namestring)
      var object namestring;
      { var boolean exists;
        with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
          { exists = (access0(namestring_asciz)==0); });
        return exists;
      }
  #endif
  #ifdef AMIGAOS
    #define file_exists(namestring)  (!(filestatus == (struct FileInfoBlock *)NULL))
    #define FILE_EXISTS_TRIVIAL
  #endif
  #if defined(UNIX) || defined(RISCOS)
    #define file_exists(namestring)  (!(filestatus == (struct stat *)NULL))
    #define FILE_EXISTS_TRIVIAL
  #endif

# Fehlermeldung wegen nicht existenter Datei
# fehler_file_not_exists();
# > STACK_0: Pathname
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_file_not_exists, (void));
  local void fehler_file_not_exists()
    { # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(STACK_0); # pathname
      pushSTACK(TheSubr(subr_self)->name);
      fehler(file_error,
             GETTEXT("~: file ~ does not exist")
            );
    }

LISPFUNN(truename,1)
# (TRUENAME pathname), CLTL S. 413
  { var object pathname = popSTACK(); # pathname-Argument
    if (builtin_stream_p(pathname))
      # Stream -> extra behandeln:
      { # muss File-Stream sein:
        pathname = as_file_stream(pathname);
        test_file_stream_named(pathname);
        # Streamtyp File-Stream
        value1 = TheStream(pathname)->strm_file_truename;
      }
      else
      { pathname = coerce_pathname(pathname); # zu einem Pathname machen
        check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        pushSTACK(pathname);
       {# Directory muss existieren:
        var object namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
        if (namenullp(STACK_0))
          # Kein Name angegeben
          { if (!nullp(ThePathname(STACK_0)->pathname_type))
              { # STACK_0 = Wert für Slot PATHNAME von FILE-ERROR
                pushSTACK(STACK_0); # pathname
                pushSTACK(TheSubr(subr_self)->name);
                fehler(file_error,
                       GETTEXT("~: pathname with type but without name makes no sense: ~")
                      );
              }
            # Kein Name und kein Typ angegeben -> pathname als Ergebnis
          }
          else
          # Name angegeben.
          { # Überprüfe, ob die Datei existiert:
            if (!file_exists(namestring)) { fehler_file_not_exists(); }
            # Datei existiert -> Pathname als Wert
          }
        value1 = popSTACK();
      }}
    mv_count=1;
  }

LISPFUNN(probe_file,1)
# (PROBE-FILE filename), CLTL S. 424
  { var object pathname = popSTACK(); # pathname-Argument
    if (builtin_stream_p(pathname))
      # Stream -> extra behandeln:
      { # muss File-Stream sein:
        pathname = as_file_stream(pathname);
        test_file_stream_named(pathname);
        # Streamtyp File-Stream -> Truename nehmen:
       {var uintB flags = TheStream(pathname)->strmflags;
        pathname = TheStream(pathname)->strm_file_truename;
        if (flags & strmflags_open_B) # Datei geöffnet ?
          # ja -> Truename sofort als Ergebnis:
          { value1 = pathname; mv_count=1; return; }
        # nein -> noch testen, ob die Datei zum Truename existiert.
      }}
      else
      { pathname = coerce_pathname(pathname); } # zu einem Pathname machen
    # pathname ist jetzt ein Pathname.
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
    # Name angegeben.
    pushSTACK(pathname);
   {# Directory muss existieren:
    var object namestring = assure_dir_exists(FALSE,TRUE); # Filename fürs Betriebssystem
    if (eq(namestring,nullobj))
      # Pfad zur Datei existiert nicht -> NIL als Wert:
      { skipSTACK(1); value1 = NIL; mv_count=1; return; }
    # Überprüfe, ob die Datei existiert:
    if (file_exists(namestring))
      { value1 = popSTACK(); mv_count=1; } # Datei existiert -> Pathname als Wert
      else
      { skipSTACK(1); value1 = NIL; mv_count=1; return; } # sonst NIL als Wert
  }}

# Stellt fest, ob ein Directory existiert.
# directory_exists(pathname)
# > pathname: ein absoluter Pathname ohne Wildcards, mit Name=NIL und Typ=NIL
# < result: TRUE wenn es ein existierendes Verzeichnis bezeichnet
# can trigger GC
  local boolean directory_exists (object pathname);
  local boolean directory_exists(pathname)
    var object pathname;
    { pushSTACK(pathname); # Pathname retten
     {var uintC stringcount = directory_namestring_parts(pathname); # Strings fürs Directory
      var object dir_namestring = string_concat(stringcount); # zusammenhängen
      # Existenztest, siehe auch assure_dir_exists():
      var boolean exists = TRUE;
      #ifdef MSDOS
        if (!(nullp(Cdr(ThePathname(STACK_0)->pathname_directory))
              #ifdef PATHNAME_OS2
              || equal(Cdr(ThePathname(STACK_0)->pathname_directory),O(pipe_subdirs))
              #endif
           ) )
          { with_sstring(dir_namestring,O(pathname_encoding),dir_namestring_asciz,len,
              { ASSERT((len > 0) && (dir_namestring_asciz[len-1] == '\\'));
                dir_namestring_asciz[len-1] = '\0'; # '\' am Schluss durch Nullbyte ersetzen
               {var struct stat statbuf;
                begin_system_call();
                if (stat(dir_namestring_asciz,&statbuf) < 0)
                  { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
                    exists = FALSE;
                  }
                else
                  { if (!S_ISDIR(statbuf.st_mode)) # gefundene Datei kein Unterdirectory ?
                      { exists = FALSE; }
                  }
                end_system_call();
              }});
          }
      #endif
      #ifdef WIN32_NATIVE
        with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz,
          { if (!nullp(Cdr(ThePathname(STACK_0)->pathname_directory)))
              { var uintL len = Sstring_length(dir_namestring);
                ASSERT((len > 0) && (dir_namestring_asciz[len-1] == '\\'));
                dir_namestring_asciz[len-1] = '\0'; # '\' am Schluss durch Nullbyte ersetzen
              }
            begin_system_call();
           {var DWORD fileattr = GetFileAttributes(dir_namestring_asciz);
            if (fileattr == 0xFFFFFFFF)
              { if (!(GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND))
                  { end_system_call(); OS_file_error(STACK_0); }
                exists = FALSE;
              }
            else
              { if (!(fileattr & FILE_ATTRIBUTE_DIRECTORY)) # gefundene Datei kein Unterdirectory ?
                  { exists = FALSE; }
              }
            end_system_call();
          }});
      #endif
      #ifdef PATHNAME_AMIGAOS
        dir_namestring = OSdirnamestring(dir_namestring); # ohne überflüssigen '/' am Schluss
        with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz,
          { # Lock für dieses Directory holen:
            set_break_sem_4(); # Unterbrechungen währenddessen verhindern
            begin_system_call();
           {var BPTR lock = Lock(dir_namestring_asciz,ACCESS_READ);
            if (lock==BPTR_NULL)
              { var LONG errcode = IoErr();
                switch (errcode)
                  { case ERROR_OBJECT_NOT_FOUND:
                    case ERROR_ACTION_NOT_KNOWN:
                      exists = FALSE;
                      break;
                    default:
                      end_system_call(); OS_file_error(STACK_0);
              }   }
            else
              { var LONGALIGNTYPE(struct FileInfoBlock) fib;
                var struct FileInfoBlock * fibptr = LONGALIGN(&fib);
               {var LONG ergebnis = Examine(lock,fibptr);
                if (!ergebnis || !(fibptr->fib_DirEntryType >= 0))
                  { exists = FALSE; }
                UnLock(lock);
              }}
            end_system_call();
            clr_break_sem_4();
          }});
      #endif
      #ifdef PATHNAME_UNIX
        pushSTACK(dir_namestring);
        pushSTACK(O(punkt_string)); # und "."
        dir_namestring = string_concat(2); # zusammenhängen
        with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz,
          { var struct stat statbuf;
            begin_system_call();
            if (stat(dir_namestring_asciz,&statbuf) < 0)
              { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
                exists = FALSE;
              }
            else
              { if (!S_ISDIR(statbuf.st_mode)) # gefundene Datei kein Unterdirectory ?
                  { exists = FALSE; }
              }
            end_system_call();
          });
      #endif
      #ifdef PATHNAME_RISCOS
        with_sstring(dir_namestring,O(pathname_encoding),dir_namestring_asciz,len,
          { ASSERT((len > 0) && (dir_namestring_asciz[len-1]=='.'));
            dir_namestring_asciz[len-1] = '\0'; # '.' am Schluss durch Nullbyte ersetzen
           {var struct stat statbuf;
            begin_system_call();
            if (stat(dir_namestring_asciz,&statbuf) < 0)
              { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
                exists = FALSE;
              }
            else
              { if (!S_ISDIR(statbuf.st_mode)) # gefundene Datei kein Unterdirectory ?
                  { exists = FALSE; }
              }
            end_system_call();
          }});
      #endif
      skipSTACK(1);
      return exists;
    }}

LISPFUNN(probe_directory,1)
# (PROBE-DIRECTORY filename) stellt fest, ob ein Directory existiert.
  { var object pathname = popSTACK(); # pathname-Argument
    pathname = coerce_pathname(pathname); # zu einem Pathname machen
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    # Überprüfe, ob Name=NIL und Typ=NIL :
    if (!(nullp(ThePathname(pathname)->pathname_name)
          && nullp(ThePathname(pathname)->pathname_type)
       ) )
      { fehler_notdir(pathname); }
    value1 = (directory_exists(pathname) ? T : NIL); mv_count=1;
  }

# UP: Stellt fest, ob eine Datei geöffnet ist.
# openp(pathname)
#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
# > pathname: absoluter Pathname, ohne Wildcards.
#endif
#ifdef PATHNAME_AMIGAOS
# > pathname: absoluter Pathname, ohne Wildcards, ohne :PARENT
#endif
#ifdef PATHNAME_UNIX
# > pathname: absoluter Pathname, ohne Wildcards, nach Auflösung
#             symbolischer Links
#endif
# < ergebnis: TRUE, falls ein geöffneter File-Stream auf diese Datei existiert.
  local boolean openp (object pathname);
  local boolean openp(pathname)
    var object pathname;
    { var object flist = O(open_files); # Liste aller offenen Files durchlaufen
      while (consp(flist))
        { var object f = Car(flist); # nächster offener Stream
          if (TheStream(f)->strmtype == strmtype_file) # File-Stream ?
            { if (equal(TheStream(f)->strm_file_truename,pathname))
                { return TRUE; }
            }
          flist = Cdr(flist);
        }
      return FALSE;
    }

# Fehlermeldung wegen Löschversuch auf geöffnete Datei
# fehler_delete_open(pathname);
# > pathname: Truename der Datei
  nonreturning_function(local, fehler_delete_open, (object pathname));
  local void fehler_delete_open(pathname)
    var object pathname;
    { pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(pathname);
      fehler(file_error,
             GETTEXT("cannot delete file ~ since there is file stream open to it")
            );
    }

LISPFUNN(delete_file,1)
# (DELETE-FILE filename), CLTL S. 424
  { var object pathname = popSTACK(); # pathname-Argument
    if (builtin_stream_p(pathname))
      # Stream -> extra behandeln:
      { var object stream = as_file_stream(pathname); # muss File-Stream sein
        test_file_stream_named(stream);
        # Streamtyp File-Stream.
        # Falls Datei geöffnet, erst Datei schließen:
        if (TheStream(stream)->strmflags & strmflags_open_B) # Datei geöffnet ?
          { pushSTACK(stream); builtin_stream_close(&STACK_0); stream = popSTACK(); }
        # Dann den Truename als zu löschende Datei nehmen:
        pathname = TheStream(stream)->strm_file_truename;
      }
      else
      { pathname = coerce_pathname(pathname); } # zu einem Pathname machen
    # pathname ist jetzt ein Pathname.
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
    # Name angegeben.
    pushSTACK(pathname);
   {# Directory muss existieren:
    var object namestring = assure_dir_exists(FALSE,TRUE); # Filename fürs Betriebssystem
    if (eq(namestring,nullobj))
      # Pfad zur Datei existiert nicht -> Wert NIL
      { skipSTACK(1); value1 = NIL; mv_count=1; return; }
    if (openp(STACK_0)) { fehler_delete_open(STACK_0); } # Keine offenen Dateien löschen!
    # Datei löschen:
    #ifdef FILE_EXISTS_TRIVIAL
    if (!file_exists(namestring))
      { skipSTACK(1); value1 = NIL; mv_count=1; return; } # File existiert nicht -> Wert NIL
    #endif
    with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
      { if (!delete_file_if_exists(namestring_asciz))
          { # File existiert nicht -> Wert NIL
            FREE_DYNAMIC_ARRAY(namestring_asciz); skipSTACK(1);
            value1 = NIL; mv_count=1; return;
          }
      });
    # Datei existierte, wurde gelöscht -> Pathname (/=NIL) als Wert
    value1 = popSTACK(); mv_count=1;
  }}

# Fehlermeldung wegen Umbenennungsversuch einer geöffneten Datei
# fehler_rename_open(pathname);
# > pathname: Truename der Datei
  nonreturning_function(local, fehler_rename_open, (object pathname));
  local void fehler_rename_open(pathname)
    var object pathname;
    { pushSTACK(pathname); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(pathname);
      fehler(file_error,
             GETTEXT("cannot rename file ~ since there is file stream open to it")
            );
    }

# UP: Führt eine Datei-Umbenennung durch.
# rename_file();
# > Stackaufbau: filename, newname, oldpathname.
# < Stackaufbau: filename, newname, oldpathname, newpathname,
#                oldtruename, oldnamestring, newtruename, newnamestring.
  local void rename_file (void);
  local void rename_file()
    { # 1. newpathname := (MERGE-PATHNAMES newname oldpathname)
      { pushSTACK(STACK_1); # newname als 1. Argument
        pushSTACK(STACK_(0+1)); # oldpathname als 2. Argument
        funcall(L(merge_pathnames),2);
        pushSTACK(value1);
      }
      # Stackaufbau: filename, newname, oldpathname, newpathname.
      # 2. oldpathname überprüfen:
      { var object oldpathname = STACK_1;
        check_no_wildcards(oldpathname); # mit Wildcards -> Fehler
        oldpathname = use_default_dir(oldpathname); # Default-Directory einfügen
        if (namenullp(oldpathname)) { fehler_noname(oldpathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(oldpathname);
       {# Directory muss existieren:
        var object old_namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
        if (openp(STACK_0)) { fehler_rename_open(STACK_0); } # Keine offenen Dateien umbenennen!
        pushSTACK(old_namestring);
      }}
      # Stackaufbau: filename, newname, oldpathname, newpathname,
      #              oldtruename, oldnamestring.
      # 3. newpathname überprüfen:
      { var object newpathname = coerce_pathname(STACK_2);
        check_no_wildcards(newpathname); # mit Wildcards -> Fehler
        newpathname = use_default_dir(newpathname); # Default-Directory einfügen
        if (namenullp(newpathname)) { fehler_noname(newpathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(newpathname);
       {# Directory muss existieren:
        var object new_namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
        # Stackaufbau: filename, newname, oldpathname, newpathname,
        #              oldtruename, oldnamestring, newtruename.
        # 4. Datei umbenennen:
        if (file_exists(new_namestring))
          # Datei existiert bereits -> nicht ohne Vorwarnung löschen
          { fehler_file_exists(S(rename_file),STACK_0); }
        pushSTACK(new_namestring);
      }}
      # Stackaufbau: filename, newname, oldpathname, newpathname,
      #              oldtruename, oldnamestring, newtruename, newnamestring.
      # Nun kann gefahrlos umbenannt werden:
      prepare_create(STACK_4);
      with_sstring_0(STACK_2,O(pathname_encoding),oldnamestring_asciz,
        with_sstring_0(STACK_0,O(pathname_encoding),newnamestring_asciz,
          { rename_file_to_nonexisting(oldnamestring_asciz,newnamestring_asciz); } ); );
    }

LISPFUNN(rename_file,2)
# (RENAME-FILE filename newname), CLTL S. 423
  { var object filename = STACK_1; # filename-Argument
    if (builtin_stream_p(filename))
      # Stream -> extra behandeln:
      { # muss File-Stream sein:
        filename = as_file_stream(filename);
        test_file_stream_named(filename);
        # Streamtyp File-Stream -> Truename verwenden:
        filename = TheStream(filename)->strm_file_truename;
        pushSTACK(filename);
        # Umbenennen:
        rename_file();
        # Stream aktualisieren:
        filename = STACK_7;
        TheStream(filename)->strm_file_name = STACK_4; # newpathname als neuer Name
        TheStream(filename)->strm_file_truename = STACK_1; # newtruename als neuer Truename
        # Handle etc. unverändert lassen
      }
      else
      { filename = coerce_pathname(filename); # zu einem Pathname machen
        pushSTACK(filename);
        # Umbenennen:
        rename_file();
      }
    value1 = STACK_4; # newpathname als 1. Wert
    value2 = STACK_3; # oldtruename als 2. Wert
    value3 = STACK_1; # newtruename als 3. Wert
    mv_count=3; skipSTACK(8); # 3 Werte
  }

# Create a file.
# create_new_file(pathstring);
# It is known that the file does not already exist.
# > pathstring: file name, ASCIZ-String
# > STACK_0: pathname
  local inline void create_new_file (char* pathstring);
  local inline void create_new_file(pathstring)
    var char* pathstring;
    {
      #ifdef AMIGAOS
        var Handle handle;
        begin_system_call();
        handle = Open(pathstring,MODE_NEWFILE);
        if (handle == Handle_NULL) { end_system_call(); OS_file_error(STACK_0); } # Error melden
        # Datei wurde erzeugt, handle ist das Handle.
        # Datei wieder schließen:
        (void) Close(handle);
        end_system_call();
      #endif
      #ifdef WIN32_NATIVE
        var Handle handle;
        begin_system_call();
        handle = CreateFile(pathstring, 0, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (handle==INVALID_HANDLE_VALUE) { end_system_call(); OS_file_error(STACK_0); }
        # Datei wurde erzeugt, handle ist das Handle.
        # Datei wieder schließen:
        if (!CloseHandle(handle)) { end_system_call(); OS_file_error(STACK_0); }
        end_system_call();
      #endif
      #if defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM)
        var int result;
        begin_system_call();
        result = creat(pathstring,my_open_mask);
        if (result<0) { end_system_call(); OS_file_error(STACK_0); } # Error melden
        setmode(ergebnis,O_BINARY);
        # Datei wurde erzeugt, ergebnis ist das Handle.
        # Datei wieder schließen:
        if (!(CLOSE(result)==0)) { end_system_call(); OS_file_error(STACK_0); } # Error melden
        end_system_call();
      #endif
      #if defined(UNIX) || defined(RISCOS)
        var int result;
        begin_system_call();
        result = OPEN(pathstring, O_WRONLY | O_BINARY | O_CREAT | O_TRUNC, my_open_mask);
        if (result<0) { end_system_call(); OS_file_error(STACK_0); } # Error melden
        # Datei wurde erzeugt, ergebnis ist das Handle.
        # Datei wieder schließen:
        if (!(CLOSE(result)==0)) { end_system_call(); OS_file_error(STACK_0); } # Error melden
        end_system_call();
      #endif
    }

# Open a file for input.
# open_input_file(pathstring,create_if_not_exists,&handle)
# > vorausgegangen: assure_dir_exists()
# > pathstring: file name, ASCIZ-String
# > create_if_not_exists: if true, the file must be created
# > STACK_0: pathname
# < handle: open file handle
# < result: whether the file could be opened (necessarily TRUE if create_if_not_exists)
  local inline boolean open_input_file (char* pathstring, boolean create_if_not_exists, Handle* handle_);
  local inline boolean open_input_file(pathstring,create_if_not_exists,handle_)
    var char* pathstring;
    var boolean create_if_not_exists;
    var Handle* handle_;
    {
      #if defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM)
        var sintW result;
        # Datei zu öffnen versuchen:
        #ifdef FILE_EXISTS_TRIVIAL
          if (file_exists(_EMA_))
            { begin_system_call();
              result = open(pathstring,O_RDONLY);
            }
            else
            { # Datei existiert nicht
              if (!create_if_not_exists) return FALSE;
              # Datei mit creat erzeugen:
              begin_system_call();
              result = creat(pathstring,my_open_mask);
            }
          if (result<0) { end_system_call(); OS_file_error(STACK_0); }
        #else
          # erst mit open erfragen, ob die Datei existiert:
          begin_system_call();
          result = open(pathstring,O_RDONLY);
          if (result<0)
            { if (errno == ENOENT) # nicht gefunden?
                { # Datei existiert nicht
                  if (!create_if_not_exists) { end_system_call(); return FALSE; }
                  # Datei mit creat erzeugen:
                  result = creat(pathstring,my_open_mask);
                  if (result<0) { end_system_call(); OS_file_error(STACK_0); }
                }
                else
                { end_system_call(); OS_file_error(STACK_0); } # sonstigen Error melden
            }
        #endif
        setmode(result,O_BINARY);
        end_system_call();
        *handle_ = result; return TRUE;
      #endif
      #ifdef AMIGAOS
        var Handle handle;
        #ifdef FILE_EXISTS_TRIVIAL
          if (file_exists(_EMA_))
            { begin_system_call();
              handle = Open(pathstring,MODE_OLDFILE);
            }
            else
            { # Datei existiert nicht
              if (!create_if_not_exists) return FALSE;
              # Datei mit Open erzeugen:
              begin_system_call();
              handle = Open(pathstring,MODE_READWRITE);
            }
        #else
          # erst mit Open erfragen, ob die Datei existiert:
          begin_system_call();
          handle = Open(pathstring,MODE_OLDFILE);
          if (handle==Handle_NULL)
            { if (IoErr()==ERROR_OBJECT_NOT_FOUND)
                { # Datei existiert nicht
                  if (!create_if_not_exists) { end_system_call(); return FALSE; }
                  # Datei mit Open erzeugen:
                  handle = Open(pathstring,MODE_READWRITE);
            }   }
        #endif
        end_system_call();
        if (handle==Handle_NULL) { OS_file_error(STACK_0); }
        *handle_ = handle; return TRUE;
      #endif
      #if defined(UNIX) || defined(RISCOS)
        var int result;
        #ifdef FILE_EXISTS_TRIVIAL
          var int oflags = O_RDONLY | O_BINARY;
          if (!file_exists(_EMA_))
            { # Datei existiert nicht
              if (!create_if_not_exists) return FALSE;
              # Datei mit open erzeugen:
              oflags |= O_CREAT;
            }
          begin_system_call();
          result = OPEN(pathstring,oflags,my_open_mask);
          end_system_call();
          if (result<0) { OS_file_error(STACK_0); }
        #else
          var int oflags = O_RDONLY | O_BINARY;
          if (create_if_not_exists) { oflags |= O_CREAT; }
          begin_system_call();
          result = OPEN(pathstring,oflags,my_open_mask);
          if (result<0)
            { if (errno == ENOENT) # nicht gefunden?
                { # Datei existiert nicht
                  if (!create_if_not_exists) { end_system_call(); return FALSE; }
                }
              end_system_call(); OS_file_error(STACK_0); # sonstigen Error melden
            }
          end_system_call();
        #endif
        *handle_ = result; return TRUE;
      #endif
      #ifdef WIN32_NATIVE
        var Handle handle;
        #ifdef FILE_EXISTS_TRIVIAL
          var DWORD flag = OPEN_EXISTING;
          if (!file_exists(_EMA_))
            { # Datei existiert nicht
              if (!create_if_not_exists) return FALSE;
              # Datei mit CreateFile erzeugen:
              flag = OPEN_ALWAYS;
            }
          begin_system_call();
          handle = CreateFile(pathstring, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                              NULL, flag, FILE_ATTRIBUTE_NORMAL, NULL);
          end_system_call();
          if (handle==INVALID_HANDLE_VALUE) { OS_file_error(STACK_0); }
        #else
          var DWORD flag = OPEN_EXISTING;
          if (create_if_not_exists) { flag = OPEN_ALWAYS; }
          begin_system_call();
          handle = CreateFile(pathstring, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                              NULL, flag, FILE_ATTRIBUTE_NORMAL, NULL);
          if (handle==INVALID_HANDLE_VALUE)
            { if (GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND) # nicht gefunden?
                { # Datei existiert nicht
                  if (!create_if_not_exists) { end_system_call(); return FALSE; }
                }
              end_system_call(); OS_file_error(STACK_0); # sonstigen Error melden
            }
          end_system_call();
        #endif
        *handle_ = handle; return TRUE;
      #endif
    }

#if defined(UNIX) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
# Open a file for output.
# open_output_file(pathstring,truncate_if_exists)
# > pathstring: file name, ASCIZ-String
# > truncate_if_exists: if true, the file is truncated to zero size
# > STACK_0: pathname
# < result: open file handle
  local inline Handle open_output_file (char* pathstring, boolean truncate_if_exists);
  local inline Handle open_output_file(pathstring,truncate_if_exists)
    var char* pathstring;
    var boolean truncate_if_exists;
    {
      #ifdef AMIGAOS
        var Handle handle;
        begin_system_call();
        handle = Open(pathstring, (truncate_if_exists ? MODE_NEWFILE : MODE_READWRITE));
        end_system_call();
        if (handle==Handle_NULL) { OS_file_error(STACK_0); } # Error melden
        return handle;
      #endif
      #if defined(UNIX) || defined(RISCOS)
        var int result;
        begin_system_call();
        result = OPEN(pathstring,
                      (truncate_if_exists ? O_RDWR | O_BINARY | O_CREAT | O_TRUNC
                                          : O_RDWR | O_BINARY | O_CREAT),
                      my_open_mask);
        end_system_call();
        if (result<0) { OS_file_error(STACK_0); } # Error melden
        return result;
      #endif
      #ifdef WIN32_NATIVE
        var Handle handle;
        begin_system_call();
        handle = CreateFile(pathstring, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
                            NULL, (truncate_if_exists ? CREATE_ALWAYS : OPEN_ALWAYS), FILE_ATTRIBUTE_NORMAL, NULL);
        end_system_call();
        if (handle==INVALID_HANDLE_VALUE) { OS_file_error(STACK_0); }
        return handle;
      #endif
    }
#endif

# Create a backup file before opening a file for output.
# create_backup_file(pathstring,delete_backup_file);
# > vorausgegangen: assure_dir_exists()
# > pathstring: file name, ASCIZ-String
# > delete_backup_file: if true, delete the backup file
# > STACK_0: pathname
  local inline void create_backup_file (char* pathstring, boolean delete_backup_file);
  local inline void create_backup_file(pathstring,delete_backup_file)
    var char* pathstring;
    var boolean delete_backup_file;
    { var object filename = STACK_0;
      if (openp(filename)) { fehler_rename_open(filename); } # Keine offenen Dateien umbenennen!
     {var object new_namestring;
      #if defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM)
        # Truename mit ".BAK" erweitern:
        # filename := (merge-pathnames ".BAK" filename) :
        filename = copy_pathname(filename); # kopieren
        ThePathname(filename)->pathname_type = O(backuptype_string); # mit Extension "BAK"
        if (openp(filename)) { fehler_delete_open(filename); } # Keine offenen Dateien löschen!
        pushSTACK(filename);
        # Directory existiert schon.
        new_namestring = assume_dir_exists(); # Filename fürs Betriebssystem
      #endif
      #if defined(UNIX) || defined(AMIGAOS) || defined(WIN32_NATIVE)
        # Truename mit "%" bzw. ".bak" bzw. "~" erweitern:
        # filename := (parse-namestring (concatenate 'string (namestring filename) "%")) :
        filename = whole_namestring(filename); # als String
        pushSTACK(filename); pushSTACK(O(backupextend_string)); # "%"
        filename = string_concat(2); # dazuhängen
        pushSTACK(filename); # retten
        pushSTACK(filename); # retten
        filename = coerce_pathname(filename); # wieder als Filename
        if (openp(filename)) { fehler_delete_open(filename); } # Keine offenen Dateien löschen!
        STACK_1 = filename;
        # Directory existiert schon. Hier keine weiteren Links verfolgen.
        new_namestring = popSTACK(); # Filename fürs Betriebssystem
      #endif
      #ifdef RISCOS
        # Dem Namen ein "~" voranstellen:
        filename = copy_pathname(filename);
        pushSTACK(filename);
        pushSTACK(O(backupprepend_string)); pushSTACK(ThePathname(filename)->pathname_name);
        { var object new_name = string_concat(2);
          filename = STACK_0;
          ThePathname(filename)->pathname_name = new_name;
        }
        if (openp(filename)) { fehler_delete_open(filename); } # Keine offenen Dateien löschen!
        new_namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
      #endif
      with_sstring_0(new_namestring,O(pathname_encoding),new_namestring_asciz,
        { # Datei (oder Link) mit diesem Namen löschen, falls vorhanden:
          delete_file_before_rename(new_namestring_asciz);
          # Datei vom alten auf diesen Namen umbenennen:
          rename_existing_file(pathstring,new_namestring_asciz);
          if (delete_backup_file) { delete_existing_file(new_namestring_asciz); }
        });
      skipSTACK(1);
    }}

# UP: erzeugt ein File-Stream
# open_file(filename,direction,if_exists,if_not_exists)
# > STACK_2: :BUFFERED argument
# > STACK_1: :EXTERNAL-FORMAT argument
# > STACK_0: :ELEMENT-TYPE argument
# > filename: Filename, ein Pathname
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO, 3 = :INPUT-IMMUTABLE)
# > if_exists: :IF-EXISTS-Argument
#         (0 = nichts, 1 = :ERROR, 2 = NIL,
#          3 = :RENAME, 4 = :RENAME-AND-DELETE, 5 = :NEW-VERSION,:SUPERSEDE,
#          6 = :APPEND, 7 = :OVERWRITE)
# > if_not_exists: :IF-DOES-NOT-EXIST-Argument
#         (0 = nichts, 1 = :ERROR, 2 = NIL, 3 = :CREATE)
# < ergebnis: Stream oder NIL
# < STACK: aufgeräumt
# can trigger GC
  local object open_file (object filename, uintB direction, uintB if_exists, uintB if_not_exists);
  local object open_file(filename,direction,if_exists,if_not_exists)
    var object filename;
    var uintB direction;
    var uintB if_exists;
    var uintB if_not_exists;
    { pushSTACK(filename); # Filename retten
      check_no_wildcards(filename); # mit Wildcards -> Fehler
      filename = use_default_dir(filename); # Default-Directory einfügen
      if (namenullp(filename)) { fehler_noname(filename); } # Kein Name angegeben -> Fehler
      pushSTACK(filename); # absPathname retten
      # Stackaufbau: Pathname, absPathname.
      { # Directory muss existieren:
        var object namestring = # Filename fürs Betriebssystem
          assure_dir_exists(FALSE,(direction == 0) && ((if_not_exists % 2) == 0)); # tolerant nur bei :PROBE und if_not_exists = 0 oder 2
        # Stackaufbau: Pathname, Truename.
        # Filename überprüfen und Handle holen:
        var object handle;
        var boolean append_flag = FALSE;
        switch (direction)
          { case 0: # Modus ist :PROBE
              if (eq(namestring,nullobj))
                # Pfad zur Datei existiert nicht, und :IF-DOES-NOT-EXIST = nichts oder NIL
                goto ergebnis_NIL;
              if (!file_exists(namestring))
                # Datei existiert nicht
                { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                  if (if_not_exists==1) # :ERROR -> Error
                    goto fehler_notfound;
                  if (!(if_not_exists==3)) # nichts oder NIL -> NIL
                    goto ergebnis_NIL;
                  # :CREATE -> Datei mit open erzeugen und schließen:
                  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
                    { prepare_create(STACK_0);
                      create_new_file(namestring_asciz);
                    });
                }
              handle = NIL; # Handle := NIL
              break;
            case 1: case 3: # Modus ist :INPUT
              { var Handle handl;
                var boolean result;
                #ifdef PATHNAME_RISCOS
                if (!file_exists(namestring))
                  { pushSTACK(namestring); prepare_create(STACK_1); namestring = popSTACK(); }
                #endif
                with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
                  { result = open_input_file(namestring_asciz,if_not_exists==3,&handl); }
                  );
                if (!result)
                  { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                    if (if_not_exists==2) # NIL -> NIL
                      goto ergebnis_NIL;
                      else # nichts oder :ERROR -> Error
                      goto fehler_notfound;
                  }
                handle = allocate_handle(handl);
              }
              break;
            default: # Modus ist :OUTPUT oder :IO
              { # Defaultwert für if_not_exists ist von if_exists abhängig:
                if (if_not_exists==0) # falls if_not_exists nicht angegeben:
                  { if (if_exists<6) # if_exists = :APPEND oder :OVERWRITE -> if_not_exists unverändert
                      { if_not_exists = 3; } # weder :APPEND noch :OVERWRITE -> Default ist :CREATE
                  }
                # Defaultwert für if_exists ist :NEW-VERSION :
                if (if_exists==0) { if_exists = 5; }
                #if defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM)
                  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
                    { # Bei if_exists=5 und if_not_exists=3 kann man sofort
                      # CREAT ansteuern, sonst muss man vorher OPEN versuchen:
                      if (!((if_exists==5) && (if_not_exists==3)))
                        { begin_system_call();
                         {var sintW ergebnis = # Datei zu öffnen versuchen
                            open(namestring_asciz,O_RDWR);
                          if (ergebnis<0)
                            { if (errno == ENOENT) # nicht gefunden?
                                # Datei existiert nicht
                                { end_system_call();
                                  # :IF-DOES-NOT-EXIST-Argument entscheidet:
                                  if (if_not_exists<2) # (Default bei :APPEND oder :OVERWRITE) oder :ERROR ?
                                    goto fehler_notfound;
                                  if (if_not_exists==2) # NIL -> NIL
                                    goto ergebnis_NIL;
                                  # :CREATE
                                }
                                else
                                { end_system_call(); OS_file_error(STACK_0); } # sonstigen Error melden
                            }
                            else
                            # Datei existiert, ergebnis ist das Handle
                            { # :IF-EXISTS-Argument entscheidet:
                              switch (if_exists)
                                { case 1: # :ERROR -> schließen und Error
                                    { if (CLOSE(ergebnis) < 0) { end_system_call(); OS_file_error(STACK_0); } # Error melden
                                      end_system_call();
                                      goto fehler_exists;
                                    }
                                  case 2: # NIL -> schließen und NIL
                                    { if (CLOSE(ergebnis) < 0) { end_system_call(); OS_file_error(STACK_0); } # Error melden
                                      end_system_call();
                                      goto ergebnis_NIL;
                                    }
                                  case 6: # :APPEND
                                    append_flag = TRUE; # am Schluss ans Ende positionieren
                                  case 7: # :OVERWRITE -> bestehende Datei benutzen
                                    setmode(ergebnis,O_BINARY);
                                    end_system_call();
                                    handle = allocate_handle(ergebnis);
                                    goto handle_ok;
                                  default: ;
                                    # :RENAME, :RENAME-AND-DELETE -> Datei umbenennen und dann neu eröffnen.
                                    # :NEW-VERSION, :SUPERSEDE -> Datei auf Länge 0 kürzen.
                                }
                              # In beiden Fällen erst die Datei schließen:
                              if (CLOSE(ergebnis) < 0) { end_system_call(); OS_file_error(STACK_0); } # Error melden
                              end_system_call();
                              if ((if_exists==3) || (if_exists==4))
                                # :RENAME oder :RENAME-AND-DELETE -> umbenennen:
                                { create_backup_file(namestring_asciz,if_exists==4); }
                            }
                        }}
                      # Datei mit CREAT erzeugen:
                      begin_system_call();
                      { var sintW ergebnis = # erzeugen
                          creat(namestring_asciz,my_open_mask);
                        if (ergebnis<0) { end_system_call(); OS_file_error(STACK_0); } # Error melden
                        setmode(ergebnis,O_BINARY);
                        end_system_call();
                        # Datei neu erzeugt, ergebnis ist das Handle
                        handle = allocate_handle(ergebnis);
                      }
                    });
                #endif
                #if defined(UNIX) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
                  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
                    { if (file_exists(namestring))
                        # Datei existiert
                        { # :IF-EXISTS-Argument entscheidet:
                          switch (if_exists)
                            { case 1: # :ERROR -> Error
                                goto fehler_exists;
                              case 2: # NIL -> NIL
                                goto ergebnis_NIL;
                              case 3: case 4: # :RENAME oder :RENAME-AND-DELETE -> umbenennen:
                                create_backup_file(namestring_asciz,if_exists==4);
                                break;
                              case 6: # :APPEND
                                append_flag = TRUE; # am Schluss ans Ende positionieren
                              default: ;
                                # :OVERWRITE -> bestehende Datei benutzen
                                # :NEW-VERSION, :SUPERSEDE -> Datei auf Länge 0 kürzen.
                        }   }
                        else
                        # Datei existiert nicht
                        { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                          if (if_not_exists<2) # (Default bei :APPEND oder :OVERWRITE) oder :ERROR ?
                            goto fehler_notfound;
                          if (if_not_exists==2) # NIL -> NIL
                            goto ergebnis_NIL;
                          # :CREATE
                        }
                      prepare_create(STACK_0);
                      # Datei mit open öffnen:
                      { # if-exists-Handling: bei if_exists<=5 Inhalt löschen,
                        # sonst (bei :APPEND, :OVERWRITE) bestehenden Inhalt lassen.
                        # if-not-exists-Handling: neue Datei erzeugen.
                        var Handle handl = open_output_file(namestring_asciz,if_exists<=5);
                        handle = allocate_handle(handl);
                      }
                  });
                #endif
                break;
              }
            ergebnis_NIL: # Ergebnis NIL
              skipSTACK(5); # beide Pathnames und drei Argumente vergessen
              return NIL;
            fehler_notfound: # Fehler, da Datei nicht gefunden
              # STACK_0 = Truename, Wert für Slot PATHNAME von FILE-ERROR
              pushSTACK(STACK_0);
              fehler(file_error,
                     GETTEXT("file ~ does not exist")
                    );
            fehler_exists: # Fehler, da Datei bereits existiert
              # STACK_0 = Truename, Wert für Slot PATHNAME von FILE-ERROR
              pushSTACK(STACK_0);
              fehler(file_error,
                     GETTEXT("a file named ~ already exists")
                    );
          }
        handle_ok:
        # handle und append_flag sind jetzt fertig.
        # Stream erzeugen:
        pushSTACK(STACK_4); # :BUFFERED argument
        pushSTACK(STACK_4); # :EXTERNAL-FORMAT argument
        pushSTACK(STACK_4); # :ELEMENT-TYPE argument
        pushSTACK(handle);
       {var object stream = make_file_stream(direction,append_flag,TRUE);
        skipSTACK(3);
        return stream;
    } }}

LISPFUN(old_open,1,0,norest,key,5,\
        (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist),kw(external_format)) )
# (SYS::OLD-OPEN filename :direction :element-type :if-exists :if-does-not-exist :external-format)
# for binary backward compatibility only.
  { pushSTACK(unbound); C_open(); }

LISPFUN(open,1,0,norest,key,6,\
        (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist),kw(external_format),kw(buffered)) )
# (OPEN filename :direction :element-type :if-exists :if-does-not-exist :external-format :buffered)
  { var object filename = STACK_6; # filename
    if (builtin_stream_p(filename))
      { # muss File-Stream sein:
        filename = as_file_stream(filename);
        test_file_stream_named(filename);
        # Streamtyp File-Stream -> Truename verwenden:
        filename = TheStream(filename)->strm_file_truename;
      }
      else
      { filename = coerce_pathname(filename); } # zu einem Pathname machen
    # filename ist jetzt ein Pathname.
   {var uintB direction;
    var uintB if_exists;
    var uintB if_not_exists;
    # :direction überprüfen und in direction übersetzen:
    { var object arg = STACK_5;
      if (eq(arg,unbound) || eq(arg,S(Kinput))) { direction = 1; }
      elif (eq(arg,S(Kinput_immutable))) { direction = 3; }
      elif (eq(arg,S(Koutput))) { direction = 4; }
      elif (eq(arg,S(Kio))) { direction = 5; }
      elif (eq(arg,S(Kprobe))) { direction = 0; }
      else
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_direction)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(open));
        fehler(type_error,
               GETTEXT("~: illegal :DIRECTION argument ~")
              );
    } }
    # :element-type wird später überprüft.
    # :if-exists überprüfen und in if_exists übersetzen:
    { var object arg = STACK_3;
      if (eq(arg,unbound)) { if_exists = 0; }
      elif (eq(arg,S(Kerror))) { if_exists = 1; }
      elif (eq(arg,NIL)) { if_exists = 2; }
      elif (eq(arg,S(Krename))) { if_exists = 3; }
      elif (eq(arg,S(Krename_and_delete))) { if_exists = 4; }
      elif (eq(arg,S(Knew_version)) || eq(arg,S(Ksupersede))) { if_exists = 5; }
      elif (eq(arg,S(Kappend))) { if_exists = 6; }
      elif (eq(arg,S(Koverwrite))) { if_exists = 7; }
      else
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_if_exists)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(open));
        fehler(type_error,
               GETTEXT("~: illegal :IF-EXISTS argument ~")
              );
    } }
    # :if-does-not-exist überprüfen und in if_not_exists übersetzen:
    { var object arg = STACK_2;
      if (eq(arg,unbound)) { if_not_exists = 0; }
      elif (eq(arg,S(Kerror))) { if_not_exists = 1; }
      elif (eq(arg,NIL)) { if_not_exists = 2; }
      elif (eq(arg,S(Kcreate))) { if_not_exists = 3; }
      else
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_if_does_not_exist)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(open));
        fehler(type_error,
               GETTEXT("~: illegal :IF-DOES-NOT-EXIST argument ~")
              );
    } }
    # :external-format wird später überprüft.
    # :buffered wird später überprüft.
    # File öffnen:
    STACK_6 = STACK_0; STACK_5 = STACK_1; skipSTACK(4);
    value1 = open_file(filename,direction,if_exists,if_not_exists);
    mv_count=1;
  }}

# UP: Liefert eine Liste aller matchenden Pathnames.
# directory_search(pathname)
# > pathname: Pathname mit Device /= :WILD
#ifdef UNIX
# > STACK_1: Circle-Flag
#endif
# > STACK_0: Full-Flag
# < ergebnis:
#     Falls name=NIL und type=NIL:     Liste aller matchenden Directories,
#     sonst (name=NIL -> name=:WILD):  Liste aller matchenden Dateien.
#     Jeweils als absoluter Pathname ohne Wildcards,
#     bzw. bei Dateien und Full-Flag /=NIL als Liste
#          (Pathname Write-Date Length)
#          mit  Pathname ohne :WILD/:WILD-INFERIORS-Komponenten,
#               Write-Date = Datum der Dateierstellung (ss mm hh dd mm yy),
#                 als Decoded-Time passend für ENCODE-UNIVERSAL-TIME,
#               Length = Länge der Datei (in Bytes).
# can trigger GC
  local object directory_search (object pathname);
  # Methode: Breadth-first-search, damit nur eine Suchoperation gleichzeitig
  # läuft.
  #
  #ifdef WATCOM
    # Die findfirst/findnext-Routinen sollen gefälligst errno setzen:
    local int findfirst (const char * path, struct ffblk * buf, unsigned int attr);
    local int findnext (struct ffblk * buf);
    local int findfirst(path,buf,attr)
      var const char * path;
      var struct ffblk * buf;
      var unsigned int attr;
      { var unsigned int result = _dos_findfirst(path,attr,buf);
        if (result==0)
          { return 0; } # kein Error
          else
          { errno = result; # = _doserrno;
            return -1; # Error
      }   }
    local int findnext(buf)
      var struct ffblk * buf;
      { var unsigned int result = _dos_findnext(buf);
        if (result==0)
          { return 0; } # kein Error
          else
          { errno = result; # = _doserrno;
            return -1; # Error
      }   }
  #endif
  #
  #if defined(MSDOS) || defined(WIN32_NATIVE)
    # Common set of macros for directory search.
    #ifdef MSDOS
      #define READDIR_wildnametype_suffix  O(wild_wild_string) # "*.*"
      #define READDIR_var_declarations  \
        var struct ffblk DTA_buffer; \
        set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
      #define READDIR_end_declarations  \
        clr_break_sem_4();
      #define READDIR_findfirst(pathstring,error_statement,done_statement)  \
        if (findfirst(pathstring,&DTA_buffer,FA_DIREC|FA_ARCH|FA_RDONLY) <0) \
          { if (!((errno==ENOENT) || (errno==ENOMORE)))                      \
              { error_statement }                                            \
            else                                                             \
              { done_statement }                                             \
          }
      #define READDIR_findnext(error_statement,done_statement)  \
        if (findnext(&DTA_buffer) <0)                                        \
          { if (!((errno==ENOENT) || (errno==ENOMORE)))                      \
              { error_statement }                                            \
            else                                                             \
              { done_statement }                                             \
          }
      #define READDIR_entry_name()  (&DTA_buffer.ff_name[0])
      #define READDIR_entry_ISDIR()  (DTA_buffer.ff_attrib & FA_DIREC)
      #define READDIR_entry_timedate(timepointp)  \
        convert_timedate((uintW)DTA_buffer.ff_ftime,(uintW)DTA_buffer.ff_fdate,timepointp);
      #define READDIR_entry_size()  (*(uintL*)(&DTA_buffer.ff_fsize))
    #endif
    #ifdef WIN32_NATIVE
      #define READDIR_wildnametype_suffix  O(wild_string) # "*"
      #define READDIR_var_declarations  \
        var WIN32_FIND_DATA filedata; \
        var HANDLE search_handle;
      #define READDIR_end_declarations
      #define READDIR_findfirst(pathstring,error_statement,done_statement)  \
        if ((search_handle = FindFirstFile(pathstring,&filedata)) == INVALID_HANDLE_VALUE)       \
          { if (!(GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND)) \
              { error_statement }                                                                \
            else                                                                                 \
              { done_statement }                                                                 \
          }
      #define READDIR_findnext(error_statement,done_statement)  \
        if (!FindNextFile(search_handle,&filedata))                                              \
          { if (!(GetLastError()==ERROR_NO_MORE_FILES) || !FindClose(search_handle))             \
              { error_statement }                                                                \
            else                                                                                 \
              { done_statement }                                                                 \
          }
      #define READDIR_entry_name()  (&filedata.cFileName[0])
      #define READDIR_entry_ISDIR()  (filedata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      #define READDIR_entry_timedate(timepointp)  \
        { var FILETIME* pftimepoint = &filedata.ftLastWriteTime;               \
          if (pftimepoint->dwLowDateTime==0 && pftimepoint->dwHighDateTime==0) \
            pftimepoint = &filedata.ftCreationTime;                            \
          convert_time(pftimepoint,timepointp);                                \
        }
      #define READDIR_entry_size()  filedata.nFileSizeLow
    #endif
  #endif
  #
  #if defined(UNIX) || defined(RISCOS)
  # Just like stat(), except that directories or files which would lead
  # to problems are silently hidden.
    local inline int stat_for_search (char* pathstring, struct stat * statbuf);
    local inline int stat_for_search(pathstring,statbuf)
      var char* pathstring;
      var struct stat * statbuf;
      {
        #ifdef UNIX_LINUX
          # Avoid searching /proc: It is a zoo containing strange animals:
          # directories which go away constantly, pseudo-regular files which
          # are really pipes, etc.
          if (asciz_equal(pathstring,"/proc")) { errno = ENOENT; return -1; }
        #endif
       {var int result = stat(pathstring,statbuf);
        #ifdef UNIX_CYGWIN32
        if ((result < 0) && (errno == EACCES)) { errno = ENOENT; }
        #endif
        return result;
      }}
  #endif
  #
  #ifdef PATHNAME_EXT83
  #
  # UP: Extrahiert Name und Typ aus dem DTA-Buffer.
  # Es wird angenommen, dass Name und Typ aus zulässigen Großbuchstaben
  # bestehen und eine Länge <= 8 bzw. 3 haben.
  # > asciz: Adresse des ASCIZ-Strings im DTA-Buffer
  # > def: Default-Typ
  # < -(STACK): Typ
  # < -(STACK): Name
  # Erniedrigt STACK um 2.
  # kann GC auslösen
    local void extract (const uintB* asciz, object def);
    local void extract(asciz,def)
      var const uintB* asciz;
      var object def;
      { pushSTACK(def); # Default-Typ in den Stack
       {# in Name.Typ aufspalten:
        var const uintB* ptr = asciz;
        var uintL count = 0;
        loop
          { var uintB ch = *ptr; # nächstes Zeichen
            if ((ch == 0) || (ch == '.')) # bei Nullbyte oder '.'
              break; # ist der Name zu Ende
            ptr++; count++; # weiterrücken
          }
        pushSTACK(n_char_to_string(asciz,count,O(pathname_encoding))); # String für Name erzeugen
        if (*ptr++ == 0) # mit Nullbyte beendet ?
          ; # ja -> Typ bleibt Default
          else
          { asciz = ptr; count = 0;
            until (*ptr++ == 0) { count++; } # bei Nullbyte ist der Typ zu Ende
            STACK_1 = n_char_to_string(asciz,count,O(pathname_encoding)); # String für Typ erzeugen
          }
      }}
  #
  # UP: Sucht Subdirectories eines gegebenen Pathname.
  # subdirs(pathstring)
  # STACK_0 = Pathname, dessen Subdirectories zu suchen sind
  # STACK_1 = Liste, auf die die Pathnames der matchenden Subdirectories
  #           gepusht werden
  # > pathstring: Suchpfad als Simple-String
  # verändert STACK_1, kann GC auslösen
    local void subdirs (object pathstring);
    local void subdirs(pathstring)
      var object pathstring;
      {
       #ifdef MSDOS
        with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz,
          { # Dateisuche gemäß DOS-Konvention:
            READDIR_var_declarations;
            # Suchanfang, suche nach Ordnern und normalen Dateien:
            begin_system_call();
            do {
              READDIR_findfirst(pathstring_asciz,
                                { end_system_call(); OS_file_error(STACK_0); },
                                break; );
              loop
                { # Stackaufbau: new-pathname-list, pathname.
                  end_system_call();
                  # gefundene Datei untersuchen:
                  if (READDIR_entry_ISDIR()) # sollte ein Unterdirectory sein
                    if (!(READDIR_entry_name()[0] == '.')) # sollte nicht mit '.' anfangen
                      # (sonst ist es wohl '.' oder '..', wird übergangen)
                      { # in Name.Typ aufspalten, Default-Typ "" :
                        extract(READDIR_entry_name(),O(leer_string));
                       {var object new_cons = allocate_cons();
                        Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
                        # new_cons = (name . type)
                        pushSTACK(new_cons);
                        new_cons = allocate_cons();
                        Car(new_cons) = popSTACK();
                        # in ein-elementiger Liste new_cons = list1 = ((name . type))
                        pushSTACK(new_cons);
                       }# Stackaufbau: new-pathname-list, pathname, list1.
                        # letzten Pathname kopieren:
                       {var object temp = copy_pathname(STACK_1);
                        pushSTACK(temp);
                        # und darin Directory um list1 = ((name . type)) verlängern:
                        # (append pathname-dir list1) = (nreconc (reverse pathname-dir) list1)
                        temp = reverse(ThePathname(temp)->pathname_directory);
                        temp = nreconc(temp,STACK_1);
                        ThePathname(STACK_0)->pathname_directory = temp;
                       }# Stackaufbau: new-pathname-list, pathname, list1, newpathname.
                        # newpathname auf die Liste new-pathname-list pushen:
                       {var object new_cons = allocate_cons();
                        Car(new_cons) = popSTACK(); skipSTACK(1);
                        Cdr(new_cons) = STACK_1; STACK_1 = new_cons;
                      }}
                  # nächstes File:
                  begin_system_call();
                  READDIR_findnext({ end_system_call(); OS_file_error(STACK_0); }, break; );
                }
              } while (FALSE);
            end_system_call();
            READDIR_end_declarations;
         });
       #endif
      }
  #
  # UP: Sucht alle Subdirectories (beliebiger Tiefe) eines gegebenen Pathname.
  # allsubdirs(pathnamelist)
  # > pathnamelist: Liste, dessen CAR der gegebene Pathname ist.
  # Die Pathnames aller echten Subdirectories (beliebiger Tiefe) werden als
  # Liste destruktiv zwischen pathnamelist und (cdr pathnamelist) gehängt.
  # < ergebnis: das ursprüngliche (cdr pathnamelist)
  # kann GC auslösen
    local object allsubdirs (object pathnamelist);
    local object allsubdirs(pathnamelist)
      var object pathnamelist;
      { pushSTACK(pathnamelist);
        pushSTACK(NIL); # new-pathname-list := NIL
        {var object pathname = Car(pathnamelist);
         pushSTACK(pathname);
         # Stackaufbau: pathnamelist, new-pathname-list, pathname.
         {var uintC stringcount =
            directory_namestring_parts(pathname); # Directory-Namestring-Teile,
          pushSTACK(O(wild_wild_string)); # "*.*"
          {var object pathstring = string_concat(stringcount+1); # zusammenhängen
           subdirs(pathstring); # alle subdirs auf new-pathname-list pushen
        }}}
        skipSTACK(1); # pathname vergessen
        { var object new_pathname_list = popSTACK();
          pathnamelist = popSTACK();
          # Stackaufbau: (leer).
          # Mit  (setf (cdr pathnamelist)
          #            (nreconc new-pathname-list (cdr pathnamelist))
          #      )
          # die new-pathname-list umdrehen und gleichzeitig einhängen:
          new_pathname_list = nreconc(new_pathname_list,Cdr(pathnamelist));
          pushSTACK(Cdr(pathnamelist)); Cdr(pathnamelist) = new_pathname_list;
          pathnamelist = new_pathname_list;
        }
        # Stackaufbau: ursprüngliches (cdr pathnamelist).
        # Liste pathnamelist durchlaufen, bis bei STACK_0 angelangt,
        # und rekursiv alle Subdirectories bestimmen und einhängen:
        until (eq(pathnamelist,STACK_0))
          { pathnamelist = allsubdirs(pathnamelist); }
        skipSTACK(1);
        return pathnamelist;
      }
  #
  # UP: Searches files contained in a given directory.
  # subfiles(pathstring);
  # STACK_0 = pathname whose files are to be searched
  # STACK_1 = list of pathnames on which the results are to be pushed
  # STACK_(0+4) = full-flag
  # > pathstring: path to be searched, a simple-string
  # modifies STACK_1, kann GC auslösen
    local void subfiles (object pathstring);
    local void subfiles(pathstring)
      var object pathstring;
      {
        #ifdef MSDOS
          with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz,
            { # Dateisuche gemäß DOS-Konvention:
              READDIR_var_declarations;
              # Suchanfang, suche nur nach normalen Dateien:
              begin_system_call();
              do {
                READDIR_findfirst(pathstring_asciz,
                                  { end_system_call(); OS_file_error(STACK_0); },
                                  break;
                                 );
                loop
                  { # Stackaufbau: ..., next-pathname.
                    end_system_call();
                    # gefundene Datei untersuchen:
                    if (!READDIR_entry_ISDIR())
                      { # in Name.Typ aufspalten, Default-Typ NIL :
                        extract(READDIR_entry_name(),NIL);
                       {# letzten Pathname kopieren und Name und Typ eintragen:
                        var object new = copy_pathname(STACK_2);
                        ThePathname(new)->pathname_name = popSTACK();
                        ThePathname(new)->pathname_type = popSTACK();
                        # Full-Flag abtesten und evtl. mehr Information besorgen:
                        if (!nullp(STACK_(0+4)))
                          { pushSTACK(new); # newpathname als 1. Listenelement
                            pushSTACK(new); # newpathname als 2. Listenelement
                            { # Uhrzeit und Datum von DOS-Format in Decoded-Time umwandeln:
                              var decoded_time timepoint;
                              READDIR_entry_timedate(&timepoint);
                              pushSTACK(timepoint.Sekunden);
                              pushSTACK(timepoint.Minuten);
                              pushSTACK(timepoint.Stunden);
                              pushSTACK(timepoint.Tag);
                              pushSTACK(timepoint.Monat);
                              pushSTACK(timepoint.Jahr);
                              new = listof(6); # 6-elementige Liste bauen
                            }
                            pushSTACK(new); # als 3. Listenelement
                            pushSTACK(UL_to_I(READDIR_entry_size())); # Länge als 4. Listenelement
                            new = listof(4); # 4-elementige Liste bauen
                          }
                        # new auf die Liste new-pathname-list pushen:
                        pushSTACK(new);
                        {var object new_cons = allocate_cons();
                         Car(new_cons) = popSTACK();
                         Cdr(new_cons) = STACK_1;
                         STACK_1 = new_cons;
                      }}}
                    # nächstes File:
                    begin_system_call();
                    READDIR_findnext({ end_system_call(); OS_file_error(STACK_0); }, break; );
                  }
                } while (FALSE);
              end_system_call();
              READDIR_end_declarations;
            });
        #endif
      }
  #
  local object directory_search(pathname)
    var object pathname;
    { pathname = use_default_dir(pathname); # Default-Directory einfügen
      # pathname ist jetzt ein Pathname, bei dem Device ein überprüfter
      # String ist und Directory kein :RELATIVE, :CURRENT, :PARENT enthält.
      pushSTACK(pathname);
      pushSTACK(ThePathname(pathname)->pathname_directory); # subdir-list
      # pathname kopieren:
      pushSTACK(copy_pathname(pathname));
      # und dessen Directory auf (:ABSOLUTE) verkürzen:
      {var object new_cons = allocate_cons(); # neues Cons mit CDR=NIL
       Car(new_cons) = S(Kabsolute); # :ABSOLUTE als CAR
       ThePathname(STACK_0)->pathname_directory = new_cons;
      }
      # und in einelementige Liste packen:
      {var object new_cons = allocate_cons();
       Car(new_cons) = STACK_0;
       STACK_0 = new_cons;
      }
      while
        # Stackaufbau: pathname, subdir-list, pathname-list.
        # Dabei enthalten die Pathnames aus pathname-list das Directory
        # nur so tief, dass es danach mit (cdr subdir-list) weitergeht.
        # Nächste subdir-Ebene abarbeiten:
        (consp (STACK_1 = Cdr(STACK_1))) # subdir-list verkürzen
        { # pathname-list durchgehen und dabei neue Liste aufbauen:
          pushSTACK(STACK_0); pushSTACK(NIL);
          loop
            { # Stackaufbau: ..., pathname-list-rest, new-pathname-list.
              var object pathname_list_rest = STACK_1;
              if (atomp(pathname_list_rest)) break;
              STACK_1 = Cdr(pathname_list_rest);
             {var object next_pathname = Car(pathname_list_rest); # nächster Pathname
              var object subdir_list = STACK_(1+2);
              pushSTACK(next_pathname); # in den Stack
              if (!eq(Car(subdir_list),S(Kwild_inferiors))) # nächstes subdir = :WILD-INFERIORS ?
                { # normales subdir:
                  var uintC stringcount =
                    directory_namestring_parts(next_pathname); # Directory-Namestring-Teile (keine GC!)
                  stringcount +=
                    subdir_namestring_parts(subdir_list); # und Strings zum nächsten subdir
                 {var object pathstring = string_concat(stringcount); # zusammenhängen
                  subdirs(pathstring); # alle subdirs auf new-pathname-list pushen
                  skipSTACK(1); # next-pathname vergessen
                }}
                else
                { # subdir = :WILD-INFERIORS -> alle Subdirs bestimmen:
                  {var object list1 = allocate_cons();
                   Car(list1) = STACK_0;
                   STACK_0 = list1; # einelementige Liste (next-pathname)
                   allsubdirs(list1); # alle Subdirectories bestimmen
                  }
                  # Liste aller Subdirectories vor new-pathname-list
                  # in umgekehrter Reihenfolge davorhängen:
                  # (nreconc subdirlist new-pathname-list)
                  {var object newsubdirlist = popSTACK();
                   STACK_0 = nreconc(newsubdirlist,STACK_0);
                } }
              # nächsten Pathname aus pathname-list-rest nehmen
            }}
         {var object new_pathname_list = popSTACK(); skipSTACK(1);
          # umdrehen und als nächste pathname-list verwenden:
          STACK_0 = nreverse(new_pathname_list);
        }}
      # Stackaufbau: pathname, nix, pathname-list.
      pathname = STACK_2;
      {var object name = ThePathname(pathname)->pathname_name;
       var object type = ThePathname(pathname)->pathname_type;
       if (nullp(name)) # Name=NIL ?
         { if (nullp(type)) # auch Typ=NIL ?
             { var object new_pathname_list = popSTACK(); # ja ->
               skipSTACK(2); return new_pathname_list; # schon fertig
             }
             else
             # nein -> verwende :WILD (statt NIL) als Name
             { name = S(Kwild); }
         }
       # Alle Files name.type in den gegebenen Subdirectories suchen:
       { var uintC stringcount =
           nametype_namestring_parts(name,type,ThePathname(pathname)->pathname_version); # Teilstrings zu Name und Typ
         var object name_type = string_concat(stringcount);
         STACK_2 = name_type;
      }}
      STACK_1 = STACK_0; # pathname-list
      STACK_0 = NIL; # new-pathname-list := NIL
      # Stackaufbau: name-type, pathname-list, new-pathname-list.
      loop
        { var object pathname_list_rest = STACK_1;
          if (atomp(pathname_list_rest)) break;
          STACK_1 = Cdr(pathname_list_rest);
         {var object next_pathname = Car(pathname_list_rest); # nächster Pathname
          var object name_type = STACK_2;
          pushSTACK(next_pathname); # in den Stack
          {var uintC stringcount =
             directory_namestring_parts(next_pathname); # Directory-Namestring-Teile (keine GC!)
           pushSTACK(name_type); stringcount += 1; # und name-type
           {var object pathstring = string_concat(stringcount); # zusammenhängen
            subfiles(pathstring);
          }}
          skipSTACK(1); # next-pathname vergessen
        }}
      {# new-pathname-list wieder umdrehen:
       var object new_pathname_list = nreverse(popSTACK());
       skipSTACK(2); return new_pathname_list;
    } }
  #
  #endif # PATHNAME_EXT83
  #
  #if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
  #
  # UP: Erweitert das Directory eines Pathname um eine Komponente.
  # > STACK_1: ein Pathname
  # > STACK_0: neue Subdir-Komponente, ein Simple-String
  # < ergebnis: neuer Pathname mit um subdir verlängertem Directory
  # Erhöht STACK um 2
  # kann GC auslösen
  local object pathname_add_subdir (void);
  local object pathname_add_subdir()
    { # Pathname kopieren und dessen Directory gemäß
      # (append x (list y)) = (nreverse (cons y (reverse x))) verlängern:
      var object pathname = copy_pathname(STACK_1);
      STACK_1 = pathname;
      pushSTACK(reverse(ThePathname(pathname)->pathname_directory));
     {var object new_cons = allocate_cons();
      Cdr(new_cons) = popSTACK();
      Car(new_cons) = popSTACK();
      new_cons = nreverse(new_cons);
      pathname = popSTACK();
      ThePathname(pathname)->pathname_directory = new_cons;
      return pathname;
    }}
  #
  #if defined(UNIX) || defined(AMIGAOS) || defined(RISCOS)
  # UP: Erweitert einen Pathname um die File-Information.
  # > STACK_1: absoluter Pathname
  # > STACK_0: absoluter Pathname, Links aufgelöst
  # > *filestatus: dessen stat-Info
  # < STACK_0: Liste (Pathname Truename Write-Date Length [Kommentar]) im :FULL-Format
  local void with_stat_info (void);
  local void with_stat_info()
    { var object newlist;
      #if defined(UNIX) || defined(RISCOS)
      var uintL size = filestatus->st_size;
      #endif
      #ifdef AMIGAOS
      var uintL size = filestatus->fib_Size;
      #endif
      # Pathname schon in STACK_1, als 1. Listenelement
      # Truename schon in STACK_0, als 2. Listenelement
      { var decoded_time timepoint; # Write-Date in decodierter Form
        #if defined(UNIX) || defined(RISCOS)
        convert_time(&filestatus->st_mtime,&timepoint);
        #endif
        #ifdef AMIGAOS
        convert_time(&filestatus->fib_Date,&timepoint);
        #endif
        pushSTACK(timepoint.Sekunden);
        pushSTACK(timepoint.Minuten);
        pushSTACK(timepoint.Stunden);
        pushSTACK(timepoint.Tag);
        pushSTACK(timepoint.Monat);
        pushSTACK(timepoint.Jahr);
        newlist = listof(6); # 6-elementige Liste bauen
      }
      pushSTACK(newlist); # als 3. Listenelement
      pushSTACK(UL_to_I(size)); # Länge als 4. Listenelement
      #if defined(UNIX) || defined(RISCOS)
      newlist = listof(4); # 4-elementige Liste bauen
      #endif
      #ifdef AMIGAOS
      pushSTACK(asciz_to_string(&filestatus->fib_Comment[0],O(pathname_encoding))); # Kommentar als 5. Listenelement
      newlist = listof(5); # 5-elementige Liste bauen
      #endif
      pushSTACK(Car(newlist)); # pathname wieder in den Stack
      pushSTACK(newlist); # Liste in den Stack
    }
  #endif
  #
  # Check whether a directory exists.
  # check_stat_directory(namestring_asciz,error_statement,exists_statement);
  # If the directory does not exist or is a file, does nothing.
  #if defined(UNIX) || defined(RISCOS)
    #define check_stat_directory(namestring_asciz,error_statement,exists_statement)  \
      { var struct stat status;                                  \
        begin_system_call();                                     \
        if (!( stat(namestring_asciz,&status) ==0))              \
          { if (!(errno==ENOENT))                                \
              { end_system_call(); error_statement }             \
              else                                               \
              # Subdirectory existiert nicht -> OK.              \
              { end_system_call(); }                             \
          }                                                      \
          else                                                   \
          # File existiert.                                      \
          { end_system_call();                                   \
            if (S_ISDIR(status.st_mode)) # Ist es ein Directory? \
              { exists_statement }                               \
      }   }
  #endif
  #ifdef AMIGAOS
    #define check_stat_directory(namestring_asciz,error_statement,exists_statement)  \
      { var LONGALIGNTYPE(struct FileInfoBlock) fib;                       \
        var struct FileInfoBlock * fibptr = LONGALIGN(&fib);               \
        set_break_sem_4();                                                 \
        begin_system_call();                                               \
       {var BPTR lock = Lock(namestring_asciz,ACCESS_READ);                \
        if (lock==BPTR_NULL)                                               \
          { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND))                        \
              { end_system_call(); clr_break_sem_4(); error_statement }    \
              else                                                         \
              # Subdirectory existiert nicht -> OK.                        \
              { end_system_call(); clr_break_sem_4(); }                    \
          }                                                                \
          else                                                             \
          # File existiert.                                                \
          { if (! Examine(lock,fibptr) )                                   \
              { UnLock(lock);                                              \
                end_system_call(); clr_break_sem_4();                      \
                error_statement                                            \
              }                                                            \
              else                                                         \
              { UnLock(lock);                                              \
                end_system_call(); clr_break_sem_4();                      \
                if (fibptr->fib_DirEntryType >= 0) # Ist es ein Directory? \
                  { exists_statement }                                     \
      }}  }   }
  #endif
  #ifdef WIN32_NATIVE
    #define check_stat_directory(namestring_asciz,error_statement,exists_statement)  \
      { var DWORD fileattr;                                                  \
        begin_system_call();                                                 \
        fileattr = GetFileAttributes(namestring_asciz);                      \
        if (fileattr == 0xFFFFFFFF)                                          \
          { if (!(GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND)) \
              { end_system_call(); error_statement }                         \
              else                                                           \
              # Subdirectory existiert nicht -> OK.                          \
              { end_system_call(); }                                         \
          }                                                                  \
          else                                                               \
          # File existiert.                                                  \
          { end_system_call();                                               \
            if (fileattr & FILE_ATTRIBUTE_DIRECTORY) # Ist es ein Directory? \
              { exists_statement }                                           \
      }   }
  #endif
  #
  #ifndef MSDOS
  # Search for a subdirectory with a given name.
  # directory_search_1subdir(subdir,namestring);
  # > STACK_0 = pathname
  # > STACK_(3+1) = new-pathname-list
  # > subdir: the new directory component to add to the pathname, if it exists
  # > namestring: the namestring (for the OS)
  # < STACK_0: replaced
  # < STACK_(3+1): augmented
  # kann GC auslösen
  local void directory_search_1subdir (object subdir, object namestring);
  local void directory_search_1subdir(subdir,namestring)
    var object subdir;
    var object namestring;
    { with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
        { check_stat_directory(namestring_asciz,
                               { OS_file_error(STACK_0); },
            { # pathname kopieren und dessen Directory um subdir verlängern:
              pushSTACK(subdir);
             {var object pathname = pathname_add_subdir();
              pushSTACK(pathname);
             }# Diesen neuen Pathname vor new-pathname-list pushen:
             {var object new_cons = allocate_cons();
              Car(new_cons) = STACK_0;
              Cdr(new_cons) = STACK_(3+1);
              STACK_(3+1) = new_cons;
            }});
        });
    }
  #endif
  #
  #ifdef UNIX
  # Returns a truename dependent hash code for a directory.
  # directory_search_hashcode()
  # STACK_0 = dir_namestring
  # STACK_1 = pathname
  # < result: a hash code, or nullobj if the directory does not exist
  # kann GC auslösen
  local object directory_search_hashcode (void);
  local object directory_search_hashcode()
    { pushSTACK(STACK_0); # Directory-Name
      pushSTACK(O(punkt_string)); # und "."
     {var object namestring = string_concat(2); # zusammenhängen
      var struct stat status;
      with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
        { begin_system_call();
          if (!( stat(namestring_asciz,&status) ==0)) # Information holen
            { if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_1); }
              end_system_call();
              FREE_DYNAMIC_ARRAY(namestring_asciz);
              return nullobj;
            }
          end_system_call();
        });
      # Eintrag existiert (welch Wunder...)
      pushSTACK(UL_to_I(status.st_dev)); # Device-Nummer und
      pushSTACK(UL_to_I(status.st_ino)); # Inode-Nummer
      {var object new_cons = allocate_cons(); # zusammenconsen
       Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
       return new_cons;
    }}}
  #endif
  #
  #if defined(UNIX) || defined(RISCOS)
  # Tests whether a directory entry actually exists.
  # (It could be a link pointing to nowhere, or an undesired directory.)
  # directory_search_direntry_ok(namestring,&statbuf)
  # STACK_2 = pathname
  # < result: TRUE and statbuf filled, or FALSE.
  local boolean directory_search_direntry_ok (object namestring, struct stat * statbuf);
  local boolean directory_search_direntry_ok(namestring,statbuf)
    var object namestring;
    var struct stat * statbuf;
    { var boolean exists = TRUE;
      with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
        { begin_system_call();
          if (!( stat_for_search(namestring_asciz,statbuf) ==0))
            { if (!((errno==ENOENT) || (errno==ELOOP_VALUE)))
                { end_system_call(); OS_file_error(STACK_2); }
              end_system_call();
              exists = FALSE;
            }
          end_system_call();
        });
      return exists;
    }
  #endif
  #
  # Scans an entire directory.
  # directory_search_scandir(recursively,next_task);
  # Stackaufbau: result-list, pathname, name&type, subdir-list, pathname-list,
  #              new-pathname-list, ht, pathname-list-rest, pathnames-to-insert,
  #              pathname, dir_namestring.
  local void directory_search_scandir (boolean recursively, signean next_task);
  local void directory_search_scandir(recursively,next_task)
    var boolean recursively;
    var signean next_task;
    {
      #if defined(UNIX) || defined(RISCOS)
        { var object namestring;
          #ifdef UNIX
          pushSTACK(STACK_0); # Directory-Name
          pushSTACK(O(punkt_string)); # und "."
          namestring = string_concat(2); # zusammenhängen
          #endif
          #ifdef RISCOS
          var object wildcard_mask;
          namestring = STACK_0; # Directory-Name
          namestring = subsstring(namestring,0,Sstring_length(namestring)-1); # Directory-Name ohne '.' am Schluss
          # Statt wildcard_match() selber aufzurufen, überlassen wir das dem Betriebssystem:
          wildcard_mask = (next_task<0 ? Car(STACK_(1+4+3)) : STACK_(2+4+3));
          #endif
          # Directory absuchen:
         {var DIR* dirp;
          set_break_sem_4();
          #ifdef UNIX
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { begin_system_call();
              dirp = opendir(namestring_asciz); # Directory öffnen
              end_system_call();
            });
          #endif
          #ifdef RISCOS
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            with_sstring_0(wildcard_mask,O(pathname_encoding),wildcard_mask_asciz,
              { # In wildcard_mask_asciz die Wildchar-Characters '?' ins synonyme '#' umwandeln:
                { var uintB* ptr = wildcard_mask_asciz;
                  while (*ptr != '\0') { if (*ptr == '?') { *ptr = '#'; } ptr++; }
                }
                begin_system_call();
                dirp = opendir(namestring_asciz,wildcard_mask_asciz); # Directory zum Suchen öffnen
                end_system_call();
              }););
          #endif
          if (dirp == (DIR*)NULL) { OS_file_error(STACK_1); }
          loop
            { var SDIRENT* dp;
              begin_system_call();
              errno = 0;
              dp = readdir(dirp); # nächsten Directory-Eintrag holen
              if (dp == (SDIRENT*)NULL) # Error oder Directory zu Ende
                { if (!(errno==0)) { end_system_call(); OS_file_error(STACK_1); }
                  end_system_call();
                  break;
                }
              end_system_call();
              # Directory-Eintrag in String umwandeln:
             {var object direntry;
              {var uintL direntry_len;
               #if defined(UNIX_CYGWIN32)
               # Neither d_reclen nor d_namlen present in DIR structure.
               direntry_len = asciz_length(dp->d_name);
               #elif defined(DIRENT_WITHOUT_NAMLEN) || defined(__USE_GNU)
               # Unter UNIX_LINUX reichte früher direntry_len := dp->d_reclen, aber
               # i.a. ist direntry_len := min(dp->d_reclen,asciz_length(dp->d_name))
               # nötig. Die GNU libc ist buggy: macht "#define d_namlen d_reclen",
               # ebenso die Linux libc-5.0.9.
               {var const uintB* ptr = (const uintB*)(&dp->d_name[0]);
                var uintL count;
                direntry_len = 0;
                dotimesL(count,dp->d_reclen,
                  { if (*ptr == '\0') break;
                    ptr++; direntry_len++;
                  });
               }
               #else
               direntry_len = dp->d_namlen;
               #endif
               direntry = n_char_to_string(&dp->d_name[0],direntry_len,O(pathname_encoding));
              }
              #ifndef RISCOS
              # "." und ".." übergehen:
              if (!(equal(direntry,O(punkt_string))
                    || equal(direntry,O(punktpunkt_string))
                 ) )
              #endif
                { pushSTACK(direntry);
                  # Stackaufbau: ..., pathname, dir_namestring, direntry.
                  # Feststellen, ob es ein Directory oder ein File ist:
                  pushSTACK(STACK_1); # Directory-Namestring
                  pushSTACK(direntry); # direntry
                 {var object namestring = string_concat(2); # zusammenhängen
                  # Information holen:
                  var struct stat status;
                  #if 1 # just an optimization
                  if (!recursively)
                    { # Try to avoid calling directory_search_direntry_ok(),
                      # since it is an expensive operation (it calls stat()).
                      if (next_task < 0)
                        {
                          #ifndef RISCOS
                          # (car subdir-list) mit direntry matchen:
                          if (wildcard_match(Car(STACK_(1+4+3)),STACK_0))
                          #endif
                            if (directory_search_direntry_ok(namestring,&status))
                              if (S_ISDIR(status.st_mode))
                                goto push_matching_subdir;
                        }
                      elif (next_task > 0)
                        {
                          #ifndef RISCOS
                          # name&type mit direntry matchen:
                          if (wildcard_match(STACK_(2+4+3),STACK_0))
                          #endif
                            if (directory_search_direntry_ok(namestring,&status))
                              if (!S_ISDIR(status.st_mode))
                                goto push_matching_file;
                        }
                      goto done_direntry;
                    }
                  #endif
                  if (directory_search_direntry_ok(namestring,&status))
                    { # Eintrag existiert und ist nicht unerwünscht.
                      if (S_ISDIR(status.st_mode)) # Ist es ein Directory?
                        # Eintrag ist ein Directory.
                        { if (recursively) # alle rekursiven Subdirectories gewünscht?
                            # ja -> zu einem Pathname machen und auf
                            # pathnames-to-insert pushen (wird nachher
                            # vor pathname-list-rest eingefügt):
                            { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                             {var object pathname = pathname_add_subdir();
                              pushSTACK(pathname);
                             }# Diesen neuen Pathname vor pathname-to-insert pushen:
                             {var object new_cons = allocate_cons();
                              Car(new_cons) = popSTACK();
                              Cdr(new_cons) = STACK_(0+3);
                              STACK_(0+3) = new_cons;
                            }}
                          if (next_task<0)
                            {
                              #ifndef RISCOS
                              # (car subdir-list) mit direntry matchen:
                              if (wildcard_match(Car(STACK_(1+4+3)),STACK_0))
                              #endif
                                push_matching_subdir:
                                # Subdirectory matcht -> zu einem Pathname
                                # machen und auf new-pathname-list pushen:
                                { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                 {var object pathname = pathname_add_subdir();
                                  pushSTACK(pathname);
                                 }# Diesen neuen Pathname vor new-pathname-list pushen:
                                 {var object new_cons = allocate_cons();
                                  Car(new_cons) = popSTACK();
                                  Cdr(new_cons) = STACK_(3+3);
                                  STACK_(3+3) = new_cons;
                            }   }}
                        }
                        else
                        # Eintrag ist ein (halbwegs) normales File.
                        { if (next_task>0)
                            {
                              #ifndef RISCOS
                              # name&type mit direntry matchen:
                              if (wildcard_match(STACK_(2+4+3),STACK_0))
                              #endif
                                push_matching_file:
                                # File matcht -> zu einem Pathname machen
                                # und auf result-list pushen:
                                {
                                  #ifndef PATHNAME_RISCOS
                                  pushSTACK(STACK_0); # direntry
                                  split_name_type(1); # in Name und Typ aufspalten
                                  {var object pathname = copy_pathname(STACK_(2+2));
                                   ThePathname(pathname)->pathname_type = popSTACK(); # Typ einsetzen
                                   ThePathname(pathname)->pathname_name = popSTACK(); # Name einsetzen
                                   pushSTACK(pathname);
                                   pushSTACK(pathname);
                                  }
                                  #else # PATHNAME_RISCOS
                                  {var object pathname = copy_pathname(STACK_2);
                                   pushSTACK(pathname);
                                   if (name_and_type && nullp(ThePathname(pathname)->pathname_type))
                                     # Move the last subdir into the type slot of the pathname.
                                     { # subdirs := (butlast subdirs) = (nreverse (cdr (reverse subdirs)))
                                       var object subdirs = reverse(ThePathname(pathname)->pathname_directory);
                                       pathname = STACK_0;
                                       ThePathname(pathname)->pathname_type = Car(subdirs);
                                       ThePathname(pathname)->pathname_directory = nreverse(Cdr(subdirs));
                                     }
                                   ThePathname(pathname)->pathname_name = STACK_1; # direntry
                                   pushSTACK(pathname);
                                  }
                                  #endif
                                  # Truename bilden (symbolische Links auflösen):
                                  assure_dir_exists(TRUE,FALSE);
                                  if (file_exists(_EMA_)) # falls File (immer noch...) existiert
                                    { if (!nullp(STACK_(0+5+4+3+2))) # :FULL gewünscht?
                                        with_stat_info(); # ja -> STACK_0 erweitern
                                      # und STACK_0 vor result-list pushen:
                                     {var object new_cons = allocate_cons();
                                      Car(new_cons) = STACK_0;
                                      Cdr(new_cons) = STACK_(4+4+3+2);
                                      STACK_(4+4+3+2) = new_cons;
                                    }}
                                  skipSTACK(2);
                            }   }
                    }   }
                  done_direntry:
                  skipSTACK(1); # direntry vergessen
            }}  }}
          begin_system_call();
          if (CLOSEDIR(dirp)) { end_system_call(); OS_file_error(STACK_1); }
          end_system_call();
          clr_break_sem_4();
        }}
      #endif
      #ifdef AMIGAOS
        # Directory absuchen:
        { var object namestring = OSdirnamestring(STACK_0);
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { set_break_sem_4();
              begin_system_call();
             {var BPTR lock = Lock(namestring_asciz,ACCESS_READ);
              var LONGALIGNTYPE(struct FileInfoBlock) fib;
              var struct FileInfoBlock * fibptr = LONGALIGN(&fib);
              if (lock==BPTR_NULL)
                { end_system_call(); OS_file_error(STACK_1); }
              if (! Examine(lock,fibptr) )
                { UnLock(lock); end_system_call(); OS_file_error(STACK_1); }
              loop
                { if (! ExNext(lock,fibptr) ) # Error oder Directory zu Ende?
                    break;
                  end_system_call();
                  # Directory-Eintrag in String umwandeln:
                 {var object direntry = asciz_to_string(&fibptr->fib_FileName[0],O(pathname_encoding));
                  pushSTACK(direntry);
                  # Stackaufbau: ..., pathname, dir_namestring, direntry.
                  # Feststellen, ob es ein Directory oder ein File ist:
                  {var boolean isdir;
                   if (fibptr->fib_DirEntryType == ST_SOFTLINK)
                     { # Einen Lock auf das Ziel holen und Examine ausführen:
                       # das geht, denn Lock() löst Links auf (bzw. versucht es)
                       pushSTACK(STACK_1); pushSTACK(STACK_(0+1));
                      {var object direntry_namestring = string_concat(2);
                       with_sstring_0(direntry_namestring,O(pathname_encoding),direntry_namestring_asciz,
                         {  begin_system_call();
                            # TODO olddir = CurrentDir(lock); ... CurrentDir(olddir)
                          { var BPTR direntry_lock = Lock(direntry_namestring_asciz,ACCESS_READ);
                            if (direntry_lock==BPTR_NULL)
                              switch (IoErr())
                                { case ERROR_OBJECT_NOT_FOUND:
                                  case ERROR_DEVICE_NOT_MOUNTED: # user canceled requester
                                    end_system_call();
                                    FREE_DYNAMIC_ARRAY(direntry_namestring_asciz);
                                    goto skip_direntry; # direntry vergessen und ignorieren
                                  default:
                                    end_system_call(); OS_file_error(STACK_2);
                                }
                           {var LONGALIGNTYPE(struct FileInfoBlock) direntry_status;
                            var struct FileInfoBlock * statusptr = LONGALIGN(&direntry_status);
                            if (! Examine(direntry_lock,statusptr) )
                              { UnLock(direntry_lock); end_system_call(); OS_file_error(STACK_2); }
                            UnLock(direntry_lock);
                            end_system_call();
                            isdir = (statusptr->fib_DirEntryType >= 0);
                         }}});
                     }}
                     else
                     { isdir = (fibptr->fib_DirEntryType >= 0); }
                   if (isdir)
                     # Eintrag ist ein Directory.
                     { if (recursively) # alle rekursiven Subdirectories gewünscht?
                         # ja -> zu einem Pathname machen und auf
                         # pathnames-to-insert pushen (wird nachher
                         # vor pathname-list-rest eingefügt):
                         { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                          {var object pathname = pathname_add_subdir();
                           pushSTACK(pathname);
                          }# Diesen neuen Pathname vor pathname-to-insert pushen:
                          {var object new_cons = allocate_cons();
                           Car(new_cons) = popSTACK();
                           Cdr(new_cons) = STACK_(0+3);
                           STACK_(0+3) = new_cons;
                         }}
                       if (next_task<0)
                         { # (car subdir-list) mit direntry matchen:
                           if (wildcard_match(Car(STACK_(1+4+3)),STACK_0))
                             # Subdirectory matcht -> zu einem Pathname
                             # machen und auf new-pathname-list pushen:
                             { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                              {var object pathname = pathname_add_subdir();
                               pushSTACK(pathname);
                              }# Diesen neuen Pathname vor new-pathname-list pushen:
                              {var object new_cons = allocate_cons();
                               Car(new_cons) = popSTACK();
                               Cdr(new_cons) = STACK_(3+3);
                               STACK_(3+3) = new_cons;
                         }   }}
                     }
                     else
                     # Eintrag ist ein (halbwegs) normales File.
                     { if (next_task>0)
                         { # name&type mit direntry matchen:
                           if (wildcard_match(STACK_(2+4+3),STACK_0))
                             # File matcht -> zu einem Pathname machen
                             # und auf result-list pushen:
                             { pushSTACK(STACK_0); # direntry
                               split_name_type(1); # in Name und Typ aufspalten
                              {var object pathname = copy_pathname(STACK_(2+2));
                               ThePathname(pathname)->pathname_type = popSTACK(); # Typ einsetzen
                               ThePathname(pathname)->pathname_name = popSTACK(); # Name einsetzen
                               pushSTACK(pathname);
                               pushSTACK(pathname);
                              }
                              assure_dir_exists(TRUE,FALSE); # Truename bilden (symbolische Links auflösen)
                              { if (!nullp(STACK_(0+5+4+3+2))) # :FULL gewünscht?
                                  with_stat_info(); # ja -> STACK_0 erweitern
                                # und STACK_0 vor result-list pushen:
                               {var object new_cons = allocate_cons();
                                Car(new_cons) = STACK_0;
                                Cdr(new_cons) = STACK_(4+4+3+2);
                                STACK_(4+4+3+2) = new_cons;
                              }}
                              skipSTACK(2);
                     }   }   }
                  }
                  skip_direntry:
                  skipSTACK(1); # direntry vergessen
                  begin_system_call();
                }}
              UnLock(lock);
              if (!(IoErr()==ERROR_NO_MORE_ENTRIES))
                { end_system_call(); OS_file_error(STACK_1); }
              end_system_call();
              clr_break_sem_4();
            }});
        }
      #endif
      #if defined(MSDOS) || defined(WIN32_NATIVE)
        pushSTACK(STACK_0); # Directory-Name
        pushSTACK(READDIR_wildnametype_suffix); # und "*.*" bzw. "*"
        { var object namestring = string_concat(2); # zusammenhängen
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { # Directory absuchen, gemäß DOS-Konvention bzw. Win32-Konvention:
              READDIR_var_declarations;
              # Suchanfang, suche nach Ordnern und normalen Dateien:
              begin_system_call();
              do {
                READDIR_findfirst(namestring_asciz,
                                  { end_system_call(); OS_file_error(STACK_1); },
                                  break; );
                loop
                  { end_system_call();
                   {# Directory-Eintrag in String umwandeln:
                    var object direntry = asciz_to_string(READDIR_entry_name(),O(pathname_encoding));
                    # "." und ".." übergehen:
                    if (!(equal(direntry,O(punkt_string))
                          || equal(direntry,O(punktpunkt_string))
                       ) )
                      { pushSTACK(direntry);
                        # Stackaufbau: ..., pathname, dir_namestring, direntry.
                        if (READDIR_entry_ISDIR()) # Ist es ein Directory?
                          # Eintrag ist ein Directory.
                          { if (recursively) # alle rekursiven Subdirectories gewünscht?
                              # ja -> zu einem Pathname machen und auf
                              # pathnames-to-insert pushen (wird nachher
                              # vor pathname-list-rest eingefügt):
                              { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                               {var object pathname = pathname_add_subdir();
                                pushSTACK(pathname);
                               }# Diesen neuen Pathname vor pathname-to-insert pushen:
                               {var object new_cons = allocate_cons();
                                Car(new_cons) = popSTACK();
                                Cdr(new_cons) = STACK_(0+3);
                                STACK_(0+3) = new_cons;
                              }}
                            if (next_task<0)
                              { # (car subdir-list) mit direntry matchen:
                                if (wildcard_match(Car(STACK_(1+4+3)),STACK_0))
                                  # Subdirectory matcht -> zu einem Pathname
                                  # machen und auf new-pathname-list pushen:
                                  { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                   {var object pathname = pathname_add_subdir();
                                    pushSTACK(pathname);
                                   }# Diesen neuen Pathname vor new-pathname-list pushen:
                                   {var object new_cons = allocate_cons();
                                    Car(new_cons) = popSTACK();
                                    Cdr(new_cons) = STACK_(3+3);
                                    STACK_(3+3) = new_cons;
                              }   }}
                          }
                          else
                          # Eintrag ist ein (halbwegs) normales File.
                          { if (next_task>0)
                              { # name&type mit direntry matchen:
                                if (wildcard_match(STACK_(2+4+3),STACK_0))
                                  # File matcht -> zu einem Pathname machen
                                  # und auf result-list pushen:
                                  { pushSTACK(STACK_0); # direntry
                                    split_name_type(1); # in Name und Typ aufspalten
                                   {var object new = copy_pathname(STACK_(2+2));
                                    ThePathname(new)->pathname_type = popSTACK(); # Typ einsetzen
                                    ThePathname(new)->pathname_name = popSTACK(); # Name einsetzen
                                    # Full-Flag abtesten und evtl. mehr Information besorgen:
                                    if (!nullp(STACK_(0+5+4+3))) # :FULL gewünscht?
                                      { pushSTACK(new); # newpathname als 1. Listenelement
                                        pushSTACK(new); # newpathname als 2. Listenelement
                                        { # Uhrzeit und Datum von DOS-Format in Decoded-Time umwandeln:
                                          var decoded_time timepoint;
                                          READDIR_entry_timedate(&timepoint);
                                          pushSTACK(timepoint.Sekunden);
                                          pushSTACK(timepoint.Minuten);
                                          pushSTACK(timepoint.Stunden);
                                          pushSTACK(timepoint.Tag);
                                          pushSTACK(timepoint.Monat);
                                          pushSTACK(timepoint.Jahr);
                                          new = listof(6); # 6-elementige Liste bauen
                                        }
                                        pushSTACK(new); # als 3. Listenelement
                                        pushSTACK(UL_to_I(READDIR_entry_size())); # Länge als 4. Listenelement
                                        new = listof(4); # 4-elementige Liste bauen
                                      }
                                    pushSTACK(new);
                                   }# und STACK_0 vor result-list pushen:
                                   {var object new_cons = allocate_cons();
                                    Car(new_cons) = popSTACK();
                                    Cdr(new_cons) = STACK_(4+4+3);
                                    STACK_(4+4+3) = new_cons;
                          }   }   }}
                        skipSTACK(1); # direntry vergessen
                      }
                    # nächstes File:
                    begin_system_call();
                    READDIR_findnext({ end_system_call(); OS_file_error(STACK_1); }, break; );
                  }}
                } while (FALSE);
              end_system_call();
              READDIR_end_declarations;
            });
        }
      #endif
    }
  #
  local object directory_search(pathname)
    var object pathname;
    {
      #ifdef PATHNAME_RISCOS
      # If we search for a file with type /= NIL, we have to interpret the last
      # subdir as the type.
      var boolean name_and_type = FALSE;
      # We need to remember if the type is wild, since this controls whether
      # found files are legal.
      var boolean type_is_wild = FALSE;
      #endif
      pathname = use_default_dir(pathname); # Default-Directory einfügen
      # pathname ist jetzt neu und ein absoluter Pathname.
      pushSTACK(NIL); # result-list := NIL
      pushSTACK(pathname);
      # Falls name=NIL und type/=NIL: Setze name := "*".
      if (nullp(ThePathname(pathname)->pathname_name)
          && !nullp(ThePathname(pathname)->pathname_type)
         )
        { ThePathname(pathname)->pathname_name = O(wild_string); }
      #ifdef PATHNAME_RISCOS
      # If the name and type are both set, then make the type part of
      # the directory specification and set the new type to NIL.
      if (!nullp(ThePathname(pathname)->pathname_name)
          && !nullp(ThePathname(pathname)->pathname_type)
         )
        { name_and_type = TRUE;
          type_is_wild = has_wildcards(ThePathname(pathname)->pathname_type);
          pushSTACK(pathname); pushSTACK(ThePathname(pathname)->pathname_type);
          STACK_0 = pathname = pathname_add_subdir();
          ThePathname(pathname)->pathname_type = NIL;
        }
      #endif
      # Zum Matchen: Name und Typ zu einem String zusammenfassen:
      if (nullp(ThePathname(pathname)->pathname_name))
        { pushSTACK(NIL); } # name=NIL -> auch type=NIL -> keine Files suchen
        else
        {var uintC stringcount = file_namestring_parts(pathname);
         var object nametype_string = string_concat(stringcount);
         pathname = STACK_0;
         pushSTACK(nametype_string);
        }
      pushSTACK(ThePathname(pathname)->pathname_directory); # subdir-list
      #ifdef PATHNAME_RISCOS
      STACK_0 = Cdr(STACK_0); # Liste fängt mit (:ABSOLUTE :ROOT ...) an, verkürze sie
      #endif
      # pathname kopieren und dabei Name und Typ streichen und
      # Directory zu (:ABSOLUTE) bzw. (:ABSOLUTE :ROOT) verkürzen:
      pathname = copy_pathname(pathname);
      ThePathname(pathname)->pathname_name = NIL;
      ThePathname(pathname)->pathname_type = NIL;
      ThePathname(pathname)->pathname_directory = O(directory_absolute);
      pushSTACK(pathname);
      # und in einelementige Liste packen:
      {var object new_cons = allocate_cons();
       Car(new_cons) = STACK_0;
       STACK_0 = new_cons;
      }
     {var boolean recursively = # Flag, ob die nächste Operation auf
        FALSE;                  # alle Subdirectories anzuwenden ist.
      loop
        # Stackaufbau: result-list, pathname, name&type, subdir-list, pathname-list.
        # result-list = Liste der fertigen Pathnames/Listen, umgedreht.
        # name&type = NIL oder Normal-Simple-String, gegen den die Filenamen zu matchen sind.
        # pathname-list = Liste der noch abzuarbeitenden Directories.
        # Dabei enthalten die Pathnames aus pathname-list das Directory
        # nur so tief, dass es danach mit (cdr subdir-list) weitergeht.
        { # Nächste subdir-Ebene abarbeiten:
          STACK_1 = Cdr(STACK_1); # subdir-list verkürzen
         {var signean next_task; # Was mit den Dirs aus pathname-list zu tun ist:
            # 0: nichts, fertig
            # 1: nach einem File gegebenen Namens/Typs sehen
            # -1: nach einem Subdirectory gegebenen Namens sehen
            # 2: nach allen Files suchen, die gegebenen Namen/Typ matchen
            # -2: nach allen Subdirectories suchen, die gegebenen Namen matchen
          if (matomp(STACK_1)) # subdir-list zu Ende?
            { var object nametype = STACK_2;
              if (nullp(nametype)) # name=NIL und type=NIL -> keine Files suchen
                { next_task = 0; }
              #if !(defined(MSDOS) || defined(WIN32_NATIVE))
              elif (!has_wildcards(nametype))
                   # === !(has_wildcards(name) || ((!nullp(type)) && has_wildcards(type)))
                { next_task = 1; } # File suchen
              #endif
              else
                { next_task = 2; } # Files mit Wildcards suchen
            }
            else
            { var object next_subdir = Car(STACK_1);
              if (eq(next_subdir,S(Kwild_inferiors))) # '...' ?
                # wird erst beim nächsten Durchlauf behandelt
                { recursively = TRUE; goto passed_subdir; }
              #ifndef MSDOS
              if (
                  #ifdef PATHNAME_AMIGAOS
                  eq(next_subdir,S(Kparent)) ||
                  #endif
                  #ifdef PATHNAME_RISCOS
                  !simple_string_p(next_subdir) ||
                  #endif
                  !has_wildcards(next_subdir)
                 )
                { next_task = -1; } # Subdir suchen
                else
              #endif
                { next_task = -2; } # Subdirs mit Wildcards suchen
            }
          # pathname-list durchgehen und dabei neue Liste aufbauen:
          pushSTACK(NIL);
          #ifdef UNIX
          if (!nullp(STACK_(1+5+1))) # :CIRCLE-Flag abfragen
            { # Hash-Tabelle aller bisher abgesuchten Directories (jeweils
              # als Cons (dev . ino)) führen:
              pushSTACK(S(Ktest)); pushSTACK(S(equal));
              funcall(L(make_hash_table),2); # (MAKE-HASH-TABLE :TEST 'EQUAL)
              pushSTACK(value1);
            }
            else
          #endif
            { pushSTACK(NIL); }
          pushSTACK(STACK_(0+2));
          loop
            { # Stackaufbau: ..., new-pathname-list, ht, pathname-list-rest.
              var object pathname_list_rest = STACK_0;
              if (atomp(pathname_list_rest)) break;
              STACK_0 = Cdr(pathname_list_rest); # Liste verkürzen
              pushSTACK(NIL); # pathnames-to-insert := NIL
              # Stackaufbau: ..., new-pathname-list, ht, pathname-list-rest, pathnames-to-insert.
             {var object pathname = Car(pathname_list_rest); # nächstes Directory
              pushSTACK(pathname); # in den Stack
              # Versuche, die Task ein wenig abzukürzen:
              if (!recursively)
                { switch (next_task)
                    { case 0: # Dieses pathname liefern
                        #ifdef UNIX
                        assure_dir_exists(FALSE,FALSE); # erst noch Links auflösen
                        #endif
                        # und STACK_0 vor result-list pushen:
                        {var object new_cons = allocate_cons();
                         Car(new_cons) = popSTACK();
                         Cdr(new_cons) = STACK_(4+4);
                         STACK_(4+4) = new_cons;
                        }
                        goto next_pathname;
                      #if !(defined(MSDOS) || defined(WIN32_NATIVE))
                      case 1: # In diesem pathname nach einem File sehen
                        ThePathname(pathname)->pathname_name = # Name (/=NIL) einsetzen
                          ThePathname(STACK_(3+4+1))->pathname_name;
                        ThePathname(pathname)->pathname_type = # Typ einsetzen
                          ThePathname(STACK_(3+4+1))->pathname_type;
                        pushSTACK(pathname);
                        #ifdef PATHNAME_RISCOS
                        if (name_and_type && nullp(ThePathname(pathname)->pathname_type))
                          # Move the last subdir into the type slot of the pathname.
                          { # subdirs := (butlast subdirs) = (nreverse (cdr (reverse subdirs)))
                            var object subdirs = reverse(ThePathname(pathname)->pathname_directory);
                            pathname = STACK_0;
                            ThePathname(pathname)->pathname_type = Car(subdirs);
                            ThePathname(pathname)->pathname_directory = nreverse(Cdr(subdirs));
                          }
                        assure_dir_exists_(TRUE,FALSE,type_is_wild); # Links auflösen, File suchen
                        if (file_exists(_EMA_) && !S_ISDIR(filestatus->st_mode)) # falls File existiert und kein Directory ist
                        #else
                        assure_dir_exists(TRUE,FALSE); # Links auflösen, File suchen
                        if (file_exists(_EMA_)) # falls File existiert
                        #endif
                          # result-list erweitern:
                          { if (!nullp(STACK_(0+5+4+2))) # :FULL gewünscht?
                              { with_stat_info(); } # ja -> STACK_0 erweitern
                            # und STACK_0 vor result-list pushen:
                           {var object new_cons = allocate_cons();
                            Car(new_cons) = STACK_0;
                            Cdr(new_cons) = STACK_(4+4+2);
                            STACK_(4+4+2) = new_cons;
                          }}
                        skipSTACK(2);
                        goto next_pathname;
                      #endif
                      #ifndef MSDOS
                      case -1: # In diesem pathname nach einem Subdirectory sehen
                        { var object namestring = assure_dir_exists(TRUE,FALSE); # Links auflösen, Directory-Namestring
                          pushSTACK(namestring); # Directory-Namestring
                          {var object subdir = Car(STACK_(1+4+1+1)); # (car subdir-list)
                           #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_RISCOS)
                           if (eq(subdir,S(Kparent))) # für Parent-Directory
                             {
                               #ifdef PATHNAME_AMIGAOS
                               pushSTACK(O(slash_string)); # zusätzliches "/" ans Ende
                               #endif
                               #ifdef PATHNAME_RISCOS
                               pushSTACK(O(parent_string)); # zusätzliches "^" ans Ende
                               #endif
                             }
                             else
                           #endif
                           pushSTACK(subdir);
                          }
                          namestring = string_concat(2); # zusammenhängen
                          # Information holen:
                          directory_search_1subdir(Car(STACK_(1+4+1)),namestring);
                        }
                        skipSTACK(1);
                        goto next_pathname;
                        #endif
                }   }
              # Um die Task zu erledigen, müssen alle Einträge dieses
              # Directory abgesucht werden:
              {{var object dir_namestring = assure_dir_exists(TRUE,FALSE); # Links auflösen, Directory-Name bilden
                pushSTACK(dir_namestring); # retten
               }# Stackaufbau: ..., pathname, dir_namestring.
                #ifdef UNIX
                if (!nullp(STACK_(1+5+4+2))) # ;CIRCLE-Flag abfragen
                  { # pathname in der Hash-Tabelle suchen:
                    var object hashcode = directory_search_hashcode();
                    if (eq(hashcode,nullobj))
                      { # Eintrag existiert doch nicht (das kann uns
                        # wohl nur bei symbolischen Links passieren)
                        # -> wird übergangen
                        skipSTACK(2); goto next_pathname;
                      }
                    # und in der Hash-Tabelle aufsuchen und ablegen:
                    if (!nullp(shifthash(STACK_(2+2),hashcode,T)))
                      # war schon drin -> wird übergangen
                      { skipSTACK(2); goto next_pathname; }
                  }
                #endif
                if (next_task==0)
                  # Pathname STACK_1 vor result-list pushen:
                  {var object new_cons = allocate_cons();
                   Car(new_cons) = STACK_1;
                   Cdr(new_cons) = STACK_(4+4+2);
                   STACK_(4+4+2) = new_cons;
                  }
                directory_search_scandir(recursively,next_task);
              }
              skipSTACK(2); # pathname und dir_namestring vergessen
              next_pathname: ;
             }# Stackaufbau: ..., new-pathname-list, ht, pathname-list-rest, pathnames-to-insert.
              # Vor dem Weiterrücken mit pathname-list-rest :
              # pathname-list-rest := (nreconc pathnames-to-insert pathname-list-rest) :
             {var object pathnames_to_insert = popSTACK();
              STACK_0 = nreconc(pathnames_to_insert,STACK_0);
            }}
          skipSTACK(2); # leere pathname-list-rest und Hash-Tabelle vergessen
          # new-pathname-list umdrehen, ersetzt die geleerte pathname-list:
          {var object new_pathname_list = popSTACK();
           STACK_0 = nreverse(new_pathname_list); # neue pathname-list
          }
          # Mit dieser Subdir-Stufe sind wir fertig.
          if (matomp(STACK_1)) break; # (atom subdir-list) -> fertig.
          recursively = FALSE; # die nächste (vorläufig) nicht-rekursiv
          passed_subdir: ;
        }}
      # Stackaufbau: result-list, pathname, name&type, subdir-list, pathname-list.
      # subdir-list ist =NIL geworden, auch pathname-list = NIL (denn beim
      # letzten Schleifendurchlauf ist immer next_task=0,1,2, und dadurch
      # wurde nichts auf new-pathname-list gepusht).
      skipSTACK(4);
      return popSTACK(); # result-list als Ergebnis
    }}
  #
  #endif # PATHNAME_NOEXT

LISPFUN(directory,0,1,norest,key,2, (kw(circle),kw(full)) )
# (DIRECTORY [pathname [:circle] [:full]]), CLTL S. 427
  { # Stackaufbau: pathname, circle, full.
    #ifdef UNIX
    # :CIRCLE-Argument hat Defaultwert NIL:
    if (eq(STACK_1,unbound)) { STACK_1 = NIL; }
    #endif
    # :FULL-Argument hat Defaultwert NIL:
    if (eq(STACK_0,unbound)) { STACK_0 = NIL; }
    # Pathname-Argument überprüfen:
   {var object pathname = STACK_2;
    if (eq(pathname,unbound))
      {
        #ifdef PATHNAME_EXT83
        pathname = O(wild_wild_string); # Default ist "*.*" bzw. "*.*;*"
        #endif
        #if defined(PATHNAME_NOEXT) || defined(PATHNAME_RISCOS)
        pathname = O(wild_string); # Default ist "*"
        #endif
      }
    pathname = coerce_pathname(pathname); # zu einem Pathname machen
    # Los geht's:
    #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
    if (eq(ThePathname(pathname)->pathname_device,S(Kwild))) # Device = :WILD ?
      # alle Devices abzusuchen
      { STACK_2 = pathname;
        pushSTACK(NIL); # bisherige Pathname-Liste := NIL
        pushSTACK(STACK_(0+1)); # full (für directory_search)
        # Stackaufbau: pathname, circle, full, pathname-list, full.
        {var char drive;
         for (drive='A'; drive<='Z'; drive++) # alle Drives durchlaufen
           if (good_drive(drive))
             { pushSTACK(n_char_to_string(&drive,1,O(pathname_encoding))); # Device, einelementiger String
              {var object newpathname = copy_pathname(STACK_(2+2+1)); # Pathname kopieren
               ThePathname(newpathname)->pathname_device = popSTACK(); # Drive übernehmen
               # innerhalb eines Laufwerks suchen:
               {var object newpathnames = directory_search(newpathname);
                # und Pathname-Liste vor STACK_1 hängen:
                STACK_1 = nreconc(newpathnames,STACK_1);
        }    }}}
        value1 = nreverse(STACK_1); # Pathname-Liste wieder umdrehen
        skipSTACK(3+2);
      }
      else
      # nur ein Device abzusuchen
    #endif
      { value1 = directory_search(pathname); # matchende Pathnames bilden
        skipSTACK(3);
      }
    mv_count=1;
  }}

LISPFUN(cd,0,1,norest,nokey,0,NIL)
# (CD [pathname]) setzt das aktuelle Laufwerk und das aktuelle Directory.
  { var object pathname = popSTACK();
    if (eq(pathname,unbound)) { pathname = O(leer_string); } # "" als Default
    pathname = coerce_pathname(pathname); # zu einem Pathname machen
    # kopieren und Name und Typ auf NIL setzen:
    pathname = copy_pathname(pathname);
    ThePathname(pathname)->pathname_name = NIL;
    ThePathname(pathname)->pathname_type = NIL;
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # absoluten Pathname draus machen
    pushSTACK(pathname);
    assure_dir_exists(FALSE,FALSE); # Directory muss existieren
    change_default(); # Default-Drive, Default-Directory setzen
    value1 = popSTACK(); mv_count=1; # neuer pathname als Wert
  }

# UP: Überprüft ein Pathname, ob Name und Typ beide =NIL sind,
# und ob das Directory "fast" existiert.
# shorter_directory(pathname,resolve_links)
# > pathname : Pathname-Argument
# > resolve_links : Flag, ob Links aufgelöst werden müssen (normalerweise ja)
# < -(STACK) : absoluter Pathname
#if defined(MSDOS) || defined(WIN32_NATIVE)
# < ergebnis: Directory-Namestring (fürs OS, ohne '\' am Schluss, Normal-Simple-String)
#endif
#if defined(UNIX) || defined(AMIGAOS)
# < ergebnis: Directory-Namestring (fürs OS, ohne '/' am Schluss, Normal-Simple-String)
#endif
#if defined(RISCOS)
# < ergebnis: Directory-Namestring (fürs OS, ohne '.' am Schluss, Normal-Simple-String)
#endif
# Erniedrigt STACK um 1.
# can trigger GC
  local object shorter_directory (object pathname, boolean resolve_links);
  local object shorter_directory(pathname,resolve_links)
    var object pathname;
    var boolean resolve_links;
    { pathname = coerce_pathname(pathname); # Argument zu einem Pathname machen
      check_no_wildcards(pathname); # mit Wildcards -> Fehler
      pathname = use_default_dir(pathname); # Default-Directory einfügen
      # Überprüfe, ob Name=NIL und Typ=NIL :
      if (!(nullp(ThePathname(pathname)->pathname_name)
            && nullp(ThePathname(pathname)->pathname_type)
         ) )
        { fehler_notdir(pathname); }
      pushSTACK(pathname); # neuen Pathname retten
      # verkürze das Directory:
      {var object subdirs = ThePathname(pathname)->pathname_directory;
       if (nullp(Cdr(subdirs))) # Root-Directory ?
         { baddir:
           # STACK_0 = pathname, Wert für Slot PATHNAME von FILE-ERROR
           pushSTACK(STACK_0);
           fehler(file_error,
                  GETTEXT("root directory not allowed here: ~")
                 );
         }
       subdirs = reverse(subdirs); # Liste kopieren und dabei umdrehen
       #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_RISCOS)
       if (eq(Car(subdirs),S(Kparent))) # letztes Subdir muss /= :PARENT sein
         goto baddir;
       #endif
       pushSTACK(subdirs); # Cons mit letztem Subdir als CAR retten
       subdirs = Cdr(subdirs); # alle Subdirs bis aufs letzte
       subdirs = nreverse(subdirs); # wieder in die richtige Reihenfolge bringen
       pathname = STACK_1;
       ThePathname(pathname)->pathname_directory = subdirs; # und in den Pathname setzen
       # Dieses Directory muss existieren:
       pushSTACK(pathname);
       # Stackaufbau: pathname, subdircons, pathname.
       {var object dir_namestring =
          (resolve_links ? assure_dir_exists(FALSE,FALSE) : assume_dir_exists());
        # Baue Subdir-String fürs Betriebssystem:
        STACK_0 = dir_namestring; # bisheriger Directory-Namestring als 1. String
        {var uintC stringcount =
           subdir_namestring_parts(STACK_1); # und Strings zum letzten Subdir
         # und kein '\' am Schluss (fürs OS)
         # und kein '/' am Schluss (fürs OS)
         {var object dirstring = string_concat(1+stringcount); # zusammenhängen
          skipSTACK(1);
          return dirstring;
    } }}}}

LISPFUNN(make_dir,1)
# (MAKE-DIR pathname) legt ein neues Unterdirectory pathname an.
  { var object pathstring = shorter_directory(STACK_0,TRUE);
    with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz,
      { make_directory(pathstring_asciz); });
    skipSTACK(2);
    value1 = T; mv_count=1; # 1 Wert T
  }

LISPFUNN(delete_dir,1)
# (DELETE-DIR pathname) entfernt das Unterdirectory pathname.
  { var object pathstring = shorter_directory(STACK_0,TRUE);
    with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz,
      { delete_directory(pathstring_asciz); });
    skipSTACK(2);
    value1 = T; mv_count=1; # 1 Wert T
  }

LISPFUN(ensure_directories_exist,1,0,norest,key,1,(kw(verbose)))
# (defun ensure-directories-exist (pathspec &key verbose)
#   (let* ((dir (pathname-directory pathspec))
#          (path (make-pathname :host (pathname-host pathspec)
#                               :device (pathname-device pathspec)
#                               :directory dir)))
#     (when (wild-pathname-p path)
#       (error (make-condition 'file-error :pathname pathspec)))
#     (if (directory path)
#       (values pathspec nil)
#       (progn
#         (loop
#           for i from 1 upto (length dir)
#           do (let ((newpath
#                      (make-pathname :host (pathname-host pathspec)
#                                     :device (pathname-device pathspec)
#                                     :directory (subseq dir 0 i))))
#                (unless (directory newpath)
#                  (let ((namestring (namestring newpath)))
#                    (when verbose
#                      (format *standard-output* "~&Creating directory: ~A~%"
#                              namestring))
#                    (ignore-errors (lisp:make-dir namestring))
#                    (unless (directory newpath)
#                      (error (make-condition 'file-error :pathname pathspec))
#              ) ) ) )
#         )
#         (values pathspec t)
# ) ) ) )
  { var object pathname = coerce_pathname(STACK_1); # Argument zu einem Pathname machen
    pathname = copy_pathname(pathname); # Kopieren und dabei name, type streichen
    ThePathname(pathname)->pathname_name = NIL;
    ThePathname(pathname)->pathname_type = NIL;
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    pushSTACK(pathname); # neuen Pathname retten
    # Stackaufbau: pathspec, verbose, pathname.
    if (directory_exists(pathname))
      { skipSTACK(2); value2 = NIL; } # pathspec, NIL als Werte
    else
      { var object subdirs = copy_list(ThePathname(STACK_0)->pathname_directory);
        pushSTACK(subdirs); pushSTACK(Cdr(subdirs));
        Cdr(subdirs) = NIL;
        ThePathname(STACK_2)->pathname_directory = subdirs;
        # Stackaufbau: pathspec, verbose, pathname, (car (last subdirs)), remaining_subdirs.
        while (mconsp(STACK_0))
          { subdirs = STACK_0;
            Cdr(STACK_1) = subdirs; STACK_1 = subdirs; STACK_0 = Cdr(subdirs); Cdr(subdirs) = NIL;
            if (!directory_exists(STACK_2))
              { if (!eq(STACK_3,unbound) && !nullp(STACK_3)) # Verbose?
                  { funcall(L(fresh_line),0); # (FRESH-LINE [*standard-output*])
                    pushSTACK(OLS(mkdirp_string)); funcall(L(write_string),1); # (WRITE-STRING "..." [*standard-output*])
                    pushSTACK(STACK_2); funcall(L(princ),1); # (PRINC pathname [*standard-output*])
                    funcall(L(terpri),0); # (TERPRI [*standard-output*])
                  }
                # NB: Brauche hier keine Links aufzulösen, weil man ja hier
                # sowieso ab der Wurzel schrittweise vorgeht.
               {var object pathstring = shorter_directory(STACK_2,FALSE);
                with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz,
                  { make_directory(pathstring_asciz); });
                skipSTACK(1);
              }}
          }
        skipSTACK(4); value2 = T; # pathspec, T als Werte
      }
    value1 = popSTACK(); mv_count=2;
  }

#ifdef UNIX
# Returns the struct passwd entry for the current user.
# The return value points to static data, or is NULL upon failure.
local struct passwd * unix_user_pwd (void);
local struct passwd * unix_user_pwd()
  {
    var const char* username;
    var struct passwd * userpasswd = NULL;
    # The manpage for GETLOGIN(3V) recommends
    # first getpwnam(getlogin()), then getpwuid(getuid()).
    begin_system_call();
    # 1. attempt: getpwnam(getenv("USER"))
    username = getenv("USER");
    if (username != NULL) {
      errno = 0; userpasswd = getpwnam(username);
      if (userpasswd != NULL) goto ok;
      if (errno != 0) { OS_error(); }
    }
    # 2. attempt: getpwnam(getlogin())
    username = getlogin();
    if (username != NULL) {
      errno = 0; userpasswd = getpwnam(username);
      if (userpasswd != NULL) goto ok;
      if (errno != 0) { OS_error(); }
    }
    # 3. attempt: getpwuid(getuid())
    errno = 0; userpasswd = getpwuid(user_uid);
    if (userpasswd != NULL) goto ok;
    if (errno != 0) { OS_error(); }
    # Everything fails, userpasswd == NULL.
   ok:
    end_system_call();
    return userpasswd;
  }
#endif

# UP: Initialisiert das Pathname-System.
# init_pathnames();
# can trigger GC
  global void init_pathnames (void);
  global void init_pathnames()
    {
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
      { # Default-Drive initialisieren:
        var char drive = default_drive();
        O(default_drive) = n_char_to_string(&drive,1,O(pathname_encoding));
      }
      #endif
      # *DEFAULT-PATHNAME-DEFAULTS* initialisieren:
      recalc_defaults_pathname();
      #ifdef USER_HOMEDIR
      #ifdef UNIX
      # Wir ziehen uns das Home-Directory und die benutzbare Shell aus dem
      # Environment. Es enthält (fast) immer mindestens folgende Variablen:
      #   LOGNAME = Username beim ersten Einloggen ("wahre" Identität des Benutzers)
      #   USER    = aktueller Username
      #   HOME    = aktuelles Home-Directory, aus /etc/passwd geholt
      #   SHELL   = aktuelle Standard-Shell, aus /etc/passwd geholt
      #   PATH    = Suchpfad bei Programmaufruf
      #   TERM    = Terminalemulation
      # Wir holen uns HOME (für "~" - Übersetzung) und SHELL (für EXECUTE).
      # Bei "~username" müssen wir das /etc/passwd - File absuchen.
      { # Im Environment nach Variable HOME suchen:
        begin_system_call();
       {var const char* homedir = getenv("HOME");
        end_system_call();
        if (!(homedir==NULL)) # gefunden?
          { O(user_homedir) = asciz_dir_to_pathname(homedir,O(misc_encoding)); } # ja -> eintragen
          else
          # nein -> Home-Directory aus dem Passwort-File holen:
          { var struct passwd * userpasswd = unix_user_pwd();
            if (!(userpasswd==NULL))
              # ja -> Homedir als Pathname eintragen
              { O(user_homedir) = asciz_dir_to_pathname(userpasswd->pw_dir,O(misc_encoding)); }
              else
              # nein -> aktuelles Directory nehmen:
              { O(user_homedir) = default_directory(); }
      }}  }
      #endif
      #ifdef WIN32
      # WinNT defines HOMEDRIVE and HOMEPATH. Win95 (which is actually not a
      # multiuser OS) lets the user set HOME himself.
      # In any case, we give preference to HOME, because the user can change
      # this.
      { var const char * home;
        begin_system_call();
        home = getenv("HOME");
        if (!(home==NULL))
          { end_system_call();
            O(user_homedir) = asciz_dir_to_pathname(home,O(misc_encoding));
          }
          else
          { var const char * homedrive = getenv("HOMEDRIVE");
            var const char * homepath = getenv("HOMEPATH");
            end_system_call();
            if (homedrive!=NULL && homepath!=NULL)
              { var char* homeall = (char*)alloca(asciz_length(homedrive)+asciz_length(homepath)+1);
                var char* ptr = homeall;
                while ((*ptr = *homedrive) != '\0') { homedrive++; ptr++; }
                while ((*ptr = *homepath) != '\0') { homepath++; ptr++; }
                *ptr = '\0';
                O(user_homedir) = asciz_dir_to_pathname(homeall,O(misc_encoding));
              }
              else
              { O(user_homedir) = use_default_dir(asciz_dir_to_pathname(".",Symbol_value(S(ascii)))); }
      }   }
      #endif
      #endif
      #ifdef HAVE_SHELL
      #ifdef UNIX
      # Die Kommando-Shell O(command_shell) bleibt unverändert, sonst
      # handelt man sich zu viele Portabilitätsprobleme ein.
      { # Im Environment nach Variable SHELL suchen:
        begin_system_call();
       {var const char* shell = getenv("SHELL");
        end_system_call();
        if (!(shell==NULL)) # gefunden?
          { O(user_shell) = asciz_to_string(shell,O(misc_encoding)); } # ja -> eintragen
          # sonst bleibt O(user_shell) auf dem Defaultwert "/bin/csh".
      }}
      #endif
      #ifdef MSDOS
      { # Im Environment nach Variable COMSPEC suchen:
        begin_system_call();
       {var const char* shell = getenv("COMSPEC");
        end_system_call();
        if (!(shell==NULL)) # gefunden?
          { O(command_shell) = asciz_to_string(shell,O(misc_encoding)); } # ja -> eintragen
          # sonst bleibt O(command_shell) auf dem Defaultwert "\\COMMAND.COM".
      }}
      #endif
      #ifdef WIN32_NATIVE
      { # Im Environment nach Variable COMSPEC suchen:
        begin_system_call();
       {var const char* shell = getenv("COMSPEC");
        if (!(shell==NULL))
          { end_system_call();
            O(command_shell) = asciz_to_string(shell,O(misc_encoding)); # eintragen
          }
          else
          { var OSVERSIONINFO v;
            if (!GetVersionEx(&v)) { OS_error(); }
            if (v.dwPlatformId == VER_PLATFORM_WIN32_NT)
              # Windows NT
              { shell = "cmd.exe"; }
              else
              # Windows 95 or else
              { shell = "command.com"; }
            end_system_call();
            O(command_shell) = ascii_to_string(shell); # eintragen
          }
      }}
      #endif
      #endif
    }

#if defined(DJUNIX) || defined(WATCOM)
# UP: Legt Datum/Uhrzeit der Datei mit dem Handle handle im 4-Byte-Buffer ab.
# get_file_write_datetime(handle);
# > handle: Handle eines (offenen) Files
# < file_datetime: Datum und Uhrzeit der Datei
  local var struct { uintW time; uintW date; } file_datetime; # Buffer fürs Ergebnis
  local void get_file_write_datetime (uintW handle);
  #if defined(DJUNIX) || defined(WATCOM)
  #include <dos.h>
  local void get_file_write_datetime(handle)
    var uintW handle;
    {
     #ifndef GNU
      var union REGS in;
      var union REGS out;
      in.regB.ah = 0x57; in.regB.al = 0; # DOS Function 57H
      in.regW.bx = handle;
      intdos(&in,&out);
      file_datetime.time = out.regW.cx;
      file_datetime.date = out.regW.dx;
     #else # dasselbe, nur effizienter
      var uintW time;
      var uintW date;
      __asm__ (# DOS Function 57H
               " movw $0x5700,%%ax ; int $0x21 "
               : "=c" /* %cx */ (time), "=d" /* %dx */ (date)     # OUT
               :                                                  # IN
               : "ax","bx","si","di" /* %eax, %ebx, %esi, %edi */ # CLOBBER
              );
      file_datetime.time = time;
      file_datetime.date = date;
     #endif
    }
  #endif
#endif

LISPFUNN(file_write_date,1)
# (FILE-WRITE-DATE file), CLTL S. 424
  {
    #ifdef AMIGAOS
    var struct DateStamp file_datetime; # Buffer für Datum/Uhrzeit einer Datei
    #endif
    #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
    var time_t file_datetime; # Buffer für Datum/Uhrzeit einer Datei
    #endif
    #ifdef WIN32_NATIVE
    var WIN32_FIND_DATA filedata;
    #endif
    var object pathname = popSTACK(); # pathname-Argument
    if (builtin_stream_p(pathname))
      # Stream -> extra behandeln:
      { # muss File-Stream sein:
        pathname = as_file_stream(pathname);
        # Streamtyp File-Stream
       #if !defined(AMIGAOS)
        if ((TheStream(pathname)->strmflags & strmflags_open_B)
            && (!nullp(TheStream(pathname)->strm_buffered_channel))
           )
          # offener File-Stream
          { # direkt mit dem Handle arbeiten:
            #if defined(DJUNIX) || defined(WATCOM)
            begin_system_call();
            get_file_write_datetime(TheHandle(TheStream(pathname)->strm_buffered_channel));
            end_system_call();
            #endif
            #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
            var struct stat status;
            begin_system_call();
            if (!( fstat(TheHandle(TheStream(pathname)->strm_buffered_channel),&status) ==0))
              { end_system_call(); OS_filestream_error(pathname); }
            end_system_call();
            file_datetime = status.st_mtime;
            #endif
            #ifdef WIN32_NATIVE
            var BY_HANDLE_FILE_INFORMATION fileinfo;
            var BOOL result;
            begin_system_call();
            result = GetFileInformationByHandle(TheHandle(TheStream(pathname)->strm_buffered_channel),&fileinfo);
            end_system_call();
            if (result)
              { filedata.ftCreationTime   = fileinfo.ftCreationTime;
                filedata.ftLastAccessTime = fileinfo.ftLastAccessTime;
                filedata.ftLastWriteTime  = fileinfo.ftLastWriteTime;
              }
              else
              { # If that failed, try the full pathname.
                test_file_stream_named(pathname);
                pathname = TheStream(pathname)->strm_file_truename;
                goto is_pathname;
              }
            #endif
          }
          else
       #endif
          # geschlossener File-Stream -> Truename als Pathname verwenden
          { test_file_stream_named(pathname);
            pathname = TheStream(pathname)->strm_file_truename;
            goto is_pathname;
          }
      }
      else
      { pathname = coerce_pathname(pathname); # zu einem Pathname machen
        is_pathname: # pathname ist jetzt wirklich ein Pathname
        check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(pathname);
       {# Directory muss existieren:
        var object namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
        #ifdef MSDOS
         #if defined(DJUNIX) || defined(WATCOM)
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { # Datei öffnen:
              begin_system_call();
              { var sintW ergebnis = # Datei zu öffnen versuchen
                  open(namestring_asciz,O_RDONLY);
                if (ergebnis < 0)
                  { end_system_call(); OS_file_error(STACK_0); } # Error melden
                # Nun enthält ergebnis das Handle des geöffneten Files.
                get_file_write_datetime(ergebnis); # Datum/Uhrzeit holen
                if (CLOSE(ergebnis) < 0) # Datei gleich wieder schließen
                  { end_system_call(); OS_file_error(STACK_0); }
              }
              end_system_call();
            });
         #else # defined(EMUNIX)
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { var struct stat statbuf;
              begin_system_call();
              if (stat(namestring_asciz,&statbuf) < 0)
                { end_system_call(); OS_file_error(STACK_0); }
              end_system_call();
              if (!S_ISREG(statbuf.st_mode)) { fehler_file_not_exists(); } # Datei muss existieren
              file_datetime = statbuf.st_mtime;
            });
         #endif
        #endif
        #ifdef AMIGAOS
        if (!file_exists(namestring)) { fehler_file_not_exists(); } # Datei muss existieren
        file_datetime = filestatus->fib_Date;
        #endif
        #if defined(UNIX) || defined(RISCOS)
        if (!file_exists(namestring)) { fehler_file_not_exists(); } # Datei muss existieren
        file_datetime = filestatus->st_mtime;
        #endif
        #ifdef WIN32_NATIVE
        # Only a directory search gives us the times.
        with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
          { var HANDLE search_handle;
            begin_system_call();
            search_handle = FindFirstFile(namestring_asciz,&filedata);
            if (search_handle==INVALID_HANDLE_VALUE)
              { if (GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND)
                  { end_system_call(); fehler_file_not_exists(); }
                end_system_call(); OS_file_error(STACK_0);
              }
            elif (!FindClose(search_handle))
              { end_system_call(); OS_file_error(STACK_0); }
            end_system_call();
          });
        #endif
        skipSTACK(1);
      }}
    # Datum/Uhrzeit steht nun im Buffer file_datetime.
    # In Decoded-Time-Format umwandeln:
    { var decoded_time timepoint;
      #if defined(DJUNIX) || defined(WATCOM)
      convert_timedate(file_datetime.time,file_datetime.date,&timepoint);
      #endif
      #if defined(UNIX) || defined(EMUNIX) || defined(AMIGAOS) || defined(RISCOS)
      convert_time(&file_datetime,&timepoint);
      #endif
      #ifdef WIN32_NATIVE
      var FILETIME* pftimepoint = &filedata.ftLastWriteTime;
      if (pftimepoint->dwLowDateTime==0 && pftimepoint->dwHighDateTime==0)
        pftimepoint = &filedata.ftCreationTime;
      convert_time(pftimepoint,&timepoint);
      #endif
      pushSTACK(timepoint.Sekunden);
      pushSTACK(timepoint.Minuten);
      pushSTACK(timepoint.Stunden);
      pushSTACK(timepoint.Tag);
      pushSTACK(timepoint.Monat);
      pushSTACK(timepoint.Jahr);
      funcall(S(encode_universal_time),6);
      # (ENCODE-UNIVERSAL-TIME Sekunden Minuten Stunden Tag Monat Jahr)
      # als Ergebnis
  } }

LISPFUNN(file_author,1)
# (FILE-AUTHOR file), CLTL S. 424
  { var object pathname = popSTACK(); # pathname-Argument
    if (builtin_stream_p(pathname))
      # Stream -> extra behandeln:
      { # muss File-Stream sein:
        pathname = as_file_stream(pathname);
        # Streamtyp File-Stream
        if (TheStream(pathname)->strmflags & strmflags_open_B)
          # offener File-Stream -> OK
          {}
          else
          # geschlossener File-Stream -> Truename als Pathname verwenden
          { test_file_stream_named(pathname);
            pathname = TheStream(pathname)->strm_file_truename;
            goto is_pathname;
          }
      }
      else
      { pathname = coerce_pathname(pathname); # zu einem Pathname machen
        is_pathname: # pathname ist jetzt wirklich ein Pathname
        # pathname ist jetzt ein Pathname.
        check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(pathname);
       {# Directory muss existieren:
        var object namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
        #ifdef MSDOS
         #if 1
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { # Datei öffnen:
              begin_system_call();
              { var sintW ergebnis = # Datei zu öffnen versuchen
                  open(namestring_asciz,O_RDONLY);
                if (ergebnis < 0)
                  { end_system_call(); OS_file_error(STACK_0); } # Error melden
                # Nun enthält ergebnis das Handle des geöffneten Files.
                if (CLOSE(ergebnis) < 0) # Datei gleich wieder schließen
                  { end_system_call(); OS_file_error(STACK_0); }
              }
              end_system_call();
            });
         #else
          with_sstring_0(namestring,O(pathname_encoding),namestring_asciz,
            { var struct stat statbuf;
              begin_system_call();
              if (stat(namestring_asciz,&statbuf) < 0)
                { end_system_call(); OS_file_error(STACK_0); }
              end_system_call();
              if (!S_ISREG(statbuf.st_mode)) { fehler_file_not_exists(); } # Datei muss existieren
            });
         #endif
        #endif
        #if defined(UNIX) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
        if (!file_exists(namestring)) { fehler_file_not_exists(); } # Datei muss existieren
        #endif
        skipSTACK(1);
      }}
    # Datei existiert -> NIL als Wert
    value1 = NIL; mv_count=1;
  }

#if defined(UNIX) || defined(MSDOS) || defined(RISCOS)

LISPFUN(execute,1,0,rest,nokey,0,NIL)
# (EXECUTE file arg1 arg2 ...) ruft ein File mit gegebenen Argumenten auf.
  {var object* args_pointer = rest_args_pointer STACKop 1;
   {var object* argptr = args_pointer; # Pointer über die Argumente
    # File überprüfen:
    { var object* file_ = &NEXT(argptr);
      var object pathname = *file_;
      pathname = coerce_pathname(pathname); # zu einem Pathname machen
      check_no_wildcards(pathname); # mit Wildcards -> Fehler
      pathname = use_default_dir(pathname); # Default-Directory einfügen
      if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
      # Name angegeben.
      pushSTACK(pathname);
     {# Directory muss existieren:
      var object namestring = assure_dir_exists(FALSE,FALSE); # Filename fürs Betriebssystem
      # Überprüfe, ob die Datei existiert:
      if (!file_exists(namestring)) { fehler_file_not_exists(); }
      *file_ = string_to_asciz(namestring,O(pathname_encoding)); # retten
      skipSTACK(1);
    }}
    # restliche Argumente überprüfen:
    { var uintC count;
      dotimesC(count,argcount,
        { var object* arg_ = &NEXT(argptr);
          pushSTACK(*arg_); funcall(L(string),1); # nächstes Argument in String umwandeln
          *arg_ = string_to_asciz(value1,O(misc_encoding)); # und ASCIZ-String umwandeln
        });
   }}
   #if defined(EMUNIX_PORTABEL)
   # (Unter OS/2 scheint system() sicherer zu sein als spawnv(). Warum?)
   # Alle Argumente (nun ASCIZ-Strings) zusammenhängen, mit Spaces dazwischen:
   { var uintL argvdata_length = 0;
     { var object* argptr = args_pointer;
       var uintC count;
       dotimespC(count,argcount+1,
         { var object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
           argvdata_length += Sbvector_length(arg)/8;
         });
     }
    {var DYNAMIC_ARRAY(argvdata,char,argvdata_length);
     { var object* argptr = args_pointer;
       var char* argvdataptr = &argvdata[0];
       var uintC count;
       dotimespC(count,argcount+1,
         { var object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
           var char* ptr = TheAsciz(arg);
           var uintL len = Sbvector_length(arg)/8;
           dotimesL(len,len-1, { *argvdataptr++ = *ptr++; } ); # und kopieren
           *argvdataptr++ = ' ';
         });
       argvdataptr[-1] = '\0';
     }
     # Programm aufrufen:
      begin_system_call();
     {var int ergebnis = system(&argvdata[0]);
      end_system_call();
      # Fertig.
      set_args_end_pointer(args_pointer); # STACK aufräumen
      # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
      value1 = (ergebnis==0 ? T : NIL); mv_count=1;
      FREE_DYNAMIC_ARRAY(argvdata);
   }}}
   #endif
   #if defined(DJUNIX) || (defined(EMUNIX) && !defined(EMUNIX_PORTABEL)) || defined(WATCOM)
   {# argv-Array aufbauen:
    var DYNAMIC_ARRAY(argv,char*,1+(uintL)argcount+1);
    { var object* argptr = args_pointer;
      var char** argvptr = &argv[0];
      var uintC count;
      dotimespC(count,argcount+1,
        { var object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
          *argvptr++ = TheAsciz(arg); # in argv einfüllen
        });
      *argvptr = NULL; # und mit Nullpointer abschließen
    }
    # Programm aufrufen:
    begin_system_call();
    {var int flags =
       #ifdef EMUNIX
         P_QUOTE  # Argumente korrekt quoten
       #else
         0
       #endif
       ;
     var int ergebnis = spawnv(P_WAIT|flags,argv[0],argv);
     end_system_call();
     if (ergebnis < 0) { OS_error(); } # Error melden
     # Fertig.
     set_args_end_pointer(args_pointer); # STACK aufräumen
     # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
     value1 = (ergebnis==0 ? T : NIL); mv_count=1;
    }
    FREE_DYNAMIC_ARRAY(argv);
   }
   #endif
   #if defined(UNIX) || defined(RISCOS)
   { # argv-Array im Stack aufbauen und Strings in den Stack kopieren:
     var uintL argvdata_length = 0;
     { var object* argptr = args_pointer;
       var uintC count;
       dotimespC(count,argcount+1,
         { var object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
           argvdata_length += Sbvector_length(arg)/8;
         });
     }
    {var DYNAMIC_ARRAY(argv,char*,1+(uintL)argcount+1);
     var DYNAMIC_ARRAY(argvdata,char,argvdata_length);
     { var object* argptr = args_pointer;
       var char** argvptr = &argv[0];
       var char* argvdataptr = &argvdata[0];
       var uintC count;
       dotimespC(count,argcount+1,
         { var object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
           var char* ptr = TheAsciz(arg);
           var uintL len = Sbvector_length(arg)/8;
           *argvptr++ = argvdataptr; # in argv einfüllen
           dotimespL(len,len, { *argvdataptr++ = *ptr++; } ); # und kopieren
         });
       *argvptr = NULL; # und mit Nullpointer abschließen
     }
     # einen neuen Prozess starten:
     { var int child;
       begin_system_call();
       begin_want_sigcld();
       if ((child = vfork()) ==0)
         # Dieses Programmstück wird vom Child-Prozess ausgeführt:
         { execv(argv[0],argv); # Programm aufrufen
           _exit(-1); # sollte dies misslingen, Child-Prozess beenden
         }
       # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
       if (child==-1)
         # Etwas ist misslungen, entweder beim vfork oder beim execv.
         # In beiden Fällen wurde errno gesetzt.
         { end_want_sigcld(); OS_error(); }
       # Warten, bis der Child-Prozess beendet wird:
      {var int status = wait2(child);
       # vgl. WAIT(2V) und #include <sys/wait.h> :
       #   WIFSTOPPED(status)  ==  ((status & 0xFF) == 0177)
       #   WEXITSTATUS(status)  == ((status & 0xFF00) >> 8)
       end_want_sigcld();
       end_system_call();
       # Fertig.
       set_args_end_pointer(args_pointer); # STACK aufräumen
       value1 = (((status & 0xFF) == 0000) # Prozess normal beendet (ohne Signal, ohne Core-Dump) ?
                 ? # ja -> Exit-Status als Wert:
                   fixnum( (status & 0xFF00) >> 8)
                 : NIL # nein -> NIL als Wert
                );
       mv_count=1;
     }}
     FREE_DYNAMIC_ARRAY(argvdata);
     FREE_DYNAMIC_ARRAY(argv);
   }}
   #endif
  }

#endif

#ifdef AMIGAOS

LISPFUN(execute,1,0,norest,nokey,0,NIL)
# (EXECUTE command-string) schickt einen String an das Betriebssystem.
# Das ist in diesem Fall mit (SHELL command-string) synonym.
  { C_shell(); } # SHELL aufrufen, selber Stackaufbau

#endif

#ifdef HAVE_SHELL

# (SHELL) ruft eine Shell auf.
# (SHELL command) ruft eine Shell auf und lässt sie ein Kommando ausführen.

#if defined(AMIGAOS)
#include <dos/dostags.h>         # für SystemTags()

LISPFUN(shell,0,1,norest,nokey,0,NIL)
  { var object command = popSTACK();
    # As I/O is on the terminal and we obviously keep a handle on it,
    # we will also get ^C^D^E^F signals from it during command execution.
    # We choose to restore the initial signal state to avoid a double
    # interruption, even though that implies that signals sent to us
    # explicitly (Break command) in this time will be ignored.
    if (eq(command,unbound))
      # Kommandointerpreter aufrufen:
      { run_time_stop();
        begin_system_call();
       {var BOOL ergebnis = FALSE;
        #if 0 # so einfach geht's wohl nicht
        ergebnis = Execute("",stdin_handle,stdout_handle);
        #else
        var Handle terminal = Open("*",MODE_READWRITE);
        if (!(terminal==Handle_NULL))
          { var ULONG signals = SetSignal(0L,0L);
            ergebnis = Execute("",terminal,Handle_NULL);
            # Restore state of break signals
            SetSignal(signals,(SIGBREAKF_CTRL_C|SIGBREAKF_CTRL_D|SIGBREAKF_CTRL_E|SIGBREAKF_CTRL_F));
            Write(terminal,NLstring,1);
            Close(terminal);
          }
        #endif
        end_system_call();
        run_time_restart();
        # Rückgabewert verwerten: ausgeführt -> T, nicht gefunden -> NIL :
        value1 = (ergebnis ? T : NIL); mv_count=1;
      }}
      else
      # einzelnes Kommando ausführen:
      { if (!stringp(command))
          { pushSTACK(command); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(command);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(type_error,
                   GETTEXT("~: the command should be a string, not ~")
                  );
          }
        with_string_0(command,O(misc_encoding),command_asciz,
          { # Kommando ausführen:
            run_time_stop();
            begin_system_call();
           {var ULONG signals = SetSignal(0L,0L);
            # Using SystemTags() instead of Execute() sends console signals to
            # the command and it gives us the command's exit code.
            var LONG ergebnis =
              SystemTags(command_asciz,
                         SYS_Input, stdin_handle,
                         # Work around bug in Emacs-18 subshell where Input() and Output()
                         # are forbiddenly the same by having Input() cloned.
                         SYS_Output, (stdin_handle == stdout_handle) ? Handle_NULL : stdout_handle,
                         SYS_UserShell, 1L,
                         TAG_DONE);
            # Restore state of break signals
            SetSignal(signals,(SIGBREAKF_CTRL_C|SIGBREAKF_CTRL_D|SIGBREAKF_CTRL_E|SIGBREAKF_CTRL_F));
            end_system_call();
            run_time_restart();
            # Rückgabewert verwerten
            value1 = L_to_I(ergebnis); mv_count=1;
          }});
      }
  }

#elif defined(WIN32_NATIVE)

LISPFUNN(shell_name,0)
# (SYSTEM::SHELL-NAME) returns the name of the command shell.
  { value1 = O(command_shell); mv_count=1; }

LISPFUN(shell,0,1,norest,nokey,0,NIL)
  { var object command = popSTACK();
    if (eq(command,unbound))
      { command = O(command_shell); }
    if (!stringp(command))
      { pushSTACK(command); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(command);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: the command should be a string, not ~")
              );
      }
   {var HANDLE prochandle;
    with_string_0(command,O(misc_encoding),command_asciz,
      { # Start new process.
        var HANDLE stdinput;
        var HANDLE stdoutput;
        var PROCESS_INFORMATION pinfo;
        begin_system_call();
        stdinput = GetStdHandle(STD_INPUT_HANDLE);
        if (stdinput == INVALID_HANDLE_VALUE) { OS_error(); }
        stdoutput = GetStdHandle(STD_OUTPUT_HANDLE);
        if (stdoutput == INVALID_HANDLE_VALUE) { OS_error(); }
        if (!MyCreateProcess(command_asciz,stdinput,stdoutput,&pinfo))
          { OS_error(); }
        if (!CloseHandle(pinfo.hThread)) { OS_error(); }
        prochandle = pinfo.hProcess;
      });
    # Wait until it terminates, get its exit status code.
    { var DWORD exitcode;
      switch (WaitForSingleObject(prochandle,INFINITE))
        { case WAIT_FAILED:
            OS_error();
          case WAIT_OBJECT_0:
            break;
          default:
            NOTREACHED
        }
      if (!GetExitCodeProcess(prochandle,&exitcode)) { OS_error(); }
      if (!CloseHandle(prochandle)) { OS_error(); }
      end_system_call();
      # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
      value1 = (exitcode==0 ? T : NIL); mv_count=1;
  }}}

#else # UNIX || MSDOS || ...

LISPFUN(shell,0,1,norest,nokey,0,NIL)
  { var object command = popSTACK();
    if (eq(command,unbound))
      { # (EXECUTE shell) ausführen:
        #ifdef UNIX
        pushSTACK(O(user_shell)); # Shell-Name
        #else # MSDOS
        pushSTACK(O(command_shell)); # Shell-Name
        #endif
        funcall(L(execute),1);
      }
      else
      #if defined(MSDOS) || defined(RISCOS)
      # Dem DOS-Kommandointerpreter muss man das Kommando bereits entlang
      # der Leerstellen in einzelne Teile zerlegt übergeben. Die Funktion
      # system() erledigt uns das zum Glück.
      { if (!stringp(command))
          { pushSTACK(command); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(command);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(type_error,
                   GETTEXT("~: the command should be a string, not ~")
                  );
          }
        with_string_0(command,O(misc_encoding),command_asciz,
          { begin_system_call();
            # Programm aufrufen:
           {var int ergebnis = system(command_asciz);
            end_system_call();
            # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
            value1 = (ergebnis==0 ? T : NIL); mv_count=1;
          }});
      }
      #else
      { # (EXECUTE shell "-c" command) ausführen:
        pushSTACK(O(command_shell)); # Shell-Name
        pushSTACK(O(command_shell_option)); # Shell-Option "-c"
        #if defined(MSDOS) && defined(EMUNIX)
        # Unter DOS 2.x, 3.x kann das Optionen-Zeichen ein anderes sein!
        if ((_osmode == DOS_MODE) && (_osmajor < 4))
          { var uintB swchar = _swchar();
            if (swchar) # evtl. "/C" durch etwas anderes ersetzen
              { TheSstring(STACK_0)->data[0] = ascii(swchar); } # (destruktiv)
          }
        #endif
        pushSTACK(command);
        funcall(L(execute),3);
      }
      #endif
  }

#endif

#endif

LISPFUNN(savemem,1)
# (SAVEMEM pathname) speichert ein Speicherabbild unter pathname ab.
  { # (OPEN pathname :direction :output) ausführen:
    # pathname als 1. Argument
    pushSTACK(S(Kdirection)); # :DIRECTION als 2. Argument
    pushSTACK(S(Koutput)); # :OUTPUT als 3. Argument
    #ifdef UNIX
    # Unter Unix mit mmap() darf man existierende .mem-Files nicht einfach
    # überschreiben, weil laufende Lisp-Prozesse dadurch abstürzen würden.
    # Deswegen :if-exists :rename-and-delete.
    #if defined(UNIX_LINUX) && defined(SINGLEMAP_MEMORY)
    # Under Linux 1.3.20, when the mem file to be saved is on an NFS volume
    # and has the same filename as the mem file we started with, the GC
    # done by savemem (once the new mem file has been created and still has
    # size 0) will crash. Looks like a bug in the Linux NFS client, which
    # causes random pages to be mapped in instead of pages from the renamed
    # old mem file. Workaround: Do a full GC, forcing all the old mem file's
    # contents into memory immediately.
    gar_col();
    #endif
    pushSTACK(S(Kif_exists)); # :IF-EXISTS als 4. Argument
    pushSTACK(S(Krename_and_delete)); # :RENAME-AND-DELETE als 5. Argument
    funcall(L(open),5);
    #else
    #ifdef SELFMADE_MMAP
    # Bei selfmade_mmap muss man das existierende .mem-File erst komplett in
    # den Speicher laden. Andere laufende Lisp-Prozesse werden aber abstürzen.
    gar_col();
    #endif
    funcall(L(open),3);
    #endif
    # Speicherabbild in die Datei schreiben:
    # (Den Stream muss die Funktion savemem() schließen, auch im Fehlerfalle.)
    savemem(value1);
    value1 = T; mv_count=1; # 1 Wert T
  }

#ifdef DYNAMIC_MODULES

LISPFUNN(dynload_modules,2)
# (SYSTEM::DYNLOAD-MODULES pathname stringlist)
# loads a shared library, containing a number of modules.
  { # Pathname in String umwandeln:
    pushSTACK(subr_self); # subr_self retten
    pushSTACK(STACK_2); pushSTACK(T); funcall(L(namestring),2); # (NAMESTRING pathname T)
    STACK_2 = value1; subr_self = popSTACK();
    # Strings überprüfen und in den Stack legen:
   {var uintL stringcount = llength(STACK_0);
    var object* arg_ = &STACK_0;
    { var uintL count;
      dotimesL(count,stringcount,
        { if (!stringp(Car(*arg_))) fehler_string(Car(*arg_));
          pushSTACK(string_to_asciz(Car(*arg_),Symbol_value(S(ascii))));
          *arg_ = Cdr(*arg_);
        });
      # test for nullp(*arg_) ??
    }
    { var const char * libpath = TheAsciz(string_to_asciz(*(arg_ STACKop 1),O(pathname_encoding)));
      var DYNAMIC_ARRAY(modnames,const char *,stringcount);
      if (stringcount > 0)
        { var uintL count;
          var object* ptr1 = STACK STACKop stringcount;
          var const char * * ptr2 = modnames;
          dotimespL(count,stringcount, { *ptr2++ = TheAsciz(NEXT(ptr1)); });
        }
      dynload_modules(libpath,stringcount,modnames);
      FREE_DYNAMIC_ARRAY(modnames);
    }
    skipSTACK(stringcount+1);
    value1 = popSTACK(); mv_count=1; # Library-Name als Wert
  }}

#endif

# =============================================================================

#ifdef HAVE_DISASSEMBLER

# Finding the full path of the executable.
# Bruno Haible 20.12.1994

# This assumes that the executable is not removed or renamed while running.

# file name of the executable
local char* executable_name = NULL;
#define default_executable_name  "lisp.run"

# file descriptor of the executable
# (Only used to verify that we find the correct executable.)
local int executable_fd = -1;

# maybe_executable(pathname)
# checks whether a given pathname may belong to the executable.
local boolean maybe_executable (const char * filename);
local boolean maybe_executable(filename)
  var const char * filename;
{ var struct stat statexe;
  var struct stat statfile;
  if (access(filename,R_OK|X_OK) < 0)
    return FALSE;
  if (executable_fd < 0)
    return TRUE;
  # If we already have an executable_fd, check that filename points to
  # the same inode.
  if (fstat(executable_fd,&statexe) < 0)
    return TRUE;
  if (stat(filename,&statfile) < 0)
    return FALSE;
  if (statfile.st_dev
      && statfile.st_dev == statexe.st_dev
      && statfile.st_ino == statexe.st_ino
     )
    return TRUE;
  return FALSE;
}

# find_executable(program_name)
# is to be called immediately after the program starts,
# with program_name = argv[0],
# before any chdir() operation and before any setenv("PATH",...).
# It determines the full program path and opens a file descriptor to
# the executable, for later use.
# Return value is 0 if successful, -1 and errno set if not.
global int find_executable (const char * program_name);
global int find_executable(program_name)
  var const char * program_name;
{ # Don't need to execute this more than once.
  if (!(executable_name == NULL)) return 0;
  #ifdef UNIX_LINUX
  # The executable is accessible as /proc/<pid>/exe. We try this first
  # because it is safer: no race condition w.r.t. the file system. It may
  # fail, however, if the user has not compiled /proc support into his
  # kernel.
  { var char buf[6+10+5];
    var int fd;
    sprintf(buf,"/proc/%d/exe",getpid());
    fd = OPEN(buf,O_RDONLY,my_open_mask);
    if (fd >= 0)
      executable_fd = fd;
  }
  #endif
  # Now we guess the executable's full path. We assume the executable
  # has been called via execlp() or execvp() with properly set up argv[0].
  # The login(1) convention to add a '-' prefix to argv[0] is not supported.
 {var boolean has_slash = FALSE;
  {var const char * p;
   for (p = program_name; *p; p++) { if (*p == '/') { has_slash = TRUE; break; } }
  }
  if (!has_slash)
    { # exec searches paths without slashes in the directory list given
      # by $PATH.
      var const char * path = getenv("PATH");
      if (!(path==NULL))
        { var const char * p;
          var const char * p_next;
          for (p = path; *p; p = p_next)
            { var const char * q;
              var uintL p_len;
              for (q = p; *q; q++) { if (*q == ':') break; }
              p_len = q-p; p_next = (*q=='\0' ? q : q+1);
              # We have a path item at p, of length p_len.
              # Now concatenate the path item and program_name.
             {var char * concat_name = (char*) malloc(p_len + strlen(program_name) + 2);
              if (concat_name == NULL) { errno = ENOMEM; goto notfound; }
              if (p_len == 0)
                # empty PATH element designates the current directory
                { strcpy(concat_name,program_name); }
                else
                { memcpy(concat_name, p, p_len);
                  sprintf(concat_name+p_len, "/%s", program_name);
                }
              if (maybe_executable(concat_name))
                # Assume we have found the executable
                { program_name = concat_name; goto resolve; }
              free(concat_name);
            }}
        }
      # Not found in the PATH, assume the current directory.
    }
  # exec treats paths containing slashes as relative to the current directory.
  if (maybe_executable(program_name))
    resolve:
    # resolve program_name:
    { executable_name = (char*) malloc(MAXPATHLEN);
      if (executable_name == NULL) { errno = ENOMEM; goto notfound; }
      if (realpath(program_name,executable_name) == NULL)
        { free(executable_name); goto notfound; }
      return 0;
    }
  errno = ENOENT;
 notfound:
  executable_name = default_executable_name; return -1;
}}

# (SYS::PROGRAM-NAME) returns the executable's name.
LISPFUNN(program_name,0)
{ value1 = asciz_to_string(executable_name,O(pathname_encoding)); mv_count=1; }

#endif

global char* argv_lisplibdir = NULL; # set during main()
# (SYS::LIB-DIRECTORY) returns clisp's private library directory (called
# $(lisplibdir) in the Makefile).
LISPFUNN(lib_directory,0)
{ if (!(argv_lisplibdir==NULL))
    { value1 = asciz_dir_to_pathname(argv_lisplibdir,O(misc_encoding)); }
    else
    { pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: library directory is not known, use a command line option to specify it")
            );
    }
  mv_count=1;
}

# =============================================================================

#ifdef EXPORT_SYSCALLS

# This piece of code is under the responsibility of Sam Steingold.

#ifdef UNIX

#define PASSWD_TO_STACK(pwd)                                   \
  pushSTACK(asciz_to_string(pwd->pw_name,O(misc_encoding)));   \
  pushSTACK(asciz_to_string(pwd->pw_passwd,O(misc_encoding))); \
  pushSTACK(UL_to_I(pwd->pw_uid));                             \
  pushSTACK(UL_to_I(pwd->pw_gid));                             \
  pushSTACK(asciz_to_string(pwd->pw_gecos,O(misc_encoding)));  \
  pushSTACK(asciz_to_string(pwd->pw_dir,O(misc_encoding)));    \
  pushSTACK(asciz_to_string(pwd->pw_shell,O(misc_encoding)))

# return the data for the user as 7 values (slots of struct passwd)
# or a list of simple vectors of length 7 is no argument was given.
LISPFUN(user_data,0,1,norest,nokey,0,NIL)
# (LISP::USER-DATA &optional user)
{
  var object user = popSTACK();
  struct passwd *pwd = NULL;

  if (nullp(user))  { # all users as a list
    int count = 0;
    begin_system_call();
    for (; (pwd = getpwent()); count++) {
      PASSWD_TO_STACK(pwd);
      funcall(L(vector),7);
      pushSTACK(value1);
    }
    endpwent();
    end_system_call();
    value1 = listof(count); mv_count = 1;
    return;
  }

  begin_system_call();
  if (posfixnump(user)) pwd = getpwuid(posfixnum_to_L(user));
  else if (eq(user,unbound) || eq(user,S(Kdefault)))
    pwd = unix_user_pwd();
  else if (symbolp(user))
    pwd = getpwnam(TheAsciz(string_to_asciz(Symbol_name(user),
                                            O(misc_encoding))));
  else if (stringp(user))
    pwd = getpwnam(TheAsciz(string_to_asciz(user,O(misc_encoding))));
  else { end_system_call(); fehler_string_integer(user); }
  end_system_call();

  if (NULL == pwd) { OS_error(); }
  PASSWD_TO_STACK(pwd);
  funcall(L(values),7);
}

# Lisp interface to stat(2), lstat(2) and fstat(2)
# the first arg can be: file stream, pathname, string, symbol, number.
# the return values are: the file descriptor (int) or the file name
# (string) on which the appropriate stat function was called,
# as well as the 13 slots of the struct stat.
LISPFUN(file_stat,1,1,norest,nokey,0,NIL)
# (LISP:FILE-STAT file &optional link-p)
{
  var object link = popSTACK();
  var object file = popSTACK();
  struct stat buf;

  if (builtin_stream_p(file)) {
    pushSTACK(file);
    funcall(L(built_in_stream_open_p),1);
    if (nullp(value1)) {        # closed stream
      file = as_file_stream(file);
      if (nullp(TheStream(file)->strm_file_truename))
        fehler_file_stream_unnamed(file);
      file = TheStream(file)->strm_file_truename;
    } else                      # open stream
      file = stream_fd(file);
  } else if (symbolp(file)) file = Symbol_name(file);

  if (pathnamep(file)) {
    pushSTACK(file);
    funcall(L(namestring),1);
    file = value1;
  }

  if (posfixnump(file)) {
    begin_system_call();
    if (0 != fstat(posfixnum_to_L(file),&buf)) { OS_error(); }
    end_system_call();
  } else if (stringp(file)) {
    char * string = TheAsciz(string_to_asciz(file,O(pathname_encoding)));
    begin_system_call();
    if (0 != ((eq(link,unbound) || nullp(link)) ?
              stat(string,&buf) : lstat(string,&buf)))
      { OS_error(); }
    end_system_call();
  } else fehler_pathname_designator(file);

  pushSTACK(file);                    # the object stat'ed
  pushSTACK(L_to_I(buf.st_dev));      # device
  pushSTACK(UL_to_I(buf.st_ino));     # inode
  pushSTACK(UL_to_I(buf.st_mode));    # protection
  pushSTACK(UL_to_I(buf.st_nlink));   # number of hard links
  pushSTACK(UL_to_I(buf.st_uid));     # user ID of owner
  pushSTACK(UL_to_I(buf.st_gid));     # group ID of owner
  pushSTACK(L_to_I(buf.st_rdev));     # device type (if inode device)
  pushSTACK(L_to_I(buf.st_size));     # total size, in bytes
  pushSTACK(UL_to_I(buf.st_blksize)); # blocksize for filesystem I/O
  pushSTACK(UL_to_I(buf.st_blocks));  # number of blocks allocated
  # 2208988800 is the number of seconds from 1900-01-01 to 1970-01-01
  pushSTACK(UL_to_I(buf.st_atime+2208988800)); # time of last access
  pushSTACK(UL_to_I(buf.st_mtime+2208988800)); # time of last modification
  pushSTACK(UL_to_I(buf.st_ctime+2208988800)); # time of last change
  funcall(L(values),14);
}

#endif # UNIX
#endif # EXPORT_SYSCALLS

# =============================================================================

#ifdef EMUNIX_PORTABEL

# Umgehen eines lästigen ENAMETOOLONG Errors bei Benutzung von langen
# Filenamen auf FAT-Drives unter OS/2:

#undef chdir
#undef access
#undef stat
#undef unlink
#undef rename
#undef __findfirst
#undef mkdir
#undef open
#undef creat
#undef spawnv

# path2 := verkürzte Kopie von path1
local void shorten_path (const char* path1, char* path2)
  { var const uintB* p1 = path1;
    var uintB* p2 = path2;
    var uintB c;
    var uintC wordlength = 0; # bisherige Länge in Name oder Typ
    var uintC maxwordlength = 8; # = 8 im Namen, = 3 im Typ
    loop
      { c = *p1++;
        if (c=='\0') { *p2++ = c; break; }
        if ((c=='\\') || (c=='/') || (c==':'))
          { *p2++ = c; wordlength = 0; maxwordlength = 8; }
        elif (c=='.')
          { *p2++ = c; wordlength = 0; maxwordlength = 3; }
        else
          { if (++wordlength <= maxwordlength) { *p2++ = c; } }
  }   }

global int my_chdir(path)
  var CONST char* path;
  { var int erg = chdir(path);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = chdir(shorter_path);
      }
    return erg;
  }

global int my_access(path,amode)
  var CONST char* path;
  var int amode;
  { var int erg = access(path,amode);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = access(shorter_path,amode);
      }
    return erg;
  }

global int my_stat(path,buf)
  var CONST char* path;
  var struct stat * buf;
  { var int erg = stat(path,buf);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = stat(shorter_path,buf);
      }
    return erg;
  }

global int my_unlink(path)
  var CONST char* path;
  { var int erg = unlink(path);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = unlink(shorter_path);
      }
    return erg;
  }

global int my_rename(oldpath,newpath)
  var CONST char* oldpath;
  var CONST char* newpath;
  { var int erg = rename(oldpath,newpath);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_oldpath = alloca(asciz_length(oldpath)+1);
        shorten_path(oldpath,shorter_oldpath);
        erg = rename(shorter_oldpath,newpath);
        if ((erg<0) && (errno==ENAMETOOLONG))
          { var char* shorter_newpath = alloca(asciz_length(newpath)+1);
            shorten_path(newpath,shorter_newpath);
            erg = rename(shorter_oldpath,shorter_newpath);
      }   }
    return erg;
  }

global int my___findfirst(path,attrib,ffblk)
  var const char* path;
  var int attrib;
  var struct ffblk * ffblk;
  { var int erg = __findfirst(path,attrib,ffblk);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = __findfirst(shorter_path,attrib,ffblk);
      }
    return erg;
  }

global int my_mkdir(path,attrib)
  var CONST char* path;
  var long attrib;
  { var int erg = mkdir(path,attrib);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = mkdir(shorter_path,attrib);
      }
    return erg;
  }

global int my_open(path,flags)
  var CONST char* path;
  var int flags;
  { var int erg = open(path,flags);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = open(shorter_path,flags);
      }
    return erg;
  }

#define creat(path,mode)  open(path,O_RDWR|O_TRUNC|O_CREAT,mode)
global int my_creat(path,pmode)
  var CONST char* path;
  var int pmode;
  { var int erg = creat(path,pmode);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = creat(shorter_path,pmode);
      }
    return erg;
  }

global int my_spawnv(pmode,path,argv)
  var int pmode;
  var CONST char* path;
  var CONST char* CONST * argv;
  { var int erg = spawnv(pmode,path,argv);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = spawnv(pmode,shorter_path,argv);
      }
    return erg;
  }

#endif

# ============================================================================
