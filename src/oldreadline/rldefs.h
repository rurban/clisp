/* rldefs.h -- changed by Bruno Haible, 16 July 1997 */

/* rldefs.h -- an attempt to isolate some of the system-specific defines
   for readline.  This should be included after any files that define
   system-specific constants like _POSIX_VERSION or USG. */

/* Copyright (C) 1987,1989 Free Software Foundation, Inc.

   This file contains the Readline Library (the Library), a set of
   routines for providing Emacs style line input to programs that ask
   for it.

   The Library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   The Library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   675 Mass Ave, Cambridge, MA 02139, USA. */

#if !defined (_RLDEFS_H)
#define _RLDEFS_H

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (READLINE_LIBRARY)
#  include "ansi_proto.h"
#else
#  include <readline/ansi_proto.h>
#endif

#if !defined (PRAGMA_ALLOCA)
#  include "memalloc.h"
#endif

#if defined (HAVE_TERMCAP_H)
#  include <termcap.h>
#endif /* HAVE_TERMCAP_H */

/* Signals */
#if defined (SIGNALBLOCK_POSIX)
#  define HAVE_POSIX_SIGNALS
#else
#  if defined (SIGNALBLOCK_BSD)
#    define HAVE_BSD_SIGNALS
#  else
#    if defined (SIGNALBLOCK_SYSV)
#      define HAVE_USG_SIGHOLD
#    endif
#  endif
#endif

#include <fcntl.h>
#if defined (OPEN_NEEDS_SYS_FILE_H)
#  include <sys/file.h>
#endif
#ifndef O_NDELAY
#  define O_NDELAY O_NONBLOCK /* Posix-style non-blocking i/o */
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

/* Choose the tty driver:
   NEW_TTY_DRIVER      uses  struct sgttyb  and ioctl() with TIOCGETP, TIOCSETN.
   TERMIO_TTY_DRIVER   uses  struct termio  and ioctl() with TCGETA, TCSETAW.
   TERMIOS_TTY_DRIVER  uses  struct termios and tcgetattr(), tcflow().
*/

#if defined(HAVE_TERMIOS_H) && defined(HAVE_TCGETATTR) && defined(HAVE_TCFLOW)

#  define TERMIOS_TTY_DRIVER /* Posix terminal I/O */
#  include <termios.h>
   extern int tcgetattr _PROTO((int fd, struct termios * termios_p));
   extern int tcsetattr _PROTO((/* int fd, int optional_actions, [const] struct termios * termios_p */));
   extern int tcflow _PROTO((int fd, int action));

#else
#  if defined(HAVE_TERMIO_H) || defined(HAVE_SYS_TERMIO_H) || defined(__MSDOS__)

#    define TERMIO_TTY_DRIVER /* System V terminal I/O */
#    ifndef __GO32__
#      ifdef HAVE_SYS_TERMIO_H
#        include <sys/termio.h>
#      else
#        include <termio.h>
#      endif
#      ifndef TCOON
#        define TCOON 1
#      endif
#    endif

#  else

#    define NEW_TTY_DRIVER /* old BSD terminal I/O */
#    include <sgtty.h>
#    include <sys/ioctl.h>

#  endif
#endif

#if defined (HAVE_SYS_STREAM_H)
#  include <sys/stream.h>  /* needed for <sys/ptem.h> on SCO */
#endif /* HAVE_SYS_STREAM_H */
#if defined (_SEQUENT_)
#  include <sys/termios.h>  /* needed for <sys/ptem.h> on Sequent PTX V4 */
#endif
#if defined (HAVE_SYS_PTEM_H)
#  include <sys/ptem.h>  /* needed for `struct winsize' on SCO */
#endif /* HAVE_SYS_PTEM_H */
#if defined (HAVE_SYS_PTE_H)
#  include <sys/pte.h>
#endif /* HAVE_SYS_PTE_H */

/* Try to get FIONREAD if it is somewhere. */
#ifdef NEED_SYS_FILIO_H
#  include <sys/filio.h>
#endif
#ifdef NEED_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif

/* Declaration of dirent, opendir(), readdir(), closedir() */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#  include <dirent.h>
   typedef struct dirent dirent;
#  define D_NAMLEN(d) strlen ((d)->d_name)
#else
#  ifdef SYSNDIR
#    include <sys/ndir.h>
#  else
#    ifdef SYSDIR
#      include <sys/dir.h>
#    else
#      ifdef NDIR
#        include <ndir.h>
#      else
#        include <dir.h>
#      endif
#    endif
#  endif
   typedef struct direct dirent;
#  define D_NAMLEN(d) ((d)->d_namlen)
#endif

/* Posix macro to check file in statbuf for directory-ness.
   This requires that <sys/stat.h> be included before this test. */
#if defined (S_IFDIR) && !defined (S_ISDIR)
#  define S_ISDIR(m) (((m)&S_IFMT) == S_IFDIR)
#endif

/* Decide which flavor of the header file describing the C library
   string functions to include and include it. */

#if defined (HAVE_STRING_H)
#  include <string.h>
#else /* !HAVE_STRING_H */
#  include <strings.h>
#endif /* !HAVE_STRING_H */

#if !defined (strchr) && !defined (__STDC__)
extern char *strchr (), *strrchr ();
#endif /* !strchr && !__STDC__ */

#if defined (HAVE_VARARGS_H)
#  include <varargs.h>
#endif /* HAVE_VARARGS_H */

/* This is needed to include support for TIOCGWINSZ and window resizing. */
#if defined (OSF1) || defined (BSD386) || defined (NetBSD) || \
    defined (__BSD_4_4__) || defined (FreeBSD) || defined (_386BSD) || \
    defined (AIX)
#  define GWINSZ_IN_SYS_IOCTL
#endif

/* Define _POSIX_VDISABLE if we are not using the `new' tty driver and
   it is not already defined.  It is used both to determine if a
   special character is disabled and to disable certain special
   characters.  Posix systems should set to 0, USG systems to -1. */
#if !defined (NEW_TTY_DRIVER) && !defined (_POSIX_VDISABLE)
#  if defined (_SVR4_VDISABLE)
#    define _POSIX_VDISABLE _SVR4_VDISABLE
#  else
#    if defined (_POSIX_VERSION) || defined (__EMX__)
#      define _POSIX_VDISABLE 0
#    else /* !_POSIX_VERSION */
#      define _POSIX_VDISABLE -1
#    endif /* !_POSIX_VERSION */
#  endif /* !_SVR4_VDISABLE */
#endif /* !NEW_TTY_DRIVER && !_POSIX_VDISABLE */

#if !defined (emacs_mode)
#  define no_mode -1
#  define vi_mode 0
#  define emacs_mode 1
#endif

/* If you cast map[key].function to type (Keymap) on a Cray,
   the compiler takes the value of map[key].function and
   divides it by 4 to convert between pointer types (pointers
   to functions and pointers to structs are different sizes).
   This is not what is wanted. */
#if defined (CRAY)
#  define FUNCTION_TO_KEYMAP(map, key)	(Keymap)((int)map[key].function)
#  define KEYMAP_TO_FUNCTION(data)	(Function *)((int)(data))
#else
#  define FUNCTION_TO_KEYMAP(map, key)	(Keymap)(map[key].function)
#  define KEYMAP_TO_FUNCTION(data)	(Function *)(data)
#endif

#ifndef savestring
extern char *xmalloc _PROTO((int bytes));
#define savestring(x) strcpy (xmalloc (1 + strlen (x)), (x))
#endif

/* Possible values for _rl_bell_preference. */
#define NO_BELL 0
#define AUDIBLE_BELL 1
#define VISIBLE_BELL 2

/* CONFIGURATION SECTION */
#include "rlconf.h"

#endif /* !_RLDEFS_H */
