/* sysdep.h -- common include file for the readline library */
/* Bruno Haible 5.12.1994 */


#ifndef _RL_SYSDEP_H
#define _RL_SYSDEP_H

#if !(defined(__MSDOS__) || defined(__EMX__) || defined(WIN32))

#include "config.h"

#else /* non-Unix systems can't execute the configure script */

#if defined(__EMX__) /* emx >= 0.8h */
#if defined(HAVE_SYS_EMX_H) /* emx <= 0.9b, define this in the makefile */
#include <sys/emx.h>
#endif
#define STDC_HEADERS
#define HAVE_UNISTD_H
#define DIRENT
#define HAVE_STDARG_H
#define HAVE_VARARGS_H
#define HAVE_TERMIO_H
#define HAVE_SYS_TERMIO_H
#define HAVE_SGTTY_H
#define HAVE_FIONREAD
#define NEED_SYS_IOCTL_H
#define HAVE_SELECT
#define HAVE_ALLOCA_H
#define HAVE_STRCHR
#define HAVE_STRRCHR
#define HAVE_STRPBRK
#define RETSIGTYPE_VOID
#if !defined(HAVE_SYS_EMX_H) || defined(_SIGSET_T) /* emx >= 0.9a */
#define HAVE_SIGACTION
#endif
#endif

#if defined(__GO32__) /* djgpp 1.10 */
#define MINIMAL
#define STDC_HEADERS
#define HAVE_UNISTD_H
#define DIRENT
#define HAVE_STRCHR
#define HAVE_STRRCHR
#define HAVE_STRPBRK
#define RETSIGTYPE_VOID
#endif

#if defined(WIN32) && defined(_MSC_VER) /* MSVC */
#define MINIMAL
#endif

#if defined(WIN32) && !defined(_MSC_VER) /* mingw32 */
#define MINIMAL
/* probably HAVE_TERMIOS_H HAVE_TCGETATTR HAVE_TCFLOW */
#endif

#endif


/* For prototypes:  extern int foo RL((int x, int y)); */
#if defined(__STDC__) || defined(__cplusplus)
#define RL(args) args
#else
#define RL(args) ()
#endif

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#ifndef alloca
#ifndef __osf__
extern void* alloca RL((int size));
#endif
#endif
#else
#ifdef _AIX
 #pragma alloca /* AIX requires this to be the first thing in the file. */
#else
extern void* alloca RL((int size)); /* either from libc.a or from alloca.o */
#endif /* _AIX */
#endif /* HAVE_ALLOCA_H */
#endif /* __GNUC__ */

#ifdef STDC_HEADERS
#include <stdlib.h> /* declares malloc(), realloc(), free(), getenv(), abort(), qsort() */
#endif
extern char* getenv RL((/* [const] char* string */));
/* SCO systems may need "#include <malloc.h>" ?? */

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h> /* declares stat(), open(), read(), write(), close(),
                                fileno(), fcntl(), ioctl(),
                                kill (), getpid() */
#endif
#ifdef __EMX__
#include <io.h> /* declares stat(), open(), read(), write(), close(), ioctl(), select() */
#endif

#include <string.h> /* declares strlen(), strcmp(), strncmp(), strcpy(), strncpy(), strcat()
                                and perhaps strchr(), strrchr(), strpbrk() */

#ifdef HAVE_STRCHR
/* <string.h> declares strchr() */
#else
/* Systems that don't have strchr should at least have index */
#define strchr index
extern char* strchr();
#endif

#ifdef HAVE_STRRCHR
/* <string.h> declares strrchr() */
#else
/* Systems that don't have strrchr should at least have rindex */
#define strrchr rindex
extern char* strrchr();
#endif

/* Declaration of dirent, opendir(), readdir(), closedir() */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
typedef struct dirent dirent;
#define D_NAMLEN(d) strlen((d)->d_name)
#else
#ifdef SYSNDIR
#include <sys/ndir.h>
#else
#ifdef SYSDIR
#include <sys/dir.h>
#else
#ifdef NDIR
#include <ndir.h>
#else
#include <dir.h>
#endif
#endif
#endif
typedef struct direct dirent;
#define D_NAMLEN(d) (d)->d_namlen
#endif

/* If on, then readline handles signals in a way that doesn't screw. */
#if !defined(MINIMAL)
#define HANDLE_SIGNALS
#endif

#endif
