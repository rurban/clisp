/* signals.c -- changed by Bruno Haible, 7 January 1995 */

/* signals.c -- signal handling support for readline. */

/* Copyright (C) 1987, 1989, 1992 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library, a library for
   reading lines of text with interactive input and history editing.

   The GNU Readline Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 1, or
   (at your option) any later version.

   The GNU Readline Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   675 Mass Ave, Cambridge, MA 02139, USA. */
#define READLINE_LIBRARY

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>

#if defined (HAVE_UNISTD_H)
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if defined (HAVE_STDLIB_H)
#  include <stdlib.h>
#else
#  include "ansi_stdlib.h"
#endif /* HAVE_STDLIB_H */

#include <errno.h>
/* Not all systems declare ERRNO in errno.h... and some systems #define it! */
#if !defined (errno)
extern int errno;
#endif /* !errno */

#include "posixstat.h"

/* System-specific feature definitions and include files. */
#include "rldefs.h"

#if defined (GWINSZ_IN_SYS_IOCTL)
#  include <sys/ioctl.h>
#endif /* GWINSZ_IN_SYS_IOCTL */

/* Some standard library routines. */
#include "readline.h"
#include "history.h"

#if !defined(HAVE_TERMCAP_H)
#if !defined(__EMX__)
extern int tputs _PROTO((/* char* cp, int affcnt, void (*outcharfun)() */));
#endif
#endif

/* Forward declarations */
int rl_set_signals _PROTO((void));
int rl_clear_signals _PROTO((void));

extern int readline_echoing_p;
extern int rl_pending_input;

extern int _rl_meta_flag;

extern void _rl_set_screen_size _PROTO((int tty, int ignore_env));
extern void _rl_kill_kbd_macro _PROTO((void));
extern int rl_clean_up_for_exit _PROTO((void));
extern void _rl_redisplay_after_sigwinch _PROTO((void));

extern void free_undo_list _PROTO((void));

#if defined (VOID_SIGHANDLER)
#  define sighandler void
#else
#  define sighandler int
#endif /* VOID_SIGHANDLER */

/* This typedef is equivalant to the one for Function; it allows us
   to say SigHandler *foo = signal (SIGKILL, SIG_IGN); */
typedef sighandler SigHandler ();

#if defined (__GO32__)
#  undef HANDLE_SIGNALS
#endif /* __GO32__ */

#if defined (STATIC_MALLOC)
static char *xmalloc _PROTO((int bytes));
static char *xrealloc _PROTO((char *pointer, int bytes));
#else
extern char *xmalloc _PROTO((int bytes));
extern char *xrealloc _PROTO((char *pointer, int bytes));
#endif /* STATIC_MALLOC */


/* **************************************************************** */
/*					        		    */
/*			   Signal Handling                          */
/*								    */
/* **************************************************************** */

#if defined (HAVE_POSIX_SIGNALS)
typedef struct sigaction saved_sighandler;
#else
typedef struct { SigHandler *sa_handler; } saved_sighandler;
#endif

#if defined (SIGWINCH)
static saved_sighandler old_sigwinch;

static sighandler
rl_handle_sigwinch (sig)
     int sig;
{
  if (readline_echoing_p)
    {
      _rl_set_screen_size (fileno (rl_instream), 1);
      _rl_redisplay_after_sigwinch ();
    }

  if (old_sigwinch.sa_handler &&
      old_sigwinch.sa_handler != (SigHandler *)SIG_IGN &&
      old_sigwinch.sa_handler != (SigHandler *)SIG_DFL)
    (*old_sigwinch.sa_handler) (sig);
#if !defined (VOID_SIGHANDLER)
  return (0);
#endif /* VOID_SIGHANDLER */
}
#endif  /* SIGWINCH */

#if defined (HANDLE_SIGNALS)
/* Interrupt handling. */

static saved_sighandler old_int, old_alrm;
#if !defined (SHELL)
static saved_sighandler old_tstp, old_ttou, old_ttin;
#endif /* !SHELL */

/* Handle an interrupt character. */
static sighandler
rl_signal_handler (sig)
     int sig;
{
#if defined (HAVE_POSIX_SIGNALS)
  sigset_t set;
#else /* !HAVE_POSIX_SIGNALS */
#  if defined (HAVE_BSD_SIGNALS)
  long omask;
#  endif /* HAVE_BSD_SIGNALS */
#endif /* !HAVE_POSIX_SIGNALS */

#if !defined (HAVE_BSD_SIGNALS) && !defined (HAVE_POSIX_SIGNALS)
  /* Since the signal will not be blocked while we are in the signal
     handler, ignore it until rl_clear_signals resets the catcher. */
  if (sig == SIGINT || sig == SIGALRM)
    signal (sig, SIG_IGN);
#endif /* !HAVE_BSD_SIGNALS */

  switch (sig)
    {
    case SIGINT:
      {
	register HIST_ENTRY *entry;

	free_undo_list ();

	entry = current_history ();
	if (entry)
	  entry->data = (char *)NULL;
      }
      _rl_kill_kbd_macro ();
      rl_clear_message ();
      rl_init_argument ();

#if defined (SIGTSTP)
    case SIGTSTP:
    case SIGTTOU:
    case SIGTTIN:
#endif /* SIGTSTP */
    case SIGALRM:
      rl_clean_up_for_exit ();
      rl_deprep_terminal ();
      rl_clear_signals ();
      rl_pending_input = 0;

#if defined (HAVE_POSIX_SIGNALS)
      sigprocmask (SIG_BLOCK, (sigset_t *)NULL, &set);
      sigdelset (&set, sig);
#else /* !HAVE_POSIX_SIGNALS */
#  if defined (HAVE_BSD_SIGNALS)
      omask = sigblock (0);
#  endif /* HAVE_BSD_SIGNALS */
#endif /* !HAVE_POSIX_SIGNALS */

#if defined (__EMX__)
      signal (sig, SIG_ACK);
#endif

      kill (getpid (), sig);

      /* Let the signal that we just sent through.  */
#if defined (HAVE_POSIX_SIGNALS)
      sigprocmask (SIG_SETMASK, &set, (sigset_t *)NULL);
#else /* !HAVE_POSIX_SIGNALS */
#  if defined (HAVE_BSD_SIGNALS)
      sigsetmask (omask & ~(sigmask (sig)));
#  endif /* HAVE_BSD_SIGNALS */
#endif /* !HAVE_POSIX_SIGNALS */

      rl_prep_terminal (_rl_meta_flag);
      rl_set_signals ();
    }

#if !defined (VOID_SIGHANDLER)
  return (0);
#endif /* !VOID_SIGHANDLER */
}

static SigHandler *
rl_sigaction (sig, new_handler, old_handler)
     int sig;
     saved_sighandler *new_handler;
     saved_sighandler *old_handler;
{
#if defined (HAVE_POSIX_SIGNALS)
  sigaction (sig, new_handler, old_handler);
#else /* !HAVE_POSIX_SIGNALS */
  old_handler->sa_handler = signal (sig, new_handler->sa_handler);
#endif /* !HAVE_POSIX_SIGNALS */
  return (SigHandler *)old_handler->sa_handler;
}

static SigHandler *
rl_set_sighandler (sig, handler, old_handler)
     int sig;
     SigHandler *handler;
     saved_sighandler *old_handler;
{
#if defined (HAVE_POSIX_SIGNALS)
  struct sigaction act;

  act.sa_handler = handler;
  act.sa_flags = 0;
  sigemptyset (&act.sa_mask);
  sigaction (sig, &act, old_handler);
  return (SigHandler *)old_handler->sa_handler;
#else /* !HAVE_POSIX_SIGNALS */
  return old_handler->sa_handler = (SigHandler *)signal (sig, handler);
#endif /* !HAVE_POSIX_SIGNALS */
}

int
rl_set_signals ()
{
  saved_sighandler dummy;

  if (rl_set_sighandler (SIGINT, rl_signal_handler, &old_int)
      == (SigHandler *)SIG_IGN)
    rl_sigaction (SIGINT, &old_int, &dummy);

  if (rl_set_sighandler (SIGALRM, rl_signal_handler, &old_alrm)
      == (SigHandler *)SIG_IGN)
    rl_sigaction (SIGALRM, &old_alrm, &dummy);

#if !defined (SHELL)

#if defined (SIGTSTP)
  if (rl_set_sighandler (SIGTSTP, rl_signal_handler, &old_tstp)
      == (SigHandler *)SIG_IGN)
    rl_sigaction (SIGTSTP, &old_tstp, &dummy);
#endif /* SIGTSTP */
#if defined (SIGTTOU)
  rl_set_sighandler (SIGTTOU, rl_signal_handler, &old_ttou);
  rl_set_sighandler (SIGTTIN, rl_signal_handler, &old_ttin);

  if ((SigHandler *)old_tstp.sa_handler == (SigHandler *)SIG_IGN)
    {
      signal (SIGTTOU, SIG_IGN);
      signal (SIGTTIN, SIG_IGN);
    }
#endif /* SIGTTOU */

#endif /* !SHELL */

#if defined (SIGWINCH)
  rl_set_sighandler (SIGWINCH, rl_handle_sigwinch, &old_sigwinch);
#endif /* SIGWINCH */
  return 0;
}

int
rl_clear_signals ()
{
  saved_sighandler dummy;

  rl_sigaction (SIGINT, &old_int, &dummy);
  rl_sigaction (SIGALRM, &old_alrm, &dummy);

#if !defined (SHELL)

#if defined (SIGTSTP)
  rl_sigaction (SIGTSTP, &old_tstp, &dummy);
#endif

#if defined (SIGTTOU)
  rl_sigaction (SIGTTOU, &old_ttou, &dummy);
  rl_sigaction (SIGTTIN, &old_ttin, &dummy);
#endif /* SIGTTOU */

#endif /* !SHELL */

#if defined (SIGWINCH)
  rl_sigaction (SIGWINCH, &old_sigwinch, &dummy);
#endif

  return 0;
}
#endif  /* HANDLE_SIGNALS */
