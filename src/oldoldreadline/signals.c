/* signals.c -- changed by Bruno Haible, 17 August 1994 */

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

#include "sysdep.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for kill(), getpid() */
#endif

#include <signal.h> /* for signal() etc. */


/* Define some macros for dealing with assorted signalling disciplines.

   These macros provide a way to use signal blocking and disabling
   without smothering your code in a pile of #ifdef's.

   SIGNALS_UNBLOCK;			Stop blocking all signals.

   {
     SIGNALS_DECLARE_SAVED (name);	Declare a variable to save the
					signal blocking state.
	...
     SIGNALS_BLOCK (SIGSTOP, name);	Block a signal, and save the previous
					state for restoration later.
	...
     SIGNALS_RESTORE (name);		Restore previous signals.
   }

*/

#ifdef SIGNALBLOCK_POSIX
							/* POSIX signals */

#define	SIGNALS_UNBLOCK \
      do { sigset_t set;	\
	sigemptyset (&set);	\
	sigprocmask (SIG_SETMASK, &set, (sigset_t *)NULL);	\
      } while (0)

#define	SIGNALS_DECLARE_SAVED(name)	sigset_t name

#define	SIGNALS_BLOCK(SIG, saved)	\
	do { sigset_t set;		\
	  sigemptyset (&set);		\
	  sigaddset (&set, SIG);	\
	  sigprocmask (SIG_BLOCK, &set, &saved);	\
	} while (0)

#define	SIGNALS_RESTORE(saved)		\
  sigprocmask (SIG_SETMASK, &saved, (sigset_t *)NULL)


#else	/* SIGNALBLOCK_POSIX */
#ifdef SIGNALBLOCK_BSD
							/* BSD signals */

#define	SIGNALS_UNBLOCK			sigsetmask (0)
#define	SIGNALS_DECLARE_SAVED(name)	int name
#define	SIGNALS_BLOCK(SIG, saved)	saved = sigblock (sigmask (SIG))
#define	SIGNALS_RESTORE(saved)		sigsetmask (saved)

#define HAVE_BSD_SIGNALS


#else  /* SIGNALBLOCK_BSD */
#ifdef SIGNALBLOCK_SYSV
							/* System V signals */

#define SIGNALS_UNBLOCK \
	do { int sig; for (sig=1; sig<=64; sig++) { sigrelse(sig); } } while (0)
#define SIGNALS_DECLARE_SAVED(name)	int name
#define SIGNALS_BLOCK(SIG, saved)	saved = SIG, sighold (SIG)
#define SIGNALS_RESTORE(saved)		sigrelse (saved)


#else  /* SIGNALBLOCK_SYSV */
							/* None of the Above */

#define	SIGNALS_UNBLOCK			/* nothing */
#define	SIGNALS_DECLARE_SAVED(name)	/* nothing */
#define	SIGNALS_BLOCK(SIG, saved)	/* nothing */
#define	SIGNALS_RESTORE(saved)		/* nothing */

#endif /* SIGNALBLOCK_SYSV */
#endif /* SIGNALBLOCK_BSD */
#endif /* SIGNALBLOCK_POSIX */

/*  End of signal handling definitions.  */

/* Return type of signal handlers (usually called RETSIGTYPE): */
#ifdef RETSIGTYPE_VOID
#define sighandler void
#else
#define sighandler int
#endif

/* This typedef is equivalant to the one for Function; it allows us
   to say SigHandler *foo = signal (SIGKILL, SIG_IGN); */
typedef sighandler SigHandler ();

#ifdef MINIMAL
#ifdef __GO32__
#include <pc.h>
#endif
#undef HANDLE_SIGNALS
#endif

#include "rlxref.h"


/* **************************************************************** */
/*					        		    */
/*			   Preventing Signals                       */
/*								    */
/* **************************************************************** */

static int sigint_blocked = 0;

SIGNALS_DECLARE_SAVED(_rl_sigint_saved);

void
_rl_block_sigint ()
{
  if (sigint_blocked)
    return;

  SIGNALS_BLOCK (SIGINT, _rl_sigint_saved);
  sigint_blocked = 1;
}

void
_rl_unblock_sigint ()
{
  if (!sigint_blocked)
    return;

  SIGNALS_RESTORE (_rl_sigint_saved);
  sigint_blocked = 0;
}


/* **************************************************************** */
/*					        		    */
/*			   Signal Handling                          */
/*								    */
/* **************************************************************** */

#if defined (SIGWINCH)
static SigHandler *old_sigwinch = (SigHandler *)NULL;

static sighandler
rl_handle_sigwinch (sig)
     int sig;
{
  char *term;

  term = rl_terminal_name;

  if (readline_echoing_p)
    {
      if (!term)
	term = getenv ("TERM");
      if (!term)
	term = "dumb";
      rl_reset_terminal (term);
#if defined (NOTDEF)
      cr ();
      rl_forced_update_display ();
#endif /* NOTDEF */
    }

  if (old_sigwinch &&
      old_sigwinch != (SigHandler *)SIG_IGN &&
      old_sigwinch != (SigHandler *)SIG_DFL)
    (*old_sigwinch) (sig);
#ifndef RETSIGTYPE_VOID
  return 0;
#endif
}
#endif  /* SIGWINCH */

#if defined (HANDLE_SIGNALS)
/* Interrupt handling. */

/* To save signal handlers, we use sigaction() if it exists,
   signal() otherwise. */
#if defined (HAVE_SIGACTION)
typedef struct sigaction saved_sighandler;
#else
typedef struct { SigHandler *sa_handler; } saved_sighandler;
#endif

/* Store the new_action as the old action, return the old action in
   old_action, just like sigaction(). Return the old handler, like signal(). */
static SigHandler *
rl_sigaction (sig, new_action, old_action)
     int sig;
     saved_sighandler *new_action;
     saved_sighandler *old_action;
{
#if defined (HAVE_SIGACTION)
  sigaction (sig, new_action, old_action);
  return old_action->sa_handler;
#else
  return old_action->sa_handler = signal (sig, new_action->sa_handler);
#endif
}

/* Installs a new handler, return the old action in old_action. */
static SigHandler *
rl_set_sighandler (sig, new_handler, old_action)
     int sig;
     SigHandler *new_handler;
     saved_sighandler *old_action;
{
#if defined (HAVE_SIGACTION)
  struct sigaction act;

  /* Use sigaction() to fill up old_action. */
  act.sa_handler = new_handler;
  act.sa_flags = 0;
  sigemptyset (&act.sa_mask);
  sigaddset (&act.sa_mask, sig);
  sigaction (sig, &act, old_action);

  /* But install new_handler using signal(),
     since filling sa_flags correctly would be difficult to do right. */
  signal (sig, new_handler);

  return (SigHandler *)old_action->sa_handler;
#else
  return old_action->sa_handler = signal (sig, new_handler);
#endif
}

static saved_sighandler old_int, old_alrm;
#if defined (SIGTSTP)
static saved_sighandler old_tstp, old_ttou, old_ttin;
#endif

/* Handle an interrupt character. */
static sighandler
rl_signal_handler (sig)
     int sig;
{
#if !(defined(SIGNALBLOCK_BSD) && defined(SIGNAL_NEED_UNBLOCK))
  /* Since the signal will not be blocked while we are in the signal
     handler, ignore it until rl_clear_signals resets the catcher. */
  if (sig == SIGINT || sig == SIGALRM)
    signal (sig, SIG_IGN);
#endif

  switch (sig)
    {
    case SIGINT:
      free_undo_list ();

      {
	register HIST_ENTRY *entry;

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

#if defined (__EMX__)
      signal (sig, SIG_ACK);
#endif

      /* Pass the signal to the program's original handler. */
      kill (getpid (), sig);

      SIGNALS_UNBLOCK;

      rl_prep_terminal ();
      rl_set_signals ();
    }

#ifndef RETSIGTYPE_VOID
  return 0;
#endif
}

void
rl_set_signals ()
{
  saved_sighandler dummy;

  if (rl_set_sighandler (SIGINT, rl_signal_handler, &old_int)
      == (SigHandler *)SIG_IGN)
    rl_sigaction (SIGINT, &old_int, &dummy);

  if (rl_set_sighandler (SIGALRM, rl_signal_handler, &old_alrm)
      == (SigHandler *)SIG_IGN)
    rl_sigaction (SIGALRM, &old_alrm, &dummy);

#if defined (SIGTSTP)
  if (rl_set_sighandler (SIGTSTP, rl_signal_handler, &old_tstp)
      == (SigHandler *)SIG_IGN)
    rl_sigaction (SIGTSTP, &old_tstp, &dummy);
  else
    {
      rl_set_sighandler (SIGTTOU, rl_signal_handler, &old_ttou);
      rl_set_sighandler (SIGTTIN, rl_signal_handler, &old_ttin);
    }
#endif

#if defined (SIGWINCH)
  old_sigwinch = (SigHandler *)signal (SIGWINCH, rl_handle_sigwinch);
#endif
}

void
rl_clear_signals ()
{
  saved_sighandler dummy;

  rl_sigaction (SIGINT, &old_int, &dummy);
  rl_sigaction (SIGALRM, &old_alrm, &dummy);

#if defined (SIGTSTP)
  rl_sigaction (SIGTSTP, &old_tstp, &dummy);
  if (old_tstp.sa_handler != (SigHandler *)SIG_IGN)
    {
      rl_sigaction (SIGTTOU, &old_ttou, &dummy);
      rl_sigaction (SIGTTIN, &old_ttin, &dummy);
    }
#endif

#if defined (SIGWINCH)
      signal (SIGWINCH, old_sigwinch);
#endif
}
#endif  /* HANDLE_SIGNALS */
