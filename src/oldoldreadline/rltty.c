/* rltty.c -- changed by Bruno Haible, 13 May 1997 */

/* rltty.c -- functions to prepare and restore the terminal for readline's
   use. */

/* Copyright (C) 1992 Free Software Foundation, Inc.

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

extern char *xmalloc RL((int bytes));
extern char *xrealloc RL((char *pointer, int bytes));

#include <sys/types.h>
#include <stdio.h> /* for fileno() */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "rltty.h"

#ifdef HAVE_SYS_STREAM_H /* some USG systems may need this */
#include <sys/stream.h>
#endif
#ifdef _SEQUENT_ /* Sequent PTX V4 needs this for <sys/ptem.h> */
#include <sys/termios.h>
#endif
#ifdef HAVE_SYS_PTEM_H /* some USG systems may need this */
#include <sys/ptem.h>
#endif

/* Try to get FIONREAD if it is somewhere. */
#ifdef NEED_SYS_FILIO_H
#include <sys/filio.h>
#endif
#ifdef NEED_SYS_IOCTL_H
#include <sys/ioctl.h>
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

#include "rlxref.h"


/* **************************************************************** */
/*								    */
/*		      Saving and Restoring the TTY	    	    */
/*								    */
/* **************************************************************** */

/* Non-zero means that the terminal is in a prepped state. */
static int terminal_prepped = 0;

#if defined (NEW_TTY_DRIVER)

/* Standard flags, including ECHO. */
static int original_tty_flags = 0;

/* Local mode flags, like LPASS8. */
static int local_mode_flags = 0;

/* Terminal characters.  This has C-s and C-q in it. */
static struct tchars original_tchars;

/* Local special characters.  This has the interrupt characters in it. */
#if defined (TIOCGLTC)
static struct ltchars original_ltchars;
#endif

/* We use this to get and set the tty_flags. */
static struct sgttyb the_ttybuff;

/* Put the terminal in CBREAK mode so that we can detect key presses. */
void
rl_prep_terminal ()
{
#ifndef __GO32__
  int tty = fileno (rl_instream);

  if (terminal_prepped)
    return;

  _rl_block_sigint ();

  /* We always get the latest tty values.  Maybe stty changed them. */
  ioctl (tty, TIOCGETP, &the_ttybuff);
  original_tty_flags = the_ttybuff.sg_flags;

  readline_echoing_p = (original_tty_flags & ECHO);

#if defined (TIOCLGET)
  ioctl (tty, TIOCLGET, &local_mode_flags);
#endif

#if !defined (ANYP)
#  define ANYP (EVENP | ODDP)
#endif

  /* If this terminal doesn't care how the 8th bit is used,
     then we can use it for the meta-key.  We check by seeing
     if BOTH odd and even parity are allowed. */
  if (the_ttybuff.sg_flags & ANYP)
    {
#if defined (PASS8)
      the_ttybuff.sg_flags |= PASS8;
#endif

      /* Hack on local mode flags if we can. */
#if defined (TIOCLGET) && defined (LPASS8)
      {
	int flags;
	flags = local_mode_flags | LPASS8;
	ioctl (tty, TIOCLSET, &flags);
      }
#endif /* TIOCLGET && LPASS8 */
    }

#if defined (TIOCGETC)
  {
    struct tchars temp;

    ioctl (tty, TIOCGETC, &original_tchars);
    temp = original_tchars;

#if defined (USE_XON_XOFF)
    /* Get rid of C-s and C-q.
       We remember the value of startc (C-q) so that if the terminal is in
       xoff state, the user can xon it by pressing that character. */
    temp.t_stopc = -1;
    temp.t_startc = -1;

    /* If there is an XON character, bind it to restart the output. */
    if (temp.t_startc != -1)
      rl_bind_key (temp.t_startc, rl_restart_output);
#endif /* USE_XON_XOFF */

    /* If there is an EOF char, bind _rl_eof_char to it. */
    if (temp.t_eofc != -1)
      _rl_eof_char = temp.t_eofc;

#if defined (NO_KILL_INTR)
    /* Get rid of C-\ and C-c. */
    temp.t_intrc = temp.t_quitc = -1;
#endif /* NO_KILL_INTR */

    ioctl (tty, TIOCSETC, &temp);
  }
#endif /* TIOCGETC */

#if defined (TIOCGLTC)
  {
    struct ltchars temp;

    ioctl (tty, TIOCGLTC, &original_ltchars);
    temp = original_ltchars;

    /* Make the interrupt keys go away.  Just enough to make people
       happy. */
    temp.t_dsuspc = -1;	/* C-y */
    temp.t_lnextc = -1;	/* C-v */

    ioctl (tty, TIOCSLTC, &temp);
  }
#endif /* TIOCGLTC */

#if defined(__MSDOS__) || defined(__EMX__) || defined(__CYGWIN32__)
  _rl_eof_char = CTRL ('Z');
#endif /* __MSDOS__ || __EMX__ */

  the_ttybuff.sg_flags &= ~(ECHO | CRMOD);
  the_ttybuff.sg_flags |= CBREAK;
  ioctl (tty, TIOCSETN, &the_ttybuff);

  terminal_prepped = 1;

  _rl_unblock_sigint ();
#endif /* !__GO32__ */
}

/* Restore the terminal to its original state. */
void
rl_deprep_terminal ()
{
#ifndef __GO32__
  int tty = fileno (rl_instream);

  if (!terminal_prepped)
    return;

  _rl_block_sigint ();

  the_ttybuff.sg_flags = original_tty_flags;
  ioctl (tty, TIOCSETN, &the_ttybuff);
  readline_echoing_p = 1;

#if defined (TIOCLGET)
  ioctl (tty, TIOCLSET, &local_mode_flags);
#endif

#if defined (TIOCSLTC)
  ioctl (tty, TIOCSLTC, &original_ltchars);
#endif

#if defined (TIOCSETC)
  ioctl (tty, TIOCSETC, &original_tchars);
#endif
  terminal_prepped = 0;

  _rl_unblock_sigint ();
#endif /* !__GO32 */
}

#else  /* !defined (NEW_TTY_DRIVER) */

#if !defined (VMIN)
#define VMIN VEOF
#endif

#if !defined (VTIME)
#define VTIME VEOL
#endif

#ifndef __GO32__
#if defined (TERMIOS_TTY_DRIVER)
static struct termios otio;
#else
static struct termio otio;
#endif /* !TERMIOS_TTY_DRIVER */
#endif /* __GO32__ */

void
rl_prep_terminal ()
{
#ifndef __GO32__
  int tty = fileno (rl_instream);
#if defined (TERMIOS_TTY_DRIVER)
  struct termios tio;
#else
  struct termio tio;
#endif /* !TERMIOS_TTY_DRIVER */

  if (terminal_prepped)
    return;

  /* Try to keep this function from being INTerrupted.  We can do it
     on POSIX and systems with BSD-like signal handling. */
  _rl_block_sigint ();

#if defined (TERMIOS_TTY_DRIVER)
  tcgetattr (tty, &tio);
#else
  ioctl (tty, TCGETA, &tio);
#endif /* !TERMIOS_TTY_DRIVER */

  otio = tio;

  readline_echoing_p = (tio.c_lflag & ECHO);

#if defined (__EMX__)
  tio.c_lflag &= ~IDEFAULT;
#endif
  tio.c_lflag &= ~(ICANON|ECHO);

  if (otio.c_cc[VEOF] != _POSIX_VDISABLE)
    _rl_eof_char = otio.c_cc[VEOF];

#if defined(__MSDOS__) || defined(__EMX__) || defined(__CYGWIN32__)
  _rl_eof_char = CTRL ('Z');
#endif /* __MSDOS__ || __EMX__ */

#if defined (USE_XON_XOFF)
#if defined (IXANY)
  tio.c_iflag &= ~(IXON|IXOFF|IXANY);
#else
  /* `strict' Posix systems do not define IXANY. */
  tio.c_iflag &= ~(IXON|IXOFF);
#endif /* IXANY */
#endif /* USE_XON_XOFF */

  /* Only turn this off if we are using all 8 bits. */
  /* |ISTRIP|INPCK */
  tio.c_iflag &= ~(ISTRIP | INPCK);

  /* Make sure we differentiate between CR and NL on input. */
  tio.c_iflag &= ~(ICRNL | INLCR);

#if !defined (HANDLE_SIGNALS)
  tio.c_lflag &= ~ISIG;
#else
  tio.c_lflag |= ISIG;
#endif

  tio.c_cc[VMIN] = 1;
  tio.c_cc[VTIME] = 0;

  /* Turn off characters that we need on Posix systems with job control,
     just to be sure.  This includes ^Y and ^V.  This should not really
     be necessary.  */
#if defined (TERMIOS_TTY_DRIVER) && defined (_POSIX_JOB_CONTROL)

#if defined (VLNEXT)
  tio.c_cc[VLNEXT] = _POSIX_VDISABLE;
#endif

#if defined (VDSUSP)
  tio.c_cc[VDSUSP] = _POSIX_VDISABLE;
#endif

#endif /* POSIX && JOB_CONTROL */

#if defined (TERMIOS_TTY_DRIVER)
  tcsetattr (tty, TCSADRAIN, &tio);
  tcflow (tty, TCOON);		/* Simulate a ^Q. */
#else
  ioctl (tty, TCSETAW, &tio);
#if !defined (__EMX__)
  ioctl (tty, TCXONC, 1);	/* Simulate a ^Q. */
#endif
#endif /* !TERMIOS_TTY_DRIVER */

  terminal_prepped = 1;

  _rl_unblock_sigint ();
#endif /* !__GO32__ */
}

void
rl_deprep_terminal ()
{
#ifndef __GO32__
  int tty = fileno (rl_instream);

  if (!terminal_prepped)
    return;

  /* Try to keep this function from being INTerrupted.  We can do it
     on POSIX and systems with BSD-like signal handling. */
  _rl_block_sigint ();

#if defined (TERMIOS_TTY_DRIVER)
  tcsetattr (tty, TCSADRAIN, &otio);
  tcflow (tty, TCOON);		/* Simulate a ^Q. */
#else /* TERMIOS_TTY_DRIVER */
  ioctl (tty, TCSETAW, &otio);
#if !defined (__EMX__)
  ioctl (tty, TCXONC, 1);	/* Simulate a ^Q. */
#endif
#endif /* !TERMIOS_TTY_DRIVER */

  terminal_prepped = 0;

  _rl_unblock_sigint ();
#endif /* !__GO32__ */
}
#endif  /* NEW_TTY_DRIVER */


/* **************************************************************** */
/*								    */
/*			Bogus Flow Control      		    */
/*								    */
/* **************************************************************** */

void
rl_restart_output (count, key)
     int count, key;
{
  int fildes = fileno (rl_outstream);
#if defined (TIOCSTART)
#if defined (apollo)
  ioctl (&fildes, TIOCSTART, 0);
#else
  ioctl (fildes, TIOCSTART, 0);
#endif /* apollo */

#else
#  if defined (TERMIOS_TTY_DRIVER)
        tcflow (fildes, TCOON);
#  else
#    if defined (TCXONC)
        ioctl (fildes, TCXONC, TCOON);
#    endif /* TCXONC */
#  endif /* !TERMIOS_TTY_DRIVER */
#endif /* TIOCSTART */
}

void
rl_stop_output (count, key)
     int count, key;
{
  int fildes = fileno (rl_instream);

#if defined (TIOCSTOP)
# if defined (apollo)
  ioctl (&fildes, TIOCSTOP, 0);
# else
  ioctl (fildes, TIOCSTOP, 0);
# endif /* apollo */
#else
# if defined (TERMIOS_TTY_DRIVER)
  tcflow (fildes, TCOOFF);
# else
#   if defined (TCXONC)
  ioctl (fildes, TCXONC, TCOON);
#   endif /* TCXONC */
# endif /* !TERMIOS_TTY_DRIVER */
#endif /* TIOCSTOP */
}

/* **************************************************************** */
/*								    */
/*			Default Key Bindings			    */
/*								    */
/* **************************************************************** */
void
rltty_set_default_bindings (keymap)
     Keymap keymap;
{
#ifndef __GO32__

#if defined (NEW_TTY_DRIVER)
  struct sgttyb ttybuff;
  int tty = fileno (rl_instream);

  if (ioctl (tty, TIOCGETP, &ttybuff) != -1)
    {
      int erase, kill;

      erase = ttybuff.sg_erase;
      kill  = ttybuff.sg_kill;

      if (erase != -1 && keymap[erase].type == ISFUNC)
	keymap[erase].function = (Function *)rl_rubout;

      if (kill != -1 && keymap[kill].type == ISFUNC)
	keymap[kill].function = (Function *)rl_unix_line_discard;
    }

#if defined (TIOCGLTC)
  {
    struct ltchars lt;

    if (ioctl (tty, TIOCGLTC, &lt) != -1)
      {
	int erase, nextc;

	erase = lt.t_werasc;
	nextc = lt.t_lnextc;

	if (erase != -1 && keymap[erase].type == ISFUNC)
	  keymap[erase].function = (Function *)rl_unix_word_rubout;

	if (nextc != -1 && keymap[nextc].type == ISFUNC)
	  keymap[nextc].function = (Function *)rl_quoted_insert;
      }
  }
#endif /* TIOCGLTC */
#else /* not NEW_TTY_DRIVER */

#if defined (TERMIOS_TTY_DRIVER)
  struct termios ttybuff;
#else
  struct termio ttybuff;
#endif /* TERMIOS_TTY_DRIVER */
  int tty = fileno (rl_instream);

#if defined (TERMIOS_TTY_DRIVER)
  if (tcgetattr (tty, &ttybuff) != -1)
#else
  if (ioctl (tty, TCGETA, &ttybuff) != -1)
#endif /* !TERMIOS_TTY_DRIVER */
    {
      int erase, kill;

      erase = ttybuff.c_cc[VERASE];
      kill = ttybuff.c_cc[VKILL];

      if (erase != _POSIX_VDISABLE &&
	  keymap[(unsigned char)erase].type == ISFUNC)
	keymap[(unsigned char)erase].function = rl_rubout;

      if (kill != _POSIX_VDISABLE &&
	  keymap[(unsigned char)kill].type == ISFUNC)
	keymap[(unsigned char)kill].function = rl_unix_line_discard;

#if defined (VLNEXT) && defined (TERMIOS_TTY_DRIVER)
      {
	int nextc;

	nextc = ttybuff.c_cc[VLNEXT];

	if (nextc != _POSIX_VDISABLE &&
	    keymap[(unsigned char)nextc].type == ISFUNC)
	  keymap[(unsigned char)nextc].function = rl_quoted_insert;
      }
#endif /* VLNEXT && TERMIOS_TTY_DRIVER */

#if defined (VWERASE)
      {
	int werase;

	werase = ttybuff.c_cc[VWERASE];

	if (werase != _POSIX_VDISABLE &&
	    keymap[(unsigned char)werase].type == ISFUNC)
	  keymap[(unsigned char)werase].function = rl_unix_word_rubout;
      }
#endif /* VWERASE */
    }
#endif /* !NEW_TTY_DRIVER */
#endif /* def __GO32__ */

#if defined(__MSDOS__) || defined(__EMX__) || defined(__CYGWIN32__)
  _rl_eof_char = CTRL ('Z');
#endif /* __MSDOS__ || __EMX__ */
}
