/* rltty.h -- changed by Bruno Haible, 23 November 1994 */

/* rltty.h -- choose the tty driver for readline's use. */

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

/* Choose the tty driver:
   NEW_TTY_DRIVER      uses  struct sgttyb  and ioctl() with TIOCGETP, TIOCSETN.
   TERMIO_TTY_DRIVER   uses  struct termio  and ioctl() with TCGETA, TCSETAW.
   TERMIOS_TTY_DRIVER  uses  struct termios and tcgetattr(), tcflow().
*/

#if defined(HAVE_TERMIOS_H) && defined(HAVE_TCGETATTR) && defined(HAVE_TCFLOW)

#define TERMIOS_TTY_DRIVER /* Posix terminal I/O */
#include <termios.h>
extern int tcgetattr RL((int fd, struct termios * termios_p));
extern int tcsetattr RL((/* int fd, int optional_actions, [const] struct termios * termios_p */));
extern int tcflow RL((int fd, int action));

#else
#if defined(HAVE_TERMIO_H) || defined(HAVE_SYS_TERMIO_H) || defined(__MSDOS__)

#define TERMIO_TTY_DRIVER /* System V terminal I/O */
#ifndef __GO32__
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#else
#include <termio.h>
#endif
#ifndef TCOON
#define TCOON 1
#endif
#endif

#else

#define NEW_TTY_DRIVER /* old BSD terminal I/O */
#include <sgtty.h>
#include <sys/ioctl.h>

#endif
#endif
