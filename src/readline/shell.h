/* shell.h -- utility functions normally provided by bash. */

/* Copyright (C) 1998 Free Software Foundation, Inc.

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

#if !defined (_SHELL_H_)
#define _SHELL_H_

#if defined (READLINE_LIBRARY)
#  include "ansi_proto.h"
#else
#  include <readline/ansi_proto.h>
#endif

extern char *(savestring) _PROTO((char *s));
extern char *single_quote _PROTO((char *string));
extern void set_lines_and_columns _PROTO((int lines, int cols));
extern char *get_env_value _PROTO((char *varname));

#endif /* _SHELL_H_ */
