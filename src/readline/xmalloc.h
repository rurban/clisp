/* xmalloc.h -- failsafe memory allocation. */

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

#if !defined (_XMALLOC_H_)
#define _XMALLOC_H_

#if defined (READLINE_LIBRARY)
#  include "rlstdc.h"
#else
#  include <readline/rlstdc.h>
#endif

extern char *xmalloc _PROTO((int bytes));
extern char *xrealloc _PROTO((void *pointer, int bytes));
extern void xfree _PROTO((char *string));

#endif /* _XMALLOC_H_ */
