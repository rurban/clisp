/* chardefs.h -- Character definitions for readline. */

/* Copyright (C) 1994 Free Software Foundation, Inc.

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

#ifndef _CHARDEFS_H
#define _CHARDEFS_H

#include <ctype.h>

#if defined (HAVE_STRING_H)
#  include <string.h>
#else
#  include <strings.h>
#endif /* HAVE_STRING_H */

#ifndef whitespace
#define whitespace(c) (((c) == ' ') || ((c) == '\t'))
#endif

#ifdef CTRL
#undef CTRL
#endif

/* Define ISOLATIN if we are supporting all ISO Latin-1 characters. */
#if defined(linux) || defined(_AIX)
#  define ISOLATIN
#endif

/* Define DOSCHARS if we are supporting all the MSDOS character set. */
#if defined(__MSDOS__) || defined(__EMX__) || defined(COHERENT)
#  define DOSCHARS
#endif

/* Define NEXTCHARS if we are supporting all the NeXT character set. */
#if defined(NeXT)
#  define NEXTCHARS
#endif

/* Some character stuff. */
#define control_character_threshold 0x020   /* Smaller than this is control. */
#define control_character_mask 0x1f	    /* 0x20 - 1 */
#define meta_character_threshold 0x07f	    /* Larger than this is Meta. */
#define control_character_bit 0x40	    /* 0x000000, must be off. */
#define meta_character_bit 0x080	    /* x0000000, must be on. */
#define largest_char 255		    /* Largest character value. */

#define CTRL_CHAR(c) ((c) < control_character_threshold)
#define META_CHAR(c) ((c) > meta_character_threshold && (c) <= largest_char)

#define CTRL(c) ((c) & control_character_mask)
#define META(c) ((c) | meta_character_bit)

#define UNMETA(c) ((c) & (~meta_character_bit))
#define UNCTRL(c) to_upper(((c)|control_character_bit))

/* Old versions
#define lowercase_p(c) (((c) > ('a' - 1) && (c) < ('z' + 1)))
#define uppercase_p(c) (((c) > ('A' - 1) && (c) < ('Z' + 1)))
#define to_upper(c) (lowercase_p(c) ? ((c) - 32) : (c))
#define to_lower(c) (uppercase_p(c) ? ((c) + 32) : (c))
#define digit_p(c)  ((c) >= '0' && (c) <= '9')
*/

#ifdef ISOLATIN
#define _lowercase_p(c) \
  (((c) >= 'a' && (c) <= 'z') || ((c) >= 223 && (c) <= 255 && (c) != 247))
#define _uppercase_p(c) \
  (((c) >= 'A' && (c) <= 'Z') || ((c) >= 192 && (c) <= 222 && (c) != 215))
#define _to_upper(c) \
  ((c) >= 'a' && (c) <= 'z' ? (c) - 32 : \
   (c) >= 224 && (c) <= 225 && (c) != 247 ? (c) - 32 : \
   (c))
#define _to_lower(c) \
  ((c) >= 'A' && (c) <= 'Z' ? (c) + 32 : \
   (c) >= 192 && (c) <= 222 && (c) != 215 ? (c) + 32 : \
   (c))
#endif
#ifdef DOSCHARS
#define _lowercase_p(c) \
  (((c) >= 'a' && (c) <= 'z')    \
   || ((c) >= 129 && (c) <= 141) \
   || ((c) == 145)               \
   || ((c) >= 147 && (c) <= 152) \
   || ((c) >= 160 && (c) <= 164))
#define _uppercase_p(c) \
  (((c) >= 'A' && (c) <= 'Z')    \
   || ((c) == 128)               \
   || ((c) >= 142 && (c) <= 144) \
   || ((c) == 146)               \
   || ((c) >= 153 && (c) <= 154) \
   || ((c) == 165))
#define _to_upper(c) \
  ((c) >= 'a' && (c) <= 'z' ? (c) - 32 : \
   (c) == 0x87 ? 0x80 : \
   (c) == 0x81 ? 0x9A : \
   (c) == 0x82 ? 0x90 : \
   (c) == 0x84 ? 0x8E : \
   (c) == 0x86 ? 0x8F : \
   (c) == 0x91 ? 0x92 : \
   (c) == 0x94 ? 0x99 : \
   (c) == 0xA4 ? 0xA5 : \
   (c))
#define _to_lower(c) \
  ((c) >= 'A' && (c) <= 'Z' ? (c) + 32 : \
   (c) == 0x80 ? 0x87 : \
   (c) == 0x9A ? 0x81 : \
   (c) == 0x90 ? 0x82 : \
   (c) == 0x8E ? 0x84 : \
   (c) == 0x8F ? 0x86 : \
   (c) == 0x92 ? 0x91 : \
   (c) == 0x99 ? 0x94 : \
   (c) == 0xA5 ? 0xA4 : \
   (c))
#endif
#ifdef NEXTCHARS
#define _lowercase_p(c) \
  (((c) >= 'a' && (c) <= 'z') \
   || ((c) >= 213 && (c) <= 231 && (c) != 225 && (c) != 227) \
   || ((c) >= 236 && (c) <= 253 && (c) != 245 && (c) != 248) \
  )
#define _uppercase_p(c) \
  (((c) >= 'A' && (c) <= 'Z') \
   || ((c) >= 129 && (c) <= 156) \
   || ((c) == 225) \
   || ((c) >= 233 && (c) <= 234) \
  )
#define _to_upper(c) \
  ((c) >= 'a' && (c) <= 'z' ? (c) - 32 : \
   (c) >= 213 && (c) <= 224 ? (c) - 84 : \
   (c) == 226 ? 141 : \
   (c) >= 228 && (c) <= 231 ? (c) - 86 : \
   (c) >= 236 && (c) <= 240 ? (c) - 90 : \
   (c) == 241 ? 225 : \
   (c) >= 242 && (c) <= 244 ? (c) - 91 : \
   (c) >= 246 && (c) <= 247 ? (c) - 92 : \
   (c) >= 249 && (c) <= 250 ? (c) - 16 : \
   (c) == 252 ? 156 : \
   (c))
#define _to_lower(c) \
  ((c) >= 'A' && (c) <= 'Z' ? (c) + 32 : \
   (c) >= 129 && (c) <= 140 ? (c) + 84 : \
   (c) == 141 ? 226 : \
   (c) >= 142 && (c) <= 145 ? (c) + 86 : \
   (c) >= 146 && (c) <= 150 ? (c) + 90 : \
   (c) >= 151 && (c) <= 153 ? (c) + 91 : \
   (c) >= 154 && (c) <= 155 ? (c) + 92 : \
   (c) == 156 ? 252 : \
   (c) == 225 ? 241 : \
   (c) >= 233 && (c) <= 234 ? (c) + 16 : \
   (c))
#endif
#ifdef _lowercase_p
/* convert to `unsigned char' first, to be 8-bit clean */
#  define lowercase_p(c) (_lowercase_p((unsigned char)(c)))
#  define uppercase_p(c) (_uppercase_p((unsigned char)(c)))
#  define to_upper(c) (char)(_to_upper((unsigned char)(c)))
#  define to_lower(c) (char)(_to_lower((unsigned char)(c)))
#else
#define lowercase_p(c) (islower(c))
#define uppercase_p(c) (isupper(c))
#ifndef to_upper
#  define to_upper(c) (islower(c) ? toupper(c) : (c))
#  define to_lower(c) (isupper(c) ? tolower(c) : (c))
#endif
#endif

#define pure_alphabetic(c) (lowercase_p(c) || uppercase_p(c))
#define isletter(c) pure_alphabetic(c)

#define digit_p(x)  (isdigit (x))

#ifndef digit_value
#define digit_value(x) ((x) - '0')
#endif

#ifndef NEWLINE
#define NEWLINE '\n'
#endif

#ifndef RETURN
#define RETURN CTRL('M')
#endif

#ifndef RUBOUT
#define RUBOUT 0x7f
#endif

#ifndef TAB
#define TAB '\t'
#endif

#ifdef ABORT_CHAR
#undef ABORT_CHAR
#endif
#define ABORT_CHAR CTRL('G')

#ifdef PAGE
#undef PAGE
#endif
#define PAGE CTRL('L')

#ifdef SPACE
#undef SPACE
#endif
#define SPACE ' '	/* XXX - was 0x20 */

#ifdef ESC
#undef ESC
#endif

#define ESC CTRL('[')

#endif  /* _CHARDEFS_H */
