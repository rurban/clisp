/* vi_keymap.c -- the keymap for vi_mode in readline (). */

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

#if !defined (BUFSIZ)
#include <stdio.h>
#endif /* !BUFSIZ */

#include "readline.h"

#if 0
extern KEYMAP_ENTRY_ARRAY vi_escape_keymap;
#endif

/* The keymap arrays for handling vi mode. */
KEYMAP_ENTRY_ARRAY vi_movement_keymap = {
  /* The regular control keys come first. */
  { ISFUNC, (Function *)0x0 },		/* Control-@ */
  { ISFUNC, (Function *)0x0 },		/* Control-a */
  { ISFUNC, (Function *)0x0 },		/* Control-b */
  { ISFUNC, (Function *)0x0 },		/* Control-c */
  { ISFUNC, (Function *) rl_vi_eof_maybe },		/* Control-d */
  { ISFUNC, (Function *) rl_emacs_editing_mode },	/* Control-e */
  { ISFUNC, (Function *)0x0 },		/* Control-f */
  { ISFUNC, (Function *) rl_abort },			/* Control-g */
  { ISFUNC, (Function *) rl_backward },		/* Control-h */
  { ISFUNC, (Function *)0x0 },		/* Control-i */
  { ISFUNC, (Function *) rl_newline },		/* Control-j */
  { ISFUNC, (Function *) rl_kill_line },		/* Control-k */
  { ISFUNC, (Function *) rl_clear_screen },		/* Control-l */
  { ISFUNC, (Function *) rl_newline },		/* Control-m */
  { ISFUNC, (Function *) rl_get_next_history },	/* Control-n */
  { ISFUNC, (Function *)0x0 },		/* Control-o */
  { ISFUNC, (Function *) rl_get_previous_history },	/* Control-p */
  { ISFUNC, (Function *) rl_quoted_insert },		/* Control-q */
  { ISFUNC, (Function *) rl_reverse_search_history }, /* Control-r */
  { ISFUNC, (Function *) rl_forward_search_history }, /* Control-s */
  { ISFUNC, (Function *) rl_transpose_chars },	/* Control-t */
  { ISFUNC, (Function *) rl_unix_line_discard },	/* Control-u */
  { ISFUNC, (Function *) rl_quoted_insert },		/* Control-v */
  { ISFUNC, (Function *) rl_unix_word_rubout },	/* Control-w */
  { ISFUNC, (Function *)0x0 },		/* Control-x */
  { ISFUNC, (Function *) rl_yank },			/* Control-y */
  { ISFUNC, (Function *)0x0 },		/* Control-z */

  { ISFUNC, (Function *)0x0 },		/* Control-[ */	/* vi_escape_keymap */
  { ISFUNC, (Function *)0x0 },		/* Control-\ */
  { ISFUNC, (Function *)0x0 },		/* Control-] */
  { ISFUNC, (Function *)0x0 },		/* Control-^ */
  { ISFUNC, (Function *) rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *) rl_forward },		/* SPACE */
  { ISFUNC, (Function *)0x0 },		/* ! */
  { ISFUNC, (Function *)0x0 },		/* " */
  { ISFUNC, (Function *) rl_vi_comment },		/* # */
  { ISFUNC, (Function *) rl_end_of_line },		/* $ */
  { ISFUNC, (Function *) rl_vi_match },		/* % */
  { ISFUNC, (Function *) rl_vi_tilde_expand },	/* & */
  { ISFUNC, (Function *)0x0 },		/* ' */
  { ISFUNC, (Function *)0x0 },		/* ( */
  { ISFUNC, (Function *)0x0 },		/* ) */
  { ISFUNC, (Function *) rl_vi_complete },		/* * */
  { ISFUNC, (Function *) rl_get_next_history},	/* + */
  { ISFUNC, (Function *) rl_vi_char_search },	/* , */
  { ISFUNC, (Function *) rl_get_previous_history },	/* - */
  { ISFUNC, (Function *) rl_vi_redo },		/* . */
  { ISFUNC, (Function *) rl_vi_search },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *) rl_beg_of_line },		/* 0 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 1 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 2 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 3 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 4 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 5 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 6 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 7 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 8 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* : */
  { ISFUNC, (Function *) rl_vi_char_search },	/* ; */
  { ISFUNC, (Function *)0x0 },		/* < */
  { ISFUNC, (Function *) rl_vi_complete },		/* = */
  { ISFUNC, (Function *)0x0 },		/* > */
  { ISFUNC, (Function *) rl_vi_search },		/* ? */
  { ISFUNC, (Function *)0x0 },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, (Function *) rl_vi_append_eol },		/* A */
  { ISFUNC, (Function *) rl_vi_prev_word},		/* B */
  { ISFUNC, (Function *) rl_vi_change_to },		/* C */
  { ISFUNC, (Function *) rl_vi_delete_to },		/* D */
  { ISFUNC, (Function *) rl_vi_end_word },		/* E */
  { ISFUNC, (Function *) rl_vi_char_search },	/* F */
  { ISFUNC, (Function *) rl_vi_fetch_history },	/* G */
  { ISFUNC, (Function *)0x0 },		/* H */
  { ISFUNC, (Function *) rl_vi_insert_beg },		/* I */
  { ISFUNC, (Function *)0x0 },		/* J */
  { ISFUNC, (Function *)0x0 },		/* K */
  { ISFUNC, (Function *)0x0 },		/* L */
  { ISFUNC, (Function *)0x0 },		/* M */
  { ISFUNC, (Function *) rl_vi_search_again },	/* N */
  { ISFUNC, (Function *)0x0 },		/* O */
  { ISFUNC, (Function *) rl_vi_put },		/* P */
  { ISFUNC, (Function *)0x0 },		/* Q */
  { ISFUNC, (Function *) rl_vi_replace },		/* R */
  { ISFUNC, (Function *) rl_vi_subst },		/* S */
  { ISFUNC, (Function *) rl_vi_char_search },	/* T */
  { ISFUNC, (Function *) rl_revert_line },		/* U */
  { ISFUNC, (Function *)0x0 },		/* V */
  { ISFUNC, (Function *) rl_vi_next_word },		/* W */
  { ISFUNC, (Function *) rl_rubout },		/* X */
  { ISFUNC, (Function *) rl_vi_yank_to },		/* Y */
  { ISFUNC, (Function *)0x0 },		/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* [ */
  { ISFUNC, (Function *) rl_vi_complete },		/* \ */
  { ISFUNC, (Function *)0x0 },		/* ] */
  { ISFUNC, (Function *) rl_vi_first_print },	/* ^ */
  { ISFUNC, (Function *) rl_vi_yank_arg },		/* _ */
  { ISFUNC, (Function *)0x0 },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *) rl_vi_append_mode },	/* a */
  { ISFUNC, (Function *) rl_vi_prev_word },		/* b */
  { ISFUNC, (Function *) rl_vi_change_to },		/* c */
  { ISFUNC, (Function *) rl_vi_delete_to },		/* d */
  { ISFUNC, (Function *) rl_vi_end_word },		/* e */
  { ISFUNC, (Function *) rl_vi_char_search },	/* f */
  { ISFUNC, (Function *)0x0 },		/* g */
  { ISFUNC, (Function *) rl_backward },		/* h */
  { ISFUNC, (Function *) rl_vi_insertion_mode },	/* i */
  { ISFUNC, (Function *) rl_get_next_history },	/* j */
  { ISFUNC, (Function *) rl_get_previous_history },	/* k */
  { ISFUNC, (Function *) rl_forward },		/* l */
  { ISFUNC, (Function *)0x0 },		/* m */
  { ISFUNC, (Function *) rl_vi_search_again },	/* n */
  { ISFUNC, (Function *)0x0 },		/* o */
  { ISFUNC, (Function *) rl_vi_put },		/* p */
  { ISFUNC, (Function *)0x0 },		/* q */
  { ISFUNC, (Function *) rl_vi_change_char },	/* r */
  { ISFUNC, (Function *) rl_vi_subst },		/* s */
  { ISFUNC, (Function *) rl_vi_char_search },	/* t */
  { ISFUNC, (Function *) rl_undo_command },		/* u */
  { ISFUNC, (Function *)0x0 },		/* v */
  { ISFUNC, (Function *) rl_vi_next_word },		/* w */
  { ISFUNC, (Function *) rl_vi_delete },		/* x */
  { ISFUNC, (Function *) rl_vi_yank_to },		/* y */
  { ISFUNC, (Function *)0x0 },		/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *)0x0 },		/* { */
  { ISFUNC, (Function *) rl_vi_column },		/* | */
  { ISFUNC, (Function *)0x0 },		/* } */
  { ISFUNC, (Function *) rl_vi_change_case },	/* ~ */
  { ISFUNC, (Function *)0x0 },		/* RUBOUT */

#if KEYMAP_SIZE > 128
  /* Undefined keys. */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 }
#endif /* KEYMAP_SIZE > 128 */
};


KEYMAP_ENTRY_ARRAY vi_insertion_keymap = {
  /* The regular control keys come first. */
  { ISFUNC, (Function *)0x0 },		/* Control-@ */
  { ISFUNC, (Function *) rl_insert },		/* Control-a */
  { ISFUNC, (Function *) rl_insert },		/* Control-b */
  { ISFUNC, (Function *) rl_insert },		/* Control-c */
  { ISFUNC, (Function *) rl_vi_eof_maybe },		/* Control-d */
  { ISFUNC, (Function *) rl_insert },		/* Control-e */
  { ISFUNC, (Function *) rl_insert },		/* Control-f */
  { ISFUNC, (Function *) rl_insert },		/* Control-g */
  { ISFUNC, (Function *) rl_rubout },		/* Control-h */
  { ISFUNC, (Function *) rl_complete },		/* Control-i */
  { ISFUNC, (Function *) rl_newline },		/* Control-j */
  { ISFUNC, (Function *) rl_insert },		/* Control-k */
  { ISFUNC, (Function *) rl_insert },		/* Control-l */
  { ISFUNC, (Function *) rl_newline },		/* Control-m */
  { ISFUNC, (Function *) rl_insert },		/* Control-n */
  { ISFUNC, (Function *) rl_insert },		/* Control-o */
  { ISFUNC, (Function *) rl_insert },		/* Control-p */
  { ISFUNC, (Function *) rl_insert },		/* Control-q */
  { ISFUNC, (Function *) rl_reverse_search_history }, /* Control-r */
  { ISFUNC, (Function *) rl_forward_search_history }, /* Control-s */
  { ISFUNC, (Function *) rl_transpose_chars },	/* Control-t */
  { ISFUNC, (Function *) rl_unix_line_discard },	/* Control-u */
  { ISFUNC, (Function *) rl_quoted_insert },		/* Control-v */
  { ISFUNC, (Function *) rl_unix_word_rubout },	/* Control-w */
  { ISFUNC, (Function *) rl_insert },		/* Control-x */
  { ISFUNC, (Function *) rl_yank },			/* Control-y */
  { ISFUNC, (Function *) rl_insert },		/* Control-z */

  { ISFUNC, (Function *) rl_vi_movement_mode },	/* Control-[ */
  { ISFUNC, (Function *) rl_insert },		/* Control-\ */
  { ISFUNC, (Function *) rl_insert },		/* Control-] */
  { ISFUNC, (Function *) rl_insert },		/* Control-^ */
  { ISFUNC, (Function *) rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *) rl_insert },		/* SPACE */
  { ISFUNC, (Function *) rl_insert },		/* ! */
  { ISFUNC, (Function *) rl_insert },		/* " */
  { ISFUNC, (Function *) rl_insert },		/* # */
  { ISFUNC, (Function *) rl_insert },		/* $ */
  { ISFUNC, (Function *) rl_insert },		/* % */
  { ISFUNC, (Function *) rl_insert },		/* & */
  { ISFUNC, (Function *) rl_insert },		/* ' */
  { ISFUNC, (Function *) rl_insert },		/* ( */
  { ISFUNC, (Function *) rl_insert },		/* ) */
  { ISFUNC, (Function *) rl_insert },		/* * */
  { ISFUNC, (Function *) rl_insert },		/* + */
  { ISFUNC, (Function *) rl_insert },		/* , */
  { ISFUNC, (Function *) rl_insert },		/* - */
  { ISFUNC, (Function *) rl_insert },		/* . */
  { ISFUNC, (Function *) rl_insert },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *) rl_insert },		/* 0 */
  { ISFUNC, (Function *) rl_insert },		/* 1 */
  { ISFUNC, (Function *) rl_insert },		/* 2 */
  { ISFUNC, (Function *) rl_insert },		/* 3 */
  { ISFUNC, (Function *) rl_insert },		/* 4 */
  { ISFUNC, (Function *) rl_insert },		/* 5 */
  { ISFUNC, (Function *) rl_insert },		/* 6 */
  { ISFUNC, (Function *) rl_insert },		/* 7 */
  { ISFUNC, (Function *) rl_insert },		/* 8 */
  { ISFUNC, (Function *) rl_insert },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *) rl_insert },		/* : */
  { ISFUNC, (Function *) rl_insert },		/* ; */
  { ISFUNC, (Function *) rl_insert },		/* < */
  { ISFUNC, (Function *) rl_insert },		/* = */
  { ISFUNC, (Function *) rl_insert },		/* > */
  { ISFUNC, (Function *) rl_insert },		/* ? */
  { ISFUNC, (Function *) rl_insert },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, (Function *) rl_insert },		/* A */
  { ISFUNC, (Function *) rl_insert },		/* B */
  { ISFUNC, (Function *) rl_insert },		/* C */
  { ISFUNC, (Function *) rl_insert },		/* D */
  { ISFUNC, (Function *) rl_insert },		/* E */
  { ISFUNC, (Function *) rl_insert },		/* F */
  { ISFUNC, (Function *) rl_insert },		/* G */
  { ISFUNC, (Function *) rl_insert },		/* H */
  { ISFUNC, (Function *) rl_insert },		/* I */
  { ISFUNC, (Function *) rl_insert },		/* J */
  { ISFUNC, (Function *) rl_insert },		/* K */
  { ISFUNC, (Function *) rl_insert },		/* L */
  { ISFUNC, (Function *) rl_insert },		/* M */
  { ISFUNC, (Function *) rl_insert },		/* N */
  { ISFUNC, (Function *) rl_insert },		/* O */
  { ISFUNC, (Function *) rl_insert },		/* P */
  { ISFUNC, (Function *) rl_insert },		/* Q */
  { ISFUNC, (Function *) rl_insert },		/* R */
  { ISFUNC, (Function *) rl_insert },		/* S */
  { ISFUNC, (Function *) rl_insert },		/* T */
  { ISFUNC, (Function *) rl_insert },		/* U */
  { ISFUNC, (Function *) rl_insert },		/* V */
  { ISFUNC, (Function *) rl_insert },		/* W */
  { ISFUNC, (Function *) rl_insert },		/* X */
  { ISFUNC, (Function *) rl_insert },		/* Y */
  { ISFUNC, (Function *) rl_insert },		/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *) rl_insert },		/* [ */
  { ISFUNC, (Function *) rl_insert },		/* \ */
  { ISFUNC, (Function *) rl_insert },		/* ] */
  { ISFUNC, (Function *) rl_insert },		/* ^ */
  { ISFUNC, (Function *) rl_insert },		/* _ */
  { ISFUNC, (Function *) rl_insert },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *) rl_insert },		/* a */
  { ISFUNC, (Function *) rl_insert },		/* b */
  { ISFUNC, (Function *) rl_insert },		/* c */
  { ISFUNC, (Function *) rl_insert },		/* d */
  { ISFUNC, (Function *) rl_insert },		/* e */
  { ISFUNC, (Function *) rl_insert },		/* f */
  { ISFUNC, (Function *) rl_insert },		/* g */
  { ISFUNC, (Function *) rl_insert },		/* h */
  { ISFUNC, (Function *) rl_insert },		/* i */
  { ISFUNC, (Function *) rl_insert },		/* j */
  { ISFUNC, (Function *) rl_insert },		/* k */
  { ISFUNC, (Function *) rl_insert },		/* l */
  { ISFUNC, (Function *) rl_insert },		/* m */
  { ISFUNC, (Function *) rl_insert },		/* n */
  { ISFUNC, (Function *) rl_insert },		/* o */
  { ISFUNC, (Function *) rl_insert },		/* p */
  { ISFUNC, (Function *) rl_insert },		/* q */
  { ISFUNC, (Function *) rl_insert },		/* r */
  { ISFUNC, (Function *) rl_insert },		/* s */
  { ISFUNC, (Function *) rl_insert },		/* t */
  { ISFUNC, (Function *) rl_insert },		/* u */
  { ISFUNC, (Function *) rl_insert },		/* v */
  { ISFUNC, (Function *) rl_insert },		/* w */
  { ISFUNC, (Function *) rl_insert },		/* x */
  { ISFUNC, (Function *) rl_insert },		/* y */
  { ISFUNC, (Function *) rl_insert },		/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *) rl_insert },		/* { */
  { ISFUNC, (Function *) rl_insert },		/* | */
  { ISFUNC, (Function *) rl_insert },		/* } */
  { ISFUNC, (Function *) rl_insert },		/* ~ */
  { ISFUNC, (Function *) rl_rubout },		/* RUBOUT */

#if KEYMAP_SIZE > 128
  /* Pure 8-bit characters (128 - 159).
     These might be used in some
     character sets. */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */
  { ISFUNC, (Function *) rl_insert },	/* ? */

  /* ISO Latin-1 characters (160 - 255) */
  { ISFUNC, (Function *) rl_insert },	/* No-break space */
  { ISFUNC, (Function *) rl_insert },	/* Inverted exclamation mark */
  { ISFUNC, (Function *) rl_insert },	/* Cent sign */
  { ISFUNC, (Function *) rl_insert },	/* Pound sign */
  { ISFUNC, (Function *) rl_insert },	/* Currency sign */
  { ISFUNC, (Function *) rl_insert },	/* Yen sign */
  { ISFUNC, (Function *) rl_insert },	/* Broken bar */
  { ISFUNC, (Function *) rl_insert },	/* Section sign */
  { ISFUNC, (Function *) rl_insert },	/* Diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Copyright sign */
  { ISFUNC, (Function *) rl_insert },	/* Feminine ordinal indicator */
  { ISFUNC, (Function *) rl_insert },	/* Left pointing double angle quotation mark */
  { ISFUNC, (Function *) rl_insert },	/* Not sign */
  { ISFUNC, (Function *) rl_insert },	/* Soft hyphen */
  { ISFUNC, (Function *) rl_insert },	/* Registered sign */
  { ISFUNC, (Function *) rl_insert },	/* Macron */
  { ISFUNC, (Function *) rl_insert },	/* Degree sign */
  { ISFUNC, (Function *) rl_insert },	/* Plus-minus sign */
  { ISFUNC, (Function *) rl_insert },	/* Superscript two */
  { ISFUNC, (Function *) rl_insert },	/* Superscript three */
  { ISFUNC, (Function *) rl_insert },	/* Acute accent */
  { ISFUNC, (Function *) rl_insert },	/* Micro sign */
  { ISFUNC, (Function *) rl_insert },	/* Pilcrow sign */
  { ISFUNC, (Function *) rl_insert },	/* Middle dot */
  { ISFUNC, (Function *) rl_insert },	/* Cedilla */
  { ISFUNC, (Function *) rl_insert },	/* Superscript one */
  { ISFUNC, (Function *) rl_insert },	/* Masculine ordinal indicator */
  { ISFUNC, (Function *) rl_insert },	/* Right pointing double angle quotation mark */
  { ISFUNC, (Function *) rl_insert },	/* Vulgar fraction one quarter */
  { ISFUNC, (Function *) rl_insert },	/* Vulgar fraction one half */
  { ISFUNC, (Function *) rl_insert },	/* Vulgar fraction three quarters */
  { ISFUNC, (Function *) rl_insert },	/* Inverted questionk mark */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter a with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter a with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter a with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter a with tilde */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter a with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter a with ring above */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter ae */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter c with cedilla */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter e with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter e with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter e with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter e with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter i with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter i with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter i with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter i with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter eth (Icelandic) */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter n with tilde */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter o with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter o with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter o with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter o with tilde */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter o with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Multiplication sign */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter o with stroke */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter u with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter u with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter u with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter u with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter Y with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin capital letter thorn (Icelandic) */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter sharp s (German) */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter a with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter a with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter a with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter a with tilde */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter a with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter a with ring above */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter ae */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter c with cedilla */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter e with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter e with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter e with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter e with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter i with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter i with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter i with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter i with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter eth (Icelandic) */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter n with tilde */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter o with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter o with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter o with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter o with tilde */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter o with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Division sign */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter o with stroke */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter u with grave */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter u with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter u with circumflex */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter u with diaeresis */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter y with acute */
  { ISFUNC, (Function *) rl_insert },	/* Latin small letter thorn (Icelandic) */
  { ISFUNC, (Function *) rl_insert }		/* Latin small letter y with diaeresis */
#endif /* KEYMAP_SIZE > 128 */
};

/* Unused for the time being. */
#if 0
KEYMAP_ENTRY_ARRAY vi_escape_keymap = {
  /* The regular control keys come first. */
  { ISFUNC, (Function *)0x0 },		/* Control-@ */
  { ISFUNC, (Function *)0x0 },		/* Control-a */
  { ISFUNC, (Function *)0x0 },		/* Control-b */
  { ISFUNC, (Function *)0x0 },		/* Control-c */
  { ISFUNC, (Function *)0x0 },		/* Control-d */
  { ISFUNC, (Function *)0x0 },		/* Control-e */
  { ISFUNC, (Function *)0x0 },		/* Control-f */
  { ISFUNC, (Function *)0x0 },		/* Control-g */
  { ISFUNC, (Function *)0x0 },		/* Control-h */
  { ISFUNC, (Function *) rl_tab_insert},		/* Control-i */
  { ISFUNC, (Function *) rl_emacs_editing_mode},	/* Control-j */
  { ISFUNC, (Function *) rl_kill_line },		/* Control-k */
  { ISFUNC, (Function *)0x0 },		/* Control-l */
  { ISFUNC, (Function *) rl_emacs_editing_mode},	/* Control-m */
  { ISFUNC, (Function *)0x0 },		/* Control-n */
  { ISFUNC, (Function *)0x0 },		/* Control-o */
  { ISFUNC, (Function *)0x0 },		/* Control-p */
  { ISFUNC, (Function *)0x0 },		/* Control-q */
  { ISFUNC, (Function *)0x0 },		/* Control-r */
  { ISFUNC, (Function *)0x0 },		/* Control-s */
  { ISFUNC, (Function *)0x0 },		/* Control-t */
  { ISFUNC, (Function *)0x0 },		/* Control-u */
  { ISFUNC, (Function *)0x0 },		/* Control-v */
  { ISFUNC, (Function *)0x0 },		/* Control-w */
  { ISFUNC, (Function *)0x0 },		/* Control-x */
  { ISFUNC, (Function *)0x0 },		/* Control-y */
  { ISFUNC, (Function *)0x0 },		/* Control-z */

  { ISFUNC, (Function *) rl_vi_movement_mode },	/* Control-[ */
  { ISFUNC, (Function *)0x0 },		/* Control-\ */
  { ISFUNC, (Function *)0x0 },		/* Control-] */
  { ISFUNC, (Function *)0x0 },		/* Control-^ */
  { ISFUNC, (Function *) rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *)0x0 },		/* SPACE */
  { ISFUNC, (Function *)0x0 },		/* ! */
  { ISFUNC, (Function *)0x0 },		/* " */
  { ISFUNC, (Function *)0x0 },		/* # */
  { ISFUNC, (Function *)0x0 },		/* $ */
  { ISFUNC, (Function *)0x0 },		/* % */
  { ISFUNC, (Function *)0x0 },		/* & */
  { ISFUNC, (Function *)0x0 },		/* ' */
  { ISFUNC, (Function *)0x0 },		/* ( */
  { ISFUNC, (Function *)0x0 },		/* ) */
  { ISFUNC, (Function *)0x0 },		/* * */
  { ISFUNC, (Function *)0x0 },		/* + */
  { ISFUNC, (Function *)0x0 },		/* , */
  { ISFUNC, (Function *)0x0 },		/* - */
  { ISFUNC, (Function *)0x0 },		/* . */
  { ISFUNC, (Function *)0x0 },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 0 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 1 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 2 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 3 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 4 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 5 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 6 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 7 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 8 */
  { ISFUNC, (Function *) rl_vi_arg_digit },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* : */
  { ISFUNC, (Function *)0x0 },		/* ; */
  { ISFUNC, (Function *)0x0 },		/* < */
  { ISFUNC, (Function *)0x0 },		/* = */
  { ISFUNC, (Function *)0x0 },		/* > */
  { ISFUNC, (Function *)0x0 },		/* ? */
  { ISFUNC, (Function *)0x0 },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* A */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* B */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* C */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* D */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* E */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* F */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* G */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* H */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* I */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* J */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* K */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* L */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* M */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* N */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* O */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* P */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* Q */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* R */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* S */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* T */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* U */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* V */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* W */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* X */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* Y */
  { ISFUNC, (Function *) rl_do_lowercase_version },	/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *) rl_arrow_keys },		/* [ */
  { ISFUNC, (Function *)0x0 },		/* \ */
  { ISFUNC, (Function *)0x0 },		/* ] */
  { ISFUNC, (Function *)0x0 },		/* ^ */
  { ISFUNC, (Function *)0x0 },		/* _ */
  { ISFUNC, (Function *)0x0 },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *)0x0 },		/* a */
  { ISFUNC, (Function *)0x0 },		/* b */
  { ISFUNC, (Function *)0x0 },		/* c */
  { ISFUNC, (Function *)0x0 },		/* d */
  { ISFUNC, (Function *)0x0 },		/* e */
  { ISFUNC, (Function *)0x0 },		/* f */
  { ISFUNC, (Function *)0x0 },		/* g */
  { ISFUNC, (Function *)0x0 },		/* h */
  { ISFUNC, (Function *)0x0 },		/* i */
  { ISFUNC, (Function *)0x0 },		/* j */
  { ISFUNC, (Function *)0x0 },		/* k */
  { ISFUNC, (Function *)0x0 },		/* l */
  { ISFUNC, (Function *)0x0 },		/* m */
  { ISFUNC, (Function *)0x0 },		/* n */
  { ISFUNC, (Function *) rl_arrow_keys },		/* o */
  { ISFUNC, (Function *)0x0 },		/* p */
  { ISFUNC, (Function *)0x0 },		/* q */
  { ISFUNC, (Function *)0x0 },		/* r */
  { ISFUNC, (Function *)0x0 },		/* s */
  { ISFUNC, (Function *)0x0 },		/* t */
  { ISFUNC, (Function *)0x0 },		/* u */
  { ISFUNC, (Function *)0x0 },		/* v */
  { ISFUNC, (Function *)0x0 },		/* w */
  { ISFUNC, (Function *)0x0 },		/* x */
  { ISFUNC, (Function *)0x0 },		/* y */
  { ISFUNC, (Function *)0x0 },		/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *)0x0 },		/* { */
  { ISFUNC, (Function *)0x0 },		/* | */
  { ISFUNC, (Function *)0x0 },		/* } */
  { ISFUNC, (Function *)0x0 },		/* ~ */
  { ISFUNC, (Function *) rl_backward_kill_word },	/* RUBOUT */

#if KEYMAP_SIZE > 128
  /* Undefined keys. */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 }
#endif /* KEYMAP_SIZE > 128 */
};
#endif
