/* rlprivate.h -- functions and variables global to the readline library,
		  but not intended for use by applications. */

/* Copyright (C) 1999 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library, a library for
   reading lines of text with interactive input and history editing.

   The GNU Readline Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   The GNU Readline Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   59 Temple Place, Suite 330, Boston, MA 02111 USA. */

#if !defined (_RL_PRIVATE_H_)
#define _RL_PRIVATE_H_

#include "rlconf.h"	/* for VISIBLE_STATS */
#include "rlstdc.h"
#include "posixjmp.h" /* defines procenv_t */

/*************************************************************************
 *									 *
 * Global functions undocumented in texinfo manual and not in readline.h *
 *									 *
 *************************************************************************/

/* terminal.c */
extern char *rl_get_termcap _PROTO((char *cap));

/*************************************************************************
 *									 *
 * Global variables undocumented in texinfo manual and not in readline.h *
 *									 *
 *************************************************************************/

/* complete.c */
extern int rl_complete_with_tilde_expansion;
#if defined (VISIBLE_STATS)
extern int rl_visible_stats;
#endif /* VISIBLE_STATS */

/* readline.c */
extern int rl_line_buffer_len;
extern int rl_numeric_arg;
extern int rl_arg_sign;
extern int rl_explicit_arg;
extern int rl_editing_mode;
extern int rl_visible_prompt_length;
extern Function *rl_last_func;
extern int readline_echoing_p;
extern int rl_key_sequence_length;

/* display.c */
extern int rl_display_fixed;

/* parens.c */
extern int rl_blink_matching_paren;

/*************************************************************************
 *									 *
 * Global functions and variables unsed and undocumented		 *
 *									 *
 *************************************************************************/

/* bind.c */
extern char *rl_untranslate_keyseq _PROTO((int seq));

/* kill.c */
extern int rl_set_retained_kills _PROTO((int num));

/* readline.c */
extern int rl_discard_argument _PROTO((void));

/* rltty.c */
extern int rl_stop_output _PROTO((int count, int key));

/* terminal.c */
extern void _rl_set_screen_size _PROTO((int rows, int cols));

/* undo.c */
extern int _rl_fix_last_undo_of_type _PROTO((int type, int start, int end));

/* util.c */
extern char *_rl_savestring _PROTO((char *s));

/*************************************************************************
 *									 *
 * Functions and variables private to the readline library		 *
 *									 *
 *************************************************************************/

/* NOTE: Functions and variables prefixed with `_rl_' are
   pseudo-global: they are global so they can be shared
   between files in the readline library, but are not intended
   to be visible to readline callers. */

/*************************************************************************
 * Undocumented private functions					 *
 *************************************************************************/

#if defined(READLINE_CALLBACKS)

/* readline.c */
extern void readline_internal_setup _PROTO((void));
extern char *readline_internal_teardown _PROTO((int eof));
extern int readline_internal_char _PROTO((void));

#endif /* READLINE_CALLBACKS */

/* bind.c */
extern void _rl_bind_if_unbound _PROTO((char *keyseq, Function *default_func));

/* display.c */
extern char *_rl_strip_prompt _PROTO((char *pmt));
extern void _rl_move_cursor_relative _PROTO((int new, char *data));
extern void _rl_move_vert _PROTO((int to));
extern void _rl_save_prompt _PROTO((void));
extern void _rl_restore_prompt _PROTO((void));
extern char *_rl_make_prompt_for_search _PROTO((int pchar));
extern void _rl_erase_at_end_of_line _PROTO((int l));
extern void _rl_clear_to_eol _PROTO((int count));
extern void _rl_clear_screen _PROTO((void));
extern void _rl_update_final _PROTO((void));
extern void _rl_redisplay_after_sigwinch _PROTO((void));
extern void _rl_clean_up_for_exit _PROTO((void));
extern void _rl_erase_entire_line _PROTO((void));
extern int _rl_currentb_display_line _PROTO((void));

/* input.c */
extern int _rl_any_typein _PROTO((void));
extern int _rl_input_available _PROTO((void));
extern void _rl_insert_typein _PROTO((int c));

/* macro.c */
extern void _rl_with_macro_input _PROTO((char *string));
extern int _rl_next_macro_key _PROTO((void));
extern void _rl_push_executing_macro _PROTO((void));
extern void _rl_pop_executing_macro _PROTO((void));
extern void _rl_add_macro_char _PROTO((int c));
extern void _rl_kill_kbd_macro _PROTO((void));

/* nls.c */
extern int _rl_init_eightbit _PROTO((void));

/* parens.c */
extern void _rl_enable_paren_matching _PROTO((int on_or_off));

/* readline.c */
extern void _rl_init_line_state _PROTO((void));
extern void _rl_set_the_line _PROTO((void));
extern int _rl_dispatch _PROTO((int key, Keymap map));
extern int _rl_init_argument _PROTO((void));
extern void _rl_fix_point _PROTO((int fix_mark_too));
extern void _rl_replace_text _PROTO((char *text, int start, int end));
extern int _rl_char_search_internal _PROTO((int count, int dir, int schar));
extern int _rl_set_mark_at_pos _PROTO((int position));

/* rltty.c */
extern int _rl_disable_tty_signals _PROTO((void));
extern int _rl_restore_tty_signals _PROTO((void));

/* terminal.c */
extern void _rl_get_screen_size _PROTO((int tty, int ignore_env));
extern int _rl_init_terminal_io _PROTO((char *terminal_name));
#ifdef _MINIX
extern void _rl_output_character_function _PROTO((int c));
#else
extern int _rl_output_character_function _PROTO((int c));
#endif
extern void _rl_output_some_chars _PROTO((char *string, int count));
extern int _rl_backspace _PROTO((int count));
extern void _rl_enable_meta_key _PROTO((void));
extern void _rl_control_keypad _PROTO((int on));

/* util.c */
extern int alphabetic _PROTO((int c));
extern int _rl_abort_internal _PROTO((void));
extern char *_rl_strindex _PROTO((char *s1, char *s2));
extern int _rl_qsort_string_compare _PROTO((char **s1, char **s2));
extern int (_rl_uppercase_p) _PROTO((int c));
extern int (_rl_lowercase_p) _PROTO((int c));
extern int (_rl_pure_alphabetic) _PROTO((int c));
extern int (_rl_digit_p) _PROTO((int c));
extern int (_rl_to_lower) _PROTO((int c));
extern int (_rl_to_upper) _PROTO((int c));
extern int (_rl_digit_value) _PROTO((int c));

/* vi_mode.c */
extern void _rl_vi_initialize_line _PROTO((void));
extern void _rl_vi_reset_last _PROTO((void));
extern void _rl_vi_set_last _PROTO((int key, int repeat, int sign));
extern int _rl_vi_textmod_command _PROTO((int c));
extern void _rl_vi_done_inserting _PROTO((void));

/* testUTF8.c */
extern int is_in_UTF8_mode __P((void));

/*************************************************************************
 * Undocumented private variables					 *
 *************************************************************************/

/* complete.c */
extern int _rl_complete_show_all;
extern int _rl_complete_mark_directories;
extern int _rl_print_completions_horizontally;
extern int _rl_completion_case_fold;

/* display.c */
extern int _rl_vis_botlin;
extern int _rl_last_c_pos;
extern int _rl_suppress_redisplay;
extern char *rl_display_prompt;

/* funmap.c */
extern char *possible_control_prefixes[];
extern char *possible_meta_prefixes[];

/* isearch.c */
extern unsigned char *_rl_isearch_terminators;

/* macro.c */
extern int _rl_defining_kbd_macro;
extern char *_rl_executing_macro;

/* readline.c */
extern int _rl_horizontal_scroll_mode;
extern int _rl_mark_modified_lines;
extern int _rl_bell_preference;
extern int _rl_meta_flag;
extern int _rl_convert_meta_chars_to_ascii;
extern int _rl_output_meta_chars;
extern char *_rl_comment_begin;
extern unsigned char _rl_parsing_conditionalized_out;
extern Keymap _rl_keymap;
extern FILE *_rl_in_stream;
extern FILE *_rl_out_stream;
extern int _rl_last_command_was_kill;
extern int _rl_eof_char;
extern procenv_t readline_top_level;

/* terminal.c */
extern int _rl_enable_keypad;
extern int _rl_enable_meta;
extern char *term_clreol;
extern char *term_clrpag;
extern char *term_im;
extern char *term_ic;
extern char *term_ei;
extern char *term_DC;
extern char *term_up;
extern char *term_dc;
extern char *term_cr;
extern char *term_IC;
extern int screenheight;
extern int screenwidth;
extern int screenchars;
extern int terminal_can_insert;
extern int _rl_term_autowrap;

/* undo.c */
extern int _rl_doing_an_undo;
extern int _rl_undo_group_level;

extern int rl_utf8_mode;

#endif /* _RL_PRIVATE_H_ */
