/* Readline.h -- the names of functions callable from within readline. */

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

#if !defined (_READLINE_H_)
#define _READLINE_H_

/* Need to include <stdio.h> for the declaration of FILE below. */
#if defined(__GNUC__) || defined(__STDC__) || defined(__cplusplus)
#  include <stdio.h>
#endif

#if defined (READLINE_LIBRARY)
#  include "ansi_proto.h"
#  include "keymaps.h"
#  include "history.h"
#  include "tilde.h"
#else
#  include <readline/ansi_proto.h>
#  include <readline/keymaps.h>
#  include <readline/history.h>
#  include <readline/tilde.h>
#endif

/* Readline data structures. */

/* Maintaining the state of undo.  We remember individual deletes and inserts
   on a chain of things to do. */

/* The actions that undo knows how to undo.  Notice that UNDO_DELETE means
   to insert some text, and UNDO_INSERT means to delete some text.   I.e.,
   the code tells undo what to undo, not how to undo it. */
enum undo_code { UNDO_DELETE, UNDO_INSERT, UNDO_BEGIN, UNDO_END };

/* What an element of THE_UNDO_LIST looks like. */
typedef struct undo_list {
  struct undo_list *next;
  int start, end;		/* Where the change took place. */
  char *text;			/* The text to insert, if undoing a delete. */
  enum undo_code what;		/* Delete, Insert, Begin, End. */
} UNDO_LIST;

/* The current undo list for RL_LINE_BUFFER. */
extern UNDO_LIST *rl_undo_list;

/* The data structure for mapping textual names to code addresses. */
typedef struct _funmap {
  char *name;
  Function *function;
} FUNMAP;

extern FUNMAP **funmap;

/* Functions available to bind to key sequences. */
/* bind.c */
extern int rl_re_read_init_file _PROTO((int count, int ignore));
extern int rl_dump_functions _PROTO((int count, int key));
extern int rl_dump_variables _PROTO((int count, int key));
extern int rl_dump_macros _PROTO((int count, int key));
/* complete.c */
extern int rl_complete _PROTO((int ignore, int invoking_key));
extern int rl_possible_completions _PROTO((int ignore, int invoking_key));
extern int rl_insert_completions _PROTO((int ignore, int invoking_key));
extern int rl_menu_complete _PROTO((int count, int ignore));
/* isearch.c */
extern int rl_reverse_search_history _PROTO((int sign, int key));
extern int rl_forward_search_history _PROTO((int sign, int key));
/* kill.c */
extern int rl_kill_line _PROTO((int direction, int ignore));
extern int rl_copy_region_to_kill _PROTO((int count, int ignore));
extern int rl_kill_region _PROTO((int count, int ignore));
extern int rl_unix_line_discard _PROTO((int count, int key));
extern int rl_unix_word_rubout _PROTO((int count, int key));
extern int rl_yank _PROTO((int count, int ignore));
extern int rl_kill_word _PROTO((int count, int key));
extern int rl_yank_pop _PROTO((int count, int key));
extern int rl_yank_nth_arg _PROTO((int count, int ignore));
extern int rl_backward_kill_word _PROTO((int count, int ignore));
extern int rl_backward_kill_line _PROTO((int direction, int ignore));
extern int rl_kill_full_line _PROTO((int count, int ignore));
extern int rl_yank_last_arg _PROTO((int count, int key));
extern int rl_copy_forward_word _PROTO((int count, int key));
extern int rl_copy_backward_word _PROTO((int count, int key));
/* readline.c */
extern int rl_set_mark _PROTO((int count, int key));
extern int rl_exchange_point_and_mark _PROTO((int count, int key));
extern int rl_beg_of_line _PROTO((int count, int key));
extern int rl_backward _PROTO((int count, int key));
extern int rl_delete _PROTO((int count, int key));
extern int rl_end_of_line _PROTO((int count, int key));
extern int rl_forward _PROTO((int count, int key));
extern int rl_newline _PROTO((int count, int key));
extern int rl_char_search _PROTO((int count, int key));
extern int rl_clear_screen _PROTO((int count, int key));
extern int rl_get_next_history _PROTO((int count, int key));
extern int rl_get_previous_history _PROTO((int count, int key));
extern int rl_quoted_insert _PROTO((int count, int key));
extern int rl_transpose_chars _PROTO((int count, int key));
extern int rl_rubout _PROTO((int count, int key));
extern int rl_backward_word _PROTO((int count, int key));
extern int rl_forward_word _PROTO((int count, int key));
extern int rl_tab_insert _PROTO((int count, int key));
extern int rl_transpose_words _PROTO((int count, int key));
extern int rl_do_lowercase_version _PROTO((int ignore1, int ignore2));
extern int rl_digit_argument _PROTO((int ignore, int key));
extern int rl_universal_argument _PROTO((int count, int key));
extern int rl_beginning_of_history _PROTO((int count, int key));
extern int rl_end_of_history _PROTO((int count, int key));
extern int rl_insert _PROTO((int count, int c));
extern int rl_upcase_word _PROTO((int count, int key));
extern int rl_downcase_word _PROTO((int count, int key));
extern int rl_capitalize_word _PROTO((int count, int key));
extern int rl_delete_horizontal_space _PROTO((int count, int ignore));
extern int rl_insert_comment _PROTO((int count, int key));
extern int rl_backward_char_search _PROTO((int count, int key));
/* rltty.c */
extern int rl_restart_output _PROTO((int count, int key));
/* search.c */
extern int rl_history_search_forward _PROTO((int count, int ignore));
extern int rl_history_search_backward _PROTO((int count, int ignore));
/* terminal.c */
extern int ding _PROTO((void));
/* undo.c */
extern int rl_undo_command _PROTO((int count, int key));
extern int rl_revert_line _PROTO((int count, int key));
/* util.c */
extern int rl_tilde_expand _PROTO((int ignore, int key));
extern int rl_abort _PROTO((int count, int key));
extern int rl_tty_status _PROTO((int count, int key));

/* Not available unless readline is compiled -DPAREN_MATCHING. */
extern int rl_insert_close _PROTO((int count, int invoking_key));

/* Not available unless READLINE_CALLBACKS is defined. */
extern void rl_callback_handler_install _PROTO((char *prompt, VFunction *linefunc));
extern void rl_callback_read_char _PROTO((void));
extern void rl_callback_handler_remove _PROTO((void));

/* Not available unless __CYGWIN32__ is defined. */
#ifdef __CYGWIN32__
extern int rl_paste_from_clipboard _PROTO((int count, int key));
#endif

/* These are *both* defined even when VI_MODE is not. */
extern int rl_vi_editing_mode _PROTO((int count, int key));
extern int rl_emacs_editing_mode _PROTO((int count, int key));

/* Non incremental history searching. */
extern int rl_noninc_forward_search _PROTO((int count, int key));
extern int rl_noninc_reverse_search _PROTO((int count, int key));
extern int rl_noninc_forward_search_again _PROTO((int count, int key));
extern int rl_noninc_reverse_search_again _PROTO((int count, int key));

/* Things for vi mode. Not available unless readline is compiled -DVI_MODE. */
extern int rl_vi_check _PROTO((void));
extern int rl_vi_undo _PROTO((int count, int key));
extern int rl_vi_redo _PROTO((int count, int c));
extern int rl_vi_tilde_expand _PROTO((int ignore, int key));
extern int rl_vi_movement_mode _PROTO((int count, int key));
extern int rl_vi_insertion_mode _PROTO((int count, int key));
extern int rl_vi_arg_digit _PROTO((int count, int c));
extern int rl_vi_prev_word _PROTO((int count, int key));
extern int rl_vi_next_word _PROTO((int count, int key));
extern int rl_vi_char_search _PROTO((int count, int key));
extern int rl_vi_eof_maybe _PROTO((int count, int c));
extern int rl_vi_append_mode _PROTO((int count, int key));
extern int rl_vi_put _PROTO((int count, int key));
extern int rl_vi_append_eol _PROTO((int count, int key));
extern int rl_vi_insert_beg _PROTO((int count, int key));
extern int rl_vi_delete _PROTO((int count, int key));
extern int rl_vi_first_print _PROTO((int count, int key));
extern int rl_vi_fword _PROTO((int count));
extern int rl_vi_fWord _PROTO((int count));
extern int rl_vi_bword _PROTO((int count));
extern int rl_vi_bWord _PROTO((int count));
extern int rl_vi_eword _PROTO((int count));
extern int rl_vi_eWord _PROTO((int count));
extern int rl_vi_end_word _PROTO((int count, int key));
extern int rl_vi_change_case _PROTO((int count, int ignore));
extern int rl_vi_match _PROTO((int ignore, int key));
extern int rl_vi_bracktype _PROTO((int c));
extern int rl_vi_change_char _PROTO((int count, int key));
extern int rl_vi_yank_arg _PROTO((int count, int key));
extern int rl_vi_search _PROTO((int count, int key));
extern int rl_vi_search_again _PROTO((int count, int key));
extern int rl_vi_subst _PROTO((int count, int key));
extern int rl_vi_overstrike _PROTO((int count, int key));
extern int rl_vi_overstrike_delete _PROTO((int count, int key));
extern int rl_vi_replace _PROTO((int count, int key));
extern int rl_vi_column _PROTO((int count, int key));
extern int rl_vi_delete_to _PROTO((int count, int key));
extern int rl_vi_change_to _PROTO((int count, int key));
extern int rl_vi_yank_to _PROTO((int count, int key));
extern int rl_vi_complete _PROTO((int ignore, int key));
extern int rl_vi_fetch_history _PROTO((int count, int c));
extern int rl_vi_set_mark _PROTO((int count, int key));
extern int rl_vi_goto_mark _PROTO((int count, int key));
extern int rl_vi_back_to_indent _PROTO((int count, int key));

/* Keyboard macro commands. */
extern int rl_start_kbd_macro _PROTO((int ignore1, int ignore2));
extern int rl_end_kbd_macro _PROTO((int count, int ignore));
extern int rl_call_last_kbd_macro _PROTO((int count, int ignore));
extern void rl_push_macro_input _PROTO((char *macro));

extern int rl_arrow_keys _PROTO((int count, int c));
extern int rl_refresh_line _PROTO((void));

/* **************************************************************** */
/*								    */
/*			Well Published Functions		    */
/*								    */
/* **************************************************************** */

/* Readline functions. */
/* Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none. */
extern char *readline _PROTO((char *prompt));

/* These functions are from bind.c. */
/* rl_add_defun (char *name, Function *function, int key)
   Add NAME to the list of named functions.  Make FUNCTION
   be the function that gets called.
   If KEY is not -1, then bind it. */
extern int rl_add_defun _PROTO((char *name, Function *function, int key));

extern Keymap rl_make_bare_keymap _PROTO((void));
extern Keymap rl_copy_keymap _PROTO((Keymap map));
extern Keymap rl_make_keymap _PROTO((void));
extern void rl_discard_keymap _PROTO((Keymap map));
extern Keymap rl_get_keymap _PROTO((void));
extern Keymap rl_get_keymap_by_name _PROTO((char *name));
extern void rl_set_keymap _PROTO((Keymap map));
extern char *rl_get_keymap_name _PROTO((Keymap map));

extern int rl_bind_key _PROTO((int key, Function *function));
extern int rl_bind_key_in_map _PROTO((int key, Function *function, Keymap map));
extern int rl_unbind_key _PROTO((int key));
extern int rl_unbind_key_in_map _PROTO((int key, Keymap map));
extern int rl_unbind_function_in_map _PROTO((Function *func, Keymap map));
extern int rl_unbind_command_in_map _PROTO((char *command, Keymap map));
extern int rl_set_key _PROTO((char *keyseq, Function *function, Keymap map));
extern int rl_generic_bind _PROTO((int type, char *keyseq, char *data, Keymap map));
extern int rl_parse_and_bind _PROTO((char *string));
/* Backwards compatibility, use rl_generic_bind instead. */
extern int rl_macro_bind _PROTO((char *keyseq, char *macro, Keymap map));
extern int rl_variable_bind _PROTO((char *name, char *value));

extern int rl_read_init_file _PROTO((char *filename));

extern Function *rl_named_function _PROTO((char *string));
extern Function *rl_function_of_keyseq _PROTO((char *keyseq, Keymap map, int *type));
extern char **rl_invoking_keyseqs _PROTO((Function *function));
extern char **rl_invoking_keyseqs_in_map _PROTO((Function *function, Keymap map));
extern void rl_function_dumper _PROTO((int print_readably));
extern void rl_variable_dumper _PROTO((int print_readably));
extern void rl_macro_dumper _PROTO((int print_readably));
extern void rl_list_funmap_names _PROTO((void));

/* Undocumented in the texinfo manual; not really useful to programs. */
extern int rl_translate_keyseq _PROTO((char *seq, char *array, int *len));
extern void rl_initialize_funmap _PROTO((void));

/* Functions for undoing. */
extern int rl_begin_undo_group _PROTO((void));
extern int rl_end_undo_group _PROTO((void));
extern void rl_add_undo _PROTO((enum undo_code what, int start, int end, char *text));
extern void free_undo_list _PROTO((void));
extern int rl_do_undo _PROTO((void));
extern int rl_modifying _PROTO((int start, int end));

/* Functions for redisplay. */
extern void rl_redisplay _PROTO((void));
extern int rl_forced_update_display _PROTO((void));
extern int rl_clear_message _PROTO((void));
extern int rl_reset_line_state _PROTO((void));
extern int rl_on_new_line _PROTO((void));

#if (defined (__STDC__) || defined(__cplusplus)) && defined (USE_VARARGS) && defined (PREFER_STDARG)
extern int rl_message (const char *, ...);
#else
extern int rl_message ();
#endif

/* Undocumented in texinfo manual. */
extern int rl_character_len _PROTO((int c, int pos));
extern int rl_show_char _PROTO((int c));
extern int crlf _PROTO((void));

/* Modifying text. */
extern int rl_insert_text _PROTO((char *string));
extern int rl_delete_text _PROTO((int from, int to));
extern int rl_kill_text _PROTO((int from, int to));
extern char *rl_copy_text _PROTO((int from, int to));

/* `Public' utility functions. */
extern int rl_reset_terminal _PROTO((char *terminal_name));
extern int rl_stuff_char _PROTO((int key));
extern int rl_read_key _PROTO((void));
extern int rl_getc _PROTO((FILE *stream));

extern int rl_initialize _PROTO((void));

/* Undocumented. */
extern int rl_expand_prompt _PROTO((char *prompt));
extern int rl_set_signals _PROTO((void));
extern int rl_clear_signals _PROTO((void));
extern int maybe_save_line _PROTO((void));
extern int maybe_unsave_line _PROTO((void));
extern int maybe_replace_line _PROTO((void));

/* Completion functions. */
/* These functions are from complete.c. */
extern int rl_complete_internal _PROTO((int what_to_do));

/* Return an array of strings which are the result of repeatadly calling
   FUNC with TEXT. */
extern char **completion_matches _PROTO((char *text, CPFunction *entry_function));
extern char *username_completion_function _PROTO((char *text, int state));
extern char *filename_completion_function _PROTO((char *text, int state));

#if defined(READLINE_LIBRARY)

#include "posixjmp.h" /* defines procenv_t */

/* NOTE: Functions and variables prefixed with `_rl_' are
   pseudo-global: they are global so they can be shared
   between files in the readline library, but are not intended
   to be visible to readline callers. */

/* Other undocumented. */
/* bind.c */
extern void rl_set_keymap_from_edit_mode _PROTO((void));
extern void _rl_bind_if_unbound _PROTO((char *keyseq, Function *default_func));
/* callback.c */
#if defined(READLINE_CALLBACKS)
extern void readline_internal_setup _PROTO((void));
#endif
/* display.c */
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
/* funmap.c */
extern int rl_add_funmap_entry _PROTO((char *name, Function *function));
extern char **rl_funmap_names _PROTO((void));
/* input.c */
extern int _rl_any_typein _PROTO((void));
extern int rl_execute_next _PROTO((int c));
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
extern int rl_insert_close _PROTO((int count, int invoking_key));
/* readline.c */
extern void _rl_init_line_state _PROTO((void));
extern void _rl_set_the_line _PROTO((void));
extern int _rl_dispatch _PROTO((int key, Keymap map));
extern int _rl_init_argument _PROTO((void));
extern void _rl_fix_point _PROTO((int fix_mark_too));
extern void _rl_replace_text _PROTO((char *text, int start, int end));
extern int _rl_char_search_internal _PROTO((int count, int dir, int schar));
extern void _rl_free_history_entry _PROTO((HIST_ENTRY *entry));
extern int _rl_set_mark_at_pos _PROTO((int position));
#if defined(READLINE_CALLBACKS)
extern void readline_internal_setup _PROTO((void));
extern char *readline_internal_teardown _PROTO((int eof));
extern int readline_internal_char _PROTO((void));
#endif
/* rltty.c */
extern void rl_prep_terminal _PROTO((int meta_flag));
extern void rl_deprep_terminal _PROTO((void));
extern void rltty_set_default_bindings _PROTO((Keymap kmap));
/* shell.c */
#include "shell.h"
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
extern void rl_extend_line_buffer _PROTO((int len));
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
/* xmalloc.c */
#include "xmalloc.h"

/* Unused and undocumented. */
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
extern char *rl_get_termcap _PROTO((char *cap));
/* undo.c */
extern int _rl_fix_last_undo_of_type _PROTO((int type, int start, int end));
/* util.c */
extern char *_rl_savestring _PROTO((char *s));

/* Undocumented variables. */
/* complete.c */
extern int _rl_complete_show_all;
extern int _rl_complete_mark_directories;
extern int _rl_print_completions_horizontally;
extern int _rl_completion_case_fold;
#if defined (VISIBLE_STATS)
extern int rl_visible_stats;
#endif /* VISIBLE_STATS */
extern int rl_complete_with_tilde_expansion;
extern int rl_completion_query_items;
extern int rl_inhibit_completion;
extern char *rl_completer_word_break_characters;
extern char *rl_basic_word_break_characters;
/* display.c */
extern int _rl_vis_botlin;
extern int _rl_last_c_pos;
extern int rl_display_fixed;
extern int _rl_suppress_redisplay;
extern char *rl_display_prompt;
/* funmap.c */
extern char *possible_control_prefixes[];
extern char *possible_meta_prefixes[];
/* macro.c */
extern int _rl_defining_kbd_macro;
extern char *_rl_executing_macro;
/* parens.c */
#if defined (PAREN_MATCHING)
extern int rl_blink_matching_paren;
#endif /* PAREN_MATCHING */
/* readline.c */
extern int _rl_horizontal_scroll_mode;
extern int _rl_mark_modified_lines;
extern int _rl_bell_preference;
extern int _rl_meta_flag;
extern int _rl_convert_meta_chars_to_ascii;
extern int _rl_output_meta_chars;
extern char *_rl_comment_begin;
extern int rl_explicit_arg;
extern int rl_editing_mode;
extern unsigned char _rl_parsing_conditionalized_out;
extern Keymap _rl_keymap;
extern char *rl_prompt;
extern int rl_visible_prompt_length;
extern Function *rl_last_func;
extern int readline_echoing_p;
extern FILE *rl_instream;
extern FILE *rl_outstream;
extern FILE *_rl_out_stream;
extern int rl_key_sequence_length;
extern int rl_pending_input;
extern HIST_ENTRY *saved_line_for_history;
extern int rl_line_buffer_len;
extern int rl_point;
extern int rl_end;
extern char *rl_line_buffer;
extern int _rl_last_command_was_kill;
extern int _rl_eof_char;
extern procenv_t readline_top_level;
extern int rl_mark;
extern int rl_done;
extern int rl_numeric_arg;
extern int rl_arg_sign;
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

#endif /* READLINE_LIBRARY */

/* **************************************************************** */
/*								    */
/*			Well Published Variables		    */
/*								    */
/* **************************************************************** */

/* The version of this incarnation of the readline library. */
extern char *rl_library_version;

/* The name of the calling program.  You should initialize this to
   whatever was in argv[0].  It is used when parsing conditionals. */
extern char *rl_readline_name;

/* The prompt readline uses.  This is set from the argument to
   readline (), and should not be assigned to directly. */
extern char *rl_prompt;

/* The line buffer that is in use. */
extern char *rl_line_buffer;

/* The location of point, and end. */
extern int rl_point;
extern int rl_end;

extern int rl_mark;

extern int rl_done;

extern int rl_pending_input;

/* Non-zero if we called this function from _rl_dispatch().  It's present
   so functions can find out whether they were called from a key binding
   or directly from an application. */
extern int rl_dispatching;

/* The name of the terminal to use. */
extern char *rl_terminal_name;

/* The input and output streams. */
extern FILE *rl_instream;
extern FILE *rl_outstream;

/* If non-zero, then this is the address of a function to call just
   before readline_internal () prints the first prompt. */
extern Function *rl_startup_hook;

/* The address of a function to call periodically while Readline is
   awaiting character input, or NULL, for no event handling. */
extern Function *rl_event_hook;

extern Function *rl_getc_function;
extern VFunction *rl_redisplay_function;
extern VFunction *rl_prep_term_function;
extern VFunction *rl_deprep_term_function;

/* Dispatch variables. */
extern Keymap rl_executing_keymap;
extern Keymap rl_binding_keymap;

/* Completion variables. */
/* Pointer to the generator function for completion_matches ().
   NULL means to use filename_entry_function (), the default filename
   completer. */
extern Function *rl_completion_entry_function;

/* If rl_ignore_some_completions_function is non-NULL it is the address
   of a function to call after all of the possible matches have been
   generated, but before the actual completion is done to the input line.
   The function is called with one argument; a NULL terminated array
   of (char *).  If your function removes any of the elements, they
   must be free()'ed. */
extern Function *rl_ignore_some_completions_function;

/* Pointer to alternative function to create matches.
   Function is called with TEXT, START, and END.
   START and END are indices in RL_LINE_BUFFER saying what the boundaries
   of TEXT are.
   If this function exists and returns NULL then call the value of
   rl_completion_entry_function to try to match, otherwise use the
   array of strings returned. */
extern CPPFunction *rl_attempted_completion_function;

/* The basic list of characters that signal a break between words for the
   completer routine.  The initial contents of this variable is what
   breaks words in the shell, i.e. "n\"\\'`@$>". */
extern char *rl_basic_word_break_characters;

/* The list of characters that signal a break between words for
   rl_complete_internal.  The default list is the contents of
   rl_basic_word_break_characters.  */
extern char *rl_completer_word_break_characters;

/* List of characters which can be used to quote a substring of the line.
   Completion occurs on the entire substring, and within the substring   
   rl_completer_word_break_characters are treated as any other character,
   unless they also appear within this list. */
extern char *rl_completer_quote_characters;

/* List of quote characters which cause a word break. */
extern char *rl_basic_quote_characters;

/* List of characters that need to be quoted in filenames by the completer. */
extern char *rl_filename_quote_characters;

/* List of characters that are word break characters, but should be left
   in TEXT when it is passed to the completion function.  The shell uses
   this to help determine what kind of completing to do. */
extern char *rl_special_prefixes;

/* If non-zero, then this is the address of a function to call when
   completing on a directory name.  The function is called with
   the address of a string (the current directory name) as an arg. */
extern Function *rl_directory_completion_hook;

/* Backwards compatibility with previous versions of readline. */
#define rl_symbolic_link_hook rl_directory_completion_hook

/* Non-zero means that the results of the matches are to be treated
   as filenames.  This is ALWAYS zero on entry, and can only be changed
   within a completion entry finder function. */
extern int rl_filename_completion_desired;

/* Non-zero means that the results of the matches are to be quoted using
   double quotes (or an application-specific quoting mechanism) if the
   filename contains any characters in rl_word_break_chars.  This is
   ALWAYS non-zero on entry, and can only be changed within a completion
   entry finder function. */
extern int rl_filename_quoting_desired;

/* Set to a function to quote a filename in an application-specific fashion.
   Called with the text to quote, the type of match found (single or multiple)
   and a pointer to the quoting character to be used, which the function can
   reset if desired. */
extern CPFunction *rl_filename_quoting_function;

/* Function to call to remove quoting characters from a filename.  Called
   before completion is attempted, so the embedded quotes do not interfere
   with matching names in the file system. */
extern CPFunction *rl_filename_dequoting_function;

/* Function to call to decide whether or not a word break character is
   quoted.  If a character is quoted, it does not break words for the
   completer. */
extern Function *rl_char_is_quoted_p;

/* Non-zero means to suppress normal filename completion after the
   user-specified completion function has been called. */
extern int rl_attempted_completion_over;

/* Set to a character describing the type of completion being attempted by
   rl_complete_internal; available for use by application completion
   functions. */
extern int rl_completion_type;

/* Character appended to completed words when at the end of the line.  The
   default is a space.  Nothing is added if this is '\0'. */
extern int rl_completion_append_character;

/* Up to this many items will be displayed in response to a
   possible-completions call.  After that, we ask the user if she
   is sure she wants to see them all.  The default value is 100. */
extern int rl_completion_query_items;

/* If non-zero, then disallow duplicates in the matches. */
extern int rl_ignore_completion_duplicates;

/* If this is non-zero, completion is (temporarily) inhibited, and the
   completion character will be inserted as any other. */
extern int rl_inhibit_completion;
   
/* Definitions available for use by readline clients. */
#define RL_PROMPT_START_IGNORE	'\001'
#define RL_PROMPT_END_IGNORE	'\002'

/* Possible values for do_replace argument to rl_filename_quoting_function,
   called by rl_complete_internal. */
#define NO_MATCH        0
#define SINGLE_MATCH    1
#define MULT_MATCH      2

#if !defined (savestring)
extern char *savestring _PROTO((char *s));	/* XXX backwards compatibility */
#endif

#endif /* _READLINE_H_ */
