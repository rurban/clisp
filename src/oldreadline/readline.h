/* readline.h -- changed by Bruno Haible, 7 January 1995 */

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
#  include "tilde.h"
#else
#  include <readline/ansi_proto.h>
#  include <readline/keymaps.h>
#  include <readline/tilde.h>
#endif

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
typedef struct {
  char *name;
  Function *function;
} FUNMAP;

extern FUNMAP **funmap;

/* The functions for manipulating the text of the line within readline.
Most of these functions are bound to keys by default. */
/* readline.c */
extern int rl_digit_argument _PROTO((int ignore, int key));
extern int rl_universal_argument _PROTO((void));
extern int rl_tty_status _PROTO((int count, int key));
extern int ding _PROTO((void));
#if defined(__GNUC__)
typedef void rl_abort_fn _PROTO((void));
extern __volatile__ rl_abort_fn rl_abort;
#else
extern void rl_abort _PROTO((void));
#endif
extern int rl_forward _PROTO((int count));
extern int rl_backward _PROTO((int count));
extern int rl_beg_of_line _PROTO((void));
extern int rl_end_of_line _PROTO((void));
extern int rl_forward_word _PROTO((int count));
extern int rl_backward_word _PROTO((int count));
extern int rl_clear_screen _PROTO((int count, int key));
extern int rl_insert _PROTO((int count, int c));
extern int rl_quoted_insert _PROTO((int count, int key));
extern int rl_tab_insert _PROTO((int count, int key));
extern int rl_newline _PROTO((int count, int key));
extern int rl_do_lowercase_version _PROTO((int ignore1, int ignore2));
extern int rl_rubout _PROTO((int count));
extern int rl_delete _PROTO((int count));
extern int rl_delete_horizontal_space _PROTO((int count, int ignore));
extern int rl_unix_word_rubout _PROTO((int count));
extern int rl_unix_line_discard _PROTO((void));
extern int rl_upcase_word _PROTO((int count, int key));
extern int rl_downcase_word _PROTO((int count, int key));
extern int rl_capitalize_word _PROTO((int count, int key));
extern int rl_transpose_words _PROTO((int count, int key));
extern int rl_transpose_chars _PROTO((int count, int key));
extern int rl_revert_line _PROTO((void));
extern int rl_undo_command _PROTO((int count, int key));
extern int rl_beginning_of_history _PROTO((int count, int key));
extern int rl_end_of_history _PROTO((int count, int key));
extern int rl_get_next_history _PROTO((int count));
extern int rl_get_previous_history _PROTO((int count));
extern int rl_kill_word _PROTO((int count));
extern int rl_backward_kill_word _PROTO((int count));
extern int rl_kill_line _PROTO((int direction));
extern int rl_backward_kill_line _PROTO((int direction));
extern int rl_kill_full_line _PROTO((int count, int ignore));
extern int rl_yank _PROTO((void));
extern int rl_yank_pop _PROTO((int count, int key));
extern int rl_yank_nth_arg _PROTO((int count, int ignore));
extern int rl_yank_last_arg _PROTO((int count, int key));
/* complete.c */
extern int rl_complete _PROTO((int ignore, int invoking_key));
extern int rl_possible_completions _PROTO((int ignore, int invoking_key));
extern int rl_insert_completions _PROTO((int ignore, int invoking_key));
extern int rl_tilde_expand _PROTO((int ignore, int key));
/* isearch.c */
extern int rl_reverse_search_history _PROTO((int sign, int key));
extern int rl_forward_search_history _PROTO((int sign, int key));
/* search.c */
extern int rl_history_search_forward _PROTO((int count, int ignore));
extern int rl_history_search_backward _PROTO((int count, int ignore));
/* bind.c */
extern int rl_re_read_init_file _PROTO((int count, int ignore));
extern int rl_read_init_file _PROTO((char *filename));
extern void rl_set_keymap_from_edit_mode _PROTO((void));
extern int rl_dump_functions _PROTO((int count, int key));
/* rltty.c */
extern void rl_prep_terminal _PROTO((int meta_flag));
extern void rl_deprep_terminal _PROTO((void));
extern int rl_restart_output _PROTO((void));
extern void rltty_set_default_bindings _PROTO((Keymap kmap));

/* `Public' utility functions. */
extern int rl_insert_text _PROTO((char *string));
extern int rl_delete_text _PROTO((int from, int to));
extern int rl_kill_text _PROTO((int from, int to));
extern int rl_complete_internal _PROTO((int what_to_do));
extern int rl_expand_prompt _PROTO((char *prompt));
extern int rl_initialize _PROTO((void));
extern int rl_set_signals _PROTO((void));
extern int rl_clear_signals _PROTO((void));
extern int rl_init_argument _PROTO((void));
extern int rl_digit_argument _PROTO((int ignore, int key));
extern int rl_read_key _PROTO((void));
extern int rl_getc _PROTO((FILE *stream));
extern int rl_stuff_char _PROTO((int key));
extern int maybe_replace_line _PROTO((void));
extern int maybe_unsave_line _PROTO((void));
extern int maybe_save_line _PROTO((void));
extern int rl_modifying _PROTO((int start, int end));

extern void rl_add_undo _PROTO((enum undo_code what, int start, int end, char *text));
extern void free_undo_list _PROTO((void));
extern int rl_do_undo _PROTO((void));
extern int rl_begin_undo_group _PROTO((void));
extern int rl_end_undo_group _PROTO((void));

/* Not available unless readline is compiled -DPAREN_MATCHING. */
extern int rl_insert_close _PROTO((int count, int invoking_key));

/* These are *both* defined even when VI_MODE is not. */
extern int rl_vi_editing_mode _PROTO((int count, int key));
extern int rl_emacs_editing_mode _PROTO((int count, int key));

/* Non incremental history searching. */
extern int rl_noninc_forward_search _PROTO((int count, int key));
extern int rl_noninc_reverse_search _PROTO((int count, int key));
extern int rl_noninc_forward_search_again _PROTO((int count, int key));
extern int rl_noninc_reverse_search_again _PROTO((int count, int key));

/* Things for vi mode. Not available unless readline is compiled -DVI_MODE. */
extern int rl_vi_textmod_command _PROTO((int c));
extern int rl_vi_redo _PROTO((int count, int c));
extern int rl_vi_yank_arg _PROTO((int count, int key));
extern int rl_vi_fetch_history _PROTO((int count, int c));
extern int rl_vi_search_again _PROTO((int count, int key));
extern int rl_vi_search _PROTO((int count, int key));
extern int rl_vi_complete _PROTO((int ignore, int key));
extern int rl_vi_tilde_expand _PROTO((int ignore, int key));
extern int rl_vi_prev_word _PROTO((int count, int key));
extern int rl_vi_next_word _PROTO((int count, int key));
extern int rl_vi_end_word _PROTO((int count, int key));
extern int rl_vi_fWord _PROTO((int count));
extern int rl_vi_bWord _PROTO((int count));
extern int rl_vi_eWord _PROTO((int count));
extern int rl_vi_fword _PROTO((int count));
extern int rl_vi_bword _PROTO((int count));
extern int rl_vi_eword _PROTO((int count));
extern int rl_vi_insert_beg _PROTO((int count, int key));
extern int rl_vi_append_mode _PROTO((void));
extern int rl_vi_append_eol _PROTO((int count, int key));
extern int rl_vi_eof_maybe _PROTO((int count, int c));
extern int rl_vi_insertion_mode _PROTO((void));
extern int rl_vi_movement_mode _PROTO((int count, int key));
extern int rl_vi_arg_digit _PROTO((int count, int c));
extern int rl_vi_change_case _PROTO((int count, int ignore));
extern int rl_vi_put _PROTO((int count, int key));
extern int rl_vi_check _PROTO((void));
extern int rl_vi_column _PROTO((int count, int key));
extern int rl_vi_delete_to _PROTO((int count, int key));
extern int rl_vi_change_to _PROTO((int count, int key));
extern int rl_vi_yank_to _PROTO((int count, int key));
extern int rl_vi_delete _PROTO((int count, int key));
extern int rl_vi_comment _PROTO((int count, int key));
extern int rl_vi_first_print _PROTO((int count, int key));
extern int rl_vi_char_search _PROTO((int count, int key));
extern int rl_vi_match _PROTO((int ignore, int key));
extern int rl_vi_bracktype _PROTO((int c));
extern int rl_vi_change_char _PROTO((int count, int key));
extern int rl_vi_subst _PROTO((int count, int key));
extern int rl_vi_overstrike _PROTO((int count, int key));
extern int rl_vi_overstrike_delete _PROTO((int count));
extern int rl_vi_replace _PROTO((int count, int key));

/* Keyboard macro commands. */
extern int rl_start_kbd_macro _PROTO((int ignore1, int ignore2));
extern int rl_end_kbd_macro _PROTO((int count, int ignore));
extern int rl_call_last_kbd_macro _PROTO((int count, int ignore));

extern int rl_refresh_line _PROTO((void));
extern int rl_arrow_keys _PROTO((int count, int key));

/* **************************************************************** */
/*								    */
/*			Well Published Variables		    */
/*								    */
/* **************************************************************** */

/* Always true. */
extern int rl_present_p;

/* The name of the calling program.  You should initialize this to
   whatever was in argv[0].  It is used when parsing conditionals. */
extern char *rl_readline_name;

/* The line buffer that is in use. */
extern char *rl_line_buffer;

/* The location of point, and end. */
extern int rl_point, rl_end;

/* The name of the terminal to use. */
extern char *rl_terminal_name;

/* The input and output streams. */
extern FILE *rl_instream, *rl_outstream;

/* The basic list of characters that signal a break between words for the
   completer routine.  The initial contents of this variable is what
   breaks words in the shell, i.e. "n\"\\'`@$>". */
extern char *rl_basic_word_break_characters;

/* The list of characters that signal a break between words for
   rl_complete_internal.  The default list is the contents of
   rl_basic_word_break_characters.  */
extern char *rl_completer_word_break_characters;

/* Basic list of quote characters */
extern char *rl_basic_quote_characters;

/* List of characters which can be used to quote a substring of the line.
   Completion occurs on the entire substring, and within the substring   
   rl_completer_word_break_characters are treated as any other character,
   unless they also appear within this list. */
extern char *rl_completer_quote_characters;

/* List of characters that are word break characters, but should be left
   in TEXT when it is passed to the completion function.  The shell uses
   this to help determine what kind of completing to do. */
extern char *rl_special_prefixes;

/* Pointer to the generator function for completion_matches ().
   NULL means to use filename_entry_function (), the default filename
   completer. */
extern CPFunction *rl_completion_entry_function;

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

/* If non-zero, then this is the address of a function to call just
   before readline_internal () prints the first prompt. */
extern Function *rl_startup_hook;

/* If non-zero, indicates that the caller of readline() has already
   output the prompt. */
extern int rl_already_prompted;

/* If non-zero, then this is the address of a function to call when
   completing on a directory name.  The function is called with
   the address of a string (the current directory name) as an arg. */
extern Function *rl_directory_completion_hook;

/* Backwards compatibility with previous versions of readline. */
#define rl_symbolic_link_hook rl_directory_completion_hook

/* The address of a function to call periodically while Readline is
   awaiting character input, or NULL, for no event handling. */
extern Function *rl_event_hook;

/* Non-zero means that modified history lines are preceded
   with an asterisk. */
extern int rl_show_star;

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

/* Non-zero means to suppress normal filename completion after the
   user-specified completion function has been called. */
extern int rl_attempted_completion_over;

/* **************************************************************** */
/*								    */
/*			Well Published Functions		    */
/*								    */
/* **************************************************************** */

/* Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none. */
extern char * readline _PROTO((char *prompt));

/* These functions are from complete.c. */
/* Return an array of strings which are the result of repeatadly calling
   FUNC with TEXT. */
extern char **completion_matches _PROTO((char *text, CPFunction *entry_function));
extern char *username_completion_function _PROTO((char *text, int state));
extern char *filename_completion_function _PROTO((char *text, int state));

/* These functions are from bind.c. */
/* rl_add_defun (char *name, Function *function, int key)
   Add NAME to the list of named functions.  Make FUNCTION
   be the function that gets called.
   If KEY is not -1, then bind it. */
extern int rl_add_defun _PROTO((char *name, Function *function, int key));
extern int rl_bind_key _PROTO((int key, Function *function));
extern int rl_bind_key_in_map _PROTO((int key, Function *function, Keymap map));
extern int rl_unbind_key _PROTO((int key));
extern int rl_unbind_key_in_map _PROTO((int key, Keymap map));
extern int rl_set_key _PROTO((char *keyseq, Function *function, Keymap map));
extern int rl_macro_bind _PROTO((char *keyseq, char *macro, Keymap map));
extern int rl_generic_bind _PROTO((int type, char *keyseq, void *data, Keymap map));
extern int rl_variable_bind _PROTO((char *name, char *value));
extern int rl_translate_keyseq _PROTO((char *seq, char *array, int *len));
extern Function *rl_named_function _PROTO((char *string));
extern Function *rl_function_of_keyseq _PROTO((char *keyseq, Keymap map, int *type));
extern int rl_parse_and_bind _PROTO((char *string));
extern Keymap rl_get_keymap _PROTO((void));
extern Keymap rl_get_keymap_by_name _PROTO((char *name));
extern void rl_set_keymap _PROTO((Keymap map));
extern char ** rl_invoking_keyseqs _PROTO((Function *function));
extern char ** rl_invoking_keyseqs_in_map _PROTO((Function *function, Keymap map));
extern void rl_function_dumper _PROTO((int print_readably));
extern int rl_re_read_init_file _PROTO((int count, int ignore));
extern void rl_list_funmap_names _PROTO((int count, int ignore));

/* Functions in funmap.c */
extern int rl_add_funmap_entry _PROTO((char *name, Function *function));
extern void rl_initialize_funmap _PROTO((void));

/* Functions in display.c */
extern void rl_redisplay _PROTO((void));
extern int rl_message ();
extern int rl_clear_message _PROTO((void));
extern int rl_reset_line_state _PROTO((void));
extern int rl_character_len _PROTO((int c, int pos));
extern int rl_show_char _PROTO((int c));
extern int rl_on_new_line _PROTO((void));
extern int rl_on_new_line_with_prompt _PROTO((void));
extern int rl_forced_update_display _PROTO((void));

extern int crlf _PROTO((void));

/* Definitions available for use by readline clients. */
#define RL_PROMPT_START_IGNORE	'\001'
#define RL_PROMPT_END_IGNORE	'\002'

#if !defined (savestring)
extern char *savestring _PROTO((char *s));	/* XXX backwards compatibility */
#endif

#endif /* _READLINE_H_ */
