/* rlxref.h -- prototypes for all readline library functions */
/* Bruno Haible 17.8.1994 */

#ifndef _XREF_H_
#define _XREF_H_

#include "sysdep.h"

#include "keymaps.h" /* defines the types Function, VFunction, Keymap */
#include "readline.h" /* defines enum undo_code */


/* bind.c */

/* Section "Binding keys" */
extern void rl_add_defun RL((char* name, VFunction* function, int key));
extern int rl_bind_key RL((int key, VFunction* function));
extern int rl_bind_key_in_map RL((int key, VFunction* function, Keymap map));
extern int rl_unbind_key RL((int key));
extern int rl_unbind_key_in_map RL((int key, Keymap map));
extern void rl_set_key RL((char* keyseq, VFunction* function, Keymap map));
extern void rl_macro_bind RL((char* keyseq, char* macro, Keymap map));
extern void rl_generic_bind RL((int type, char* keyseq, void* data, Keymap map));
extern int rl_translate_keyseq RL((char * seq, char * array, int* len));
extern VFunction * rl_named_function RL((char* string));
extern VFunction * rl_function_of_keyseq RL((char* keyseq, Keymap map, int* type));
extern void rl_re_read_init_file RL((int count, int ignore));
extern int rl_read_init_file RL((char* filename));

/* Section "Parser Directives" */
extern void rl_parse_and_bind RL((char* string));
extern void rl_variable_bind RL((char* name, char* value));
extern Keymap rl_get_keymap_by_name RL((char* name));
extern void rl_set_keymap RL((Keymap map));
extern Keymap rl_get_keymap RL((void));
extern void rl_set_keymap_from_edit_mode RL((void));

/* Section "Key Binding and Function Information" */
extern void rl_list_funmap_names RL((int ignore));
extern char** rl_invoking_keyseqs RL((VFunction* function));
extern void rl_dump_functions RL((int count));
extern void rl_function_dumper RL((int print_readably));


/* complete.c */

/* Section "Completion matching, from readline's point of view." */
extern void rl_complete RL((int ignore, int invoking_key));
extern void rl_possible_completions RL((void));
extern void rl_insert_completions RL((int ignore, int invoking_key));
extern int get_y_or_n RL((void));
extern void rl_complete_internal RL((int what_to_do));
extern char* username_completion_function RL((char* text, int state));

/* Section "Completion" */
extern char ** completion_matches RL((char* text, char* (*entry_function)()));
extern char* filename_completion_function RL((char* text, int state));
extern void rl_tilde_expand RL((int ignore, int key));

extern int rl_completion_query_items;


/* display.c */

/* Section "Display stuff" */
extern void rl_redisplay RL((void));
extern void rl_on_new_line RL((void));
extern void rl_on_new_line_with_prompt RL((void));
extern void rl_forced_update_display RL((void));
extern void _rl_move_cursor_relative RL((int new, char* data));
extern void _rl_move_vert RL((int to));
extern void rl_show_char RL((int c));
extern int rl_character_len RL((int c, int pos));
#if defined(__STDC__) && defined(HAVE_STDARG_H)
extern void rl_message RL((char* string, ...));
#else
#if defined(HAVE_VARARGS_H)
extern void rl_message RL(());
#else
extern void rl_message RL((char* string, long arg1, long arg2));
#endif
#endif
extern void rl_clear_message RL((void));
extern void rl_reset_line_state RL((void));
extern void _rl_erase_at_end_of_line RL((int l));

extern int rl_display_fixed;
/* extern char* rl_display_prompt; */
extern int _rl_last_c_pos;
/* extern int _rl_last_v_pos; */
extern int _rl_vis_botlin;


/* funmap.c */

#include "funmap.h"

extern void rl_add_funmap_entry RL((char* name, VFunction* function));
extern void rl_initialize_funmap RL((void));
extern char** rl_funmap_names RL((void));

extern char* possible_control_prefixes[];
extern char* possible_meta_prefixes[];


/* history.c */

#include "history.h"

/* Section "History Functions" */
extern HISTORY_STATE* history_get_history_state RL((void));
extern void history_set_history_state RL((HISTORY_STATE* state));
extern void using_history RL((void));
extern int history_total_bytes RL((void));
extern void add_history RL((char* string));
extern HIST_ENTRY *replace_history_entry RL((int which, char* line, void* data));
extern int where_history RL((void));
extern int history_search RL((char* string, int direction));
extern int history_search_prefix RL((char* string, int direction));
extern HIST_ENTRY *remove_history RL((int which));
extern void stifle_history RL((int max));
extern int unstifle_history RL((void));
extern int read_history RL((char* filename));
extern int read_history_range RL((char* filename, int from, int to));
extern void history_truncate_file RL((char* fname, int lines));
extern int append_history RL((int nelements, char* filename));
extern int write_history RL((char* filename));
extern HIST_ENTRY *current_history RL((void));
extern HIST_ENTRY *previous_history RL((void));
extern HIST_ENTRY *next_history RL((void));
extern HIST_ENTRY **history_list RL((void));
extern HIST_ENTRY *history_get RL((int offset));
extern int history_search_pos RL((char* string, int dir, int pos));
extern int history_set_pos RL((int pos));

/* Section "History Expansion" */
extern char *get_history_event RL((char* string, int* caller_index, int delimiting_quote));
extern int history_expand RL((char* string, char** output));
extern char *history_arg_extract RL((int first, int last, char* string));
extern char **history_tokenize RL((char* string));


/* isearch.c */

/* Section "I-Search and Searching" */
extern void rl_reverse_search_history RL((int sign, int key));
extern void rl_forward_search_history RL((int sign, int key));


/* keymaps.c */

#include "keymaps.h"

extern Keymap rl_make_bare_keymap RL((void));
extern Keymap rl_copy_keymap RL((Keymap map));
extern Keymap rl_make_keymap RL((void));
extern void rl_discard_keymap RL((Keymap map));


/* parens.c */

extern void rl_insert_close RL((int count, int invoking_key));


/* readline.c */

/* Section "Top Level Functions" */
extern char* readline RL((char* prompt));
extern char* readline_internal RL((void));

/* Section "Character Input Buffering" */
extern void rl_stuff_char RL((int key));
extern int ibuffer_space RL((void));
extern int rl_get_char RL((int *key));
extern int rl_unget_char RL((int key));
extern void rl_gather_tyi RL((void));
extern int rl_read_key RL((void));
extern void rl_dispatch RL((int key, Keymap map));

/* Section "Hacking Keyboard Macros" */
extern void rl_start_kbd_macro RL((int ignore1, int ignore2));
extern void rl_end_kbd_macro RL((int count, int ignore));
extern void rl_call_last_kbd_macro RL((int count, int ignore));
extern void _rl_kill_kbd_macro RL((void));

/* Section "Initializations" */
extern void rl_initialize RL((void));
extern void readline_initialize_everything RL((void));
extern void readline_default_bindings RL((void));

/* Section "Numeric Arguments" */
extern void rl_digit_argument RL((int ignore, int key));
extern void rl_discard_argument RL((void));
extern void rl_init_argument RL((void));
extern void rl_universal_argument RL((void));
extern void rl_digit_loop RL((void));

/* Section "Terminal and Termcap" */
extern void rl_reset_terminal RL((char *terminal_name));
extern void _rl_set_screen_size RL((int tty, int ignore_env));
extern void init_terminal_io RL((char *terminal_name));
extern int _rl_output_character_function RL((int c));
extern void _rl_output_some_chars RL((char* string, int count));
extern void backspace RL((int count));
extern void cr RL((void));
extern void crlf RL((void));

/* Section "Utility Functions" */
extern int alphabetic RL((int c));
extern int numeric RL((int c));
extern int ding RL((void));
#ifdef __GNUC__
typedef void rl_abort_fn RL((void));
extern __volatile__ rl_abort_fn rl_abort;
#else
extern void rl_abort RL((void));
#endif
extern char* rl_copy_text RL((int from, int to));
extern void rl_extend_line_buffer RL((int len));

/* Section "Insert and Delete" */
extern void rl_insert_text RL((char* string));
extern void rl_delete_text RL((int from, int to));

/* Section "Readline character functions" */

/* Section "Movement Commands" */
extern void rl_forward RL((int count));
extern void rl_backward RL((int count));
extern void rl_beg_of_line RL((void));
extern void rl_end_of_line RL((void));
extern void rl_forward_word RL((int count));
extern void rl_backward_word RL((int count));
extern void rl_refresh_line RL((void));
extern void rl_clear_screen RL((void));
extern void rl_arrow_keys RL((int count, int c));

/* Section "Text commands" */
extern void rl_insert RL((int count, int c));
extern void rl_quoted_insert RL((int count));
extern void rl_tab_insert RL((int count));
extern void rl_newline RL((int count, int key));
extern void rl_clean_up_for_exit RL((void));
extern void rl_do_lowercase_version RL((int ignore1, int ignore2));
extern void rl_rubout RL((int count));
extern void rl_delete RL((int count, int invoking_key));
extern void rl_delete_horizontal_space RL((int count, int ignore));

/* Section "Kill commands" */
extern void rl_unix_word_rubout RL((int count));
extern void rl_unix_line_discard RL((void));

/* Section "Commands For Typos" */

/* Section "Changing Case" */
extern void rl_upcase_word RL((int count));
extern void rl_downcase_word RL((int count));
extern void rl_capitalize_word RL((int count));

/* Section "Transposition" */
extern void rl_transpose_words RL((int count));
extern void rl_transpose_chars RL((int count));

/* Section "Undo, and Undoing" */
extern void rl_add_undo RL((enum undo_code what, int start, int end, char* text));
extern void free_undo_list RL((void));
extern int rl_do_undo RL((void));
extern void rl_begin_undo_group RL((void));
extern void rl_end_undo_group RL((void));
extern void rl_modifying RL((int start, int end));
extern void rl_revert_line RL((void));
extern void rl_undo_command RL((int count));

/* Section "History Utilities" */
extern void start_using_history RL((void));
extern void free_history_entry RL((HIST_ENTRY* entry));
extern void maybe_replace_line RL((void));
extern void maybe_unsave_line RL((void));
extern void maybe_save_line RL((void));

/* Section "History Commands" */
extern void rl_beginning_of_history RL((void));
extern void rl_end_of_history RL((void));
extern void rl_get_next_history RL((int count));
extern void rl_get_previous_history RL((int count));

extern void rl_execute_next RL((int c));

/* Section "The Mark and the Region." */
extern void rl_set_mark RL((int position));
extern void rl_exchange_mark_and_point RL((void));

/* Section "Killing Mechanism" */
extern void rl_set_retained_kills RL((int num));
extern void rl_kill_text RL((int from, int to));

/* Section "Killing Commands" */
extern void rl_kill_word RL((int count));
extern void rl_backward_kill_word RL((int count));
extern void rl_kill_line RL((int direction));
extern void rl_backward_kill_line RL((int direction));
extern void rl_yank RL((void));
extern void rl_yank_pop RL((void));
extern void rl_yank_nth_arg RL((int count, int ignore));

/* Section "Switching Modes" */
extern void rl_vi_editing_mode RL((void));
extern void rl_emacs_editing_mode RL((void));
#define no_mode -1
#define vi_mode 0
#define emacs_mode 1

/* Section "USG (System V) Support" */
extern int rl_getc RL((FILE* stream));

extern char* _rl_savestring RL((char* str));
#define savestring(x) _rl_savestring(x)

extern Keymap _rl_keymap;
extern char* rl_prompt;
extern int readline_echoing_p;
extern int rl_line_buffer_len;
extern int _rl_eof_char;
extern VFunction *rl_last_func;
extern HIST_ENTRY* saved_line_for_history;
extern int rl_editing_mode;
extern int screenwidth;
extern unsigned char _rl_parsing_conditionalized_out;
extern int _rl_horizontal_scroll_mode;
extern int _rl_mark_modified_lines;
extern int _rl_prefer_visible_bell;
extern int _rl_meta_flag;
extern int rl_blink_matching_paren;
extern int _rl_convert_meta_chars_to_ascii;
extern int rl_complete_with_tilde_expansion;
extern int rl_explicit_arg;
extern char* term_clreol;
extern char* term_IC;
extern char* term_im;
extern char* term_ei;
extern char* term_ic;
extern char* term_DC;
extern int rl_pending_input;


/* rltty.c */

/* Section "Saving and Restoring the TTY" */
extern void rl_prep_terminal RL((void));
extern void rl_deprep_terminal RL((void));

/* Section "Bogus Flow Control" */
extern void rl_restart_output RL((int count, int key));
extern void rl_stop_output RL((int count, int key));

/* Section "Default Key Bindings" */
extern void rltty_set_default_bindings RL((Keymap keymap));


/* search.c */

extern void rl_noninc_forward_search RL((int count, int key));
extern void rl_noninc_reverse_search RL((int count, int key));
extern void rl_noninc_forward_search_again RL((int count, int key));
extern void rl_noninc_reverse_search_again RL((int count, int key));


/* signals.c */

/* Section "Preventing Signals" */
extern void _rl_block_sigint RL((void));
extern void _rl_unblock_sigint RL((void));

/* Section "Signal Handling" */
extern void rl_set_signals RL((void));
extern void rl_clear_signals RL((void));


/* tilde.c */

extern char *tilde_expand RL((char* filename));
extern char *tilde_expand_word RL((char* filename));


/* vi_mode.c */

#if defined (VI_MODE)

/* Section "VI Emulation Mode" */
extern void rl_vi_set_last RL((void));
extern int rl_vi_textmod_command RL((int c));
extern void rl_vi_redo RL((int count, int c));
extern void rl_vi_yank_arg RL((int count));
extern void rl_vi_fetch_history RL((int count, int c));
extern void rl_vi_search_again RL((int count, int key));
extern void rl_vi_search RL((int count, int key));
extern void rl_vi_complete RL((int ignore, int key));
extern void rl_vi_tilde_expand RL((int ignore, int key));
extern void rl_vi_prev_word RL((int count, int key));
extern void rl_vi_next_word RL((int count, int key));
extern void rl_vi_end_word RL((int count, int key));
extern void rl_vi_fWord RL((int count));
extern void rl_vi_bWord RL((int count));
extern void rl_vi_eWord RL((int count));
extern void rl_vi_fword RL((int count));
extern void rl_vi_bword RL((int count));
extern void rl_vi_eword RL((int count));
extern void rl_vi_insert_beg RL((void));
extern void rl_vi_append_mode RL((void));
extern void rl_vi_append_eol RL((void));
extern void rl_vi_eof_maybe RL((int count, int c));
extern void rl_vi_insertion_mode RL((void));
extern void rl_vi_movement_mode RL((void));
extern void vi_done_inserting RL((void));
extern void rl_vi_arg_digit RL((int count, int c));
extern void rl_vi_change_case RL((int count, int ignore));
extern void rl_vi_put RL((int count, int key));
extern void rl_vi_check RL((void));
extern void rl_vi_column RL((int count));
extern int rl_vi_domove RL((int key, int* nextkey));
extern void rl_digit_loop1 RL((void));
extern void rl_vi_delete_to RL((int count, int key));
extern void rl_vi_change_to RL((int count, int key));
extern void rl_vi_yank_to RL((int count, int key));
extern void rl_vi_delete RL((int count));
extern void rl_vi_comment RL((void));
extern void rl_vi_first_print RL((void));
extern void rl_back_to_indent RL((int ignore1, int ignore2));
extern void rl_vi_char_search RL((int count, int key));
extern void rl_vi_match RL((void));
extern int rl_vi_bracktype RL((int c));
extern void rl_vi_change_char RL((int count, int key));
extern void rl_vi_subst RL((int count, int key));
extern void rl_vi_overstrike RL((int count, int key));
extern void rl_vi_overstrike_delete RL((int count));
extern void rl_vi_replace RL((int count, int key));
extern int rl_vi_possible_completions RL((void));

extern int _rl_vi_doing_insert;
extern int _rl_vi_last_command;
extern int _rl_vi_last_repeat;
extern int _rl_vi_last_arg_sign;
extern char* rl_vi_comment_begin;

#endif


/* xmalloc.c */

extern char* xmalloc RL((int bytes));
extern char* xrealloc RL((char* pointer, int bytes));


#endif

