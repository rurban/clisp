/* tilde.h: Externally available variables and function in libtilde.a. */

#ifndef _TILDE_H_
#define _TILDE_H_

#ifndef RL
/* For prototypes:  extern int foo RL((int x, int y)); */
#if defined(__STDC__) || defined(__cplusplus)
#define RL(args) args
#else
#define RL(args) ()
#endif
#endif

/* Function pointers can be declared as (Function *)foo. */
#ifndef _FUNCTION_DEF
#define _FUNCTION_DEF
#ifdef __cplusplus
typedef int Function (...);
typedef void VFunction (...);
typedef char *CPFunction (...);
typedef char **CPPFunction (...);
#else
typedef int Function ();
typedef void VFunction ();
typedef char *CPFunction ();
typedef char **CPPFunction ();
#endif
#endif

/* If non-null, this contains the address of a function to call if the
   standard meaning for expanding a tilde fails.  The function is called
   with the text (sans tilde, as in "foo"), and returns a malloc()'ed string
   which is the expansion, or a NULL pointer if there is no expansion. */
extern CPFunction *tilde_expansion_failure_hook;

/* When non-null, this is a NULL terminated array of strings which
   are duplicates for a tilde prefix.  Bash uses this to expand
   `=~' and `:~'. */
extern char **tilde_additional_prefixes;

/* When non-null, this is a NULL terminated array of strings which match
   the end of a username, instead of just "/".  Bash sets this to
   `:' and `=~'. */
extern char **tilde_additional_suffixes;

/* Return a new string which is the result of tilde expanding STRING. */
extern char *tilde_expand RL((char* filename));

/* Do the work of tilde expansion on FILENAME.  FILENAME starts with a
   tilde.  If there is no expansion, call tilde_expansion_failure_hook. */
extern char *tilde_expand_word RL((char* filename));

#endif /* _TILDE_H_ */
