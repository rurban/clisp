/* funmap.h -- Manipulation of readline funmaps. */
/* Bruno Haible 16.3.1993 */

#ifndef _FUNMAP_H_
#define _FUNMAP_H_

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

/* The data structure for mapping textual names to code addresses. */
typedef struct {
  char *name;
  VFunction *function;
} FUNMAP;

extern FUNMAP** funmap;

extern void rl_add_funmap_entry RL((char* name, VFunction* function));
extern void rl_initialize_funmap RL((void));
extern char** rl_funmap_names RL((void));

#endif /* _FUNMAP_H_ */
