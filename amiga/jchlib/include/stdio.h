/* Tiny GCC Library
 * stdio.h
 * Bruno Haible 12.4.1997
 */

#ifndef _STDIO_H_
#define _STDIO_H_

#include <exec/types.h>

typedef BCPL FILE;

#define stdin Input_handle
#define stdout Output_handle
#define stderr Output_handle
extern FILE* stdin;
extern FILE* stdout;
extern FILE* stderr;

#define fprintf BPTRfprintf
extern void fprintf (FILE*, ...);

#endif /* _STDLIO_H_ */
