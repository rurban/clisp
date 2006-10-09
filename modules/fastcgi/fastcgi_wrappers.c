/* fastcgi_wrappers.c

   These functions are in a separate file that includes the FastCGI
   headers, which override the normal stdio routines.

   Copyright (C) 2003 Alma Mater Software, Inc.
   Author: "John Kelley Hinsdale" <hin@alma.com>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2 as
   published by the Free Software Foundation; see file GNU-GPL.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

   $Id$
*/

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef _WIN32
#include <process.h>
#else
extern char **environ;
#endif

/* For strchr(), strncmp() and friends */
#define _GNU_SOURCE
#include <string.h> 

#include "fcgi_stdio.h"

/* Crank this up as needed */
#define TEMPBUFSIZE 65536

/* Local functions */
static char * read_stdio(FILE *);
static int    write_stdio(FILE *, char *, int);

/* Externally visible functions */

/* Searh environment for variable */
char * fcgi_getenv(char * var) {
  char **envp = environ;
  for ( ; *envp != NULL; envp++) {
	char * equ = strchr(*envp, '=');
	if ( ! equ )
	  continue;
	if ( 0 == strncmp(var, *envp, equ - *envp) )
	  return equ + 1;
  }
  /* Variable not found in environment */
  return 0;
}


/* Return the entire enviroment as a null-terminated linearized string
   array, e.g.: { "KEY0", "VAL0", "KEY1", "VAL1", NULL } */

char ** fcgi_env() {
  char **envp = environ, **result = 0;
  int nvar = 0, i = 0;
  
  /* Count up # of vars.  Allocate space for array of twice that many
     strings (key & value for each env var) plus a terminating NULL
     pointer. */
  for ( ; *envp != NULL; envp++ )
    nvar++;
  result = (char **) malloc(sizeof *result * (2 * nvar + 1));
  
  envp = environ;
  i = 0;
  for ( ; *envp != NULL; envp++, i+=2 ) {
    char * equ = strchr(*envp, '=');
    if ( ! equ ) {
      /* Env. string is ot of form KEY=VAL.  Unclear if this ever
         occurs.  If it does, treat the whole thing as the variable
         and map it to NIL; this is distinct from "VAR=" which will
         map to the empty string */
      result[i] = strdup(*envp);
      result[i+1] = 0;
    }
    else {
      result[i] = strndup(*envp, equ - *envp);
      result[i+1] = strdup(equ + 1);
    }
  }

  /* Terminate string array */
  result[i] = 0;
  return result;
}


/* Read some bytes from stdin.  Return a null-terminated string.  This
   does NOT necessarily read up to end of file, but rather will read until
   its buffer is full.  Therefore, if you want to slurp in the entire contents
   of STDIN (which you usually do) you have to call this repeatedly.

   Furthermore, the result is returned to CLISP as a null-terminated
   string, so that occurrences of NUL (binary 0) characters in the
   data will cause Lisp to get a short string.  In web forms, however,
   such data will usually be encoded.

*/
char * fcgi_read_stdin() {
  return read_stdio(stdin);
}
static char * read_stdio(FILE * f) {

  static char 	buf[TEMPBUFSIZE + 1];
  size_t 	  	nact = 0;

  if ( ! feof(f) )
	nact = fread(buf, 1, TEMPBUFSIZE, f);
  if ( ferror(f) )
	nact = 0;
  buf[nact] = '\0';
  return buf;
}

/* Write to stdout or stderr */
int fcgi_write_stdout(char * data, int len) {
  return write_stdio(stdout, data, len);
}
int fcgi_write_stderr(char * data, int len) {
  return write_stdio(stderr, data, len);
}
int write_stdio(FILE * f, char * data, int len) {
  return fwrite(data, 1, len, f);
}

/* Wrappers. These are needed only due to the user of upper case (how
   annoying) */
int fcgi_is_cgi_wrapper() {
  return FCGX_IsCGI();
}
int fcgi_accept_wrapper() {
  return FCGI_Accept();
}
void fcgi_finish_wrapper() {
  FCGI_Finish();
}
