/* tilde.c -- changed by Bruno Haible, 8 January 1995 */

/* tilde.c -- Tilde expansion code (~/foo := $HOME/foo). */

/* Copyright (C) 1988,1989, 1991 Free Software Foundation, Inc.

   This file is part of GNU Readline, a library for reading lines
   of text with interactive input and history editing.

   Readline is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Readline is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "sysdep.h"

#ifdef STDC_HEADERS
#include <stdlib.h> /* declares free(), getenv() */
#endif
#include <string.h> /* declares strlen(), strcmp(), strncmp(), strcpy(), strncpy(), strcat() */

#if !(defined(__MSDOS__) || defined(__EMX__))
#include <pwd.h> /* declares getpwnam(), getpwuid(), endpwent() */
extern struct passwd *getpwnam (/* [const] char *name */);
extern struct passwd *getpwuid RL((GETPWUID_UID_T uid));
#endif

#include "rlxref.h"
#include "tilde.h"

#if !defined (NULL)
#  define NULL 0x0
#endif

extern char *xmalloc RL((int bytes));
extern char *xrealloc RL((char *pointer, int bytes));

/* Forward declarations. */
static int tilde_find_prefix RL((char* string, int* len));
static int tilde_find_suffix RL((char* string));

/* The default value of tilde_additional_prefixes.  This is set to
   whitespace preceding a tilde so that simple programs which do not
   perform any word separation get desired behaviour. */
static char *default_prefixes[] =
  { " ~", "\t~", (char *)NULL };

/* The default value of tilde_additional_suffixes.  This is set to
   whitespace or newline so that simple programs which do not
   perform any word separation get desired behaviour. */
static char *default_suffixes[] =
  { " ", "\n", (char *)NULL };

/* If non-null, this contains the address of a function to call if the
   standard meaning for expanding a tilde fails.  The function is called
   with the text (sans tilde, as in "foo"), and returns a malloc()'ed string
   which is the expansion, or a NULL pointer if there is no expansion. */
CPFunction *tilde_expansion_failure_hook = (CPFunction *)NULL;

/* When non-null, this is a NULL terminated array of strings which
   are duplicates for a tilde prefix.  Bash uses this to expand
   `=~' and `:~'. */
char **tilde_additional_prefixes = default_prefixes;

/* When non-null, this is a NULL terminated array of strings which match
   the end of a username, instead of just "/".  Bash sets this to
   `:' and `=~'. */
char **tilde_additional_suffixes = default_suffixes;

/* Find the start of a tilde expansion in STRING, and return the index of
   the tilde which starts the expansion.  Place the length of the text
   which identified this tilde starter in LEN, excluding the tilde itself. */
static int
tilde_find_prefix (string, len)
     char *string;
     int *len;
{
  register int i, j, string_len;
  register char **prefixes = tilde_additional_prefixes;

  string_len = strlen (string);
  *len = 0;

  if (!*string || *string == '~')
    return (0);

  if (prefixes)
    {
      for (i = 0; i < string_len; i++)
	{
	  for (j = 0; prefixes[j]; j++)
	    {
	      if (strncmp (string + i, prefixes[j], strlen (prefixes[j])) == 0)
		{
		  *len = strlen (prefixes[j]) - 1;
		  return (i + *len);
		}
	    }
	}
    }
  return (string_len);
}

/* Find the end of a tilde expansion in STRING, and return the index of
   the character which ends the tilde definition.  */
static int
tilde_find_suffix (string)
     char *string;
{
  register int i, j, string_len;
  register char **suffixes = tilde_additional_suffixes;

  string_len = strlen (string);

  for (i = 0; i < string_len; i++)
    {
      if (string[i] == '/' || !string[i])
	break;

      for (j = 0; suffixes && suffixes[j]; j++)
	{
	  if (strncmp (string + i, suffixes[j], strlen (suffixes[j])) == 0)
	    return (i);
	}
    }
  return (i);
}

/* Return a new string which is the result of tilde expanding FILENAME. */
char *
tilde_expand (filename)
     char *filename;
{
  char *result;
  int result_size, result_index;

  result_size = result_index = 0;
  result = (char *)NULL;

  /* Scan through FILENAME expanding tildes as we come to them. */
  while (1)
    {
      register int start, end;
      char *tilde_word, *expansion;
      int len;

      /* Make START point to the tilde which starts the expansion. */
      start = tilde_find_prefix (filename, &len);

      /* Copy the skipped text into the result. */
      /* This test is always true the first time, since result_index
	 is 0, result_size is 0, and start is >= 0.  So we malloc here.  */
      if ((result_index + start + 1) > result_size) {
	result_size += (start + 20);
	result = (char *)xrealloc (result, 1 + result_size);
      }

      strncpy (result + result_index, filename, start);
      result_index += start;

      /* Advance FILENAME upto the starting tilde. */
      filename += start;

      /* Make END be the index of one after the last character of the
	 username. */
      end = tilde_find_suffix (filename);

      /* If both START and END are zero, we are all done. */
      if (!start && !end)
	break;

      /* Expand the entire tilde word, and copy it into RESULT. */
      tilde_word = (char *)xmalloc (1 + end);
      strncpy (tilde_word, filename, end);
      tilde_word[end] = '\0';
      filename += end;

      expansion = tilde_expand_word (tilde_word);
      free (tilde_word);

      len = strlen (expansion);
      if ((result_index + len + 1) > result_size)
	result = (char *)xrealloc (result, 1 + (result_size += (len + 20)));

      strcpy (result + result_index, expansion);
      result_index += len;
      free (expansion);
    }

  result[result_index] = '\0';

  return (result);
}

/* Do the work of tilde expansion on FILENAME.  FILENAME starts with a
   tilde.  If there is no expansion, call tilde_expansion_failure_hook. */
char *
tilde_expand_word (filename)
     char *filename;
{
  char *dirname = filename ? savestring (filename) : (char *)NULL;

  if (dirname && *dirname == '~')
    {
      char *temp_name;
      if (!dirname[1] || dirname[1] == '/')
	{
	  /* Prepend $HOME to the rest of the string. */
	  char *temp_home = getenv ("HOME");

#if !(defined(__MSDOS__) || defined(__EMX__))
	  /* If there is no HOME variable, look up the directory in
	     the password database. */
	  if (!temp_home)
	    {
	      struct passwd *entry;

	      entry = getpwuid (getuid ());
	      if (entry)
		temp_home = entry->pw_dir;
	    }
#endif

	  temp_name = (char *)alloca (1 + strlen (&dirname[1])
				      + (temp_home ? strlen (temp_home) : 0));
	  temp_name[0] = '\0';
	  if (temp_home)
	    strcpy (temp_name, temp_home);
	  strcat (temp_name, &dirname[1]);
	  free (dirname);
	  dirname = savestring (temp_name);
	}
      else
	{
#if !(defined(__MSDOS__) || defined(__EMX__))
	  struct passwd *user_entry;
#endif
	  char *username = (char *)alloca (strlen (dirname));
	  int i, c;

	  for (i = 1; (c = dirname[i]) != '\0' ; i++)
	    {
	      if (c == '/')
		break;
	      else
		username[i - 1] = c;
	    }
	  username[i - 1] = '\0';

#if !(defined(__MSDOS__) || defined(__EMX__))
	  if (!(user_entry = getpwnam (username)))
	    {
#endif
	      /* If the calling program has a special syntax for
		 expanding tildes, and we couldn't find a standard
		 expansion, then let them try. */
	      if (tilde_expansion_failure_hook)
		{
		  char *expansion;

		  expansion = (*tilde_expansion_failure_hook) (username);

		  if (expansion)
		    {
		      temp_name = (char *)alloca (1 + strlen (expansion)
						  + strlen (&dirname[i]));
		      strcpy (temp_name, expansion);
		      strcat (temp_name, &dirname[i]);
		      free (expansion);
		      free (dirname);
		      dirname = savestring (temp_name);
		    }
		}
	      /* We shouldn't report errors. */
#if !(defined(__MSDOS__) || defined(__EMX__))
	    }
	  else
	    {
	      temp_name = (char *)alloca (1 + strlen (user_entry->pw_dir)
					  + strlen (&dirname[i]));
	      strcpy (temp_name, user_entry->pw_dir);
	      strcat (temp_name, &dirname[i]);
	      free (dirname);
	      dirname = savestring (temp_name);
	    }
	  endpwent ();
#endif
	}
    }
  return (dirname);
}

#if defined (TEST)
#undef NULL
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  char *result, line[512];
  int done = 0;

  while (!done)
    {
      printf ("~expand: ");
      fflush (stdout);

      if (!gets (line))
	strcpy (line, "done");

      if ((strcmp (line, "done") == 0) ||
	  (strcmp (line, "quit") == 0) ||
	  (strcmp (line, "exit") == 0))
	{
	  done = 1;
	  break;
	}

      result = tilde_expand (line);
      printf ("  --> %s\n", result);
      free (result);
    }
  exit (0);
}
#endif /* TEST */

