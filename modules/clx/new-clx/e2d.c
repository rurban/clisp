/*
 *  Copyright (c) 1996-1997 by Gilbert Baumann, distributed under GPL
 *
 *    Title: 	Incredible weird program to produce equal weird CLISP modules
 *    Created:	Tue Jun 25 09:35:27 1996
 *    Author: 	Gilbert Baumann
 *		<unk6@rz.uni-karlsruhe.de>
 *       RCS:   $Id$
 *
 *
 *  This whole bunch of code written quick and dirty. [Mit der heiﬂen Nadel gestrickt!]
 */

/*
 * Revision 1.10  1997-09-21  bruno
 * - don't use memmove, for portability
 *
 * Revision 1.9  1997-09-17  bruno
 * - include <stdlib.h>, not <malloc.h>, for malloc() declaration
 *
 * Revision 1.8  1997-06-22  bruno
 * - compilation in WIDE mode works now (use `nullobj' instead of `0')
 *
 * $Log$
 * Revision 1.7  1996/10/11  15:07:58  gilbert
 * - removed all GETTEXT, it is broken with 07-22
 * - '#if 0'ed the error message in init_function_1, because err_asciz_out seems not to
 *   be available under 07-22
 * - made building of preload code optional, since we have now a better way to do this.
 *
 * Revision 1.6  1996/09/28  17:28:52  gilbert
 * - Got a few problems with line numbers causing very consfusing error
 *   messages during compiling; This should now be finally fixed.
 *
 * Revision 1.5  1996/09/27  13:12:45  gilbert
 * Changed on 'int' decl to 'uintC' to avoid annoying warnings about
 * comparison between signed and unsigned in code produced by e2d.
 * Further switched the message on emulated to some not so fearing sound.
 * This code should die, die, die! Why is it still alife? Grmpf!
 *
 * Revision 1.4  1996/07/30  22:58:12  gilbert
 * Nix besonderes, nur fuer Bruno.
 *
 * Revision 1.3  1996/07/25  02:08:41  gilbert
 * Fixed some bugs.
 *
 * Revision 1.2  1996/06/29  19:49:30  gilbert
 * Some major cleanups
 * Signatures not supported by CLISP are compiled by using inline code.
 */


/** TODO **
 *
 *  o Should'nt we rather make a whole preproccessor of it, named d2c or c2c?
 *  o Way to specify exact C name for subrs.
 *
 *  o BTW. Is it worth to write a CLISP specific peep-hole optimizer?
 *         [All this stack handling is not very clever compiled!]
 *
 *  o Automatic creation of export lists
 *  o Syntax check for inlined LISP data?
 *  o the #line statements should be put more carefuly!
 *    [I still have from time to time some diffs in line numbers]
 *  o quote '"' and '\' in LISP literals.
 *  o the user should not been forced to shout his function names.
 *  o think about saying 'defun (SETF FOO) (3) .... '
 */

#if defined(__TURBOC__) || defined(__GO32__) || defined(__WATCOMC__)
#define STDC_HEADERS 1
#else
#if defined(unix) || defined(__unix) || defined(_AIX) || defined(sinix) || defined(__POSIX__)
#include "unixconf.h"
#endif
#endif

#include <stdio.h>
#include <string.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#include <ctype.h>

/** Simply a list of all implemented subr signatures **/
char *valid_argtypes[] =
{
  "0, 0, norest, nokey",	"1, 0, norest, nokey",
  "2, 0, norest, nokey",	"3, 0, norest, nokey",
  "4, 0, norest, nokey",	"5, 0, norest, nokey",
  "6, 0, norest, nokey",	"0, 1, norest, nokey",
  "1, 1, norest, nokey",	"2, 1, norest, nokey",
  "3, 1, norest, nokey",	"4, 1, norest, nokey",
  "0, 2, norest, nokey",	"1, 2, norest, nokey",
  "2, 2, norest, nokey",	"0, 3, norest, nokey",
  "0, 4, norest, nokey",	"0, 5, norest, nokey",

  "0, 0, rest, nokey",		"1, 0, rest, nokey",
  "2, 0, rest, nokey",		"3, 0, rest, nokey",
  
  "0, 0, norest, key",		"1, 0, norest, key",
  "2, 0, norest, key",		"3, 0, norest, key",
  "4, 0, norest, key",		"0, 1, norest, key",
  "1, 1, norest, key",		"1, 2, norest, key",
  0,
};
/* Is it a good idea to hardwire this here? We could also skim the
 * lispbibl.d file for that.
 */

char e_fname [256];		/* Name of the source file (.e)  */
char d_fname [256];		/* Name of the output file (.d) */
char tabs_fname [256];		/* Name of the table file (.tabs.c) */
char module_name [256];		/* Name of the module w/o any suffix */


/*** Simple List Datatype **/

/* First of all we need a couple of lists of strings, so let us
   quickly define a simple datatype for that: */

typedef struct list { char *car; struct list *cdr; } list;
#define nil 0

/* From now on I use LISP notation for these lists, since everybody
   then knows, what I mean */

list *cons (char *ca, list *cd)
{
  list *r = (list*) malloc (sizeof (list));
  r->car = ca;
  r->cdr = cd;
  return r;
}

list *reappend (list *x, list *y)
     /* Stupid recursive implementation of a non-destructive reverse. */
{
  return x ? reappend (x->cdr, cons (x->car, y)) : nil;
}

#define reverse(x)  reappend(x,nil)
#define push(x,lst) ((lst) = cons(strdup(x),(lst)))
#define pushnew(x,y) fpushnew((x),&(y))

list *member (char *item, list *set)
{
  if (set == nil) return set;
  if (!strcasecmp (item, set->car)) return set;
  return member (item, set->cdr);
}

void fpushnew (char *item, list **stackf)
{
  if (!member (item, *stackf))
    push (item, *stackf);
}

int length (list *x)
{
  int n = 0;
  for(; x; x = x->cdr, n++);
  return n;
}

list *object_tab = nil;
list *object_init_tab = nil;
list *subr_tab = nil;
list *sym_tab = nil;
list *sexpr_tab = nil;
list *sexpr_name_tab = nil;
list *kw_tab = nil;

list *packages = nil;		/* List of all packages */
list *known_packages = nil;	/* List of all well known packages */

char *default_package = "LISP";	/* Default package to use (configurable?) */

void usage (void)
{
  fprintf (stderr, "usage: e2d <module-name> [<in>] [<out>]\n");
  exit (1);
}

#define MAX_DEF_LEN 40960	/* Why is this hardwired? */

struct defun
{
  char *lisp_name;		/* Name of function */
  char *lisp_pack;		/* Name of package */
  char *c_name;			/* C function name */
};

char *make_c_name (char *lisp_name)
     /* Converts a LISP name to a legal C name;
      * BUG: This function does not care about possible name clashes, or
      *      an identifier maximum length, the linker could understand.
      *      [Using gcc is not a solution on every system, since sometimes
      *       the vendors linker is used.]
      */
{
  char *temp = (char*)malloc (strlen (lisp_name) * 2 + 1); /* XXX may not be suffient! */
  char *r, *s, *d;

  d = temp;
  for (s = lisp_name; *s; s++)
    {
      if (isalnum(*s)) *(d++) = tolower (*s);
      else
	switch (*s)
	  {
	  case '-':
	    if (s[1] == '>')
	      { *(d++) = '2'; s++; }
	    else
	      *(d++) = '_';
	    break;

	  case ':':
	    *(d++) = '_'; break;

	  case '*':
	    strcpy (d, "_STAR_"); d += 6;
	    break;

	  case '%':
	    strcpy (d, "_PERCENT_"); d += 9;
	    break;
	    
	  default:
	    /* Well, we could be more clever here, simply insert the character as
	     * 'Cooo'!
	     */
	    fprintf (stderr, "Illegal character in symbol name `%s'.\n", lisp_name);
	    exit (1);
	  }
    }
  *d = 0;
  r = strdup (temp);
  free (temp);
  return r;
}

char *split_args (char *s, char **args)
{
  char *d, *e;
  int level = 0;
  while (*s != '\0' && isspace(*s)) s++;
  d = s;
  for (;*d;d++)
    {
      if (level == 0 && *d == ',')
	{
	  *d = 0;
	  *args = s;
	  e = d-1;
	  while (e >= s && isspace(*e)) e--;
	  *(e+1) = 0;
	  return split_args (d+1, args+1);
	}
      if (level == 0 && *d == ')')
	{
	  *d = 0;
	  *args = s;
	  return d+1;
	}
      if (*d == '(') level++;
      if (*d == ')') level--;
    }
  
  fprintf (stderr, "Missing ')'.\n");
  exit (1);
}

char *genvar (char *initval)
     /* Generate a new variable, which has to be initialized with the
      * value of initval. Is is to be pushed on the object_tab.
      */
{
  static int nn = 0;
  static char foo[100];

  nn++;
  sprintf (foo, "object v_%.5d;", nn); push (foo, object_tab);
  sprintf (foo, "\"%s\"", initval);   push (foo, object_init_tab);
  sprintf (foo, "v_%.5d", nn);
  return foo;
}


/*** Handling signatures; parsing and generating inline code ***/

int valid_signature_p (char *req, char *opt, char *rest, char *key)
     /* Check if the signature consisting of 'req','opt','rest' and
      * 'key' is valid. (i.e understood by the current version of
      * CLISP.
      * BUG: The arguments 'req' and 'opt' should be integers rather than strings.
      *      The compare operation should also be based on numerical values?!
      */
{
  static char tmp [32];

  if (strlen (req) + strlen (opt) + strlen (rest) + strlen (key) + 8 + 1 > sizeof(tmp))
    {
      /* If the whole length of the resulting sigature is too long, it could be hardly valid! */
      return 0;
    }
  else
    {
      char **q;
      sprintf (tmp, "%s, %s, %s, %s", req, opt, rest, key);
      for (q = valid_argtypes; *q; q++)
	if (!strcmp (tmp, *q))
	  return 1;
      return 0;
    }
}

char *parse_body (FILE *in, char *yet, char *buffer, int buffer_size)
{
  char *s;
  int level = 1;
  int c;

  char fetchc (void)
    {
      if (s[0] == '/' && s[1] == '*')
	{
	  /* BUG -- this implements nested comments! */
	  s += 2;
	  for(;;)
	    {
	      while (fetchc() != '*');
	      if (fetchc() == '/')
		return fetchc();
	    }
	}
      else
	if (*s)
	  return *(s++);
	else
	  if (fgets (s, buffer_size - (s - buffer), in) == NULL)
	    {
	      /* XXX lno or funcnam! */
	      fprintf (stderr, "EOF during parsing of function body.\n");	
	      fflush (stderr);
	      exit (1);
	    }
	  else
	    return fetchc ();
    }
  
  strcpy (buffer, yet);
  s = buffer;

  while ((c = fetchc (), isascii (c) && isspace (c)));	/* Skip white spaces */
  if (c != '{')
    {
      /* XXX see above XXX */
      fprintf (stderr, "Expecting a function body starting with '{' [got %c instead].\n",
	               c);
      fprintf (stderr, "Current buffer:\n");
      {
	char *t;
	for (t = buffer; *t; t++)
	  {
	    if (t == s) fprintf (stderr, "<*>");
	    if (*t == '\n') fprintf (stderr, "\\n"); else fprintf (stderr, "%c", *t);
	  }
	if (t == s) fprintf (stderr, "<*>");
	fprintf (stderr, "\n");
      }
      exit (1);
    }

  while (level != 0)
    {
      /* BUG -- what about string or character literals */
      int c = fetchc ();
      if (c == '{') level++;
      if (c == '}') level--;
    }

  /* s points directly beyond the closing brace */
  return s;
}

void parse_keywords (char *kws, int *n_out, char ***res_out)
{
  int n = 0;
  int i = 0;
  char **res;
  
  void count (char *s)	{ n++; }
  void put (char *s)	{ res[i++] = strdup (s); }

  /* TODO: Neat message, if keyword is no keyword.  */
  
  void parse (char *s, void (*consume)(char *))
    {
      static char buf[100];
      char *d;

      while (*s != ')')
	{
	  d = buf;
	  while (*s && isascii(*s) && isspace (*s)) s++;
	  while (*s && !(isascii(*s) && isspace (*s)) && *s!=')') *(d++) = *(s++);
	  if (*s == 0)
	    {
	      fprintf (stderr, "Error: Keyword list '%s' is missing ')'\n", kws);
	      exit (1);
	    }
	  *d = 0;
	  consume (buf);
	}
      s++;
      while (*s && isascii(*s) && isspace (*s)) s++;
      if (*s)
	{
	  fprintf (stderr, "Error: Garbage at end of keyword list '%s'.\n", kws);
	  exit (1);
	}
    }

  while (*kws && isascii(*kws) && isspace (*kws)) kws++;
  if (*kws != '(')
    {
      /* hugh?! */
      fprintf (stderr, "Error: Malformed keyword list: '%s'\n", kws);
      exit (1);
    }
  else
    {
      parse (kws+1, count);
      res = (char**)malloc (sizeof (char*) * (n+1));
      parse (kws+1, put);
      res[n] = 0;
      *n_out = n;
      *res_out = res;
      return;
    }
}

char *sexpr (char *x);

#if 0
#define X "\n"
#else
#define X ""
#endif

void compile_signature (FILE *sink, int req, int opt, int restflag, int keyflag, char *keys)
{
  if (restflag)
    {
      fprintf (stderr, "Bug: Extended signatures with &rest parameters are not yet supported.\n");
      /* But rather trivial, isn't it? */
      exit (1);
    }

  {
    char *ZU_WENIG = "{ pushSTACK (TheSubr(subr_self)->name); "
                     "  fehler (error, (\"EVAL/APPLY: too few arguments given to ~\"));"
                     "}";
    char *ZU_VIELE = "{ pushSTACK (TheSubr(subr_self)->name); "
                     "  fehler (error, (\"EVAL/APPLY: too many arguments given to ~\"));"
                     "}";
    char *KWS_ODD  = "{"
                     "  pushSTACK (TheSubr(subr_self)->name);"
                     "  fehler(error, (\"EVAL/APPLY: keyword arguments for ~ should occur pairwise\"));"
                     "}";
    char *KWS_INV  = "{ pushSTACK (%s);"
                     "  pushSTACK (TheSubr(subr_self)->name);"
		     "  pushSTACK (STACK_(i-1));"
                     "  fehler(error, (\"EVAL/APPLY: keyword ~ is illegal for ~. The possible keywords are ~\"));"
                     "}";
    
    int minargc, maxargc = -1;
    
    minargc = req;
    if (restflag || keyflag) maxargc = -1; else maxargc = minargc + opt;

    fprintf (sink, "if (argcount < %d) %s "X, minargc, ZU_WENIG);
    if (maxargc != -1)
      fprintf (sink, "if (argcount > %d) %s "X, maxargc, ZU_VIELE);

    /* process optional arguments */
    if (opt)
      fprintf (sink, "for (;argcount < %d; argcount++) pushSTACK(unbound); "X, req+opt);

    if (keyflag)
      {
	char **kws;
	int n_kws, i;

	parse_keywords (keys, &n_kws, &kws);
	if (n_kws == 0)
	  {
	    fprintf (stderr, "Error: hugh? ... there are't any keywords?");
	    exit (1);
	  }
	fprintf (sink, "{"X);
	fprintf (sink, "  uintC i;"X);
	fprintf (sink, "  if ((argcount-%d)%%2) %s "X, req+opt, KWS_ODD);
	fprintf (sink, "  skipSTACK ((-%d)); "X, n_kws);
	fprintf (sink, "  {"X);
	fprintf (sink, "    for (i = 0; i < argcount-%d; i++) "X, req+opt);
	fprintf (sink, "      STACK_(i) = STACK_(i+%d); "X, n_kws);
	fprintf (sink, "  } "X);
	for (i = 0; i < n_kws; i++)
	  fprintf (sink, "  STACK_(argcount-%d+%d) = unbound;"X,
		   (req+opt), i);
	fprintf (sink, "  for (i = argcount-%d; i > 0; i -= 2) "X, req+opt);
	fprintf (sink, "  { "X);
	for (i = 0; i < n_kws; i++)
	  {
	    fprintf (sink, "   %sif (eq (STACK_(i-1), %s))"X,
		     (i == 0) ? "" : "else ", sexpr (kws[i]));
	    fprintf (sink, "     STACK_(argcount-%d+%d) = STACK_(i-2);"X,
		     (req+opt), (n_kws - i)-1);
	  }
	fprintf (sink, "    else ");
	fprintf (sink, KWS_INV, sexpr (keys));
	fprintf (sink, ""X);
	fprintf (sink, "  } "X);
	fprintf (sink, "  skipSTACK (argcount - %d);"X, req+opt);  /* This will be optional on key_allow! */
	fprintf (sink, "}"X);
      }
  }
}

void parse_signature (char **arg, int *req_out, int *opt_out,
		                  int *restflag_out, int *keyflag_out)
{
  if (sscanf (arg[0], "%d", req_out) != 1)
    {
      fprintf (stderr, "Error: Parsing of signature failed. req field must be an integer.\n");
      exit (1);
    }

  if (sscanf (arg[1], "%d", opt_out) != 1)
    {
      fprintf (stderr, "Error: Parsing of signature failed. opt field must be an integer.\n");
      exit (1);
    }

       if (!strcmp (arg[2], "rest"))   *restflag_out = 1;
  else if (!strcmp (arg[2], "norest")) *restflag_out = 0;
  else
    {
      fprintf (stderr, "Error: During parsing of signature: rest field must be an 'rest' or 'norest'.\n");
      exit (1);
    }

       if (!strcmp (arg[3], "key"))    *keyflag_out = 1;
  else if (!strcmp (arg[3], "nokey"))  *keyflag_out = 0;
  else
    {
      fprintf (stderr, "Error: During parsing of signature: key field must be an 'key' or 'nokey'.\n");
      exit (1);
    }
}


/*** DEFUN -- handling defun forms ***/
char *do_defun (FILE *in, FILE *out, char *line)
{
  char *result;
  char name[100];
  char *sym_name, *pack;
  char *c_name;
  int level = 0;
  int n = 0, i;
  char *s, *d = line;
  char *arg[7];
  int simple_p;
  arg[0] = arg[1] = arg[2] = arg[3] = arg[4] = arg[5] = arg[6] = 0;
  
  for(;;)
    {
      for (s = d; *s; s++)
        if (*s == '(')
          level++;
        else if (*s == ')')
          level--;
      if (level == 0)
        break;
      if (fgets (s, MAX_DEF_LEN-(s-line), in) == NULL)
	{
	  fprintf (stderr, "EOF during parsing of definition.\n");
	  fprintf (stderr, "%s",line);
	  fflush (stderr);
	  exit(1);
	}
      n++;
      d = s;
    }

  s = line;
  d = name;
  while ((*s) && isspace (*s)) { s++; }
  while ((*s) && !isspace (*s)) { *(d++) = *(s++); }
  *d = 0;
  c_name = make_c_name (name);

  while ((*s) && ((*s)!='(')) s++;
  if (!*s) { fprintf (stderr, "Missing '('.\n"); exit(1); }
  s++;
  result = split_args (s, arg);

  { char *k; for (k = name; *k; k++) *k = toupper (*k); }
/*  fprintf (stderr, "; %s\n", name);fflush(stderr);*/

  /* Canonicalize the arg vector */

  arg[0] = arg[0]?:"0";		/* BTW. x?=:y would be nice. */
  simple_p = !arg[1];
  arg[1] = arg[1]?:"0";
  arg[2] = arg[2]?:"norest";
  arg[3] = arg[3]?:"nokey";
  arg[4] = arg[4]?:"0";
  
  for (i = 0; i < n; i++)
    fprintf (out, "\n"); /* keep lines in sync */
  
  /* Check if the signature could be understood by CLISP */
  if (valid_signature_p (arg[0], arg[1], arg[2], arg[3]))
    {
      /* Understood by CLISP, fine. */
      if (simple_p)
	fprintf (out, "LISPFUNN (%s, %s)", c_name, arg[0]);
      else
 	fprintf (out, "LISPFUN (%s, %s, %s, %s, %s, %s, NIL)",
		 c_name, arg[0], arg[1], arg[2], arg[3], arg[4]);
    }
  else
    {
      char *body = (char*) malloc (40960);	/* That should be enough */
      
      /* CLISP does not know the signature, bad. */
      fprintf (stderr, "Notice: Signature (%s, %s, %s, %s) for function '%s' will be emulated.\n",
	               arg[0], arg[1], arg[2], arg[3],
	               name);
      fprintf (out, "LISPFUN (%s, 0, 0, rest, nokey, 0, NIL)", c_name);
      
      /* Now slurp in the whole definition of the function and
       * interpret the arguments on our own.
       */

      {
	char *coke;
	int len;
	char *pepsi;
	int req, opt, restflag, keyflag;

	coke = parse_body (in, result, body, 40960);
	len  = strlen (body);
	pepsi = body + len;
	while (pepsi != coke) { pepsi[0] = pepsi[-1]; pepsi--; }
	coke[0] = '}';		/* hack, hack */
	result = body;

	parse_signature (arg, &req, &opt, &restflag, &keyflag);

	fprintf (out, "{");
	compile_signature (out, req, opt, restflag, keyflag, arg[5]);
	/* patch arg[0] .. arg[5] */
	arg[0] = "0";
	arg[1] = "0";
	arg[2] = "rest";
	arg[3] = "nokey";
	arg[4] = "0";
	arg[5] = "NIL";
      }
    }

  if ((sym_name = strchr (name, ':')))
    {
      *(sym_name++) = 0;
      pack = name;
    }
  else
    {
      pack = default_package;
      sym_name = name;
    }

  pushnew (pack, packages);
  
  {
    static char subr [4096];
    static char sym [1024];
    sprintf (subr,
	     "{ (lisp_function)(&C_%s), nullobj,NIL,0, %s, %s, (uintB)subr_%s, (uintB)subr_%s, %s, },",
	     c_name,
	     arg[0], arg[1], arg[2], arg[3], arg[4]);
    sprintf (sym , "{ \"%s\", \"%s\", },", pack, sym_name);
    push (subr, subr_tab);
    push (sym, sym_tab);
    if (arg[5] && !!strcmp(arg[5],"NIL"))
      {
	static int nn = 0;
	static char ob[4000];
	static char oi[4000];
	nn++;
	sprintf (ob, "object kw_%.5d;", nn); push (ob, object_tab);
	sprintf (oi, "\"#%s\"", arg[5]); push (oi, object_init_tab);
	sprintf (ob, "object kw_%.5d_fun;", nn); push (ob, object_tab);
	sprintf (oi, "\"%s::%s\"", pack, sym_name); push (oi, object_init_tab);
	sprintf (oi, "kw_%.5d", nn); push (oi, kw_tab);
      }
  }
  return result;
}


/*** DEFVAR -- handling variable definitions ***/

void do_defvar (FILE *in, FILE *out, char *line)
{
  char *var, *val, *s;

  if ((s = strchr (line, '=')))
    {
      var = line; *s = 0; val = s+1;
      if ((s = strchr (val, ';')))
	{
	  *s = 0;
	}
      else
	{
	  fprintf (stderr, "Missing semicolon!\n");
	  exit(1);
	}
    }
  else
    {
      var = line;
      val = "NIL";
    }
  fprintf (out, "#define %s (module__%s__object_tab.%s)\n", var, module_name, genvar(val));
}

char *put_quoted_string (char *dest, char *src)
{
  *(dest++) = '"';
  for (;*src;src++)
    switch (*src)
      {
      case '\\': case '"':
	*(dest++) = '\\'; *(dest++) = *src; break;
      case '\n':
	*(dest++) = '\\'; *(dest++) = 'n'; break;
      case '\t':
	*(dest++) = '\\'; *(dest++) = 't'; break;
      default:
	*(dest++) = *src;
      }

  *(dest++) = '"';
  *(dest++) = 0;
  return dest;
}

char *sexpr (char *x)
{
  static int nn = 0;
  static char foo[10000];
  list *q, *p;
  
  for (q = sexpr_tab, p = sexpr_name_tab; q; q = q->cdr, p = p->cdr)
    if (!strcasecmp (x, q->car))
      {
	sprintf (foo, "(module__%s__object_tab.%s)/*%s*/",
		 module_name, p->car, x);
	return foo;
      }
  
  nn++;
  sprintf (foo, "object o_%.5d;", nn); push (foo, object_tab);
  put_quoted_string (foo, x); push (foo, object_init_tab);
  push (x, sexpr_tab);
  sprintf (foo, "o_%.5d", nn);
  push (foo, sexpr_name_tab);
  sprintf (foo, "(module__%s__object_tab.o_%.5d)/*%s*/",
	   module_name, nn, x);
  return foo;
}

void process_line (FILE *in, FILE *out, char *line)
{
  if (!strncmp (line, "defvar ", 7))
    do_defvar (in, out, line+7);
  else
    {
      char *s;
      char *sexp;
      for (s = line; *s; )
	{
	  if (*s == '`')
	    {
	      s++;
	      sexp = s;
	      while ((*s) && (*s)!='`') s++;
	      *s = 0;
	      fprintf (out, "%s", sexpr (sexp));
	      s++;
	    }
	  else
	    if (!strncmp (s, "defun ", 6))
	      {
		s = do_defun (in,out,s+6);
	      }
	    else
	      if (!strncmp (s, "# ", 2))
		{
		  fprintf (out, "#line ");
		  s += 2;
		}
	      else
		putc (*(s++), out);
	}
    }
}

void conv (FILE *in, FILE *out)
{
  char line [MAX_DEF_LEN];
  while (fgets (line, sizeof(line), in))
      process_line (in,out,line);
}

void care_about_packages (FILE *sink)
{
  list *q;
  list *to_create = nil;

  /* First of all inform the user about used packages ... */
  fprintf (stderr, ";Used packages: ");
  for (q = packages; q; q = q->cdr)
    fprintf (stderr, "%s%s", q->car, q->cdr ? ", " : "");
  fprintf (stderr, ".\n");

  /* Collect all packages, which are not well known, and which must be created ... */
  for (q = packages; q; q = q->cdr)
    if (!member (q->car, known_packages))
      push (q->car, to_create);

#if EMIT_PRELOAD
  /* Now emit the preload module */
  fprintf (sink,
	   "struct { object o_fake; } module__%s_PRELOAD__object_tab;\n"
	   "object_initdata module__%s_PRELOAD__object_tab_initdata[1] = {0}; \n"
	   "uintC module__%s_PRELOAD__object_tab_size = 0;\n"
	   "uintC module__%s_PRELOAD__subr_tab_size = 0;\n"
	   "subr_ module__%s_PRELOAD__subr_tab [1] = {0};\n"
	   "subr_initdata module__%s_PRELOAD__subr_tab_initdata[1] = {0};\n"
	   "void module__%s_PRELOAD__init_function_2 (module_ *module) { }\n",
	   module_name, module_name, module_name,
	   module_name, module_name, module_name, module_name);
  
  fprintf (sink, "void module__%s_PRELOAD__init_function_1 (module_ *module)\n"
	         "{\n"
	         "  module_%s_PRELOAD_init_p = 1;\n  \n",
	         module_name, module_name);
  for (q = to_create; q; q = q->cdr)
    {
      fprintf (sink, "  if (nullp (find_package (ascii_to_string (\"%s\"))))\n"
	             "    {\n"
	             "      pushSTACK (ascii_to_string (\"%s\"));\n"
	             "      funcall (L (make_package), 1);\n"
	             "    }\n",
	       q->car, q->car);
    }
  fprintf (sink, "}\n");
#endif
}

void init (void)
{
  /* Initalize the well known package list */
  push ("LISP", known_packages); 	/* Sure! */
  push ("SYSTEM", known_packages); 	/* Sure! */
  push ("USER", known_packages); 	/* Bad style, but legal... */
#if 0
  /* Should put them on the list either way round?  Well, these names
   * are somewhat resevered and because of that nobody should
   * put something there.
   */
  push ("FFI", known_packages);		/* very likely */
  push ("SCREEN", known_packages); 	/* also likely */
#endif
}

int main (int argc, char **argv)
{
  if (argc<2 || argc>4) usage();

  init ();
  
  strcpy (module_name, argv[1]);
  if (argc>2) strcpy (e_fname, argv[2]); else sprintf (e_fname, "%s.e", module_name);
  if (argc>3) strcpy (d_fname, argv[3]); else sprintf (d_fname, "%s.d", module_name);
  sprintf (tabs_fname, "%s.tabs.c", module_name);
  fprintf (stderr, ";Converting module `%s' (%s --> %s)\n", module_name, e_fname, d_fname);

  {
    FILE *in = fopen (e_fname, "r");
    FILE *out = fopen (d_fname, "w");
    list *q;
    
    conv (in, out);
    fprintf (out, "uintC module__%s__subr_tab_size = %d;\n", module_name, length (subr_tab));
    fprintf (out, "subr_ module__%s__subr_tab [%d] = \n", module_name, length (subr_tab));
    fprintf (out, "{\n");
    for (q = subr_tab; q; q = q->cdr)
      fprintf (out, "    %s\n", q->car);
    fprintf (out, "};\n");

    fprintf (out, "subr_initdata module__%s__subr_tab_initdata[%d] =\n",
	     module_name, length (subr_tab));
    fprintf (out, "{\n");
    for (q = sym_tab; q; q = q->cdr)
      fprintf (out, "    %s\n", q->car);
    fprintf (out, "};\n");

#if EMIT_PRELOAD
    fprintf (out, "static int module_%s_PRELOAD_init_p = 0;\n", module_name);
#endif
    fprintf (out, "void module__%s__init_function_1 (module_ *module)\n", module_name);
    fprintf (out, "{\n");

#if 0 && EMIT_PRELOAD
    fprintf (out, "  if(!module_%s_PRELOAD_init_p)\n", module_name);
    fprintf (out, "    {\n");
    fprintf (out,
     "       err_asciz_out (\"Module %s: WARNING:\\n\"\n"
     "                      \"  Probably you forgot to include the '%s_PRELOAD' module.\\n\"\n"
     "                      \"  (Or you did in the wrong order; '%s_PRELOAD' must been\\n\"\n"
     "                      \"   initialized *prior* to module %s!)\");\n",
	     module_name, module_name, module_name, module_name);
    fprintf (out, "    }\n");
    /* NOTE: This will hardly ever been called! */
#endif

    for (q = kw_tab; q; q = q->cdr)
      fprintf (out, "  TheSubr(Symbol_function(module__%s__object_tab.%s_fun))->keywords"
	                 "= module__%s__object_tab.%s;\n",
	       module_name, q->car, module_name, q->car);
    fprintf (out, "}\n");
    care_about_packages (out);
    
    fclose (out);
    out = fopen (tabs_fname, "w");
    fprintf (out, "/* tables */\n");
    fprintf (out, "struct { \n");
    for (q = object_tab; q; q = q->cdr)
      fprintf (out, "    %s\n", q->car);
    fprintf (out, "\n} module__%s__object_tab;\n", module_name);
    fprintf (out, "object_initdata module__%s__object_tab_initdata[%d] = \n",
	     module_name, length (object_tab));
    fprintf (out, "{\n");
    for (q = object_init_tab; q; q = q->cdr)
      fprintf (out, "    %s,\n", q->car);
    fprintf (out, "};\n");
    fprintf (out, "uintC module__%s__object_tab_size = %d;\n", module_name, length (object_tab));
    fprintf (out, "\n");

    fclose (in);
    fclose (out);
  }
  
  /* We got here -- probably an error. :-) */
  return 0;
}


