/*
 * Driver program for CLISP.
 *
 * Needed so that we can write #!/usr/local/bin/clisp as the first line
 * of a Lisp script. Normally, only real executables and no shell scripts
 * can be mentioned after #!.
 *
 * Since we are at it, this driver program also implements the "-K" option.
 * All other options are passed to the main program.
 *
 * Bruno Haible 31.3.1997-2024
 * Sam Steingold 1998-2009, 2011
 */

/*
 * Macros passed during compilation:
 * LISPLIBDIR  string containing the directory with lisp.run and lispinit.mem
 * LOCALEDIR   string containing the locale directory
 */

#if 0
/*
 * Note: This file is preprocessed. Only the #if's and #include's with
 * no space after the # are resolved. The other ones are preserved by
 * the preprocessing.
 * This double pre-processing (once by txt2c on the build system
 * and once by cpp on the target system) is necessary because
 * the _distmakefile has this command:
 * $(CC) $(CFLAGS) -DLISPLIBDIR='"$(lisplibdir)"' -DLOCALEDIR='"$(localedir)"' src/clisp.c -o $(bindir)/clisp
 * So, _clisp.c is preprocessed for a first time on the system that
 * builds a binary distribution,
 * then preprocessed a second time while being compiled on the target system,
 * therefore we cannot not just include lispbibl.c and be done with it:
 * lispbibl.c contains #ifs that you cannot transport from one system to
 * another (e.g. from Solaris 9 to Solaris 10).
 * This is preferred over the 'hardcode' command, because that command
 * violates all abstraction, by assuming special things about object
 * files and executables.
 * (E.g. if some day encrypted or signed executables appear, the
 * 'hardcode' program will not work any more.)
 */
#endif

#include "config.h"

/* Declare strlen(), strcpy(), strcat(). */
# include <string.h>
/* Declare stderr, perror(). */
# include <stdio.h>

#if defined(WIN32_NATIVE)
# include <windows.h>
int shell_quote (char * dest, const char * source);
BOOL real_path (LPCSTR namein, LPSTR nameout);
# include "w32shell.c"
#endif

# include <stdlib.h>            /* getenv, abort */
#if defined(UNIX)
/* needed for execname.c to work */
# include <sys/types.h>         /* stat */
# include <sys/stat.h>          /* stat */
# include <unistd.h>            /* access */
# include <errno.h>             /* ENOMEM, ENOENT, errno */
# include <sys/param.h>         /* MAXPATHLEN */
#endif
int find_executable (const char * program_name);
#include <execname.c>

#if defined(UNIX_BINARY_DISTRIB)
# if !ENABLE_RELOCATABLE

char room_for_lisplibdir[10240] = "%MAGIC%LISPLIBDIR=" LISPLIBDIR;
# undef LISPLIBDIR
# define LISPLIBDIR  &room_for_lisplibdir[7 + strlen("LISPLIBDIR") + 1]

char room_for_localedir[10240] = "%MAGIC%LOCALEDIR=" LOCALEDIR;
# undef LOCALEDIR
# define LOCALEDIR  &room_for_localedir[7 + strlen("LOCALEDIR") + 1]

# endif
#endif

static int usage (char *program_name, char *option) {
  fprintf(stderr,"%s: invalid command-line option (%s), try `%s --help'\n",
          program_name,option,program_name);
  return 1;
}
#define USAGE(o) usage(program_name,o)

int main (int argc, char* argv[])
{
  char* lisplibdir;
  char* localedir;
  char* argv_lisplibdir = NULL;
#if defined(WIN32_NATIVE) && !defined(__MINGW32__)
  char* argv_linkingset = (char*)"";
#else
  char* argv_linkingset = (char*)"base";
#endif
  char* argv_memfile = NULL;
  char* argv_localedir = NULL;
  char* program_name;
  /*
   * To determine whether -K was given, we go through the options.
   * Because when "clisp foo.lisp -K" is invoked, the "-K" is an argument
   * for foo.lisp, not for clisp. Therefore we have to stop when we encounter
   * the first non-option, non-option-argument. Unfortunately, we have to
   * know about all of clisp's options.
   */
  program_name = argv[0];
  /*
   * The program_name may be absolute or relative if "clisp" is called
   * directly. (For example, "sh /usr/local/bin/clisp ..." will make it
   * absolute, "time clisp ..." will make it relative.)
   * If "clisp" is used as a script interpreter, program_name will be
   * - the full absolute pathname, on Solaris,
   * - only the basename, on Linux, AIX.
   * It follows that we cannot tell whether we have been called as
   * script interpreter or directly.
   */
# if ENABLE_RELOCATABLE
  /* Put this executable's absolute path into executable_name. */
  if (find_executable(program_name) < 0) {
    fprintf(stderr,"%s: cannot figure out the absolute executable path\n",
            program_name);
    return 1;
  }
  /* Figure out lisplibdir and localedir. */
  {
    unsigned int libdir_len;
    const char *p;
    char *mem;
    /* The libdir is determined as `dirname $executable_name`. */
    for (p = executable_name + strlen(executable_name);; p--) {
      if (p == executable_name) abort();
      if (*p == '/') break;
#ifdef WIN32_NATIVE
      if (*p == '\\') break;
#endif
    }
    libdir_len = p - executable_name;
    mem = (char*)malloc((libdir_len+1)+(libdir_len+7+1));
    if (mem == NULL) goto oom;
    lisplibdir = mem;
    localedir = mem + (libdir_len+1);
    /* Compute lisplibdir from it. */
    memcpy(lisplibdir,executable_name,libdir_len);
    lisplibdir[libdir_len] = '\0';
    /* Compute localedir from it. */
    memcpy(localedir,executable_name,libdir_len);
    localedir[libdir_len] = *p; /* directory separator */
    memcpy(localedir+libdir_len+1,"locale",6);
    localedir[libdir_len+7] = '\0';
  }
# else
  lisplibdir = LISPLIBDIR;
  localedir = LOCALEDIR;
# endif
  /*
   * Script execution on Unix is implemented like this:
   * - The basename/fullname of the interpreter is put into argv[0].
   * - (Optional - only if at least one interpreter-arg is present.) Next
   *   comes the tail of the "#!..." line. On Linux, AIX:
   *   with leading whitespace stripped, but whitespace inside it
   *   untouched (!). On Solaris: with leading whitespace stripped,
   *   and cut off at the next whitespace character (!!).
   * - Next comes the filename of the script.
   * - Then all the arguments of the script.
   * We therefore need to split argv[1] into pieces. We shouldn't split
   * "-x" arguments into pieces. However, fortunately, the "-x" argument
   * cannot appear as argv[1] (because "-x" must precede it), and it
   * cannot appear within the "#!..." line (because "#!" and "-x" are
   * mutually exclusive).
   * Also, don't split argv[1] into pieces if it is the filename of the
   * script. To determine this, look whether argv[1] is an option versus
   * a filename.
   * As a workaround against the Solaris problem, we split not
   * only at normal spaces, but also at hard spaces (in ISO-8859-1 encoding
   * and in UTF-8 encoding).
   * See <impnotes.html#quickstart>.
   */
  if (argc > 1 && argv[1][0] == '-') {
    int wordcount = 0; /* number of pieces in argv[1] */
    { char* arg = argv[1];
      int inword = 0;
      while (*arg != '\0') {
        int utf8_hard_space_p = (*arg == (char)0xC2 && arg[1] == (char)0xA0);
        int spacep = (*arg == '\t' || *arg == ' ' || *arg == (char)0xA0 || utf8_hard_space_p);
        if (!inword && !spacep) wordcount++;
        inword = !spacep;
        arg += 1 + utf8_hard_space_p;
      }
    }
    {int old_argc = argc;
     char** old_argv = argv;
     int new_argc = argc + wordcount - 1;
     char** new_argv = (char**) malloc((new_argc+1)*sizeof(char*));
     if (!new_argv) goto oom;
     argc = new_argc;
     argv = new_argv;
     /* Copy argv[0] unmodified. */
     *new_argv++ = *old_argv++;
     { /* Split argv[1] into pieces. */
       char* arg = *old_argv++;
       int inword = 0;
       while (*arg != '\0') {
         int utf8_hard_space_p = (*arg == (char)0xC2 && arg[1] == (char)0xA0);
         int spacep = (*arg == '\t' || *arg == ' ' || *arg == (char)0xA0 || utf8_hard_space_p);
         if (!inword) {
           if (!spacep) { *new_argv++ = arg; }
         } else {
           if (spacep) { *arg = '\0'; }
         }
         inword = !spacep;
         arg += 1 + utf8_hard_space_p;
       }
     }
     { /* Copy argv[2..argc-1] unmodified. */
       int i;
       for (i = old_argc-2; i > 0; i--) { *new_argv++ = *old_argv++; }
     }
     *new_argv = NULL;
    }
  }
  /*
   * Done with script interpreter argument handling.
   */
  { char** argptr = &argv[1];
    char** argptr_limit = &argv[argc];
    enum { for_exec, for_init, for_compile } argv_for = for_exec;
    while (argptr < argptr_limit) {
      char* arg = *argptr++;
      if ((arg[0] == '-') && !(arg[1] == '\0')) {
        switch (arg[1]) {
#        define OPTION_ARG                               \
          if (arg[2] == '\0') {                          \
            if (argptr < argptr_limit) arg = *argptr++;  \
            else return USAGE(arg);                      \
          } else { arg = &arg[2]; }
          /* Options to which we have to pay attention. */
          case 'b':             /* this is NOT a lisp.run option!!! */
            /* we could also use
                  clisp -q -norc -x '(namestring *lib-directory*)'
               instead of "clisp -b", but this shortcut saves an exec
               and really ensures that no debugging output
               (e.g., "STACK size" in spvw.d) gets in the way. */
            /* not puts() so that the woe32 CR will not get in the way;
               this makes screen output ugly, but this option is for
               Makefiles $(clisp -b) anyway, and that usage is now saved */
            printf("%s",lisplibdir);
            return 0;
          case 'B':
            OPTION_ARG;
            lisplibdir = argv_lisplibdir = arg;
            break;
          case 'K':             /* this is NOT a lisp.run option!!! */
            OPTION_ARG;
            argv_linkingset = arg;
            break;
          case 'M':
            OPTION_ARG;
            argv_memfile = arg;
            break;
          case 'N':
            OPTION_ARG;
            argv_localedir = arg;
            break;
          /* Skippable options without arguments. */
          case 'h':
          case 'd':
          case 'q':
          case 'I':
          case 'C':
          case 'l':
          case 'a':
          case 't':
          case 'w':
          case 'n': /* -norc */
          case 'r': /* -repl */
          case 'v':
            break;
          case '-':
            if (arg[2] == '\0') goto done_options; /* --: end of arguments */
            else break;   /* GNU-style long options --help, --version */
          /* Skippable options with arguments. */
          case 'm':
          case 'L':
          case 'o':
          case 'p':
          case 'x':
            OPTION_ARG;
            break;
          case 'E':
            if (argptr < argptr_limit) argptr++; else return USAGE("-E");
            break;
          case 'i':
            if (arg[2] == '\0') argv_for = for_init;
            break;
          case 'c':
            argv_for = for_compile;
            break;
          default:
            return USAGE(arg);
        }
      } else {
        switch (argv_for) {
          case for_init:
          case for_compile:
            break;
          case for_exec:
            /* All the remaining options are for the Lisp program. */
            goto done_options;
        }
      }
    }
   done_options: ;
  }
  { char* linkingsetdir;
    char* executable;
    char** new_argv;
    /* Compute linking set. */
    if (argv_linkingset == NULL || strlen(argv_linkingset)==0) {
      linkingsetdir = lisplibdir;
    } else if (argv_linkingset[0]=='/') {
      linkingsetdir = argv_linkingset;
    } else {
      linkingsetdir =
        (char*)malloc(strlen(lisplibdir)+1+strlen(argv_linkingset)+1);
      if (!linkingsetdir) goto oom;
      strcpy(linkingsetdir, lisplibdir);
      strcat(linkingsetdir, "/");
      strcat(linkingsetdir, argv_linkingset);
    }
    { /* Compute executable's name. */
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN)
      const char* execname = "lisp.exe";
#else
      const char* execname = "lisp.run";
#endif
      executable = (char*)malloc(strlen(linkingsetdir)+1+strlen(execname)+1);
      if (!executable) goto oom;
      strcpy(executable, linkingsetdir);
      strcat(executable, "/");
      strcat(executable, execname);
    }
    /* Compute new arguments. */
    new_argv = (char**)malloc((argc+6+1)*sizeof(char*));
    if (!new_argv) goto oom;
    new_argv[0] = executable;
    { char** argptr = &argv[1];
      char** argptr_limit = &argv[argc];
      char** new_argptr = &new_argv[1];
      if (!argv_lisplibdir) {
        *new_argptr++ = (char*)"-B";
        *new_argptr++ = lisplibdir;
      }
      if (!argv_memfile) {
        const char* filename = "lispinit.mem";
        argv_memfile =
          (char*)malloc(strlen(linkingsetdir)+1+strlen(filename)+1);
        if (!argv_memfile) goto oom;
        strcpy(argv_memfile, linkingsetdir);
        strcat(argv_memfile, "/");
        strcat(argv_memfile, filename);
        *new_argptr++ = (char*)"-M";
        *new_argptr++ = argv_memfile;
      }
      if (!argv_localedir) {
        *new_argptr++ = (char*)"-N";
        *new_argptr++ = localedir;
      }
      while (argptr < argptr_limit) { *new_argptr++ = *argptr++; }
      *new_argptr = NULL;
    }
    /* Launch the executable. */
#if defined(WIN32_NATIVE)
  #define W32ERR(f)  { what = #f; goto w32err; }
    {
      PROCESS_INFORMATION pinfo;
      char * command_line = NULL, *what;
      int cmd_len = 0, cmd_pos = 0;
      DWORD exitcode = 0;
      STARTUPINFO sinfo;
      char resolved[MAX_PATH];
      BOOL com_initialized = (CoInitialize(NULL) == S_OK);
      sinfo.cb = sizeof(STARTUPINFO);
      sinfo.lpReserved = NULL;
      sinfo.lpDesktop = NULL;
      sinfo.lpTitle = NULL;
      sinfo.cbReserved2 = 0;
      sinfo.lpReserved2 = NULL;
      sinfo.dwFlags = STARTF_USESTDHANDLES;
      sinfo.hStdInput  = GetStdHandle(STD_INPUT_HANDLE);
      if (sinfo.hStdInput == INVALID_HANDLE_VALUE)  W32ERR(STD_INPUT_HANDLE);
      sinfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
      if (sinfo.hStdOutput == INVALID_HANDLE_VALUE) W32ERR(STD_OUTPUT_HANDLE);
      sinfo.hStdError  = GetStdHandle(STD_ERROR_HANDLE);
      if (sinfo.hStdError == INVALID_HANDLE_VALUE)  W32ERR(STD_ERROR_HANDLE);
      /* conmand line */
      for (argv = new_argv; *argv; argv++) cmd_len += 2*strlen(*argv)+3;
      command_line = (char*)malloc(cmd_len);
      if (!command_line) goto oom;
      command_line[0] = 0;
      for (argv = new_argv; *argv; argv++) {
        if (cmd_pos) command_line[cmd_pos++] = ' ';
        cmd_pos += shell_quote(command_line+cmd_pos,*argv);
        if (cmd_pos > cmd_len) abort();
      }
      if (!CreateProcess((real_path(executable,resolved)
                          ? resolved : executable),
                         command_line, NULL, NULL, 1, 0,
                         NULL, NULL, &sinfo, &pinfo))
        W32ERR(CreateProcess);
      /* there is no reason to wait for CLISP to terminate, except that
         if we do not, the console i/o breaks down. */
      if (WaitForSingleObject(pinfo.hProcess,INFINITE) == WAIT_FAILED)
        W32ERR(WaitForSingleObject);
      if (!GetExitCodeProcess(pinfo.hProcess,&exitcode)) W32ERR(GetExitCodeProcess);
      if (!CloseHandle(pinfo.hProcess)) W32ERR(CloseHandle);
      if (com_initialized) CoUninitialize();
      return exitcode;
     w32err: {
      char *ret = NULL;
      int status = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                                 FORMAT_MESSAGE_FROM_SYSTEM |
                                 FORMAT_MESSAGE_IGNORE_INSERTS,
                                 NULL,GetLastError(),0,(LPTSTR) &ret,0,NULL);
      fprintf(stderr,"%s:\n * %s\n * %s\n * [%s]\n%s: %s\n",program_name,
              executable,resolved,command_line,what,
              (status ? ret : "FormatMessage() failed"));
      if (status) LocalFree(ret);
      return 1;
     }
    }
  #undef W32ERR
#else
    execv(executable,new_argv);
    { /* execv() returns only if there was an error. */
      int saved_errno = errno;
      fprintf(stderr,"%s: ",program_name);
      errno = saved_errno; perror(executable);
    }
    free(new_argv);
#endif
    return 1;
  }
  oom: {
    fprintf(stderr,"%s: out of memory\n",program_name);
    return 1;
  }
}
