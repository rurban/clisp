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
 * Bruno Haible 31.3.1997
 * Sam Steingold 1998-2005
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
 */
#endif

/* needed for execname.c to work
   the only reason at this time is the UNIX stat() headers */
# include "lispbibl.c"

/* Declare strlen(), strcpy(), strcat(). */
# include <string.h>
/* Declare stderr. */
# include <stdio.h>

#if defined(WIN32_NATIVE)
# include "w32shell.c"
#endif

# include "execname.c"

#if !defined(HAVE_PERROR_DECL)
/* Both <errno.h> and <stdio.h> failed to declare perror(). Declare it now. */
# if defined(__cplusplus)
extern "C" void perror (const char *);
# else
extern void perror (const char *);
# endif
#endif

#if defined(UNIX_BINARY_DISTRIB)

char room_for_lisplibdir[10240] = "%MAGIC%LISPLIBDIR=" LISPLIBDIR;
# undef LISPLIBDIR
# define LISPLIBDIR  &room_for_lisplibdir[7 + strlen("LISPLIBDIR") + 1]

char room_for_localedir[10240] = "%MAGIC%LOCALEDIR=" LOCALEDIR;
# undef LOCALEDIR
# define LOCALEDIR  &room_for_localedir[7 + strlen("LOCALEDIR") + 1]

#endif

int main (int argc, char* argv[])
{
  char* lisplibdir = LISPLIBDIR;
  char* localedir = LOCALEDIR;
  char* argv_lisplibdir = NULL;
#if defined(WIN32_NATIVE) && !defined(__MINGW32__)
  char* argv_linkingset = "";
#else
  char* argv_linkingset = "base";
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
   * - the full absolute pathname, on SunOS 4, Solaris, HP-UX, IRIX,
   * - only the basename, on Linux, AIX, OSF/1.
   * It follows that we cannot tell whether we have been called as
   * script interpreter or directly.
   */
  if (NULL == lisplibdir) {
    /* put my absolute path into executable_name */
    if (-1 == find_executable(program_name)) {
      fprintf(stderr,"%s: cannot figure out the absolute executable path",
              program_name);
      return -1;
    }
    { /* figure out lisplibdir and localedir */
      int exec_len = strlen(executable_name), lib_len;
      char *p = executable_name + exec_len;
      while (*p != '/' && *p != '\\') p--;
      lib_len = p-executable_name;
      lisplibdir = (char*)malloc(lib_len+1);
      if (NULL == lisplibdir) goto oom;
      strncpy(lisplibdir,executable_name,lib_len);
      lisplibdir[lib_len] = 0;
      if (NULL == localedir) {
        localedir = (char*)malloc(lib_len + 10);
        if (NULL == localedir) goto oom;
        strcpy(localedir,lisplibdir);
        strcat(localedir,"/locale/");
      }
    }
  }
  /*
   * Script execution on Unix is implemented like this:
   * - The basename/fullname of the interpreter is put into argv[0].
   * - (Optional - only if at least one interpreter-arg is present.) Next
   *   comes the tail of the "#!..." line. On SunOS 4, Linux, IRIX, AIX,
   *   OSF/1: with leading whitespace stripped, but whitespace inside it
   *   untouched (!). On Solaris, HP-UX: with leading whitespace stripped,
   *   and cut off at the next whitespace character (!!).
   * - Next comes the filename of the script.
   * - Then all the arguments of the script.
   * We therefore need to split argv[1] into pieces. We shouldn't split
   * "-x" arguments into pieces. However, fortunately, the "-x" argument
   * cannot appear as argv[1] (because "-x" must precede it), and it
   * cannot appear within the "#!..." line (because "#!" and "-x" are
   * mutually exclusive).
   * As a workaround against the Solaris/HP-UX problem, we split not
   * only at normal spaces, but also at hard spaces.
   * See <impnotes.html#quickstart>.
   */
  if (argc > 1) {
    int wordcount = 0; /* number of pieces in argv[1] */
    { char* arg = argv[1];
      int inword = 0;
      while (*arg != '\0') {
        int spacep = (*arg == '\t' || *arg == ' ' || *arg == (char)0xA0);
        if (!inword && !spacep) wordcount++;
        inword = !spacep;
        arg++;
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
         int spacep = (*arg == '\t' || *arg == ' ' || *arg == (char)0xA0);
         if (!inword) {
           if (!spacep) { *new_argv++ = arg; }
         } else {
           if (spacep) { *arg = '\0'; }
         }
         inword = !spacep;
         arg++;
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
#        define OPTION_ARG  \
          if (arg[2] == '\0') \
            { if (argptr < argptr_limit) arg = *argptr++; else goto usage; } \
          else { arg = &arg[2]; }
          /* Options to which we have to pay attention. */
          case 'B':
            OPTION_ARG;
            lisplibdir = argv_lisplibdir = arg;
            break;
          case 'K':
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
          case 'w':
          case 'n': /* -norc */
          case 'r': /* -repl */
          case 'v':
          case '-':
            break;
          /* Skippable options with arguments. */
          case 'm':
          case 's':
          case 't':
          case 'L':
          case 'o':
          case 'p':
          case 'x':
            OPTION_ARG;
            break;
          case 'E':
            if (argptr < argptr_limit) argptr++; else goto usage;
            break;
          case 'i':
            if (arg[2] == '\0') argv_for = for_init;
            break;
          case 'c':
            argv_for = for_compile;
            break;
          default:
            goto usage;
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
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
      char* execname = "lisp.exe";
#else
      char* execname = "lisp.run";
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
        *new_argptr++ = "-B";
        *new_argptr++ = lisplibdir;
      }
      if (!argv_memfile) {
        char* filename = "lispinit.mem";
        argv_memfile =
          (char*)malloc(strlen(linkingsetdir)+1+strlen(filename)+1);
        if (!argv_memfile) goto oom;
        strcpy(argv_memfile, linkingsetdir);
        strcat(argv_memfile, "/");
        strcat(argv_memfile, filename);
        *new_argptr++ = "-M";
        *new_argptr++ = argv_memfile;
      }
      if (!argv_localedir) {
        *new_argptr++ = "-N";
        *new_argptr++ = localedir;
      }
      while (argptr < argptr_limit) { *new_argptr++ = *argptr++; }
      *new_argptr = NULL;
    }
    /* Launch the executable. */
#if defined(WIN32_NATIVE)
    {
      PROCESS_INFORMATION pinfo;
      char * command_line = NULL;
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
      if (sinfo.hStdInput == INVALID_HANDLE_VALUE)  goto w32err;
      sinfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
      if (sinfo.hStdOutput == INVALID_HANDLE_VALUE) goto w32err;
      sinfo.hStdError  = GetStdHandle(STD_ERROR_HANDLE);
      if (sinfo.hStdError == INVALID_HANDLE_VALUE)  goto w32err;
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
        goto w32err;
      /* there is no reason to wait for CLISP to terminate, except that
         if we do not, the console i/o breaks down. */
      if (WaitForSingleObject(pinfo.hProcess,INFINITE) == WAIT_FAILED)
        goto w32err;
      if (!GetExitCodeProcess(pinfo.hProcess,&exitcode)) goto w32err;
      if (!CloseHandle(pinfo.hProcess)) goto w32err;
      if (com_initialized) CoUninitialize();
      return exitcode;
     w32err:
      fprintf(stderr,"%s:\n * %s\n * %s\n * [%s]\n = %s\n",program_name,
              executable,resolved,command_line,strerror(GetLastError()));
      return 1;
    }
#else
    execv(executable,new_argv);
    { /* execv() returns only if there was an error. */
      int saved_errno = errno;
      fprintf(stderr,"%s: ",program_name);
      errno = saved_errno; perror(executable);
    }
#endif
    return 1;
  }
  usage: {
    fprintf(stderr,"%s: invalid command-line option, try `%s --help'\n",
            program_name,program_name);
    return 1;
  }
  oom: {
    fprintf(stderr,"%s: out of memory\n",program_name);
    return 1;
  }
}
