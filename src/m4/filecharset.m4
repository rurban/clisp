dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.13)

AC_DEFUN([CL_FILECHARSET],
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([CL_CLOSEDIR])dnl
AC_MSG_CHECKING(for the valid characters in filenames)
AC_CACHE_VAL(cl_cv_os_valid_filename_char,[
if test $cross_compiling = no; then
dnl Create the subdirectory the test program will use for its files.
mkdir conftestdir
cat > conftest.c <<EOF
#include "confdefs.h"
#include <sys/types.h>
#include <stdlib.h>
/* Declare chdir(). */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <stdio.h>
/* Declare open(). */
#include <fcntl.h>
#ifdef OPEN_NEEDS_SYS_FILE_H
#include <sys/file.h>
#endif
/* Declare opendir(), readdir(), closedir(). */
#include <$ac_header_dirent>
#ifdef DIRENT
#define SDIRENT struct dirent
#else
#define SDIRENT struct direct
#endif
changequote(,)dnl
/* A small program which checks for each character whether or not it is
 * valid in filenames. */
#define N 256
int main ()
{
#if defined(__CYGWIN32__) || defined(__MINGW32__)
  /* The test below would cause a dialog box to pop up (ch == ':'),
     and create files which cause "rm -rf conftest*" to hang
     (ch == '"') || (ch == '<') || (ch == '>') || (ch == 197) || (ch == 206).
     Also, it would make appear that (ch >= 'A') && (ch <= 'Z') doesn't work,
     because it would create files in lower case. But we don't want to forbid
     upper case letters in file names. */
  printf("(ch >= 1) && (ch <= 127) && (ch != 34) && (ch != 42) && (ch != 47) && (ch != 58) && (ch != 60) && (ch != 62) && (ch != 63) && (ch != 92) || (ch == 131) || (ch >= 160) && (ch != 197) && (ch != 206)\n");
#else
  char legal[N];
  char filename[4];
  int i;
  if (chdir("conftestdir") < 0) exit(1);
  for (i = 0; i < N; i++) legal[i] = 0;
  strcpy(filename,"a_z");
  for (i = 0; i < N; i++)
    if (i != '\0')
      { filename[1] = i;
        /* Determine whether the filename is valid: create a file
         * and check that it is present afterwards, under the same name. */
        { int fd = open(filename, O_CREAT | O_RDWR, 0644);
          if (fd >=0)
            { DIR* dirp = opendir(".");
              if (dirp != (DIR*)0)
                { SDIRENT* d;
                  while ((d = readdir(dirp)))
                    { if (!strcmp(d->d_name,".")) continue;
                      if (!strcmp(d->d_name,"..")) continue;
                      if (!strcmp(d->d_name,filename)) legal[i] = 1;
                      /* Remove the file even if its name is something else. */
                      unlink(d->d_name);
                    }
                  closedir(dirp);
                }
              close(fd);
      } }   }
  /* Output a boolean expression equivalent to legal[ch] (0 <= ch < N). */
  { int need_or = 0;
    int z;
    for (z = 0; z < N; )
      { int x, y;
        if (! legal[z]) { z++; continue; }
        x = z;
        if (need_or) printf(" || ");
        z++;
        if ((z < N) && legal[z])
          { do { do { z++; } while ((z < N) && legal[z]);
                 y = z-1;
                 z++;
               } while ((z < N) && legal[z]);
            { int premises = 0;
              if (x > 0) premises++;
              if (y < N-1) premises++;
              for (i = x; i <= y; i++)
                if (! legal[i])
                  premises++;
              if (premises > 1) printf("(");
              { int need_and = 0;
                if (x > 0) { printf("(ch >= %d)",x); need_and = 1; }
                if (y < N-1)
                  { if (need_and) printf(" && ");
                    printf("(ch <= %d)",y);
                    need_and = 1;
                  }
                for (i = x; i <= y; i++)
                  if (! legal[i])
                    { if (need_and) printf(" && ");
                      printf("(ch != %d)",i);
                      need_and = 1;
                    }
                if (!need_and) printf("1");
              }
              if (premises > 1) printf(")");
            }
            z = y+1;
          }
          else
          { printf("(ch == %d)",x); z++; }
        need_or = 1;
      }
    printf("\n");
  }
#endif
  exit(0);
}
changequote([,])dnl
EOF
AC_TRY_EVAL(ac_link)
cl_cv_os_valid_filename_char=`./conftest`
fi
rm -rf conftest*
])
if test -z "$cl_cv_os_valid_filename_char"; then
  cl_cv_os_valid_filename_charset="guessing 7-bit"
else
  if test "$cl_cv_os_valid_filename_char" = '((ch >= 1) && (ch != 47))'; then
    cl_cv_os_valid_filename_charset="8-bit"
  else
    cl_cv_os_valid_filename_charset="7-bit"
  fi
fi
AC_MSG_RESULT($cl_cv_os_valid_filename_charset)
if test -n "$cl_cv_os_valid_filename_char"; then
  AC_DEFINE_UNQUOTED(VALID_FILENAME_CHAR,$cl_cv_os_valid_filename_char,[expression in ch which is true if ch is a valid character in filenames])
fi
])
