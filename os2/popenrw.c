/* popenrw.c (emx+gcc) -- Copyright (c) 1993 by Bruno Haible */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>
#include <unistd.h>
#include <io.h>
#include <fcntl.h>
#include <errno.h>

int popenrw (const char *command, FILE **rpipe, FILE **wpipe)
{
  int iph[2], oph[2];
  FILE *istream, *ostream;
  const char *sh, *base, *opt;
  int stdin_private, stdout_private, stdin_handle, stdout_handle, saved_errno;
  int i;

  /*        write        system        read        */
  /* parent  <-   iph[0]   <-   iph[1]  <-   child */
  /* parent  ->   oph[1]   ->   oph[0]  ->   child */
  /*        read         system        write       */

  if (pipe (iph) == -1 || pipe (oph) == -1)
    return -1;
  if (fcntl (iph[0], F_SETFD, 1) == -1 || fcntl (iph[1], F_SETFD, 1) == -1
      || fcntl (oph[0], F_SETFD, 1) == -1 || fcntl (oph[1], F_SETFD, 1) == -1)
    goto fail1;
  stdin_private = fcntl (STDIN_FILENO, F_GETFD, 0);
  stdout_private = fcntl (STDOUT_FILENO, F_GETFD, 0);
  if (stdin_private == -1 || stdout_private == -1)
    goto fail1;
  stdin_handle = dup (STDIN_FILENO);
  if (stdin_handle == -1)
    goto fail1;
  stdout_handle = dup (STDOUT_FILENO);
  if (stdout_handle == -1)
    goto fail1;
  fcntl (stdin_handle, F_SETFD, 1);
  fcntl (stdout_handle, F_SETFD, 1);
  if (close (STDIN_FILENO) == -1 || close (STDOUT_FILENO) == -1)
    goto fail2;
  i = dup (oph[0]);
  if (i != STDIN_FILENO)
    { errno = EBADF; goto fail2; }
  i = dup (iph[1]);
  if (i != STDOUT_FILENO)
    { errno = EBADF; goto fail2; }
  if (close (oph[0]) == -1)
    goto fail2;
  if (close (iph[1]) == -1)
    goto fail2;
  istream = fdopen (iph[0], "r");
  if (istream == NULL)
    goto fail2;
  ostream = fdopen (oph[1], "w");
  if (ostream == NULL)
    { fclose (istream); goto fail2; }
  sh = getenv ("EMXSHELL");
  if (sh == NULL)
    sh = getenv ("COMSPEC");
  if (sh == NULL)
    { fclose(istream); fclose(ostream); errno = ENOENT; goto fail2; }
  base = _getname (sh);
  if (stricmp (base, "cmd.exe") == 0 || stricmp (base, "4os2.exe") == 0)
    opt = "/c";
  else
    opt = "-c";
  i = spawnlp (P_NOWAIT, sh, sh, opt, command, NULL);
  if (i == -1)
    { fclose(istream); fclose(ostream); goto fail2; }
  istream->_pid = ostream->_pid = i;
  close (STDIN_FILENO);
  close (STDOUT_FILENO);
  dup (stdin_handle);
  dup (stdout_handle);
  close (stdin_handle);
  close (stdout_handle);
  fcntl (STDIN_FILENO, F_SETFD, stdin_private);
  fcntl (STDOUT_FILENO, F_SETFD, stdout_private);
  *rpipe = istream;
  *wpipe = ostream;
  return 0;
fail2:
  saved_errno = errno;
  close (STDIN_FILENO);
  close (STDOUT_FILENO);
  dup (stdin_handle);
  dup (stdout_handle);
  close (stdin_handle);
  close (stdout_handle);
  fcntl (STDIN_FILENO, F_SETFD, stdin_private);
  fcntl (STDOUT_FILENO, F_SETFD, stdout_private);
  errno = saved_errno;
fail1:
  saved_errno = errno;
  close (iph[0]);
  close (iph[1]);
  close (oph[0]);
  close (oph[1]);
  errno = saved_errno;
  return -1;
}
