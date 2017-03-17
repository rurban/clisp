/* Support for 'make'-like reasoning outside of 'make':
   Determine whether a file is newer than some other file.

   Some shells have a 'test' built-in that supports the syntax
     test $file1 -nt $file2
   (see https://www.gnu.org/software/coreutils/manual/html_node/File-characteristic-tests.html
   and https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html )
   but this is not portable
   (see http://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html ).

   newer $file1 $file2
   Returns with exit code 0 if file1 is newer (according to modification date)
   than file2, or if file1 exists and file2 does not.
   Returns with exit code 1 otherwise. */

#include <config.h> /* gnulib requirement */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "stat-time.h" /* get_stat_mtime */
#include "timespec.h" /* timespec_cmp */

int main (int argc, char* argv[]) {
  const char *file1;
  const char *file2;
  struct stat statbuf1;
  struct stat statbuf2;
  struct timespec mtime1;
  struct timespec mtime2;

  if (argc != 3) {
    fprintf(stderr,"Usage: newer file1 file\n");
    return 1;
  }
  file1 = argv[1];
  file2 = argv[2];

  if (stat(file1, &statbuf1) < 0) {
    if (errno == ENOENT)
      return 1;
    else {
      perror("error accessing file1");
      return 1;
    }
  }
  if (stat(file2, &statbuf2) < 0) {
    if (errno == ENOENT)
      return 0;
    else {
      perror("error accessing file2");
      return 1;
    }
  }

  mtime1 = get_stat_mtime(&statbuf1);
  mtime2 = get_stat_mtime(&statbuf2);
  if (timespec_cmp(mtime1, mtime2) > 0)
    return 0;
  else
    return 1;
}
