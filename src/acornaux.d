# Auxiliary functions for CLISP on Acorn RISC OS
# Peter Burwood 20.12.1994, 11.7.1997

#include "lispbibl.c"

#ifdef RISCOS


#ifndef WIDE_SOFT

# CLISP wants malloc() to return addresses < 0x01000000. Turn off dynamic area
# usage for heap/stack in UnixLib since Clisp needs the heap/stack to live in
# the bottom 16MB of memory and the dynamic areas could be anywhere in the
# logical address range between 0x04000000-0x80000000 or 0xA0000000-0xFFFFFFFF
# and we can't ask for a specific base logical address (reserved to Acorn).
int __dynamic_no_da;

#endif


# New versions of opendir(), readdir() and closedir() to replace those in
# UnixLib. This is so that a wildcarded search name can be passed to readdir()
# to allow the underlying RISC OS filing system to do wildcard and case
# sensitivity processing of filenames.
# Otherwise we would have the strange situation that in a directory
# containing a file "abc", (OPEN "abc") and (OPEN "Abc") are equivalent,
# however (DIRECTORY "A*") returns NIL - because RISC OS readdir() returns
# "abc" and CLISP thinks it doesn't match - whereas (DIRECTORY "a*")
# returns (#"abc").

# The implementation of opendir() and closedir() is exactly the same as the
# UnixLib 3.6e, but to avoid future problems where opendir() could load some
# filenames from the directory, they are reproduced here.
# telldir() and seekdir() are not supported.

# While copying this small piece of code, I have fixed four (4) bugs in
# this code. I dare not bet whether there are hundreds or thousands similar
# bugs in the whole UnixLib. -- Bruno

#include <string.h>
#include <stdlib.h>

#include <sys/unix.h>
#include <sys/syslib.h>
#include <sys/os.h>
#include <sys/types.h>
#include <dirent.h>

# Size of the buffers we employ. Should be >= MAXNAMLEN.
#define DIRBUFSIZ 1024

typedef struct { DIR dir;
                 char * wildcard; # search wildcards
                 struct dirent entry; # used to return entries
                                      # (could use a static buffer instead)
               }
        big_dir;

DIR * opendir (const char * name, const char * wildcard)
{
  { int r[10];
    os_error * err;

    # ensure directory exists
    err = os_file(0x05,(char*)name,r);
    if (err) { __seterr(err); return NULL; }
    # make sure it is a directory or image directory
    if (!(r[0]==2 || r[0]==3)) { return NULL; }
  }
  { big_dir * bd;
    DIR * d;

    bd = (big_dir *) malloc(sizeof(big_dir));
    if (!bd) { return NULL; }
    d = &bd->dir;
    if (!(d->dd_buf = malloc(DIRBUFSIZ)))
      { free(bd); return NULL; }
    if (!(d->dd_name = strdup(name)))
      { free(d->dd_buf); free(bd); return NULL; }
    if (!(bd->wildcard = strdup(wildcard)))
      { free(d->dd_name); free(d->dd_buf); free(bd); return NULL; }
    d->dd_fd = 0;
    d->dd_loc = 0;
    d->dd_size = 0;
    d->dd_bsize = DIRBUFSIZ;
    d->dd_off = 0;
    d->dd_off2 = 0;
    return d;
} }

struct dirent * readdir (DIR * d)
{
  big_dir * bd;

  if (!d) { return NULL; }

  bd = (big_dir *) ((char *) d - offsetof(big_dir,dir));
  # Now d = &bd->dir.

  while (d->dd_loc >= d->dd_size)
    {
      int r[10];
      os_error * err;
      char * s;
      int i;

      if (d->dd_off2 < 0) { return NULL; }

      r[0] = 9;
      r[1] = (long) d->dd_name;
      r[2] = (long) d->dd_buf;
      r[3] = DIRBUFSIZ / MAXNAMLEN;
      r[4] = d->dd_off2;
      r[5] = DIRBUFSIZ;
      r[6] = (long) bd->wildcard;
      err = os_swi(0x0c,r);
      if (err) { __seterr(err); return NULL; }

      # find the end of r[3] strings in the buffer:
      s = (char *) d->dd_buf;
      for (i = r[3]; i > 0; i--) # loop r[3] times
        { while (*s) { s++; } # skip a string
          s++;                # and its terminating '\0'
        }
      d->dd_loc = 0;
      d->dd_size = s - d->dd_buf;
      d->dd_off = d->dd_off2 * DIRBUFSIZ;
      d->dd_off2 = r[4];
      if (r[4] < 0 && r[3] == 0) { return NULL; } # end of directory reached
    }
  # Now 0 <= d->dd_loc < d->dd_size, return the name beginning at d->dd_loc.
  {
    char * s = d->dd_buf + d->dd_loc;
    int i = strlen(s) + 1;
    struct dirent * result = &bd->entry;

    result->d_off = d->dd_off;
    d->dd_off += i; d->dd_loc += i;
    result->d_fileno = 0;
    if (i < MAXNAMLEN)
      { memcpy(result->d_name,s,i);
        result->d_namlen = i-1;
      }
    else
      { memcpy(result->d_name,s,MAXNAMLEN-1);
        result->d_name[MAXNAMLEN-1] = '\0';
        result->d_namlen = MAXNAMLEN-1;
      }
    result->d_reclen = DIRSIZ(result);

    return result;
} }

int closedir (DIR * d)
{
  big_dir * bd;

  if (!d) { return -1; }

  bd = (big_dir *) ((char *) d - offsetof(big_dir,dir));
  # Now d = &bd->dir.

  free(bd->wildcard);
  free(d->dd_name);
  free(d->dd_buf);
  free(bd);

  return 0;
}


#endif # RISCOS
