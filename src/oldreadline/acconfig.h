/* acconfig.h
   This file is in the public domain.

   Descriptive text for the C preprocessor macros that
   the distributed Autoconf macros can define.
   No software package will use all of them; autoheader copies the ones
   your configure.in uses into your configuration header file templates.

   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  Although this order
   can split up related entries, it makes it easier to check whether
   a given entry is in the file.

   Leave the following blank line there!!  Autoheader needs it.  */


/* Define if your system defines TIOCGWINSZ in sys/ioctl.h.  */
#undef GWINSZ_IN_SYS_IOCTL

#undef HAVE_GETPW_DECLS

#undef OPEN_NEEDS_SYS_FILE_H

/* CL_SIGNALBLOCK */
/* how to block and unblock signals */
#undef SIGNALBLOCK_SYSV
#undef SIGNALBLOCK_POSIX
#undef SIGNALBLOCK_BSD


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
