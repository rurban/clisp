/* config.h for EMX 0.8h, 0.9a */

#ifndef _RL_CONFIG_H
#define _RL_CONFIG_H

#define HAVE_SELECT
#define HAVE_STRPBRK
#define HAVE_UNISTD_H
#define HAVE_STDLIB_H
#define HAVE_VARARGS_H
#define HAVE_STRING_H
#define HAVE_ALLOCA_H
#define HAVE_DIRENT_H
#define HAVE_TERMCAP_H
#define DIRENT
#define HAVE_TERMIO_H
#define HAVE_SYS_TERMIO_H
#define HAVE_SGTTY_H
#define HAVE_FIONREAD
#define NEED_SYS_IOCTL_H

#include <sys/emx.h>
#ifdef _SIGSET_T /* emx 0.9a */
#define HAVE_SIGACTION
#endif

#include <io.h> /* declares stat(), open(), read(), write(), close(), ioctl(), select() */

#endif
