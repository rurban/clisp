#include <string.h>

extern const char* locale_charset ();

/*
 * return:
 *  0: unknown mode
 *  1: UTF-8 mode
 * rch: these escapes above are unportable & don't work well, for example MC hangs in xterm :(
 */
int is_in_UTF8_mode ()
{
  const char* charset = locale_charset ();
  return (charset != NULL && strcmp (charset, "UTF-8") == 0);
}
