/* GCC Library
 * Jörg Höhle, 21-Nov-92
 * from parseargs (c) Matthew Dillon
 */

/*
 * Returns argc for arguments.  Does NOT include arg0
 */
int
_tokenize(volatile char* copy, int len)
{
  int ac = 0;
  for (;;) {
    while (*copy == ' ' || *copy == '\t') ++copy;
    if (*copy == 0 || *copy == '\n') break;
    ++ac;
    if (*copy == '\"') {
      do {
	++copy;
      } while (*copy && *copy != '\n' && *copy != '\"') ;
    } else {
      while (*copy && *copy != '\n' && *copy != ' ' && *copy != '\t')
	++copy;
    }
    if (*copy == 0) break;
    *copy++ = 0;
  }
  return(ac);
}

/*
 * Dumps 'ac' arguments into av beginning at index 0.
 */
void
_dumpargs(char* copy, char** av, int ac)
{
  while (ac) {
    while (*copy == ' ' || *copy == '\t') ++copy;
    if (*copy == '\"') ++copy;
    *av++ = copy;
    while (*copy) ++copy;
    ++copy;
    --ac;
  }
}

