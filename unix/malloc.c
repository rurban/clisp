/* compile:  cc -o malloc malloc.c
   try:      malloc 50000
             malloc 100000
             malloc 1000000
*/
#include <stdio.h>

void printf_address (addr)
  unsigned long addr;
{ if (sizeof(unsigned long) <= 4)
    printf ("#x%8X", (unsigned int)addr);
  else
    printf ("#x%8X%08X",(unsigned int)(addr>>32),(unsigned int)(addr&0xFFFFFFFF));
}

int main (argc,argv)
  int argc;
  char** argv;
{ int i;
  for (i = 1; i < argc; i++)
    { int arg = atoi(argv[i]);
      unsigned long result = (unsigned long)malloc(arg);
      printf ("malloc(%d) = ",arg); printf_address (result); printf ("\n");
    }
  printf ("&main = "); printf_address ((unsigned long)&main); printf ("\n");
  exit(0);
}
