/* Konversionsprogramm IBMPC-Zeichensatz -> Atari-Zeichensatz */
/* Bruno Haible 15.1.1992 */

#include <stdio.h>

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int atari, ibmpc;
#define ATARI(x) atari=x;
#define IBMPC(y) ibmpc=y;
#define _ tabelle[ibmpc]=atari;
  { int i;
    for (i=0;i<128;i++) { IBMPC(i) ATARI(i) _ }
  }
  IBMPC(20) ATARI(188) _ /* ¶ */
  IBMPC(21) ATARI(221) _ /* § */
  { int i; /* ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥ áíóúñÑªº¿¬½¼¡«» */
    for (i=128;i<176;i++) if (!(i==158)) { IBMPC(i) ATARI(i) _ }
  }
  IBMPC(158) ATARI(-1) _ /* Pt */
  { int i;
    for (i=176;i<224;i++) { IBMPC(i) ATARI(-1) _ }
  }
  IBMPC(224) ATARI(224) _ /*  */
  IBMPC(225) ATARI(158) _ /* ß */
  IBMPC(226) ATARI(226) _ /*  */
  IBMPC(227) ATARI(227) _ /*  */
  IBMPC(228) ATARI(228) _ /*  */
  IBMPC(229) ATARI(229) _ /*  */
  IBMPC(230) ATARI(230) _ /* µ */
  IBMPC(231) ATARI(231) _ /*  */
  IBMPC(232) ATARI(232) _ /*  */
  IBMPC(233) ATARI(233) _ /*  */
  IBMPC(234) ATARI(234) _ /*  */
  IBMPC(235) ATARI(235) _ /*  */
  IBMPC(236) ATARI(223) _ /*  */
  IBMPC(237) ATARI(237) _ /*  */
  IBMPC(238) ATARI(238) _ /*  */
  IBMPC(239) ATARI(239) _ /*  */
  IBMPC(240) ATARI(240) _ /*  */
  IBMPC(241) ATARI(241) _ /* ± */
  IBMPC(242) ATARI(242) _ /*  */
  IBMPC(243) ATARI(243) _ /*  */
  IBMPC(244) ATARI(244) _ /*  */
  IBMPC(245) ATARI(245) _ /*  */
  IBMPC(246) ATARI(246) _ /* ÷ */
  IBMPC(247) ATARI(247) _ /*  */
  IBMPC(248) ATARI(248) _ /* ° */
  IBMPC(249) ATARI(249) _ /*  */
  IBMPC(250) ATARI(250) _ /*  */
  IBMPC(251) ATARI(251) _ /*  */
  IBMPC(252) ATARI(252) _ /*  */
  IBMPC(253) ATARI(253) _ /* ² */
  IBMPC(254) ATARI(-1) _
  IBMPC(255) ATARI(-1) _
#undef _
#undef IBMPC
#undef ATARI
  { int fehler = 0;
    int c;
    while (!((c = getchar()) == EOF))
      { c = tabelle[c];
        if (c < 0) { fehler++; } else putchar(c);
      }
    if (!(fehler == 0))
      { fprintf(stderr,"%d illegal characters\n",fehler); exit(1); }
      else
      if (ferror(stdin) || ferror(stdout))
        { exit(1); }
        else
        { exit(0); }
} }
