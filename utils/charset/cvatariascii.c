/* Konversionsprogramm Atari-Zeichensatz -> ASCII-Zeichensatz */
/* Bruno Haible 13.12.1991 */

#include <stdio.h>

main ()
{ static long tabelle[256];
  /* Tabelle initialisieren: */
  int atari;
  long ascii;
#define ATARI(x) atari=x;
#define ASCII(y) ascii=y;
#define ASCII2(y1,y2) ascii=(y2<<8)|y1;
#define ASCII3(y1,y2,y3) ascii=(y3<<16)|(y2<<8)|y1;
#define _ tabelle[atari]=ascii;
  { int i;
    for (i=0;i<128;i++) { ATARI(i) ASCII(i) _ }
  }
  ATARI(128) ASCII('C') _ /* Ç */
  ATARI(129) ASCII2('u','e') _ /* ü */
  ATARI(130) ASCII2('\'','e') _ /* é */
  ATARI(131) ASCII2('^','a') _ /* â */
  ATARI(132) ASCII2('a','e') _ /* ä */
  ATARI(133) ASCII2('`','a') _ /* à */
  ATARI(134) ASCII('a') _ /* å */
  ATARI(135) ASCII('c') _ /* ç */
  ATARI(136) ASCII2('^','e') _ /* ê */
  ATARI(137) ASCII2('\"','e') _ /* ë */
  ATARI(138) ASCII2('`','e') _ /* è */
  ATARI(139) ASCII2('\"','i') _ /* ï */
  ATARI(140) ASCII2('^','i') _ /* î */
  ATARI(141) ASCII2('`','i') _ /* ì */
  ATARI(142) ASCII2('A','e') _ /* Ä */
  ATARI(143) ASCII('A') _ /* Å */
  ATARI(144) ASCII2('\'','E') _ /* É */
  ATARI(145) ASCII2('a','e') _ /* æ */
  ATARI(146) ASCII2('A','E') _ /* Æ */
  ATARI(147) ASCII2('^','o') _ /* ô */
  ATARI(148) ASCII2('o','e') _ /* ö */
  ATARI(149) ASCII2('`','o') _ /* ò */
  ATARI(150) ASCII2('^','u') _ /* û */
  ATARI(151) ASCII2('`','u') _ /* ù */
  ATARI(152) ASCII2('\"','y') _ /* ÿ */
  ATARI(153) ASCII2('O','e') _ /* Ö */
  ATARI(154) ASCII2('U','e') _ /* Ü */
  ATARI(155) ASCII('c') _ /* ¢ */
  ATARI(156) ASCII2('l','b') _ /* £ */
  ATARI(157) ASCII3('y','e','n') _ /* ¥ */
  ATARI(158) ASCII2('s','s') _ /* ß */
  ATARI(159) ASCII2('f','l') _
  ATARI(160) ASCII2('\'','a') _ /* á */
  ATARI(161) ASCII2('\'','i') _ /* í */
  ATARI(162) ASCII2('\'','o') _ /* ó */
  ATARI(163) ASCII2('\'','u') _ /* ú */
  ATARI(164) ASCII2('~','n') _ /* ñ */
  ATARI(165) ASCII2('~','N') _ /* Ñ */
  ATARI(166) ASCII('a') _ /* ª */
  ATARI(167) ASCII('o') _ /* º */
  ATARI(168) ASCII('?') _ /* ¿ */
  ATARI(169) ASCII(0) _
  ATARI(170) ASCII3('n','o','t') _ /* ¬ */
  ATARI(171) ASCII3('1','/','2') _ /* ½ */
  ATARI(172) ASCII3('1','/','4') _ /* ¼ */
  ATARI(173) ASCII('!') _ /* ¡ */
  ATARI(174) ASCII2('<','<') _ /* « */
  ATARI(175) ASCII2('>','>') _ /* » */
  ATARI(176) ASCII2('~','a') _ /* ã */
  ATARI(177) ASCII2('~','o') _ /* õ */
  ATARI(178) ASCII('O') _ /* Ø */
  ATARI(179) ASCII('o') _ /* ø */
  ATARI(180) ASCII2('o','e') _ /* oe */
  ATARI(181) ASCII2('O','e') _ /* OE */
  ATARI(182) ASCII2('`','A') _ /* À */
  ATARI(183) ASCII2('~','A') _ /* Ã */
  ATARI(184) ASCII2('~','O') _ /* Õ */
  ATARI(185) ASCII('\"') _ /* ¨ */
  ATARI(186) ASCII('\'') _ /* ´ */
  ATARI(187) ASCII('+') _ /* + */
  ATARI(188) ASCII('P') _ /* ¶ */
  ATARI(189) ASCII3('(','c',')') _ /* © */
  ATARI(190) ASCII3('(','R',')') _ /* ® */
  ATARI(191) ASCII2('T','M') _ /* TM */
  ATARI(192) ASCII2('i','j') _
  ATARI(193) ASCII2('I','J') _
  ATARI(194) ASCII(0) _
  ATARI(195) ASCII(0) _
  ATARI(196) ASCII(0) _
  ATARI(197) ASCII(0) _
  ATARI(198) ASCII(0) _
  ATARI(199) ASCII(0) _
  ATARI(200) ASCII(0) _
  ATARI(201) ASCII(0) _
  ATARI(202) ASCII(0) _
  ATARI(203) ASCII(0) _
  ATARI(204) ASCII(0) _
  ATARI(205) ASCII(0) _
  ATARI(206) ASCII(0) _
  ATARI(207) ASCII(0) _
  ATARI(208) ASCII(0) _
  ATARI(209) ASCII(0) _
  ATARI(210) ASCII(0) _
  ATARI(211) ASCII(0) _
  ATARI(212) ASCII(0) _
  ATARI(213) ASCII(0) _
  ATARI(214) ASCII(0) _
  ATARI(215) ASCII(0) _
  ATARI(216) ASCII(0) _
  ATARI(217) ASCII(0) _
  ATARI(218) ASCII(0) _
  ATARI(219) ASCII(0) _
  ATARI(220) ASCII(0) _
  ATARI(221) ASCII2('S','S') _ /* § */
  ATARI(222) ASCII('^') _
  ATARI(223) ASCII2('o','o') _
  ATARI(224) ASCII(0) _
  ATARI(225) ASCII(0) _
  ATARI(226) ASCII(0) _
  ATARI(227) ASCII(0) _
  ATARI(228) ASCII(0) _
  ATARI(229) ASCII(0) _
  ATARI(230) ASCII('u') _ /* µ */
  ATARI(231) ASCII(0) _
  ATARI(232) ASCII(0) _
  ATARI(233) ASCII(0) _
  ATARI(234) ASCII(0) _
  ATARI(235) ASCII(0) _
  ATARI(236) ASCII(0) _
  ATARI(237) ASCII(0) _
  ATARI(238) ASCII2('i','n') _
  ATARI(239) ASCII('n') _
  ATARI(240) ASCII('=') _
  ATARI(241) ASCII2('+','-') _ /* ± */
  ATARI(242) ASCII2('>','=') _
  ATARI(243) ASCII2('<','=') _
  ATARI(244) ASCII(0) _
  ATARI(245) ASCII(0) _
  ATARI(246) ASCII(':') _ /* ÷ */
  ATARI(247) ASCII('=') _
  ATARI(248) ASCII2('^','0') _ /* ° */
  ATARI(249) ASCII(0) _
  ATARI(250) ASCII(0) _
  ATARI(251) ASCII(0) _
  ATARI(252) ASCII(0) _
  ATARI(253) ASCII2('^','2') _ /* ² */
  ATARI(254) ASCII2('^','3') _ /* ³ */
  ATARI(255) ASCII(0) _ /* ¯ */
#undef _
#undef ASCII3
#undef ASCII2
#undef ASCII
#undef ATARI
  { int fehler = 0;
    int c;
    while (!((c = getchar()) == EOF))
      { long cx = tabelle[c];
        if (cx == 0)
          { fehler++; }
          else
          { do { putchar(cx & 0xFF); cx = cx>>8; } while (!(cx == 0)); }
      }
    if (!(fehler == 0))
      { fprintf(stderr,"%d illegal characters\n",fehler); exit(1); }
      else
      if (ferror(stdin) || ferror(stdout))
        { exit(1); }
        else
        { exit(0); }
} }
