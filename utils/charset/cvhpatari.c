/* Konversionsprogramm HPUX-Zeichensatz -> Atari-Zeichensatz */
/* Bruno Haible 5.9.1991 */

#include <stdio.h>

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int atari, hp;
#define ATARI(x) atari=x;
#define HP(y) hp=y;
#define _ tabelle[hp]=atari;
  { int i;
    for (i=0;i<128;i++) { HP(i) ATARI(i) _ }
  }
  HP(160) ATARI(-1) _
  HP(161) ATARI(182) _ /* À */
  HP(162) ATARI(-1) _
  HP(163) ATARI(-1) _
  HP(164) ATARI(-1) _
  HP(165) ATARI(-1) _
  HP(166) ATARI(-1) _
  HP(167) ATARI(-1) _
  HP(168) ATARI(186) _ /* ´ */
  HP(169) ATARI(-1) _
  HP(170) ATARI(-1) _
  HP(171) ATARI(185) _ /* ¨ */
  HP(172) ATARI(-1) _
  HP(173) ATARI(-1) _
  HP(174) ATARI(-1) _
  HP(175) ATARI(156) _ /* £ */
  HP(176) ATARI(255) _ /* ¯ */
  HP(177) ATARI(-1) _
  HP(178) ATARI(-1) _
  HP(179) ATARI(248) _ /* ° */
  HP(180) ATARI(128) _ /* Ç */
  HP(181) ATARI(135) _ /* ç */
  HP(182) ATARI(165) _ /* Ñ */
  HP(183) ATARI(164) _ /* ñ */
  HP(184) ATARI(173) _ /* ¡ */
  HP(185) ATARI(168) _ /* ¿ */
  HP(186) ATARI(-1) _
  HP(187) ATARI(156) _ /* £ */
  HP(188) ATARI(157) _ /* ¥ */
  HP(189) ATARI(221) _ /* § */
  HP(190) ATARI(159) _ /*  */
  HP(191) ATARI(155) _ /* ¢ */
  HP(192) ATARI(131) _ /* â */
  HP(193) ATARI(136) _ /* ê */
  HP(194) ATARI(147) _ /* ô */
  HP(195) ATARI(150) _ /* û */
  HP(196) ATARI(160) _ /* á */
  HP(197) ATARI(130) _ /* é */
  HP(198) ATARI(162) _ /* ó */
  HP(199) ATARI(163) _ /* ú */
  HP(200) ATARI(133) _ /* à */
  HP(201) ATARI(138) _ /* è */
  HP(202) ATARI(149) _ /* ò */
  HP(203) ATARI(151) _ /* ù */
  HP(204) ATARI(132) _ /* ä */
  HP(205) ATARI(137) _ /* ë */
  HP(206) ATARI(148) _ /* ö */
  HP(207) ATARI(129) _ /* ü */
  HP(208) ATARI(143) _ /* Å */
  HP(209) ATARI(140) _ /* î */
  HP(210) ATARI(178) _ /* Ø */
  HP(211) ATARI(146) _ /* Æ */
  HP(212) ATARI(134) _ /* å */
  HP(213) ATARI(161) _ /* í */
  HP(214) ATARI(179) _ /* ø */
  HP(215) ATARI(145) _ /* æ */
  HP(216) ATARI(142) _ /* Ä */
  HP(217) ATARI(141) _ /* ì */
  HP(218) ATARI(153) _ /* Ö */
  HP(219) ATARI(154) _ /* Ü */
  HP(220) ATARI(144) _ /* É */
  HP(221) ATARI(139) _ /* ï */
  HP(222) ATARI(158) _ /* ß */
  HP(223) ATARI(-1) _
  HP(224) ATARI(-1) _
  HP(225) ATARI(183) _ /* Ã */
  HP(226) ATARI(176) _ /* ã */
  HP(227) ATARI(-1) _
  HP(228) ATARI(-1) _
  HP(229) ATARI(-1) _
  HP(230) ATARI(-1) _
  HP(231) ATARI(-1) _
  HP(232) ATARI(-1) _
  HP(233) ATARI(184) _ /* Õ */
  HP(234) ATARI(177) _ /* õ */
  HP(235) ATARI(-1) _
  HP(236) ATARI(-1) _
  HP(237) ATARI(-1) _
  HP(238) ATARI(-1) _
  HP(239) ATARI(152) _ /* ÿ */
  HP(240) ATARI(-1) _
  HP(241) ATARI(-1) _
  HP(242) ATARI(-1) _
  HP(243) ATARI(230) _ /* µ */
  HP(244) ATARI(188) _ /* ¶ */
  HP(245) ATARI(-1) _
  HP(246) ATARI(-1) _
  HP(247) ATARI(172) _ /* ¼ */
  HP(248) ATARI(171) _ /* ½ */
  HP(249) ATARI(166) _ /* ª */
  HP(250) ATARI(167) _ /* º */
  HP(251) ATARI(174) _ /* « */
  HP(252) ATARI(-1) _
  HP(253) ATARI(175) _ /* » */
  HP(254) ATARI(241) _ /* ± */
  HP(255) ATARI(-1) _
#undef _
#undef HP
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
