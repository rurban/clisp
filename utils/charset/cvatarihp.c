/* Konversionsprogramm Atari-Zeichensatz -> HPUX-Zeichensatz */
/* Bruno Haible 5.9.1991 */

#include <stdio.h>

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int atari, hp;
#define ATARI(x) atari=x;
#define HP(y) hp=y;
#define _ tabelle[atari]=hp;
  { int i;
    for (i=0;i<128;i++) { HP(i) ATARI(i) _ }
  }
  HP(180) ATARI(128) _ /* Ç */
  HP(207) ATARI(129) _ /* ü */
  HP(197) ATARI(130) _ /* é */
  HP(192) ATARI(131) _ /* â */
  HP(204) ATARI(132) _ /* ä */
  HP(200) ATARI(133) _ /* à */
  HP(212) ATARI(134) _ /* å */
  HP(181) ATARI(135) _ /* ç */
  HP(193) ATARI(136) _ /* ê */
  HP(205) ATARI(137) _ /* ë */
  HP(201) ATARI(138) _ /* è */
  HP(221) ATARI(139) _ /* ï */
  HP(209) ATARI(140) _ /* î */
  HP(217) ATARI(141) _ /* ì */
  HP(216) ATARI(142) _ /* Ä */
  HP(208) ATARI(143) _ /* Å */
  HP(220) ATARI(144) _ /* É */
  HP(215) ATARI(145) _ /* æ */
  HP(211) ATARI(146) _ /* Æ */
  HP(194) ATARI(147) _ /* ô */
  HP(206) ATARI(148) _ /* ö */
  HP(202) ATARI(149) _ /* ò */
  HP(195) ATARI(150) _ /* û */
  HP(203) ATARI(151) _ /* ù */
  HP(239) ATARI(152) _ /* ÿ */
  HP(218) ATARI(153) _ /* Ö */
  HP(219) ATARI(154) _ /* Ü */
  HP(191) ATARI(155) _ /* ¢ */
  HP(187) ATARI(156) _ /* £ */
  HP(188) ATARI(157) _ /* ¥ */
  HP(222) ATARI(158) _ /* ß */
  HP(190) ATARI(159) _ /*  */
  HP(196) ATARI(160) _ /* á */
  HP(213) ATARI(161) _ /* í */
  HP(198) ATARI(162) _ /* ó */
  HP(199) ATARI(163) _ /* ú */
  HP(183) ATARI(164) _ /* ñ */
  HP(182) ATARI(165) _ /* Ñ */
  HP(249) ATARI(166) _ /* ª */
  HP(250) ATARI(167) _ /* º */
  HP(185) ATARI(168) _ /* ¿ */
  HP(-1) ATARI(169) _ /*  */
  HP(-1) ATARI(170) _ /* ¬ */
  HP(248) ATARI(171) _ /* ½ */
  HP(247) ATARI(172) _ /* ¼ */
  HP(184) ATARI(173) _ /* ¡ */
  HP(251) ATARI(174) _ /* « */
  HP(253) ATARI(175) _ /* » */
  HP(226) ATARI(176) _ /* ã */
  HP(234) ATARI(177) _ /* õ */
  HP(210) ATARI(178) _ /* Ø */
  HP(214) ATARI(179) _ /* ø */
  HP(-1) ATARI(180) _ /* oe */
  HP(-1) ATARI(181) _ /* OE */
  HP(161) ATARI(182) _ /* À */
  HP(225) ATARI(183) _ /* Ã */
  HP(233) ATARI(184) _ /* Õ */
  HP(171) ATARI(185) _ /* ¨ */
  HP(168) ATARI(186) _ /* ´ */
  HP(43) ATARI(187) _ /* + */
  HP(244) ATARI(188) _ /* ¶ */
  HP(-1) ATARI(189) _ /* © */
  HP(-1) ATARI(190) _ /* ® */
  HP(-1) ATARI(191) _ /* TM */
  HP(-1) ATARI(192) _
  HP(-1) ATARI(193) _
  HP(-1) ATARI(194) _
  HP(-1) ATARI(195) _
  HP(-1) ATARI(196) _
  HP(-1) ATARI(197) _
  HP(-1) ATARI(198) _
  HP(-1) ATARI(199) _
  HP(-1) ATARI(200) _
  HP(-1) ATARI(201) _
  HP(-1) ATARI(202) _
  HP(-1) ATARI(203) _
  HP(-1) ATARI(204) _
  HP(-1) ATARI(205) _
  HP(-1) ATARI(206) _
  HP(-1) ATARI(207) _
  HP(-1) ATARI(208) _
  HP(-1) ATARI(209) _
  HP(-1) ATARI(210) _
  HP(-1) ATARI(211) _
  HP(-1) ATARI(212) _
  HP(-1) ATARI(213) _
  HP(-1) ATARI(214) _
  HP(-1) ATARI(215) _
  HP(-1) ATARI(216) _
  HP(-1) ATARI(217) _
  HP(-1) ATARI(218) _
  HP(-1) ATARI(219) _
  HP(-1) ATARI(220) _
  HP(189) ATARI(221) _ /* § */
  HP(-1) ATARI(222) _
  HP(-1) ATARI(223) _
  HP(-1) ATARI(224) _
  HP(-1) ATARI(225) _
  HP(-1) ATARI(226) _
  HP(-1) ATARI(227) _
  HP(-1) ATARI(228) _
  HP(-1) ATARI(229) _
  HP(243) ATARI(230) _ /* µ */
  HP(-1) ATARI(231) _
  HP(-1) ATARI(232) _
  HP(-1) ATARI(233) _
  HP(-1) ATARI(234) _
  HP(-1) ATARI(235) _
  HP(-1) ATARI(236) _
  HP(-1) ATARI(237) _
  HP(-1) ATARI(238) _
  HP(-1) ATARI(239) _
  HP(-1) ATARI(240) _
  HP(254) ATARI(241) _ /* ± */
  HP(-1) ATARI(242) _
  HP(-1) ATARI(243) _
  HP(-1) ATARI(244) _
  HP(-1) ATARI(245) _
  HP(-1) ATARI(246) _
  HP(-1) ATARI(247) _
  HP(179) ATARI(248) _ /* ° */
  HP(-1) ATARI(249) _
  HP(-1) ATARI(250) _
  HP(-1) ATARI(251) _
  HP(-1) ATARI(252) _
  HP(-1) ATARI(253) _
  HP(-1) ATARI(254) _
  HP(176) ATARI(255) _ /* ¯ */
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
