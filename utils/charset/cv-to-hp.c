/* Konversionsprogramm SUN4-Zeichensatz -> HPUX-Zeichensatz */
/* Bruno Haible 4.9.1991 */

#include <stdio.h>

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int hp, sun4;
#define HP(x) hp=x;
#define SUN4(y) sun4=y;
#define _ tabelle[sun4]=hp;
  { int i;
    for (i=0;i<128;i++) { SUN4(i) HP(i) _ }
  }
  { int i;
    for (i=0;i<32;i++) { SUN4(128+i) HP(128+i) _ }
  }
  SUN4(160) HP(160) _ /*   */
  SUN4(161) HP(184) _ /* ¡ */
  SUN4(162) HP(191) _ /* ¢ */
  SUN4(163) HP(187) _ /* £ */
  SUN4(164) HP(186) _ /* ¤ */
  SUN4(165) HP(188) _ /* ¥ */
  SUN4(166) HP(-1) _ /* ¦ */
  SUN4(167) HP(189) _ /* § */
  SUN4(168) HP(171) _ /* ¨ */
  SUN4(169) HP(-1) _ /* © */
  SUN4(170) HP(249) _ /* ª */
  SUN4(171) HP(251) _ /* « */
  SUN4(172) HP(-1) _ /* ¬ */
  SUN4(173) HP(246) _ /* ­ */
  SUN4(174) HP(-1) _ /* ® */
  SUN4(175) HP(176) _ /* ¯ */
  SUN4(176) HP(179) _ /* ° */
  SUN4(177) HP(254) _ /* ± */
  SUN4(178) HP(-1) _ /* ² */
  SUN4(179) HP(-1) _ /* ³ */
  SUN4(180) HP(168) _ /* ´ */
  SUN4(181) HP(243) _ /* µ */
  SUN4(182) HP(244) _ /* ¶ */
  SUN4(183) HP(242) _ /* · */
  SUN4(184) HP(-1) _ /* ¸ */
  SUN4(185) HP(-1) _ /* ¹ */
  SUN4(186) HP(250) _ /* º */
  SUN4(187) HP(253) _ /* » */
  SUN4(188) HP(247) _ /* ¼ */
  SUN4(189) HP(248) _ /* ½ */
  SUN4(190) HP(245) _ /* ¾ */
  SUN4(191) HP(185) _ /* ¿ */
  SUN4(192) HP(161) _ /* À */
  SUN4(193) HP(224) _ /* Á */
  SUN4(194) HP(162) _ /* Â */
  SUN4(195) HP(225) _ /* Ã */
  SUN4(196) HP(216) _ /* Ä */
  SUN4(197) HP(208) _ /* Å */
  SUN4(198) HP(211) _ /* Æ */
  SUN4(199) HP(180) _ /* Ç */
  SUN4(200) HP(163) _ /* È */
  SUN4(201) HP(220) _ /* É */
  SUN4(202) HP(164) _ /* Ê */
  SUN4(203) HP(165) _ /* Ë */
  SUN4(204) HP(230) _ /* Ì */
  SUN4(205) HP(229) _ /* Í */
  SUN4(206) HP(166) _ /* Î */
  SUN4(207) HP(167) _ /* Ï */
  SUN4(208) HP(227) _ /* Ð */
  SUN4(209) HP(182) _ /* Ñ */
  SUN4(210) HP(232) _ /* Ò */
  SUN4(211) HP(231) _ /* Ó */
  SUN4(212) HP(223) _ /* Ô */
  SUN4(213) HP(233) _ /* Õ */
  SUN4(214) HP(218) _ /* Ö */
  SUN4(215) HP(-1) _ /* × */
  SUN4(216) HP(210) _ /* Ø */
  SUN4(217) HP(173) _ /* Ù */
  SUN4(218) HP(237) _ /* Ú */
  SUN4(219) HP(174) _ /* Û */
  SUN4(220) HP(219) _ /* Ü */
  SUN4(221) HP(177) _ /* Ý */
  SUN4(222) HP(240) _ /* Þ */
  SUN4(223) HP(222) _ /* ß */
  SUN4(224) HP(200) _ /* à */
  SUN4(225) HP(196) _ /* á */
  SUN4(226) HP(192) _ /* â */
  SUN4(227) HP(226) _ /* ã */
  SUN4(228) HP(204) _ /* ä */
  SUN4(229) HP(212) _ /* å */
  SUN4(230) HP(215) _ /* æ */
  SUN4(231) HP(181) _ /* ç */
  SUN4(232) HP(201) _ /* è */
  SUN4(233) HP(197) _ /* é */
  SUN4(234) HP(193) _ /* ê */
  SUN4(235) HP(205) _ /* ë */
  SUN4(236) HP(217) _ /* ì */
  SUN4(237) HP(213) _ /* í */
  SUN4(238) HP(209) _ /* î */
  SUN4(239) HP(221) _ /* ï */
  SUN4(240) HP(228) _ /* ð */
  SUN4(241) HP(183) _ /* ñ */
  SUN4(242) HP(202) _ /* ò */
  SUN4(243) HP(198) _ /* ó */
  SUN4(244) HP(194) _ /* ô */
  SUN4(245) HP(234) _ /* õ */
  SUN4(246) HP(206) _ /* ö */
  SUN4(247) HP(-1) _ /* ÷ */
  SUN4(248) HP(214) _ /* ø */
  SUN4(249) HP(203) _ /* ù */
  SUN4(250) HP(199) _ /* ú */
  SUN4(251) HP(195) _ /* û */
  SUN4(252) HP(207) _ /* ü */
  SUN4(253) HP(178) _ /* ý */
  SUN4(254) HP(241) _ /* þ */
  SUN4(255) HP(239) _ /* ÿ */
#undef _
#undef SUN4
#undef HP
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
