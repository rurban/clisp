/* Konversionsprogramm Latin1-Zeichensatz -> NeXTstep-Zeichensatz */
/* Bruno Haible 7.7.1994 */

/* Benutzt recode-3.3/nextstep.c.
   Copyright (C) 1993 Free Software Foundation, Inc.
   Francois Pinard <pinard@iro.umontreal.ca>, 1993.
*/

#include <stdio.h>

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int next, latin;
#define NEXT(x) next=x;
#define LATIN(y) latin=y;
#define _ tabelle[latin]=next;
  { int i;
    for (i=0;i<128;i++) { NEXT(i) LATIN(i) _ }
  }
  { int i;
    for (i=0;i<32;i++) { NEXT(-1) LATIN(128+i) _ }
  }
  NEXT(128) LATIN(160) _		/* non-breakable space */
  NEXT(161) LATIN(161) _		/* inverted exclamation mark */
  NEXT(162) LATIN(162) _		/* cent sign */
  NEXT(163) LATIN(163) _		/* pound sterling sign */
  NEXT(168) LATIN(164) _		/* currency sign */
  NEXT(165) LATIN(165) _		/* yen sign  */
  NEXT(181) LATIN(166) _		/* broken bar */
  NEXT(167) LATIN(167) _		/* section (german paragraph) */
  NEXT(200) LATIN(168) _		/* dieresis */
  NEXT(160) LATIN(169) _		/* copyright */
  NEXT(227) LATIN(170) _		/* feminine ordinal indicator */
  NEXT(171) LATIN(171) _		/* gouillemot left */
  NEXT(190) LATIN(172) _		/* logical not */
  NEXT('-') LATIN(173) _		/* hyphen */
  NEXT(176) LATIN(174) _		/* registered trademark */
  NEXT(197) LATIN(175) _		/* macron */
  NEXT(202) LATIN(176) _		/* ring */
  NEXT(209) LATIN(177) _		/* plusminus */
  NEXT(201) LATIN(178) _		/* superscript two */
  NEXT(204) LATIN(179) _		/* superscript 3 */
  NEXT(194) LATIN(180) _		/* accent acute */
  NEXT(157) LATIN(181) _		/* greek letter mu (micro sign) */
  NEXT(182) LATIN(182) _		/* paragraph sign */
  NEXT(180) LATIN(183) _		/* centered period */
  NEXT(203) LATIN(184) _		/* cedilla */
  NEXT(192) LATIN(185) _		/* superscript one */
  NEXT(235) LATIN(186) _		/* masculine ordinal indicator */
  NEXT(187) LATIN(187) _		/* gouillemot right */
  NEXT(210) LATIN(188) _		/* 1/4 */
  NEXT(211) LATIN(189) _		/* 1/2 */
  NEXT(212) LATIN(190) _		/* 3/4 */
  NEXT(191) LATIN(191) _		/* inverted question mark */
  NEXT(129) LATIN(192) _		/* A with accent grave */
  NEXT(130) LATIN(193) _		/* A with accent acute */
  NEXT(131) LATIN(194) _		/* A with circumflex  */
  NEXT(132) LATIN(195) _		/* A with tilde */
  NEXT(133) LATIN(196) _		/* A with dieresis  */
  NEXT(134) LATIN(197) _		/* A with ring  */
  NEXT(225) LATIN(198) _		/* AE */
  NEXT(135) LATIN(199) _		/* C with cedilla */
  NEXT(136) LATIN(200) _		/* E with accent grave */
  NEXT(137) LATIN(201) _		/* E with accent acute */
  NEXT(138) LATIN(202) _		/* E with circumflex */
  NEXT(139) LATIN(203) _		/* E with dieresis */
  NEXT(140) LATIN(204) _		/* I with accent grave */
  NEXT(141) LATIN(205) _		/* I with accent acute */
  NEXT(142) LATIN(206) _		/* I with circumflex */
  NEXT(143) LATIN(207) _		/* I with dieresis */
  NEXT(144) LATIN(208) _		/* capital icelandic Eth */
  NEXT(145) LATIN(209) _		/* N with tilde */
  NEXT(146) LATIN(210) _		/* O with accent grave */
  NEXT(147) LATIN(211) _		/* O with accent acute */
  NEXT(148) LATIN(212) _		/* O with circumflex */
  NEXT(149) LATIN(213) _		/* O with tilde */
  NEXT(150) LATIN(214) _		/* O with dieresis */
  NEXT(158) LATIN(215) _		/* multiply */
  NEXT(233) LATIN(216) _		/* O with slash */
  NEXT(151) LATIN(217) _		/* U with accent grave */
  NEXT(152) LATIN(218) _		/* U with accent acute */
  NEXT(153) LATIN(219) _		/* U with circumflex */
  NEXT(154) LATIN(220) _		/* U with dieresis */
  NEXT(155) LATIN(221) _		/* Y with accent acute */
  NEXT(156) LATIN(222) _		/* capital icelandig thorn */
  NEXT(251) LATIN(223) _		/* german sz */
  NEXT(213) LATIN(224) _		/* a with accent grave */
  NEXT(214) LATIN(225) _		/* a with accent acute */
  NEXT(215) LATIN(226) _		/* a with circumflex */
  NEXT(216) LATIN(227) _		/* a with tilde */
  NEXT(217) LATIN(228) _		/* a with dieresis */
  NEXT(218) LATIN(229) _		/* a with ring */
  NEXT(241) LATIN(230) _		/* ae */
  NEXT(219) LATIN(231) _		/* c with cedilla */
  NEXT(220) LATIN(232) _		/* e with accent grave */
  NEXT(221) LATIN(233) _		/* e with accent acute */
  NEXT(222) LATIN(234) _		/* e with circumflex */
  NEXT(223) LATIN(235) _		/* e with dieresis */
  NEXT(224) LATIN(236) _		/* i with accent grave */
  NEXT(226) LATIN(237) _		/* i with accent acute */
  NEXT(228) LATIN(238) _		/* i with circumflex */
  NEXT(229) LATIN(239) _		/* i with dieresis */
  NEXT(230) LATIN(240) _		/* small icelandic eth */
  NEXT(231) LATIN(241) _		/* n with tilde */
  NEXT(236) LATIN(242) _		/* o with accent grave */
  NEXT(237) LATIN(243) _		/* o with accent acute */
  NEXT(238) LATIN(244) _		/* o with circumflex */
  NEXT(239) LATIN(245) _		/* o with tilde */
  NEXT(240) LATIN(246) _		/* o with dieresis */
  NEXT(159) LATIN(247) _		/* divide */
  NEXT(249) LATIN(248) _		/* o with slash */
  NEXT(242) LATIN(249) _		/* u with accent grave */
  NEXT(243) LATIN(250) _		/* u with accent acute */
  NEXT(244) LATIN(251) _		/* u with circumflex */
  NEXT(246) LATIN(252) _		/* u with dieresis */
  NEXT(247) LATIN(253) _		/* y with accent acute */
  NEXT(252) LATIN(254) _		/* small icelandic thorn */
  NEXT(253) LATIN(255) _		/* y with dieresis */
#undef _
#undef LATIN
#undef NEXT
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
