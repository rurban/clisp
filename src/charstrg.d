# Funktionen für Characters und Strings für CLISP
# Bruno Haible 1990-2000

#include "lispbibl.c"


# Character-Umwandlungstabellen:
#if defined(UNICODE)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 # von Unicodes.
#elif defined(ISOLATIN_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A E0 ... F6 F8 ... FE
 #  Groß  41 ... 5A C0 ... D6 D8 ... DE
 #  Beide aA ... zZ àÀ ... öÖ øØ ... þÞ
#elif defined(HPROMAN8_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A C4 C5 D5 C6 C7 B2 C0 C1 D1 C2 C3 C8 C9 D9 CA CB
 #  Groß  41 ... 5A E0 DC E5 E7 ED B1 A2 A4 A6 DF AE A1 A3 E6 E8 AD
 #  Was   aA ... zZ áÁ éÉ íÍ óÓ úÚ ýÝ âÂ êÊ îÎ ôÔ ûÛ àÀ èÈ ìÌ òÒ ùÙ
 #  Klein CC CD DD CE CF EF E2 B7 EA D4 D7 D6 B5 EC E4 F1
 #  Groß  D8 A5 A7 DA DB EE E1 B6 E9 D0 D3 D2 B4 EB E3 F0
 #  Was   äÄ ëË ïÏ öÖ üÜ ÿŸ ãÃ ñÑ õÕ åÅ æÆ øØ çÇ šŠ ðÐ þÞ
#elif defined(NEXTSTEP_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A D5 ... E0 E2 E4 ... E7 EC ... F0 F1 F2 .. F4 F6 F7 F9 FA FC
 #  Groß  41 ... 5A 81 ... 8C 8D 8E ... 91 92 ... 96 E1 97 .. 99 9A 9B E9 EA 9C
 #  Was   aA ... zZ àÀ ... ìÌ íÍ îÎ ... ñÑ òÒ ... öÖ æÆ ùÙ .. ûÛ üÜ ýÝ øØ œŒ þÞ
#elif defined(IBMPC_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A 87 81 82 84 86 91 94 A4
 #  Groß  41 ... 5A 80 9A 90 8E 8F 92 99 A5
 #  Beide aA ... zZ çÇ üÜ éÉ äÄ åÅ æÆ öÖ ñÑ
#else # defined(ASCII_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A
 #  Groß  41 ... 5A
 #  Beide aA ... zZ
#endif

#ifdef UNICODE
# No-conversion table, used by up_case_table and down_case_table.
static const cint nop_page[256] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};
#endif

# Wandelt Byte ch in einen Großbuchstaben
# up_case(ch)
  global chart up_case (chart ch);
  global chart up_case(ch)
    var chart ch;
    {
      #ifdef UNICODE
      #include "uni_upcase.c"
      var cint c = as_cint(ch);
      return as_chart((c+up_case_table[c>>8][c&0xFF])&(bit(char_int_len)-1));
      #else
      # Tabelle für Umwandlung in Großbuchstaben:
      local const cint up_case_table[char_code_limit] =
        #if defined(ISOLATIN_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xF7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xFF,
          };
        #elif defined(HPROMAN8_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB4,0xB6,0xB6,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xA2,0xA4,0xDF,0xAE,0xE0,0xDC,0xE7,0xB2,0xA1,0xA3,0xE8,0xAD,0xD8,0xA5,0xDA,0xDB,
            0xD0,0xA6,0xD2,0xD3,0xD0,0xE5,0xD2,0xD3,0xD8,0xE6,0xDA,0xDB,0xDC,0xA7,0xDE,0xDF,
            0xE0,0xE1,0xE1,0xE3,0xE3,0xE5,0xE6,0xE7,0xE8,0xE9,0xE9,0xEB,0xEB,0xED,0xEE,0xEE,
            0xF0,0xF0,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(NEXTSTEP_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,
            0x8C,0xE1,0x8D,0xE3,0x8E,0x8F,0x90,0x91,0xE8,0xE9,0xEA,0xEB,0x92,0x93,0x94,0x95,
            0x96,0xE1,0x97,0x98,0x99,0xF5,0x9A,0x9B,0xF8,0xE9,0xEA,0xFB,0x9C,0xFD,0xFE,0xFF,
          };
        #elif defined(IBMPC_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x9A,0x90,0x83,0x8E,0x85,0x8F,0x80,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x92,0x92,0x93,0x99,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA5,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #else # Standard-Ascii-Umwandlungstabelle: Nur a..z --> A..Z
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #endif
      return as_chart(up_case_table[as_cint(ch)]);
      #endif
    }

# Wandelt Byte ch in einen Kleinbuchstaben
# down_case(ch)
  global chart down_case (chart ch);
  global chart down_case(ch)
    var chart ch;
    {
      #ifdef UNICODE
      #include "uni_downcase.c"
      var cint c = as_cint(ch);
      return as_chart((c+down_case_table[c>>8][c&0xFF])&(bit(char_int_len)-1));
      #else
      # Tabelle für Umwandlung in Kleinbuchstaben:
      local const cint down_case_table[char_code_limit] =
        #if defined(ISOLATIN_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xD7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(HPROMAN8_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xC8,0xC0,0xC9,0xC1,0xCD,0xD1,0xDD,0xA8,0xA9,0xAA,0xAB,0xAC,0xCB,0xC3,0xAF,
            0xB0,0xB2,0xB2,0xB3,0xB5,0xB5,0xB7,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD4,0xD1,0xD6,0xD7,0xD4,0xD5,0xD6,0xD7,0xCC,0xD9,0xCE,0xCF,0xC5,0xDD,0xDE,0xC2,
            0xC4,0xE2,0xE2,0xE4,0xE4,0xD5,0xD9,0xC6,0xCA,0xEA,0xEA,0xEC,0xEC,0xC7,0xEF,0xEF,
            0xF1,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(NEXTSTEP_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,0xE0,0xE2,0xE4,0xE5,
            0xE6,0xE7,0xEC,0xED,0xEE,0xEF,0xF0,0xF2,0xF3,0xF4,0xF6,0xF7,0xFC,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xF1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xF9,0xFA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(IBMPC_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x87,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x84,0x86,
            0x82,0x91,0x91,0x93,0x94,0x95,0x96,0x97,0x98,0x94,0x81,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA4,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #else # Standard-Ascii-Umwandlungstabelle: Nur A..Z --> a..z
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #endif
      return as_chart(down_case_table[as_cint(ch)]);
      #endif
    }

#ifdef UNICODE
# Table of Unicode character attributes.
# unicode_attribute(c)
# > cint c: a character code
# < result: 0 = non-graphic
#           1 = graphic, but not alphanumeric
#           2 = graphic and numeric
#           3 = graphic and alphabetic
  #include "uni_attribute.c"
  #define unicode_attribute(c)  \
    ((unicode_attribute_table[(c)>>10][((c)>>2)&0xFF] >> (((c)&0x3)*2)) & 0x3)
#endif

# UP: Stellt fest, ob ein Character alphabetisch ist.
# alphap(ch)
# > ch: Character-Code
# < ergebnis: true falls alphabetisch, false sonst.
# Alphabetische Characters sind die mit einem Code c, mit
#if defined(UNICODE)
# java.lang.Character.isLetter(c)
#else
# $41 <= c <= $5A oder $61 <= c <= $7A
#if defined(ISOLATIN_CHS)
# oder $C0 <= c außer c=$D7,$F7.
#elif defined(HPROMAN8_CHS)
# oder $A1 <= c <= $A7 oder $AD <= c <= $AE oder $B1 <= c <= $B7 außer c=$B3
# oder $C0 <= c <= $F1.
#elif defined(NEXTSTEP_CHS)
# oder $81 <= c <= $9D oder $D5 <= c <= $FD.
#elif defined(IBMPC_CHS)
# oder $80 <= c <= $9A oder $9F <= c <= $A7.
#endif
#endif
# Darin sind (siehe CLTL S. 236 oben) aller Uppercase- und alle Lowercase-
# Characters enthalten.
  local bool alphap (chart ch);
  local bool alphap(ch)
    var chart ch;
    {
      var cint c = as_cint(ch);
      #ifdef UNICODE
      return (unicode_attribute(c) == 3 ? true : false);
      #else
      if (c < 0x41) goto no; if (c <= 0x5A) goto yes;
      if (c < 0x61) goto no; if (c <= 0x7A) goto yes;
      #if defined(ISOLATIN_CHS)
      if (c < 0xC0) goto no;
      if ((c == 0xD7) || (c == 0xF7)) goto no; else goto yes;
      #elif defined(HPROMAN8_CHS)
      if (c < 0xA1) goto no;
      if (c > 0xF1) goto no; if (c >= 0xC0) goto yes;
      if (c <= 0xA7) goto yes;
      if (c < 0xB1) {
        if (c < 0xAD) goto no; if (c <= 0xAE) goto yes; goto no;
      } else {
        if (c > 0xB7) goto no; if (c == 0xB3) goto no; else goto yes;
      }
      #elif defined(NEXTSTEP_CHS)
      if (c < 0x81) goto no; if (c <= 0x9D) goto yes;
      if (c < 0xD5) goto no; if (c <= 0xFD) goto yes;
      #elif defined(IBMPC_CHS)
      if (c < 0x80) goto no; if (c <= 0x9A) goto yes;
      if (c < 0x9F) goto no; if (c <= 0xA7) goto yes;
      #endif
      no: return false;
      yes: return true;
      #endif
    }

# Stellt fest, ob ein Character numerisch ist.
# numericp(ch)
# > ch: Character-Code
# < ergebnis: true falls numerisch, false sonst.
  local bool numericp (chart ch);
  #ifdef UNICODE
    #define numericp(ch)  (unicode_attribute(as_cint(ch)) == 2)
  #else
    #define numericp(ch)  (('0' <= as_cint(ch)) && (as_cint(ch) <= '9'))
  #endif

# Stellt fest, ob ein Character alphanumerisch ist.
# alphanumericp(ch)
# > ch: Character-Code
# < ergebnis: true falls alphanumerisch, false sonst.
# Alphanumerische Characters sind die alphabetischen und die Ziffern.
  global bool alphanumericp (chart ch);
  global bool alphanumericp(ch)
    var chart ch;
    {
      #ifdef UNICODE
      var cint c = as_cint(ch);
      return (unicode_attribute(c) >= 2 ? true : false);
      #else
      if (numericp(ch)) return true;
      return alphap(ch);
      #endif
    }

# Stellt fest, ob ein Character ein Graphic-Character ("druckend") ist.
# graphic_char_p(ch)
# > ch: Character-Code
# < ergebnis: true falls druckend, false sonst.
# Graphic-Characters sind die mit einem Code c, mit
#if defined(UNICODE)
#       (java.lang.Character.isDefined(c) || c == 0x20AC)
#       && !(c < 0x0020 || (0x007F <= c <= 0x009F))
#elif defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS)
#       $20 <= c <= $7E oder $A0 <= c < $100.
#elif defined(NEXTSTEP_CHS)
#       $20 <= c <= $7E oder $80 <= c <= $A5 oder c in {$A7,$A8,$AA,$AB,$AE..$B7}
#       oder $BA <= c <= $FD außer c = $CD.
#elif defined(IBMPC_CHS)
#       $20 <= c < $100 oder c in {1,..,6}u{14,..,25}u{28,..,31}.
#       [c=11 und c=12 werden zwar auch druckend ausgegeben, aber c=12
#        ist unser #\Page, und c=11 streichen wir aus Gleichberechtigungs-
#        gründen.]
#else # defined(ASCII_CHS)
#       $20 <= c <= $7E.
#endif
  global bool graphic_char_p (chart ch);
  global bool graphic_char_p(ch)
    var chart ch;
    {
      # This would be the same as iswprint(ch), assuming wide characters were
      # Unicode.
      var cint c = as_cint(ch);
      #ifdef UNICODE
      return (unicode_attribute(c) == 0 ? false : true);
      #else
      #if defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS)
      if ((('~' >= c) && (c >= ' ')) || (c >= 0xA0)) goto yes; else goto no;
      #elif defined(NEXTSTEP_CHS)
      if (c <= '~') { if (c >= ' ') goto yes; else goto no; }
      if (c < 0xC0) {
        if (c < 0xA0) { if (c >= 0x80) goto yes; else goto no; }
        # Bit c-0xA0 aus der 32-Bit-Zahl %11111100111111111100110110111111 holen:
        if (0xFCFFCDBF & bit(c-0xA0)) goto yes; else goto no;
      } else {
        if ((c <= 0xFD) && !(c == 0xCD)) goto yes; else goto no;
      }
      #elif defined(IBMPC_CHS)
      if (c >= ' ') goto yes; # >= ' ' -> ja
      # 0 <= c < 32.
      # Bit c aus der 32-Bit-Zahl %11110011111111111100000001111110 holen:
      if (0xF3FFC07EUL & bit(c)) goto yes; else goto no;
      #else # defined(ASCII_CHS)
      if (c >= ' ') goto yes; else goto no;
      #endif
      no: return false;
      yes: return true;
      #endif
    }

# Returns the screen display width of a character.
# char_width(ch)
# > ch: character code
# < result: number of output columns occupied by ch
  global uintL char_width (chart ch);
  #ifdef UNICODE
  /*
   * Non-spacing attribute table.
   * See PropList.txt, or grep '^[^;]*;[^;]*;[^;]*;[^;]*;NSM;' UnicodeData.txt
   * Control characters are also marked non-spacing here, because they are not
   * printable. Zero width characters are also marked non-spacing here.
   */
  static const unsigned char nonspacing_table_data[15*64] = {
    /* 0x0000-0x01ff */
    0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, /* 0x0000-0x003f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, /* 0x0040-0x007f */
    0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, /* 0x0080-0x00bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x00c0-0x00ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0100-0x013f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0140-0x017f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0180-0x01bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x01c0-0x01ff */
    /* 0x0200-0x03ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0200-0x023f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0240-0x027f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0280-0x02bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x02c0-0x02ff */
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, /* 0x0300-0x033f */
    0xff, 0x7f, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, /* 0x0340-0x037f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0380-0x03bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x03c0-0x03ff */
    /* 0x0400-0x05ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0400-0x043f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0440-0x047f */
    0x78, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0480-0x04bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x04c0-0x04ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0500-0x053f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0540-0x057f */
    0x00, 0x00, 0xfe, 0xff, 0xfb, 0xff, 0xff, 0xbb, /* 0x0580-0x05bf */
    0x16, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x05c0-0x05ff */
    /* 0x0600-0x07ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0600-0x063f */
    0x00, 0xf8, 0x3f, 0x00, 0x00, 0x00, 0x01, 0x00, /* 0x0640-0x067f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0680-0x06bf */
    0x00, 0x00, 0xc0, 0xff, 0x9f, 0x3d, 0x00, 0x00, /* 0x06c0-0x06ff */
    0x00, 0x80, 0x02, 0x00, 0x00, 0x00, 0xff, 0xff, /* 0x0700-0x073f */
    0xff, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0740-0x077f */
    0x00, 0x00, 0x00, 0x00, 0xc0, 0xff, 0x01, 0x00, /* 0x0780-0x07bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x07c0-0x07ff */
    /* 0x0800-0x09ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0800-0x083f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0840-0x087f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0880-0x08bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x08c0-0x08ff */
    0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, /* 0x0900-0x093f */
    0xfe, 0x21, 0x1e, 0x00, 0x0c, 0x00, 0x00, 0x00, /* 0x0940-0x097f */
    0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, /* 0x0980-0x09bf */
    0x1e, 0x20, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, /* 0x09c0-0x09ff */
    /* 0x0a00-0x0bff */
    0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, /* 0x0a00-0x0a3f */
    0x86, 0x39, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, /* 0x0a40-0x0a7f */
    0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, /* 0x0a80-0x0abf */
    0xbe, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0ac0-0x0aff */
    0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x90, /* 0x0b00-0x0b3f */
    0x0e, 0x20, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0b40-0x0b7f */
    0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0b80-0x0bbf */
    0x01, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0bc0-0x0bff */
    /* 0x0c00-0x0dff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, /* 0x0c00-0x0c3f */
    0xc1, 0x3d, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0c40-0x0c7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, /* 0x0c80-0x0cbf */
    0x40, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0cc0-0x0cff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0d00-0x0d3f */
    0x0e, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0d40-0x0d7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0d80-0x0dbf */
    0x00, 0x04, 0x5c, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0dc0-0x0dff */
    /* 0x0e00-0x0fff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf2, 0x07, /* 0x0e00-0x0e3f */
    0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0e40-0x0e7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf2, 0x1b, /* 0x0e80-0x0ebf */
    0x00, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0ec0-0x0eff */
    0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0xa0, 0x02, /* 0x0f00-0x0f3f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x7f, /* 0x0f40-0x0f7f */
    0xdf, 0x00, 0xff, 0xfe, 0xff, 0xff, 0xff, 0x1f, /* 0x0f80-0x0fbf */
    0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x0fc0-0x0fff */
    /* 0x1000-0x11ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0xc5, 0x02, /* 0x1000-0x103f */
    0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, /* 0x1040-0x107f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1080-0x10bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x10c0-0x10ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1100-0x113f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1140-0x117f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1180-0x11bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x11c0-0x11ff */
    /* 0x1600-0x17ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1600-0x163f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1640-0x167f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1680-0x16bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x16c0-0x16ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1700-0x173f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1740-0x177f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x3f, /* 0x1780-0x17bf */
    0x40, 0xfe, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x17c0-0x17ff */
    /* 0x1800-0x19ff */
    0x00, 0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1800-0x183f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1840-0x187f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, /* 0x1880-0x18bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x18c0-0x18ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1900-0x193f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1940-0x197f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x1980-0x19bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x19c0-0x19ff */
    /* 0x2000-0x21ff */
    0x00, 0xf8, 0x00, 0x00, 0x00, 0x7c, 0x00, 0x00, /* 0x2000-0x203f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x00, 0x00, /* 0x2040-0x207f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x2080-0x20bf */
    0x00, 0x00, 0xff, 0xff, 0x0f, 0x00, 0x00, 0x00, /* 0x20c0-0x20ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x2100-0x213f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x2140-0x217f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x2180-0x21bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x21c0-0x21ff */
    /* 0x3000-0x31ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x00, 0x00, /* 0x3000-0x303f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x3040-0x307f */
    0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, /* 0x3080-0x30bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x30c0-0x30ff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x3100-0x313f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x3140-0x317f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x3180-0x31bf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x31c0-0x31ff */
    /* 0xfa00-0xfbff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfa00-0xfa3f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfa40-0xfa7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfa80-0xfabf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfac0-0xfaff */
    0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, /* 0xfb00-0xfb3f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfb40-0xfb7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfb80-0xfbbf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfbc0-0xfbff */
    /* 0xfe00-0xffff */
    0x00, 0x00, 0x00, 0x00, 0x0f, 0x00, 0x00, 0x00, /* 0xfe00-0xfe3f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfe40-0xfe7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xfe80-0xfebf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, /* 0xfec0-0xfeff */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xff00-0xff3f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xff40-0xff7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xff80-0xffbf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e  /* 0xffc0-0xffff */
  };
  static const signed char nonspacing_table_ind[128] = {
     0,  1,  2,  3,  4,  5,  6,  7, /* 0x0000-0x0fff */
     8, -1, -1,  9, 10, -1, -1, -1, /* 0x1000-0x1fff */
    11, -1, -1, -1, -1, -1, -1, -1, /* 0x2000-0x2fff */
    12, -1, -1, -1, -1, -1, -1, -1, /* 0x3000-0x3fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0x4000-0x4fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0x5000-0x5fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0x6000-0x6fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0x7000-0x7fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0x8000-0x8fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0x9000-0x9fff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0xa000-0xafff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0xb000-0xbfff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0xc000-0xcfff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0xd000-0xdfff */
    -1, -1, -1, -1, -1, -1, -1, -1, /* 0xe000-0xefff */
    -1, -1, -1, -1, -1, 13, -1, 14  /* 0xf000-0xffff */
  };
  #endif
  global uintL char_width(ch)
    var chart ch;
    {
      # This would be the same as wcwidth(ch), assuming wide characters were
      # Unicode, except that for non-printable characters we return 0, not -1.
      #ifdef UNICODE
      var cint c = as_cint(ch);
      # Test for non-spacing character.
      {
        int ind = nonspacing_table_ind[c >> 9];
        if (ind >= 0)
          if ((nonspacing_table_data[64*ind + ((c >> 3) & 63)] >> (c & 7)) & 1)
            return 0;
      }
      # Test for double-width character.
      # Generated from "grep '^....;[WF]' EastAsianWidth.txt"
      # and            "grep '^....;[^WF]' EastAsianWidth.txt"
      if (c >= 0x1100
          && ((c < 0x1160) /* Hangul Jamo */
              || (c >= 0x2e80 && c < 0xa4d0  /* CJK ... Yi */
                  && !((c & ~0x0011) == 0x300a || c == 0x303f))
              || (c >= 0xac00 && c < 0xd7a4) /* Hangul Syllables */
              || (c >= 0xf900 && c < 0xfb00) /* CJK Compatibility Ideographs */
              || (c >= 0xfe30 && c < 0xfe70) /* CJK Compatibility Forms */
              || (c >= 0xff00 && c < 0xff60) /* Fullwidth Forms */
              || (c >= 0xffe0 && c < 0xffe7)))
        return 2;
      return 1;
      #else
      return (graphic_char_p(ch) ? 1 : 0);
      #endif
    }

#ifdef HAVE_SMALL_SSTRING
# Copies an array of scint to an array of chart.
# scintcopy(src,dest,len);
# > scint* src: small characters
# > chart* dest: room for normal characters
# > uintL len: number of characters to be copied, > 0
  global void scintcopy (const scint* src, chart* dest, uintL len);
  global void scintcopy(src,dest,len)
    var const scint* src;
    var chart* dest;
    var uintL len;
    {
      dotimespL(len,len, {
        *dest++ = as_chart(*src++);
      });
    }
#endif

# UP: verfolgt einen String.
# unpack_string(string,&tot_len,&fil_len,&offset)
# > object string: the string.
# < uintL tot_len: the total length of the string
# < uintL fil_len: the fill-pointer length of the string
# < uintL offset: offset in the data vector.
# < object return: data vector
local object unpack_string (object string, uintL* tot_len, uintL* fil_len,
                            uintL* offset) {
  if (simple_string_p(string)) {
    var uintL len = Sstring_length(string);
    if (tot_len) *tot_len = len;
    if (fil_len) *fil_len = len;
    *offset = 0;
    return string;
  } else {
   # string, but not simple-string => follow the displacement
   # determine the length (like vector_length() in array.d):
    var uintL tot_size;
    var uintL fil_size;
    {
      var Iarray addr = TheIarray(string);
      var uintL offset_fil = offsetofa(iarray_,dims);
      if (iarray_flags(addr) & bit(arrayflags_dispoffset_bit))
        offset_fil += sizeof(uintL);
      var uintL offset_tot = offset_fil;
      if (iarray_flags(addr) & bit(arrayflags_fillp_bit))
        offset_fil += sizeof(uintL);
      fil_size = *(uintL*)pointerplus(addr,offset_fil);
      tot_size = *(uintL*)pointerplus(addr,offset_tot);
    }
    if (tot_len) *tot_len = tot_size;
    if (fil_len) *fil_len = fil_size;
    # follow the displacement:
    *offset = 0;
    return iarray_displace_check(string,fil_size,offset);
  }
}

# UP: unpack a string
# unpack_string_ro(string,&len,&offset)  [for read-only access]
# > object string: a string
# < uintL len: the fill-pointer length of the string
# < uintL offset: offset into the datastorage vector
# < object result: datastorage vector
global object unpack_string_ro (object string, uintL* len, uintL* offset) {
  return unpack_string(string,NULL,len,offset);
}

# UP: unpack a string
# unpack_string_rw(string,&len)  [for read-write access]
# > object string: a string
# < uintL len: the fill-pointer length of the string
# < chart* result: the beginning of the characters
global chart* unpack_string_rw (object string, uintL* len) {
  var uintL index = 0;
  var object unpacked = unpack_string(string,NULL,len,&index);
  check_sstring_mutable(unpacked);
  return &TheSstring(unpacked)->data[index];
}

# UP: vergleicht zwei Strings auf Gleichheit
# string_gleich(string1,string2)
# > string1: String
# > string2: simple-string
# < ergebnis: /=0, wenn gleich
  global bool string_gleich (object string1, object string2);
  global bool string_gleich(string1,string2)
    var object string1;
    var object string2;
    {
      var uintL len1;
      var uintL offset1;
      string1 = unpack_string_ro(string1,&len1,&offset1);
      # Längenvergleich:
      if (!(len1 == Sstring_length(string2)))
        goto no;
      # Now both strings have exactly len1 characters. Compare them.
      if (len1 > 0) {
        SstringDispatch(string1,
          { var const chart* ptr1 = &TheSstring(string1)->data[offset1];
            SstringDispatch(string2,
              { var const chart* ptr2 = &TheSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(*ptr1++,*ptr2++))
                    goto no;
                });
              },
              { var const scint* ptr2 = &TheSmallSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(*ptr1++,as_chart(*ptr2++)))
                    goto no;
                });
              }
              );
          },
          { var const scint* ptr1 = &TheSmallSstring(string1)->data[offset1];
            SstringDispatch(string2,
              { var const chart* ptr2 = &TheSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(as_chart(*ptr1++),*ptr2++))
                    goto no;
                });
              },
              { var const scint* ptr2 = &TheSmallSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(as_chart(*ptr1++),as_chart(*ptr2++)))
                    goto no;
                });
              }
              );
          }
          );
      }
      return true;
     no: return false;
    }

# UP: vergleicht zwei Strings auf Gleichheit, case-insensitive
# string_equal(string1,string2)
# > string1: String
# > string2: simple-string
# < ergebnis: /=0, wenn gleich
  global bool string_equal (object string1, object string2);
  global bool string_equal(string1,string2)
    var object string1;
    var object string2;
    {
      var uintL len1;
      var uintL offset1;
      string1 = unpack_string_ro(string1,&len1,&offset1);
      # Längenvergleich:
      if (!(len1 == Sstring_length(string2)))
        goto no;
      # Now both strings have exactly len1 characters. Compare them.
      if (len1 > 0) {
        SstringDispatch(string1,
          { var const chart* ptr1 = &TheSstring(string1)->data[offset1];
            SstringDispatch(string2,
              { var const chart* ptr2 = &TheSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(up_case(*ptr1++),up_case(*ptr2++)))
                    goto no;
                });
              },
              { var const scint* ptr2 = &TheSmallSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(up_case(*ptr1++),up_case(as_chart(*ptr2++))))
                    goto no;
                });
              }
              );
          },
          { var const scint* ptr1 = &TheSmallSstring(string1)->data[offset1];
            SstringDispatch(string2,
              { var const chart* ptr2 = &TheSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(up_case(as_chart(*ptr1++)),up_case(*ptr2++)))
                    goto no;
                });
              },
              { var const scint* ptr2 = &TheSmallSstring(string2)->data[0];
                var uintL count;
                dotimespL(count,len1, {
                  if (!chareq(up_case(as_chart(*ptr1++)),up_case(as_chart(*ptr2++))))
                    goto no;
                });
              }
              );
          }
          );
      }
      return true;
     no: return false;
    }

#ifdef UNICODE
# UP: Bildet einen Simple-String mit gegebenen Elementen.
# stringof(len)
# > uintL len: gewünschte Vektorlänge
# > auf STACK: len Characters, erstes zuoberst
# < ergebnis: Simple-String mit diesen Objekten
# Erhöht STACK
# changes STACK, can trigger GC
  global object stringof (uintL len);
  global object stringof(len)
    var uintL len;
    {
      var object new_string = allocate_string(len);
      if (len > 0) {
        var object* topargptr = STACK STACKop len;
        var object* argptr = topargptr;
        var chart* ptr = &TheSstring(new_string)->data[0];
        dotimespL(len,len, {
          *ptr++ = char_code(NEXT(argptr));
        });
        set_args_end_pointer(topargptr);
      }
      return new_string;
    }
#endif

# UP: kopiert einen String und macht dabei einen Simple-String draus.
# copy_string(string)
# > string: String
# < ergebnis: mutable Normal-Simple-String mit denselben Zeichen
# can trigger GC
  global object copy_string (object string);
  global object copy_string(string)
    var object string;
    {
      var uintL len;
      var uintL offset;
      string = unpack_string_ro(string,&len,&offset);
      pushSTACK(string); # String retten
      var object new_string = allocate_string(len);
      # new_string = neuer Normal-Simple-String mit vorgegebener Länge len
      string = popSTACK(); # String zurück
      if (len > 0) {
        SstringDispatch(string,
          { chartcopy(&TheSstring(string)->data[offset],&TheSstring(new_string)->data[0],len); },
          { scintcopy(&TheSmallSstring(string)->data[offset],&TheSstring(new_string)->data[0],len); }
          );
      }
      return new_string;
    }

# UP: wandelt einen String in einen Simple-String um.
# coerce_ss(obj)
# > obj: Lisp-Objekt, sollte ein String sein.
# < ergebnis: Simple-String mit denselben Zeichen
# can trigger GC
  global object coerce_ss (object obj);
  global object coerce_ss(obj)
    var object obj;
    {
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj))
        switch (Record_type(obj)) {
          case_Rectype_Sstring_above;
          case_Rectype_ostring_above;
          default: break;
        }
      switch (0)
      #endif
        {
          case_sstring:
            # Simple-String, unverändert zurück
            return obj;
          case_ostring:
            # sonstiger String, kopieren
            return copy_string(obj);
          default:
            pushSTACK(obj); # TYPE-ERROR slot DATUM
            pushSTACK(S(string)); # TYPE-ERROR slot EXPECTED-TYPE
            pushSTACK(obj);
            fehler(type_error,
                   GETTEXT("This is not a string: ~")
                  );
        }
    }

#ifndef TYPECODES
# UP: wandelt einen String in einen immutablen Simple-String um.
# coerce_imm_ss(obj)
# > obj: Lisp-Objekt, sollte ein String sein.
# < ergebnis: immutabler Simple-String mit denselben Zeichen
# can trigger GC
  global object coerce_imm_ss (object obj);
  global object coerce_imm_ss(obj)
    var object obj;
    { if (orecordp(obj))
        switch (Record_type(obj)) {
          #ifdef HAVE_SMALL_SSTRING
          case Rectype_Imm_SmallSstring:
          #endif
          case Rectype_Imm_Sstring:
            # immutabler Simple-String, unverändert zurück
            return obj;
          case Rectype_Sstring:
          case Rectype_string:
            # sonstiger String, kopieren
            {
              var uintL len;
              var uintL offset;
              var object string = unpack_string_ro(obj,&len,&offset);
              #ifdef HAVE_SMALL_SSTRING
              if (Record_type(string) == Rectype_Imm_SmallSstring) {
                pushSTACK(string);
                var object new_string = allocate_imm_small_string(len);
                string = popSTACK();
                if (len > 0) {
                  var const scint* ptr1 = &TheSmallSstring(string)->data[offset];
                  var scint* ptr2 = &TheSmallSstring(new_string)->data[0];
                  var uintL count;
                  dotimespL(count,len, { *ptr2++ = *ptr1++; } );
                }
                return new_string;
              }
              # We use alloca for small-simple-strings, therefore their length
              # should not be too large, or we risk an SP overflow and
              # core dump.
              if (len < 0x10000) {
                # Check if all characters fit into a small-simple-string:
                if (len > 0) {
                  var const chart* ptr = &TheSstring(string)->data[offset];
                  var uintL count;
                  dotimespL(count,len, {
                    if (as_cint(*ptr++) >= small_char_int_limit)
                      goto make_nonsmall;
                  });
                }
                pushSTACK(string);
                var object new_string = allocate_imm_small_string(len);
                string = popSTACK();
                if (len > 0) {
                  var const chart* ptr1 = &TheSstring(string)->data[offset];
                  var scint* ptr2 = &TheSmallSstring(new_string)->data[0];
                  var uintL count;
                  dotimespL(count,len, { *ptr2++ = as_cint(*ptr1++); } );
                }
                return new_string;
              }
             make_nonsmall:
              #endif
              pushSTACK(string);
              var object new_string = allocate_imm_string(len);
              string = popSTACK();
              if (len > 0)
                chartcopy(&TheSstring(string)->data[offset],&TheSstring(new_string)->data[0],len);
              return new_string;
            }
          default:
            break;
        }
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(S(string)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      fehler(type_error,
             GETTEXT("This is not a string: ~")
            );
    }
#endif

#ifdef HAVE_SMALL_SSTRING
# UP: wandelt einen String in einen Normal-Simple-String um.
# coerce_normal_ss(obj)
# > obj: Lisp-Objekt, sollte ein String sein.
# < ergebnis: Normal-Simple-String mit denselben Zeichen
# can trigger GC
  global object coerce_normal_ss (object obj);
  global object coerce_normal_ss(obj)
    var object obj;
    {
      if (orecordp(obj))
        switch (Record_type(obj)) {
          case Rectype_Imm_Sstring:
          case Rectype_Sstring:
            # Normal-Simple-String, unverändert zurück
            return obj;
          case Rectype_Imm_SmallSstring:
          case Rectype_string:
            # sonstiger String, kopieren
            return copy_string(obj);
          default:
            break;
        }
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(S(string)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      fehler(type_error,
             GETTEXT("This is not a string: ~")
            );
    }
#endif

#if 0 # unused
#ifdef HAVE_SMALL_SSTRING
# UP: wandelt einen String in einen immutablen Normal-Simple-String um.
# coerce_imm_normal_ss(obj)
# > obj: Lisp-Objekt, sollte ein String sein.
# < ergebnis: immutabler Normal-Simple-String mit denselben Zeichen
# can trigger GC
  global object coerce_imm_normal_ss (object obj);
  global object coerce_imm_normal_ss(obj)
    var object obj;
    {
      if (orecordp(obj))
        switch (Record_type(obj)) {
          case Rectype_Imm_Sstring:
            # immutabler Normal-Simple-String, unverändert zurück
            return obj;
          case Rectype_Imm_SmallSstring:
          case Rectype_Sstring:
          case Rectype_string:
            # sonstiger String, kopieren
            {
              var uintL len;
              var uintL offset;
              var object string = unpack_string_ro(obj,&len,&offset);
              pushSTACK(string);
              var object new_string = allocate_imm_string(len);
              string = popSTACK();
              if (len > 0) {
                SstringDispatch(string,
                  { chartcopy(&TheSstring(string)->data[offset],&TheSstring(new_string)->data[0],len); },
                  { scintcopy(&TheSmallSstring(string)->data[offset],&TheSstring(new_string)->data[0],len); }
                  );
              }
              return new_string;
            }
          default:
            break;
        }
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(S(string)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      fehler(type_error,
             GETTEXT("This is not a string: ~")
            );
    }
#endif
#endif

# UP: Konversion eines Objekts zu einem Character
# coerce_char(obj)
# > obj: Lisp-Objekt
# < ergebnis: Character oder NIL
  global object coerce_char (object obj);
  global object coerce_char(obj)
    var object obj;
    {
      if (charp(obj)) {
        return obj; # Character unverändert zurück
      } elif (symbolp(obj)) {
        # obj ist ein Symbol
        obj = TheSymbol(obj)->pname; goto string;
      } elif (stringp(obj)) {
       string: # obj ist ein String
        var uintL len;
        var uintL offset;
        var object string = unpack_string_ro(obj,&len,&offset);
        # ab ptr kommen len Characters
        if (len==1) {
          SstringDispatch(string,
            { return code_char(TheSstring(string)->data[offset]); },
            { return code_char(as_chart(TheSmallSstring(string)->data[offset])); }
            );
        }
      } elif (nullp(Symbol_value(S(coerce_fixnum_char_ansi)))
              && posfixnump(obj)) {
        var uintL code = posfixnum_to_L(obj);
        if (code < char_code_limit)
          # obj ist ein Fixnum >=0, < char_code_limit
          return code_char(as_chart(code));
      }
      # war nichts von allem -> nicht in Character umwandelbar
      return NIL; # NIL als Ergebnis
    }

# Character-Namen:
# Nur die Characters mit Font 0 und Bits 0 haben Namen. Unter diesen
# sind alle non-graphic Characters und das Space.
# Vom Reader wird allerdings auch
# - die Syntax #\A für das Character A (usw. für alle Characters),
# - die Syntax #\^A für das Character 'A'-64 (usw. für alle Control-Characters
#   des ASCII-Zeichensatzes) und
# - die Syntax #\Code231 für das Character mit dem Code 231 (dezimal)
# akzeptiert, dies für alle Characters aus Font 0.

# Tabelle der Character-Namen:
# in CONSTOBJ.D definiert,
  #ifdef AMIGA_CHARNAMES
    #define charname_table_length  45  # Länge der Tabelle
    #define charname_table  ((object*)(&object_tab.charname_0)) # Tabelle fängt mit charname_0 an
  #endif
  #ifdef MSDOS_CHARNAMES
    #define charname_table_length  13  # Länge der Tabelle
    #define charname_table  ((object*)(&object_tab.charname_0)) # Tabelle fängt mit charname_0 an
  #endif
  #ifdef WIN32_CHARNAMES
    #define charname_table_length  13  # Länge der Tabelle
    #define charname_table  ((object*)(&object_tab.charname_0)) # Tabelle fängt mit charname_0 an
  #endif
  #ifdef UNIX_CHARNAMES
    #define charname_table_length  46  # Länge der Tabelle
    #define charname_table  ((object*)(&object_tab.charname_0bis)) # Tabelle fängt mit charname_0bis an
  #endif
# Tabelle der Codes zu diesen Namen:
  local const uintB charname_table_codes [charname_table_length]
    #ifdef AMIGA_CHARNAMES
      = { 0,1,2,3,4,5,6,BEL,BS,TAB,NL,11,PG,CR,14,15,16,17,18,19,20,21,22,
          23,24,25,26,ESC,28,29,30,31,' ',127,7,8,9,LF,10,12,13,27,127,RUBOUT,
          155,
        };
    #endif
    #ifdef MSDOS_CHARNAMES
      = { 0,BEL,BS,TAB,NL,11,PG,CR,26,ESC,' ',RUBOUT,LF, };
    #endif
    #ifdef WIN32_CHARNAMES
      = { 0,BEL,BS,TAB,NL,11,PG,CR,26,ESC,' ',RUBOUT,LF, };
    #endif
    #ifdef UNIX_CHARNAMES
      = { 0,7,BS,TAB,NL,LF,PG,CR,27,32,RUBOUT,127,
          0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
          20,21,22,23,24,25,26,27,28,29,30,31,32,127,
        };
    #endif
# Zum Namen charname_table[i] gehört der Code charname_table_codes[i]
# (für 0 <= i < charname_table_length).

#ifdef UNICODE

# Table of Unicode character names, derived from UnicodeData.txt.
#include "uninames.c"

# Returns the word with a given index.
  local const char* unicode_name_word (uintWL index, uintL* lengthp);
  local const char* unicode_name_word(index,lengthp)
    var uintWL index;
    var uintL* lengthp;
    {
      ASSERT(index < UNICODE_CHARNAME_NUM_WORDS);
      # Binary search for i with
      #   unicode_name_by_length[i].ind_offset <= index
      # and
      #   index < unicode_name_by_length[i+1].ind_offset
      var uintL i1 = 0;
      var uintL i2 = sizeof(unicode_name_by_length)/sizeof(unicode_name_by_length[0]) - 1;
      while (i2-i1 > 1) {
        var uintL i = (i1+i2)>>1;
        if (unicode_name_by_length[i].ind_offset <= index)
          i1 = i;
        else
          i2 = i;
      }
      var uintL i = i1;
      ASSERT(unicode_name_by_length[i].ind_offset <= index
             && index < unicode_name_by_length[i+1].ind_offset);
      *lengthp = i;
      return &unicode_name_words[unicode_name_by_length[i].extra_offset + (index-unicode_name_by_length[i].ind_offset)*i];
    }

# Looks up the index of a word.
  local sintL unicode_name_word_lookup (const char* word, uintL length);
  local sintL unicode_name_word_lookup(word,length)
    var const char* word;
    var uintL length;
    {
      if (length > 0 && length < sizeof(unicode_name_by_length)/sizeof(unicode_name_by_length[0]) - 1) {
        # Binary search among the words of given length.
        var uintL extra_offset = unicode_name_by_length[length].extra_offset;
        var uintL i0 = unicode_name_by_length[length].ind_offset;
        var uintL i1 = i0;
        var uintL i2 = unicode_name_by_length[length+1].ind_offset;
        while (i2-i1 > 0) {
          var uintL i = (i1+i2)>>1;
          var const char* p = &unicode_name_words[extra_offset + (i-i0)*length];
          var const char* w = word;
          var uintL n = length;
          loop {
            if (*p < *w) {
              if (i1 == i)
                return -1;
              # Note here: i1 < i < i2.
              i1 = i;
              break;
            }
            if (*p > *w) {
              # Note here: i1 <= i < i2.
              i2 = i;
              break;
            }
            p++; w++; n--;
            if (n == 0)
              return i;
          }
        }
      }
      return -1;
    }

# Auxiliary tables for Hangul syllable names, see the Unicode 3.0 book,
# sections 3.11 and 4.4.
static const char jamo_initial_short_name[19][3] = {
  "G", "GG", "N", "D", "DD", "R", "M", "B", "BB", "S", "SS", "", "J", "JJ",
  "C", "K", "T", "P", "H"
};
static const char jamo_medial_short_name[21][4] = {
  "A", "AE", "YA", "YAE", "EO", "E", "YEO", "YE", "O", "WA", "WAE", "OE", "YO",
  "U", "WEO", "WE", "WI", "YU", "EU", "YI", "I"
};
static const char jamo_final_short_name[28][3] = {
  "", "G", "GG", "GS", "N", "NI", "NH", "D", "L", "LG", "LM", "LB", "LS", "LT",
  "LP", "LH", "M", "B", "BS", "S", "SS", "NG", "J", "C", "K", "T", "P", "H"
};

#endif

# UP: Liefert den Namen eines Zeichens.
# char_name(code)
# > chart code: Code eines Zeichens
# < ergebnis: Simple-String (Name dieses Zeichens) oder NIL
# can trigger GC
  global object char_name (chart code);
  global object char_name(code)
    var chart code;
    {
      var cint c = as_cint(code);
      {
        var const uintB* codes_ptr = &charname_table_codes[0];
        var object* strings_ptr = &charname_table[0];
        var uintC count;
        dotimesC(count,charname_table_length, {
          if (c == *codes_ptr++) # code mit charname_table_codes[i] vergleichen
            return *strings_ptr; # String charname_table[i] aus der Tabelle holen
          strings_ptr++;
        });
      }
      # nicht gefunden
      #ifdef UNICODE
      # Try to find the long name, from UnicodeData.txt. It is the second
      # semicolon separated field from (sys::unicode-attributes-line c).
      #ifdef AWFULLY_SLOW
      {
        pushSTACK(fixnum(c));
        funcall(S(unicode_attributes_line),1);
        var object line = value1;
        if (!nullp(line)) {
          var uintL len = Sstring_length(line);
          var uintL i1;
          var uintL i2;
          for (i1 = 0; i1 < len; i1++)
            if (chareq(TheSstring(line)->data[i1],ascii(';'))) {
              i1++;
              for (i2 = i1; i2 < len; i2++)
                if (chareq(TheSstring(line)->data[i2],ascii(';'))) {
                  if (!chareq(TheSstring(line)->data[i1],ascii('<'))) {
                    var object name = subsstring(line,i1,i2);
                    # Replace ' ' with '_':
                    var uintL count = i2-i1;
                    if (count > 0) {
                      var chart* ptr = &TheSstring(name)->data[0];
                      dotimespL(count,count, {
                        if (chareq(*ptr,ascii(' ')))
                          *ptr = ascii('_');
                        ptr++;
                      });
                    }
                    return name;
                  }
                  break;
                }
              break;
            }
        }
      }
      #else
      # Here is a much faster implementation.
      if (c >= 0xAC00 && c <= 0xD7A3) {
        # Special case for Hangul syllables. Keeps the tables small.
        var char buf[16+7];
        var char* ptr;
        memcpy(buf,"HANGUL_SYLLABLE_",16);
        ptr = buf+16;
        var uintL tmp = c - 0xAC00;
        var uintL index3 = tmp % 28; tmp = tmp / 28;
        var uintL index2 = tmp % 21; tmp = tmp / 21;
        var uintL index1 = tmp;
        var const char* q;
        q = jamo_initial_short_name[index1];
        while (*q != '\0') { *ptr++ = *q++; }
        q = jamo_medial_short_name[index2];
        while (*q != '\0') { *ptr++ = *q++; }
        q = jamo_final_short_name[index3];
        while (*q != '\0') { *ptr++ = *q++; }
        return n_char_to_string(buf,ptr-buf,Symbol_value(S(ascii)));
      } else {
        var const uint16* words;
        {
          # Binary search in unicode_code_to_name.
          var uintL i1 = 0;
          var uintL i2 = sizeof(unicode_code_to_name)/sizeof(unicode_code_to_name[0]);
          loop {
            var uintL i = (i1+i2)>>1;
            if (unicode_code_to_name[i].code == c) {
              words = &unicode_names[unicode_code_to_name[i].name];
              break;
            } else if (unicode_code_to_name[i].code < c) {
              if (i1 == i) {
                words = NULL;
                break;
              }
              # Note here: i1 < i < i2.
              i1 = i;
            } else if (unicode_code_to_name[i].code > c) {
              # Note here: i1 <= i < i2.
              i2 = i;
            }
          }
        }
        if (words != NULL) {
          # Found it in unicode_code_to_name. Now concatenate the words.
          var char buf[UNICODE_CHARNAME_MAX_LENGTH];
          var char* ptr = buf;
          loop {
            var uintL wordlen;
            var const char* word = unicode_name_word(*words>>1,&wordlen);
            do { *ptr++ = *word++; } while (--wordlen > 0);
            if ((*words & 1) == 0)
              break;
            *ptr++ = '_';
            words++;
          }
          return n_char_to_string(buf,ptr-buf,Symbol_value(S(ascii)));
        }
      }
      #endif
      # CLHS (glossary "name" 5) specifies that all non-graphic characters have
      # a name. Let's give a name to all of them, it's more uniform (and avoids
      # printer errors).
      /* if (!graphic_char_p(code)) */
      {
        var object name = allocate_string(5);
        local char hex_table[] = "0123456789ABCDEF";
        TheSstring(name)->data[0] = ascii('U');
        TheSstring(name)->data[1] = ascii(hex_table[(c>>12)&0x0F]);
        TheSstring(name)->data[2] = ascii(hex_table[(c>>8)&0x0F]);
        TheSstring(name)->data[3] = ascii(hex_table[(c>>4)&0x0F]);
        TheSstring(name)->data[4] = ascii(hex_table[c&0x0F]);
        return name;
      }
      #endif
      return NIL;
    }

# UP: Bestimmt das Character mit einem gegebenen Namen
# name_char(string)
# > string: String
# < ergebnis: Character mit diesem Namen, oder NIL falls keins existiert
  global object name_char (object string);
  global object name_char(string)
    var object string;
    {
      {
        var const uintB* codes_ptr = &charname_table_codes[0];
        var object* strings_ptr = &charname_table[0];
        var uintC count;
        dotimesC(count,charname_table_length, {
          if (string_equal(string,*strings_ptr++)) # string mit charname_table[i] vergleichen
            return code_char(as_chart(*codes_ptr)); # Code charname_table_codes[i] aus der Tabelle holen
          codes_ptr++;
        });
      }
      # kein Character mit diesem Namen gefunden
      #ifdef UNICODE
      {
        var uintL len;
        var uintL offset;
        string = unpack_string_ro(string,&len,&offset);
        if (len > 1 && len <= UNICODE_CHARNAME_MAX_LENGTH) {
          var const chart* charptr;
          unpack_sstring_alloca(string,len,offset, charptr=);
          # Test for Uxxxx syntax.
          if (len == 5
              && (chareq(charptr[0],ascii('U')) || chareq(charptr[0],ascii('u')))) {
            # Hexadezimalzahl entziffern:
            var uintL code = 0;
            var uintL index = 1;
            var const chart* tmpcharptr = charptr+1;
            loop {
              var cint c = as_cint(*tmpcharptr++); # nächstes Character
              # soll Hexadezimalziffer sein:
              if (c > 'f') break;
              if (c >= 'a') { c -= 'a'-'A'; }
              if (c < '0') break;
              if (c <= '9') { c = c - '0'; }
              else if ((c >= 'A') && (c <= 'F')) { c = c - 'A' + 10; }
              else break;
              code = 16*code + c; # Ziffer dazunehmen
              # code soll < char_code_limit bleiben:
              if (code >= char_code_limit) break; # sollte nicht passieren
              index++;
              if (index == len) {
                # Charactername war vom Typ "Uxxxx" mit code = xxxx < char_code_limit
                # Don't test for graphic_char_p - see comment in char_name().
                # This also avoids us special-casing the #\Uxxxx syntax in io.d.
                /* if (!graphic_char_p(as_chart(code))) */
                return code_char(as_chart(code));
              }
            }
          }
          # Test for word1_word2_... syntax.
          {
            var char buf[UNICODE_CHARNAME_MAX_LENGTH];
            var char* ptr = buf;
            loop {
              var cint c = as_cint(*charptr++);
              if (!(c >= ' ' && c <= '~'))
                break;
              *ptr++ = (char)(c >= 'a' && c <= 'z' ? c-'a'+'A' : c);
              if (--len == 0)
                goto filled_buf;
            }
            if (false) {
             filled_buf:
              # Convert the constituents to uint16 words.
              var uint16 words[UNICODE_CHARNAME_MAX_WORDS];
              var uint16* wordptr = words;
              {
                var const char* p1 = buf;
                loop {
                  var const char* p2 = p1;
                  while (p2 < ptr && *p2 != '_') p2++;
                  var sintL word = unicode_name_word_lookup(p1,p2-p1);
                  if (word < 0)
                    break;
                  if (wordptr == &words[UNICODE_CHARNAME_MAX_WORDS])
                    break;
                  *wordptr++ = word;
                  if (p2 == ptr)
                    goto filled_words;
                  p1 = p2+1;
                  # Special case for Hangul syllables. Keeps the tables small.
                  if (wordptr == &words[2]
                      && words[0] == UNICODE_CHARNAME_WORD_HANGUL
                      && words[1] == UNICODE_CHARNAME_WORD_SYLLABLE) {
                    # Split the last word [p1..ptr) into three parts:
                    # 1) [BCDGHJKMNPRST]
                    # 2) [AEIOUWY]
                    # 3) [BCDGHIJKLMNPST]
                    var const char* p2 = p1;
                    while (p2 < ptr
                           && (*p2=='B' || *p2=='C' || *p2=='D' || *p2=='G'
                               || *p2=='H' || *p2=='J' || *p2=='K' || *p2=='M'
                               || *p2=='N' || *p2=='P' || *p2=='R' || *p2=='S'
                               || *p2=='T'))
                      p2++;
                    var const char* p3 = p2;
                    while (p3 < ptr
                           && (*p3=='A' || *p3=='E' || *p3=='I' || *p3=='O'
                               || *p3=='U' || *p3=='W' || *p3=='Y'))
                      p3++;
                    var const char* p4 = p3;
                    while (p4 < ptr
                           && (*p4=='B' || *p4=='C' || *p4=='D' || *p4=='G'
                               || *p4=='H' || *p4=='I' || *p4=='J' || *p4=='K'
                               || *p4=='L' || *p4=='M' || *p4=='N' || *p4=='P'
                               || *p4=='S' || *p4=='T'))
                      p4++;
                    if (p4 == ptr) {
                      var uintL n1 = p2-p1;
                      var uintL n2 = p3-p2;
                      var uintL n3 = p4-p3;
                      if (n1 <= 2 && (n2 >= 1 && n2 <= 3) && n3 <= 2) {
                        var uintL index1;
                        for (index1 = 0; index1 < 19; index1++)
                          if (memcmp(jamo_initial_short_name[index1],p1,n1)==0
                              && jamo_initial_short_name[index1][n1] == '\0') {
                            var uintL index2;
                            for (index2 = 0; index2 < 21; index2++)
                              if (memcmp(jamo_medial_short_name[index2],p2,n2)==0
                                  && jamo_medial_short_name[index2][n2] == '\0') {
                                var uintL index3;
                                for (index3 = 0; index3 < 28; index3++)
                                  if (memcmp(jamo_final_short_name[index3],p3,n3)==0
                                      && jamo_final_short_name[index3][n3] == '\0') {
                                    return code_char(as_chart(0xAC00 + (index1*21+index2)*28+index3));
                                  }
                                break;
                              }
                            break;
                          }
                      }
                    }
                  }
                }
              }
              if (false) {
               filled_words:
                # Multiply by 2, to simplify later comparisons.
                var uintL words_length = wordptr - words;
                {
                  var sintL i = words_length-1;
                  words[i] = 2*words[i];
                  for (; --i >= 0; )
                    words[i] = 2*words[i] + 1;
                }
                # Binary search in unicode_name_to_code.
                var uintL i1 = 0;
                var uintL i2 = sizeof(unicode_name_to_code)/sizeof(unicode_name_to_code[0]);
                loop {
                  var uintL i = (i1+i2)>>1;
                  var const uint16* w = words;
                  var const uint16* p = &unicode_names[unicode_name_to_code[i].name];
                  var uintL n = words_length;
                  loop {
                    if (*p < *w) {
                      if (i1 == i)
                        goto name_not_found;
                      # Note here: i1 < i < i2.
                      i1 = i;
                      break;
                    } else if (*p > *w) {
                      # Note here: i1 <= i < i2.
                      i2 = i;
                      break;
                    }
                    p++; w++; n--;
                    if (n == 0)
                      return code_char(as_chart(unicode_name_to_code[i].code));
                  }
                }
               name_not_found:;
              }
            }
          }
        }
      }
      #endif
      return NIL;
    }

LISPFUNN(standard_char_p,1) # (STANDARD-CHAR-P char), CLTL S. 234
# (standard-char-p char) ==
#   (or (char= char #\Newline) (char<= #\Space char #\~))
# Standard-Chars sind die mit einem Code c, mit
#       $20 <= c <= $7E oder c = NL.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    var chart ch = char_code(arg);
    var cint c = as_cint(ch);
    if ((('~' >= c) && (c >= ' ')) || (c == NL)) {
      value1 = T; mv_count=1;
    } else {
      value1 = NIL; mv_count=1;
    }
  }

LISPFUNN(graphic_char_p,1) # (GRAPHIC-CHAR-P char), CLTL S. 234
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    if (graphic_char_p(char_code(arg)))
      goto yes;
    else
      goto no;
   yes: value1 = T; mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(char_width,1) # (CHAR-WIDTH char)
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    value1 = fixnum(char_width(char_code(arg))); mv_count=1;
  }

LISPFUNN(string_char_p,1) # (STRING-CHAR-P char), CLTL S. 235
# Alle Characters sind String-Chars.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
   yes: value1 = T; mv_count=1; return;
  }

#if (base_char_code_limit < char_code_limit)
LISPFUNN(base_char_p,1) # (SYSTEM::BASE-CHAR-P char)
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    if (as_cint(char_code(arg)) >= base_char_code_limit)
      goto no;
    else
      goto yes;
   no: value1 = NIL; mv_count=1; return;
   yes: value1 = T; mv_count=1; return;
  }
#endif

LISPFUNN(alpha_char_p,1) # (ALPHA-CHAR-P char), CLTL S. 235
# Teste mit ALPHAP.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    if (alphap(char_code(arg)))
      goto yes;
    else
      goto no;
   yes: value1 = T; mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(upper_case_p,1) # (UPPER-CASE-P char), CLTL S. 235
# Upper-case-Characters sind die mit einem Code c mit 0 <= c < $100, die
# von (downcase char) verschieden sind.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    var chart ch = char_code(arg);
    if (!chareq(down_case(ch),ch))
      goto yes;
    else
      goto no;
   yes: value1 = T; mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(lower_case_p,1) # (LOWER-CASE-P char), CLTL S. 235
# Lower-case-Characters sind die mit einem Code c mit 0 <= c < $100, die
# von (upcase char) verschieden sind.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    var chart ch = char_code(arg);
    if (!chareq(up_case(ch),ch))
      goto yes;
    else
      goto no;
   yes: value1 = T; mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(both_case_p,1) # (BOTH-CASE-P char), CLTL S. 235
# (both-case-p char) == (or (upper-case-p char) (lower-case-p char))
# Both-case-Characters sind die mit einem Code c mit 0 <= c < $100, bei denen
# (downcase char) und (upcase char) verschieden sind.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    var chart ch = char_code(arg);
    if (!chareq(down_case(ch),up_case(ch)))
      goto yes;
    else
      goto no;
   yes: value1 = T; mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

# UP: Uberprüft ein optionales Radix-Argument
# test_radix_arg()
# > STACK_0: Argument, Default ist 10
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Radix, ein Integer >=2, <=36
# erhöht STACK um 1
  local uintWL test_radix_arg (void);
  local uintWL test_radix_arg()
    {
      var object arg = popSTACK(); # Argument
      if (eq(arg,unbound))
        return 10;
      if (posfixnump(arg)) {
        var uintL radix = posfixnum_to_L(arg);
        if ((2 <= radix) && (radix <= 36))
          return radix;
      }
      # Fehler.
      pushSTACK(arg); # TYPE-ERROR slot DATUM
      pushSTACK(O(type_radix)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: the radix must be an integer between 2 and 36, not ~")
            );
    }

LISPFUN(digit_char_p,1,1,norest,nokey,0,NIL)
# (DIGIT-CHAR-P char [radix]), CLTL S. 236
# Methode:
# Test, ob radix ein Integer >=2 und <=36 ist.
# char muss ein Character <= 'z' sein, sonst NIL als Ergebnis.
# Falls radix<=10: c muss >= '0' und < '0'+radix sein, sonst NIL.
# Falls radix>=10: c muss >= '0' und <= '9' oder
#                  (upcase c) muss >= 'A' und < 'A'-10+radix sein, sonst NIL.
  {
    var uintWL radix = test_radix_arg(); # Zahlbasis, >=2, <=36
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    var chart ch = char_code(arg);
    var cint c = as_cint(ch);
    #ifdef UNICODE
      switch (c >> 8) {
        case 0x00:
          if ((c >= 0x0030) && (c <= 0x0039)) { c -= 0x0030; break; }
          if ((c >= 0x0041) && (c <= 0x005a)) { c -= 0x0037; break; }
          if ((c >= 0x0061) && (c <= 0x007a)) { c -= 0x0057; break; }
          goto no;
        case 0x06:
          if ((c >= 0x0660) && (c <= 0x0669)) { c -= 0x0660; break; }
          if ((c >= 0x06f0) && (c <= 0x06f9)) { c -= 0x06f0; break; }
          goto no;
        case 0x09:
          if ((c >= 0x0966) && (c <= 0x096f)) { c -= 0x0966; break; }
          if ((c >= 0x09e6) && (c <= 0x09ef)) { c -= 0x09e6; break; }
          goto no;
        case 0x0A:
          if ((c >= 0x0a66) && (c <= 0x0a6f)) { c -= 0x0a66; break; }
          if ((c >= 0x0ae6) && (c <= 0x0aef)) { c -= 0x0ae6; break; }
          goto no;
        case 0x0B:
          if ((c >= 0x0b66) && (c <= 0x0b6f)) { c -= 0x0b66; break; }
          if ((c >= 0x0be7) && (c <= 0x0bef)) { c -= 0x0be6; break; }
          goto no;
        case 0x0C:
          if ((c >= 0x0c66) && (c <= 0x0c6f)) { c -= 0x0c66; break; }
          if ((c >= 0x0ce6) && (c <= 0x0cef)) { c -= 0x0ce6; break; }
          goto no;
        case 0x0D:
          if ((c >= 0x0d66) && (c <= 0x0d6f)) { c -= 0x0d66; break; }
          goto no;
        case 0x0E:
          if ((c >= 0x0e50) && (c <= 0x0e59)) { c -= 0x0e50; break; }
          if ((c >= 0x0ed0) && (c <= 0x0ed9)) { c -= 0x0ed0; break; }
          goto no;
        case 0x0F:
          if ((c >= 0x0f20) && (c <= 0x0f29)) { c -= 0x0f20; break; }
          goto no;
        case 0xFF:
          if ((c >= 0xff10) && (c <= 0xff19)) { c -= 0xff10; break; }
          goto no;
        default:
          goto no;
      }
    #else
      if (c > 'z') goto no; # zu groß -> nein
      if (c >= 'a') { c -= 'a'-'A'; } # Character >='a',<='z' in Großbuchstaben wandeln
      # Nun ist $00 <= ch <= $60.
      if (c < '0') goto no;
      # $30 <= c <= $60 in Zahlwert umwandeln:
      if (c <= '9') { c = c - '0'; }
      else if (c >= 'A') { c = c - 'A' + 10; }
      else goto no;
    #endif
    # Nun ist c der Zahlwert der Ziffer, >=0, <=41.
    if (c >= radix) goto no; # nur gültig, falls 0 <= c < radix.
    # Wert als Fixnum zurück:
    value1 = fixnum(c); mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(alphanumericp,1) # (ALPHANUMERICP char), CLTL S. 236
# Alphanumerische Characters sind die Ziffern '0',...,'9' und die
# alphabetischen Characters.
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    if (alphanumericp(char_code(arg)))
      goto yes;
    else
      goto no;
   yes: value1 = T; mv_count=1; return;
   no: value1 = NIL; mv_count=1; return;
  }

# Zeichenvergleichsfunktionen:
# Die Vergleiche CHAR=,... vergleichen das gesamte oint (oder äquivalent,
# nur das cint).
# Die Vergleiche CHAR-EQUAL,... wandeln die Codes in Großbuchstaben um und
# vergleichen diese.

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# Characters sind. Wenn nein, Error.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_char_args (uintC argcount, const object* args_pointer);
  local void test_char_args(argcount,args_pointer)
    var uintC argcount;
    var const object* args_pointer;
    {
      dotimespC(argcount,argcount+1, {
        var object arg = NEXT(args_pointer); # nächstes Argument
        if (!charp(arg)) # muss ein Character sein
          fehler_char(arg);
      });
    }

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# Characters sind. Wenn nein, Error. Streicht von ihnen Bits und Font
# und wandelt sie in Großbuchstaben um.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_char_args_upcase (uintC argcount, object* args_pointer);
  local void test_char_args_upcase(argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    {
      dotimespC(argcount,argcount+1, {
        var object* argptr = &NEXT(args_pointer);
        var object arg = *argptr; # nächstes Argument
        if (!charp(arg)) # muss ein Character sein
          fehler_char(arg);
        *argptr = code_char(up_case(char_code(arg))); # durch Großbuchstaben ersetzen
      });
    }

# UP: (CHAR= char {char}) bei überprüften Argumenten
  local Values char_gleich (uintC argcount, object* args_pointer);
  local Values char_gleich (argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[n].
    # for i:=n-1 to 0 step -1 do ( if Arg[i]/=x then return(NIL) ), return(T).
    {
      var object x = popSTACK(); # letztes Argument nehmen
      dotimesC(argcount,argcount, {
        if (!eq(popSTACK(),x))
          goto no;
      });
     yes: value1 = T; goto ok;
     no: value1 = NIL; goto ok;
     ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR/= char {char}) bei überprüften Argumenten
  local Values char_ungleich (uintC argcount, object* args_pointer);
  local Values char_ungleich (argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for j:=n-1 to 0 step -1 do
    #   x:=Arg[j+1], for i:=j to 0 step -1 do
    #                   if Arg[i]=x then return(NIL),
    # return(T).
    {
      var object* arg_j_ptr = args_end_pointer;
      var uintC j = argcount;
      until (j==0) {
        var object x = BEFORE(arg_j_ptr); # nächst-letztes Argument
        # mit allen Argumenten davor vergleichen:
        var object* arg_i_ptr = arg_j_ptr;
        var uintC i;
        dotimespC(i,j, {
          if (eq(BEFORE(arg_i_ptr),x))
            goto no;
        });
        j--;
      }
     yes: value1 = T; goto ok;
     no: value1 = NIL; goto ok;
     ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR< char {char}) bei überprüften Argumenten
  local Values char_kleiner (uintC argcount, object* args_pointer);
  local Values char_kleiner (argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char<= Arg[i-1] then return(NIL),
    # return(T).
    {
      dotimesC(argcount,argcount, {
        var object x = popSTACK();
        if (as_oint(x) <= as_oint(STACK_0))
          goto no;
      });
     yes: value1 = T; goto ok;
     no: value1 = NIL; goto ok;
     ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR> char {char}) bei überprüften Argumenten
  local Values char_groesser (uintC argcount, object* args_pointer);
  local Values char_groesser (argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char>= Arg[i-1] then return(NIL),
    # return(T).
    {
      dotimesC(argcount,argcount, {
        var object x = popSTACK();
        if (as_oint(x) >= as_oint(STACK_0))
          goto no;
      });
     yes: value1 = T; goto ok;
     no: value1 = NIL; goto ok;
     ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR<= char {char}) bei überprüften Argumenten
  local Values char_klgleich (uintC argcount, object* args_pointer);
  local Values char_klgleich (argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char< Arg[i-1] then return(NIL),
    # return(T).
    {
      dotimesC(argcount,argcount, {
        var object x = popSTACK();
        if (as_oint(x) < as_oint(STACK_0))
          goto no;
      });
     yes: value1 = T; goto ok;
     no: value1 = NIL; goto ok;
     ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR>= char {char}) bei überprüften Argumenten
  local Values char_grgleich (uintC argcount, object* args_pointer);
  local Values char_grgleich (argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char> Arg[i-1] then return(NIL),
    # return(T).
    {
      dotimesC(argcount,argcount, {
        var object x = popSTACK();
        if (as_oint(x) > as_oint(STACK_0))
          goto no;
      });
     yes: value1 = T; goto ok;
     no: value1 = NIL; goto ok;
     ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

LISPFUN(char_gleich,1,0,rest,nokey,0,NIL) # (CHAR= char {char}), CLTL S. 237
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_gleich(argcount,args_pointer);
  }

LISPFUN(char_ungleich,1,0,rest,nokey,0,NIL) # (CHAR/= char {char}), CLTL S. 237
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_ungleich(argcount,args_pointer);
  }

LISPFUN(char_kleiner,1,0,rest,nokey,0,NIL) # (CHAR< char {char}), CLTL S. 237
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_kleiner(argcount,args_pointer);
  }

LISPFUN(char_groesser,1,0,rest,nokey,0,NIL) # (CHAR> char {char}), CLTL S. 237
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_groesser(argcount,args_pointer);
  }

LISPFUN(char_klgleich,1,0,rest,nokey,0,NIL) # (CHAR<= char {char}), CLTL S. 237
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_klgleich(argcount,args_pointer);
  }

LISPFUN(char_grgleich,1,0,rest,nokey,0,NIL) # (CHAR>= char {char}), CLTL S. 237
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_grgleich(argcount,args_pointer);
  }

LISPFUN(char_equal,1,0,rest,nokey,0,NIL) # (CHAR-EQUAL char {char}), CLTL S. 239
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_gleich(argcount,args_pointer);
  }

LISPFUN(char_not_equal,1,0,rest,nokey,0,NIL) # (CHAR-NOT-EQUAL char {char}), CLTL S. 239
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_ungleich(argcount,args_pointer);
  }

LISPFUN(char_lessp,1,0,rest,nokey,0,NIL) # (CHAR-LESSP char {char}), CLTL S. 239
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_kleiner(argcount,args_pointer);
  }

LISPFUN(char_greaterp,1,0,rest,nokey,0,NIL) # (CHAR-GREATERP char {char}), CLTL S. 239
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_groesser(argcount,args_pointer);
  }

LISPFUN(char_not_greaterp,1,0,rest,nokey,0,NIL) # (CHAR-NOT-GREATERP char {char}), CLTL S. 239
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_klgleich(argcount,args_pointer);
  }

LISPFUN(char_not_lessp,1,0,rest,nokey,0,NIL) # (CHAR-NOT-LESSP char {char}), CLTL S. 239
  {
    var object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_grgleich(argcount,args_pointer);
  }

LISPFUNN(char_code,1) # (CHAR-CODE char), CLTL S. 239
  {
    var object arg = popSTACK(); # Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    value1 = fixnum(as_cint(char_code(arg))); # Ascii-Code als Fixnum
    mv_count=1;
  }

LISPFUNN(code_char,1)
# (CODE-CHAR code)
  {
    var object codeobj = popSTACK(); # code-Argument
    if (!integerp(codeobj)) {
      # code-Argument ist kein Integer.
      pushSTACK(codeobj); # TYPE-ERROR slot DATUM
      pushSTACK(S(integer)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(codeobj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: the code argument should be an integer, not ~")
            );
    }
    # codeobj ist jetzt ein Integer.
    var uintL code;
    # Teste, ob  0 <= code < char_code_limit :
    if (posfixnump(codeobj) && ((code = posfixnum_to_L(codeobj)) < char_code_limit)) {
      value1 = code_char(as_chart(code)); mv_count=1; # Character basteln
    } else {
      value1 = NIL; mv_count=1; # sonst Wert NIL
    }
  }

LISPFUNN(character,1) # (CHARACTER object), CLTL S. 241
  {
    var object trial = coerce_char(STACK_0); # Argument in Character umwandeln
    if (nullp(trial)) { # erfolglos?
      # Argument still in STACK_0, TYPE-ERROR slot DATUM
      pushSTACK(O(type_designator_character)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(STACK_1);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: cannot coerce ~ to a character")
            );
    } else {
      value1 = trial; mv_count=1; skipSTACK(1);
    }
  }

LISPFUNN(char_upcase,1) # (CHAR-UPCASE char), CLTL S. 241
  {
    var object arg = popSTACK(); # char-Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    value1 = code_char(up_case(char_code(arg))); # in Großbuchstaben umwandeln
    mv_count=1;
  }

LISPFUNN(char_downcase,1) # (CHAR-DOWNCASE char), CLTL S. 241
  {
    var object arg = popSTACK(); # char-Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    value1 = code_char(down_case(char_code(arg))); # in Kleinbuchstaben umwandeln
    mv_count=1;
  }

LISPFUN(digit_char,1,1,norest,nokey,0,NIL)
# (DIGIT-CHAR weight [radix]), CLTL2 S. 384
  # Methode:
  # Alles müssen Integers sein, radix zwischen 2 und 36.
  # Falls 0 <= weight < radix, konstruiere
  #     ein Character aus '0',...,'9','A',...,'Z' mit Wert weight.
  # Sonst Wert NIL.
  {
    var uintWL radix = test_radix_arg(); # radix-Argument, >=2, <=36
    var object weightobj = popSTACK(); # weight-Argument
    if (!integerp(weightobj)) {
      # weight-Argument ist kein Integer.
      pushSTACK(weightobj); # TYPE-ERROR slot DATUM
      pushSTACK(S(integer)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(weightobj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: the weight argument should be an integer, not ~")
            );
    }
    # weightobj ist jetzt ein Integer.
    # Teste, ob 0<=weight<radix, sonst NIL:
    var uintL weight;
    if (posfixnump(weightobj) && ((weight = posfixnum_to_L(weightobj)) < radix)) {
      weight = weight + '0'; # in Ziffer umwandeln
      if (weight > '9')
        weight += 'A'-'0'-10; # oder Buchstaben draus machen
      value1 = ascii_char(weight); # Character basteln
      mv_count=1;
    } else {
      value1 = NIL; mv_count=1;
    }
  }

LISPFUNN(char_int,1) # (CHAR-INT char), CLTL S. 242
  {
    var object arg = popSTACK(); # char-Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    value1 = fixnum(as_cint(char_code(arg))); mv_count=1;
  }

LISPFUNN(int_char,1) # (INT-CHAR integer), CLTL S. 242
  {
    var object arg = popSTACK(); # integer-Argument
    if (integerp(arg)) {
      # bei 0 <= arg < char_code_limit in Character umwandeln, sonst NIL
      var uintL i;
      if ((posfixnump(arg)) && ((i = posfixnum_to_L(arg)) < char_code_limit)) {
        value1 = code_char(as_chart(i)); mv_count=1;
      } else {
        value1 = NIL; mv_count=1;
      }
    } else {
      # arg kein Integer -> Fehler:
      pushSTACK(arg); # TYPE-ERROR slot DATUM
      pushSTACK(S(integer)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument should be an integer, not ~")
            );
    }
  }

LISPFUNN(char_name,1) # (CHAR-NAME char), CLTL S. 242
  {
    var object arg = popSTACK(); # char-Argument
    if (!charp(arg)) # muss ein Character sein
      fehler_char(arg);
    value1 = char_name(char_code(arg));
    mv_count=1;
  }


# Fehler, wenn Index-Argument kein Integer ist.
  nonreturning_function(local, fehler_int, (object kw, object obj));
  local void fehler_int(kw,obj)
    var object kw;
    var object obj;
    {
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(S(integer)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      if (eq(kw,nullobj)) {
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: index should be an integer, not ~")
              );
      } else {
        pushSTACK(kw); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-index should be an integer, not ~")
              );
      }
    }

# Fehler, wenn Index-Argument kein Integer oder NIL ist.
  nonreturning_function(local, fehler_int_null, (object kw, object obj));
  local void fehler_int_null(kw,obj)
    var object kw;
    var object obj;
    {
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(O(type_end_index)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      if (eq(kw,nullobj)) {
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: index should be NIL or an integer, not ~")
              );
      } else {
        pushSTACK(kw); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-index should be NIL or an integer, not ~")
              );
      }
    }

# Fehler, wenn Index-Argument negativ ist.
  nonreturning_function(local, fehler_posint, (object kw, object obj));
  local void fehler_posint(kw,obj)
    var object kw;
    var object obj;
    {
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      if (eq(kw,nullobj)) {
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: index should not be negative: ~")
              );
      } else {
        pushSTACK(kw); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-index should not be negative: ~")
              );
      }
    }

# Fehler, wenn Index-Argument nicht <= Grenze.
  nonreturning_function(local, fehler_cmp_inclusive, (object kw, object obj, uintL grenze));
  local void fehler_cmp_inclusive(kw,obj,grenze)
    var object kw;
    var object obj;
    var uintL grenze;
    {
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(NIL);
      pushSTACK(obj);
      {
        var object tmp;
        pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(grenze));
        tmp = listof(3);
        STACK_1 = tmp; # TYPE-ERROR slot EXPECTED-TYPE
      }
      if (eq(kw,nullobj)) {
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: index ~ should not be greater than the length of the string")
              );
      } else {
        pushSTACK(kw); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-index ~ should not be greater than the length of the string")
              );
      }
    }

# Fehler, wenn Index-Argument nicht < Grenze.
  nonreturning_function(local, fehler_cmp_exclusive, (object kw, object obj, uintL grenze));
  local void fehler_cmp_exclusive(kw,obj,grenze)
    var object kw;
    var object obj;
    var uintL grenze;
    {
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(NIL);
      pushSTACK(obj);
      {
        var object tmp;
        pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(grenze));
        tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
        STACK_1 = tmp; # TYPE-ERROR slot EXPECTED-TYPE
      }
      if (eq(kw,nullobj)) {
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: index ~ should be less than the length of the string")
              );
      } else {
        pushSTACK(kw); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-index ~ should be less than the length of the string")
              );
      }
    }

# Macro: Überprüft ein Index-Argument
# test_index(woher,wohin_zuweisung,def,default,vergleich,grenze,ucname,lcname)
# woher : expression, woher der Index (als object) kommt.
# wohin_zuweisung : weist das Ergebnis (als uintL) zu.
# def : 0 wenn nicht auf Defaultwerte zu testen ist,
#       1 wenn bei unbound der Default eingesetzt wird,
#       2 wenn bei unbound oder NIL der Default eingesetzt wird.
# default : expression, die als Defaultwert in diesem Falle dient.
# grenze : obere Grenze
# vergleich : Vergleich mit der oberen Grenze
# kw : Keyword, das den Index identifiziert, oder nullobj
  #define test_index(woher,wohin_zuweisung,def,default,vergleich,grenze,kw)  \
    { var object index = woher; # Index-Argument                                \
      if (def && ((eq(index,unbound)) || ((def==2) && (eq(index,NIL)))))        \
        { wohin_zuweisung default; }                                            \
        else                                                                    \
        { # muss ein Integer sein:                                              \
          if (!integerp(index))                                                 \
            { if (def==2) fehler_int_null(kw,index); else fehler_int(kw,index); } \
          # index ist ein Integer.                                              \
          if (!(positivep(index)))                                              \
            { fehler_posint(kw,index); }                                        \
          # index ist >=0.                                                      \
          if (!((posfixnump(index)) &&                                          \
                ((wohin_zuweisung posfixnum_to_L(index)) vergleich grenze)      \
             ) )                                                                \
            { if (0 vergleich 0)                                                \
                # "<= grenze" - Vergleich nicht erfüllt                         \
                { fehler_cmp_inclusive(kw,index,grenze); }                      \
                else                                                            \
                # "< grenze" - Vergleich nicht erfüllt                          \
                { fehler_cmp_exclusive(kw,index,grenze); }                      \
            }                                                                   \
    }   }

# UP: check the index argument for string functions
# > STACK_0: Argument
# > len: length of the strings (< array-total-size-limit)
# > subr_self: caller (ein SUBR)
# < return: index in the string
  local uintL test_index_arg (uintL len);
  local uintL test_index_arg(len)
    var uintL len;
    {
      var uintL i;
      # i := Index STACK_0, no default value, must be <len:
      test_index(STACK_0,i=,0,0,<,len,nullobj);
      return i;
    }

LISPFUNN(char,2) # (CHAR string index), CLTL S. 300
  {
    var object string = STACK_1;
    if (!stringp(string))
      fehler_string(string);
    var uintL len;
    var uintL offset;
    # almost unpack_string_ro() -- but need tot_len, not fil_len
    string = unpack_string(string,&len,NULL,&offset);
    var uintL index = test_index_arg(len);
    var chart ch;
    SstringDispatch(string,
      { ch = TheSstring(string)->data[offset+index]; },
      { ch = as_chart(TheSmallSstring(string)->data[offset+index]); }
      );
    value1 = code_char(ch); mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(schar,2) # (SCHAR string integer), CLTL S. 300
  {
    var object string = STACK_1;
    if (!simple_string_p(string))
      fehler_sstring(string);
    var uintL index = test_index_arg(Sstring_length(string));
    var chart ch;
    SstringDispatch(string,
      { ch = TheSstring(string)->data[index]; },
      { ch = as_chart(TheSmallSstring(string)->data[index]); }
      );
    value1 = code_char(ch); mv_count=1;
    skipSTACK(2);
  }

# UP: Überprüft ein in einen String einzusetzendes Character
# test_newchar_arg()
# > STACK_0: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Argument als Character
# erhöht STACK um 1
  local object test_newchar_arg (void);
  local object test_newchar_arg()
    {
      var object arg = popSTACK(); # Argument
      if (charp(arg)) {
        return arg;
      } else {
        pushSTACK(arg); # TYPE-ERROR slot DATUM
        pushSTACK(S(character)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: argument should be a character, not ~")
              );
      }
    }

LISPFUNN(store_char,3) # (SYSTEM::STORE-CHAR string index newchar)
                       # = (SETF (CHAR string index) newchar), CLTL S. 300
  {
    var object newchar = test_newchar_arg(); # newchar-Argument
    var object string = STACK_1; # string-Argument
    if (!stringp(string)) # muss ein String sein
      fehler_string(string);
    var uintL len;
    # almost unpack_string_rw() -- but need tot_len, not fil_len
    var uintL offset = 0;
    var object unpacked = unpack_string(string,&len,NULL,&offset);
    check_sstring_mutable(unpacked);
    var chart* charptr = &TheSstring(unpacked)->data[offset];
    charptr += test_index_arg(len); # go to the element addressed by index
    *charptr = char_code(newchar); # put in the character
    value1 = newchar; mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(store_schar,3) # (SYSTEM::STORE-SCHAR simple-string index newchar)
                        # = (SETF (SCHAR simple-string index) newchar), CLTL S. 300
  {
    var object newchar = test_newchar_arg(); # newchar-Argument
    var object string = STACK_1; # string-Argument
    if (!simple_string_p(string)) # muss ein Simple-String sein
      fehler_sstring(string);
    check_sstring_mutable(string);
    # zum vom Index angesprochenen Element gehen
    var chart* charptr = &TheSstring(string)->data[0] + test_index_arg(Sstring_length(string));
    *charptr = char_code(newchar); # Character eintragen
    value1 = newchar; mv_count=1;
    skipSTACK(2);
  }

# UP: Überprüft die Grenzen für ein String-Argument
# test_string_limits_ro(&arg)  [for read-only access]
# > STACK_2: String-Argument
# > STACK_1: optionales :start-Argument
# > STACK_0: optionales :end-Argument
# > subr_self: Aufrufer (ein SUBR)
# < stringarg arg: description of the argument
# < result: String-Argument
# erhöht STACK um 3
  global object test_string_limits_ro (stringarg* arg);
  global object test_string_limits_ro(arg)
    var stringarg* arg;
    {
      var uintL len;
      var uintL start;
      var uintL end;
      # String-Argument überprüfen:
      var object string = STACK_2;
      if (!stringp(string))
        fehler_string(string);
      arg->string = unpack_string_ro(string,&len,&arg->offset);
      # Nun ist len die Länge (<2^oint_data_len).
      # :START-Argument überprüfen:
        # start := Index STACK_1, Defaultwert 0, muss <=len sein:
        test_index(STACK_1,start=,1,0,<=,len,S(Kstart));
      # start ist jetzt der Wert des :START-Arguments.
      # :END-Argument überprüfen:
        # end := Index STACK_0, Defaultwert len, muss <=len sein:
        test_index(STACK_0,end=,2,len,<=,len,S(Kend));
      # end ist jetzt der Wert des :END-Arguments.
      # Vergleiche :START und :END Argumente:
      if (!(start <= end)) {
        pushSTACK(STACK_0); # :END-Index
        pushSTACK(STACK_2); # :START-Index
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: :start-index ~ must not be greater than :end-index ~")
              );
      }
      skipSTACK(3);
      # Ergebnisse herausgeben:
      arg->index = start; arg->len = end-start;
      return string;
    }

# UP: Überprüft die Grenzen für ein String-Argument
# test_string_limits_rw(&arg)  [for read-write access]
# > STACK_2: String-Argument
# > STACK_1: optionales :start-Argument
# > STACK_0: optionales :end-Argument
# > subr_self: Aufrufer (ein SUBR)
# < stringarg arg: description of the argument
# < result: String-Argument
# erhöht STACK um 3
#ifdef TYPECODES
  #define test_string_limits_rw(arg)  test_string_limits_ro(arg)
#else
  local object test_string_limits_rw (stringarg* arg);
  local object test_string_limits_rw(arg)
    var stringarg* arg;
    {
      var object string = test_string_limits_ro(arg);
      if (arg->len > 0)
        if (!(Record_type(arg->string) == Rectype_Sstring))
          fehler_sstring_immutable(string);
      return string;
    }
#endif

# UP: Überprüft ein String/Symbol/Character-Argument
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Argument als String
# can trigger GC
  local object test_stringsymchar_arg (object obj);
  local object test_stringsymchar_arg(obj)
    var object obj;
    {
      if (stringp(obj)) # String: unverändert zurück
        return obj;
      if (symbolp(obj)) # Symbol: Printnamen verwenden
        return TheSymbol(obj)->pname;
      if (charp(obj)) { # Character: einelementigen String daraus machen:
        var object new_string = allocate_string(1);
        TheSstring(new_string)->data[0] = char_code(obj);
        return new_string;
      }
      pushSTACK(obj); # TYPE-ERROR slot DATUM
      pushSTACK(O(type_stringsymchar)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ should be a string, a symbol or a character")
            );
    }

# UP: Überprüft die Grenzen für 1 String/Symbol-Argument und kopiert es
# test_1_stringsym_limits(&string,&len)
# > STACK_2: String/Symbol-Argument
# > STACK_1: optionales :start-Argument
# > STACK_0: optionales :end-Argument
# > subr_self: Aufrufer (ein SUBR)
# < object string: Kopie des Strings
# < uintL len: Anzahl der angesprochenen Characters
# < chart* ergebnis: Ab hier kommen die angesprochenen Characters
# erhöht STACK um 3
# can trigger GC
  local chart* test_1_stringsym_limits (object* string_, uintL* len_);
  local chart* test_1_stringsym_limits(string_,len_)
    var object* string_;
    var uintL* len_;
    {
      var object string;
      var uintL len;
      var uintL start;
      var uintL end;
      # String/Symbol-Argument überprüfen:
      string = test_stringsymchar_arg(STACK_2);
      len = vector_length(string);
      # Nun ist len die Länge (<2^oint_data_len).
      # :START-Argument überprüfen:
        # start := Index STACK_1, Defaultwert 0, muss <=len sein:
        test_index(STACK_1,start=,1,0,<=,len,S(Kstart));
      # start ist jetzt der Wert des :START-Arguments.
      # :END-Argument überprüfen:
        # end := Index STACK_0, Defaultwert len, muss <=len sein:
        test_index(STACK_0,end=,2,len,<=,len,S(Kend));
      # end ist jetzt der Wert des :END-Arguments.
      # Vergleiche :START und :END Argumente:
      if (!(start <= end)) {
        pushSTACK(STACK_0); # :END-Index
        pushSTACK(STACK_2); # :START-Index
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: :start-index ~ must not be greater than :end-index ~")
              );
      }
      skipSTACK(3);
      # String kopieren und Ergebnisse herausgeben:
      *string_ = string = copy_string(string); # String kopieren
      *len_ = end-start; return &TheSstring(string)->data[start];
    }

# UP: Überprüft die Grenzen für 2 String/Symbol-Argumente
# test_2_stringsym_limits(&arg1,&arg2)
# > STACK_5: String/Symbol-Argument1
# > STACK_4: String/Symbol-Argument2
# > STACK_3: optionales :start1-Argument
# > STACK_2: optionales :end1-Argument
# > STACK_1: optionales :start2-Argument
# > STACK_0: optionales :end2-Argument
# > subr_self: Aufrufer (ein SUBR)
# < stringarg arg1: description of argument1
# < stringarg arg2: description of argument2
# erhöht STACK um 6
  local void test_2_stringsym_limits (stringarg* arg1, stringarg* arg2);
  local void test_2_stringsym_limits(arg1,arg2)
    var stringarg* arg1;
    var stringarg* arg2;
    {
      var uintL len1;
      var uintL len2;
      {
        # String/Symbol-Argument1 überprüfen:
        var object string1 = test_stringsymchar_arg(STACK_5);
        pushSTACK(string1); # string1 retten
        # String/Symbol-Argument2 überprüfen:
        var object string2 = test_stringsymchar_arg(STACK_(4+1));
        arg2->string = unpack_string_ro(string2,&len2,&arg2->offset);
        # Nun ist len2 die Länge (<2^oint_data_len) von string2.
        string1 = popSTACK(); # string1 zurück
        arg1->string = unpack_string_ro(string1,&len1,&arg1->offset);
        # Nun ist len1 die Länge (<2^oint_data_len) von string1.
      }
      # :START1 und :END1 überprüfen:
      {
        var uintL start1;
        var uintL end1;
        # :START1-Argument überprüfen:
          # start1 := Index STACK_3, Defaultwert 0, muss <=len1 sein:
          test_index(STACK_3,start1=,1,0,<=,len1,S(Kstart1));
        # start1 ist jetzt der Wert des :START1-Arguments.
        # :END1-Argument überprüfen:
          # end1 := Index STACK_2, Defaultwert len1, muss <=len1 sein:
          test_index(STACK_2,end1=,2,len1,<=,len1,S(Kend1));
        # end1 ist jetzt der Wert des :END1-Arguments.
        # Vergleiche :START1 und :END1 Argumente:
        if (!(start1 <= end1)) {
          pushSTACK(STACK_2); # :END1-Index
          pushSTACK(STACK_4); # :START1-Index
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: :start1-index ~ must not be greater than :end1-index ~")
                );
        }
        # Ergebnisse zu string1 herausgeben:
        arg1->index = start1; arg1->len = end1-start1;
      }
      # :START2 und :END2 überprüfen:
      {
        var uintL start2;
        var uintL end2;
        # :START2-Argument überprüfen:
          # start2 := Index STACK_1, Defaultwert 0, muss <=len2 sein:
          test_index(STACK_1,start2=,1,0,<=,len2,S(Kstart2));
        # start2 ist jetzt der Wert des :START2-Arguments.
        # :END2-Argument überprüfen:
          # end2 := Index STACK_0, Defaultwert len2, muss <=len2 sein:
          test_index(STACK_0,end2=,2,len2,<=,len2,S(Kend2));
        # end2 ist jetzt der Wert des :END2-Arguments.
        # Vergleiche :START2 und :END2 Argumente:
        if (!(start2 <= end2)) {
          pushSTACK(STACK_0); # :END2-Index
          pushSTACK(STACK_2); # :START2-Index
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: :start2-index ~ must not be greater than :end2-index ~")
                );
        }
        # Ergebnisse zu string2 herausgeben:
        arg2->index = start2; arg2->len = end2-start2;
        # Fertig.
        skipSTACK(6);
      }
    }

# UP: vergleicht zwei gleichlange Strings auf Gleichheit
# > string1,offset1: Ab hier kommen die angesprochenen Characters im String1
# > string2,offset2: Ab hier kommen die angesprochenen Characters im String2
# > len: Anzahl der angesprochenen Characters in String1 und in String2, > 0
# < ergebnis: true falls gleich, false sonst.
  global bool string_eqcomp (object string1, uintL offset1, object string2, uintL offset2, uintL len);
  global bool string_eqcomp(string1,offset1,string2,offset2,len)
    var object string1;
    var uintL offset1;
    var object string2;
    var uintL offset2;
    var uintL len;
    {
      SstringDispatch(string1,
        { var const chart* charptr1 = &TheSstring(string1)->data[offset1];
          SstringDispatch(string2,
            { var const chart* charptr2 = &TheSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(*charptr1++,*charptr2++))
                  goto no;
              });
            },
            { var const scint* charptr2 = &TheSmallSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(*charptr1++,as_chart(*charptr2++)))
                  goto no;
              });
            }
            );
        },
        { var const scint* charptr1 = &TheSmallSstring(string1)->data[offset1];
          SstringDispatch(string2,
            { var const chart* charptr2 = &TheSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(as_chart(*charptr1++),*charptr2++))
                  goto no;
              });
            },
            { var const scint* charptr2 = &TheSmallSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(as_chart(*charptr1++),as_chart(*charptr2++)))
                  goto no;
              });
            }
            );
        }
        );
      return true;
     no: return false;
    }

# UP: vergleicht zwei Strings
# > arg1: Ab hier kommen die angesprochenen Characters im String1
# > arg2: Ab hier kommen die angesprochenen Characters im String2
# < arg1.index: Stelle des ersten Unterschieds im String1
# < ergebnis: 0 falls gleich,
#             -1 falls String1 echt vor String2 kommt,
#             +1 falls String1 echt nach String2 kommt.
  local signean string_comp (stringarg* arg1, const stringarg* arg2);
  local signean string_comp(arg1,arg2)
    var stringarg* arg1;
    var const stringarg* arg2;
    {
      var uintL len1 = arg1->len;
      var uintL len2 = arg2->len;
      SstringDispatch(arg1->string,
        { var const chart* charptr1_0 = &TheSstring(arg1->string)->data[arg1->offset];
          var const chart* charptr1 = &charptr1_0[arg1->index];
          SstringDispatch(arg2->string,
            { var const chart* charptr2 = &TheSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto A_string1_end;
                if (len2==0) goto A_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(*charptr1++,*charptr2++)) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
              # zwei verschiedene Characters gefunden
              arg1->index = --charptr1 - charptr1_0;
              if (charlt(*charptr1,*--charptr2))
                return signean_minus; # String1 < String2
              else
                return signean_plus; # String1 > String2
            },
            { var const scint* charptr2 = &TheSmallSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto A_string1_end;
                if (len2==0) goto A_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(*charptr1++,as_chart(*charptr2++))) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
              # zwei verschiedene Characters gefunden
              arg1->index = --charptr1 - charptr1_0;
              if (charlt(*charptr1,as_chart(*--charptr2)))
                return signean_minus; # String1 < String2
              else
                return signean_plus; # String1 > String2
            }
            );
         A_string1_end: # String1 zu Ende
          arg1->index = charptr1 - charptr1_0;
          if (len2==0)
            return signean_null; # String1 = String2
          else
            return signean_minus; # String1 ist echtes Anfangsstück von String2
         A_string2_end: # String2 zu Ende, String1 noch nicht
          arg1->index = charptr1 - charptr1_0;
          return signean_plus; # String2 ist echtes Anfangsstück von String1
        },
        { var const scint* charptr1_0 = &TheSmallSstring(arg1->string)->data[arg1->offset];
          var const scint* charptr1 = &charptr1_0[arg1->index];
          SstringDispatch(arg2->string,
            { var const chart* charptr2 = &TheSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto B_string1_end;
                if (len2==0) goto B_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(as_chart(*charptr1++),*charptr2++)) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
              # zwei verschiedene Characters gefunden
              arg1->index = --charptr1 - charptr1_0;
              if (charlt(as_chart(*charptr1),*--charptr2))
                return signean_minus; # String1 < String2
              else
                return signean_plus; # String1 > String2
            },
            { var const scint* charptr2 = &TheSmallSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto B_string1_end;
                if (len2==0) goto B_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(as_chart(*charptr1++),as_chart(*charptr2++))) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
              # zwei verschiedene Characters gefunden
              arg1->index = --charptr1 - charptr1_0;
              if (charlt(as_chart(*charptr1),as_chart(*--charptr2)))
                return signean_minus; # String1 < String2
              else
                return signean_plus; # String1 > String2
            }
            );
         B_string1_end: # String1 zu Ende
          arg1->index = charptr1 - charptr1_0;
          if (len2==0)
            return signean_null; # String1 = String2
          else
            return signean_minus; # String1 ist echtes Anfangsstück von String2
         B_string2_end: # String2 zu Ende, String1 noch nicht
          arg1->index = charptr1 - charptr1_0;
          return signean_plus; # String2 ist echtes Anfangsstück von String1
        }
        );
    }

LISPFUN(string_gleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 300
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (((arg1.len==arg2.len)
               && ((arg1.len==0)
                   || string_eqcomp(arg1.string,arg1.offset+arg1.index,
                                    arg2.string,arg2.offset+arg2.index,
                                    arg1.len
                                   )
              )   )
              ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_ungleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING/= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp(&arg1,&arg2)==0 ? NIL : fixnum(arg1.index));
    mv_count=1;
  }

LISPFUN(string_kleiner,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING< string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp(&arg1,&arg2)<0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

LISPFUN(string_groesser,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING> string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp(&arg1,&arg2)>0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

LISPFUN(string_klgleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING<= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp(&arg1,&arg2)<=0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

LISPFUN(string_grgleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING>= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp(&arg1,&arg2)>=0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

# UP: vergleicht zwei gleichlange Strings auf Gleichheit, case-insensitive
# > string1,offset1: Ab hier kommen die angesprochenen Characters im String1
# > string2,offset2: Ab hier kommen die angesprochenen Characters im String2
# > len: Anzahl der angesprochenen Characters in String1 und in String2, > 0
# < ergebnis: true falls gleich, false sonst.
  global bool string_eqcomp_ci (object string1, uintL offset1, object string2, uintL offset2, uintL len);
  global bool string_eqcomp_ci(string1,offset1,string2,offset2,len)
    var object string1;
    var uintL offset1;
    var object string2;
    var uintL offset2;
    var uintL len;
    { SstringDispatch(string1,
        { var const chart* charptr1 = &TheSstring(string1)->data[offset1];
          SstringDispatch(string2,
            { var const chart* charptr2 = &TheSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(up_case(*charptr1++),up_case(*charptr2++)))
                  goto no;
              });
            },
            { var const scint* charptr2 = &TheSmallSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(up_case(*charptr1++),up_case(as_chart(*charptr2++))))
                  goto no;
              });
            }
            );
        },
        { var const scint* charptr1 = &TheSmallSstring(string1)->data[offset1];
          SstringDispatch(string2,
            { var const chart* charptr2 = &TheSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(up_case(as_chart(*charptr1++)),up_case(*charptr2++)))
                  goto no;
              });
            },
            { var const scint* charptr2 = &TheSmallSstring(string2)->data[offset2];
              dotimespL(len,len, {
                if (!chareq(up_case(as_chart(*charptr1++)),up_case(as_chart(*charptr2++))))
                  goto no;
              });
            }
            );
        }
        );
      return true;
     no: return false;
    }

# UP: vergleicht zwei Strings, case-insensitive
# > arg1: Ab hier kommen die angesprochenen Characters im String1
# > arg2: Ab hier kommen die angesprochenen Characters im String2
# < arg1.index: Stelle des ersten Unterschieds im String1
# < ergebnis: 0 falls gleich,
#             -1 falls String1 echt vor String2 kommt,
#             +1 falls String1 echt nach String2 kommt.
  local signean string_comp_ci (stringarg* arg1, const stringarg* arg2);
  local signean string_comp_ci(arg1,arg2)
    var stringarg* arg1;
    var const stringarg* arg2;
    {
      var uintL len1 = arg1->len;
      var uintL len2 = arg2->len;
      SstringDispatch(arg1->string,
        { var const chart* charptr1_0 = &TheSstring(arg1->string)->data[arg1->offset];
          var const chart* charptr1 = &charptr1_0[arg1->index];
          var chart ch1;
          var chart ch2;
          SstringDispatch(arg2->string,
            { var const chart* charptr2 = &TheSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto A_string1_end;
                if (len2==0) goto A_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(ch1 = up_case(*charptr1++), ch2 = up_case(*charptr2++))) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
            },
            { var const scint* charptr2 = &TheSmallSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto A_string1_end;
                if (len2==0) goto A_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(ch1 = up_case(*charptr1++), ch2 = up_case(as_chart(*charptr2++)))) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
            }
            );
          # zwei verschiedene Characters gefunden
          arg1->index = --charptr1 - charptr1_0;
          if (charlt(ch1,ch2))
            return signean_minus; # String1 < String2
          else
            return signean_plus; # String1 > String2
         A_string1_end: # String1 zu Ende
          arg1->index = charptr1 - charptr1_0;
          if (len2==0)
            return signean_null; # String1 = String2
          else
            return signean_minus; # String1 ist echtes Anfangsstück von String2
         A_string2_end: # String2 zu Ende, String1 noch nicht
          arg1->index = charptr1 - charptr1_0;
          return signean_plus; # String2 ist echtes Anfangsstück von String1
        },
        { var const scint* charptr1_0 = &TheSmallSstring(arg1->string)->data[arg1->offset];
          var const scint* charptr1 = &charptr1_0[arg1->index];
          var chart ch1;
          var chart ch2;
          SstringDispatch(arg2->string,
            { var const chart* charptr2 = &TheSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto B_string1_end;
                if (len2==0) goto B_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(ch1 = up_case(as_chart(*charptr1++)), ch2 = up_case(*charptr2++))) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
            },
            { var const scint* charptr2 = &TheSmallSstring(arg2->string)->data[arg2->offset+arg2->index];
              loop {
                # einer der Strings zu Ende ?
                if (len1==0) goto B_string1_end;
                if (len2==0) goto B_string2_end;
                # nächste Characters vergleichen:
                if (!chareq(ch1 = up_case(as_chart(*charptr1++)), ch2 = up_case(as_chart(*charptr2++)))) break;
                # beide Zähler erniedrigen:
                len1--; len2--;
              }
            }
            );
          # zwei verschiedene Characters gefunden
          arg1->index = --charptr1 - charptr1_0;
          if (charlt(ch1,ch2))
            return signean_minus; # String1 < String2
          else
            return signean_plus; # String1 > String2
         B_string1_end: # String1 zu Ende
          arg1->index = charptr1 - charptr1_0;
          if (len2==0)
            return signean_null; # String1 = String2
          else
            return signean_minus; # String1 ist echtes Anfangsstück von String2
         B_string2_end: # String2 zu Ende, String1 noch nicht
          arg1->index = charptr1 - charptr1_0;
          return signean_plus; # String2 ist echtes Anfangsstück von String1
        }
        );
    }

LISPFUN(string_equal,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-EQUAL string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (((arg1.len==arg2.len)
               && ((arg1.len==0)
                   || string_eqcomp_ci(arg1.string,arg1.offset+arg1.index,
                                       arg2.string,arg2.offset+arg2.index,
                                       arg1.len
                                      )
              )   )
              ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_not_equal,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-NOT-EQUAL string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp_ci(&arg1,&arg2)==0 ? NIL : fixnum(arg1.index));
    mv_count=1;
  }

LISPFUN(string_lessp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-LESSP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp_ci(&arg1,&arg2)<0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

LISPFUN(string_greaterp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-GREATERP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp_ci(&arg1,&arg2)>0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

LISPFUN(string_not_greaterp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-NOT-GREATERP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp_ci(&arg1,&arg2)<=0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

LISPFUN(string_not_lessp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-NOT-LESSP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # vergleichen:
    value1 = (string_comp_ci(&arg1,&arg2)>=0 ? fixnum(arg1.index) : NIL);
    mv_count=1;
  }

# UP: sucht einen String String1 in einem anderen String String2
# > arg1: Ab hier kommen die angesprochenen Characters im String1
# > arg2: Ab hier kommen die angesprochenen Characters im String2
# > eqcomp: Vergleichsfunktion, &string_eqcomp oder &string_eqcomp_ci
# < ergebnis: NIL falls nicht gefunden,
#             Position im String2 (als Fixnum) falls gefunden.
  # eqcomp_fun sei der Typ einer solchen Vergleichsfunktion:
  typedef bool (*eqcomp_fun) (object string1, uintL offset1, object string2, uintL offset2, uintL len);
  local object string_search (const stringarg* arg1, const stringarg* arg2, eqcomp_fun eqcomp);
  local object string_search(arg1,arg2,eqcomp)
    var const stringarg* arg1;
    var const stringarg* arg2;
    var eqcomp_fun eqcomp;
    {
      var uintL len1 = arg1->len;
      var uintL len2 = arg2->len;
      if (len1 > len2) goto notfound; # Nur bei len1<=len2 kann String1 in String2 vorkommen.
      # Schleife:
      # for i=0..len2-len1:
      #   vergleiche String1 mit den len1 Characters ab charptr2[i].
      # Dazu Schleife len2-len1+1 mal durchlaufen, charptr2 und start2 wachsen.
      {
        var object string1 = arg1->string;
        var uintL offset1 = arg1->offset + arg1->index;
        var object string2 = arg2->string;
        var uintL offset2 = arg2->offset + arg2->index;
        var uintL count;
        if (len1==0) goto found;
        dotimespL(count,len2-len1+1, {
          if ((*eqcomp)(string1,offset1,string2,offset2,len1)) # vergleichen
            goto found;
          offset2++;
        });
        goto notfound;
       found: return fixnum(offset2 - arg2->offset);
      }
     notfound: return NIL;
    }

LISPFUN(search_string_gleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (SYS::SEARCH-STRING= string1 string2 [:start1] [:end1] [:start2] [:end2])
# = (search string1 string2 :test #'char= [:start1] [:end1] [:start2] [:end2])
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # String1 in String2 suchen:
    value1 = string_search(&arg1,&arg2,&string_eqcomp);
    mv_count=1;
  }

LISPFUN(search_string_equal,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (SYS::SEARCH-STRING-EQUAL string1 string2 [:start1] [:end1] [:start2] [:end2])
# = (search string1 string2 :test #'char-equal [:start1] [:end1] [:start2] [:end2])
  {
    var stringarg arg1;
    var stringarg arg2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&arg1,&arg2);
    # String1 in String2 suchen:
    value1 = string_search(&arg1,&arg2,&string_eqcomp_ci);
    mv_count=1;
  }

LISPFUN(make_string,1,0,norest,key,2, (kw(initial_element),kw(element_type)) )
# (MAKE-STRING size :initial-element :element-type)
  {
    var uintL size;
    # size überprüfen:
    if (!posfixnump(STACK_2)) { # size muss Fixnum >= 0 sein
      pushSTACK(STACK_2); # TYPE-ERROR slot DATUM
      pushSTACK(O(type_posfixnum)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(STACK_(2+2)); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: the string length ~ should be nonnegative fixnum")
            );
    }
    size = posfixnum_to_L(STACK_2);
    # element-type überprüfen:
    if (!eq(STACK_0,unbound)) {
      var object eltype = STACK_0;
      if (!eq(eltype,S(character))) {
        # Verify (SUBTYPEP eltype 'CHARACTER):
        pushSTACK(eltype); pushSTACK(S(character)); funcall(S(subtypep),2);
        if (nullp(value1)) {
          pushSTACK(STACK_0); # eltype
          pushSTACK(S(character)); # CHARACTER
          pushSTACK(S(Kelement_type)); # :ELEMENT-TYPE
          pushSTACK(S(make_string));
          fehler(error,
                 GETTEXT("~: ~ argument must be a subtype of ~, not ~")
                );
        }
      }
    }
    var object new_string = allocate_string(size); # neuen String besorgen
    # evtl. mit initial-element füllen:
    var object initial_element = STACK_1;
    if (eq(initial_element,unbound)) {
      # nicht angegeben -> nichts zu tun
    } else {
      if (!charp(initial_element)) { # sonst: muss ein Character sein
        pushSTACK(initial_element); # TYPE-ERROR slot DATUM
        pushSTACK(S(character)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(initial_element); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: :initial-element ~ should be of type character")
              );
      } else {
        var chart ch = char_code(initial_element);
        # String mit ch vollschreiben:
        if (!(size==0)) {
          var chart* charptr = &TheSstring(new_string)->data[0];
          dotimespL(size,size, { *charptr++ = ch; } );
        }
      }
    }
    value1 = new_string; mv_count=1; skipSTACK(3);
  }

LISPFUNN(string_both_trim,3)
# (SYS::STRING-BOTH-TRIM character-bag-left character-bag-right string)
# Grundfunktion für
# STRING-TRIM, STRING-LEFT-TRIM, STRING-RIGHT-TRIM, CLTL S. 302
# Methode:
# (let ((l (length string)))
#   (do ((i 0 (1+ i)))
#       (nil)
#     (when (or (= i l)
#               (not (find (char string i) character-bag-left))
#           )
#       (do ((j l (1- j)))
#           (nil)
#         (when (or (= i j)
#                   (not (find (char string (1- j)) character-bag-right))
#               )
#           (return (if (and (= i 0) (= j l)) string (substring string i j)))
# ) ) ) ) )
  {
    var object string = test_stringsymchar_arg(popSTACK()); # Argument in String umwandeln
    pushSTACK(string); # und wieder in den Stack
    pushSTACK(fixnum(vector_length(string))); # Länge als Fixnum in den Stack
    pushSTACK(Fixnum_0); # i := 0
    # Stackaufbau: bag-left, bag-right, string, l, i
    loop {
      if (eq(STACK_0,STACK_1)) break; # bei i = l (beides Fixnums): Schleife fertig
      # (char string i) bestimmen:
      pushSTACK(STACK_2); pushSTACK(STACK_1); funcall(L(char),2);
      # (find (char ...) character-bag-left) bestimmen:
      pushSTACK(value1); pushSTACK(STACK_5); funcall(L(find),2);
      if (nullp(value1)) break; # char nicht in character-bag-left -> Schleife fertig
      STACK_0 = fixnum_inc(STACK_0,1); # i := (1+ i)
    }
    pushSTACK(STACK_1); # j := l
    # Stackaufbau: bag-left, bag-right, string, l, i, j
    loop {
      if (eq(STACK_0,STACK_1)) break; # bei j = i (beides Fixnums): Schleife fertig
      # (char string (1- j)) bestimmen:
      pushSTACK(STACK_3); pushSTACK(fixnum_inc(STACK_1,-1)); funcall(L(char),2);
      # (find (char ...) character-bag-right) bestimmen:
      pushSTACK(value1); pushSTACK(STACK_5); funcall(L(find),2);
      if (nullp(value1)) break; # char nicht in character-bag-right -> Schleife fertig
      STACK_0 = fixnum_inc(STACK_0,-1); # j := (1- j)
    }
    # Stackaufbau: bag-left, bag-right, string, l, i, j
    # Die Zeichen mit Index <i oder >=j des Strings wegwerfen:
    var object j = popSTACK();
    var object i = popSTACK();
    var object l = popSTACK();
    string = popSTACK();
    skipSTACK(2);
    if (eq(i,Fixnum_0) && eq(j,l)) {
      value1 = string; # bei i=0 und j=l ist nichts zu tun, string als Wert
    } else {
      # Teilstück der Indizes >=i, <j herauskopieren:
      # (substring string i j) als Wert
      pushSTACK(string); pushSTACK(i); pushSTACK(j); funcall(L(substring),3);
    }
    mv_count=1;
  }

LISPFUN(string_width,1,0,norest,key,2, (kw(start),kw(end)) )
  {
    var stringarg arg;
    var object string = test_string_limits_ro(&arg);
    var uintL width = 0;
    var uintL len = arg.len;
    if (len > 0) {
      SstringDispatch(string,
        {
          var const chart* charptr = &TheSstring(arg.string)->data[arg.offset];
          dotimespL(len,len, {
            width += char_width(*charptr); charptr++;
          });
        },
        {
          var const uintB* charptr = &TheSmallSstring(arg.string)->data[arg.offset];
          dotimespL(len,len, {
            width += char_width(as_chart(*charptr)); charptr++;
          });
        }
        );
    }
    # width <= 2*arg.len.
    value1 = UL_to_I(width); mv_count=1;
  }

# UP: wandelt die Characters eines Stringstücks in Großbuchstaben
# nstring_upcase(charptr,len);
# > chart* charptr: Ab hier kommen die angesprochenen Characters
# > uintL len: Anzahl der angesprochenen Characters
  global void nstring_upcase (chart* charptr, uintL len);
  global void nstring_upcase(charptr,len)
    var chart* charptr;
    var uintL len;
    {
      dotimesL(len,len, {
        *charptr = up_case(*charptr); charptr++;
      });
    }

# UP: wandelt einen String in Großbuchstaben
# string_upcase(string)
# > string: String
# < ergebnis: neuer Normal-Simple-String, in Großbuchstaben
# can trigger GC
  global object string_upcase (object string);
  global object string_upcase(string)
    var object string;
    {
      string = copy_string(string); # kopieren und dabei zum Normal-Simple-String machen
      nstring_upcase(&TheSstring(string)->data[0],Sstring_length(string)); # umwandeln
      return string;
    }

LISPFUN(nstring_upcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (NSTRING-UPCASE string :start :end), CLTL S. 304
  {
    var stringarg arg;
    var object string = test_string_limits_rw(&arg);
    nstring_upcase(&TheSstring(arg.string)->data[arg.offset+arg.index],arg.len);
    value1 = string; mv_count=1;
  }

LISPFUN(string_upcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (STRING-UPCASE string :start :end), CLTL S. 303
  {
    var object string;
    var uintL len;
    var chart* charptr = test_1_stringsym_limits(&string,&len);
    nstring_upcase(charptr,len);
    value1 = string; mv_count=1;
  }

# UP: wandelt die Characters eines Stringstücks in Kleinbuchstaben
# nstring_downcase(charptr,len);
# > chart* charptr: Ab hier kommen die angesprochenen Characters
# > uintL len: Anzahl der angesprochenen Characters
  global void nstring_downcase (chart* charptr, uintL len);
  global void nstring_downcase(charptr,len)
    var chart* charptr;
    var uintL len;
    {
      dotimesL(len,len, {
        *charptr = down_case(*charptr); charptr++;
      });
    }

# UP: wandelt einen String in Kleinbuchstaben
# string_downcase(string)
# > string: String
# < ergebnis: neuer Normal-Simple-String, in Kleinbuchstaben
# can trigger GC
  global object string_downcase (object string);
  global object string_downcase(string)
    var object string;
    {
      string = copy_string(string); # kopieren und dabei zum Normal-Simple-String machen
      nstring_downcase(&TheSstring(string)->data[0],Sstring_length(string)); # umwandeln
      return string;
    }

LISPFUN(nstring_downcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (NSTRING-DOWNCASE string :start :end), CLTL S. 304
  {
    var stringarg arg;
    var object string = test_string_limits_rw(&arg);
    nstring_downcase(&TheSstring(arg.string)->data[arg.offset+arg.index],arg.len);
    value1 = string; mv_count=1;
  }

LISPFUN(string_downcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (STRING-DOWNCASE string :start :end), CLTL S. 303
  {
    var object string;
    var uintL len;
    var chart* charptr = test_1_stringsym_limits(&string,&len);
    nstring_downcase(charptr,len);
    value1 = string; mv_count=1;
  }

# UP: wandelt die Worte eines Stringstücks in solche, die
# mit Großbuchstaben anfangen und mit Kleinbuchstaben weitergehen.
# nstring_capitalize(charptr,len);
# > chart* charptr: Ab hier kommen die angesprochenen Characters
# > uintL len: Anzahl der angesprochenen Characters
  global void nstring_capitalize (chart* charptr, uintL len);
  # Methode:
  # Jeweils abwechselnd nach Wortanfang suchen (und nichts umwandeln)
  # bzw. nach Wortende suchen (und dabei umwandeln).
  global void nstring_capitalize(charptr,len)
    var chart* charptr;
    var uintL len;
    {
      # Suche den nächsten Wortanfang:
     suche_wortanfang:
      until (len==0) {
        if (alphanumericp(*charptr))
          goto wortanfang;
        charptr++; len--;
      }
      return; # len=0 -> String zu Ende
      # Wortanfang gefunden
     wortanfang:
      *charptr = up_case(*charptr); # Zeichen in Großbuchstaben umwandeln
      charptr++;
      # Suche das Wortende:
      until (--len==0) {
        # mitten im Wort
        if (!(alphanumericp(*charptr)))
          goto suche_wortanfang;
        *charptr = down_case(*charptr); # Zeichen in Kleinbuchstaben umwandeln
        charptr++;
      }
      return; # len=0 -> String zu Ende
    }

LISPFUN(nstring_capitalize,1,0,norest,key,2, (kw(start),kw(end)) )
# (NSTRING-CAPITALIZE string :start :end), CLTL S. 304
  {
    var stringarg arg;
    var object string = test_string_limits_rw(&arg);
    nstring_capitalize(&TheSstring(arg.string)->data[arg.offset+arg.index],arg.len);
    value1 = string; mv_count=1;
  }

LISPFUN(string_capitalize,1,0,norest,key,2, (kw(start),kw(end)) )
# (STRING-CAPITALIZE string :start :end), CLTL S. 303
  {
    var object string;
    var uintL len;
    var chart* charptr = test_1_stringsym_limits(&string,&len);
    nstring_capitalize(charptr,len);
    value1 = string; mv_count=1;
  }

LISPFUNN(string,1) # (STRING object), CLTL S. 304
  {
    value1 = test_stringsymchar_arg(popSTACK()); mv_count=1;
  }

LISPFUNN(name_char,1) # (NAME-CHAR name), CLTL S. 243
  {
    # Argument in einen String umwandeln, Character mit diesem Namen suchen:
    value1 = name_char(test_stringsymchar_arg(popSTACK()));
    mv_count=1;
  }

# Returns a substring of a simple-string.
# subsstring(string,start,end)
# > object string: a simple-string
# > uintL start: start index
# > uintL end: end index
# with 0 <= start <= end <= Sstring_length(string)
# < object result: (subseq string start end), a freshly created normal-simple-string
  global object subsstring (object string, uintL start, uintL end);
  global object subsstring(string,start,end)
    var object string;
    var uintL start;
    var uintL end;
    {
      var uintL count = end - start;
      pushSTACK(string);
      var object new_string = allocate_string(count);
      string = popSTACK();
      if (count > 0) {
        SstringDispatch(string,
          { chartcopy(&TheSstring(string)->data[start],&TheSstring(new_string)->data[0],count); },
          { scintcopy(&TheSmallSstring(string)->data[start],&TheSstring(new_string)->data[0],count); }
          );
      }
      return new_string;
    }

LISPFUN(substring,2,1,norest,nokey,0,NIL)
# (SUBSTRING string start [end]) wie SUBSEQ, aber nur für Strings
  {
    var object string;
    var uintL len;
    var uintL start;
    var uintL end;
    # String/Symbol-Argument überprüfen:
    string = test_stringsymchar_arg(STACK_2);
    len = vector_length(string);
    # Nun ist len die Länge (<2^oint_data_len).
    # :START-Argument überprüfen:
      # start := Index STACK_1, Defaultwert 0, muss <=len sein:
      test_index(STACK_1,start=,1,0,<=,len,S(Kstart));
    # start ist jetzt der Wert des :START-Arguments.
    # :END-Argument überprüfen:
      # end := Index STACK_0, Defaultwert len, muss <=len sein:
      test_index(STACK_0,end=,2,len,<=,len,S(Kend));
    # end ist jetzt der Wert des :END-Arguments.
    # Vergleiche :START und :END Argumente:
    if (!(start <= end)) {
      pushSTACK(STACK_0); # :END-Index
      pushSTACK(STACK_2); # :START-Index
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: :start-index ~ must not be greater than :end-index ~")
            );
    }
    skipSTACK(3);
    # Teilstring herausziehen:
    pushSTACK(string); # alten String retten
    var uintL count = end-start; # Anzahl der zu kopierenden Characters
    var object new_string = allocate_string(count); # neuer String
    string = popSTACK(); # alter String
    if (count > 0) {
      var uintL len; # nochmals die Länge des alten Strings
      var uintL offset;
      string = unpack_string_ro(string,&len,&offset);
      SstringDispatch(string,
        { chartcopy(&TheSstring(string)->data[offset+start],&TheSstring(new_string)->data[0],count); },
        { scintcopy(&TheSmallSstring(string)->data[offset+start],&TheSstring(new_string)->data[0],count); }
        );
    }
    value1 = new_string; mv_count=1;
  }

# UP: bildet einen aus mehreren Strings zusammengehängten String.
# string_concat(argcount)
# > uintC argcount: Anzahl der Argumente
# > auf dem STACK: die Argumente (sollten Strings sein)
# > subr_self: Aufrufer (ein SUBR) (unnötig, falls alle Argumente Strings sind)
# < ergebnis: Gesamtstring, neu erzeugt
# < STACK: aufgeräumt
# can trigger GC
  global object string_concat (uintC argcount);
  global object string_concat(argcount)
    var uintC argcount;
    {
      var object* args_pointer = (args_end_pointer STACKop argcount);
      # args_pointer = Pointer über die Argumente
      # Überprüfe, ob es alles Strings sind, und addiere die Längen:
      var uintL total_length = 0;
      if (argcount > 0) {
        var object* argptr = args_pointer;
        var uintC count;
        dotimespC(count,argcount, {
          var object arg = NEXT(argptr); # nächstes Argument
          if (!stringp(arg))
            fehler_string(arg);
          total_length += vector_length(arg);
        });
      }
      # total_length ist jetzt die Gesamtlänge.
      var object new_string = allocate_string(total_length); # neuer String
      if (argcount > 0) {
        var chart* charptr2 = &TheSstring(new_string)->data[0];
        var object* argptr = args_pointer;
        dotimespC(argcount,argcount, {
          var object arg = NEXT(argptr); # nächster Argument-String
          var uintL len; # dessen Länge
          var uintL offset;
          var object string = unpack_string_ro(arg,&len,&offset);
          if (len > 0) {
            # Kopiere len Characters von string nach charptr2:
            SstringDispatch(string,
              { chartcopy(&TheSstring(string)->data[offset],charptr2,len); },
              { scintcopy(&TheSmallSstring(string)->data[offset],charptr2,len); }
              );
            charptr2 += len;
          }
        });
      }
      set_args_end_pointer(args_pointer); # STACK aufräumen
      return new_string;
    }

LISPFUN(string_concat,0,0,rest,nokey,0,NIL)
# (STRING-CONCAT {string})
# bildet einen aus den Argumenten zusammengehängten String
  {
    value1 = string_concat(argcount); mv_count=1;
  }

