# Encodings (character sets and conversions) for CLISP
# Bruno Haible 8.11.1998

#include "lispbibl.c"

# =============================================================================
#                             Individual encodings

#ifdef UNICODE

# NOTE 1! The mblen function has to be consistent with the mbstowcs function
# (when called with stream = nullobj).
# The wcslen function has to be consistent with the wcstombs function (when
# called with stream = nullobj).

# NOTE 2! The conversion from bytes to characters (mbstowcs function) is
# subject to the following restriction: At most one byte lookahead is needed.
# This means, when someone calls mbstowcs for converting one character, and
# he tries it with 1 byte, then with one more byte, then with one more byte,
# and so on: when the conversion succeeds for the first time, it will leave at
# most one byte in the buffer. stream.d (rd_ch_buffered, rd_ch_array_buffered)
# heavily depend on this.

local char hex_table[] = "0123456789ABCDEF";

# Error, when a character cannot be converted to an encoding.
# fehler_unencodable(encoding);
  nonreturning_function(global, fehler_unencodable, (object encoding, chart ch));
  global void fehler_unencodable(encoding,ch)
    var object encoding;
    var chart ch;
    { pushSTACK(TheEncoding(encoding)->enc_charset);
      pushSTACK(ascii_char(hex_table[as_cint(ch)&0x0F]));
      pushSTACK(ascii_char(hex_table[(as_cint(ch)>>4)&0x0F]));
      pushSTACK(ascii_char(hex_table[(as_cint(ch)>>8)&0x0F]));
      pushSTACK(ascii_char(hex_table[(as_cint(ch)>>12)&0x0F]));
      fehler(error,
             DEUTSCH ? "Zeichen #\\u$$$$ kann im Zeichensatz ~ nicht dargestellt werden." :
             ENGLISH ? "Character #\\u$$$$ cannot be represented in the character set ~" :
             FRANCAIS ? "Le caractère #\\u$$$$ ne peut pas être représenté en ~." :
             ""
            );
    }

# -----------------------------------------------------------------------------
#                              Unicode-16 encoding

# Unicode-16 encoding in two flavours:
# The big-endian format (files starting with 0xFE 0xFF),
# the little-endian format (files starting with 0xFF 0xFE).

# min. bytes per character = 2
# max. bytes per character = 2

global uintL uni16_mblen (object encoding, const uintB* src, const uintB* srcend);
global void uni16be_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global void uni16le_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global uintL uni16_wcslen (object encoding, const chart* src, const chart* srcend);
global void uni16be_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);
global void uni16le_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);

# Bytes to characters.

global uintL uni16_mblen(encoding,src,srcend)
  var object encoding;
  var const uintB* src;
  var const uintB* srcend;
  { return floor(srcend-src,2); }

global void uni16be_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  { var const uintB* src = *srcp;
    var chart* dest = *destp;
    var uintL count = floor(srcend-src,2);
    if (count > destend-dest) { count = destend-dest; }
    if (count > 0)
      { dotimespL(count,count,
          { *dest++ = as_chart(((cint)src[0] << 8) | (cint)src[1]);
            src += 2;
          });
        *srcp = src;
        *destp = dest;
  }   }

global void uni16le_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  { var const uintB* src = *srcp;
    var chart* dest = *destp;
    var uintL count = floor(srcend-src,2);
    if (count > destend-dest) { count = destend-dest; }
    if (count > 0)
      { dotimespL(count,count,
          { *dest++ = as_chart((cint)src[0] | ((cint)src[1] << 8));
            src += 2;
          });
        *srcp = src;
        *destp = dest;
  }   }

# Characters to bytes.

global uintL uni16_wcslen(encoding,src,srcend)
  var object encoding;
  var const chart* src;
  var const chart* srcend;
  { return (srcend-src)*2; }

global void uni16be_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  { var const chart* src = *srcp;
    var uintB* dest = *destp;
    var uintL count = floor(destend-dest,2);
    if (count > srcend-src) { count = srcend-src; }
    if (count > 0)
      { dotimespL(count,count,
          { var cint ch = as_cint(*src++);
            dest[0] = (uintB)(ch>>8); dest[1] = (uintB)ch;
            dest += 2;
          });
        *srcp = src;
        *destp = dest;
  }   }

global void uni16le_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  { var const chart* src = *srcp;
    var uintB* dest = *destp;
    var uintL count = floor(destend-dest,2);
    if (count > srcend-src) { count = srcend-src; }
    if (count > 0)
      { dotimespL(count,count,
          { var cint ch = as_cint(*src++);
            dest[0] = (uintB)ch; dest[1] = (uintB)(ch>>8);
            dest += 2;
          });
        *srcp = src;
        *destp = dest;
  }   }

# -----------------------------------------------------------------------------
#                                UTF-8 encoding

# See http://www.stonehand.com/unicode/standard/fss-utf.html
# or  Linux 2.0.x, file linux/fs/nls.c
#                   cmask  cval  shift     maxval           bits
#  1 byte sequence   0x80  0x00   0*6         0x7F  0XXXXXXX
#  2 byte sequence   0xE0  0xC0   1*6        0x7FF  110XXXXX 10XXXXXX
#  3 byte sequence   0xF0  0xE0   2*6       0xFFFF  1110XXXX 10XXXXXX 10XXXXXX
#  4 byte sequence   0xF8  0xF0   3*6     0x1FFFFF  11110XXX 10XXXXXX 10XXXXXX 10XXXXXX
#  5 byte sequence   0xFC  0xF8   4*6    0x3FFFFFF  111110XX 10XXXXXX 10XXXXXX 10XXXXXX 10XXXXXX
#  6 byte sequence   0xFE  0xFC   5*6   0x7FFFFFFF  1111110X 10XXXXXX 10XXXXXX 10XXXXXX 10XXXXXX 10XXXXXX
#
# We support only 16-bit Unicode characters, i.e. those which can be encoded
# with at most 3 bytes. Characters outside this range give an error.
# Spurious bytes of the form 10XXXXXX are ignored. (This resync feature is one
# of the benefits of the UTF encoding.)

# min. bytes per character = 1
# max. bytes per character = 3

global uintL utf8_mblen (object encoding, const uintB* src, const uintB* srcend);
global void utf8_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global uintL utf8_wcslen (object encoding, const chart* src, const chart* srcend);
global void utf8_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);

# Bytes to characters.

# Error when an invalid 1-byte sequence was encountered.
# fehler_utf8_invalid1(encoding,b1);
  nonreturning_function(local, fehler_utf8_invalid1, (object encoding, uintB b1));
  local void fehler_utf8_invalid1(encoding,b1)
    var object encoding;
    var uintB b1;
    { pushSTACK(TheEncoding(encoding)->enc_charset);
      pushSTACK(ascii_char(hex_table[b1&0x0F]));
      pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
      fehler(error,
             DEUTSCH ? "Ungültiges Byte #x$$ in ~ Konversion, kein Unicode-16" :
             ENGLISH ? "invalid byte #x$$ in ~ conversion, not a Unicode-16" :
             FRANCAIS ? "octet invalide #x$$ pendant conversion ~, pas un Unicode-16" :
             ""
            );
    }

# Error when an invalid 2-byte sequence was encountered.
# fehler_utf8_invalid2(encoding,b1,b2);
  nonreturning_function(local, fehler_utf8_invalid2, (object encoding, uintB b1, uintB b2));
  local void fehler_utf8_invalid2(encoding,b1,b2)
    var object encoding;
    var uintB b1;
    var uintB b2;
    { pushSTACK(TheEncoding(encoding)->enc_charset);
      pushSTACK(ascii_char(hex_table[b2&0x0F]));
      pushSTACK(ascii_char(hex_table[(b2>>4)&0x0F]));
      pushSTACK(ascii_char(hex_table[b1&0x0F]));
      pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
      fehler(error,
             DEUTSCH ? "Ungültige Byte-Folge #x$$ #x$$ in ~ Konversion" :
             ENGLISH ? "invalid byte sequence #x$$ #x$$ in ~ conversion" :
             FRANCAIS ? "séquence d'octets invalide #x$$ #x$$ pendant conversion ~" :
             ""
            );
    }

# Error when an invalid 3-byte sequence was encountered.
# fehler_utf8_invalid3(encoding,b1,b2,b3);
  nonreturning_function(local, fehler_utf8_invalid3, (object encoding, uintB b1, uintB b2, uintB b3));
  local void fehler_utf8_invalid3(encoding,b1,b2,b3)
    var object encoding;
    var uintB b1;
    var uintB b2;
    var uintB b3;
    { pushSTACK(TheEncoding(encoding)->enc_charset);
      pushSTACK(ascii_char(hex_table[b3&0x0F]));
      pushSTACK(ascii_char(hex_table[(b3>>4)&0x0F]));
      pushSTACK(ascii_char(hex_table[b2&0x0F]));
      pushSTACK(ascii_char(hex_table[(b2>>4)&0x0F]));
      pushSTACK(ascii_char(hex_table[b1&0x0F]));
      pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
      fehler(error,
             DEUTSCH ? "Ungültige Byte-Folge #x$$ #x$$ #x$$ in ~ Konversion" :
             ENGLISH ? "invalid byte sequence #x$$ #x$$ #x$$ in ~ conversion" :
             FRANCAIS ? "séquence d'octets invalide #x$$ #x$$ #x$$ pendant conversion ~" :
             ""
            );
    }

global uintL utf8_mblen(encoding,src,srcend)
  var object encoding;
  var const uintB* src;
  var const uintB* srcend;
  { var uintL count = 0;
    while (src < srcend) {
      var uintB c = src[0];
      if (c < 0x80) { # 1 byte sequence
        src += 1;
        count++;
        continue;
      }
      if (c < 0xC0) { src++; continue; } # skip spurious 10XXXXXX byte
      if (c < 0xE0) { # 2 byte sequence
        if (src+2 > srcend) break;
        if ((src[1] ^ 0x80) < 0x40) {
          src += 2;
          count++;
          continue;
        }
        fehler_utf8_invalid2(encoding,c,src[1]);
      }
      if (c < 0xF0) { # 3 byte sequence
        if (src+3 > srcend) break;
        if (((src[1] ^ 0x80) < 0x40) && ((src[2] ^ 0x80) < 0x40)) {
          src += 3;
          count++;
          continue;
        }
        fehler_utf8_invalid3(encoding,c,src[1],src[2]);
      }
      fehler_utf8_invalid1(encoding,c);
    }
    return count;
  }

global void utf8_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  { var const uintB* src = *srcp;
    var chart* dest = *destp;
    while (src < srcend) {
      var uintB c = src[0];
      if (c < 0x80) { # 1 byte sequence
        if (dest == destend) break;
        *dest++ = as_chart((cint)c);
        src += 1;
        continue;
      }
      if (c < 0xC0) { src++; continue; } # skip spurious 10XXXXXX byte
      if (dest == destend) break;
      if (c < 0xE0) { # 2 byte sequence
        if (src+2 > srcend) break;
        if ((src[1] ^ 0x80) < 0x40) {
          *dest++ = as_chart(((cint)(c & 0x1F) << 6) | (cint)(src[1] ^ 0x80));
          src += 2;
          continue;
        }
        fehler_utf8_invalid2(encoding,c,src[1]);
      }
      if (c < 0xF0) { # 3 byte sequence
        if (src+3 > srcend) break;
        if (((src[1] ^ 0x80) < 0x40) && ((src[2] ^ 0x80) < 0x40)) {
          *dest++ = as_chart(((cint)(c & 0x0F) << 12) | ((cint)(src[1] ^ 0x80) << 6) | (cint)(src[2] ^ 0x80));
          src += 3;
          continue;
        }
        fehler_utf8_invalid3(encoding,c,src[1],src[2]);
      }
      fehler_utf8_invalid1(encoding,c);
    }
    *srcp = src;
    *destp = dest;
  }

# Characters to bytes.

global uintL utf8_wcslen(encoding,src,srcend)
  var object encoding;
  var const chart* src;
  var const chart* srcend;
  { var uintL destlen = 0;
    while (src < srcend) {
      var cint ch = as_cint(*src++);
      destlen += (ch < 0x80 ? 1 : ch < 0x800 ? 2 : 3);
    }
    return destlen;
  }

global void utf8_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  { var const chart* src = *srcp;
    var uintB* dest = *destp;
    while (src < srcend) {
      var cint ch = as_cint(*src);
      var uintL count = (ch < 0x80 ? 1 : ch < 0x800 ? 2 : 3);
      if (dest+count > destend) break;
      src++;
      if (ch < 0x80) { # 1 byte sequence
        *dest++ = ch;
      } else if (ch < 0x800) { # 2 byte sequence
        *dest++ = 0xC0 | (ch >> 6);
        *dest++ = 0x80 | (ch & 0x3F);
      } else { # ch < 0x10000, 3 byte sequence
        *dest++ = 0xE0 | (ch >> 12);
        *dest++ = 0x80 | ((ch >> 6) & 0x3F);
        *dest++ = 0x80 | (ch & 0x3F);
      }
    }
    *srcp = src;
    *destp = dest;
  }

# -----------------------------------------------------------------------------
#                                Java encoding

# This is ISO 8859-1 with \uXXXX escape sequences, denoting Unicode characters.
# See the Java Language Specification.
# Thick is quick&dirty: The text is supposed not to contain \u except as part
# of \uXXXX escape sequences.

# min. bytes per character = 1
# max. bytes per character = 6

global uintL java_mblen (object encoding, const uintB* src, const uintB* srcend);
global void java_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global uintL java_wcslen (object encoding, const chart* src, const chart* srcend);
global void java_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);

# Bytes to characters.

global uintL java_mblen(encoding,src,srcend)
  var object encoding;
  var const uintB* src;
  var const uintB* srcend;
  { var uintL count = 0;
    while (src < srcend) {
      var uintB c;
      if (src[0] != '\\') {
        src += 1;
        count++;
        continue;
      }
      if (src+2 > srcend) break;
      if (src[1] != 'u') {
        src += 1;
        count++;
        continue;
      }
      if (src+3 > srcend) break;
      c = src[2];
      if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))) {
        src += 2; # skip incomplete \u sequence
        continue;
      }
      if (src+4 > srcend) break;
      c = src[3];
      if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))) {
        src += 3; # skip incomplete \u sequence
        continue;
      }
      if (src+5 > srcend) break;
      c = src[4];
      if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))) {
        src += 4; # skip incomplete \u sequence
        continue;
      }
      if (src+6 > srcend) break;
      c = src[5];
      if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))) {
        src += 5; # skip incomplete \u sequence
        continue;
      }
      src += 6; # complete \u sequence
      count++;
      continue;
    }
    return count;
  }

global void java_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  { var const uintB* src = *srcp;
    var chart* dest = *destp;
    while (src < srcend) {
      var uintB c;
      var cint ch;
      c = src[0];
      if (c != '\\') {
        if (dest==destend) break;
        *dest++ = as_chart((cint)c);
        src += 1;
        continue;
      }
      if (src+2 > srcend) break;
      if (src[1] != 'u') {
        if (dest==destend) break;
        *dest++ = as_chart((cint)c);
        src += 1;
        continue;
      }
      if (src+3 > srcend) break;
      c = src[2];
      if (c >= '0' && c <= '9') { c -= '0'; }
      elif (c >= 'A' && c <= 'F') { c -= 'A'-10; }
      elif (c >= 'a' && c <= 'f') { c -= 'a'-10; }
      else {
        src += 2; # skip incomplete \u sequence
        continue;
      }
      ch = (cint)c << 12;
      if (src+4 > srcend) break;
      c = src[3];
      if (c >= '0' && c <= '9') { c -= '0'; }
      elif (c >= 'A' && c <= 'F') { c -= 'A'-10; }
      elif (c >= 'a' && c <= 'f') { c -= 'a'-10; }
      else {
        src += 3; # skip incomplete \u sequence
        continue;
      }
      ch |= (cint)c << 8;
      if (src+5 > srcend) break;
      c = src[4];
      if (c >= '0' && c <= '9') { c -= '0'; }
      elif (c >= 'A' && c <= 'F') { c -= 'A'-10; }
      elif (c >= 'a' && c <= 'f') { c -= 'a'-10; }
      else {
        src += 4; # skip incomplete \u sequence
        continue;
      }
      ch |= (cint)c << 4;
      if (src+6 > srcend) break;
      c = src[5];
      if (c >= '0' && c <= '9') { c -= '0'; }
      elif (c >= 'A' && c <= 'F') { c -= 'A'-10; }
      elif (c >= 'a' && c <= 'f') { c -= 'a'-10; }
      else {
        src += 5; # skip incomplete \u sequence
        continue;
      }
      ch |= (cint)c;
      if (dest==destend) break;
      *dest++ = as_chart(ch);
      src += 6; # complete \u sequence
      continue;
    }
    *srcp = src;
    *destp = dest;
  }

# Characters to bytes.

global uintL java_wcslen(encoding,src,srcend)
  var object encoding;
  var const chart* src;
  var const chart* srcend;
  { var uintL destlen = 0;
    while (src < srcend) {
      var cint ch = as_cint(*src++);
      destlen += (ch < 0x80 ? 1 : 6);
    }
    return destlen;
  }

global void java_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  { var const chart* src = *srcp;
    var uintB* dest = *destp;
    while (src < srcend) {
      var cint ch = as_cint(*src);
      var uintL count = (ch < 0x80 ? 1 : 6);
      if (dest+count > destend) break;
      src++;
      if (ch < 0x80) { # 1 byte sequence
        *dest++ = ch;
      } else { # 6 byte sequence
        local char hex_table[] = "0123456789abcdef";
        *dest++ = '\\';
        *dest++ = 'u';
        *dest++ = hex_table[(ch>>12)&0x0F];
        *dest++ = hex_table[(ch>>8)&0x0F];
        *dest++ = hex_table[(ch>>4)&0x0F];
        *dest++ = hex_table[ch&0x0F];
      }
    }
    *srcp = src;
    *destp = dest;
  }

# -----------------------------------------------------------------------------
#                            8-bit NLS characters sets

# min. bytes per character = 1
# max. bytes per character = 1

typedef struct nls_table {
        const char* charset;
        const unsigned char* const* page_uni2charset;
        const unsigned short* charset2uni;
        int is_ascii_extension;
} nls_table;

static const unsigned char nopage[256] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x00-0x07 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x08-0x0f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x10-0x17 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x18-0x1f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x20-0x27 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x28-0x2f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x30-0x37 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x38-0x3f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x40-0x47 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x48-0x4f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x50-0x57 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x58-0x5f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x60-0x67 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x68-0x6f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x70-0x77 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x78-0x7f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x80-0x87 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x88-0x8f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x90-0x97 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x98-0x9f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xa0-0xa7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xa8-0xaf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xb0-0xb7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xb8-0xbf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xc0-0xc7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xc8-0xcf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xd0-0xd7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xd8-0xdf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xe0-0xe7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xe8-0xef */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xf0-0xf7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  /* 0xf8-0xff */
};

#include "nls_ascii.c"
#include "nls_iso8859_1.c"
#include "nls_iso8859_2.c"
#include "nls_iso8859_3.c"
#include "nls_iso8859_4.c"
#include "nls_iso8859_5.c"
#include "nls_iso8859_6.c"
#include "nls_iso8859_7.c"
#include "nls_iso8859_8.c"
#include "nls_iso8859_9.c"
#include "nls_iso8859_14.c"
#include "nls_iso8859_15.c"
#include "nls_koi8_r.c"
#include "nls_mac_arabic.c"
#include "nls_mac_centraleurope.c"
#include "nls_mac_croatian.c"
#include "nls_mac_cyrillic.c"
#include "nls_mac_dingbat.c"
#include "nls_mac_greek.c"
#include "nls_mac_hebrew.c"
#include "nls_mac_iceland.c"
#include "nls_mac_roman.c"
#include "nls_mac_romania.c"
#include "nls_mac_symbol.c"
#include "nls_mac_thai.c"
#include "nls_mac_turkish.c"
#include "nls_mac_ukraine.c"
#include "nls_cp437_ms.c"
#include "nls_cp437_ibm.c"
#include "nls_cp737.c"
#include "nls_cp775.c"
#include "nls_cp850.c"
#include "nls_cp852_ms.c"
#include "nls_cp852_ibm.c"
#include "nls_cp855.c"
#include "nls_cp857.c"
#include "nls_cp860_ms.c"
#include "nls_cp860_ibm.c"
#include "nls_cp861_ms.c"
#include "nls_cp861_ibm.c"
#include "nls_cp862_ms.c"
#include "nls_cp862_ibm.c"
#include "nls_cp863_ms.c"
#include "nls_cp863_ibm.c"
#include "nls_cp864_ms.c"
#include "nls_cp864_ibm.c"
#include "nls_cp865_ms.c"
#include "nls_cp865_ibm.c"
#include "nls_cp866.c"
#include "nls_cp869_ms.c"
#include "nls_cp869_ibm.c"
#include "nls_cp874_ms.c"
#include "nls_cp874_ibm.c"
#include "nls_cp1250.c"
#include "nls_cp1251.c"
#include "nls_cp1252.c"
#include "nls_cp1253.c"
#include "nls_cp1254.c"
#include "nls_cp1255.c"
#include "nls_cp1256.c"
#include "nls_cp1257.c"
#include "nls_cp1258.c"
#include "nls_nextstep.c"

#define nls_first_sym  S(ascii)
#define nls_last_sym  S(nextstep)
#define nls_num_encodings  (&symbol_tab_data.S_nextstep - &symbol_tab_data.S_ascii + 1)

static const nls_table * const nls_tables[] = {
  &nls_ascii_table,
  &nls_iso8859_1_table,
  &nls_iso8859_2_table,
  &nls_iso8859_3_table,
  &nls_iso8859_4_table,
  &nls_iso8859_5_table,
  &nls_iso8859_6_table,
  &nls_iso8859_7_table,
  &nls_iso8859_8_table,
  &nls_iso8859_9_table,
  &nls_iso8859_14_table,
  &nls_iso8859_15_table,
  &nls_koi8_r_table,
  &nls_mac_arabic_table,
  &nls_mac_centraleurope_table,
  &nls_mac_croatian_table,
  &nls_mac_cyrillic_table,
  &nls_mac_dingbat_table,
  &nls_mac_greek_table,
  &nls_mac_hebrew_table,
  &nls_mac_iceland_table,
  &nls_mac_roman_table,
  &nls_mac_romania_table,
  &nls_mac_symbol_table,
  &nls_mac_thai_table,
  &nls_mac_turkish_table,
  &nls_mac_ukraine_table,
  &nls_cp437_ms_table,
  &nls_cp437_ibm_table,
  &nls_cp737_table,
  &nls_cp775_table,
  &nls_cp850_table,
  &nls_cp852_ms_table,
  &nls_cp852_ibm_table,
  &nls_cp855_table,
  &nls_cp857_table,
  &nls_cp860_ms_table,
  &nls_cp860_ibm_table,
  &nls_cp861_ms_table,
  &nls_cp861_ibm_table,
  &nls_cp862_ms_table,
  &nls_cp862_ibm_table,
  &nls_cp863_ms_table,
  &nls_cp863_ibm_table,
  &nls_cp864_ms_table,
  &nls_cp864_ibm_table,
  &nls_cp865_ms_table,
  &nls_cp865_ibm_table,
  &nls_cp866_table,
  &nls_cp869_ms_table,
  &nls_cp869_ibm_table,
  &nls_cp874_ms_table,
  &nls_cp874_ibm_table,
  &nls_cp1250_table,
  &nls_cp1251_table,
  &nls_cp1252_table,
  &nls_cp1253_table,
  &nls_cp1254_table,
  &nls_cp1255_table,
  &nls_cp1256_table,
  &nls_cp1257_table,
  &nls_cp1258_table,
  &nls_nextstep_table
};

global uintL nls_mblen (object encoding, const uintB* src, const uintB* srcend);
global void nls_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global void nls_asciiext_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global uintL nls_wcslen (object encoding, const chart* src, const chart* srcend);
global void nls_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);
global void nls_asciiext_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);

# Bytes to characters.

# Error when an invalid byte was encountered.
# fehler_nls_invalid(encoding,b);
  nonreturning_function(local, fehler_nls_invalid, (object encoding, uintB b));
  local void fehler_nls_invalid(encoding,b)
    var object encoding;
    var uintB b;
    { pushSTACK(TheEncoding(encoding)->enc_charset);
      pushSTACK(ascii_char(hex_table[b&0x0F]));
      pushSTACK(ascii_char(hex_table[(b>>4)&0x0F]));
      fehler(error,
             DEUTSCH ? "Ungültiges Byte #x$$ in ~ Konversion" :
             ENGLISH ? "invalid byte #x$$ in ~ conversion" :
             FRANCAIS ? "octet invalide #x$$ pendant conversion ~" :
             ""
            );
    }

global uintL nls_mblen(encoding,src,srcend)
  var object encoding;
  var const uintB* src;
  var const uintB* srcend;
  { return (srcend-src); }

global void nls_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  { var const uintB* src = *srcp;
    var chart* dest = *destp;
    var uintL count = destend-dest;
    if (count > srcend-src) { count = srcend-src; }
    if (count > 0)
      { var const nls_table* table = (const nls_table*) ThePseudofun(TheEncoding(encoding)->enc_table);
        var const unsigned short* cvtable = table->charset2uni;
        var uintB b;
        dotimespL(count,count,
          { b = *src++;
           {var cint ch = cvtable[b];
            if (ch == 0xFFFD) goto bad;
            *dest++ = as_chart(ch);
          }});
        *srcp = src;
        *destp = dest;
        return;
       bad:
        fehler_nls_invalid(encoding,b);
  }   }

# Same thing, specially optimized for ASCII extensions.
global void nls_asciiext_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  { var const uintB* src = *srcp;
    var chart* dest = *destp;
    var uintL count = destend-dest;
    if (count > srcend-src) { count = srcend-src; }
    if (count > 0)
      { var const nls_table* table = (const nls_table*) ThePseudofun(TheEncoding(encoding)->enc_table);
        var const unsigned short* cvtable = table->charset2uni;
        var uintB b;
        dotimespL(count,count,
          { b = *src++;
           {var cint ch;
            if (b < 0x80)
              { ch = b; } # avoid memory reference (big speedup!)
              else
              { ch = cvtable[b];
                if (ch == 0xFFFD) goto bad;
              }
            *dest++ = as_chart(ch);
          }});
        *srcp = src;
        *destp = dest;
        return;
       bad:
        fehler_nls_invalid(encoding,b);
  }   }

# Characters to bytes.

global uintL nls_wcslen(encoding,src,srcend)
  var object encoding;
  var const chart* src;
  var const chart* srcend;
  { return (srcend-src); }

global void nls_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  { var const chart* src = *srcp;
    var uintB* dest = *destp;
    var uintL count = srcend-src;
    if (count > destend-dest) { count = destend-dest; }
    if (count > 0)
      { var const nls_table* table = (const nls_table*) ThePseudofun(TheEncoding(encoding)->enc_table);
        var const unsigned char* const* cvtable = table->page_uni2charset;
        var chart ch;
        dotimespL(count,count,
          { ch = *src++;
           {var uintB b = cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF];
            if (b == 0 && !chareq(ch,ascii(0))) goto bad;
            *dest++ = b;
          }});
        *srcp = src;
        *destp = dest;
        return;
       bad:
        fehler_unencodable(encoding,ch);
  }   }

# Same thing, specially optimized for ASCII extensions.
global void nls_asciiext_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  { var const chart* src = *srcp;
    var uintB* dest = *destp;
    var uintL count = srcend-src;
    if (count > destend-dest) { count = destend-dest; }
    if (count > 0)
      { var const nls_table* table = (const nls_table*) ThePseudofun(TheEncoding(encoding)->enc_table);
        var const unsigned char* const* cvtable = table->page_uni2charset;
        var chart ch;
        dotimespL(count,count,
          { ch = *src++;
           {var uintB b;
            if (as_cint(ch) < 0x80)
              { b = (uintB)as_cint(ch); } # avoid memory reference (big speedup!)
              else
              { b = cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF];
                if (b == 0) goto bad;
              }
            *dest++ = b;
          }});
        *srcp = src;
        *destp = dest;
        return;
       bad:
        fehler_unencodable(encoding,ch);
  }   }

# -----------------------------------------------------------------------------
#                             iconv-based encodings

# They are defined in stream.d because they need to access internals of
# the ChannelStream.

#ifdef HAVE_ICONV

extern uintL iconv_mblen (object encoding, const uintB* src, const uintB* srcend);
extern void iconv_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
extern uintL iconv_wcslen (object encoding, const chart* src, const chart* srcend);
extern void iconv_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);

#endif # HAVE_ICONV

# -----------------------------------------------------------------------------

#endif # UNICODE

# =============================================================================
#                              General functions

LISPFUN(make_encoding,0,0,norest,key,2,
        (kw(charset),kw(line_terminator)) )
# (MAKE-ENCODING [:charset] [:line-terminator])
# creates a new encoding.
  { var object arg;
    # Check the :CHARSET argument.
    arg = STACK_1;
    if (eq(arg,unbound) || eq(arg,S(Kdefault)))
      { arg = O(default_file_encoding); }
    elif (encodingp(arg))
      { }
    #ifdef UNICODE
    elif (symbolp(arg) && constantp(TheSymbol(arg))
          && encodingp(Symbol_value(arg))
         )
      { arg = Symbol_value(arg); }
    #ifdef HAVE_ICONV
    elif (stringp(arg))
      { pushSTACK(coerce_ss(arg));
       {var object encoding = allocate_encoding();
        TheEncoding(encoding)->enc_eol = S(Kunix);
        TheEncoding(encoding)->enc_charset = popSTACK();
        TheEncoding(encoding)->enc_mblen    = P(iconv_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(iconv_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(iconv_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(iconv_wcstombs);
        TheEncoding(encoding)->min_bytes_per_char = 1;
        TheEncoding(encoding)->max_bytes_per_char = 6; # unfounded assumption
        arg = encoding;
      }}
    #endif
    #endif
    else
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(encoding)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(make_encoding));
        fehler(type_error,
               DEUTSCH ? "~: Als :CHARSET-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :CHARSET argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :CHARSET." :
               ""
              );
      }
    STACK_1 = arg;
    # Check the :LINE-TERMINATOR argument.
    arg = STACK_0;
    if (!(eq(arg,unbound)
          || eq(arg,S(Kunix)) || eq(arg,S(Kmac)) || eq(arg,S(Kdos))
       ) )
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_line_terminator)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(make_encoding));
        fehler(type_error,
               DEUTSCH ? "~: Als :LINE-TERMINATOR-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :LINE-TERMINATOR argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :LINE-TERMINATOR." :
               ""
              );
      }
    # Create a new encoding.
    if (eq(arg,unbound) || eq(arg,TheEncoding(STACK_1)->enc_eol))
      { value1 = STACK_1; }
      else
      { var object encoding = allocate_encoding();
        var object old_encoding = STACK_1;
        { var const object* ptr1 = &TheRecord(old_encoding)->recdata[0];
          var object* ptr2 = &TheRecord(encoding)->recdata[0];
          var uintC count;
          dotimesC(count,encoding_length, { *ptr2++ = *ptr1++; } );
          memcpy(ptr2,ptr1,encoding_xlength);
        }
        TheEncoding(encoding)->enc_eol = STACK_0;
        value1 = encoding;
      }
    mv_count=1;
    skipSTACK(2);
  }

# -----------------------------------------------------------------------------
#                          Elementary string functions

# UP: Liefert einen LISP-String mit vorgegebenem Inhalt.
# make_string(charptr,len,encoding)
# > uintB* charptr: Adresse einer Zeichenfolge
# > uintL len: Länge der Zeichenfolge
# > object encoding: Encoding
# < ergebnis: Normal-Simple-String mit den len Zeichen ab charptr als Inhalt
# kann GC auslösen
  #ifdef UNICODE
    global object make_string (const uintB* bptr, uintL blen, object encoding);
    global object make_string(bptr,blen,encoding)
      var const uintB* bptr;
      var uintL blen;
      var object encoding;
      { var const uintB* bendptr = bptr+blen;
        var uintL clen = Encoding_mblen(encoding)(encoding,bptr,bendptr);
        pushSTACK(encoding);
       {var object obj = allocate_string(clen);
        encoding = popSTACK();
        { var chart* cptr = &TheSstring(obj)->data[0];
          var chart* cendptr = cptr+clen;
          Encoding_mbstowcs(encoding)(encoding,nullobj,&bptr,bendptr,&cptr,cendptr);
          ASSERT(cptr == cendptr);
        }
        return obj;
      }}
  #else
    global object make_string_ (const uintB* bptr, uintL len);
    global object make_string_(bptr,len)
      var const uintB* bptr;
      var uintL len;
      { var object obj = allocate_string(len); # String allozieren
        if (len > 0)
          { var chart* ptr = &TheSstring(obj)->data[0];
            # Zeichenfolge von bptr nach ptr kopieren:
            dotimespL(len,len, { *ptr++ = as_chart(*bptr++); } );
          }
        return obj;
      }
  #endif

# UP: Wandelt einen ASCIZ-String in einen LISP-String um.
# asciz_to_string(asciz,encoding)
# ascii_to_string(asciz)
# > char* asciz: ASCIZ-String
#       (Adresse einer durch ein Nullbyte abgeschlossenen Zeichenfolge)
# > object encoding: Encoding
# < ergebnis: Normal-Simple-String mit der Zeichenfolge (ohne Nullbyte) als Inhalt
# kann GC auslösen
  #ifdef UNICODE
    global object asciz_to_string (const char * asciz, object encoding);
    global object asciz_to_string(asciz,encoding)
      var const char* asciz;
      var object encoding;
      { return make_string((const uintB*)asciz,asciz_length(asciz),encoding); }
  #else
    global object asciz_to_string_ (const char * asciz);
    global object asciz_to_string_(asciz)
      var const char* asciz;
      { return make_string_((const uintB*)asciz,asciz_length(asciz)); }
  #endif
  global object ascii_to_string (const char * asciz);
  global object ascii_to_string(asciz)
    var const char* asciz;
    { var const uintB* bptr = (const uintB*)asciz;
      var uintL len = asciz_length(asciz);
      var object obj = allocate_string(len); # String allozieren
      if (len > 0)
        { var chart* ptr = &TheSstring(obj)->data[0];
          # Zeichenfolge von bptr nach ptr kopieren:
          dotimespL(len,len,
            { var uintB b = *bptr++;
              ASSERT(b < 0x80);
              *ptr++ = as_chart(b);
            });
        }
      return obj;
    }

# UP: Wandelt einen String in einen ASCIZ-String um.
# string_to_asciz(obj,encoding)
# > object obj: String
# > object encoding: Encoding
# < ergebnis: Simple-Bit-Vektor mit denselben Zeichen als Bytes und einem
#             Nullbyte mehr am Schluss
# < TheAsciz(ergebnis): Adresse der darin enthaltenen Bytefolge
# kann GC auslösen
  #ifdef UNICODE
    global object string_to_asciz (object obj, object encoding);
    global object string_to_asciz(obj,encoding)
      var object obj;
      var object encoding;
      {  var uintL len;
         var uintL offset;
         var object string = unpack_string_ro(obj,&len,&offset);
         var const chart* srcptr;
         unpack_sstring_alloca(string,len,offset, srcptr=);
       { var uintL bytelen = cslen(encoding,srcptr,len);
         pushSTACK(encoding);
         pushSTACK(string);
        {var object newasciz = allocate_bit_vector((bytelen+1)*8);
         string = popSTACK();
         encoding = popSTACK();
         unpack_sstring_alloca(string,len,offset, srcptr=);
         cstombs(encoding,srcptr,len,&TheSbvector(newasciz)->data[0],bytelen);
         TheSbvector(newasciz)->data[bytelen] = '\0';
         return newasciz;
      }}}
  #else
    global object string_to_asciz_ (object obj);
    global object string_to_asciz_(obj)
      var object obj;
      { pushSTACK(obj); # String retten
       {var object newasciz = allocate_bit_vector((vector_length(obj)+1)*8);
        obj = popSTACK(); # String zurück
        { var uintL len;
          var uintL offset;
          var object string = unpack_string_ro(obj,&len,&offset);
          var const chart* sourceptr = &TheSstring(string)->data[offset];
          # Source-String: Länge in len, Bytes ab sourceptr
          var uintB* destptr = &TheSbvector(newasciz)->data[0];
          # Destination-String: Bytes ab destptr
          { # Kopierschleife:
            var uintL count;
            dotimesL(count,len, { *destptr++ = as_cint(*sourceptr++); } );
            *destptr++ = '\0'; # Nullbyte anfügen
        } }
        return newasciz;
      }}
  #endif

# =============================================================================
#                               Initialization

# Initialize the encodings.
# init_encodings();
  global void init_encodings (void);
  global void init_encodings()
    { # Compile-time checks:
        ASSERT(sizeof(chart) == sizeof(cint));
      #ifdef UNICODE
      { var object symbol = S(unicode_16_big_endian);
        var object encoding = allocate_encoding();
        TheEncoding(encoding)->enc_eol = S(Kunix);
        TheEncoding(encoding)->enc_charset = symbol;
        TheEncoding(encoding)->enc_mblen    = P(uni16_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(uni16be_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(uni16_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(uni16be_wcstombs);
        TheEncoding(encoding)->min_bytes_per_char = 2;
        TheEncoding(encoding)->max_bytes_per_char = 2;
        define_constant(symbol,encoding);
      }
      { var object symbol = S(unicode_16_little_endian);
        var object encoding = allocate_encoding();
        TheEncoding(encoding)->enc_eol = S(Kunix);
        TheEncoding(encoding)->enc_charset = symbol;
        TheEncoding(encoding)->enc_mblen    = P(uni16_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(uni16le_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(uni16_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(uni16le_wcstombs);
        TheEncoding(encoding)->min_bytes_per_char = 2;
        TheEncoding(encoding)->max_bytes_per_char = 2;
        define_constant(symbol,encoding);
      }
      { var object symbol = S(utf_8);
        var object encoding = allocate_encoding();
        TheEncoding(encoding)->enc_eol = S(Kunix);
        TheEncoding(encoding)->enc_charset = symbol;
        TheEncoding(encoding)->enc_mblen    = P(utf8_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(utf8_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(utf8_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(utf8_wcstombs);
        TheEncoding(encoding)->min_bytes_per_char = 1;
        TheEncoding(encoding)->max_bytes_per_char = 3;
        define_constant(symbol,encoding);
      }
      { var object symbol = S(java);
        var object encoding = allocate_encoding();
        TheEncoding(encoding)->enc_eol = S(Kunix);
        TheEncoding(encoding)->enc_charset = symbol;
        TheEncoding(encoding)->enc_mblen    = P(java_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(java_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(java_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(java_wcstombs);
        TheEncoding(encoding)->min_bytes_per_char = 1;
        TheEncoding(encoding)->max_bytes_per_char = 6;
        define_constant(symbol,encoding);
      }
      { var object symbol = nls_first_sym;
        var const nls_table * const * ptr = &nls_tables[0];
        var uintC count;
        ASSERT(nls_num_encodings == sizeof(nls_tables)/sizeof(nls_tables[0]));
        dotimesC(count,sizeof(nls_tables)/sizeof(nls_tables[0]),
          { var object encoding = allocate_encoding();
            TheEncoding(encoding)->enc_eol = S(Kunix);
            TheEncoding(encoding)->enc_charset = symbol;
            TheEncoding(encoding)->enc_mblen    = P(nls_mblen);
            TheEncoding(encoding)->enc_mbstowcs = ((*ptr)->is_ascii_extension
                                                   ? P(nls_asciiext_mbstowcs)
                                                   : P(nls_mbstowcs)
                                                  );
            TheEncoding(encoding)->enc_wcslen   = P(nls_wcslen);
            TheEncoding(encoding)->enc_wcstombs = ((*ptr)->is_ascii_extension
                                                   ? P(nls_asciiext_wcstombs)
                                                   : P(nls_wcstombs)
                                                  );
            TheEncoding(encoding)->enc_table    = P(**ptr);
            TheEncoding(encoding)->min_bytes_per_char = 1;
            TheEncoding(encoding)->max_bytes_per_char = 1;
            define_constant(symbol,encoding);
            symbol = objectplus(symbol,(soint)sizeof(*TheSymbol(symbol))<<(oint_addr_shift-addr_shift));
            ptr++;
          });
      }
      #endif
      # Initialize O(default_encoding):
        #ifdef UNICODE
        #if defined(ISOLATIN_CHS)
        pushSTACK(Symbol_value(S(iso8859_1)));
        #elif defined(NEXTSTEP_CHS)
        pushSTACK(Symbol_value(S(nextstep)));
        #elif defined(IBMPC_CHS)
        pushSTACK(Symbol_value(S(cp437_ibm)));
        #else
        pushSTACK(Symbol_value(S(ascii)));
        #endif
        #else
        pushSTACK(unbound);
        #endif
        #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
        pushSTACK(S(Kdos));
        #else
        pushSTACK(S(Kunix));
        #endif
        C_make_encoding();
        O(default_file_encoding) = value1;
      #ifdef UNICODE
      { var object encoding = O(default_file_encoding);
        O(internal_encoding) = encoding;
        O(pathname_encoding) = encoding;
        O(terminal_encoding) = encoding;
        #if defined(HAVE_FFI) || defined(HAVE_AFFI)
          O(foreign_encoding) = encoding;
        #endif
        O(misc_encoding) = encoding;
      }
      #endif
    }

