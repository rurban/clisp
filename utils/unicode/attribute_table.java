/**
 * Program to print the attribute table for the entire Unicode character set.
 *
 * @author Bruno Haible
 */
import java.io.*;
public class attribute_table {
  static String toHexString1 (int i) {
    return new String(new char[] { "0123456789abcdef".charAt(i) });
  }
  static String toHexString2 (int i) {
    return  toHexString1((i>>4)&0x0f)
           +toHexString1(i&0x0f);
  }
  static String toHexString4 (int i) {
    return  toHexString1((i>>12)&0x0f)
           +toHexString1((i>>8)&0x0f)
           +toHexString1((i>>4)&0x0f)
           +toHexString1(i&0x0f);
  }
  static boolean isGraphic (char ch) {
    return (Character.isDefined(ch) || ch == (char)0x20AC)
           && !((int)ch < 0x0020 || ((int)ch >= 0x007F && (int)ch <= 0x009F));
  }
  static boolean isAlpha (char ch) {
    return Character.isLetter(ch);
  }
  static boolean isNumeric (char ch) {
    return Character.isDigit(ch);
  }
  static boolean equals256 (byte[] x, byte[] y) {
    for (int i = 0; i < 256; i++)
      if (!(x[i] == y[i]))
        return false;
    return true;
  }
  public static void main (String[] args) throws IOException {
    if (args.length != 1 && args.length != 2) {
      System.err.println("Usage: java attribute_table uni_attribute.c");
      System.exit(1);
    }
    String filename = args[0];
    String directory = (args.length > 1 ? args[1] : "");
    // Compute the attribute table.
    final int NON_GRAPHIC = 0;
    final int GRAPHIC_NON_ALPHANUMERIC = 1;
    final int NUMERIC = 2;
    final int ALPHABETIC = 3;
    int[] attribute = new int[0x10000];
    for (int i = 0; i < 0x10000; i++) {
      char ch = (char)i;
      boolean graphic = isGraphic(ch);
      boolean alpha = isAlpha(ch);
      boolean numeric = isNumeric(ch);
      if (alpha && numeric)
        System.err.println("Character 0x"+toHexString4(i)+" is both alpha and numeric");
      if (alpha && !graphic)
        System.err.println("Character 0x"+toHexString4(i)+" is alpha but not graphic");
      if (numeric && !graphic)
        System.err.println("Character 0x"+toHexString4(i)+" is numeric but not graphic");
      attribute[i] = (alpha ? ALPHABETIC :
                      numeric ? NUMERIC :
                      graphic ? GRAPHIC_NON_ALPHANUMERIC :
                      NON_GRAPHIC
                     );
    }
    // Output the attribute table.
    System.out.println("Creating "+directory+filename);
    FileOutputStream fstream = new FileOutputStream(directory+filename);
    PrintStream stream = new PrintStream(fstream);
    stream.println("/*");
    stream.println(" * "+filename);
    stream.println(" *");
    stream.println(" * Common Lisp character attribute table.");
    stream.println(" * Generated automatically by the attribute_table utility.");
    stream.println(" */");
    stream.println();
    byte[][] table = new byte[64][];
    for (int i1 = 0; i1 < 64; i1++) {
      table[i1] = new byte[256];
      for (int i2 = 0; i2 < 256; i2++) {
        int b = 0;
        for (int i3 = 0; i3 < 4; i3++) {
          int i = 1024*i1 + 4*i2 + i3;
          b |= attribute[i] << (2*i3);
        }
        table[i1][i2] = (byte)b;
      }
    }
    int[] pages = new int[64]; // Remove duplicate pages, to save space.
    for (int i1 = 0; i1 < 64; i1++) {
      int j;
      for (j = 0; j < i1; j++)
        if (equals256(table[j],table[i1]))
          break;
      pages[i1] = j;
    }
    for (int i1 = 0; i1 < 64; i1++)
      if (pages[i1] == i1) {
        stream.println("static const uintB unicode_attribute_table_page"+toHexString2(4*i1)+"[256] = {");
        for (int i2a = 0; i2a < 32; i2a++) {
          stream.print("  ");
          for (int i2b = 0; i2b < 8; i2b++) {
            int i2 = 8*i2a + i2b;
            stream.print("0x"+toHexString2(table[i1][i2])+(i2<255?",":" ")+" ");
          }
          stream.println("/* 0x"+toHexString4(1024*i1+32*i2a)+"-0x"+toHexString4(1024*i1+32*i2a+31)+" */");
        }
        stream.println("};");
        stream.println();
      }
    stream.println("const uintB * const unicode_attribute_table[64] = {");
    for (int i1a = 0; i1a < 32; i1a++) {
      stream.print(" ");
      for (int i1b = 0; i1b < 2; i1b++) {
        int i1 = 2*i1a+i1b;
        stream.print(" unicode_attribute_table_page"+toHexString2(4*pages[i1])+(i1<63?",":" "));
      }
      stream.println();
    }
    stream.println("};");
    stream.println();
    stream.close();
    fstream.close();
    System.exit(0);
  }
}
