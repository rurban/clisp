/**
 * Program to print the downcase table for the entire Unicode character set.
 *
 * @author Bruno Haible
 */
import java.io.*;
public class downcase_table {
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
  // Common Lisp only wants the bijective lower/upper case conversions.
  public static boolean isCLBothCase (char ch) {
    char ch1 = Character.toUpperCase(ch);
    char ch2 = Character.toLowerCase(ch);
    return ((ch1 == ch || ch2 == ch)
            && (ch1 != ch2)
            && Character.toUpperCase(ch1) == ch1
            && Character.toUpperCase(ch2) == ch1
            && Character.toLowerCase(ch1) == ch2
            && Character.toLowerCase(ch2) == ch2
           );
  }
  public static void main (String[] args) throws IOException {
    if (args.length != 1 && args.length != 2) {
      System.err.println("Usage: java downcase_table uni_downcase.c");
      System.exit(1);
    }
    String filename = args[0];
    String directory = (args.length > 1 ? args[1] : "");
    System.out.println("Creating "+directory+filename);
    FileOutputStream fstream = new FileOutputStream(directory+filename);
    PrintStream stream = new PrintStream(fstream);
    stream.println("/*");
    stream.println(" * "+filename);
    stream.println(" *");
    stream.println(" * Common Lisp downcase table.");
    stream.println(" * Generated automatically by the downcase_table utility.");
    stream.println(" */");
    stream.println();
    boolean[] pages = new boolean[256];
    for (int p = 0; p < 256; p++)
      for (int i1 = 0; i1 < 256; i1++) {
        int i = 256*p + i1;
        char ch = (char)i;
        if (isCLBothCase(ch) && (Character.toLowerCase(ch) != ch)) {
          pages[p] = true;
          break;
        }
      }
    for (int p = 0; p < 256; p++)
      if (pages[p]) {
        stream.println("static const cint down_case_table_page"+toHexString2(p)+"[256] = {");
        for (int i1 = 0; i1 < 32; i1++) {
          stream.print("  ");
          for (int i2 = 0; i2 < 8; i2++) {
            int i = 256*p + 8*i1 + i2;
            char ch = (char)i;
            char ch2 = (isCLBothCase(ch) ? Character.toLowerCase(ch) : ch);
            int j = ((int)ch2 - (int)ch) & 0xffff;
            stream.print("0x"+toHexString4(j)+(8*i1+i2<255?",":" ")+" ");
          }
          stream.println("/* 0x"+toHexString2(8*i1)+"-0x"+toHexString2(8*i1+7)+" */");
        }
        stream.println("};");
        stream.println();
      }
    stream.println("static const cint * const down_case_table[256] = {");
    for (int p1 = 0; p1 < 64; p1++) {
      stream.print("  ");
      for (int p2 = 0; p2 < 4; p2++) {
        int p = 4*p1 + p2;
        if (pages[p])
          stream.print("down_case_table_page"+toHexString2(p)+(p<255?",":" ")+" ");
        else
          stream.print("nop_page"+(p<255?",":" ")+" ");
      }
      stream.println("/* 0x"+toHexString2(4*p1)+"-0x"+toHexString2(4*p1+3)+" */");
    }
    stream.println("};");
    stream.println();
    stream.close();
    fstream.close();
    System.exit(0);
  }
}
