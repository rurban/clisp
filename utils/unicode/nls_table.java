/**
 * Program to print the character set conversion tables for a particular
 * characters set. Examines the output of "native2ascii".
 *
 * @author Bruno Haible
 */
import java.io.*;
public class nls_table {
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
  public static void main (String[] args) throws IOException {
    if (args.length != 1 && args.length != 2)
      System.exit(1);
    String filename = args[0];
    String directory = (args.length > 1 ? args[1] : "");
    System.out.println("Creating "+directory+filename);
    String charsetname = filename.substring(0,filename.length()-2).substring(4).replace('_','-');
    int[] charset = new int[256];
    // Analyze the input.
    {
      DataInputStream stream = new DataInputStream(System.in);
      for (int i = 0; i < 256; i++) {
        int j;
        if (i == 10 || i == 13)
          j = i;
        else {
          String l = stream.readLine();
          if (l.length() == 1)
            j = (int)l.charAt(0);
          else if (l.length() == 6 && l.charAt(0) == '\\' && l.charAt(1) == 'u')
            j = Integer.valueOf(l.substring(2),16).intValue();
          else {
            System.err.println("unknown translation for character "+i);
            j = 0;
          }
        }
        charset[i] = j;
      }
      stream.close();
    }
    // Print the output file.
    {
      FileOutputStream fstream = new FileOutputStream(directory+filename);
      PrintStream stream = new PrintStream(fstream);
      stream.println("/*");
      stream.println(" * nls_"+charsetname+".c");
      stream.println(" *");
      stream.println(" * Charset "+charsetname+" translation tables.");
      stream.println(" * Generated automatically by the nls_table utility.");
      stream.println(" */");
      stream.println();
      stream.println("#define charset2uni "+charsetname.replace('-','_')+"_charset2uni");
      stream.println();
      stream.println("static const unsigned short charset2uni[256] = {");
      for (int i1 = 0; i1 < 16; i1++) {
        stream.println("  /* 0x"+toHexString2(16*i1)+" */");
        for (int i2 = 0; i2 < 2; i2++) {
          stream.print("  ");
          for (int i3 = 0; i3 < 8; i3++) {
            if (i3 > 0)
              stream.print(" ");
            int i = 16*i1 + 8*i2 + i3;
            int j = charset[i];
            stream.print("0x"+toHexString4(j)+(i<255?",":" "));
          }
          stream.println();
        }
      }
      stream.println("};");
      stream.println();
      boolean[] pages = new boolean[256];
      for (int i = 0; i < 256; i++)
        if (charset[i] != 0xfffd)
          pages[charset[i]>>8] = true;
      stream.println("#define uni2charset "+charsetname.replace('-','_')+"_uni2charset");
      for (int p = 0; p < 256; p++)
        if (pages[p])
          stream.println("#define page"+toHexString2(p)+" "+charsetname.replace('-','_')+"_page"+toHexString2(p));
      stream.println();
      for (int p = 0; p < 256; p++)
        if (pages[p]) {
          stream.println("static const unsigned char page"+toHexString2(p)+"[256] = {");
          for (int j1 = 0; j1 < 32; j1++) {
            stream.print("  ");
            for (int j2 = 0; j2 < 8; j2++) {
              int j = 256*p + 8*j1 + j2;
              int i;
              if (j != 0xfffd) {
                for (i = 0; i < 256; i++)
                  if (charset[i] == j)
                    break;
                if (i == 256)
                  i = 0;
              } else
                i = 0;
              stream.print("0x"+toHexString2(i)+(8*j1+j2<255?",":" ")+" ");
            }
            stream.println("/* 0x"+toHexString2(8*j1)+"-0x"+toHexString2(8*j1+7)+" */");
          }
          stream.println("};");
          stream.println();
        }
      stream.println("static const unsigned char * const uni2charset[256] = {");
      for (int p1 = 0; p1 < 32; p1++) {
        stream.print("  ");
        for (int p2 = 0; p2 < 8; p2++) {
          int p = 8*p1 + p2;
          if (pages[p])
            stream.print("page"+toHexString2(p));
          else
            stream.print("nopage");
          stream.print((p<255?",":" ")+" ");
        }
        stream.println();
      }
      stream.println("};");
      stream.println();
      for (int p = 255; p >= 0; p--)
        if (pages[p])
          stream.println("#undef page"+toHexString2(p));
      stream.println();
      stream.println("struct nls_table nls_"+charsetname.replace('-','_')+"_table = {");
      stream.println("  \""+charsetname+"\",");
      stream.println("  uni2charset,");
      stream.println("  charset2uni,");
      {
        boolean is_ascii_extension = true;
        for (int i = 0; i < 128; i++)
          if (charset[i] != i) {
            is_ascii_extension = false;
            break;
          }
        stream.println("  "+(is_ascii_extension?"1":"0"));
      }
      stream.println("};");
      stream.println();
      stream.println("#undef uni2charset");
      stream.println("#undef charset2uni");
      stream.println();
      stream.close();
      fstream.close();
    }
    System.exit(0);
  }
}
