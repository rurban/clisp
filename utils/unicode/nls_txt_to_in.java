/**
 * Program for converting character set descriptions as those found on
 * ftp.unicode.org to .in files containing one character per line.
 *
 * @author Bruno Haible
 */
import java.io.*;
import java.util.*;
public class nls_txt_to_in {
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
    if (args.length != 0)
      System.exit(1);
    int[] charset = new int[256];
    // Unassigned entries are mapped to 0xfffd.
    for (int i = 0; i < 256; i++)
      charset[i] = 0xfffd;
    // Some of the .TXT files lack the ASCII characters.
    for (int i = 0; i < 128; i++)
      charset[i] = i;
    // Read and interpret the .TXT file line by line.
    {
      BufferedReader stream = new BufferedReader(new InputStreamReader(System.in));
      for (;;) {
        String line = stream.readLine();
        if (line == null)
          break;
        if (line.indexOf('#') >= 0)
          line = line.substring(0,line.indexOf('#'));
        StringTokenizer tokenizer = new StringTokenizer(line," \t",false);
        if (!tokenizer.hasMoreTokens()) continue;
        String token1 = tokenizer.nextToken();
        if (!tokenizer.hasMoreTokens()) continue;
        String token2 = tokenizer.nextToken();
        if (token1.startsWith("0x") && token2.startsWith("0x")) {
          try {
            int num1 = Integer.parseInt(token1.substring(2),16);
            int num2 = Integer.parseInt(token2.substring(2),16);
            charset[num1] = num2;
          } catch (NumberFormatException e) {
            System.err.println("unknown tokens: "+token1+" "+token2);
          }
        } else
          System.err.println("unknown tokens: "+token1+" "+token2);
      }
      stream.close();
    }
    // Output the charset.
    {
      PrintStream stream = System.out;
      for (int i = 0; i < 256; i++)
        if (i != 10 && i != 13) {
          int j = charset[i];
          if (j < 128)
            stream.println((char)j);
          else
            stream.println("\\u"+toHexString4(j));
        }
    }
    System.exit(0);
  }
}
