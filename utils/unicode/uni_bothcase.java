/**
 * Program to print those characters which have strange toUpperCase/toLowerCase
 * mappings.
 *
 * @author Bruno Haible
 */
// Common Lisp (see ANSI CL section 13.1.4.3) knows only about characters
// which have both case or are in bijection with a character of the opposite
// case.
public class uni_bothcase {
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
  public static boolean isCLBothCase (char ch) {
    return Character.toLowerCase(ch) != Character.toUpperCase(ch);
  }
  public static void main (String[] args) {
    for (int i = 0; i < 0x10000; i++) {
      char ch = (char)i;
      char ch1 = Character.toUpperCase(ch);
      char ch2 = Character.toLowerCase(ch);
      if (!(ch1 == ch || ch2 == ch))
        System.out.println("character 0x"+toHexString4(ch)+" is neither upper nor lower case");
      else {
        if (isCLBothCase(ch)) {
          // Check whether
          //   toUpper(toUpper(ch)) == toUpper(ch)
          //   toUpper(toLower(ch)) == toUpper(ch)
          //   toLower(toUpper(ch)) == toLower(ch)
          //   toLower(toLower(ch)) == toLower(ch)
          if (ch == ch1) {
            // ch is upper case
            if (!(Character.toLowerCase(ch2) == ch2))
              System.out.println("character 0x"+toHexString4(ch)+" has a lower case mapping 0x"+toHexString4(ch2)+" which is not lower case");
            else if (!(Character.toUpperCase(ch2) == ch))
              System.out.println("character 0x"+toHexString4(ch)+" has a lower case mapping 0x"+toHexString4(ch2)+" whose upper case mapping isn't the first character");
          } else if (ch == ch2) {
            // ch is lower case
            if (!(Character.toUpperCase(ch1) == ch1))
              System.out.println("character 0x"+toHexString4(ch)+" has an upper case mapping 0x"+toHexString4(ch1)+" which is not upper case");
            else if (!(Character.toLowerCase(ch1) == ch))
              System.out.println("character 0x"+toHexString4(ch)+" has an upper case mapping 0x"+toHexString4(ch1)+" whose lower case mapping isn't the first character");
          }
        }
      }
    }
    System.exit(0);
  }
}
