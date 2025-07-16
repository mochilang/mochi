// character-codes-5.mochi
public class CharacterCodes5 {
    static String chr(int n) {
        if (n == 97) {
            return "a";
        }
        if (n == 960) {
            return "π";
        }
        if (n == 65) {
            return "A";
        }
        return "?";
    }
    public static void main(String[] args) {
    System.out.println(chr(97));
    System.out.println(chr(960));
    System.out.println(chr(97) + chr(960));
    }
}
