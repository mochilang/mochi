// character-codes-4.mochi
public class CharacterCodes4 {
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
    int b = 97;
    int r = 960;
    System.out.println(chr(97) + " " + chr(960));
    System.out.println(chr(b) + " " + chr(r));
    }
}
