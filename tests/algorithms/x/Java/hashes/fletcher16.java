public class Main {
    static String ascii_chars;

    static long ord(String ch) {
        long i = 0L;
        while (i < _runeLen(ascii_chars)) {
            if ((ascii_chars.substring((int)((long)(i)), (int)((long)(i))+1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static long fletcher16(String text) {
        long sum1 = 0L;
        long sum2_1 = 0L;
        long i_2 = 0L;
        while (i_2 < _runeLen(text)) {
            long code_1 = ord(text.substring((int)((long)(i_2)), (int)((long)(i_2))+1));
            sum1 = Math.floorMod((sum1 + code_1), 255);
            sum2_1 = Math.floorMod((sum1 + sum2_1), 255);
            i_2 = i_2 + 1;
        }
        return sum2_1 * 256 + sum1;
    }
    public static void main(String[] args) {
        ascii_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
