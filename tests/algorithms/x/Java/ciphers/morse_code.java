public class Main {
    static String[] CHARS;
    static String[] CODES;
    static String msg_1;
    static String enc;
    static String dec;

    static String to_upper_char(String c) {
        if ((c.equals("a"))) {
            return "A";
        }
        if ((c.equals("b"))) {
            return "B";
        }
        if ((c.equals("c"))) {
            return "C";
        }
        if ((c.equals("d"))) {
            return "D";
        }
        if ((c.equals("e"))) {
            return "E";
        }
        if ((c.equals("f"))) {
            return "F";
        }
        if ((c.equals("g"))) {
            return "G";
        }
        if ((c.equals("h"))) {
            return "H";
        }
        if ((c.equals("i"))) {
            return "I";
        }
        if ((c.equals("j"))) {
            return "J";
        }
        if ((c.equals("k"))) {
            return "K";
        }
        if ((c.equals("l"))) {
            return "L";
        }
        if ((c.equals("m"))) {
            return "M";
        }
        if ((c.equals("n"))) {
            return "N";
        }
        if ((c.equals("o"))) {
            return "O";
        }
        if ((c.equals("p"))) {
            return "P";
        }
        if ((c.equals("q"))) {
            return "Q";
        }
        if ((c.equals("r"))) {
            return "R";
        }
        if ((c.equals("s"))) {
            return "S";
        }
        if ((c.equals("t"))) {
            return "T";
        }
        if ((c.equals("u"))) {
            return "U";
        }
        if ((c.equals("v"))) {
            return "V";
        }
        if ((c.equals("w"))) {
            return "W";
        }
        if ((c.equals("x"))) {
            return "X";
        }
        if ((c.equals("y"))) {
            return "Y";
        }
        if ((c.equals("z"))) {
            return "Z";
        }
        return c;
    }

    static String to_upper(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            res = res + String.valueOf(to_upper_char(s.substring(i, i+1)));
            i = i + 1;
        }
        return res;
    }

    static int index_of(String[] xs, String target) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if ((xs[i_1].equals(target))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String encrypt(String message) {
        String msg = String.valueOf(to_upper(message));
        String res_1 = "";
        int i_2 = 0;
        while (i_2 < _runeLen(msg)) {
            String c = msg.substring(i_2, i_2+1);
            int idx = index_of(((String[])(CHARS)), c);
            if (idx >= 0) {
                if (!(res_1.equals(""))) {
                    res_1 = res_1 + " ";
                }
                res_1 = res_1 + CODES[idx];
            }
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static String[] split_spaces(String s) {
        String[] res_2 = ((String[])(new String[]{}));
        String current = "";
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            String ch = s.substring(i_3, i_3+1);
            if ((ch.equals(" "))) {
                if (!(current.equals(""))) {
                    res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                    current = "";
                }
            } else {
                current = current + ch;
            }
            i_3 = i_3 + 1;
        }
        if (!(current.equals(""))) {
            res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        }
        return res_2;
    }

    static String decrypt(String message) {
        String[] parts = ((String[])(split_spaces(message)));
        String res_3 = "";
        for (String code : parts) {
            int idx_1 = index_of(((String[])(CODES)), code);
            if (idx_1 >= 0) {
                res_3 = res_3 + CHARS[idx_1];
            }
        }
        return res_3;
    }
    public static void main(String[] args) {
        CHARS = ((String[])(new String[]{"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "&", "@", ":", ",", ".", "'", "\"", "?", "/", "=", "+", "-", "(", ")", "!", " "}));
        CODES = ((String[])(new String[]{".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--..", ".----", "..---", "...--", "....-", ".....", "-....", "--...", "---..", "----.", "-----", ".-...", ".--.-.", "---...", "--..--", ".-.-.-", ".----.", ".-..-.", "..--..", "-..-.", "-...-", ".-.-.", "-....-", "-.--.", "-.--.-", "-.-.--", "/"}));
        msg_1 = "Morse code here!";
        System.out.println(msg_1);
        enc = String.valueOf(encrypt(msg_1));
        System.out.println(enc);
        dec = String.valueOf(decrypt(enc));
        System.out.println(dec);
        System.out.println(encrypt("Sos!"));
        System.out.println(decrypt("... --- ... -.-.--"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
