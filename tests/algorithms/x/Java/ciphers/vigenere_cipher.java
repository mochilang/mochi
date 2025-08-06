public class Main {
    static String LETTERS;
    static String LETTERS_LOWER;
    static String key;
    static String message;
    static String key_up;
    static String encrypted = null;
    static int key_index = 0;
    static int i_2 = 0;
    static String decrypted = null;

    static int find_index(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String to_upper_char(String ch) {
        int idx = find_index(LETTERS_LOWER, ch);
        if (idx >= 0) {
            return LETTERS.substring(idx, idx+1);
        }
        return ch;
    }

    static String to_lower_char(String ch) {
        int idx_1 = find_index(LETTERS, ch);
        if (idx_1 >= 0) {
            return LETTERS_LOWER.substring(idx_1, idx_1+1);
        }
        return ch;
    }

    static boolean is_upper(String ch) {
        return find_index(LETTERS, ch) >= 0;
    }

    static String to_upper_string(String s) {
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            res = res + String.valueOf(to_upper_char(s.substring(i_1, i_1+1)));
            i_1 = i_1 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        LETTERS_LOWER = "abcdefghijklmnopqrstuvwxyz";
        key = "HDarji";
        message = "This is Harshil Darji from Dharmaj.";
        key_up = String.valueOf(to_upper_string(key));
        encrypted = "";
        key_index = 0;
        i_2 = 0;
        while (i_2 < _runeLen(message)) {
            String symbol = message.substring(i_2, i_2+1);
            String upper_symbol = String.valueOf(to_upper_char(symbol));
            int num = find_index(LETTERS, upper_symbol);
            if (num >= 0) {
                num = num + find_index(LETTERS, key_up.substring(key_index, key_index+1));
                num = Math.floorMod(num, _runeLen(LETTERS));
                if (((Boolean)(is_upper(symbol)))) {
                    encrypted = encrypted + LETTERS.substring(num, num+1);
                } else {
                    encrypted = encrypted + String.valueOf(to_lower_char(LETTERS.substring(num, num+1)));
                }
                key_index = key_index + 1;
                if (key_index == _runeLen(key_up)) {
                    key_index = 0;
                }
            } else {
                encrypted = encrypted + symbol;
            }
            i_2 = i_2 + 1;
        }
        System.out.println(encrypted);
        decrypted = "";
        key_index = 0;
        i_2 = 0;
        while (i_2 < _runeLen(encrypted)) {
            String symbol_1 = encrypted.substring(i_2, i_2+1);
            String upper_symbol_1 = String.valueOf(to_upper_char(symbol_1));
            int num_1 = find_index(LETTERS, upper_symbol_1);
            if (num_1 >= 0) {
                num_1 = num_1 - find_index(LETTERS, key_up.substring(key_index, key_index+1));
                num_1 = Math.floorMod(num_1, _runeLen(LETTERS));
                if (((Boolean)(is_upper(symbol_1)))) {
                    decrypted = decrypted + LETTERS.substring(num_1, num_1+1);
                } else {
                    decrypted = decrypted + String.valueOf(to_lower_char(LETTERS.substring(num_1, num_1+1)));
                }
                key_index = key_index + 1;
                if (key_index == _runeLen(key_up)) {
                    key_index = 0;
                }
            } else {
                decrypted = decrypted + symbol_1;
            }
            i_2 = i_2 + 1;
        }
        System.out.println(decrypted);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
