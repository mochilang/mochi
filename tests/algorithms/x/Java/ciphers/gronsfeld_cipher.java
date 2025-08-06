public class Main {
    static String ASCII_UPPERCASE;
    static String ASCII_LOWERCASE;
    static int NEG_ONE;

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return NEG_ONE;
    }

    static String to_uppercase(String s) {
        String result = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            int idx = index_of(ASCII_LOWERCASE, ch);
            if (idx == NEG_ONE) {
                result = result + ch;
            } else {
                result = result + _substr(ASCII_UPPERCASE, idx, idx + 1);
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static String gronsfeld(String text, String key) {
        int ascii_len = _runeLen(ASCII_UPPERCASE);
        int key_len = _runeLen(key);
        if (key_len == 0) {
            throw new RuntimeException(String.valueOf("integer modulo by zero"));
        }
        String upper_text = String.valueOf(to_uppercase(text));
        String encrypted = "";
        int i_2 = 0;
        while (i_2 < _runeLen(upper_text)) {
            String ch_1 = _substr(upper_text, i_2, i_2 + 1);
            int idx_1 = index_of(ASCII_UPPERCASE, ch_1);
            if (idx_1 == NEG_ONE) {
                encrypted = encrypted + ch_1;
            } else {
                int key_idx = Math.floorMod(i_2, key_len);
                int shift = Integer.parseInt(_substr(key, key_idx, key_idx + 1));
                int new_position = Math.floorMod((idx_1 + shift), ascii_len);
                encrypted = encrypted + _substr(ASCII_UPPERCASE, new_position, new_position + 1);
            }
            i_2 = i_2 + 1;
        }
        return encrypted;
    }
    public static void main(String[] args) {
        ASCII_UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        ASCII_LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
        NEG_ONE = 0 - 1;
        System.out.println(gronsfeld("hello", "412"));
        System.out.println(gronsfeld("hello", "123"));
        System.out.println(gronsfeld("", "123"));
        System.out.println(gronsfeld("yes, ¥€$ - _!@#%?", "0"));
        System.out.println(gronsfeld("yes, ¥€$ - _!@#%?", "01"));
        System.out.println(gronsfeld("yes, ¥€$ - _!@#%?", "012"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
