public class Main {
    static String UPPER;
    static String LOWER;
    static String BASE_TOP;
    static String BASE_BOTTOM;

    static String to_upper(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            int j = 0;
            boolean replaced = false;
            while (j < _runeLen(LOWER)) {
                if ((_substr(LOWER, j, j + 1).equals(ch))) {
                    res = res + _substr(UPPER, j, j + 1);
                    replaced = true;
                    break;
                }
                j = j + 1;
            }
            if (!replaced) {
                res = res + ch;
            }
            i = i + 1;
        }
        return res;
    }

    static int char_index(String c) {
        int i_1 = 0;
        while (i_1 < _runeLen(UPPER)) {
            if ((_substr(UPPER, i_1, i_1 + 1).equals(c))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String rotate_right(String s, int k) {
        int n = _runeLen(s);
        int shift = Math.floorMod(k, n);
        return _substr(s, n - shift, n) + _substr(s, 0, n - shift);
    }

    static String[] table_for(String c) {
        int idx = char_index(c);
        int shift_1 = idx / 2;
        String row1 = String.valueOf(rotate_right(BASE_BOTTOM, shift_1));
        String[] pair = ((String[])(new String[]{BASE_TOP, row1}));
        return pair;
    }

    static String[][] generate_table(String key) {
        String up = String.valueOf(to_upper(key));
        int i_2 = 0;
        String[][] result = ((String[][])(new String[][]{}));
        while (i_2 < _runeLen(up)) {
            String ch_1 = _substr(up, i_2, i_2 + 1);
            String[] pair_1 = ((String[])(table_for(ch_1)));
            result = ((String[][])(appendObj(result, pair_1)));
            i_2 = i_2 + 1;
        }
        return result;
    }

    static int str_index(String s, String ch) {
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            if ((_substr(s, i_3, i_3 + 1).equals(ch))) {
                return i_3;
            }
            i_3 = i_3 + 1;
        }
        return 0 - 1;
    }

    static int[] get_position(String[] table, String ch) {
        int row = 0;
        if (str_index(table[0], ch) == 0 - 1) {
            row = 1;
        }
        int col = str_index(table[row], ch);
        return new int[]{row, col};
    }

    static String get_opponent(String[] table, String ch) {
        int[] pos = ((int[])(get_position(((String[])(table)), ch)));
        int row_1 = pos[0];
        int col_1 = pos[1];
        if (col_1 == 0 - 1) {
            return ch;
        }
        if (row_1 == 1) {
            return _substr(table[0], col_1, col_1 + 1);
        }
        return _substr(table[1], col_1, col_1 + 1);
    }

    static String encrypt(String key, String words) {
        String[][] table = ((String[][])(generate_table(key)));
        String up_words = String.valueOf(to_upper(words));
        String cipher = "";
        int count = 0;
        int i_4 = 0;
        while (i_4 < _runeLen(up_words)) {
            String ch_2 = _substr(up_words, i_4, i_4 + 1);
            cipher = cipher + String.valueOf(get_opponent(((String[])(table[count])), ch_2));
            count = Math.floorMod((count + 1), table.length);
            i_4 = i_4 + 1;
        }
        return cipher;
    }

    static String decrypt(String key, String words) {
        String res_1 = String.valueOf(encrypt(key, words));
        return res_1;
    }

    static void main() {
        System.out.println(encrypt("marvin", "jessica"));
        System.out.println(decrypt("marvin", "QRACRWU"));
    }
    public static void main(String[] args) {
        UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        LOWER = "abcdefghijklmnopqrstuvwxyz";
        BASE_TOP = "ABCDEFGHIJKLM";
        BASE_BOTTOM = "NOPQRSTUVWXYZ";
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
