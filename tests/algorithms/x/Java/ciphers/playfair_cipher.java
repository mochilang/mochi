public class Main {

    static boolean contains(String[] xs, String x) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(x))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int index_of(String[] xs, String x) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if ((xs[i_1].equals(x))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String prepare_input(String dirty) {
        String letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String upper_dirty = dirty.toUpperCase();
        String filtered = "";
        int i_2 = 0;
        while (i_2 < _runeLen(upper_dirty)) {
            String c = _substr(upper_dirty, i_2, i_2 + 1);
            if (letters.contains(c)) {
                filtered = filtered + c;
            }
            i_2 = i_2 + 1;
        }
        if (_runeLen(filtered) < 2) {
            return filtered;
        }
        String clean = "";
        i_2 = 0;
        while (i_2 < _runeLen(filtered) - 1) {
            String c1 = _substr(filtered, i_2, i_2 + 1);
            String c2 = _substr(filtered, i_2 + 1, i_2 + 2);
            clean = clean + c1;
            if ((c1.equals(c2))) {
                clean = clean + "X";
            }
            i_2 = i_2 + 1;
        }
        clean = clean + _substr(filtered, _runeLen(filtered) - 1, _runeLen(filtered));
        if (Math.floorMod(_runeLen(clean), 2) == 1) {
            clean = clean + "X";
        }
        return clean;
    }

    static String[] generate_table(String key) {
        String alphabet = "ABCDEFGHIKLMNOPQRSTUVWXYZ";
        String[] table = ((String[])(new String[]{}));
        String upper_key = key.toUpperCase();
        int i_3 = 0;
        while (i_3 < _runeLen(upper_key)) {
            String c_1 = _substr(upper_key, i_3, i_3 + 1);
            if (alphabet.contains(c_1)) {
                if (!(Boolean)(contains(((String[])(table)), c_1))) {
                    table = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(table), java.util.stream.Stream.of(c_1)).toArray(String[]::new)));
                }
            }
            i_3 = i_3 + 1;
        }
        i_3 = 0;
        while (i_3 < _runeLen(alphabet)) {
            String c_2 = _substr(alphabet, i_3, i_3 + 1);
            if (!(Boolean)(contains(((String[])(table)), c_2))) {
                table = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(table), java.util.stream.Stream.of(c_2)).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return table;
    }

    static String encode(String plaintext, String key) {
        String[] table_1 = ((String[])(generate_table(key)));
        String text = String.valueOf(prepare_input(plaintext));
        String cipher = "";
        int i_4 = 0;
        while (i_4 < _runeLen(text)) {
            String c1_1 = _substr(text, i_4, i_4 + 1);
            String c2_1 = _substr(text, i_4 + 1, i_4 + 2);
            int idx1 = index_of(((String[])(table_1)), c1_1);
            int idx2 = index_of(((String[])(table_1)), c2_1);
            int row1 = idx1 / 5;
            int col1 = Math.floorMod(idx1, 5);
            int row2 = idx2 / 5;
            int col2 = Math.floorMod(idx2, 5);
            if (row1 == row2) {
                cipher = cipher + table_1[row1 * 5 + Math.floorMod((col1 + 1), 5)];
                cipher = cipher + table_1[row2 * 5 + Math.floorMod((col2 + 1), 5)];
            } else             if (col1 == col2) {
                cipher = cipher + table_1[(Math.floorMod((row1 + 1), 5)) * 5 + col1];
                cipher = cipher + table_1[(Math.floorMod((row2 + 1), 5)) * 5 + col2];
            } else {
                cipher = cipher + table_1[row1 * 5 + col2];
                cipher = cipher + table_1[row2 * 5 + col1];
            }
            i_4 = i_4 + 2;
        }
        return cipher;
    }

    static String decode(String cipher, String key) {
        String[] table_2 = ((String[])(generate_table(key)));
        String plain = "";
        int i_5 = 0;
        while (i_5 < _runeLen(cipher)) {
            String c1_2 = _substr(cipher, i_5, i_5 + 1);
            String c2_2 = _substr(cipher, i_5 + 1, i_5 + 2);
            int idx1_1 = index_of(((String[])(table_2)), c1_2);
            int idx2_1 = index_of(((String[])(table_2)), c2_2);
            int row1_1 = idx1_1 / 5;
            int col1_1 = Math.floorMod(idx1_1, 5);
            int row2_1 = idx2_1 / 5;
            int col2_1 = Math.floorMod(idx2_1, 5);
            if (row1_1 == row2_1) {
                plain = plain + table_2[row1_1 * 5 + Math.floorMod((col1_1 + 4), 5)];
                plain = plain + table_2[row2_1 * 5 + Math.floorMod((col2_1 + 4), 5)];
            } else             if (col1_1 == col2_1) {
                plain = plain + table_2[(Math.floorMod((row1_1 + 4), 5)) * 5 + col1_1];
                plain = plain + table_2[(Math.floorMod((row2_1 + 4), 5)) * 5 + col2_1];
            } else {
                plain = plain + table_2[row1_1 * 5 + col2_1];
                plain = plain + table_2[row2_1 * 5 + col1_1];
            }
            i_5 = i_5 + 2;
        }
        return plain;
    }

    static void main() {
        System.out.println("Encoded:" + " " + String.valueOf(encode("BYE AND THANKS", "GREETING")));
        System.out.println("Decoded:" + " " + String.valueOf(decode("CXRBANRLBALQ", "GREETING")));
    }
    public static void main(String[] args) {
        main();
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
