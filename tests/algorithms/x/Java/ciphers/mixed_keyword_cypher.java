public class Main {
    static String UPPER;
    static String LOWER;

    static String to_upper(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i+1);
            int j = 0;
            boolean found = false;
            while (j < 26) {
                if ((ch.equals(LOWER.substring(j, j+1)))) {
                    res = res + UPPER.substring(j, j+1);
                    found = true;
                    break;
                }
                j = j + 1;
            }
            if (found == false) {
                res = res + ch;
            }
            i = i + 1;
        }
        return res;
    }

    static boolean contains(String[] xs, String x) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if ((xs[i_1].equals(x))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static boolean contains_char(String s, String ch) {
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            if ((s.substring(i_2, i_2+1).equals(ch))) {
                return true;
            }
            i_2 = i_2 + 1;
        }
        return false;
    }

    static String get_value(String[] keys, String[] values, String key) {
        int i_3 = 0;
        while (i_3 < keys.length) {
            if ((keys[i_3].equals(key))) {
                return values[i_3];
            }
            i_3 = i_3 + 1;
        }
        return null;
    }

    static void print_mapping(String[] keys, String[] values) {
        String s = "{";
        int i_4 = 0;
        while (i_4 < keys.length) {
            s = s + "'" + keys[i_4] + "': '" + values[i_4] + "'";
            if (i_4 + 1 < keys.length) {
                s = s + ", ";
            }
            i_4 = i_4 + 1;
        }
        s = s + "}";
        System.out.println(s);
    }

    static String mixed_keyword(String keyword, String plaintext, boolean verbose) {
        String alphabet = UPPER;
        String keyword_u = String.valueOf(to_upper(keyword));
        String plaintext_u = String.valueOf(to_upper(plaintext));
        String[] unique = ((String[])(new String[]{}));
        int i_5 = 0;
        while (i_5 < _runeLen(keyword_u)) {
            String ch_1 = keyword_u.substring(i_5, i_5+1);
            if (((Boolean)(contains_char(alphabet, ch_1))) && contains(((String[])(unique)), ch_1) == false) {
                unique = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(unique), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            }
            i_5 = i_5 + 1;
        }
        int num_unique = unique.length;
        String[] shifted = ((String[])(new String[]{}));
        i_5 = 0;
        while (i_5 < unique.length) {
            shifted = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(shifted), java.util.stream.Stream.of(unique[i_5])).toArray(String[]::new)));
            i_5 = i_5 + 1;
        }
        i_5 = 0;
        while (i_5 < _runeLen(alphabet)) {
            String ch_2 = alphabet.substring(i_5, i_5+1);
            if (contains(((String[])(unique)), ch_2) == false) {
                shifted = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(shifted), java.util.stream.Stream.of(ch_2)).toArray(String[]::new)));
            }
            i_5 = i_5 + 1;
        }
        String[][] modified = ((String[][])(new String[][]{}));
        int k = 0;
        while (k < shifted.length) {
            String[] row = ((String[])(new String[]{}));
            int r = 0;
            while (r < num_unique && k + r < shifted.length) {
                row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(shifted[k + r])).toArray(String[]::new)));
                r = r + 1;
            }
            modified = ((String[][])(appendObj(modified, row)));
            k = k + num_unique;
        }
        String[] keys = ((String[])(new String[]{}));
        String[] values = ((String[])(new String[]{}));
        int column = 0;
        int letter_index = 0;
        while (column < num_unique) {
            int row_idx = 0;
            while (row_idx < modified.length) {
                String[] row_1 = ((String[])(modified[row_idx]));
                if (row_1.length <= column) {
                    break;
                }
                keys = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys), java.util.stream.Stream.of(alphabet.substring(letter_index, letter_index+1))).toArray(String[]::new)));
                values = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(values), java.util.stream.Stream.of(row_1[column])).toArray(String[]::new)));
                letter_index = letter_index + 1;
                row_idx = row_idx + 1;
            }
            column = column + 1;
        }
        if (((Boolean)(verbose))) {
            print_mapping(((String[])(keys)), ((String[])(values)));
        }
        String result = "";
        i_5 = 0;
        while (i_5 < _runeLen(plaintext_u)) {
            String ch_3 = plaintext_u.substring(i_5, i_5+1);
            String mapped = String.valueOf(get_value(((String[])(keys)), ((String[])(values)), ch_3));
            if ((mapped == null)) {
                result = result + ch_3;
            } else {
                result = result + mapped;
            }
            i_5 = i_5 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        LOWER = "abcdefghijklmnopqrstuvwxyz";
        System.out.println(mixed_keyword("college", "UNIVERSITY", true));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
