public class Main {

    static int pow2(int n) {
        int res = 1;
        int i = 0;
        while (i < n) {
            res = res * 2;
            i = i + 1;
        }
        return res;
    }

    static int bit_and(int a, int b) {
        int x = a;
        int y = b;
        int res_1 = 0;
        int bit = 1;
        while (x > 0 || y > 0) {
            if (Math.floorMod(x, 2) == 1 && Math.floorMod(y, 2) == 1) {
                res_1 = res_1 + bit;
            }
            x = ((Number)((Math.floorDiv(x, 2)))).intValue();
            y = ((Number)((Math.floorDiv(y, 2)))).intValue();
            bit = bit * 2;
        }
        return res_1;
    }

    static int bit_or(int a, int b) {
        int x_1 = a;
        int y_1 = b;
        int res_2 = 0;
        int bit_1 = 1;
        while (x_1 > 0 || y_1 > 0) {
            if (Math.floorMod(x_1, 2) == 1 || Math.floorMod(y_1, 2) == 1) {
                res_2 = res_2 + bit_1;
            }
            x_1 = ((Number)((Math.floorDiv(x_1, 2)))).intValue();
            y_1 = ((Number)((Math.floorDiv(y_1, 2)))).intValue();
            bit_1 = bit_1 * 2;
        }
        return res_2;
    }

    static int char_to_index(String ch) {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        int i_1 = 0;
        while (i_1 < _runeLen(letters)) {
            if ((letters.substring(i_1, i_1 + 1).equals(ch))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return 26;
    }

    static int bitap_string_match(String text, String pattern) {
        if ((pattern.equals(""))) {
            return 0;
        }
        int m = _runeLen(pattern);
        if (m > _runeLen(text)) {
            return -1;
        }
        int limit = pow2(m + 1);
        int all_ones = limit - 1;
        int[] pattern_mask = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < 27) {
            pattern_mask = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pattern_mask), java.util.stream.IntStream.of(all_ones)).toArray()));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < m) {
            String ch = pattern.substring(i_2, i_2 + 1);
            int idx = char_to_index(ch);
pattern_mask[idx] = bit_and(pattern_mask[idx], all_ones - pow2(i_2));
            i_2 = i_2 + 1;
        }
        int state = all_ones - 1;
        i_2 = 0;
        while (i_2 < _runeLen(text)) {
            String ch_1 = text.substring(i_2, i_2 + 1);
            int idx_1 = char_to_index(ch_1);
            state = bit_or(state, pattern_mask[idx_1]);
            state = Math.floorMod((state * 2), limit);
            if (bit_and(state, pow2(m)) == 0) {
                return i_2 - m + 1;
            }
            i_2 = i_2 + 1;
        }
        return -1;
    }

    static void main() {
        System.out.println(_p(bitap_string_match("abdabababc", "ababc")));
        System.out.println(_p(bitap_string_match("abdabababc", "")));
        System.out.println(_p(bitap_string_match("abdabababc", "c")));
        System.out.println(_p(bitap_string_match("abdabababc", "fofosdfo")));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
