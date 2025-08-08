public class Main {

    static int find_index(String[] xs, String x) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(x))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String get_word_pattern(String word) {
        String w = word.toUpperCase();
        String[] letters = ((String[])(new String[]{}));
        String[] numbers = ((String[])(new String[]{}));
        int next_num = 0;
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(w)) {
            String ch = w.substring(i_1, i_1+1);
            int idx = find_index(((String[])(letters)), ch);
            String num_str = null;
            if (idx >= 0) {
                num_str = numbers[idx];
            } else {
                num_str = _p(next_num);
                letters = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(letters), java.util.stream.Stream.of(ch)).toArray(String[]::new)));
                numbers = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers), java.util.stream.Stream.of(num_str)).toArray(String[]::new)));
                next_num = next_num + 1;
            }
            if (i_1 > 0) {
                res = res + ".";
            }
            res = res + num_str;
            i_1 = i_1 + 1;
        }
        return res;
    }

    static void main() {
        System.out.println(get_word_pattern(""));
        System.out.println(get_word_pattern(" "));
        System.out.println(get_word_pattern("pattern"));
        System.out.println(get_word_pattern("word pattern"));
        System.out.println(get_word_pattern("get word pattern"));
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
