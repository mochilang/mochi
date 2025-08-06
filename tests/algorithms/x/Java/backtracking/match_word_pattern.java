public class Main {

    static String get_value(String[] keys, String[] values, String key) {
        int i = 0;
        while (i < keys.length) {
            if ((keys[i].equals(key))) {
                return values[i];
            }
            i = i + 1;
        }
        return null;
    }

    static boolean contains_value(String[] values, String value) {
        int i_1 = 0;
        while (i_1 < values.length) {
            if ((values[i_1].equals(value))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static boolean backtrack(String pattern, String input_string, int pi, int si, String[] keys, String[] values) {
        if (pi == _runeLen(pattern) && si == _runeLen(input_string)) {
            return true;
        }
        if (pi == _runeLen(pattern) || si == _runeLen(input_string)) {
            return false;
        }
        String ch = _substr(pattern, pi, pi + 1);
        String mapped = String.valueOf(get_value(((String[])(keys)), ((String[])(values)), ch));
        if (!(mapped == null)) {
            if ((_substr(input_string, si, si + _runeLen(mapped)).equals(mapped))) {
                return backtrack(pattern, input_string, pi + 1, si + _runeLen(mapped), ((String[])(keys)), ((String[])(values)));
            }
            return false;
        }
        int end = si + 1;
        while (end <= _runeLen(input_string)) {
            String substr = _substr(input_string, si, end);
            if (((Boolean)(contains_value(((String[])(values)), substr)))) {
                end = end + 1;
                continue;
            }
            String[] new_keys = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys), java.util.stream.Stream.of(ch)).toArray(String[]::new)));
            String[] new_values = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(values), java.util.stream.Stream.of(substr)).toArray(String[]::new)));
            if (((Boolean)(backtrack(pattern, input_string, pi + 1, end, ((String[])(new_keys)), ((String[])(new_values)))))) {
                return true;
            }
            end = end + 1;
        }
        return false;
    }

    static boolean match_word_pattern(String pattern, String input_string) {
        String[] keys = ((String[])(new String[]{}));
        String[] values = ((String[])(new String[]{}));
        return backtrack(pattern, input_string, 0, 0, ((String[])(keys)), ((String[])(values)));
    }

    static void main() {
        System.out.println(match_word_pattern("aba", "GraphTreesGraph"));
        System.out.println(match_word_pattern("xyx", "PythonRubyPython"));
        System.out.println(match_word_pattern("GG", "PythonJavaPython"));
    }
    public static void main(String[] args) {
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
