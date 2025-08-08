public class Main {
    static String lowercase;
    static String uppercase;

    static int index_of(String s, String c) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(c))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static boolean is_alpha(String c) {
        return index_of(lowercase, c) >= 0 || index_of(uppercase, c) >= 0;
    }

    static String to_upper(String c) {
        int idx = index_of(lowercase, c);
        if (idx >= 0) {
            return _substr(uppercase, idx, idx + 1);
        }
        return c;
    }

    static String[] wave(String txt) {
        String[] result = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(txt)) {
            String ch = _substr(txt, i_1, i_1 + 1);
            if (((Boolean)(is_alpha(ch)))) {
                String prefix = _substr(txt, 0, i_1);
                String suffix = _substr(txt, i_1 + 1, _runeLen(txt));
                result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(prefix + String.valueOf(to_upper(ch)) + suffix)).toArray(String[]::new)));
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        lowercase = "abcdefghijklmnopqrstuvwxyz";
        uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        System.out.println(_p(wave("cat")));
        System.out.println(_p(wave("one")));
        System.out.println(_p(wave("book")));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
