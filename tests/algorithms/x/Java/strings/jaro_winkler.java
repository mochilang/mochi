public class Main {

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        } else {
            return b;
        }
    }

    static int max_int(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    static boolean[] repeat_bool(int n, boolean value) {
        boolean[] res = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < n) {
            res = ((boolean[])(appendBool(res, ((Boolean)(value)))));
            i = i + 1;
        }
        return res;
    }

    static boolean[] set_bool(boolean[] xs, int idx, boolean value) {
        boolean[] res_1 = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 == idx) {
                res_1 = ((boolean[])(appendBool(res_1, ((Boolean)(value)))));
            } else {
                res_1 = ((boolean[])(appendBool(res_1, ((Boolean)(xs[i_1])))));
            }
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static double jaro_winkler(String s1, String s2) {
        int len1 = _runeLen(s1);
        int len2 = _runeLen(s2);
        int limit = Math.floorDiv(min_int(len1, len2), 2);
        boolean[] match1 = ((boolean[])(repeat_bool(len1, false)));
        boolean[] match2 = ((boolean[])(repeat_bool(len2, false)));
        int matches = 0;
        int i_2 = 0;
        while (i_2 < len1) {
            int start = max_int(0, i_2 - limit);
            int end = min_int(i_2 + limit + 1, len2);
            int j = start;
            while (j < end) {
                if (!match2[j] && (_substr(s1, i_2, i_2 + 1).equals(_substr(s2, j, j + 1)))) {
                    match1 = ((boolean[])(set_bool(((boolean[])(match1)), i_2, true)));
                    match2 = ((boolean[])(set_bool(((boolean[])(match2)), j, true)));
                    matches = matches + 1;
                    break;
                }
                j = j + 1;
            }
            i_2 = i_2 + 1;
        }
        if (matches == 0) {
            return 0.0;
        }
        int transpositions = 0;
        int k = 0;
        i_2 = 0;
        while (i_2 < len1) {
            if (match1[i_2]) {
                while (!match2[k]) {
                    k = k + 1;
                }
                if (!(_substr(s1, i_2, i_2 + 1).equals(_substr(s2, k, k + 1)))) {
                    transpositions = transpositions + 1;
                }
                k = k + 1;
            }
            i_2 = i_2 + 1;
        }
        double m = ((Number)(matches)).doubleValue();
        double jaro = ((m / (((Number)(len1)).doubleValue())) + (m / (((Number)(len2)).doubleValue())) + ((m - (((Number)(transpositions)).doubleValue()) / 2.0) / m)) / 3.0;
        int prefix_len = 0;
        i_2 = 0;
        while (i_2 < 4 && i_2 < len1 && i_2 < len2) {
            if ((_substr(s1, i_2, i_2 + 1).equals(_substr(s2, i_2, i_2 + 1)))) {
                prefix_len = prefix_len + 1;
            } else {
                break;
            }
            i_2 = i_2 + 1;
        }
        return jaro + 0.1 * (((Number)(prefix_len)).doubleValue()) * (1.0 - jaro);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(jaro_winkler("hello", "world")));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
