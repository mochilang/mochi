public class Main {

    static int min3(int a, int b, int c) {
        int m = a;
        if (b < m) {
            m = b;
        }
        if (c < m) {
            m = c;
        }
        return m;
    }

    static int helper(String word1, String word2, int[][] cache, int i, int j, int len1, int len2) {
        if (i >= len1) {
            return len2 - j;
        }
        if (j >= len2) {
            return len1 - i;
        }
        if (cache[i][j] != (0 - 1)) {
            return cache[i][j];
        }
        int diff = 0;
        if (!(_substr(word1, i, i + 1).equals(_substr(word2, j, j + 1)))) {
            diff = 1;
        }
        int delete_cost = 1 + helper(word1, word2, ((int[][])(cache)), i + 1, j, len1, len2);
        int insert_cost = 1 + helper(word1, word2, ((int[][])(cache)), i, j + 1, len1, len2);
        int replace_cost = diff + helper(word1, word2, ((int[][])(cache)), i + 1, j + 1, len1, len2);
cache[i][j] = min3(delete_cost, insert_cost, replace_cost);
        return cache[i][j];
    }

    static int min_distance_up_bottom(String word1, String word2) {
        int len1 = _runeLen(word1);
        int len2 = _runeLen(word2);
        int[][] cache = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < len1; _v++) {
            int[] row = ((int[])(new int[]{}));
            for (int _2 = 0; _2 < len2; _2++) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0 - 1)).toArray()));
            }
            cache = ((int[][])(appendObj(cache, row)));
        }
        return helper(word1, word2, ((int[][])(cache)), 0, 0, len1, len2);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(min_distance_up_bottom("intention", "execution")));
            System.out.println(_p(min_distance_up_bottom("intention", "")));
            System.out.println(_p(min_distance_up_bottom("", "")));
            System.out.println(_p(min_distance_up_bottom("zooicoarchaeologist", "zoologist")));
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
