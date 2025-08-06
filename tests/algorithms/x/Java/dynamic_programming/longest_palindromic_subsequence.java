public class Main {

    static String reverse(String s) {
        String result = "";
        int i = _runeLen(s) - 1;
        while (i >= 0) {
            result = result + s.substring(i, i + 1);
            i = i - 1;
        }
        return result;
    }

    static int max_int(int a, int b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static int longest_palindromic_subsequence(String s) {
        String rev = String.valueOf(reverse(s));
        int n = _runeLen(s);
        int m = _runeLen(rev);
        int[][] dp = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 <= n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j <= m) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            dp = ((int[][])(appendObj(dp, row)));
            i_1 = i_1 + 1;
        }
        i_1 = 1;
        while (i_1 <= n) {
            int j_1 = 1;
            while (j_1 <= m) {
                String a_char = s.substring(i_1 - 1, i_1);
                String b_char = rev.substring(j_1 - 1, j_1);
                if ((a_char.equals(b_char))) {
dp[i_1][j_1] = 1 + dp[i_1 - 1][j_1 - 1];
                } else {
dp[i_1][j_1] = max_int(dp[i_1 - 1][j_1], dp[i_1][j_1 - 1]);
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return dp[n][m];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(longest_palindromic_subsequence("bbbab")));
            System.out.println(_p(longest_palindromic_subsequence("bbabcbcab")));
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
