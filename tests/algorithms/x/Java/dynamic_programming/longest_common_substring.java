public class Main {

    static String longest_common_substring(String text1, String text2) {
        if (_runeLen(text1) == 0 || _runeLen(text2) == 0) {
            return "";
        }
        int m = _runeLen(text1);
        int n = _runeLen(text2);
        int[][] dp = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < m + 1) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n + 1) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            dp = ((int[][])(appendObj(dp, row)));
            i = i + 1;
        }
        int end_pos = 0;
        int max_len = 0;
        int ii = 1;
        while (ii <= m) {
            int jj = 1;
            while (jj <= n) {
                if ((_substr(text1, ii - 1, ii).equals(_substr(text2, jj - 1, jj)))) {
dp[ii][jj] = 1 + dp[ii - 1][jj - 1];
                    if (dp[ii][jj] > max_len) {
                        max_len = dp[ii][jj];
                        end_pos = ii;
                    }
                }
                jj = jj + 1;
            }
            ii = ii + 1;
        }
        return _substr(text1, end_pos - max_len, end_pos);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(longest_common_substring("abcdef", "xabded"));
            System.out.println("\n");
            System.out.println(longest_common_substring("zxabcdezy", "yzabcdezx"));
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
}
