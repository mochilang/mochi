public class Main {

    static int find_min(int[] numbers) {
        int n = numbers.length;
        int s = 0;
        int idx = 0;
        while (idx < n) {
            s = s + numbers[idx];
            idx = idx + 1;
        }
        boolean[][] dp = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i <= n) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j <= s) {
                row = ((boolean[])(appendBool(row, false)));
                j = j + 1;
            }
            dp = ((boolean[][])(appendObj(dp, row)));
            i = i + 1;
        }
        i = 0;
        while (i <= n) {
dp[i][0] = true;
            i = i + 1;
        }
        int j_1 = 1;
        while (j_1 <= s) {
dp[0][j_1] = false;
            j_1 = j_1 + 1;
        }
        i = 1;
        while (i <= n) {
            j_1 = 1;
            while (j_1 <= s) {
dp[i][j_1] = dp[i - 1][j_1];
                if (numbers[i - 1] <= j_1) {
                    if (((Boolean)(dp[i - 1][j_1 - numbers[i - 1]]))) {
dp[i][j_1] = true;
                    }
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        int diff = 0;
        j_1 = s / 2;
        while (j_1 >= 0) {
            if (((Boolean)(dp[n][j_1]))) {
                diff = s - 2 * j_1;
                break;
            }
            j_1 = j_1 - 1;
        }
        return diff;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(find_min(((int[])(new int[]{1, 2, 3, 4, 5})))));
            System.out.println(_p(find_min(((int[])(new int[]{5, 5, 5, 5, 5})))));
            System.out.println(_p(find_min(((int[])(new int[]{5, 5, 5, 5})))));
            System.out.println(_p(find_min(((int[])(new int[]{3})))));
            System.out.println(_p(find_min(((int[])(new int[]{})))));
            System.out.println(_p(find_min(((int[])(new int[]{1, 2, 3, 4})))));
            System.out.println(_p(find_min(((int[])(new int[]{0, 0, 0, 0})))));
            System.out.println(_p(find_min(((int[])(new int[]{-1, -5, 5, 1})))));
            System.out.println(_p(find_min(((int[])(new int[]{9, 9, 9, 9, 9})))));
            System.out.println(_p(find_min(((int[])(new int[]{1, 5, 10, 3})))));
            System.out.println(_p(find_min(((int[])(new int[]{-1, 0, 1})))));
            System.out.println(_p(find_min(((int[])(new int[]{10, 9, 8, 7, 6, 5, 4, 3, 2, 1})))));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
