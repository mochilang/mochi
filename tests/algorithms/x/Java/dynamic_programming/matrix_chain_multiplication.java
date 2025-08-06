public class Main {
    static int INF;

    static int matrix_chain_multiply(int[] arr) {
        if (arr.length < 2) {
            return 0;
        }
        int n = arr.length;
        int[][] dp = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(INF)).toArray()));
                j = j + 1;
            }
            dp = ((int[][])(appendObj(dp, row)));
            i = i + 1;
        }
        i = n - 1;
        while (i > 0) {
            int j_1 = i;
            while (j_1 < n) {
                if (i == j_1) {
dp[i][j_1] = 0;
                } else {
                    int k = i;
                    while (k < j_1) {
                        int cost = dp[i][k] + dp[k + 1][j_1] + arr[i - 1] * arr[k] * arr[j_1];
                        if (cost < dp[i][j_1]) {
dp[i][j_1] = cost;
                        }
                        k = k + 1;
                    }
                }
                j_1 = j_1 + 1;
            }
            i = i - 1;
        }
        return dp[1][n - 1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000;
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
}
