public class Main {

    static int partition(int m) {
        int[][] memo = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < m + 1) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < m) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            memo = ((int[][])(appendObj(memo, row)));
            i = i + 1;
        }
        i = 0;
        while (i < m + 1) {
memo[i][0] = 1;
            i = i + 1;
        }
        int n = 0;
        while (n < m + 1) {
            int k = 1;
            while (k < m) {
memo[n][k] = memo[n][k] + memo[n][k - 1];
                if (n - k > 0) {
memo[n][k] = memo[n][k] + memo[n - k - 1][k];
                }
                k = k + 1;
            }
            n = n + 1;
        }
        return memo[m][m - 1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(partition(5));
            System.out.println(partition(7));
            System.out.println(partition(100));
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
