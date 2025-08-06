public class Main {

    static int[] copy_list(int[] src) {
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        while (i < src.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(src[i])).toArray()));
            i = i + 1;
        }
        return result;
    }

    static int[][] subset_combinations(int[] elements, int n) {
        int r = elements.length;
        if (n > r) {
            return new int[][]{};
        }
        int[][][] dp = ((int[][][])(new int[][][]{}));
        int i_1 = 0;
        while (i_1 <= r) {
            dp = ((int[][][])(appendObj(dp, new int[][]{})));
            i_1 = i_1 + 1;
        }
dp[0] = ((int[][])(appendObj(dp[0], new int[]{})));
        i_1 = 1;
        while (i_1 <= r) {
            int j = i_1;
            while (j > 0) {
                int[][] prevs = ((int[][])(dp[j - 1]));
                int k = 0;
                while (k < prevs.length) {
                    int[] prev = ((int[])(prevs[k]));
                    int[] comb = ((int[])(copy_list(((int[])(prev)))));
                    comb = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(comb), java.util.stream.IntStream.of(elements[i_1 - 1])).toArray()));
dp[j] = ((int[][])(appendObj(dp[j], comb)));
                    k = k + 1;
                }
                j = j - 1;
            }
            i_1 = i_1 + 1;
        }
        return dp[n];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(subset_combinations(((int[])(new int[]{10, 20, 30, 40})), 2)));
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
