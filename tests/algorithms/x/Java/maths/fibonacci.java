public class Main {
    static java.util.Map<Integer,Integer> fib_cache_global = null;
    static java.util.Map<Integer,Integer> fib_memo_cache = null;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double powf(double x, int n) {
        double res = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            res = res * x;
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int roundf(double x) {
        if (x >= 0.0) {
            return ((Number)((x + 0.5))).intValue();
        }
        return ((Number)((x - 0.5))).intValue();
    }

    static int[] fib_iterative(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (n == 0) {
            return new int[]{0};
        }
        int[] fib = ((int[])(new int[]{0, 1}));
        int i_2 = 2;
        while (i_2 <= n) {
            fib = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(fib), java.util.stream.IntStream.of(fib[i_2 - 1] + fib[i_2 - 2])).toArray()));
            i_2 = i_2 + 1;
        }
        return fib;
    }

    static int fib_recursive_term(int i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (i < 2) {
            return i;
        }
        return fib_recursive_term(i - 1) + fib_recursive_term(i - 2);
    }

    static int[] fib_recursive(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        int[] res_1 = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 <= n) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(fib_recursive_term(i_3))).toArray()));
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static int fib_recursive_cached_term(int i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (i < 2) {
            return i;
        }
        if (((Boolean)(fib_cache_global.containsKey(i)))) {
            return ((int)(fib_cache_global).getOrDefault(i, 0));
        }
        int val = fib_recursive_cached_term(i - 1) + fib_recursive_cached_term(i - 2);
fib_cache_global.put(i, val);
        return val;
    }

    static int[] fib_recursive_cached(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        int[] res_2 = ((int[])(new int[]{}));
        int j = 0;
        while (j <= n) {
            res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(fib_recursive_cached_term(j))).toArray()));
            j = j + 1;
        }
        return res_2;
    }

    static int fib_memoization_term(int num) {
        if (((Boolean)(fib_memo_cache.containsKey(num)))) {
            return ((int)(fib_memo_cache).getOrDefault(num, 0));
        }
        int value = fib_memoization_term(num - 1) + fib_memoization_term(num - 2);
fib_memo_cache.put(num, value);
        return value;
    }

    static int[] fib_memoization(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        int[] out = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 <= n) {
            out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(fib_memoization_term(i_4))).toArray()));
            i_4 = i_4 + 1;
        }
        return out;
    }

    static int[] fib_binet(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (n >= 1475) {
            throw new RuntimeException(String.valueOf("n is too large"));
        }
        double sqrt5 = sqrt(5.0);
        double phi = (1.0 + sqrt5) / 2.0;
        int[] res_3 = ((int[])(new int[]{}));
        int i_5 = 0;
        while (i_5 <= n) {
            int val_1 = roundf(powf(phi, i_5) / sqrt5);
            res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(val_1)).toArray()));
            i_5 = i_5 + 1;
        }
        return res_3;
    }

    static int[][] matrix_mul(int[][] a, int[][] b) {
        int a00 = a[0][0] * b[0][0] + a[0][1] * b[1][0];
        int a01 = a[0][0] * b[0][1] + a[0][1] * b[1][1];
        int a10 = a[1][0] * b[0][0] + a[1][1] * b[1][0];
        int a11 = a[1][0] * b[0][1] + a[1][1] * b[1][1];
        return new int[][]{new int[]{a00, a01}, new int[]{a10, a11}};
    }

    static int[][] matrix_pow(int[][] m, int power) {
        if (power < 0) {
            throw new RuntimeException(String.valueOf("power is negative"));
        }
        int[][] result = ((int[][])(new int[][]{new int[]{1, 0}, new int[]{0, 1}}));
        int[][] base = ((int[][])(m));
        int p = power;
        while (p > 0) {
            if (Math.floorMod(p, 2) == 1) {
                result = ((int[][])(matrix_mul(((int[][])(result)), ((int[][])(base)))));
            }
            base = ((int[][])(matrix_mul(((int[][])(base)), ((int[][])(base)))));
            p = ((Number)((p / 2))).intValue();
        }
        return result;
    }

    static int fib_matrix(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (n == 0) {
            return 0;
        }
        int[][] m = ((int[][])(new int[][]{new int[]{1, 1}, new int[]{1, 0}}));
        int[][] res_4 = ((int[][])(matrix_pow(((int[][])(m)), n - 1)));
        return res_4[0][0];
    }

    static int run_tests() {
        int[] expected = ((int[])(new int[]{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55}));
        int[] it = ((int[])(fib_iterative(10)));
        int[] rec = ((int[])(fib_recursive(10)));
        int[] cache = ((int[])(fib_recursive_cached(10)));
        int[] memo = ((int[])(fib_memoization(10)));
        int[] bin = ((int[])(fib_binet(10)));
        int m_1 = fib_matrix(10);
        if (it != expected) {
            throw new RuntimeException(String.valueOf("iterative failed"));
        }
        if (rec != expected) {
            throw new RuntimeException(String.valueOf("recursive failed"));
        }
        if (cache != expected) {
            throw new RuntimeException(String.valueOf("cached failed"));
        }
        if (memo != expected) {
            throw new RuntimeException(String.valueOf("memoization failed"));
        }
        if (bin != expected) {
            throw new RuntimeException(String.valueOf("binet failed"));
        }
        if (m_1 != 55) {
            throw new RuntimeException(String.valueOf("matrix failed"));
        }
        return m_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            fib_cache_global = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
            fib_memo_cache = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>(java.util.Map.ofEntries(java.util.Map.entry(0, 0), java.util.Map.entry(1, 1), java.util.Map.entry(2, 1)))));
            System.out.println(_p(run_tests()));
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
