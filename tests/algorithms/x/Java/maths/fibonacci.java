public class Main {
    static java.util.Map<Long,Long> fib_cache_global = null;
    static java.util.Map<Long,Long> fib_memo_cache = null;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0L;
        while (i_1 < 10) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double powf(double x, long n) {
        double res = 1.0;
        long i_3 = 0L;
        while (i_3 < n) {
            res = res * x;
            i_3 = i_3 + 1;
        }
        return res;
    }

    static long roundf(double x) {
        if (x >= 0.0) {
            return ((Number)((x + 0.5))).intValue();
        }
        return ((Number)((x - 0.5))).intValue();
    }

    static long[] fib_iterative(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (n == 0) {
            return new long[]{0};
        }
        long[] fib_1 = ((long[])(new long[]{0, 1}));
        long i_5 = 2L;
        while (i_5 <= n) {
            fib_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(fib_1), java.util.stream.LongStream.of(fib_1[(int)(i_5 - 1)] + fib_1[(int)(i_5 - 2)])).toArray()));
            i_5 = i_5 + 1;
        }
        return fib_1;
    }

    static long fib_recursive_term(long i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (i < 2) {
            return i;
        }
        return fib_recursive_term(i - 1) + fib_recursive_term(i - 2);
    }

    static long[] fib_recursive(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        long[] res_2 = ((long[])(new long[]{}));
        long i_7 = 0L;
        while (i_7 <= n) {
            res_2 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_2), java.util.stream.LongStream.of(fib_recursive_term(i_7))).toArray()));
            i_7 = i_7 + 1;
        }
        return res_2;
    }

    static long fib_recursive_cached_term(long i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (i < 2) {
            return i;
        }
        if (fib_cache_global.containsKey(i)) {
            return ((long)(fib_cache_global).getOrDefault(i, 0L));
        }
        long val_1 = fib_recursive_cached_term(i - 1) + fib_recursive_cached_term(i - 2);
fib_cache_global.put(i, val_1);
        return val_1;
    }

    static long[] fib_recursive_cached(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        long[] res_4 = ((long[])(new long[]{}));
        long j_1 = 0L;
        while (j_1 <= n) {
            res_4 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_4), java.util.stream.LongStream.of(fib_recursive_cached_term(j_1))).toArray()));
            j_1 = j_1 + 1;
        }
        return res_4;
    }

    static long fib_memoization_term(long num) {
        if (fib_memo_cache.containsKey(num)) {
            return ((long)(fib_memo_cache).getOrDefault(num, 0L));
        }
        long value_1 = fib_memoization_term(num - 1) + fib_memoization_term(num - 2);
fib_memo_cache.put(num, value_1);
        return value_1;
    }

    static long[] fib_memoization(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        long[] out_1 = ((long[])(new long[]{}));
        long i_9 = 0L;
        while (i_9 <= n) {
            out_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(out_1), java.util.stream.LongStream.of(fib_memoization_term(i_9))).toArray()));
            i_9 = i_9 + 1;
        }
        return out_1;
    }

    static long[] fib_binet(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (n >= 1475) {
            throw new RuntimeException(String.valueOf("n is too large"));
        }
        double sqrt5_1 = sqrt(5.0);
        double phi_1 = (1.0 + sqrt5_1) / 2.0;
        long[] res_6 = ((long[])(new long[]{}));
        long i_11 = 0L;
        while (i_11 <= n) {
            long val_3 = roundf(powf(phi_1, i_11) / sqrt5_1);
            res_6 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_6), java.util.stream.LongStream.of(val_3)).toArray()));
            i_11 = i_11 + 1;
        }
        return res_6;
    }

    static long[][] matrix_mul(long[][] a, long[][] b) {
        long a00 = a[(int)(0)][(int)(0)] * b[(int)(0)][(int)(0)] + a[(int)(0)][(int)(1)] * b[(int)(1)][(int)(0)];
        long a01_1 = a[(int)(0)][(int)(0)] * b[(int)(0)][(int)(1)] + a[(int)(0)][(int)(1)] * b[(int)(1)][(int)(1)];
        long a10_1 = a[(int)(1)][(int)(0)] * b[(int)(0)][(int)(0)] + a[(int)(1)][(int)(1)] * b[(int)(1)][(int)(0)];
        long a11_1 = a[(int)(1)][(int)(0)] * b[(int)(0)][(int)(1)] + a[(int)(1)][(int)(1)] * b[(int)(1)][(int)(1)];
        return new long[][]{new long[]{a00, a01_1}, new long[]{a10_1, a11_1}};
    }

    static long[][] matrix_pow(long[][] m, long power) {
        if (power < 0) {
            throw new RuntimeException(String.valueOf("power is negative"));
        }
        long[][] result_1 = ((long[][])(new long[][]{new long[]{1, 0}, new long[]{0, 1}}));
        long[][] base_1 = ((long[][])(m));
        long p_1 = power;
        while (p_1 > 0) {
            if (Math.floorMod(p_1, 2) == 1) {
                result_1 = ((long[][])(matrix_mul(((long[][])(result_1)), ((long[][])(base_1)))));
            }
            base_1 = ((long[][])(matrix_mul(((long[][])(base_1)), ((long[][])(base_1)))));
            p_1 = ((Number)((Math.floorDiv(p_1, 2)))).intValue();
        }
        return result_1;
    }

    static long fib_matrix(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if (n == 0) {
            return 0;
        }
        long[][] m_1 = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 0}}));
        long[][] res_8 = ((long[][])(matrix_pow(((long[][])(m_1)), n - 1)));
        return res_8[(int)(0)][(int)(0)];
    }

    static long run_tests() {
        long[] expected = ((long[])(new long[]{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55}));
        long[] it_1 = ((long[])(fib_iterative(10L)));
        long[] rec_1 = ((long[])(fib_recursive(10L)));
        long[] cache_1 = ((long[])(fib_recursive_cached(10L)));
        long[] memo_1 = ((long[])(fib_memoization(10L)));
        long[] bin_1 = ((long[])(fib_binet(10L)));
        long m_3 = fib_matrix(10L);
        if (!java.util.Arrays.equals(it_1, expected)) {
            throw new RuntimeException(String.valueOf("iterative failed"));
        }
        if (!java.util.Arrays.equals(rec_1, expected)) {
            throw new RuntimeException(String.valueOf("recursive failed"));
        }
        if (!java.util.Arrays.equals(cache_1, expected)) {
            throw new RuntimeException(String.valueOf("cached failed"));
        }
        if (!java.util.Arrays.equals(memo_1, expected)) {
            throw new RuntimeException(String.valueOf("memoization failed"));
        }
        if (!java.util.Arrays.equals(bin_1, expected)) {
            throw new RuntimeException(String.valueOf("binet failed"));
        }
        if (m_3 != 55) {
            throw new RuntimeException(String.valueOf("matrix failed"));
        }
        return m_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            fib_cache_global = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
            fib_memo_cache = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>(java.util.Map.ofEntries(java.util.Map.entry(0L, 0L), java.util.Map.entry(1L, 1L), java.util.Map.entry(2L, 1L)))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
