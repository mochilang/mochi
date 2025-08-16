public class Main {
    static java.util.Map<Long,Long> fib_cache_global = null;
    static java.util.Map<Long,Long> fib_memo_cache = null;

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 10L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double powf(double x, long n) {
        double res = (double)(1.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            res = (double)((double)(res) * (double)(x));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res;
    }

    static long roundf(double x) {
        if ((double)(x) >= (double)(0.0)) {
            return ((Number)(((double)(x) + (double)(0.5)))).intValue();
        }
        return ((Number)(((double)(x) - (double)(0.5)))).intValue();
    }

    static long[] fib_iterative(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if ((long)(n) == 0L) {
            return new long[]{0};
        }
        long[] fib_1 = ((long[])(new long[]{0, 1}));
        long i_5 = 2L;
        while ((long)(i_5) <= (long)(n)) {
            fib_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(fib_1), java.util.stream.LongStream.of((long)((long)(fib_1[(int)((long)((long)(i_5) - 1L))]) + (long)(fib_1[(int)((long)((long)(i_5) - 2L))])))).toArray()));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return fib_1;
    }

    static long fib_recursive_term(long i) {
        if ((long)(i) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if ((long)(i) < 2L) {
            return i;
        }
        return (long)(fib_recursive_term((long)((long)(i) - 1L))) + (long)(fib_recursive_term((long)((long)(i) - 2L)));
    }

    static long[] fib_recursive(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        long[] res_2 = ((long[])(new long[]{}));
        long i_7 = 0L;
        while ((long)(i_7) <= (long)(n)) {
            res_2 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_2), java.util.stream.LongStream.of((long)(fib_recursive_term((long)(i_7))))).toArray()));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return res_2;
    }

    static long fib_recursive_cached_term(long i) {
        if ((long)(i) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if ((long)(i) < 2L) {
            return i;
        }
        if (fib_cache_global.containsKey(i)) {
            return ((long)(fib_cache_global).getOrDefault(i, 0L));
        }
        long val_1 = (long)((long)(fib_recursive_cached_term((long)((long)(i) - 1L))) + (long)(fib_recursive_cached_term((long)((long)(i) - 2L))));
fib_cache_global.put(i, (long)(val_1));
        return val_1;
    }

    static long[] fib_recursive_cached(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        long[] res_4 = ((long[])(new long[]{}));
        long j_1 = 0L;
        while ((long)(j_1) <= (long)(n)) {
            res_4 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_4), java.util.stream.LongStream.of((long)(fib_recursive_cached_term((long)(j_1))))).toArray()));
            j_1 = (long)((long)(j_1) + 1L);
        }
        return res_4;
    }

    static long fib_memoization_term(long num) {
        if (fib_memo_cache.containsKey(num)) {
            return ((long)(fib_memo_cache).getOrDefault(num, 0L));
        }
        long value_1 = (long)((long)(fib_memoization_term((long)((long)(num) - 1L))) + (long)(fib_memoization_term((long)((long)(num) - 2L))));
fib_memo_cache.put(num, (long)(value_1));
        return value_1;
    }

    static long[] fib_memoization(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        long[] out_1 = ((long[])(new long[]{}));
        long i_9 = 0L;
        while ((long)(i_9) <= (long)(n)) {
            out_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(out_1), java.util.stream.LongStream.of((long)(fib_memoization_term((long)(i_9))))).toArray()));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return out_1;
    }

    static long[] fib_binet(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if ((long)(n) >= 1475L) {
            throw new RuntimeException(String.valueOf("n is too large"));
        }
        double sqrt5_1 = (double)(sqrt((double)(5.0)));
        double phi_1 = (double)((double)(((double)(1.0) + (double)(sqrt5_1))) / (double)(2.0));
        long[] res_6 = ((long[])(new long[]{}));
        long i_11 = 0L;
        while ((long)(i_11) <= (long)(n)) {
            long val_3 = (long)(roundf((double)((double)(powf((double)(phi_1), (long)(i_11))) / (double)(sqrt5_1))));
            res_6 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_6), java.util.stream.LongStream.of((long)(val_3))).toArray()));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return res_6;
    }

    static long[][] matrix_mul(long[][] a, long[][] b) {
        long a00 = (long)((long)((long)(a[(int)(0L)][(int)(0L)]) * (long)(b[(int)(0L)][(int)(0L)])) + (long)((long)(a[(int)(0L)][(int)(1L)]) * (long)(b[(int)(1L)][(int)(0L)])));
        long a01_1 = (long)((long)((long)(a[(int)(0L)][(int)(0L)]) * (long)(b[(int)(0L)][(int)(1L)])) + (long)((long)(a[(int)(0L)][(int)(1L)]) * (long)(b[(int)(1L)][(int)(1L)])));
        long a10_1 = (long)((long)((long)(a[(int)(1L)][(int)(0L)]) * (long)(b[(int)(0L)][(int)(0L)])) + (long)((long)(a[(int)(1L)][(int)(1L)]) * (long)(b[(int)(1L)][(int)(0L)])));
        long a11_1 = (long)((long)((long)(a[(int)(1L)][(int)(0L)]) * (long)(b[(int)(0L)][(int)(1L)])) + (long)((long)(a[(int)(1L)][(int)(1L)]) * (long)(b[(int)(1L)][(int)(1L)])));
        return new long[][]{new long[]{a00, a01_1}, new long[]{a10_1, a11_1}};
    }

    static long[][] matrix_pow(long[][] m, long power) {
        if ((long)(power) < 0L) {
            throw new RuntimeException(String.valueOf("power is negative"));
        }
        long[][] result_1 = ((long[][])(new long[][]{new long[]{1, 0}, new long[]{0, 1}}));
        long[][] base_1 = ((long[][])(m));
        long p_1 = (long)(power);
        while ((long)(p_1) > 0L) {
            if (Math.floorMod(p_1, 2) == 1L) {
                result_1 = ((long[][])(matrix_mul(((long[][])(result_1)), ((long[][])(base_1)))));
            }
            base_1 = ((long[][])(matrix_mul(((long[][])(base_1)), ((long[][])(base_1)))));
            p_1 = (long)(((Number)((Math.floorDiv(p_1, 2)))).intValue());
        }
        return result_1;
    }

    static long fib_matrix(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n is negative"));
        }
        if ((long)(n) == 0L) {
            return 0;
        }
        long[][] m_1 = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 0}}));
        long[][] res_8 = ((long[][])(matrix_pow(((long[][])(m_1)), (long)((long)(n) - 1L))));
        return res_8[(int)(0L)][(int)(0L)];
    }

    static long run_tests() {
        long[] expected = ((long[])(new long[]{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55}));
        long[] it_1 = ((long[])(fib_iterative(10L)));
        long[] rec_1 = ((long[])(fib_recursive(10L)));
        long[] cache_1 = ((long[])(fib_recursive_cached(10L)));
        long[] memo_1 = ((long[])(fib_memoization(10L)));
        long[] bin_1 = ((long[])(fib_binet(10L)));
        long m_3 = (long)(fib_matrix(10L));
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
        if ((long)(m_3) != 55L) {
            throw new RuntimeException(String.valueOf("matrix failed"));
        }
        return m_3;
    }
    public static void main(String[] args) {
        fib_cache_global = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        fib_memo_cache = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>(java.util.Map.ofEntries(java.util.Map.entry(0L, 0L), java.util.Map.entry(1L, 1L), java.util.Map.entry(2L, 1L)))));
        System.out.println(_p(run_tests()));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
