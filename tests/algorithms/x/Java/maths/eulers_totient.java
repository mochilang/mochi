public class Main {

    static long[] totient(long n) {
        boolean[] is_prime = ((boolean[])(new boolean[]{}));
        long[] totients_1 = ((long[])(new long[]{}));
        long[] primes_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(n)) {
            is_prime = ((boolean[])(appendBool(is_prime, true)));
            totients_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(totients_1), java.util.stream.LongStream.of((long)((long)(i_1) - 1L))).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        i_1 = 2L;
        while ((long)(i_1) <= (long)(n)) {
            if (is_prime[(int)((long)(i_1))]) {
                primes_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(primes_1), java.util.stream.LongStream.of((long)(i_1))).toArray()));
            }
            long j_1 = 0L;
            while ((long)(j_1) < (long)(primes_1.length)) {
                long p_1 = (long)(primes_1[(int)((long)(j_1))]);
                if ((long)((long)(i_1) * (long)(p_1)) >= (long)(n)) {
                    break;
                }
is_prime[(int)((long)((long)(i_1) * (long)(p_1)))] = false;
                if (Math.floorMod(i_1, p_1) == 0L) {
totients_1[(int)((long)((long)(i_1) * (long)(p_1)))] = (long)((long)(totients_1[(int)((long)(i_1))]) * (long)(p_1));
                    break;
                }
totients_1[(int)((long)((long)(i_1) * (long)(p_1)))] = (long)((long)(totients_1[(int)((long)(i_1))]) * (long)(((long)(p_1) - 1L)));
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return totients_1;
    }

    static void test_totient() {
        long[] expected = ((long[])(new long[]{-1, 0, 1, 2, 2, 4, 2, 6, 4, 6, 9}));
        long[] res_1 = ((long[])(totient(10L)));
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(expected.length)) {
            if ((long)(res_1[(int)((long)(idx_1))]) != (long)(expected[(int)((long)(idx_1))])) {
                throw new RuntimeException(String.valueOf("totient mismatch at " + _p(idx_1)));
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
    }

    static void main() {
        test_totient();
        long n_1 = 10L;
        long[] res_3 = ((long[])(totient((long)(n_1))));
        long i_3 = 1L;
        while ((long)(i_3) < (long)(n_1)) {
            System.out.println(_p(i_3) + " has " + _p(_geti(res_3, ((Number)(i_3)).intValue())) + " relative primes.");
            i_3 = (long)((long)(i_3) + 1L);
        }
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
