public class Main {

    static long[] slow_primes(long max_n) {
        long[] result = ((long[])(new long[]{}));
        long i_1 = 2L;
        while ((long)(i_1) <= (long)(max_n)) {
            long j_1 = 2L;
            boolean is_prime_1 = true;
            while ((long)(j_1) < (long)(i_1)) {
                if (Math.floorMod(i_1, j_1) == 0L) {
                    is_prime_1 = false;
                    break;
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            if (is_prime_1) {
                result = ((long[])(appendLong(result, (long)(i_1))));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static long[] primes(long max_n) {
        long[] result_1 = ((long[])(new long[]{}));
        long i_3 = 2L;
        while ((long)(i_3) <= (long)(max_n)) {
            long j_3 = 2L;
            boolean is_prime_3 = true;
            while ((long)((long)(j_3) * (long)(j_3)) <= (long)(i_3)) {
                if (Math.floorMod(i_3, j_3) == 0L) {
                    is_prime_3 = false;
                    break;
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            if (is_prime_3) {
                result_1 = ((long[])(appendLong(result_1, (long)(i_3))));
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result_1;
    }

    static long[] fast_primes(long max_n) {
        long[] result_2 = ((long[])(new long[]{}));
        if ((long)(max_n) >= 2L) {
            result_2 = ((long[])(appendLong(result_2, 2L)));
        }
        long i_5 = 3L;
        while ((long)(i_5) <= (long)(max_n)) {
            long j_5 = 3L;
            boolean is_prime_5 = true;
            while ((long)((long)(j_5) * (long)(j_5)) <= (long)(i_5)) {
                if (Math.floorMod(i_5, j_5) == 0L) {
                    is_prime_5 = false;
                    break;
                }
                j_5 = (long)((long)(j_5) + 2L);
            }
            if (is_prime_5) {
                result_2 = ((long[])(appendLong(result_2, (long)(i_5))));
            }
            i_5 = (long)((long)(i_5) + 2L);
        }
        return result_2;
    }
    public static void main(String[] args) {
        System.out.println(_p(slow_primes(25L)));
        System.out.println(_p(primes(25L)));
        System.out.println(_p(fast_primes(25L)));
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
}
