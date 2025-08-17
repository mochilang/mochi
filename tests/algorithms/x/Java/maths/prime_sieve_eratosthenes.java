public class Main {

    static long[] prime_sieve_eratosthenes(long num) {
        if ((long)(num) <= 0L) {
            throw new RuntimeException(String.valueOf("Input must be a positive integer"));
        }
        boolean[] primes_1 = ((boolean[])(new boolean[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(num)) {
            primes_1 = ((boolean[])(appendBool(primes_1, true)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long p_1 = 2L;
        while ((long)((long)(p_1) * (long)(p_1)) <= (long)(num)) {
            if (primes_1[(int)((long)(p_1))]) {
                long j_1 = (long)((long)(p_1) * (long)(p_1));
                while ((long)(j_1) <= (long)(num)) {
primes_1[(int)((long)(j_1))] = false;
                    j_1 = (long)((long)(j_1) + (long)(p_1));
                }
            }
            p_1 = (long)((long)(p_1) + 1L);
        }
        long[] result_1 = ((long[])(new long[]{}));
        long k_1 = 2L;
        while ((long)(k_1) <= (long)(num)) {
            if (primes_1[(int)((long)(k_1))]) {
                result_1 = ((long[])(appendLong(result_1, (long)(k_1))));
            }
            k_1 = (long)((long)(k_1) + 1L);
        }
        return result_1;
    }

    static boolean list_eq(long[] a, long[] b) {
        if ((long)(a.length) != (long)(b.length)) {
            return false;
        }
        long i_3 = 0L;
        while ((long)(i_3) < (long)(a.length)) {
            if ((long)(a[(int)((long)(i_3))]) != (long)(b[(int)((long)(i_3))])) {
                return false;
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return true;
    }

    static void test_prime_sieve_eratosthenes() {
        if (!(Boolean)list_eq(((long[])(prime_sieve_eratosthenes(10L))), ((long[])(new long[]{2, 3, 5, 7})))) {
            throw new RuntimeException(String.valueOf("test 10 failed"));
        }
        if (!(Boolean)list_eq(((long[])(prime_sieve_eratosthenes(20L))), ((long[])(new long[]{2, 3, 5, 7, 11, 13, 17, 19})))) {
            throw new RuntimeException(String.valueOf("test 20 failed"));
        }
        if (!(Boolean)list_eq(((long[])(prime_sieve_eratosthenes(2L))), ((long[])(new long[]{2})))) {
            throw new RuntimeException(String.valueOf("test 2 failed"));
        }
        if ((long)(prime_sieve_eratosthenes(1L).length) != 0L) {
            throw new RuntimeException(String.valueOf("test 1 failed"));
        }
    }

    static void main() {
        test_prime_sieve_eratosthenes();
        System.out.println(_p(prime_sieve_eratosthenes(20L)));
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
