public class Main {

    static long isqrt(long n) {
        long r = 0L;
        while ((long)((long)(((long)(r) + 1L)) * (long)(((long)(r) + 1L))) <= (long)(n)) {
            r = (long)((long)(r) + 1L);
        }
        return r;
    }

    static long[] prime_sieve(long num) {
        if ((long)(num) <= 0L) {
            throw new RuntimeException(String.valueOf("Invalid input, please enter a positive integer."));
        }
        boolean[] sieve_1 = ((boolean[])(new boolean[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(num)) {
            sieve_1 = ((boolean[])(appendBool(sieve_1, true)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] prime_1 = ((long[])(new long[]{}));
        long start_1 = 2L;
        long end_1 = (long)(isqrt((long)(num)));
        while ((long)(start_1) <= (long)(end_1)) {
            if (sieve_1[(int)((long)(start_1))]) {
                prime_1 = ((long[])(appendLong(prime_1, (long)(start_1))));
                long j_1 = (long)((long)(start_1) * (long)(start_1));
                while ((long)(j_1) <= (long)(num)) {
                    if (sieve_1[(int)((long)(j_1))]) {
sieve_1[(int)((long)(j_1))] = false;
                    }
                    j_1 = (long)((long)(j_1) + (long)(start_1));
                }
            }
            start_1 = (long)((long)(start_1) + 1L);
        }
        long k_1 = (long)((long)(end_1) + 1L);
        while ((long)(k_1) <= (long)(num)) {
            if (sieve_1[(int)((long)(k_1))]) {
                prime_1 = ((long[])(appendLong(prime_1, (long)(k_1))));
            }
            k_1 = (long)((long)(k_1) + 1L);
        }
        return prime_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(prime_sieve(50L)));
        System.out.println(_p(prime_sieve(25L)));
        System.out.println(_p(prime_sieve(10L)));
        System.out.println(_p(prime_sieve(9L)));
        System.out.println(_p(prime_sieve(2L)));
        System.out.println(_p(prime_sieve(1L)));
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
