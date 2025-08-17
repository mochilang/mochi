public class Main {

    static long[] prime_factors(long n) {
        if ((long)(n) < 2L) {
            return new long[]{};
        }
        long num_1 = (long)(n);
        long i_1 = 2L;
        long[] factors_1 = ((long[])(new long[]{}));
        while ((long)((long)(i_1) * (long)(i_1)) <= (long)(num_1)) {
            if (Math.floorMod(num_1, i_1) == 0L) {
                factors_1 = ((long[])(appendLong(factors_1, (long)(i_1))));
                num_1 = Math.floorDiv(num_1, i_1);
            } else {
                i_1 = (long)((long)(i_1) + 1L);
            }
        }
        if ((long)(num_1) > 1L) {
            factors_1 = ((long[])(appendLong(factors_1, (long)(num_1))));
        }
        return factors_1;
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

    static void test_prime_factors() {
        if (!(Boolean)list_eq(((long[])(prime_factors(0L))), ((long[])(new long[]{})))) {
            throw new RuntimeException(String.valueOf("prime_factors(0) failed"));
        }
        if (!(Boolean)list_eq(((long[])(prime_factors(100L))), ((long[])(new long[]{2, 2, 5, 5})))) {
            throw new RuntimeException(String.valueOf("prime_factors(100) failed"));
        }
        if (!(Boolean)list_eq(((long[])(prime_factors(2560L))), ((long[])(new long[]{2, 2, 2, 2, 2, 2, 2, 2, 2, 5})))) {
            throw new RuntimeException(String.valueOf("prime_factors(2560) failed"));
        }
        if (!(Boolean)list_eq(((long[])(prime_factors(97L))), ((long[])(new long[]{97})))) {
            throw new RuntimeException(String.valueOf("prime_factors(97) failed"));
        }
    }

    static void main() {
        test_prime_factors();
        System.out.println(_p(prime_factors(100L)));
        System.out.println(_p(prime_factors(2560L)));
        System.out.println(_p(prime_factors(97L)));
    }
    public static void main(String[] args) {
        main();
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
