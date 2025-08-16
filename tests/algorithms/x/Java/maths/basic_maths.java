public class Main {

    static long pow_int(long base, long exp) {
        long result = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result = (long)((long)(result) * (long)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static long[] prime_factors(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Only positive integers have prime factors"));
        }
        long num_1 = (long)(n);
        long[] pf_1 = ((long[])(new long[]{}));
        while (Math.floorMod(num_1, 2) == 0L) {
            pf_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(pf_1), java.util.stream.LongStream.of(2L)).toArray()));
            num_1 = Math.floorDiv(num_1, 2);
        }
        long i_3 = 3L;
        while ((long)((long)(i_3) * (long)(i_3)) <= (long)(num_1)) {
            while (Math.floorMod(num_1, i_3) == 0L) {
                pf_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(pf_1), java.util.stream.LongStream.of((long)(i_3))).toArray()));
                num_1 = Math.floorDiv(num_1, i_3);
            }
            i_3 = (long)((long)(i_3) + 2L);
        }
        if ((long)(num_1) > 2L) {
            pf_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(pf_1), java.util.stream.LongStream.of((long)(num_1))).toArray()));
        }
        return pf_1;
    }

    static long number_of_divisors(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        long num_3 = (long)(n);
        long div_1 = 1L;
        long temp_1 = 1L;
        while (Math.floorMod(num_3, 2) == 0L) {
            temp_1 = (long)((long)(temp_1) + 1L);
            num_3 = Math.floorDiv(num_3, 2);
        }
        div_1 = (long)((long)(div_1) * (long)(temp_1));
        long i_5 = 3L;
        while ((long)((long)(i_5) * (long)(i_5)) <= (long)(num_3)) {
            temp_1 = 1L;
            while (Math.floorMod(num_3, i_5) == 0L) {
                temp_1 = (long)((long)(temp_1) + 1L);
                num_3 = Math.floorDiv(num_3, i_5);
            }
            div_1 = (long)((long)(div_1) * (long)(temp_1));
            i_5 = (long)((long)(i_5) + 2L);
        }
        if ((long)(num_3) > 1L) {
            div_1 = (long)((long)(div_1) * 2L);
        }
        return div_1;
    }

    static long sum_of_divisors(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        long num_5 = (long)(n);
        long s_1 = 1L;
        long temp_3 = 1L;
        while (Math.floorMod(num_5, 2) == 0L) {
            temp_3 = (long)((long)(temp_3) + 1L);
            num_5 = Math.floorDiv(num_5, 2);
        }
        if ((long)(temp_3) > 1L) {
            s_1 = (long)((long)(s_1) * ((Number)((Math.floorDiv(((long)(((long)(pow_int(2L, (long)(temp_3))) - 1L))), ((long)((2L - 1L))))))).intValue());
        }
        long i_7 = 3L;
        while ((long)((long)(i_7) * (long)(i_7)) <= (long)(num_5)) {
            temp_3 = 1L;
            while (Math.floorMod(num_5, i_7) == 0L) {
                temp_3 = (long)((long)(temp_3) + 1L);
                num_5 = Math.floorDiv(num_5, i_7);
            }
            if ((long)(temp_3) > 1L) {
                s_1 = (long)((long)(s_1) * ((Number)((Math.floorDiv(((long)(((long)(pow_int((long)(i_7), (long)(temp_3))) - 1L))), ((long)(((long)(i_7) - 1L))))))).intValue());
            }
            i_7 = (long)((long)(i_7) + 2L);
        }
        return s_1;
    }

    static boolean contains(long[] arr, long x) {
        long idx = 0L;
        while ((long)(idx) < (long)(arr.length)) {
            if ((long)(arr[(int)((long)(idx))]) == (long)(x)) {
                return true;
            }
            idx = (long)((long)(idx) + 1L);
        }
        return false;
    }

    static long[] unique(long[] arr) {
        long[] result_1 = ((long[])(new long[]{}));
        long idx_2 = 0L;
        while ((long)(idx_2) < (long)(arr.length)) {
            long v_1 = (long)(arr[(int)((long)(idx_2))]);
            if (!(Boolean)contains(((long[])(result_1)), (long)(v_1))) {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(v_1))).toArray()));
            }
            idx_2 = (long)((long)(idx_2) + 1L);
        }
        return result_1;
    }

    static long euler_phi(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        long s_3 = (long)(n);
        long[] factors_1 = ((long[])(unique(((long[])(prime_factors((long)(n)))))));
        long idx_4 = 0L;
        while ((long)(idx_4) < (long)(factors_1.length)) {
            long x_1 = (long)(factors_1[(int)((long)(idx_4))]);
            s_3 = (long)(((Number)((Math.floorDiv(((long)(s_3)), ((long)(x_1)))))).intValue() * (long)(((long)(x_1) - 1L)));
            idx_4 = (long)((long)(idx_4) + 1L);
        }
        return s_3;
    }
    public static void main(String[] args) {
        System.out.println(_p(prime_factors(100L)));
        System.out.println(_p(number_of_divisors(100L)));
        System.out.println(_p(sum_of_divisors(100L)));
        System.out.println(_p(euler_phi(100L)));
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
