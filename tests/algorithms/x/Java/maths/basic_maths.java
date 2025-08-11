public class Main {

    static long pow_int(long base, long exp) {
        long result = 1;
        long i_1 = 0;
        while (i_1 < exp) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static long[] prime_factors(long n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive integers have prime factors"));
        }
        long num_1 = n;
        long[] pf_1 = ((long[])(new long[]{}));
        while (Math.floorMod(num_1, 2) == 0) {
            pf_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(pf_1), java.util.stream.LongStream.of(2)).toArray()));
            num_1 = Math.floorDiv(num_1, 2);
        }
        long i_3 = 3;
        while (i_3 * i_3 <= num_1) {
            while (Math.floorMod(num_1, i_3) == 0) {
                pf_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(pf_1), java.util.stream.LongStream.of(i_3)).toArray()));
                num_1 = Math.floorDiv(num_1, i_3);
            }
            i_3 = i_3 + 2;
        }
        if (num_1 > 2) {
            pf_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(pf_1), java.util.stream.LongStream.of(num_1)).toArray()));
        }
        return pf_1;
    }

    static long number_of_divisors(long n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        long num_3 = n;
        long div_1 = 1;
        long temp_1 = 1;
        while (Math.floorMod(num_3, 2) == 0) {
            temp_1 = temp_1 + 1;
            num_3 = Math.floorDiv(num_3, 2);
        }
        div_1 = div_1 * temp_1;
        long i_5 = 3;
        while (i_5 * i_5 <= num_3) {
            temp_1 = 1;
            while (Math.floorMod(num_3, i_5) == 0) {
                temp_1 = temp_1 + 1;
                num_3 = Math.floorDiv(num_3, i_5);
            }
            div_1 = div_1 * temp_1;
            i_5 = i_5 + 2;
        }
        if (num_3 > 1) {
            div_1 = div_1 * 2;
        }
        return div_1;
    }

    static long sum_of_divisors(long n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        long num_5 = n;
        long s_1 = 1;
        long temp_3 = 1;
        while (Math.floorMod(num_5, 2) == 0) {
            temp_3 = temp_3 + 1;
            num_5 = Math.floorDiv(num_5, 2);
        }
        if (temp_3 > 1) {
            s_1 = s_1 * ((Number)((Math.floorDiv((pow_int(2, temp_3) - 1), (2 - 1))))).intValue();
        }
        long i_7 = 3;
        while (i_7 * i_7 <= num_5) {
            temp_3 = 1;
            while (Math.floorMod(num_5, i_7) == 0) {
                temp_3 = temp_3 + 1;
                num_5 = Math.floorDiv(num_5, i_7);
            }
            if (temp_3 > 1) {
                s_1 = s_1 * ((Number)((Math.floorDiv((pow_int(i_7, temp_3) - 1), (i_7 - 1))))).intValue();
            }
            i_7 = i_7 + 2;
        }
        return s_1;
    }

    static boolean contains(long[] arr, long x) {
        long idx = 0;
        while (idx < arr.length) {
            if (arr[(int)(idx)] == x) {
                return true;
            }
            idx = idx + 1;
        }
        return false;
    }

    static long[] unique(long[] arr) {
        long[] result_1 = ((long[])(new long[]{}));
        long idx_2 = 0;
        while (idx_2 < arr.length) {
            long v_1 = arr[(int)(idx_2)];
            if (!(Boolean)contains(((long[])(result_1)), v_1)) {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of(v_1)).toArray()));
            }
            idx_2 = idx_2 + 1;
        }
        return result_1;
    }

    static long euler_phi(long n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        long s_3 = n;
        long[] factors_1 = ((long[])(unique(((long[])(prime_factors(n))))));
        long idx_4 = 0;
        while (idx_4 < factors_1.length) {
            long x_1 = factors_1[(int)(idx_4)];
            s_3 = ((Number)((Math.floorDiv(s_3, x_1)))).intValue() * (x_1 - 1);
            idx_4 = idx_4 + 1;
        }
        return s_3;
    }
    public static void main(String[] args) {
        System.out.println(_p(prime_factors(100)));
        System.out.println(_p(number_of_divisors(100)));
        System.out.println(_p(sum_of_divisors(100)));
        System.out.println(_p(euler_phi(100)));
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
