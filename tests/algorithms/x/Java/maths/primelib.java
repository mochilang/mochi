public class Main {

    static long abs_int(long x) {
        if ((long)(x) < 0L) {
            return -x;
        }
        return x;
    }

    static long gcd_iter(long a, long b) {
        long x = (long)(abs_int((long)(a)));
        long y_1 = (long)(abs_int((long)(b)));
        while ((long)(y_1) != 0L) {
            long t_1 = (long)(y_1);
            y_1 = Math.floorMod(x, y_1);
            x = (long)(t_1);
        }
        return x;
    }

    static boolean is_prime(long n) {
        if ((long)(n) <= 1L) {
            return false;
        }
        long d_1 = 2L;
        while ((long)((long)(d_1) * (long)(d_1)) <= (long)(n)) {
            if (Math.floorMod(n, d_1) == 0L) {
                return false;
            }
            d_1 = (long)((long)(d_1) + 1L);
        }
        return true;
    }

    static long[] sieve_er(long n) {
        long[] nums = ((long[])(new long[]{}));
        long i_1 = 2L;
        while ((long)(i_1) <= (long)(n)) {
            nums = ((long[])(appendLong(nums, (long)(i_1))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(nums.length)) {
            long j_1 = (long)((long)(idx_1) + 1L);
            while ((long)(j_1) < (long)(nums.length)) {
                if ((long)(nums[(int)((long)(idx_1))]) != 0L) {
                    if (Math.floorMod(nums[(int)((long)(j_1))], nums[(int)((long)(idx_1))]) == 0L) {
nums[(int)((long)(j_1))] = 0L;
                    }
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        long[] res_1 = ((long[])(new long[]{}));
        long k_1 = 0L;
        while ((long)(k_1) < (long)(nums.length)) {
            long v_1 = (long)(nums[(int)((long)(k_1))]);
            if ((long)(v_1) != 0L) {
                res_1 = ((long[])(appendLong(res_1, (long)(v_1))));
            }
            k_1 = (long)((long)(k_1) + 1L);
        }
        return res_1;
    }

    static long[] get_prime_numbers(long n) {
        long[] ans = ((long[])(new long[]{}));
        long num_1 = 2L;
        while ((long)(num_1) <= (long)(n)) {
            if (is_prime((long)(num_1))) {
                ans = ((long[])(appendLong(ans, (long)(num_1))));
            }
            num_1 = (long)((long)(num_1) + 1L);
        }
        return ans;
    }

    static long[] prime_factorization(long number) {
        if ((long)(number) == 0L) {
            return new long[]{0};
        }
        if ((long)(number) == 1L) {
            return new long[]{1};
        }
        long[] ans_2 = ((long[])(new long[]{}));
        if (is_prime((long)(number))) {
            ans_2 = ((long[])(appendLong(ans_2, (long)(number))));
            return ans_2;
        }
        long quotient_1 = (long)(number);
        long factor_1 = 2L;
        while ((long)(quotient_1) != 1L) {
            if (is_prime((long)(factor_1)) && Math.floorMod(quotient_1, factor_1) == 0L) {
                ans_2 = ((long[])(appendLong(ans_2, (long)(factor_1))));
                quotient_1 = Math.floorDiv(quotient_1, factor_1);
            } else {
                factor_1 = (long)((long)(factor_1) + 1L);
            }
        }
        return ans_2;
    }

    static long greatest_prime_factor(long number) {
        long[] factors = ((long[])(prime_factorization((long)(number))));
        long m_1 = (long)(factors[(int)(0L)]);
        long i_3 = 1L;
        while ((long)(i_3) < (long)(factors.length)) {
            if ((long)(factors[(int)((long)(i_3))]) > (long)(m_1)) {
                m_1 = (long)(factors[(int)((long)(i_3))]);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return m_1;
    }

    static long smallest_prime_factor(long number) {
        long[] factors_1 = ((long[])(prime_factorization((long)(number))));
        long m_3 = (long)(factors_1[(int)(0L)]);
        long i_5 = 1L;
        while ((long)(i_5) < (long)(factors_1.length)) {
            if ((long)(factors_1[(int)((long)(i_5))]) < (long)(m_3)) {
                m_3 = (long)(factors_1[(int)((long)(i_5))]);
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        return m_3;
    }

    static long kg_v(long number1, long number2) {
        if ((long)(number1) < 1L || (long)(number2) < 1L) {
            throw new RuntimeException(String.valueOf("numbers must be positive"));
        }
        long g_1 = (long)(gcd_iter((long)(number1), (long)(number2)));
        return ((Number)((Math.floorDiv(((long)(number1)), ((long)(g_1)))))).intValue() * (long)(number2);
    }

    static boolean is_even(long number) {
        return Math.floorMod(number, 2) == 0L;
    }

    static boolean is_odd(long number) {
        return Math.floorMod(number, 2) != 0L;
    }

    static long[] goldbach(long number) {
        if (!(Boolean)is_even((long)(number)) || (long)(number) <= 2L) {
            throw new RuntimeException(String.valueOf("number must be even and > 2"));
        }
        long[] primes_1 = ((long[])(get_prime_numbers((long)(number))));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(primes_1.length)) {
            long j_3 = (long)((long)(i_7) + 1L);
            while ((long)(j_3) < (long)(primes_1.length)) {
                if ((long)((long)(primes_1[(int)((long)(i_7))]) + (long)(primes_1[(int)((long)(j_3))])) == (long)(number)) {
                    return new long[]{primes_1[(int)((long)(i_7))], primes_1[(int)((long)(j_3))]};
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return new long[]{};
    }

    static long get_prime(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n must be non-negative"));
        }
        long index_1 = 0L;
        long ans_4 = 2L;
        while ((long)(index_1) < (long)(n)) {
            index_1 = (long)((long)(index_1) + 1L);
            ans_4 = (long)((long)(ans_4) + 1L);
            while (!(Boolean)is_prime((long)(ans_4))) {
                ans_4 = (long)((long)(ans_4) + 1L);
            }
        }
        return ans_4;
    }

    static long[] get_primes_between(long p1, long p2) {
        boolean bad1 = !(Boolean)is_prime((long)(p1));
        boolean bad2_1 = !(Boolean)is_prime((long)(p2));
        if (bad1 || bad2_1 || (long)(p1) >= (long)(p2)) {
            throw new RuntimeException(String.valueOf("arguments must be prime and p1 < p2"));
        }
        long num_3 = (long)((long)(p1) + 1L);
        while ((long)(num_3) < (long)(p2)) {
            if (is_prime((long)(num_3))) {
                break;
            }
            num_3 = (long)((long)(num_3) + 1L);
        }
        long[] ans_6 = ((long[])(new long[]{}));
        while ((long)(num_3) < (long)(p2)) {
            ans_6 = ((long[])(appendLong(ans_6, (long)(num_3))));
            num_3 = (long)((long)(num_3) + 1L);
            while ((long)(num_3) < (long)(p2)) {
                if (is_prime((long)(num_3))) {
                    break;
                }
                num_3 = (long)((long)(num_3) + 1L);
            }
        }
        return ans_6;
    }

    static long[] get_divisors(long n) {
        if ((long)(n) < 1L) {
            throw new RuntimeException(String.valueOf("n must be >= 1"));
        }
        long[] ans_8 = ((long[])(new long[]{}));
        long d_3 = 1L;
        while ((long)(d_3) <= (long)(n)) {
            if (Math.floorMod(n, d_3) == 0L) {
                ans_8 = ((long[])(appendLong(ans_8, (long)(d_3))));
            }
            d_3 = (long)((long)(d_3) + 1L);
        }
        return ans_8;
    }

    static boolean is_perfect_number(long number) {
        if ((long)(number) <= 1L) {
            throw new RuntimeException(String.valueOf("number must be > 1"));
        }
        long[] divisors_1 = ((long[])(get_divisors((long)(number))));
        long sum_1 = 0L;
        long i_9 = 0L;
        while ((long)(i_9) < (long)((long)(divisors_1.length) - 1L)) {
            sum_1 = (long)((long)(sum_1) + (long)(divisors_1[(int)((long)(i_9))]));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return (long)(sum_1) == (long)(number);
    }

    static long[] simplify_fraction(long numerator, long denominator) {
        if ((long)(denominator) == 0L) {
            throw new RuntimeException(String.valueOf("denominator cannot be zero"));
        }
        long g_3 = (long)(gcd_iter((long)(abs_int((long)(numerator))), (long)(abs_int((long)(denominator)))));
        return new long[]{Math.floorDiv(((long)(numerator)), ((long)(g_3))), Math.floorDiv(((long)(denominator)), ((long)(g_3)))};
    }

    static long factorial(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n must be >= 0"));
        }
        long ans_10 = 1L;
        long i_11 = 1L;
        while ((long)(i_11) <= (long)(n)) {
            ans_10 = (long)((long)(ans_10) * (long)(i_11));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return ans_10;
    }

    static long fib(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n must be >= 0"));
        }
        if ((long)(n) <= 1L) {
            return 1;
        }
        long tmp_1 = 0L;
        long fib1_1 = 1L;
        long ans_12 = 1L;
        long i_13 = 0L;
        while ((long)(i_13) < (long)((long)(n) - 1L)) {
            tmp_1 = (long)(ans_12);
            ans_12 = (long)((long)(ans_12) + (long)(fib1_1));
            fib1_1 = (long)(tmp_1);
            i_13 = (long)((long)(i_13) + 1L);
        }
        return ans_12;
    }
    public static void main(String[] args) {
        System.out.println(_p(is_prime(97L)));
        System.out.println(_p(sieve_er(20L)));
        System.out.println(_p(get_prime_numbers(20L)));
        System.out.println(_p(prime_factorization(287L)));
        System.out.println(_p(greatest_prime_factor(287L)));
        System.out.println(_p(smallest_prime_factor(287L)));
        System.out.println(_p(kg_v(8L, 10L)));
        System.out.println(_p(goldbach(28L)));
        System.out.println(_p(get_prime(8L)));
        System.out.println(_p(get_primes_between(3L, 23L)));
        System.out.println(_p(get_divisors(28L)));
        System.out.println(_p(is_perfect_number(28L)));
        System.out.println(_p(simplify_fraction(10L, 20L)));
        System.out.println(_p(factorial(5L)));
        System.out.println(_p(fib(10L)));
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
