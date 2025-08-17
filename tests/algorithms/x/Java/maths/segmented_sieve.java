public class Main {

    static long min_int(long a, long b) {
        if ((long)(a) < (long)(b)) {
            return a;
        }
        return b;
    }

    static long int_sqrt(long n) {
        long r = 0L;
        while ((long)((long)(((long)(r) + 1L)) * (long)(((long)(r) + 1L))) <= (long)(n)) {
            r = (long)((long)(r) + 1L);
        }
        return r;
    }

    static long[] sieve(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Number must instead be a positive integer"));
        }
        long[] in_prime_1 = ((long[])(new long[]{}));
        long start_1 = 2L;
        long end_1 = (long)(int_sqrt((long)(n)));
        long[] temp_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)((long)(end_1) + 1L)) {
            temp_1 = ((long[])(appendLong(temp_1, 1L)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] prime_1 = ((long[])(new long[]{}));
        while ((long)(start_1) <= (long)(end_1)) {
            if ((long)(temp_1[(int)((long)(start_1))]) == 1L) {
                in_prime_1 = ((long[])(appendLong(in_prime_1, (long)(start_1))));
                long j_1 = (long)((long)(start_1) * (long)(start_1));
                while ((long)(j_1) <= (long)(end_1)) {
temp_1[(int)((long)(j_1))] = 0L;
                    j_1 = (long)((long)(j_1) + (long)(start_1));
                }
            }
            start_1 = (long)((long)(start_1) + 1L);
        }
        i_1 = 0L;
        while ((long)(i_1) < (long)(in_prime_1.length)) {
            prime_1 = ((long[])(appendLong(prime_1, (long)(in_prime_1[(int)((long)(i_1))]))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long low_1 = (long)((long)(end_1) + 1L);
        long high_1 = (long)(min_int((long)(2L * (long)(end_1)), (long)(n)));
        while ((long)(low_1) <= (long)(n)) {
            long[] tempSeg_1 = ((long[])(new long[]{}));
            long size_1 = (long)((long)((long)(high_1) - (long)(low_1)) + 1L);
            long k_1 = 0L;
            while ((long)(k_1) < (long)(size_1)) {
                tempSeg_1 = ((long[])(appendLong(tempSeg_1, 1L)));
                k_1 = (long)((long)(k_1) + 1L);
            }
            long idx_1 = 0L;
            while ((long)(idx_1) < (long)(in_prime_1.length)) {
                long each_1 = (long)(in_prime_1[(int)((long)(idx_1))]);
                long t_1 = (long)(((Number)((Math.floorDiv(((long)(low_1)), ((long)(each_1)))))).intValue() * (long)(each_1));
                if ((long)(t_1) < (long)(low_1)) {
                    t_1 = (long)((long)(t_1) + (long)(each_1));
                }
                long j2_1 = (long)(t_1);
                while ((long)(j2_1) <= (long)(high_1)) {
tempSeg_1[(int)((long)((long)(j2_1) - (long)(low_1)))] = 0L;
                    j2_1 = (long)((long)(j2_1) + (long)(each_1));
                }
                idx_1 = (long)((long)(idx_1) + 1L);
            }
            long j3_1 = 0L;
            while ((long)(j3_1) < (long)(tempSeg_1.length)) {
                if ((long)(tempSeg_1[(int)((long)(j3_1))]) == 1L) {
                    prime_1 = ((long[])(appendLong(prime_1, (long)((long)(j3_1) + (long)(low_1)))));
                }
                j3_1 = (long)((long)(j3_1) + 1L);
            }
            low_1 = (long)((long)(high_1) + 1L);
            high_1 = (long)(min_int((long)((long)(high_1) + (long)(end_1)), (long)(n)));
        }
        return prime_1;
    }

    static boolean lists_equal(long[] a, long[] b) {
        if ((long)(a.length) != (long)(b.length)) {
            return false;
        }
        long m_1 = 0L;
        while ((long)(m_1) < (long)(a.length)) {
            if ((long)(a[(int)((long)(m_1))]) != (long)(b[(int)((long)(m_1))])) {
                return false;
            }
            m_1 = (long)((long)(m_1) + 1L);
        }
        return true;
    }

    static void test_sieve() {
        long[] e1 = ((long[])(sieve(8L)));
        if (!(Boolean)lists_equal(((long[])(e1)), ((long[])(new long[]{2, 3, 5, 7})))) {
            throw new RuntimeException(String.valueOf("sieve(8) failed"));
        }
        long[] e2_1 = ((long[])(sieve(27L)));
        if (!(Boolean)lists_equal(((long[])(e2_1)), ((long[])(new long[]{2, 3, 5, 7, 11, 13, 17, 19, 23})))) {
            throw new RuntimeException(String.valueOf("sieve(27) failed"));
        }
    }

    static void main() {
        test_sieve();
        System.out.println(_p(sieve(30L)));
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
