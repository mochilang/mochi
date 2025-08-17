public class Main {

    static long[] bubble_sort(long[] xs) {
        long[] arr = ((long[])(xs));
        long n_1 = (long)(arr.length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n_1)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)((long)(n_1) - (long)(i_1)) - 1L)) {
                if ((long)(arr[(int)((long)(j_1))]) > (long)(arr[(int)((long)((long)(j_1) + 1L))])) {
                    long tmp_1 = (long)(arr[(int)((long)(j_1))]);
arr[(int)((long)(j_1))] = (long)(arr[(int)((long)((long)(j_1) + 1L))]);
arr[(int)((long)((long)(j_1) + 1L))] = (long)(tmp_1);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return arr;
    }

    static long[] factors(long num) {
        long[] values = ((long[])(new long[]{1}));
        long i_3 = 2L;
        while ((long)((long)(i_3) * (long)(i_3)) <= (long)(num)) {
            if (Math.floorMod(num, i_3) == 0L) {
                values = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(values), java.util.stream.LongStream.of((long)(i_3))).toArray()));
                Object d_1 = Math.floorDiv(((long)(num)), ((long)(i_3)));
                if (((Number)(d_1)).intValue() != (long)(i_3)) {
                    values = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(values), java.util.stream.LongStream.of(((Number)(d_1)).longValue())).toArray()));
                }
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return bubble_sort(((long[])(values)));
    }

    static long sum_list(long[] xs) {
        long total = 0L;
        long i_5 = 0L;
        while ((long)(i_5) < (long)(xs.length)) {
            total = (long)((long)(total) + (long)(xs[(int)((long)(i_5))]));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return total;
    }

    static boolean abundant(long n) {
        return (long)(sum_list(((long[])(factors((long)(n)))))) > (long)(n);
    }

    static boolean semi_perfect(long number) {
        if ((long)(number) <= 0L) {
            return true;
        }
        long[] values_2 = ((long[])(factors((long)(number))));
        boolean[] possible_1 = ((boolean[])(new boolean[]{}));
        long j_3 = 0L;
        while ((long)(j_3) <= (long)(number)) {
            possible_1 = ((boolean[])(appendBool(possible_1, (long)(j_3) == 0L)));
            j_3 = (long)((long)(j_3) + 1L);
        }
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(values_2.length)) {
            long v_1 = (long)(values_2[(int)((long)(idx_1))]);
            long s_1 = (long)(number);
            while ((long)(s_1) >= (long)(v_1)) {
                if (possible_1[(int)((long)((long)(s_1) - (long)(v_1)))]) {
possible_1[(int)((long)(s_1))] = true;
                }
                s_1 = (long)((long)(s_1) - 1L);
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        return possible_1[(int)((long)(number))];
    }

    static boolean weird(long number) {
        return abundant((long)(number)) && (semi_perfect((long)(number)) == false);
    }

    static void run_tests() {
        if (!java.util.Arrays.equals(factors(12L), new long[]{1, 2, 3, 4, 6})) {
            throw new RuntimeException(String.valueOf("factors 12 failed"));
        }
        if (!java.util.Arrays.equals(factors(1L), new long[]{1})) {
            throw new RuntimeException(String.valueOf("factors 1 failed"));
        }
        if (!java.util.Arrays.equals(factors(100L), new long[]{1, 2, 4, 5, 10, 20, 25, 50})) {
            throw new RuntimeException(String.valueOf("factors 100 failed"));
        }
        if ((abundant(0L) != true)) {
            throw new RuntimeException(String.valueOf("abundant 0 failed"));
        }
        if ((abundant(1L) != false)) {
            throw new RuntimeException(String.valueOf("abundant 1 failed"));
        }
        if ((abundant(12L) != true)) {
            throw new RuntimeException(String.valueOf("abundant 12 failed"));
        }
        if ((abundant(13L) != false)) {
            throw new RuntimeException(String.valueOf("abundant 13 failed"));
        }
        if ((abundant(20L) != true)) {
            throw new RuntimeException(String.valueOf("abundant 20 failed"));
        }
        if ((semi_perfect(0L) != true)) {
            throw new RuntimeException(String.valueOf("semi_perfect 0 failed"));
        }
        if ((semi_perfect(1L) != true)) {
            throw new RuntimeException(String.valueOf("semi_perfect 1 failed"));
        }
        if ((semi_perfect(12L) != true)) {
            throw new RuntimeException(String.valueOf("semi_perfect 12 failed"));
        }
        if ((semi_perfect(13L) != false)) {
            throw new RuntimeException(String.valueOf("semi_perfect 13 failed"));
        }
        if ((weird(0L) != false)) {
            throw new RuntimeException(String.valueOf("weird 0 failed"));
        }
        if ((weird(70L) != true)) {
            throw new RuntimeException(String.valueOf("weird 70 failed"));
        }
        if ((weird(77L) != false)) {
            throw new RuntimeException(String.valueOf("weird 77 failed"));
        }
    }

    static void main() {
        run_tests();
        long[] nums_1 = ((long[])(new long[]{69, 70, 71}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(nums_1.length)) {
            long n_3 = (long)(nums_1[(int)((long)(i_7))]);
            if (weird((long)(n_3))) {
                System.out.println(_p(n_3) + " is weird.");
            } else {
                System.out.println(_p(n_3) + " is not weird.");
            }
            i_7 = (long)((long)(i_7) + 1L);
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
}
