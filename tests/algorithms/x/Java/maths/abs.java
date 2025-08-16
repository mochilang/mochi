public class Main {

    static double abs_val(double num) {
        if ((double)(num) < (double)(0.0)) {
            return -num;
        }
        return num;
    }

    static long abs_min(long[] x) {
        if ((long)(x.length) == 0L) {
            throw new RuntimeException(String.valueOf("abs_min() arg is an empty sequence"));
        }
        long j_1 = (long)(x[(int)(0L)]);
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(x.length)) {
            long i_1 = (long)(x[(int)((long)(idx_1))]);
            if ((double)(abs_val((double)(((Number)(i_1)).doubleValue()))) < (double)(abs_val((double)(((Number)(j_1)).doubleValue())))) {
                j_1 = (long)(i_1);
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        return j_1;
    }

    static long abs_max(long[] x) {
        if ((long)(x.length) == 0L) {
            throw new RuntimeException(String.valueOf("abs_max() arg is an empty sequence"));
        }
        long j_3 = (long)(x[(int)(0L)]);
        long idx_3 = 0L;
        while ((long)(idx_3) < (long)(x.length)) {
            long i_3 = (long)(x[(int)((long)(idx_3))]);
            if ((double)(abs_val((double)(((Number)(i_3)).doubleValue()))) > (double)(abs_val((double)(((Number)(j_3)).doubleValue())))) {
                j_3 = (long)(i_3);
            }
            idx_3 = (long)((long)(idx_3) + 1L);
        }
        return j_3;
    }

    static long abs_max_sort(long[] x) {
        if ((long)(x.length) == 0L) {
            throw new RuntimeException(String.valueOf("abs_max_sort() arg is an empty sequence"));
        }
        long[] arr_1 = ((long[])(new long[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(x.length)) {
            arr_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(arr_1), java.util.stream.LongStream.of((long)(x[(int)((long)(i_5))]))).toArray()));
            i_5 = (long)((long)(i_5) + 1L);
        }
        long n_1 = (long)(arr_1.length);
        long a_1 = 0L;
        while ((long)(a_1) < (long)(n_1)) {
            long b_1 = 0L;
            while ((long)(b_1) < (long)((long)((long)(n_1) - (long)(a_1)) - 1L)) {
                if ((double)(abs_val((double)(((Number)(arr_1[(int)((long)(b_1))])).doubleValue()))) > (double)(abs_val((double)(((Number)(arr_1[(int)((long)((long)(b_1) + 1L))])).doubleValue())))) {
                    long temp_1 = (long)(arr_1[(int)((long)(b_1))]);
arr_1[(int)((long)(b_1))] = (long)(arr_1[(int)((long)((long)(b_1) + 1L))]);
arr_1[(int)((long)((long)(b_1) + 1L))] = (long)(temp_1);
                }
                b_1 = (long)((long)(b_1) + 1L);
            }
            a_1 = (long)((long)(a_1) + 1L);
        }
        return arr_1[(int)((long)((long)(n_1) - 1L))];
    }

    static void test_abs_val() {
        if ((double)(abs_val((double)(0.0))) != (double)(0.0)) {
            throw new RuntimeException(String.valueOf("abs_val(0) failed"));
        }
        if ((double)(abs_val((double)(34.0))) != (double)(34.0)) {
            throw new RuntimeException(String.valueOf("abs_val(34) failed"));
        }
        if ((double)(abs_val((double)(-100000000000.0))) != (double)(100000000000.0)) {
            throw new RuntimeException(String.valueOf("abs_val large failed"));
        }
        long[] a_3 = ((long[])(new long[]{-3, -1, 2, -11}));
        if ((long)(abs_max(((long[])(a_3)))) != (long)((-11))) {
            throw new RuntimeException(String.valueOf("abs_max failed"));
        }
        if ((long)(abs_max_sort(((long[])(a_3)))) != (long)((-11))) {
            throw new RuntimeException(String.valueOf("abs_max_sort failed"));
        }
        if ((long)(abs_min(((long[])(a_3)))) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("abs_min failed"));
        }
    }

    static void main() {
        test_abs_val();
        System.out.println(abs_val((double)(-34.0)));
    }
    public static void main(String[] args) {
        main();
    }
}
