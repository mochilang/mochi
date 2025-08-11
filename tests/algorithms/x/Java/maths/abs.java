public class Main {

    static double abs_val(double num) {
        if (num < 0.0) {
            return -num;
        }
        return num;
    }

    static long abs_min(long[] x) {
        if (x.length == 0) {
            throw new RuntimeException(String.valueOf("abs_min() arg is an empty sequence"));
        }
        long j_1 = x[(int)(0)];
        long idx_1 = 0;
        while (idx_1 < x.length) {
            long i_1 = x[(int)(idx_1)];
            if (abs_val(((Number)(i_1)).doubleValue()) < abs_val(((Number)(j_1)).doubleValue())) {
                j_1 = i_1;
            }
            idx_1 = idx_1 + 1;
        }
        return j_1;
    }

    static long abs_max(long[] x) {
        if (x.length == 0) {
            throw new RuntimeException(String.valueOf("abs_max() arg is an empty sequence"));
        }
        long j_3 = x[(int)(0)];
        long idx_3 = 0;
        while (idx_3 < x.length) {
            long i_3 = x[(int)(idx_3)];
            if (abs_val(((Number)(i_3)).doubleValue()) > abs_val(((Number)(j_3)).doubleValue())) {
                j_3 = i_3;
            }
            idx_3 = idx_3 + 1;
        }
        return j_3;
    }

    static long abs_max_sort(long[] x) {
        if (x.length == 0) {
            throw new RuntimeException(String.valueOf("abs_max_sort() arg is an empty sequence"));
        }
        long[] arr_1 = ((long[])(new long[]{}));
        long i_5 = 0;
        while (i_5 < x.length) {
            arr_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(arr_1), java.util.stream.LongStream.of(x[(int)(i_5)])).toArray()));
            i_5 = i_5 + 1;
        }
        long n_1 = arr_1.length;
        long a_1 = 0;
        while (a_1 < n_1) {
            long b_1 = 0;
            while (b_1 < n_1 - a_1 - 1) {
                if (abs_val(((Number)(arr_1[(int)(b_1)])).doubleValue()) > abs_val(((Number)(arr_1[(int)(b_1 + 1)])).doubleValue())) {
                    long temp_1 = arr_1[(int)(b_1)];
arr_1[(int)(b_1)] = arr_1[(int)(b_1 + 1)];
arr_1[(int)(b_1 + 1)] = temp_1;
                }
                b_1 = b_1 + 1;
            }
            a_1 = a_1 + 1;
        }
        return arr_1[(int)(n_1 - 1)];
    }

    static void test_abs_val() {
        if (abs_val(0.0) != 0.0) {
            throw new RuntimeException(String.valueOf("abs_val(0) failed"));
        }
        if (abs_val(34.0) != 34.0) {
            throw new RuntimeException(String.valueOf("abs_val(34) failed"));
        }
        if (abs_val(-100000000000.0) != 100000000000.0) {
            throw new RuntimeException(String.valueOf("abs_val large failed"));
        }
        long[] a_3 = ((long[])(new long[]{-3, -1, 2, -11}));
        if (abs_max(((long[])(a_3))) != (-11)) {
            throw new RuntimeException(String.valueOf("abs_max failed"));
        }
        if (abs_max_sort(((long[])(a_3))) != (-11)) {
            throw new RuntimeException(String.valueOf("abs_max_sort failed"));
        }
        if (abs_min(((long[])(a_3))) != (-1)) {
            throw new RuntimeException(String.valueOf("abs_min failed"));
        }
    }

    static void main() {
        test_abs_val();
        System.out.println(abs_val(-34.0));
    }
    public static void main(String[] args) {
        main();
    }
}
