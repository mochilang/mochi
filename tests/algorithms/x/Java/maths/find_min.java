public class Main {

    static double find_min_iterative(double[] nums) {
        if ((long)(nums.length) == 0L) {
            throw new RuntimeException(String.valueOf("find_min_iterative() arg is an empty sequence"));
        }
        double min_num_1 = (double)(nums[(int)(0L)]);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(nums.length)) {
            double num_1 = (double)(nums[(int)((long)(i_1))]);
            if ((double)(num_1) < (double)(min_num_1)) {
                min_num_1 = (double)(num_1);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return min_num_1;
    }

    static double find_min_recursive(double[] nums, long left, long right) {
        long n = (long)(nums.length);
        if ((long)(n) == 0L) {
            throw new RuntimeException(String.valueOf("find_min_recursive() arg is an empty sequence"));
        }
        if ((long)(left) >= (long)(n) || (long)(left) < (long)((0L - (long)(n))) || (long)(right) >= (long)(n) || (long)(right) < (long)((0L - (long)(n)))) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        long l_1 = (long)(left);
        long r_1 = (long)(right);
        if ((long)(l_1) < 0L) {
            l_1 = (long)((long)(n) + (long)(l_1));
        }
        if ((long)(r_1) < 0L) {
            r_1 = (long)((long)(n) + (long)(r_1));
        }
        if ((long)(l_1) == (long)(r_1)) {
            return nums[(int)((long)(l_1))];
        }
        long mid_1 = Math.floorDiv(((long)(l_1) + (long)(r_1)), 2);
        double left_min_1 = (double)(find_min_recursive(((double[])(nums)), (long)(l_1), (long)(mid_1)));
        double right_min_1 = (double)(find_min_recursive(((double[])(nums)), (long)((long)(mid_1) + 1L), (long)(r_1)));
        if ((double)(left_min_1) <= (double)(right_min_1)) {
            return left_min_1;
        }
        return right_min_1;
    }

    static void test_find_min() {
        double[] a = ((double[])(new double[]{3.0, 2.0, 1.0}));
        if ((double)(find_min_iterative(((double[])(a)))) != (double)(1.0)) {
            throw new RuntimeException(String.valueOf("iterative test1 failed"));
        }
        if ((double)(find_min_recursive(((double[])(a)), 0L, (long)((long)(a.length) - 1L))) != (double)(1.0)) {
            throw new RuntimeException(String.valueOf("recursive test1 failed"));
        }
        double[] b_1 = ((double[])(new double[]{-3.0, -2.0, -1.0}));
        if ((double)(find_min_iterative(((double[])(b_1)))) != (double)((-3.0))) {
            throw new RuntimeException(String.valueOf("iterative test2 failed"));
        }
        if ((double)(find_min_recursive(((double[])(b_1)), 0L, (long)((long)(b_1.length) - 1L))) != (double)((-3.0))) {
            throw new RuntimeException(String.valueOf("recursive test2 failed"));
        }
        double[] c_1 = ((double[])(new double[]{3.0, -3.0, 0.0}));
        if ((double)(find_min_iterative(((double[])(c_1)))) != (double)((-3.0))) {
            throw new RuntimeException(String.valueOf("iterative test3 failed"));
        }
        if ((double)(find_min_recursive(((double[])(c_1)), 0L, (long)((long)(c_1.length) - 1L))) != (double)((-3.0))) {
            throw new RuntimeException(String.valueOf("recursive test3 failed"));
        }
        double[] d_1 = ((double[])(new double[]{1.0, 3.0, 5.0, 7.0, 9.0, 2.0, 4.0, 6.0, 8.0, 10.0}));
        if ((double)(find_min_recursive(((double[])(d_1)), (long)((0L - (long)(d_1.length))), (long)((0L - 1L)))) != (double)(1.0)) {
            throw new RuntimeException(String.valueOf("negative index test failed"));
        }
    }

    static void main() {
        test_find_min();
        double[] sample_1 = ((double[])(new double[]{0.0, 1.0, 2.0, 3.0, 4.0, 5.0, -3.0, 24.0, -56.0}));
        System.out.println(_p(find_min_iterative(((double[])(sample_1)))));
    }
    public static void main(String[] args) {
        main();
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
