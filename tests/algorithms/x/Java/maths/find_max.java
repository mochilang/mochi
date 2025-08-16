public class Main {

    static long normalize_index(long index, long n) {
        if ((long)(index) < 0L) {
            return (long)(n) + (long)(index);
        }
        return index;
    }

    static double find_max_iterative(double[] nums) {
        if ((long)(nums.length) == 0L) {
            throw new RuntimeException(String.valueOf("find_max_iterative() arg is an empty sequence"));
        }
        double max_num_1 = (double)(nums[(int)(0L)]);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(nums.length)) {
            double x_1 = (double)(nums[(int)((long)(i_1))]);
            if ((double)(x_1) > (double)(max_num_1)) {
                max_num_1 = (double)(x_1);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return max_num_1;
    }

    static double find_max_recursive(double[] nums, long left, long right) {
        long n = (long)(nums.length);
        if ((long)(n) == 0L) {
            throw new RuntimeException(String.valueOf("find_max_recursive() arg is an empty sequence"));
        }
        if ((long)(left) >= (long)(n) || (long)(left) < (long)((0L - (long)(n))) || (long)(right) >= (long)(n) || (long)(right) < (long)((0L - (long)(n)))) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        long l_1 = (long)(normalize_index((long)(left), (long)(n)));
        long r_1 = (long)(normalize_index((long)(right), (long)(n)));
        if ((long)(l_1) == (long)(r_1)) {
            return nums[(int)((long)(l_1))];
        }
        long mid_1 = Math.floorDiv(((long)(l_1) + (long)(r_1)), 2);
        double left_max_1 = (double)(find_max_recursive(((double[])(nums)), (long)(l_1), (long)(mid_1)));
        double right_max_1 = (double)(find_max_recursive(((double[])(nums)), (long)((long)(mid_1) + 1L), (long)(r_1)));
        if ((double)(left_max_1) >= (double)(right_max_1)) {
            return left_max_1;
        }
        return right_max_1;
    }

    static void test_find_max() {
        double[] arr = ((double[])(new double[]{2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0}));
        if ((double)(find_max_iterative(((double[])(arr)))) != (double)(94.0)) {
            throw new RuntimeException(String.valueOf("find_max_iterative failed"));
        }
        if ((double)(find_max_recursive(((double[])(arr)), 0L, (long)((long)(arr.length) - 1L))) != (double)(94.0)) {
            throw new RuntimeException(String.valueOf("find_max_recursive failed"));
        }
        if ((double)(find_max_recursive(((double[])(arr)), (long)(-arr.length), (long)(-1))) != (double)(94.0)) {
            throw new RuntimeException(String.valueOf("negative index handling failed"));
        }
    }

    static void main() {
        test_find_max();
        double[] nums_1 = ((double[])(new double[]{2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0}));
        System.out.println(find_max_iterative(((double[])(nums_1))));
        System.out.println(find_max_recursive(((double[])(nums_1)), 0L, (long)((long)(nums_1.length) - 1L)));
    }
    public static void main(String[] args) {
        main();
    }
}
