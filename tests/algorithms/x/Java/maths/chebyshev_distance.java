public class Main {

    static double abs(double x) {
        if ((double)(x) >= (double)(0.0)) {
            return x;
        } else {
            return -x;
        }
    }

    static double chebyshev_distance(double[] point_a, double[] point_b) {
        if ((long)(point_a.length) != (long)(point_b.length)) {
            throw new RuntimeException(String.valueOf("Both points must have the same dimension."));
        }
        double max_diff_1 = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(point_a.length)) {
            double diff_1 = ((Number)(Math.abs((double)(point_a[(int)((long)(i_1))]) - (double)(point_b[(int)((long)(i_1))])))).doubleValue();
            if ((double)(diff_1) > (double)(max_diff_1)) {
                max_diff_1 = (double)(diff_1);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return max_diff_1;
    }
    public static void main(String[] args) {
        System.out.println(chebyshev_distance(((double[])(new double[]{1.0, 1.0})), ((double[])(new double[]{2.0, 2.0}))));
        System.out.println(chebyshev_distance(((double[])(new double[]{1.0, 1.0, 9.0})), ((double[])(new double[]{2.0, 2.0, -5.2}))));
    }
}
