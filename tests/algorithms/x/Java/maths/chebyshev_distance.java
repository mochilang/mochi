public class Main {

    static double abs(double x) {
        if (x >= 0.0) {
            return x;
        } else {
            return -x;
        }
    }

    static double chebyshev_distance(double[] point_a, double[] point_b) {
        if (point_a.length != point_b.length) {
            throw new RuntimeException(String.valueOf("Both points must have the same dimension."));
        }
        double max_diff_1 = 0.0;
        long i_1 = 0;
        while (i_1 < point_a.length) {
            double diff_1 = ((Number)(Math.abs(point_a[(int)(i_1)] - point_b[(int)(i_1)]))).doubleValue();
            if (diff_1 > max_diff_1) {
                max_diff_1 = diff_1;
            }
            i_1 = i_1 + 1;
        }
        return max_diff_1;
    }
    public static void main(String[] args) {
        System.out.println(chebyshev_distance(((double[])(new double[]{1.0, 1.0})), ((double[])(new double[]{2.0, 2.0}))));
        System.out.println(chebyshev_distance(((double[])(new double[]{1.0, 1.0, 9.0})), ((double[])(new double[]{2.0, 2.0, -5.2}))));
    }
}
