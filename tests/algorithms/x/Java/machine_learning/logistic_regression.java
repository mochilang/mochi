public class Main {
    static double[][] x;
    static double[] y_1;
    static double alpha;
    static long iterations;
    static double[] theta_2;

    static double expApprox(double x) {
        double y = x;
        boolean is_neg_1 = false;
        if (x < 0.0) {
            is_neg_1 = true;
            y = -x;
        }
        double term_1 = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1;
        while (n_1 < 30) {
            term_1 = term_1 * y / (((Number)(n_1)).doubleValue());
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        if (((Boolean)(is_neg_1))) {
            return 1.0 / sum_1;
        }
        return sum_1;
    }

    static double sigmoid(double z) {
        return 1.0 / (1.0 + expApprox(-z));
    }

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        long i_1 = 0;
        while (i_1 < a.length) {
            s = s + a[(int)(i_1)] * b[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static double[] zeros(long n) {
        double[] res = ((double[])(new double[]{}));
        long i_3 = 0;
        while (i_3 < n) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_3 = i_3 + 1;
        }
        return res;
    }

    static double[] logistic_reg(double alpha, double[][] x, double[] y, long iterations) {
        long m = x.length;
        long n_3 = x[(int)(0)].length;
        double[] theta_1 = ((double[])(zeros(n_3)));
        long iter_1 = 0;
        while (iter_1 < iterations) {
            double[] grad_1 = ((double[])(zeros(n_3)));
            long i_5 = 0;
            while (i_5 < m) {
                double z_1 = dot(((double[])(x[(int)(i_5)])), ((double[])(theta_1)));
                double h_1 = sigmoid(z_1);
                long k_1 = 0;
                while (k_1 < n_3) {
grad_1[(int)(k_1)] = grad_1[(int)(k_1)] + (h_1 - y[(int)(i_5)]) * x[(int)(i_5)][(int)(k_1)];
                    k_1 = k_1 + 1;
                }
                i_5 = i_5 + 1;
            }
            long k2_1 = 0;
            while (k2_1 < n_3) {
theta_1[(int)(k2_1)] = theta_1[(int)(k2_1)] - alpha * grad_1[(int)(k2_1)] / (((Number)(m)).doubleValue());
                k2_1 = k2_1 + 1;
            }
            iter_1 = iter_1 + 1;
        }
        return theta_1;
    }
    public static void main(String[] args) {
        x = ((double[][])(new double[][]{new double[]{0.5, 1.5}, new double[]{1.0, 1.0}, new double[]{1.5, 0.5}, new double[]{3.0, 3.5}, new double[]{3.5, 3.0}, new double[]{4.0, 4.0}}));
        y_1 = ((double[])(new double[]{0.0, 0.0, 0.0, 1.0, 1.0, 1.0}));
        alpha = 0.1;
        iterations = 1000;
        theta_2 = ((double[])(logistic_reg(alpha, ((double[][])(x)), ((double[])(y_1)), iterations)));
        for (int i = 0; i < theta_2.length; i++) {
            System.out.println(theta_2[(int)(i)]);
        }
    }
}
