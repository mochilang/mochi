public class Main {
    static double PI;
    static double[][] kernel;

    static double to_radians(double deg) {
        return deg * PI / 180.0;
    }

    static double sin_taylor(double x) {
        double term = x;
        double sum = x;
        int i = 1;
        while (i < 10) {
            double k1 = 2.0 * (((Number)(i)).doubleValue());
            double k2 = k1 + 1.0;
            term = -term * x * x / (k1 * k2);
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double cos_taylor(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int i_1 = 1;
        while (i_1 < 10) {
            double k1_1 = 2.0 * (((Number)(i_1)).doubleValue()) - 1.0;
            double k2_1 = 2.0 * (((Number)(i_1)).doubleValue());
            term_1 = -term_1 * x * x / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static double exp_taylor(double x) {
        double term_2 = 1.0;
        double sum_2 = 1.0;
        double i_2 = 1.0;
        while (i_2 < 20.0) {
            term_2 = term_2 * x / i_2;
            sum_2 = sum_2 + term_2;
            i_2 = i_2 + 1.0;
        }
        return sum_2;
    }

    static double[][] gabor_filter_kernel(int ksize, double sigma, double theta, double lambd, double gamma, double psi) {
        int size = ksize;
        if (Math.floorMod(size, 2) == 0) {
            size = size + 1;
        }
        double[][] gabor = ((double[][])(new double[][]{}));
        int y = 0;
        while (y < size) {
            double[] row = ((double[])(new double[]{}));
            int x = 0;
            while (x < size) {
                double px = ((Number)((x - Math.floorDiv(size, 2)))).doubleValue();
                double py = ((Number)((y - Math.floorDiv(size, 2)))).doubleValue();
                double rad = to_radians(theta);
                double cos_theta = cos_taylor(rad);
                double sin_theta = sin_taylor(rad);
                double x_rot = cos_theta * px + sin_theta * py;
                double y_rot = -sin_theta * px + cos_theta * py;
                double exponent = -(x_rot * x_rot + gamma * gamma * y_rot * y_rot) / (2.0 * sigma * sigma);
                double value = exp_taylor(exponent) * cos_taylor(2.0 * PI * x_rot / lambd + psi);
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(value)).toArray()));
                x = x + 1;
            }
            gabor = ((double[][])(appendObj((double[][])gabor, row)));
            y = y + 1;
        }
        return gabor;
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        kernel = ((double[][])(gabor_filter_kernel(3, 8.0, 0.0, 10.0, 0.0, 0.0)));
        System.out.println(java.util.Arrays.deepToString(kernel));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
