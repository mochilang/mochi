public class Main {
    static double[][] image;
    static double[][] kernel;
    static double[][] conv;
    static double[][] activated;
    static double[][] pooled;
    static double[] flat;
    static double[] weights;
    static double bias;
    static double output_1;
    static double probability;

    static double[][] conv2d(double[][] image, double[][] kernel) {
        int rows = image.length;
        int cols = image[0].length;
        int k = kernel.length;
        double[][] output = ((double[][])(new double[][]{}));
        int i = 0;
        while (i <= rows - k) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j <= cols - k) {
                double sum = 0.0;
                int ki = 0;
                while (ki < k) {
                    int kj = 0;
                    while (kj < k) {
                        sum = sum + image[i + ki][j + kj] * kernel[ki][kj];
                        kj = kj + 1;
                    }
                    ki = ki + 1;
                }
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(sum)).toArray()));
                j = j + 1;
            }
            output = ((double[][])(appendObj(output, row)));
            i = i + 1;
        }
        return output;
    }

    static double[][] relu_matrix(double[][] m) {
        double[][] out = ((double[][])(new double[][]{}));
        for (double[] row : m) {
            double[] new_row = ((double[])(new double[]{}));
            for (double v : row) {
                if (v > 0.0) {
                    new_row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_row), java.util.stream.DoubleStream.of(v)).toArray()));
                } else {
                    new_row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
            }
            out = ((double[][])(appendObj(out, new_row)));
        }
        return out;
    }

    static double[][] max_pool2x2(double[][] m) {
        int rows_1 = m.length;
        int cols_1 = m[0].length;
        double[][] out_1 = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < rows_1) {
            double[] new_row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < cols_1) {
                double max_val = m[i_1][j_1];
                if (m[i_1][j_1 + 1] > max_val) {
                    max_val = m[i_1][j_1 + 1];
                }
                if (m[i_1 + 1][j_1] > max_val) {
                    max_val = m[i_1 + 1][j_1];
                }
                if (m[i_1 + 1][j_1 + 1] > max_val) {
                    max_val = m[i_1 + 1][j_1 + 1];
                }
                new_row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_row_1), java.util.stream.DoubleStream.of(max_val)).toArray()));
                j_1 = j_1 + 2;
            }
            out_1 = ((double[][])(appendObj(out_1, new_row_1)));
            i_1 = i_1 + 2;
        }
        return out_1;
    }

    static double[] flatten(double[][] m) {
        double[] res = ((double[])(new double[]{}));
        for (double[] row : m) {
            for (double v : row) {
                res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(v)).toArray()));
            }
        }
        return res;
    }

    static double dense(double[] inputs, double[] weights, double bias) {
        double s = bias;
        int i_2 = 0;
        while (i_2 < inputs.length) {
            s = s + inputs[i_2] * weights[i_2];
            i_2 = i_2 + 1;
        }
        return s;
    }

    static double exp_approx(double x) {
        double sum_1 = 1.0;
        double term = 1.0;
        int i_3 = 1;
        while (i_3 <= 10) {
            term = term * x / i_3;
            sum_1 = sum_1 + term;
            i_3 = i_3 + 1;
        }
        return sum_1;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + exp_approx(-x));
    }
    public static void main(String[] args) {
        image = ((double[][])(new double[][]{new double[]{0.0, 1.0, 1.0, 0.0, 0.0, 0.0}, new double[]{0.0, 1.0, 1.0, 0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0, 1.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0, 1.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0}}));
        kernel = ((double[][])(new double[][]{new double[]{1.0, 0.0, -1.0}, new double[]{1.0, 0.0, -1.0}, new double[]{1.0, 0.0, -1.0}}));
        conv = ((double[][])(conv2d(((double[][])(image)), ((double[][])(kernel)))));
        activated = ((double[][])(relu_matrix(((double[][])(conv)))));
        pooled = ((double[][])(max_pool2x2(((double[][])(activated)))));
        flat = ((double[])(flatten(((double[][])(pooled)))));
        weights = ((double[])(new double[]{0.5, -0.4, 0.3, 0.1}));
        bias = 0.0;
        output_1 = dense(((double[])(flat)), ((double[])(weights)), bias);
        probability = sigmoid(output_1);
        if (probability >= 0.5) {
            System.out.println("Abnormality detected");
        } else {
            System.out.println("Normal");
        }
        System.out.println("Probability:");
        System.out.println(probability);
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
