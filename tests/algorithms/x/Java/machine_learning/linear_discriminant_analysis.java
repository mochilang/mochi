public class Main {
    static double PI;
    static double TWO_PI;
    static long seed = 0;

    static long rand() {
        seed = ((long)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (((Number)(rand())).doubleValue()) / 2147483648.0;
    }

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double cos(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2_1 = y * y;
        double y4_1 = y2_1 * y2_1;
        double y6_1 = y4_1 * y2_1;
        return 1.0 - y2_1 / 2.0 + y4_1 / 24.0 - y6_1 / 720.0;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 10) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_1 = t;
        double sum_1 = 0.0;
        long n_1 = 1;
        while (n_1 <= 19) {
            sum_1 = sum_1 + term_1 / (((Number)(n_1)).doubleValue());
            term_1 = term_1 * t * t;
            n_1 = n_1 + 2;
        }
        return 2.0 * sum_1;
    }

    static double[] gaussian_distribution(double mean, double std_dev, long instance_count) {
        double[] res = ((double[])(new double[]{}));
        long i_3 = 0;
        while (i_3 < instance_count) {
            double u1_1 = random();
            double u2_1 = random();
            double r_1 = sqrtApprox(-2.0 * ln(u1_1));
            double theta_1 = TWO_PI * u2_1;
            double z_1 = r_1 * cos(theta_1);
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(mean + z_1 * std_dev)).toArray()));
            i_3 = i_3 + 1;
        }
        return res;
    }

    static long[] y_generator(long class_count, long[] instance_count) {
        long[] res_1 = ((long[])(new long[]{}));
        long k_1 = 0;
        while (k_1 < class_count) {
            long i_5 = 0;
            while (i_5 < instance_count[(int)(k_1)]) {
                res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of(k_1)).toArray()));
                i_5 = i_5 + 1;
            }
            k_1 = k_1 + 1;
        }
        return res_1;
    }

    static double calculate_mean(long instance_count, double[] items) {
        double total = 0.0;
        long i_7 = 0;
        while (i_7 < instance_count) {
            total = total + items[(int)(i_7)];
            i_7 = i_7 + 1;
        }
        return total / (((Number)(instance_count)).doubleValue());
    }

    static double calculate_probabilities(long instance_count, long total_count) {
        return (((Number)(instance_count)).doubleValue()) / (((Number)(total_count)).doubleValue());
    }

    static double calculate_variance(double[][] items, double[] means, long total_count) {
        double[] squared_diff = ((double[])(new double[]{}));
        long i_9 = 0;
        while (i_9 < items.length) {
            long j_1 = 0;
            while (j_1 < items[(int)(i_9)].length) {
                double diff_1 = items[(int)(i_9)][(int)(j_1)] - means[(int)(i_9)];
                squared_diff = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(squared_diff), java.util.stream.DoubleStream.of(diff_1 * diff_1)).toArray()));
                j_1 = j_1 + 1;
            }
            i_9 = i_9 + 1;
        }
        double sum_sq_1 = 0.0;
        long k_3 = 0;
        while (k_3 < squared_diff.length) {
            sum_sq_1 = sum_sq_1 + squared_diff[(int)(k_3)];
            k_3 = k_3 + 1;
        }
        long n_classes_1 = means.length;
        return (1.0 / (((Number)((total_count - n_classes_1))).doubleValue())) * sum_sq_1;
    }

    static long[] predict_y_values(double[][] x_items, double[] means, double variance, double[] probabilities) {
        long[] results = ((long[])(new long[]{}));
        long i_11 = 0;
        while (i_11 < x_items.length) {
            long j_3 = 0;
            while (j_3 < x_items[(int)(i_11)].length) {
                double[] temp_1 = ((double[])(new double[]{}));
                long k_5 = 0;
                while (k_5 < x_items.length) {
                    double discr_1 = x_items[(int)(i_11)][(int)(j_3)] * (means[(int)(k_5)] / variance) - (means[(int)(k_5)] * means[(int)(k_5)]) / (2.0 * variance) + ln(probabilities[(int)(k_5)]);
                    temp_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(temp_1), java.util.stream.DoubleStream.of(discr_1)).toArray()));
                    k_5 = k_5 + 1;
                }
                long max_idx_1 = 0;
                double max_val_1 = temp_1[(int)(0)];
                long t_2 = 1;
                while (t_2 < temp_1.length) {
                    if (temp_1[(int)(t_2)] > max_val_1) {
                        max_val_1 = temp_1[(int)(t_2)];
                        max_idx_1 = t_2;
                    }
                    t_2 = t_2 + 1;
                }
                results = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(results), java.util.stream.LongStream.of(max_idx_1)).toArray()));
                j_3 = j_3 + 1;
            }
            i_11 = i_11 + 1;
        }
        return results;
    }

    static double accuracy(long[] actual_y, long[] predicted_y) {
        long correct = 0;
        long i_13 = 0;
        while (i_13 < actual_y.length) {
            if (actual_y[(int)(i_13)] == predicted_y[(int)(i_13)]) {
                correct = correct + 1;
            }
            i_13 = i_13 + 1;
        }
        return (((Number)(correct)).doubleValue()) / (((Number)(actual_y.length)).doubleValue()) * 100.0;
    }

    static void main() {
        seed = 1;
        long[] counts_1 = ((long[])(new long[]{20, 20, 20}));
        double[] means_1 = ((double[])(new double[]{5.0, 10.0, 15.0}));
        double std_dev_1 = 1.0;
        double[][] x_1 = ((double[][])(new double[][]{}));
        long i_15 = 0;
        while (i_15 < counts_1.length) {
            x_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(x_1), java.util.stream.Stream.of(gaussian_distribution(means_1[(int)(i_15)], std_dev_1, counts_1[(int)(i_15)]))).toArray(double[][]::new)));
            i_15 = i_15 + 1;
        }
        long[] y_2 = ((long[])(y_generator(counts_1.length, ((long[])(counts_1)))));
        double[] actual_means_1 = ((double[])(new double[]{}));
        i_15 = 0;
        while (i_15 < counts_1.length) {
            actual_means_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(actual_means_1), java.util.stream.DoubleStream.of(calculate_mean(counts_1[(int)(i_15)], ((double[])(x_1[(int)(i_15)]))))).toArray()));
            i_15 = i_15 + 1;
        }
        long total_count_1 = 0;
        i_15 = 0;
        while (i_15 < counts_1.length) {
            total_count_1 = total_count_1 + counts_1[(int)(i_15)];
            i_15 = i_15 + 1;
        }
        double[] probabilities_1 = ((double[])(new double[]{}));
        i_15 = 0;
        while (i_15 < counts_1.length) {
            probabilities_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(probabilities_1), java.util.stream.DoubleStream.of(calculate_probabilities(counts_1[(int)(i_15)], total_count_1))).toArray()));
            i_15 = i_15 + 1;
        }
        double variance_1 = calculate_variance(((double[][])(x_1)), ((double[])(actual_means_1)), total_count_1);
        long[] predicted_1 = ((long[])(predict_y_values(((double[][])(x_1)), ((double[])(actual_means_1)), variance_1, ((double[])(probabilities_1)))));
        System.out.println(java.util.Arrays.toString(predicted_1));
        System.out.println(accuracy(((long[])(y_2)), ((long[])(predicted_1))));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        TWO_PI = 6.283185307179586;
        seed = 1;
        main();
    }
}
