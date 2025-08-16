public class Main {
    static double PI = (double)(3.141592653589793);
    static double TWO_PI = (double)(6.283185307179586);
    static long seed = 1L;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return seed;
    }

    static double random() {
        return (double)((((Number)(rand())).doubleValue())) / (double)(2147483648.0);
    }

    static double _mod(double x, double m) {
        return (double)(x) - (double)((double)((((Number)(((Number)((double)(x) / (double)(m))).intValue())).doubleValue())) * (double)(m));
    }

    static double cos(double x) {
        double y = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_1 = (double)((double)(y) * (double)(y));
        double y4_1 = (double)((double)(y2_1) * (double)(y2_1));
        double y6_1 = (double)((double)(y4_1) * (double)(y2_1));
        return (double)((double)((double)(1.0) - (double)((double)(y2_1) / (double)(2.0))) + (double)((double)(y4_1) / (double)(24.0))) - (double)((double)(y6_1) / (double)(720.0));
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 10L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double ln(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_1 = (double)(t);
        double sum_1 = (double)(0.0);
        long n_1 = 1L;
        while ((long)(n_1) <= 19L) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)((((Number)(n_1)).doubleValue()))));
            term_1 = (double)((double)((double)(term_1) * (double)(t)) * (double)(t));
            n_1 = (long)((long)(n_1) + 2L);
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double[] gaussian_distribution(double mean, double std_dev, long instance_count) {
        double[] res = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(instance_count)) {
            double u1_1 = (double)(random());
            double u2_1 = (double)(random());
            double r_1 = (double)(sqrtApprox((double)((double)(-2.0) * (double)(ln((double)(u1_1))))));
            double theta_1 = (double)((double)(TWO_PI) * (double)(u2_1));
            double z_1 = (double)((double)(r_1) * (double)(cos((double)(theta_1))));
            res = ((double[])(appendDouble(res, (double)((double)(mean) + (double)((double)(z_1) * (double)(std_dev))))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res;
    }

    static long[] y_generator(long class_count, long[] instance_count) {
        long[] res_1 = ((long[])(new long[]{}));
        long k_1 = 0L;
        while ((long)(k_1) < (long)(class_count)) {
            long i_5 = 0L;
            while ((long)(i_5) < (long)(instance_count[(int)((long)(k_1))])) {
                res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of((long)(k_1))).toArray()));
                i_5 = (long)((long)(i_5) + 1L);
            }
            k_1 = (long)((long)(k_1) + 1L);
        }
        return res_1;
    }

    static double calculate_mean(long instance_count, double[] items) {
        double total = (double)(0.0);
        long i_7 = 0L;
        while ((long)(i_7) < (long)(instance_count)) {
            total = (double)((double)(total) + (double)(items[(int)((long)(i_7))]));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return (double)(total) / (double)((((Number)(instance_count)).doubleValue()));
    }

    static double calculate_probabilities(long instance_count, long total_count) {
        return (double)((((Number)(instance_count)).doubleValue())) / (double)((((Number)(total_count)).doubleValue()));
    }

    static double calculate_variance(double[][] items, double[] means, long total_count) {
        double[] squared_diff = ((double[])(new double[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(items.length)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)(items[(int)((long)(i_9))].length)) {
                double diff_1 = (double)((double)(items[(int)((long)(i_9))][(int)((long)(j_1))]) - (double)(means[(int)((long)(i_9))]));
                squared_diff = ((double[])(appendDouble(squared_diff, (double)((double)(diff_1) * (double)(diff_1)))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_9 = (long)((long)(i_9) + 1L);
        }
        double sum_sq_1 = (double)(0.0);
        long k_3 = 0L;
        while ((long)(k_3) < (long)(squared_diff.length)) {
            sum_sq_1 = (double)((double)(sum_sq_1) + (double)(squared_diff[(int)((long)(k_3))]));
            k_3 = (long)((long)(k_3) + 1L);
        }
        long n_classes_1 = (long)(means.length);
        return (double)(((double)(1.0) / (double)((((Number)(((long)(total_count) - (long)(n_classes_1)))).doubleValue())))) * (double)(sum_sq_1);
    }

    static long[] predict_y_values(double[][] x_items, double[] means, double variance, double[] probabilities) {
        long[] results = ((long[])(new long[]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(x_items.length)) {
            long j_3 = 0L;
            while ((long)(j_3) < (long)(x_items[(int)((long)(i_11))].length)) {
                double[] temp_1 = ((double[])(new double[]{}));
                long k_5 = 0L;
                while ((long)(k_5) < (long)(x_items.length)) {
                    double discr_1 = (double)((double)((double)((double)(x_items[(int)((long)(i_11))][(int)((long)(j_3))]) * (double)(((double)(means[(int)((long)(k_5))]) / (double)(variance)))) - (double)((double)(((double)(means[(int)((long)(k_5))]) * (double)(means[(int)((long)(k_5))]))) / (double)(((double)(2.0) * (double)(variance))))) + (double)(ln((double)(probabilities[(int)((long)(k_5))]))));
                    temp_1 = ((double[])(appendDouble(temp_1, (double)(discr_1))));
                    k_5 = (long)((long)(k_5) + 1L);
                }
                long max_idx_1 = 0L;
                double max_val_1 = (double)(temp_1[(int)(0L)]);
                long t_2 = 1L;
                while ((long)(t_2) < (long)(temp_1.length)) {
                    if ((double)(temp_1[(int)((long)(t_2))]) > (double)(max_val_1)) {
                        max_val_1 = (double)(temp_1[(int)((long)(t_2))]);
                        max_idx_1 = (long)(t_2);
                    }
                    t_2 = (long)((long)(t_2) + 1L);
                }
                results = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(results), java.util.stream.LongStream.of((long)(max_idx_1))).toArray()));
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_11 = (long)((long)(i_11) + 1L);
        }
        return results;
    }

    static double accuracy(long[] actual_y, long[] predicted_y) {
        long correct = 0L;
        long i_13 = 0L;
        while ((long)(i_13) < (long)(actual_y.length)) {
            if ((long)(actual_y[(int)((long)(i_13))]) == (long)(predicted_y[(int)((long)(i_13))])) {
                correct = (long)((long)(correct) + 1L);
            }
            i_13 = (long)((long)(i_13) + 1L);
        }
        return (double)((double)((((Number)(correct)).doubleValue())) / (double)((((Number)(actual_y.length)).doubleValue()))) * (double)(100.0);
    }

    static void main() {
        seed = 1L;
        long[] counts_1 = ((long[])(new long[]{20, 20, 20}));
        double[] means_1 = ((double[])(new double[]{5.0, 10.0, 15.0}));
        double std_dev_1 = (double)(1.0);
        double[][] x_1 = ((double[][])(new double[][]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(counts_1.length)) {
            x_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(x_1), java.util.stream.Stream.of(new double[][]{gaussian_distribution((double)(means_1[(int)((long)(i_15))]), (double)(std_dev_1), (long)(counts_1[(int)((long)(i_15))]))})).toArray(double[][]::new)));
            i_15 = (long)((long)(i_15) + 1L);
        }
        long[] y_2 = ((long[])(y_generator((long)(counts_1.length), ((long[])(counts_1)))));
        double[] actual_means_1 = ((double[])(new double[]{}));
        i_15 = 0L;
        while ((long)(i_15) < (long)(counts_1.length)) {
            actual_means_1 = ((double[])(appendDouble(actual_means_1, (double)(calculate_mean((long)(counts_1[(int)((long)(i_15))]), ((double[])(x_1[(int)((long)(i_15))])))))));
            i_15 = (long)((long)(i_15) + 1L);
        }
        long total_count_1 = 0L;
        i_15 = 0L;
        while ((long)(i_15) < (long)(counts_1.length)) {
            total_count_1 = (long)((long)(total_count_1) + (long)(counts_1[(int)((long)(i_15))]));
            i_15 = (long)((long)(i_15) + 1L);
        }
        double[] probabilities_1 = ((double[])(new double[]{}));
        i_15 = 0L;
        while ((long)(i_15) < (long)(counts_1.length)) {
            probabilities_1 = ((double[])(appendDouble(probabilities_1, (double)(calculate_probabilities((long)(counts_1[(int)((long)(i_15))]), (long)(total_count_1))))));
            i_15 = (long)((long)(i_15) + 1L);
        }
        double variance_1 = (double)(calculate_variance(((double[][])(x_1)), ((double[])(actual_means_1)), (long)(total_count_1)));
        long[] predicted_1 = ((long[])(predict_y_values(((double[][])(x_1)), ((double[])(actual_means_1)), (double)(variance_1), ((double[])(probabilities_1)))));
        System.out.println(java.util.Arrays.toString(predicted_1));
        System.out.println(accuracy(((long[])(y_2)), ((long[])(predicted_1))));
    }
    public static void main(String[] args) {
        main();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
