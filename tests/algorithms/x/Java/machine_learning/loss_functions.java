public class Main {

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double maxf(double a, double b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static double minf(double a, double b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static double clip(double x, double lo, double hi) {
        return maxf(lo, minf(x, hi));
    }

    static double to_float(long x) {
        return x * 1.0;
    }

    static double powf(double base, double exp) {
        double result = 1.0;
        long i_1 = 0;
        long n_1 = ((Number)(Main::exp)).intValue();
        while (i_1 < n_1) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double ln(double x) {
        if (x <= 0.0) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = (x - 1.0) / (x + 1.0);
        double y2_1 = y_1 * y_1;
        double term_1 = y_1;
        double sum_1 = 0.0;
        long k_1 = 0;
        while (k_1 < 10) {
            double denom_1 = to_float(2 * k_1 + 1);
            sum_1 = sum_1 + term_1 / denom_1;
            term_1 = term_1 * y2_1;
            k_1 = k_1 + 1;
        }
        return 2.0 * sum_1;
    }

    static double exp(double x) {
        double term_2 = 1.0;
        double sum_3 = 1.0;
        long n_3 = 1;
        while (n_3 < 20) {
            term_2 = term_2 * x / to_float(n_3);
            sum_3 = sum_3 + term_2;
            n_3 = n_3 + 1;
        }
        return sum_3;
    }

    static double mean(double[] v) {
        double total = 0.0;
        long i_3 = 0;
        while (i_3 < v.length) {
            total = total + v[(int)(i_3)];
            i_3 = i_3 + 1;
        }
        return total / to_float(v.length);
    }

    static double binary_cross_entropy(double[] y_true, double[] y_pred, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_1 = ((double[])(new double[]{}));
        long i_5 = 0;
        while (i_5 < y_true.length) {
            double yt_1 = y_true[(int)(i_5)];
            double yp_1 = clip(y_pred[(int)(i_5)], epsilon, 1.0 - epsilon);
            double loss_1 = -(yt_1 * ln(yp_1) + (1.0 - yt_1) * ln(1.0 - yp_1));
            losses_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_1), java.util.stream.DoubleStream.of(loss_1)).toArray()));
            i_5 = i_5 + 1;
        }
        return mean(((double[])(losses_1)));
    }

    static double binary_focal_cross_entropy(double[] y_true, double[] y_pred, double gamma, double alpha, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_3 = ((double[])(new double[]{}));
        long i_7 = 0;
        while (i_7 < y_true.length) {
            double yt_3 = y_true[(int)(i_7)];
            double yp_3 = clip(y_pred[(int)(i_7)], epsilon, 1.0 - epsilon);
            double term1_1 = alpha * powf(1.0 - yp_3, gamma) * yt_3 * ln(yp_3);
            double term2_1 = (1.0 - alpha) * powf(yp_3, gamma) * (1.0 - yt_3) * ln(1.0 - yp_3);
            losses_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_3), java.util.stream.DoubleStream.of(-(term1_1 + term2_1))).toArray()));
            i_7 = i_7 + 1;
        }
        return mean(((double[])(losses_3)));
    }

    static double categorical_cross_entropy(double[][] y_true, double[][] y_pred, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same shape."));
        }
        long rows_1 = y_true.length;
        double total_2 = 0.0;
        long i_9 = 0;
        while (i_9 < rows_1) {
            if (y_true[(int)(i_9)].length != y_pred[(int)(i_9)].length) {
                throw new RuntimeException(String.valueOf("Input arrays must have the same shape."));
            }
            double sum_true_1 = 0.0;
            double sum_pred_1 = 0.0;
            long j_1 = 0;
            while (j_1 < y_true[(int)(i_9)].length) {
                double yt_5 = y_true[(int)(i_9)][(int)(j_1)];
                double yp_6 = y_pred[(int)(i_9)][(int)(j_1)];
                if ((yt_5 != 0.0 && yt_5 != 1.0)) {
                    throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
                }
                sum_true_1 = sum_true_1 + yt_5;
                sum_pred_1 = sum_pred_1 + yp_6;
                j_1 = j_1 + 1;
            }
            if (sum_true_1 != 1.0) {
                throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
            }
            if (absf(sum_pred_1 - 1.0) > epsilon) {
                throw new RuntimeException(String.valueOf("Predicted probabilities must sum to approximately 1."));
            }
            j_1 = 0;
            while (j_1 < y_true[(int)(i_9)].length) {
                double yp_7 = clip(y_pred[(int)(i_9)][(int)(j_1)], epsilon, 1.0);
                total_2 = total_2 - (y_true[(int)(i_9)][(int)(j_1)] * ln(yp_7));
                j_1 = j_1 + 1;
            }
            i_9 = i_9 + 1;
        }
        return total_2;
    }

    static double categorical_focal_cross_entropy(double[][] y_true, double[][] y_pred, double[] alpha, double gamma, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Shape of y_true and y_pred must be the same."));
        }
        long rows_3 = y_true.length;
        long cols_1 = y_true[(int)(0)].length;
        double[] a_1 = ((double[])(alpha));
        if (a_1.length == 0) {
            double[] tmp_1 = ((double[])(new double[]{}));
            long j_3 = 0;
            while (j_3 < cols_1) {
                tmp_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(tmp_1), java.util.stream.DoubleStream.of(1.0)).toArray()));
                j_3 = j_3 + 1;
            }
            a_1 = ((double[])(tmp_1));
        }
        if (a_1.length != cols_1) {
            throw new RuntimeException(String.valueOf("Length of alpha must match the number of classes."));
        }
        double total_4 = 0.0;
        long i_11 = 0;
        while (i_11 < rows_3) {
            if (y_true[(int)(i_11)].length != cols_1 || y_pred[(int)(i_11)].length != cols_1) {
                throw new RuntimeException(String.valueOf("Shape of y_true and y_pred must be the same."));
            }
            double sum_true_3 = 0.0;
            double sum_pred_3 = 0.0;
            long j_5 = 0;
            while (j_5 < cols_1) {
                double yt_7 = y_true[(int)(i_11)][(int)(j_5)];
                double yp_10 = y_pred[(int)(i_11)][(int)(j_5)];
                if ((yt_7 != 0.0 && yt_7 != 1.0)) {
                    throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
                }
                sum_true_3 = sum_true_3 + yt_7;
                sum_pred_3 = sum_pred_3 + yp_10;
                j_5 = j_5 + 1;
            }
            if (sum_true_3 != 1.0) {
                throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
            }
            if (absf(sum_pred_3 - 1.0) > epsilon) {
                throw new RuntimeException(String.valueOf("Predicted probabilities must sum to approximately 1."));
            }
            double row_loss_1 = 0.0;
            j_5 = 0;
            while (j_5 < cols_1) {
                double yp_11 = clip(y_pred[(int)(i_11)][(int)(j_5)], epsilon, 1.0);
                row_loss_1 = row_loss_1 + a_1[(int)(j_5)] * powf(1.0 - yp_11, gamma) * y_true[(int)(i_11)][(int)(j_5)] * ln(yp_11);
                j_5 = j_5 + 1;
            }
            total_4 = total_4 - row_loss_1;
            i_11 = i_11 + 1;
        }
        return total_4 / to_float(rows_3);
    }

    static double hinge_loss(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Length of predicted and actual array must be same."));
        }
        double[] losses_5 = ((double[])(new double[]{}));
        long i_13 = 0;
        while (i_13 < y_true.length) {
            double yt_9 = y_true[(int)(i_13)];
            if ((yt_9 != (-1.0) && yt_9 != 1.0)) {
                throw new RuntimeException(String.valueOf("y_true can have values -1 or 1 only."));
            }
            double pred_1 = y_pred[(int)(i_13)];
            double l_1 = maxf(0.0, 1.0 - yt_9 * pred_1);
            losses_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_5), java.util.stream.DoubleStream.of(l_1)).toArray()));
            i_13 = i_13 + 1;
        }
        return mean(((double[])(losses_5)));
    }

    static double huber_loss(double[] y_true, double[] y_pred, double delta) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_6 = 0.0;
        long i_15 = 0;
        while (i_15 < y_true.length) {
            double diff_1 = y_true[(int)(i_15)] - y_pred[(int)(i_15)];
            double adiff_1 = absf(diff_1);
            if (adiff_1 <= delta) {
                total_6 = total_6 + 0.5 * diff_1 * diff_1;
            } else {
                total_6 = total_6 + delta * (adiff_1 - 0.5 * delta);
            }
            i_15 = i_15 + 1;
        }
        return total_6 / to_float(y_true.length);
    }

    static double mean_squared_error(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_7 = ((double[])(new double[]{}));
        long i_17 = 0;
        while (i_17 < y_true.length) {
            double diff_3 = y_true[(int)(i_17)] - y_pred[(int)(i_17)];
            losses_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_7), java.util.stream.DoubleStream.of(diff_3 * diff_3)).toArray()));
            i_17 = i_17 + 1;
        }
        return mean(((double[])(losses_7)));
    }

    static double mean_absolute_error(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_8 = 0.0;
        long i_19 = 0;
        while (i_19 < y_true.length) {
            total_8 = total_8 + absf(y_true[(int)(i_19)] - y_pred[(int)(i_19)]);
            i_19 = i_19 + 1;
        }
        return total_8 / to_float(y_true.length);
    }

    static double mean_squared_logarithmic_error(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_10 = 0.0;
        long i_21 = 0;
        while (i_21 < y_true.length) {
            double a_3 = ln(1.0 + y_true[(int)(i_21)]);
            double b_1 = ln(1.0 + y_pred[(int)(i_21)]);
            double diff_5 = a_3 - b_1;
            total_10 = total_10 + diff_5 * diff_5;
            i_21 = i_21 + 1;
        }
        return total_10 / to_float(y_true.length);
    }

    static double mean_absolute_percentage_error(double[] y_true, double[] y_pred, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("The length of the two arrays should be the same."));
        }
        double total_12 = 0.0;
        long i_23 = 0;
        while (i_23 < y_true.length) {
            double yt_11 = y_true[(int)(i_23)];
            if (yt_11 == 0.0) {
                yt_11 = epsilon;
            }
            total_12 = total_12 + absf((yt_11 - y_pred[(int)(i_23)]) / yt_11);
            i_23 = i_23 + 1;
        }
        return total_12 / to_float(y_true.length);
    }

    static double perplexity_loss(long[][] y_true, double[][][] y_pred, double epsilon) {
        long batch = y_true.length;
        if (batch != y_pred.length) {
            throw new RuntimeException(String.valueOf("Batch size of y_true and y_pred must be equal."));
        }
        long sentence_len_1 = y_true[(int)(0)].length;
        if (sentence_len_1 != y_pred[(int)(0)].length) {
            throw new RuntimeException(String.valueOf("Sentence length of y_true and y_pred must be equal."));
        }
        long vocab_size_1 = y_pred[(int)(0)][(int)(0)].length;
        long b_3 = 0;
        double total_perp_1 = 0.0;
        while (b_3 < batch) {
            if (y_true[(int)(b_3)].length != sentence_len_1 || y_pred[(int)(b_3)].length != sentence_len_1) {
                throw new RuntimeException(String.valueOf("Sentence length of y_true and y_pred must be equal."));
            }
            double sum_log_1 = 0.0;
            long j_7 = 0;
            while (j_7 < sentence_len_1) {
                long label_1 = y_true[(int)(b_3)][(int)(j_7)];
                if (label_1 >= vocab_size_1) {
                    throw new RuntimeException(String.valueOf("Label value must not be greater than vocabulary size."));
                }
                double prob_1 = clip(y_pred[(int)(b_3)][(int)(j_7)][(int)(label_1)], epsilon, 1.0);
                sum_log_1 = sum_log_1 + ln(prob_1);
                j_7 = j_7 + 1;
            }
            double mean_log_1 = sum_log_1 / to_float(sentence_len_1);
            double perp_1 = exp(-mean_log_1);
            total_perp_1 = total_perp_1 + perp_1;
            b_3 = b_3 + 1;
        }
        return total_perp_1 / to_float(batch);
    }

    static double smooth_l1_loss(double[] y_true, double[] y_pred, double beta) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("The length of the two arrays should be the same."));
        }
        double total_14 = 0.0;
        long i_25 = 0;
        while (i_25 < y_true.length) {
            double diff_7 = absf(y_true[(int)(i_25)] - y_pred[(int)(i_25)]);
            if (diff_7 < beta) {
                total_14 = total_14 + 0.5 * diff_7 * diff_7 / beta;
            } else {
                total_14 = total_14 + diff_7 - 0.5 * beta;
            }
            i_25 = i_25 + 1;
        }
        return total_14 / to_float(y_true.length);
    }

    static double kullback_leibler_divergence(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_16 = 0.0;
        long i_27 = 0;
        while (i_27 < y_true.length) {
            total_16 = total_16 + y_true[(int)(i_27)] * ln(y_true[(int)(i_27)] / y_pred[(int)(i_27)]);
            i_27 = i_27 + 1;
        }
        return total_16;
    }

    static void main() {
        double[] y_true_bc = ((double[])(new double[]{0.0, 1.0, 1.0, 0.0, 1.0}));
        double[] y_pred_bc_1 = ((double[])(new double[]{0.2, 0.7, 0.9, 0.3, 0.8}));
        System.out.println(binary_cross_entropy(((double[])(y_true_bc)), ((double[])(y_pred_bc_1)), 1e-15));
        System.out.println(binary_focal_cross_entropy(((double[])(y_true_bc)), ((double[])(y_pred_bc_1)), 2.0, 0.25, 1e-15));
        double[][] y_true_cce_1 = ((double[][])(new double[][]{new double[]{1.0, 0.0, 0.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 1.0}}));
        double[][] y_pred_cce_1 = ((double[][])(new double[][]{new double[]{0.9, 0.1, 0.0}, new double[]{0.2, 0.7, 0.1}, new double[]{0.0, 0.1, 0.9}}));
        System.out.println(categorical_cross_entropy(((double[][])(y_true_cce_1)), ((double[][])(y_pred_cce_1)), 1e-15));
        double[] alpha_1 = ((double[])(new double[]{0.6, 0.2, 0.7}));
        System.out.println(categorical_focal_cross_entropy(((double[][])(y_true_cce_1)), ((double[][])(y_pred_cce_1)), ((double[])(alpha_1)), 2.0, 1e-15));
        double[] y_true_hinge_1 = ((double[])(new double[]{-1.0, 1.0, 1.0, -1.0, 1.0}));
        double[] y_pred_hinge_1 = ((double[])(new double[]{-4.0, -0.3, 0.7, 5.0, 10.0}));
        System.out.println(hinge_loss(((double[])(y_true_hinge_1)), ((double[])(y_pred_hinge_1))));
        double[] y_true_huber_1 = ((double[])(new double[]{0.9, 10.0, 2.0, 1.0, 5.2}));
        double[] y_pred_huber_1 = ((double[])(new double[]{0.8, 2.1, 2.9, 4.2, 5.2}));
        System.out.println(huber_loss(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1)), 1.0));
        System.out.println(mean_squared_error(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1))));
        System.out.println(mean_absolute_error(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1))));
        System.out.println(mean_squared_logarithmic_error(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1))));
        double[] y_true_mape_1 = ((double[])(new double[]{10.0, 20.0, 30.0, 40.0}));
        double[] y_pred_mape_1 = ((double[])(new double[]{12.0, 18.0, 33.0, 45.0}));
        System.out.println(mean_absolute_percentage_error(((double[])(y_true_mape_1)), ((double[])(y_pred_mape_1)), 1e-15));
        long[][] y_true_perp_1 = ((long[][])(new long[][]{new long[]{1, 4}, new long[]{2, 3}}));
        double[][][] y_pred_perp_1 = ((double[][][])(new double[][][]{new double[][]{new double[]{0.28, 0.19, 0.21, 0.15, 0.17}, new double[]{0.24, 0.19, 0.09, 0.18, 0.3}}, new double[][]{new double[]{0.03, 0.26, 0.21, 0.18, 0.32}, new double[]{0.28, 0.1, 0.33, 0.15, 0.14}}}));
        System.out.println(perplexity_loss(((long[][])(y_true_perp_1)), ((double[][][])(y_pred_perp_1)), 1e-07));
        double[] y_true_smooth_1 = ((double[])(new double[]{3.0, 5.0, 2.0, 7.0}));
        double[] y_pred_smooth_1 = ((double[])(new double[]{2.9, 4.8, 2.1, 7.2}));
        System.out.println(smooth_l1_loss(((double[])(y_true_smooth_1)), ((double[])(y_pred_smooth_1)), 1.0));
        double[] y_true_kl_1 = ((double[])(new double[]{0.2, 0.3, 0.5}));
        double[] y_pred_kl_1 = ((double[])(new double[]{0.3, 0.3, 0.4}));
        System.out.println(kullback_leibler_divergence(((double[])(y_true_kl_1)), ((double[])(y_pred_kl_1))));
    }
    public static void main(String[] args) {
        main();
    }
}
