public class Main {

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double maxf(double a, double b) {
        if ((double)(a) > (double)(b)) {
            return a;
        }
        return b;
    }

    static double minf(double a, double b) {
        if ((double)(a) < (double)(b)) {
            return a;
        }
        return b;
    }

    static double clip(double x, double lo, double hi) {
        return maxf((double)(lo), (double)(minf((double)(x), (double)(hi))));
    }

    static double to_float(long x) {
        return (double)(x) * (double)(1.0);
    }

    static double powf(double base, double exp) {
        double result = (double)(1.0);
        long i_1 = 0L;
        long n_1 = (long)(((Number)(exp)).intValue());
        while ((long)(i_1) < (long)(n_1)) {
            result = (double)((double)(result) * (double)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double ln(double x) {
        if ((double)(x) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double y2_1 = (double)((double)(y_1) * (double)(y_1));
        double term_1 = (double)(y_1);
        double sum_1 = (double)(0.0);
        long k_1 = 0L;
        while ((long)(k_1) < 10L) {
            double denom_1 = (double)(((Number)((long)(2L * (long)(k_1)) + 1L)).doubleValue());
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)(denom_1)));
            term_1 = (double)((double)(term_1) * (double)(y2_1));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double exp(double x) {
        double term_2 = (double)(1.0);
        double sum_3 = (double)(1.0);
        long n_3 = 1L;
        while ((long)(n_3) < 20L) {
            term_2 = (double)((double)((double)(term_2) * (double)(x)) / (double)(((Number)(n_3)).doubleValue()));
            sum_3 = (double)((double)(sum_3) + (double)(term_2));
            n_3 = (long)((long)(n_3) + 1L);
        }
        return sum_3;
    }

    static double mean(double[] v) {
        double total = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(v.length)) {
            total = (double)((double)(total) + (double)(v[(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(total) / (double)(((Number)(v.length)).doubleValue());
    }

    static double binary_cross_entropy(double[] y_true, double[] y_pred, double epsilon) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(y_true.length)) {
            double yt_1 = (double)(y_true[(int)((long)(i_5))]);
            double yp_1 = (double)(clip((double)(y_pred[(int)((long)(i_5))]), (double)(epsilon), (double)((double)(1.0) - (double)(epsilon))));
            double loss_1 = (double)(-((double)((double)(yt_1) * (double)(ln((double)(yp_1)))) + (double)((double)(((double)(1.0) - (double)(yt_1))) * (double)(ln((double)((double)(1.0) - (double)(yp_1)))))));
            losses_1 = ((double[])(appendDouble(losses_1, (double)(loss_1))));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return mean(((double[])(losses_1)));
    }

    static double binary_focal_cross_entropy(double[] y_true, double[] y_pred, double gamma, double alpha, double epsilon) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_3 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(y_true.length)) {
            double yt_3 = (double)(y_true[(int)((long)(i_7))]);
            double yp_3 = (double)(clip((double)(y_pred[(int)((long)(i_7))]), (double)(epsilon), (double)((double)(1.0) - (double)(epsilon))));
            double term1_1 = (double)((double)((double)((double)(alpha) * (double)(powf((double)((double)(1.0) - (double)(yp_3)), (double)(gamma)))) * (double)(yt_3)) * (double)(ln((double)(yp_3))));
            double term2_1 = (double)((double)((double)((double)(((double)(1.0) - (double)(alpha))) * (double)(powf((double)(yp_3), (double)(gamma)))) * (double)(((double)(1.0) - (double)(yt_3)))) * (double)(ln((double)((double)(1.0) - (double)(yp_3)))));
            losses_3 = ((double[])(appendDouble(losses_3, (double)(-((double)(term1_1) + (double)(term2_1))))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return mean(((double[])(losses_3)));
    }

    static double categorical_cross_entropy(double[][] y_true, double[][] y_pred, double epsilon) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same shape."));
        }
        long rows_1 = (long)(y_true.length);
        double total_2 = (double)(0.0);
        long i_9 = 0L;
        while ((long)(i_9) < (long)(rows_1)) {
            if ((long)(y_true[(int)((long)(i_9))].length) != (long)(y_pred[(int)((long)(i_9))].length)) {
                throw new RuntimeException(String.valueOf("Input arrays must have the same shape."));
            }
            double sum_true_1 = (double)(0.0);
            double sum_pred_1 = (double)(0.0);
            long j_1 = 0L;
            while ((long)(j_1) < (long)(y_true[(int)((long)(i_9))].length)) {
                double yt_5 = (double)(y_true[(int)((long)(i_9))][(int)((long)(j_1))]);
                double yp_6 = (double)(y_pred[(int)((long)(i_9))][(int)((long)(j_1))]);
                if (((double)(yt_5) != (double)(0.0) && (double)(yt_5) != (double)(1.0))) {
                    throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
                }
                sum_true_1 = (double)((double)(sum_true_1) + (double)(yt_5));
                sum_pred_1 = (double)((double)(sum_pred_1) + (double)(yp_6));
                j_1 = (long)((long)(j_1) + 1L);
            }
            if ((double)(sum_true_1) != (double)(1.0)) {
                throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
            }
            if ((double)(absf((double)((double)(sum_pred_1) - (double)(1.0)))) > (double)(epsilon)) {
                throw new RuntimeException(String.valueOf("Predicted probabilities must sum to approximately 1."));
            }
            j_1 = 0L;
            while ((long)(j_1) < (long)(y_true[(int)((long)(i_9))].length)) {
                double yp_7 = (double)(clip((double)(y_pred[(int)((long)(i_9))][(int)((long)(j_1))]), (double)(epsilon), (double)(1.0)));
                total_2 = (double)((double)(total_2) - (double)(((double)(y_true[(int)((long)(i_9))][(int)((long)(j_1))]) * (double)(ln((double)(yp_7))))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_9 = (long)((long)(i_9) + 1L);
        }
        return total_2;
    }

    static double categorical_focal_cross_entropy(double[][] y_true, double[][] y_pred, double[] alpha, double gamma, double epsilon) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Shape of y_true and y_pred must be the same."));
        }
        long rows_3 = (long)(y_true.length);
        long cols_1 = (long)(y_true[(int)(0L)].length);
        double[] a_1 = ((double[])(alpha));
        if ((long)(a_1.length) == 0L) {
            double[] tmp_1 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(cols_1)) {
                tmp_1 = ((double[])(appendDouble(tmp_1, (double)(1.0))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            a_1 = ((double[])(tmp_1));
        }
        if ((long)(a_1.length) != (long)(cols_1)) {
            throw new RuntimeException(String.valueOf("Length of alpha must match the number of classes."));
        }
        double total_4 = (double)(0.0);
        long i_11 = 0L;
        while ((long)(i_11) < (long)(rows_3)) {
            if ((long)(y_true[(int)((long)(i_11))].length) != (long)(cols_1) || (long)(y_pred[(int)((long)(i_11))].length) != (long)(cols_1)) {
                throw new RuntimeException(String.valueOf("Shape of y_true and y_pred must be the same."));
            }
            double sum_true_3 = (double)(0.0);
            double sum_pred_3 = (double)(0.0);
            long j_5 = 0L;
            while ((long)(j_5) < (long)(cols_1)) {
                double yt_7 = (double)(y_true[(int)((long)(i_11))][(int)((long)(j_5))]);
                double yp_10 = (double)(y_pred[(int)((long)(i_11))][(int)((long)(j_5))]);
                if (((double)(yt_7) != (double)(0.0) && (double)(yt_7) != (double)(1.0))) {
                    throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
                }
                sum_true_3 = (double)((double)(sum_true_3) + (double)(yt_7));
                sum_pred_3 = (double)((double)(sum_pred_3) + (double)(yp_10));
                j_5 = (long)((long)(j_5) + 1L);
            }
            if ((double)(sum_true_3) != (double)(1.0)) {
                throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
            }
            if ((double)(absf((double)((double)(sum_pred_3) - (double)(1.0)))) > (double)(epsilon)) {
                throw new RuntimeException(String.valueOf("Predicted probabilities must sum to approximately 1."));
            }
            double row_loss_1 = (double)(0.0);
            j_5 = 0L;
            while ((long)(j_5) < (long)(cols_1)) {
                double yp_11 = (double)(clip((double)(y_pred[(int)((long)(i_11))][(int)((long)(j_5))]), (double)(epsilon), (double)(1.0)));
                row_loss_1 = (double)((double)(row_loss_1) + (double)((double)((double)((double)(a_1[(int)((long)(j_5))]) * (double)(powf((double)((double)(1.0) - (double)(yp_11)), (double)(gamma)))) * (double)(y_true[(int)((long)(i_11))][(int)((long)(j_5))])) * (double)(ln((double)(yp_11)))));
                j_5 = (long)((long)(j_5) + 1L);
            }
            total_4 = (double)((double)(total_4) - (double)(row_loss_1));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return (double)(total_4) / (double)(((Number)(rows_3)).doubleValue());
    }

    static double hinge_loss(double[] y_true, double[] y_pred) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Length of predicted and actual array must be same."));
        }
        double[] losses_5 = ((double[])(new double[]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(y_true.length)) {
            double yt_9 = (double)(y_true[(int)((long)(i_13))]);
            if (((double)(yt_9) != (double)((-1.0)) && (double)(yt_9) != (double)(1.0))) {
                throw new RuntimeException(String.valueOf("y_true can have values -1 or 1 only."));
            }
            double pred_1 = (double)(y_pred[(int)((long)(i_13))]);
            double l_1 = (double)(maxf((double)(0.0), (double)((double)(1.0) - (double)((double)(yt_9) * (double)(pred_1)))));
            losses_5 = ((double[])(appendDouble(losses_5, (double)(l_1))));
            i_13 = (long)((long)(i_13) + 1L);
        }
        return mean(((double[])(losses_5)));
    }

    static double huber_loss(double[] y_true, double[] y_pred, double delta) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_6 = (double)(0.0);
        long i_15 = 0L;
        while ((long)(i_15) < (long)(y_true.length)) {
            double diff_1 = (double)((double)(y_true[(int)((long)(i_15))]) - (double)(y_pred[(int)((long)(i_15))]));
            double adiff_1 = (double)(absf((double)(diff_1)));
            if ((double)(adiff_1) <= (double)(delta)) {
                total_6 = (double)((double)(total_6) + (double)((double)((double)(0.5) * (double)(diff_1)) * (double)(diff_1)));
            } else {
                total_6 = (double)((double)(total_6) + (double)((double)(delta) * (double)(((double)(adiff_1) - (double)((double)(0.5) * (double)(delta))))));
            }
            i_15 = (long)((long)(i_15) + 1L);
        }
        return (double)(total_6) / (double)(((Number)(y_true.length)).doubleValue());
    }

    static double mean_squared_error(double[] y_true, double[] y_pred) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_7 = ((double[])(new double[]{}));
        long i_17 = 0L;
        while ((long)(i_17) < (long)(y_true.length)) {
            double diff_3 = (double)((double)(y_true[(int)((long)(i_17))]) - (double)(y_pred[(int)((long)(i_17))]));
            losses_7 = ((double[])(appendDouble(losses_7, (double)((double)(diff_3) * (double)(diff_3)))));
            i_17 = (long)((long)(i_17) + 1L);
        }
        return mean(((double[])(losses_7)));
    }

    static double mean_absolute_error(double[] y_true, double[] y_pred) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_8 = (double)(0.0);
        long i_19 = 0L;
        while ((long)(i_19) < (long)(y_true.length)) {
            total_8 = (double)((double)(total_8) + (double)(absf((double)((double)(y_true[(int)((long)(i_19))]) - (double)(y_pred[(int)((long)(i_19))])))));
            i_19 = (long)((long)(i_19) + 1L);
        }
        return (double)(total_8) / (double)(((Number)(y_true.length)).doubleValue());
    }

    static double mean_squared_logarithmic_error(double[] y_true, double[] y_pred) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_10 = (double)(0.0);
        long i_21 = 0L;
        while ((long)(i_21) < (long)(y_true.length)) {
            double a_3 = (double)(ln((double)((double)(1.0) + (double)(y_true[(int)((long)(i_21))]))));
            double b_1 = (double)(ln((double)((double)(1.0) + (double)(y_pred[(int)((long)(i_21))]))));
            double diff_5 = (double)((double)(a_3) - (double)(b_1));
            total_10 = (double)((double)(total_10) + (double)((double)(diff_5) * (double)(diff_5)));
            i_21 = (long)((long)(i_21) + 1L);
        }
        return (double)(total_10) / (double)(((Number)(y_true.length)).doubleValue());
    }

    static double mean_absolute_percentage_error(double[] y_true, double[] y_pred, double epsilon) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("The length of the two arrays should be the same."));
        }
        double total_12 = (double)(0.0);
        long i_23 = 0L;
        while ((long)(i_23) < (long)(y_true.length)) {
            double yt_11 = (double)(y_true[(int)((long)(i_23))]);
            if ((double)(yt_11) == (double)(0.0)) {
                yt_11 = (double)(epsilon);
            }
            total_12 = (double)((double)(total_12) + (double)(absf((double)((double)(((double)(yt_11) - (double)(y_pred[(int)((long)(i_23))]))) / (double)(yt_11)))));
            i_23 = (long)((long)(i_23) + 1L);
        }
        return (double)(total_12) / (double)(((Number)(y_true.length)).doubleValue());
    }

    static double perplexity_loss(long[][] y_true, double[][][] y_pred, double epsilon) {
        long batch = (long)(y_true.length);
        if ((long)(batch) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Batch size of y_true and y_pred must be equal."));
        }
        long sentence_len_1 = (long)(y_true[(int)(0L)].length);
        if ((long)(sentence_len_1) != (long)(y_pred[(int)(0L)].length)) {
            throw new RuntimeException(String.valueOf("Sentence length of y_true and y_pred must be equal."));
        }
        long vocab_size_1 = (long)(y_pred[(int)(0L)][(int)(0L)].length);
        long b_3 = 0L;
        double total_perp_1 = (double)(0.0);
        while ((long)(b_3) < (long)(batch)) {
            if ((long)(y_true[(int)((long)(b_3))].length) != (long)(sentence_len_1) || (long)(y_pred[(int)((long)(b_3))].length) != (long)(sentence_len_1)) {
                throw new RuntimeException(String.valueOf("Sentence length of y_true and y_pred must be equal."));
            }
            double sum_log_1 = (double)(0.0);
            long j_7 = 0L;
            while ((long)(j_7) < (long)(sentence_len_1)) {
                long label_1 = (long)(y_true[(int)((long)(b_3))][(int)((long)(j_7))]);
                if ((long)(label_1) >= (long)(vocab_size_1)) {
                    throw new RuntimeException(String.valueOf("Label value must not be greater than vocabulary size."));
                }
                double prob_1 = (double)(clip((double)(y_pred[(int)((long)(b_3))][(int)((long)(j_7))][(int)((long)(label_1))]), (double)(epsilon), (double)(1.0)));
                sum_log_1 = (double)((double)(sum_log_1) + (double)(ln((double)(prob_1))));
                j_7 = (long)((long)(j_7) + 1L);
            }
            double mean_log_1 = (double)((double)(sum_log_1) / (double)(((Number)(sentence_len_1)).doubleValue()));
            double perp_1 = (double)(exp((double)(-mean_log_1)));
            total_perp_1 = (double)((double)(total_perp_1) + (double)(perp_1));
            b_3 = (long)((long)(b_3) + 1L);
        }
        return (double)(total_perp_1) / (double)(((Number)(batch)).doubleValue());
    }

    static double smooth_l1_loss(double[] y_true, double[] y_pred, double beta) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("The length of the two arrays should be the same."));
        }
        double total_14 = (double)(0.0);
        long i_25 = 0L;
        while ((long)(i_25) < (long)(y_true.length)) {
            double diff_7 = (double)(absf((double)((double)(y_true[(int)((long)(i_25))]) - (double)(y_pred[(int)((long)(i_25))]))));
            if ((double)(diff_7) < (double)(beta)) {
                total_14 = (double)((double)(total_14) + (double)((double)((double)((double)(0.5) * (double)(diff_7)) * (double)(diff_7)) / (double)(beta)));
            } else {
                total_14 = (double)((double)((double)(total_14) + (double)(diff_7)) - (double)((double)(0.5) * (double)(beta)));
            }
            i_25 = (long)((long)(i_25) + 1L);
        }
        return (double)(total_14) / (double)(((Number)(y_true.length)).doubleValue());
    }

    static double kullback_leibler_divergence(double[] y_true, double[] y_pred) {
        if ((long)(y_true.length) != (long)(y_pred.length)) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_16 = (double)(0.0);
        long i_27 = 0L;
        while ((long)(i_27) < (long)(y_true.length)) {
            total_16 = (double)((double)(total_16) + (double)((double)(y_true[(int)((long)(i_27))]) * (double)(ln((double)((double)(y_true[(int)((long)(i_27))]) / (double)(y_pred[(int)((long)(i_27))]))))));
            i_27 = (long)((long)(i_27) + 1L);
        }
        return total_16;
    }

    static void main() {
        double[] y_true_bc = ((double[])(new double[]{0.0, 1.0, 1.0, 0.0, 1.0}));
        double[] y_pred_bc_1 = ((double[])(new double[]{0.2, 0.7, 0.9, 0.3, 0.8}));
        System.out.println(binary_cross_entropy(((double[])(y_true_bc)), ((double[])(y_pred_bc_1)), (double)(1e-15)));
        System.out.println(binary_focal_cross_entropy(((double[])(y_true_bc)), ((double[])(y_pred_bc_1)), (double)(2.0), (double)(0.25), (double)(1e-15)));
        double[][] y_true_cce_1 = ((double[][])(new double[][]{new double[]{1.0, 0.0, 0.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 1.0}}));
        double[][] y_pred_cce_1 = ((double[][])(new double[][]{new double[]{0.9, 0.1, 0.0}, new double[]{0.2, 0.7, 0.1}, new double[]{0.0, 0.1, 0.9}}));
        System.out.println(categorical_cross_entropy(((double[][])(y_true_cce_1)), ((double[][])(y_pred_cce_1)), (double)(1e-15)));
        double[] alpha_1 = ((double[])(new double[]{0.6, 0.2, 0.7}));
        System.out.println(categorical_focal_cross_entropy(((double[][])(y_true_cce_1)), ((double[][])(y_pred_cce_1)), ((double[])(alpha_1)), (double)(2.0), (double)(1e-15)));
        double[] y_true_hinge_1 = ((double[])(new double[]{-1.0, 1.0, 1.0, -1.0, 1.0}));
        double[] y_pred_hinge_1 = ((double[])(new double[]{-4.0, -0.3, 0.7, 5.0, 10.0}));
        System.out.println(hinge_loss(((double[])(y_true_hinge_1)), ((double[])(y_pred_hinge_1))));
        double[] y_true_huber_1 = ((double[])(new double[]{0.9, 10.0, 2.0, 1.0, 5.2}));
        double[] y_pred_huber_1 = ((double[])(new double[]{0.8, 2.1, 2.9, 4.2, 5.2}));
        System.out.println(huber_loss(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1)), (double)(1.0)));
        System.out.println(mean_squared_error(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1))));
        System.out.println(mean_absolute_error(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1))));
        System.out.println(mean_squared_logarithmic_error(((double[])(y_true_huber_1)), ((double[])(y_pred_huber_1))));
        double[] y_true_mape_1 = ((double[])(new double[]{10.0, 20.0, 30.0, 40.0}));
        double[] y_pred_mape_1 = ((double[])(new double[]{12.0, 18.0, 33.0, 45.0}));
        System.out.println(mean_absolute_percentage_error(((double[])(y_true_mape_1)), ((double[])(y_pred_mape_1)), (double)(1e-15)));
        long[][] y_true_perp_1 = ((long[][])(new long[][]{new long[]{1, 4}, new long[]{2, 3}}));
        double[][][] y_pred_perp_1 = ((double[][][])(new double[][][]{new double[][]{new double[]{0.28, 0.19, 0.21, 0.15, 0.17}, new double[]{0.24, 0.19, 0.09, 0.18, 0.3}}, new double[][]{new double[]{0.03, 0.26, 0.21, 0.18, 0.32}, new double[]{0.28, 0.1, 0.33, 0.15, 0.14}}}));
        System.out.println(perplexity_loss(((long[][])(y_true_perp_1)), ((double[][][])(y_pred_perp_1)), (double)(1e-07)));
        double[] y_true_smooth_1 = ((double[])(new double[]{3.0, 5.0, 2.0, 7.0}));
        double[] y_pred_smooth_1 = ((double[])(new double[]{2.9, 4.8, 2.1, 7.2}));
        System.out.println(smooth_l1_loss(((double[])(y_true_smooth_1)), ((double[])(y_pred_smooth_1)), (double)(1.0)));
        double[] y_true_kl_1 = ((double[])(new double[]{0.2, 0.3, 0.5}));
        double[] y_pred_kl_1 = ((double[])(new double[]{0.3, 0.3, 0.4}));
        System.out.println(kullback_leibler_divergence(((double[])(y_true_kl_1)), ((double[])(y_pred_kl_1))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
