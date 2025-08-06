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

    static double to_float(int x) {
        return x * 1.0;
    }

    static double powf(double base, double exp) {
        double result = 1.0;
        int i = 0;
        int n = ((Number)(exp)).intValue();
        while (i < n) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double ln(double x) {
        if (x <= 0.0) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y = (x - 1.0) / (x + 1.0);
        double y2 = y * y;
        double term = y;
        double sum = 0.0;
        int k = 0;
        while (k < 10) {
            double denom = to_float(2 * k + 1);
            sum = sum + term / denom;
            term = term * y2;
            k = k + 1;
        }
        return 2.0 * sum;
    }

    static double exp(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int n_1 = 1;
        while (n_1 < 20) {
            term_1 = term_1 * x / to_float(n_1);
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double mean(double[] v) {
        double total = 0.0;
        int i_1 = 0;
        while (i_1 < v.length) {
            total = total + v[i_1];
            i_1 = i_1 + 1;
        }
        return total / to_float(v.length);
    }

    static double binary_cross_entropy(double[] y_true, double[] y_pred, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < y_true.length) {
            double yt = y_true[i_2];
            double yp = clip(y_pred[i_2], epsilon, 1.0 - epsilon);
            double loss = -(yt * ln(yp) + (1.0 - yt) * ln(1.0 - yp));
            losses = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses), java.util.stream.DoubleStream.of(loss)).toArray()));
            i_2 = i_2 + 1;
        }
        return mean(((double[])(losses)));
    }

    static double binary_focal_cross_entropy(double[] y_true, double[] y_pred, double gamma, double alpha, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_1 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < y_true.length) {
            double yt_1 = y_true[i_3];
            double yp_1 = clip(y_pred[i_3], epsilon, 1.0 - epsilon);
            double term1 = alpha * powf(1.0 - yp_1, gamma) * yt_1 * ln(yp_1);
            double term2 = (1.0 - alpha) * powf(yp_1, gamma) * (1.0 - yt_1) * ln(1.0 - yp_1);
            losses_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_1), java.util.stream.DoubleStream.of(-(term1 + term2))).toArray()));
            i_3 = i_3 + 1;
        }
        return mean(((double[])(losses_1)));
    }

    static double categorical_cross_entropy(double[][] y_true, double[][] y_pred, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same shape."));
        }
        int rows = y_true.length;
        double total_1 = 0.0;
        int i_4 = 0;
        while (i_4 < rows) {
            if (y_true[i_4].length != y_pred[i_4].length) {
                throw new RuntimeException(String.valueOf("Input arrays must have the same shape."));
            }
            double sum_true = 0.0;
            double sum_pred = 0.0;
            int j = 0;
            while (j < y_true[i_4].length) {
                double yt_2 = y_true[i_4][j];
                double yp_2 = y_pred[i_4][j];
                if ((yt_2 != 0.0 && yt_2 != 1.0)) {
                    throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
                }
                sum_true = sum_true + yt_2;
                sum_pred = sum_pred + yp_2;
                j = j + 1;
            }
            if (sum_true != 1.0) {
                throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
            }
            if (absf(sum_pred - 1.0) > epsilon) {
                throw new RuntimeException(String.valueOf("Predicted probabilities must sum to approximately 1."));
            }
            j = 0;
            while (j < y_true[i_4].length) {
                double yp_3 = clip(y_pred[i_4][j], epsilon, 1.0);
                total_1 = total_1 - (y_true[i_4][j] * ln(yp_3));
                j = j + 1;
            }
            i_4 = i_4 + 1;
        }
        return total_1;
    }

    static double categorical_focal_cross_entropy(double[][] y_true, double[][] y_pred, double[] alpha, double gamma, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Shape of y_true and y_pred must be the same."));
        }
        int rows_1 = y_true.length;
        int cols = y_true[0].length;
        double[] a = ((double[])(alpha));
        if (a.length == 0) {
            double[] tmp = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < cols) {
                tmp = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(tmp), java.util.stream.DoubleStream.of(1.0)).toArray()));
                j_1 = j_1 + 1;
            }
            a = ((double[])(tmp));
        }
        if (a.length != cols) {
            throw new RuntimeException(String.valueOf("Length of alpha must match the number of classes."));
        }
        double total_2 = 0.0;
        int i_5 = 0;
        while (i_5 < rows_1) {
            if (y_true[i_5].length != cols || y_pred[i_5].length != cols) {
                throw new RuntimeException(String.valueOf("Shape of y_true and y_pred must be the same."));
            }
            double sum_true_1 = 0.0;
            double sum_pred_1 = 0.0;
            int j_2 = 0;
            while (j_2 < cols) {
                double yt_3 = y_true[i_5][j_2];
                double yp_4 = y_pred[i_5][j_2];
                if ((yt_3 != 0.0 && yt_3 != 1.0)) {
                    throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
                }
                sum_true_1 = sum_true_1 + yt_3;
                sum_pred_1 = sum_pred_1 + yp_4;
                j_2 = j_2 + 1;
            }
            if (sum_true_1 != 1.0) {
                throw new RuntimeException(String.valueOf("y_true must be one-hot encoded."));
            }
            if (absf(sum_pred_1 - 1.0) > epsilon) {
                throw new RuntimeException(String.valueOf("Predicted probabilities must sum to approximately 1."));
            }
            double row_loss = 0.0;
            j_2 = 0;
            while (j_2 < cols) {
                double yp_5 = clip(y_pred[i_5][j_2], epsilon, 1.0);
                row_loss = row_loss + a[j_2] * powf(1.0 - yp_5, gamma) * y_true[i_5][j_2] * ln(yp_5);
                j_2 = j_2 + 1;
            }
            total_2 = total_2 - row_loss;
            i_5 = i_5 + 1;
        }
        return total_2 / to_float(rows_1);
    }

    static double hinge_loss(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Length of predicted and actual array must be same."));
        }
        double[] losses_2 = ((double[])(new double[]{}));
        int i_6 = 0;
        while (i_6 < y_true.length) {
            double yt_4 = y_true[i_6];
            if ((yt_4 != (-1.0) && yt_4 != 1.0)) {
                throw new RuntimeException(String.valueOf("y_true can have values -1 or 1 only."));
            }
            double pred = y_pred[i_6];
            double l = maxf(0.0, 1.0 - yt_4 * pred);
            losses_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_2), java.util.stream.DoubleStream.of(l)).toArray()));
            i_6 = i_6 + 1;
        }
        return mean(((double[])(losses_2)));
    }

    static double huber_loss(double[] y_true, double[] y_pred, double delta) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_3 = 0.0;
        int i_7 = 0;
        while (i_7 < y_true.length) {
            double diff = y_true[i_7] - y_pred[i_7];
            double adiff = absf(diff);
            if (adiff <= delta) {
                total_3 = total_3 + 0.5 * diff * diff;
            } else {
                total_3 = total_3 + delta * (adiff - 0.5 * delta);
            }
            i_7 = i_7 + 1;
        }
        return total_3 / to_float(y_true.length);
    }

    static double mean_squared_error(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double[] losses_3 = ((double[])(new double[]{}));
        int i_8 = 0;
        while (i_8 < y_true.length) {
            double diff_1 = y_true[i_8] - y_pred[i_8];
            losses_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(losses_3), java.util.stream.DoubleStream.of(diff_1 * diff_1)).toArray()));
            i_8 = i_8 + 1;
        }
        return mean(((double[])(losses_3)));
    }

    static double mean_absolute_error(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_4 = 0.0;
        int i_9 = 0;
        while (i_9 < y_true.length) {
            total_4 = total_4 + absf(y_true[i_9] - y_pred[i_9]);
            i_9 = i_9 + 1;
        }
        return total_4 / to_float(y_true.length);
    }

    static double mean_squared_logarithmic_error(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_5 = 0.0;
        int i_10 = 0;
        while (i_10 < y_true.length) {
            double a_1 = ln(1.0 + y_true[i_10]);
            double b = ln(1.0 + y_pred[i_10]);
            double diff_2 = a_1 - b;
            total_5 = total_5 + diff_2 * diff_2;
            i_10 = i_10 + 1;
        }
        return total_5 / to_float(y_true.length);
    }

    static double mean_absolute_percentage_error(double[] y_true, double[] y_pred, double epsilon) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("The length of the two arrays should be the same."));
        }
        double total_6 = 0.0;
        int i_11 = 0;
        while (i_11 < y_true.length) {
            double yt_5 = y_true[i_11];
            if (yt_5 == 0.0) {
                yt_5 = epsilon;
            }
            total_6 = total_6 + absf((yt_5 - y_pred[i_11]) / yt_5);
            i_11 = i_11 + 1;
        }
        return total_6 / to_float(y_true.length);
    }

    static double perplexity_loss(int[][] y_true, double[][][] y_pred, double epsilon) {
        int batch = y_true.length;
        if (batch != y_pred.length) {
            throw new RuntimeException(String.valueOf("Batch size of y_true and y_pred must be equal."));
        }
        int sentence_len = y_true[0].length;
        if (sentence_len != y_pred[0].length) {
            throw new RuntimeException(String.valueOf("Sentence length of y_true and y_pred must be equal."));
        }
        int vocab_size = y_pred[0][0].length;
        int b_1 = 0;
        double total_perp = 0.0;
        while (b_1 < batch) {
            if (y_true[b_1].length != sentence_len || y_pred[b_1].length != sentence_len) {
                throw new RuntimeException(String.valueOf("Sentence length of y_true and y_pred must be equal."));
            }
            double sum_log = 0.0;
            int j_3 = 0;
            while (j_3 < sentence_len) {
                int label = y_true[b_1][j_3];
                if (label >= vocab_size) {
                    throw new RuntimeException(String.valueOf("Label value must not be greater than vocabulary size."));
                }
                double prob = clip(y_pred[b_1][j_3][label], epsilon, 1.0);
                sum_log = sum_log + ln(prob);
                j_3 = j_3 + 1;
            }
            double mean_log = sum_log / to_float(sentence_len);
            double perp = exp(-mean_log);
            total_perp = total_perp + perp;
            b_1 = b_1 + 1;
        }
        return total_perp / to_float(batch);
    }

    static double smooth_l1_loss(double[] y_true, double[] y_pred, double beta) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("The length of the two arrays should be the same."));
        }
        double total_7 = 0.0;
        int i_12 = 0;
        while (i_12 < y_true.length) {
            double diff_3 = absf(y_true[i_12] - y_pred[i_12]);
            if (diff_3 < beta) {
                total_7 = total_7 + 0.5 * diff_3 * diff_3 / beta;
            } else {
                total_7 = total_7 + diff_3 - 0.5 * beta;
            }
            i_12 = i_12 + 1;
        }
        return total_7 / to_float(y_true.length);
    }

    static double kullback_leibler_divergence(double[] y_true, double[] y_pred) {
        if (y_true.length != y_pred.length) {
            throw new RuntimeException(String.valueOf("Input arrays must have the same length."));
        }
        double total_8 = 0.0;
        int i_13 = 0;
        while (i_13 < y_true.length) {
            total_8 = total_8 + y_true[i_13] * ln(y_true[i_13] / y_pred[i_13]);
            i_13 = i_13 + 1;
        }
        return total_8;
    }

    static void main() {
        double[] y_true_bc = ((double[])(new double[]{0.0, 1.0, 1.0, 0.0, 1.0}));
        double[] y_pred_bc = ((double[])(new double[]{0.2, 0.7, 0.9, 0.3, 0.8}));
        System.out.println(binary_cross_entropy(((double[])(y_true_bc)), ((double[])(y_pred_bc)), 1e-15));
        System.out.println(binary_focal_cross_entropy(((double[])(y_true_bc)), ((double[])(y_pred_bc)), 2.0, 0.25, 1e-15));
        double[][] y_true_cce = ((double[][])(new double[][]{new double[]{1.0, 0.0, 0.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 1.0}}));
        double[][] y_pred_cce = ((double[][])(new double[][]{new double[]{0.9, 0.1, 0.0}, new double[]{0.2, 0.7, 0.1}, new double[]{0.0, 0.1, 0.9}}));
        System.out.println(categorical_cross_entropy(((double[][])(y_true_cce)), ((double[][])(y_pred_cce)), 1e-15));
        double[] alpha = ((double[])(new double[]{0.6, 0.2, 0.7}));
        System.out.println(categorical_focal_cross_entropy(((double[][])(y_true_cce)), ((double[][])(y_pred_cce)), ((double[])(alpha)), 2.0, 1e-15));
        double[] y_true_hinge = ((double[])(new double[]{-1.0, 1.0, 1.0, -1.0, 1.0}));
        double[] y_pred_hinge = ((double[])(new double[]{-4.0, -0.3, 0.7, 5.0, 10.0}));
        System.out.println(hinge_loss(((double[])(y_true_hinge)), ((double[])(y_pred_hinge))));
        double[] y_true_huber = ((double[])(new double[]{0.9, 10.0, 2.0, 1.0, 5.2}));
        double[] y_pred_huber = ((double[])(new double[]{0.8, 2.1, 2.9, 4.2, 5.2}));
        System.out.println(huber_loss(((double[])(y_true_huber)), ((double[])(y_pred_huber)), 1.0));
        System.out.println(mean_squared_error(((double[])(y_true_huber)), ((double[])(y_pred_huber))));
        System.out.println(mean_absolute_error(((double[])(y_true_huber)), ((double[])(y_pred_huber))));
        System.out.println(mean_squared_logarithmic_error(((double[])(y_true_huber)), ((double[])(y_pred_huber))));
        double[] y_true_mape = ((double[])(new double[]{10.0, 20.0, 30.0, 40.0}));
        double[] y_pred_mape = ((double[])(new double[]{12.0, 18.0, 33.0, 45.0}));
        System.out.println(mean_absolute_percentage_error(((double[])(y_true_mape)), ((double[])(y_pred_mape)), 1e-15));
        int[][] y_true_perp = ((int[][])(new int[][]{new int[]{1, 4}, new int[]{2, 3}}));
        double[][][] y_pred_perp = ((double[][][])(new double[][][]{new double[][]{new double[]{0.28, 0.19, 0.21, 0.15, 0.17}, new double[]{0.24, 0.19, 0.09, 0.18, 0.3}}, new double[][]{new double[]{0.03, 0.26, 0.21, 0.18, 0.32}, new double[]{0.28, 0.1, 0.33, 0.15, 0.14}}}));
        System.out.println(perplexity_loss(((int[][])(y_true_perp)), ((double[][][])(y_pred_perp)), 1e-07));
        double[] y_true_smooth = ((double[])(new double[]{3.0, 5.0, 2.0, 7.0}));
        double[] y_pred_smooth = ((double[])(new double[]{2.9, 4.8, 2.1, 7.2}));
        System.out.println(smooth_l1_loss(((double[])(y_true_smooth)), ((double[])(y_pred_smooth)), 1.0));
        double[] y_true_kl = ((double[])(new double[]{0.2, 0.3, 0.5}));
        double[] y_pred_kl = ((double[])(new double[]{0.3, 0.3, 0.4}));
        System.out.println(kullback_leibler_divergence(((double[])(y_true_kl)), ((double[])(y_pred_kl))));
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
}
