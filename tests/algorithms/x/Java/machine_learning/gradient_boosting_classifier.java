public class Main {
    static class Stump {
        long feature;
        double threshold;
        double left;
        double right;
        Stump(long feature, double threshold, double left, double right) {
            this.feature = feature;
            this.threshold = threshold;
            this.left = left;
            this.right = right;
        }
        Stump() {}
        @Override public String toString() {
            return String.format("{'feature': %s, 'threshold': %s, 'left': %s, 'right': %s}", String.valueOf(feature), String.valueOf(threshold), String.valueOf(left), String.valueOf(right));
        }
    }

    static double[][] features;
    static double[] target;
    static Stump[] models_1;
    static double[] predictions;
    static double acc;

    static double exp_approx(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        long i_1 = 1;
        while (i_1 < 10) {
            term = term * x / (((Number)(i_1)).doubleValue());
            sum_1 = sum_1 + term;
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static double signf(double x) {
        if (x >= 0.0) {
            return 1.0;
        }
        return -1.0;
    }

    static double[] gradient(double[] target, double[] preds) {
        long n = target.length;
        double[] residuals_1 = ((double[])(new double[]{}));
        long i_3 = 0;
        while (i_3 < n) {
            double t_1 = target[(int)(i_3)];
            double y_1 = preds[(int)(i_3)];
            double exp_val_1 = exp_approx(t_1 * y_1);
            double res_1 = -t_1 / (1.0 + exp_val_1);
            residuals_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(residuals_1), java.util.stream.DoubleStream.of(res_1)).toArray()));
            i_3 = i_3 + 1;
        }
        return residuals_1;
    }

    static double[] predict_raw(Stump[] models, double[][] features, double learning_rate) {
        long n_1 = features.length;
        double[] preds_1 = ((double[])(new double[]{}));
        long i_5 = 0;
        while (i_5 < n_1) {
            preds_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(preds_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_5 = i_5 + 1;
        }
        long m_1 = 0;
        while (m_1 < models.length) {
            Stump stump_1 = models[(int)(m_1)];
            i_5 = 0;
            while (i_5 < n_1) {
                double value_1 = features[(int)(i_5)][(int)(stump_1.feature)];
                if (value_1 <= stump_1.threshold) {
preds_1[(int)(i_5)] = preds_1[(int)(i_5)] + learning_rate * stump_1.left;
                } else {
preds_1[(int)(i_5)] = preds_1[(int)(i_5)] + learning_rate * stump_1.right;
                }
                i_5 = i_5 + 1;
            }
            m_1 = m_1 + 1;
        }
        return preds_1;
    }

    static double[] predict(Stump[] models, double[][] features, double learning_rate) {
        double[] raw = ((double[])(predict_raw(((Stump[])(models)), ((double[][])(features)), learning_rate)));
        double[] result_1 = ((double[])(new double[]{}));
        long i_7 = 0;
        while (i_7 < raw.length) {
            result_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_1), java.util.stream.DoubleStream.of(signf(raw[(int)(i_7)]))).toArray()));
            i_7 = i_7 + 1;
        }
        return result_1;
    }

    static Stump train_stump(double[][] features, double[] residuals) {
        long n_samples = features.length;
        long n_features_1 = features[(int)(0)].length;
        long best_feature_1 = 0;
        double best_threshold_1 = 0.0;
        double best_error_1 = 1000000000.0;
        double best_left_1 = 0.0;
        double best_right_1 = 0.0;
        long j_1 = 0;
        while (j_1 < n_features_1) {
            long t_index_1 = 0;
            while (t_index_1 < n_samples) {
                double t_3 = features[(int)(t_index_1)][(int)(j_1)];
                double sum_left_1 = 0.0;
                long count_left_1 = 0;
                double sum_right_1 = 0.0;
                long count_right_1 = 0;
                long i_9 = 0;
                while (i_9 < n_samples) {
                    if (features[(int)(i_9)][(int)(j_1)] <= t_3) {
                        sum_left_1 = sum_left_1 + residuals[(int)(i_9)];
                        count_left_1 = count_left_1 + 1;
                    } else {
                        sum_right_1 = sum_right_1 + residuals[(int)(i_9)];
                        count_right_1 = count_right_1 + 1;
                    }
                    i_9 = i_9 + 1;
                }
                double left_val_1 = 0.0;
                if (count_left_1 != 0) {
                    left_val_1 = sum_left_1 / (((Number)(count_left_1)).doubleValue());
                }
                double right_val_1 = 0.0;
                if (count_right_1 != 0) {
                    right_val_1 = sum_right_1 / (((Number)(count_right_1)).doubleValue());
                }
                double error_1 = 0.0;
                i_9 = 0;
                while (i_9 < n_samples) {
                    double pred_1 = features[(int)(i_9)][(int)(j_1)] <= t_3 ? left_val_1 : right_val_1;
                    double diff_1 = residuals[(int)(i_9)] - pred_1;
                    error_1 = error_1 + diff_1 * diff_1;
                    i_9 = i_9 + 1;
                }
                if (error_1 < best_error_1) {
                    best_error_1 = error_1;
                    best_feature_1 = j_1;
                    best_threshold_1 = t_3;
                    best_left_1 = left_val_1;
                    best_right_1 = right_val_1;
                }
                t_index_1 = t_index_1 + 1;
            }
            j_1 = j_1 + 1;
        }
        return new Stump(best_feature_1, best_threshold_1, best_left_1, best_right_1);
    }

    static Stump[] fit(long n_estimators, double learning_rate, double[][] features, double[] target) {
        Stump[] models = ((Stump[])(new Stump[]{}));
        long m_3 = 0;
        while (m_3 < n_estimators) {
            double[] preds_3 = ((double[])(predict_raw(((Stump[])(models)), ((double[][])(features)), learning_rate)));
            double[] grad_1 = ((double[])(gradient(((double[])(target)), ((double[])(preds_3)))));
            double[] residuals_3 = ((double[])(new double[]{}));
            long i_11 = 0;
            while (i_11 < grad_1.length) {
                residuals_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(residuals_3), java.util.stream.DoubleStream.of(-grad_1[(int)(i_11)])).toArray()));
                i_11 = i_11 + 1;
            }
            Stump stump_3 = train_stump(((double[][])(features)), ((double[])(residuals_3)));
            models = ((Stump[])(java.util.stream.Stream.concat(java.util.Arrays.stream(models), java.util.stream.Stream.of(stump_3)).toArray(Stump[]::new)));
            m_3 = m_3 + 1;
        }
        return models;
    }

    static double accuracy(double[] preds, double[] target) {
        long n_2 = target.length;
        long correct_1 = 0;
        long i_13 = 0;
        while (i_13 < n_2) {
            if (preds[(int)(i_13)] == target[(int)(i_13)]) {
                correct_1 = correct_1 + 1;
            }
            i_13 = i_13 + 1;
        }
        return (((Number)(correct_1)).doubleValue()) / (((Number)(n_2)).doubleValue());
    }
    public static void main(String[] args) {
        features = ((double[][])(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{3.0}, new double[]{4.0}}));
        target = ((double[])(new double[]{-1.0, -1.0, 1.0, 1.0}));
        models_1 = ((Stump[])(fit(5, 0.5, ((double[][])(features)), ((double[])(target)))));
        predictions = ((double[])(predict(((Stump[])(models_1)), ((double[][])(features)), 0.5)));
        acc = accuracy(((double[])(predictions)), ((double[])(target)));
        System.out.println("Accuracy: " + _p(acc));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
