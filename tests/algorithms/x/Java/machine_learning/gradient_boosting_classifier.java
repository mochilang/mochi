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

    static double[][] features = ((double[][])(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{3.0}, new double[]{4.0}}));
    static double[] target;
    static Stump[] models_1;
    static double[] predictions;
    static double acc;

    static double exp_approx(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        long i_1 = 1L;
        while ((long)(i_1) < 10L) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)((((Number)(i_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum_1;
    }

    static double signf(double x) {
        if ((double)(x) >= (double)(0.0)) {
            return 1.0;
        }
        return -1.0;
    }

    static double[] gradient(double[] target, double[] preds) {
        long n = (long)(target.length);
        double[] residuals_1 = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            double t_1 = (double)(target[(int)((long)(i_3))]);
            double y_1 = (double)(preds[(int)((long)(i_3))]);
            double exp_val_1 = (double)(exp_approx((double)((double)(t_1) * (double)(y_1))));
            double res_1 = (double)((double)(-t_1) / (double)(((double)(1.0) + (double)(exp_val_1))));
            residuals_1 = ((double[])(appendDouble(residuals_1, (double)(res_1))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return residuals_1;
    }

    static double[] predict_raw(Stump[] models, double[][] features, double learning_rate) {
        long n_1 = (long)(features.length);
        double[] preds_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(n_1)) {
            preds_1 = ((double[])(appendDouble(preds_1, (double)(0.0))));
            i_5 = (long)((long)(i_5) + 1L);
        }
        long m_1 = 0L;
        while ((long)(m_1) < (long)(models.length)) {
            Stump stump_1 = models[(int)((long)(m_1))];
            i_5 = 0L;
            while ((long)(i_5) < (long)(n_1)) {
                double value_1 = (double)(features[(int)((long)(i_5))][(int)((long)(stump_1.feature))]);
                if ((double)(value_1) <= (double)(stump_1.threshold)) {
preds_1[(int)((long)(i_5))] = (double)((double)(preds_1[(int)((long)(i_5))]) + (double)((double)(learning_rate) * (double)(stump_1.left)));
                } else {
preds_1[(int)((long)(i_5))] = (double)((double)(preds_1[(int)((long)(i_5))]) + (double)((double)(learning_rate) * (double)(stump_1.right)));
                }
                i_5 = (long)((long)(i_5) + 1L);
            }
            m_1 = (long)((long)(m_1) + 1L);
        }
        return preds_1;
    }

    static double[] predict(Stump[] models, double[][] features, double learning_rate) {
        double[] raw = ((double[])(predict_raw(((Stump[])(models)), ((double[][])(features)), (double)(learning_rate))));
        double[] result_1 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(raw.length)) {
            result_1 = ((double[])(appendDouble(result_1, (double)(signf((double)(raw[(int)((long)(i_7))]))))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return result_1;
    }

    static Stump train_stump(double[][] features, double[] residuals) {
        long n_samples = (long)(features.length);
        long n_features_1 = (long)(features[(int)(0L)].length);
        long best_feature_1 = 0L;
        double best_threshold_1 = (double)(0.0);
        double best_error_1 = (double)(1000000000.0);
        double best_left_1 = (double)(0.0);
        double best_right_1 = (double)(0.0);
        long j_1 = 0L;
        while ((long)(j_1) < (long)(n_features_1)) {
            long t_index_1 = 0L;
            while ((long)(t_index_1) < (long)(n_samples)) {
                double t_3 = (double)(features[(int)((long)(t_index_1))][(int)((long)(j_1))]);
                double sum_left_1 = (double)(0.0);
                long count_left_1 = 0L;
                double sum_right_1 = (double)(0.0);
                long count_right_1 = 0L;
                long i_9 = 0L;
                while ((long)(i_9) < (long)(n_samples)) {
                    if ((double)(features[(int)((long)(i_9))][(int)((long)(j_1))]) <= (double)(t_3)) {
                        sum_left_1 = (double)((double)(sum_left_1) + (double)(residuals[(int)((long)(i_9))]));
                        count_left_1 = (long)((long)(count_left_1) + 1L);
                    } else {
                        sum_right_1 = (double)((double)(sum_right_1) + (double)(residuals[(int)((long)(i_9))]));
                        count_right_1 = (long)((long)(count_right_1) + 1L);
                    }
                    i_9 = (long)((long)(i_9) + 1L);
                }
                double left_val_1 = (double)(0.0);
                if ((long)(count_left_1) != 0L) {
                    left_val_1 = (double)((double)(sum_left_1) / (double)((((Number)(count_left_1)).doubleValue())));
                }
                double right_val_1 = (double)(0.0);
                if ((long)(count_right_1) != 0L) {
                    right_val_1 = (double)((double)(sum_right_1) / (double)((((Number)(count_right_1)).doubleValue())));
                }
                double error_1 = (double)(0.0);
                i_9 = 0L;
                while ((long)(i_9) < (long)(n_samples)) {
                    double pred_1 = (double)((double)(features[(int)((long)(i_9))][(int)((long)(j_1))]) <= (double)(t_3) ? left_val_1 : right_val_1);
                    double diff_1 = (double)((double)(residuals[(int)((long)(i_9))]) - (double)(pred_1));
                    error_1 = (double)((double)(error_1) + (double)((double)(diff_1) * (double)(diff_1)));
                    i_9 = (long)((long)(i_9) + 1L);
                }
                if ((double)(error_1) < (double)(best_error_1)) {
                    best_error_1 = (double)(error_1);
                    best_feature_1 = (long)(j_1);
                    best_threshold_1 = (double)(t_3);
                    best_left_1 = (double)(left_val_1);
                    best_right_1 = (double)(right_val_1);
                }
                t_index_1 = (long)((long)(t_index_1) + 1L);
            }
            j_1 = (long)((long)(j_1) + 1L);
        }
        return new Stump(best_feature_1, best_threshold_1, best_left_1, best_right_1);
    }

    static Stump[] fit(long n_estimators, double learning_rate, double[][] features, double[] target) {
        Stump[] models = ((Stump[])(new Stump[]{}));
        long m_3 = 0L;
        while ((long)(m_3) < (long)(n_estimators)) {
            double[] preds_3 = ((double[])(predict_raw(((Stump[])(models)), ((double[][])(features)), (double)(learning_rate))));
            double[] grad_1 = ((double[])(gradient(((double[])(target)), ((double[])(preds_3)))));
            double[] residuals_3 = ((double[])(new double[]{}));
            long i_11 = 0L;
            while ((long)(i_11) < (long)(grad_1.length)) {
                residuals_3 = ((double[])(appendDouble(residuals_3, (double)(-grad_1[(int)((long)(i_11))]))));
                i_11 = (long)((long)(i_11) + 1L);
            }
            Stump stump_3 = train_stump(((double[][])(features)), ((double[])(residuals_3)));
            models = ((Stump[])(java.util.stream.Stream.concat(java.util.Arrays.stream(models), java.util.stream.Stream.of(stump_3)).toArray(Stump[]::new)));
            m_3 = (long)((long)(m_3) + 1L);
        }
        return models;
    }

    static double accuracy(double[] preds, double[] target) {
        long n_2 = (long)(target.length);
        long correct_1 = 0L;
        long i_13 = 0L;
        while ((long)(i_13) < (long)(n_2)) {
            if ((double)(preds[(int)((long)(i_13))]) == (double)(target[(int)((long)(i_13))])) {
                correct_1 = (long)((long)(correct_1) + 1L);
            }
            i_13 = (long)((long)(i_13) + 1L);
        }
        return (double)((((Number)(correct_1)).doubleValue())) / (double)((((Number)(n_2)).doubleValue()));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            target = ((double[])(new double[]{-1.0, -1.0, 1.0, 1.0}));
            models_1 = ((Stump[])(fit(5L, (double)(0.5), ((double[][])(features)), ((double[])(target)))));
            predictions = ((double[])(predict(((Stump[])(models_1)), ((double[][])(features)), (double)(0.5))));
            acc = (double)(accuracy(((double[])(predictions)), ((double[])(target))));
            System.out.println("Accuracy: " + _p(acc));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
