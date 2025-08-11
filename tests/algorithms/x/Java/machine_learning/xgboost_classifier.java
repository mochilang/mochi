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


    static double mean(double[] xs) {
        double sum = 0.0;
        long i_1 = 0;
        while (i_1 < xs.length) {
            sum = sum + xs[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return sum / (xs.length * 1.0);
    }

    static double stump_predict(Stump s, double[] x) {
        if (x[(int)(s.feature)] < s.threshold) {
            return s.left;
        }
        return s.right;
    }

    static Stump train_stump(double[][] features, double[] residuals) {
        long best_feature = 0;
        double best_threshold_1 = 0.0;
        double best_error_1 = 1000000000.0;
        double best_left_1 = 0.0;
        double best_right_1 = 0.0;
        long num_features_1 = features[(int)(0)].length;
        long f_1 = 0;
        while (f_1 < num_features_1) {
            long i_3 = 0;
            while (i_3 < features.length) {
                double threshold_1 = features[(int)(i_3)][(int)(f_1)];
                double[] left_1 = ((double[])(new double[]{}));
                double[] right_1 = ((double[])(new double[]{}));
                long j_1 = 0;
                while (j_1 < features.length) {
                    if (features[(int)(j_1)][(int)(f_1)] < threshold_1) {
                        left_1 = ((double[])(concat(left_1, new double[]{residuals[(int)(j_1)]})));
                    } else {
                        right_1 = ((double[])(concat(right_1, new double[]{residuals[(int)(j_1)]})));
                    }
                    j_1 = j_1 + 1;
                }
                if (left_1.length != 0 && right_1.length != 0) {
                    double left_mean_1 = mean(((double[])(left_1)));
                    double right_mean_1 = mean(((double[])(right_1)));
                    double err_1 = 0.0;
                    j_1 = 0;
                    while (j_1 < features.length) {
                        double pred_1 = features[(int)(j_1)][(int)(f_1)] < threshold_1 ? left_mean_1 : right_mean_1;
                        double diff_1 = residuals[(int)(j_1)] - pred_1;
                        err_1 = err_1 + diff_1 * diff_1;
                        j_1 = j_1 + 1;
                    }
                    if (err_1 < best_error_1) {
                        best_error_1 = err_1;
                        best_feature = f_1;
                        best_threshold_1 = threshold_1;
                        best_left_1 = left_mean_1;
                        best_right_1 = right_mean_1;
                    }
                }
                i_3 = i_3 + 1;
            }
            f_1 = f_1 + 1;
        }
        return new Stump(best_feature, best_threshold_1, best_left_1, best_right_1);
    }

    static Stump[] boost(double[][] features, long[] targets, long rounds) {
        Stump[] model = ((Stump[])(new Stump[]{}));
        double[] preds_1 = ((double[])(new double[]{}));
        long i_5 = 0;
        while (i_5 < targets.length) {
            preds_1 = ((double[])(concat(preds_1, new double[]{0.0})));
            i_5 = i_5 + 1;
        }
        long r_1 = 0;
        while (r_1 < rounds) {
            double[] residuals_1 = ((double[])(new double[]{}));
            long j_3 = 0;
            while (j_3 < targets.length) {
                residuals_1 = ((double[])(concat(residuals_1, new double[]{targets[(int)(j_3)] - preds_1[(int)(j_3)]})));
                j_3 = j_3 + 1;
            }
            Stump stump_1 = train_stump(((double[][])(features)), ((double[])(residuals_1)));
            model = ((Stump[])(concat(model, new Stump[]{stump_1})));
            j_3 = 0;
            while (j_3 < preds_1.length) {
preds_1[(int)(j_3)] = preds_1[(int)(j_3)] + stump_predict(stump_1, ((double[])(features[(int)(j_3)])));
                j_3 = j_3 + 1;
            }
            r_1 = r_1 + 1;
        }
        return model;
    }

    static double predict(Stump[] model, double[] x) {
        double score = 0.0;
        long i_7 = 0;
        while (i_7 < model.length) {
            Stump s_1 = model[(int)(i_7)];
            if (x[(int)(s_1.feature)] < s_1.threshold) {
                score = score + s_1.left;
            } else {
                score = score + s_1.right;
            }
            i_7 = i_7 + 1;
        }
        return score;
    }

    static void main() {
        double[][] features = ((double[][])(new double[][]{new double[]{5.1, 3.5}, new double[]{4.9, 3.0}, new double[]{6.2, 3.4}, new double[]{5.9, 3.0}}));
        long[] targets_1 = ((long[])(new long[]{0, 0, 1, 1}));
        Stump[] model_2 = ((Stump[])(boost(((double[][])(features)), ((long[])(targets_1)), 3)));
        String out_1 = "";
        long i_9 = 0;
        while (i_9 < features.length) {
            double s_3 = predict(((Stump[])(model_2)), ((double[])(features[(int)(i_9)])));
            long label_1 = s_3 >= 0.5 ? 1 : 0;
            if (i_9 == 0) {
                out_1 = _p(label_1);
            } else {
                out_1 = out_1 + " " + _p(label_1);
            }
            i_9 = i_9 + 1;
        }
        System.out.println(out_1);
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
