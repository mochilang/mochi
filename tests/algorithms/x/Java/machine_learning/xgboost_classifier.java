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
        double sum = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs.length)) {
            sum = (double)((double)(sum) + (double)(xs[(int)((long)(i_1))]));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return (double)(sum) / (double)(((double)(xs.length) * (double)(1.0)));
    }

    static double stump_predict(Stump s, double[] x) {
        if ((double)(x[(int)((long)(s.feature))]) < (double)(s.threshold)) {
            return s.left;
        }
        return s.right;
    }

    static Stump train_stump(double[][] features, double[] residuals) {
        long best_feature = 0L;
        double best_threshold_1 = (double)(0.0);
        double best_error_1 = (double)(1000000000.0);
        double best_left_1 = (double)(0.0);
        double best_right_1 = (double)(0.0);
        long num_features_1 = (long)(features[(int)(0L)].length);
        long f_1 = 0L;
        while ((long)(f_1) < (long)(num_features_1)) {
            long i_3 = 0L;
            while ((long)(i_3) < (long)(features.length)) {
                double threshold_1 = (double)(features[(int)((long)(i_3))][(int)((long)(f_1))]);
                double[] left_1 = ((double[])(new double[]{}));
                double[] right_1 = ((double[])(new double[]{}));
                long j_1 = 0L;
                while ((long)(j_1) < (long)(features.length)) {
                    if ((double)(features[(int)((long)(j_1))][(int)((long)(f_1))]) < (double)(threshold_1)) {
                        left_1 = ((double[])(concat(left_1, new double[]{residuals[(int)((long)(j_1))]})));
                    } else {
                        right_1 = ((double[])(concat(right_1, new double[]{residuals[(int)((long)(j_1))]})));
                    }
                    j_1 = (long)((long)(j_1) + 1L);
                }
                if ((long)(left_1.length) != 0L && (long)(right_1.length) != 0L) {
                    double left_mean_1 = (double)(mean(((double[])(left_1))));
                    double right_mean_1 = (double)(mean(((double[])(right_1))));
                    double err_1 = (double)(0.0);
                    j_1 = 0L;
                    while ((long)(j_1) < (long)(features.length)) {
                        double pred_1 = (double)((double)(features[(int)((long)(j_1))][(int)((long)(f_1))]) < (double)(threshold_1) ? left_mean_1 : right_mean_1);
                        double diff_1 = (double)((double)(residuals[(int)((long)(j_1))]) - (double)(pred_1));
                        err_1 = (double)((double)(err_1) + (double)((double)(diff_1) * (double)(diff_1)));
                        j_1 = (long)((long)(j_1) + 1L);
                    }
                    if ((double)(err_1) < (double)(best_error_1)) {
                        best_error_1 = (double)(err_1);
                        best_feature = (long)(f_1);
                        best_threshold_1 = (double)(threshold_1);
                        best_left_1 = (double)(left_mean_1);
                        best_right_1 = (double)(right_mean_1);
                    }
                }
                i_3 = (long)((long)(i_3) + 1L);
            }
            f_1 = (long)((long)(f_1) + 1L);
        }
        return new Stump(best_feature, best_threshold_1, best_left_1, best_right_1);
    }

    static Stump[] boost(double[][] features, long[] targets, long rounds) {
        Stump[] model = ((Stump[])(new Stump[]{}));
        double[] preds_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(targets.length)) {
            preds_1 = ((double[])(concat(preds_1, new double[]{0.0})));
            i_5 = (long)((long)(i_5) + 1L);
        }
        long r_1 = 0L;
        while ((long)(r_1) < (long)(rounds)) {
            double[] residuals_1 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(targets.length)) {
                residuals_1 = ((double[])(concat(residuals_1, new double[]{(double)(targets[(int)((long)(j_3))]) - (double)(preds_1[(int)((long)(j_3))])})));
                j_3 = (long)((long)(j_3) + 1L);
            }
            Stump stump_1 = train_stump(((double[][])(features)), ((double[])(residuals_1)));
            model = ((Stump[])(concat(model, new Stump[]{stump_1})));
            j_3 = 0L;
            while ((long)(j_3) < (long)(preds_1.length)) {
preds_1[(int)((long)(j_3))] = (double)((double)(preds_1[(int)((long)(j_3))]) + (double)(stump_predict(stump_1, ((double[])(features[(int)((long)(j_3))])))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            r_1 = (long)((long)(r_1) + 1L);
        }
        return model;
    }

    static double predict(Stump[] model, double[] x) {
        double score = (double)(0.0);
        long i_7 = 0L;
        while ((long)(i_7) < (long)(model.length)) {
            Stump s_1 = model[(int)((long)(i_7))];
            if ((double)(x[(int)((long)(s_1.feature))]) < (double)(s_1.threshold)) {
                score = (double)((double)(score) + (double)(s_1.left));
            } else {
                score = (double)((double)(score) + (double)(s_1.right));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return score;
    }

    static void main() {
        double[][] features = ((double[][])(new double[][]{new double[]{5.1, 3.5}, new double[]{4.9, 3.0}, new double[]{6.2, 3.4}, new double[]{5.9, 3.0}}));
        long[] targets_1 = ((long[])(new long[]{0, 0, 1, 1}));
        Stump[] model_2 = ((Stump[])(boost(((double[][])(features)), ((long[])(targets_1)), 3L)));
        String out_1 = "";
        long i_9 = 0L;
        while ((long)(i_9) < (long)(features.length)) {
            double s_3 = (double)(predict(((Stump[])(model_2)), ((double[])(features[(int)((long)(i_9))]))));
            long label_1 = (long)((double)(s_3) >= (double)(0.5) ? 1 : 0);
            if ((long)(i_9) == 0L) {
                out_1 = _p(label_1);
            } else {
                out_1 = out_1 + " " + _p(label_1);
            }
            i_9 = (long)((long)(i_9) + 1L);
        }
        System.out.println(out_1);
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
