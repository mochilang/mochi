public class Main {
    static class Stump {
        int feature;
        double threshold;
        double left;
        double right;
        Stump(int feature, double threshold, double left, double right) {
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
        int i = 0;
        while (i < xs.length) {
            sum = sum + xs[i];
            i = i + 1;
        }
        return sum / (xs.length * 1.0);
    }

    static double stump_predict(Stump s, double[] x) {
        if (x[s.feature] < s.threshold) {
            return s.left;
        }
        return s.right;
    }

    static Stump train_stump(double[][] features, double[] residuals) {
        int best_feature = 0;
        double best_threshold = 0.0;
        double best_error = 1000000000.0;
        double best_left = 0.0;
        double best_right = 0.0;
        int num_features = features[0].length;
        int f = 0;
        while (f < num_features) {
            int i_1 = 0;
            while (i_1 < features.length) {
                double threshold = features[i_1][f];
                double[] left = ((double[])(new double[]{}));
                double[] right = ((double[])(new double[]{}));
                int j = 0;
                while (j < features.length) {
                    if (features[j][f] < threshold) {
                        left = ((double[])(concat(left, new double[]{residuals[j]})));
                    } else {
                        right = ((double[])(concat(right, new double[]{residuals[j]})));
                    }
                    j = j + 1;
                }
                if (left.length != 0 && right.length != 0) {
                    double left_mean = mean(((double[])(left)));
                    double right_mean = mean(((double[])(right)));
                    double err = 0.0;
                    j = 0;
                    while (j < features.length) {
                        double pred = features[j][f] < threshold ? left_mean : right_mean;
                        double diff = residuals[j] - pred;
                        err = err + diff * diff;
                        j = j + 1;
                    }
                    if (err < best_error) {
                        best_error = err;
                        best_feature = f;
                        best_threshold = threshold;
                        best_left = left_mean;
                        best_right = right_mean;
                    }
                }
                i_1 = i_1 + 1;
            }
            f = f + 1;
        }
        return new Stump(best_feature, best_threshold, best_left, best_right);
    }

    static Stump[] boost(double[][] features, int[] targets, int rounds) {
        Stump[] model = ((Stump[])(new Stump[]{}));
        double[] preds = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < targets.length) {
            preds = ((double[])(concat(preds, new double[]{0.0})));
            i_2 = i_2 + 1;
        }
        int r = 0;
        while (r < rounds) {
            double[] residuals = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < targets.length) {
                residuals = ((double[])(concat(residuals, new double[]{targets[j_1] - preds[j_1]})));
                j_1 = j_1 + 1;
            }
            Stump stump = train_stump(((double[][])(features)), ((double[])(residuals)));
            model = ((Stump[])(concat(model, new Stump[]{stump})));
            j_1 = 0;
            while (j_1 < preds.length) {
preds[j_1] = preds[j_1] + stump_predict(stump, ((double[])(features[j_1])));
                j_1 = j_1 + 1;
            }
            r = r + 1;
        }
        return model;
    }

    static double predict(Stump[] model, double[] x) {
        double score = 0.0;
        int i_3 = 0;
        while (i_3 < model.length) {
            Stump s = model[i_3];
            if (x[s.feature] < s.threshold) {
                score = score + s.left;
            } else {
                score = score + s.right;
            }
            i_3 = i_3 + 1;
        }
        return score;
    }

    static void main() {
        double[][] features = ((double[][])(new double[][]{new double[]{5.1, 3.5}, new double[]{4.9, 3.0}, new double[]{6.2, 3.4}, new double[]{5.9, 3.0}}));
        int[] targets = ((int[])(new int[]{0, 0, 1, 1}));
        Stump[] model_1 = ((Stump[])(boost(((double[][])(features)), ((int[])(targets)), 3)));
        String out = "";
        int i_4 = 0;
        while (i_4 < features.length) {
            double s_1 = predict(((Stump[])(model_1)), ((double[])(features[i_4])));
            int label = s_1 >= 0.5 ? 1 : 0;
            if (i_4 == 0) {
                out = _p(label);
            } else {
                out = out + " " + _p(label);
            }
            i_4 = i_4 + 1;
        }
        System.out.println(out);
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
        return String.valueOf(v);
    }
}
