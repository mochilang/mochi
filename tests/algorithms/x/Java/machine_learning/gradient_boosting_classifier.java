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

    static double[][] features;
    static double[] target;
    static Stump[] models_1;
    static double[] predictions;
    static double acc;

    static double exp_approx(double x) {
        double term = 1.0;
        double sum = 1.0;
        int i = 1;
        while (i < 10) {
            term = term * x / (((Number)(i)).doubleValue());
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double signf(double x) {
        if (x >= 0.0) {
            return 1.0;
        }
        return -1.0;
    }

    static double[] gradient(double[] target, double[] preds) {
        int n = target.length;
        double[] residuals = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            double t = target[i_1];
            double y = preds[i_1];
            double exp_val = exp_approx(t * y);
            double res = -t / (1.0 + exp_val);
            residuals = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(residuals), java.util.stream.DoubleStream.of(res)).toArray()));
            i_1 = i_1 + 1;
        }
        return residuals;
    }

    static double[] predict_raw(Stump[] models, double[][] features, double learning_rate) {
        int n_1 = features.length;
        double[] preds = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < n_1) {
            preds = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(preds), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_2 = i_2 + 1;
        }
        int m = 0;
        while (m < models.length) {
            Stump stump = models[m];
            i_2 = 0;
            while (i_2 < n_1) {
                double value = features[i_2][stump.feature];
                if (value <= stump.threshold) {
preds[i_2] = preds[i_2] + learning_rate * stump.left;
                } else {
preds[i_2] = preds[i_2] + learning_rate * stump.right;
                }
                i_2 = i_2 + 1;
            }
            m = m + 1;
        }
        return preds;
    }

    static double[] predict(Stump[] models, double[][] features, double learning_rate) {
        double[] raw = ((double[])(predict_raw(((Stump[])(models)), ((double[][])(features)), learning_rate)));
        double[] result = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < raw.length) {
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(signf(raw[i_3]))).toArray()));
            i_3 = i_3 + 1;
        }
        return result;
    }

    static Stump train_stump(double[][] features, double[] residuals) {
        int n_samples = features.length;
        int n_features = features[0].length;
        int best_feature = 0;
        double best_threshold = 0.0;
        double best_error = 1000000000.0;
        double best_left = 0.0;
        double best_right = 0.0;
        int j = 0;
        while (j < n_features) {
            int t_index = 0;
            while (t_index < n_samples) {
                double t_1 = features[t_index][j];
                double sum_left = 0.0;
                int count_left = 0;
                double sum_right = 0.0;
                int count_right = 0;
                int i_4 = 0;
                while (i_4 < n_samples) {
                    if (features[i_4][j] <= t_1) {
                        sum_left = sum_left + residuals[i_4];
                        count_left = count_left + 1;
                    } else {
                        sum_right = sum_right + residuals[i_4];
                        count_right = count_right + 1;
                    }
                    i_4 = i_4 + 1;
                }
                double left_val = 0.0;
                if (count_left != 0) {
                    left_val = sum_left / (((Number)(count_left)).doubleValue());
                }
                double right_val = 0.0;
                if (count_right != 0) {
                    right_val = sum_right / (((Number)(count_right)).doubleValue());
                }
                double error = 0.0;
                i_4 = 0;
                while (i_4 < n_samples) {
                    double pred = features[i_4][j] <= t_1 ? left_val : right_val;
                    double diff = residuals[i_4] - pred;
                    error = error + diff * diff;
                    i_4 = i_4 + 1;
                }
                if (error < best_error) {
                    best_error = error;
                    best_feature = j;
                    best_threshold = t_1;
                    best_left = left_val;
                    best_right = right_val;
                }
                t_index = t_index + 1;
            }
            j = j + 1;
        }
        return new Stump(best_feature, best_threshold, best_left, best_right);
    }

    static Stump[] fit(int n_estimators, double learning_rate, double[][] features, double[] target) {
        Stump[] models = ((Stump[])(new Stump[]{}));
        int m_1 = 0;
        while (m_1 < n_estimators) {
            double[] preds_1 = ((double[])(predict_raw(((Stump[])(models)), ((double[][])(features)), learning_rate)));
            double[] grad = ((double[])(gradient(((double[])(target)), ((double[])(preds_1)))));
            double[] residuals_1 = ((double[])(new double[]{}));
            int i_5 = 0;
            while (i_5 < grad.length) {
                residuals_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(residuals_1), java.util.stream.DoubleStream.of(-grad[i_5])).toArray()));
                i_5 = i_5 + 1;
            }
            Stump stump_1 = train_stump(((double[][])(features)), ((double[])(residuals_1)));
            models = ((Stump[])(java.util.stream.Stream.concat(java.util.Arrays.stream(models), java.util.stream.Stream.of(stump_1)).toArray(Stump[]::new)));
            m_1 = m_1 + 1;
        }
        return models;
    }

    static double accuracy(double[] preds, double[] target) {
        int n_2 = target.length;
        int correct = 0;
        int i_6 = 0;
        while (i_6 < n_2) {
            if (preds[i_6] == target[i_6]) {
                correct = correct + 1;
            }
            i_6 = i_6 + 1;
        }
        return (((Number)(correct)).doubleValue()) / (((Number)(n_2)).doubleValue());
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            features = ((double[][])(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{3.0}, new double[]{4.0}}));
            target = ((double[])(new double[]{-1.0, -1.0, 1.0, 1.0}));
            models_1 = ((Stump[])(fit(5, 0.5, ((double[][])(features)), ((double[])(target)))));
            predictions = ((double[])(predict(((Stump[])(models_1)), ((double[][])(features)), 0.5)));
            acc = accuracy(((double[])(predictions)), ((double[])(target)));
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
