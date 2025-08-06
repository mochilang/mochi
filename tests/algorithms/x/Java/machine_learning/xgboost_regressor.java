public class Main {
    static class Dataset {
        double[][] data;
        double[] target;
        Dataset(double[][] data, double[] target) {
            this.data = data;
            this.target = target;
        }
        Dataset() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'target': %s}", String.valueOf(data), String.valueOf(target));
        }
    }

    static class Tree {
        double threshold;
        double left_value;
        double right_value;
        Tree(double threshold, double left_value, double right_value) {
            this.threshold = threshold;
            this.left_value = left_value;
            this.right_value = right_value;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'threshold': %s, 'left_value': %s, 'right_value': %s}", String.valueOf(threshold), String.valueOf(left_value), String.valueOf(right_value));
        }
    }


    static Dataset data_handling(Dataset dataset) {
        return dataset;
    }

    static double[] xgboost(double[][] features, double[] target, double[][] test_features) {
        double learning_rate = 0.5;
        int n_estimators = 3;
        Tree[] trees = ((Tree[])(new Tree[]{}));
        double[] predictions = ((double[])(new double[]{}));
        int i = 0;
        while (i < target.length) {
            predictions = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(predictions), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i = i + 1;
        }
        int est = 0;
        while (est < n_estimators) {
            double[] residuals = ((double[])(new double[]{}));
            int j = 0;
            while (j < target.length) {
                residuals = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(residuals), java.util.stream.DoubleStream.of(target[j] - predictions[j])).toArray()));
                j = j + 1;
            }
            double sum_feat = 0.0;
            j = 0;
            while (j < features.length) {
                sum_feat = sum_feat + features[j][0];
                j = j + 1;
            }
            double threshold = sum_feat / (((Number)(features.length)).doubleValue());
            double left_sum = 0.0;
            int left_count = 0;
            double right_sum = 0.0;
            int right_count = 0;
            j = 0;
            while (j < features.length) {
                if (features[j][0] <= threshold) {
                    left_sum = left_sum + residuals[j];
                    left_count = left_count + 1;
                } else {
                    right_sum = right_sum + residuals[j];
                    right_count = right_count + 1;
                }
                j = j + 1;
            }
            double left_value = 0.0;
            if (left_count > 0) {
                left_value = left_sum / (((Number)(left_count)).doubleValue());
            }
            double right_value = 0.0;
            if (right_count > 0) {
                right_value = right_sum / (((Number)(right_count)).doubleValue());
            }
            j = 0;
            while (j < features.length) {
                if (features[j][0] <= threshold) {
predictions[j] = predictions[j] + learning_rate * left_value;
                } else {
predictions[j] = predictions[j] + learning_rate * right_value;
                }
                j = j + 1;
            }
            trees = ((Tree[])(java.util.stream.Stream.concat(java.util.Arrays.stream(trees), java.util.stream.Stream.of(new Tree(threshold, left_value, right_value))).toArray(Tree[]::new)));
            est = est + 1;
        }
        double[] preds = ((double[])(new double[]{}));
        int t = 0;
        while (t < test_features.length) {
            double pred = 0.0;
            int k = 0;
            while (k < trees.length) {
                if (test_features[t][0] <= trees[k].threshold) {
                    pred = pred + learning_rate * trees[k].left_value;
                } else {
                    pred = pred + learning_rate * trees[k].right_value;
                }
                k = k + 1;
            }
            preds = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(preds), java.util.stream.DoubleStream.of(pred)).toArray()));
            t = t + 1;
        }
        return preds;
    }

    static double mean_absolute_error(double[] y_true, double[] y_pred) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < y_true.length) {
            double diff = y_true[i_1] - y_pred[i_1];
            if (diff < 0.0) {
                diff = -diff;
            }
            sum = sum + diff;
            i_1 = i_1 + 1;
        }
        return sum / (((Number)(y_true.length)).doubleValue());
    }

    static double mean_squared_error(double[] y_true, double[] y_pred) {
        double sum_1 = 0.0;
        int i_2 = 0;
        while (i_2 < y_true.length) {
            double diff_1 = y_true[i_2] - y_pred[i_2];
            sum_1 = sum_1 + diff_1 * diff_1;
            i_2 = i_2 + 1;
        }
        return sum_1 / (((Number)(y_true.length)).doubleValue());
    }

    static void main() {
        Dataset california = new Dataset(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{3.0}, new double[]{4.0}}, new double[]{2.0, 3.0, 4.0, 5.0});
        Dataset ds = data_handling(california);
        double[][] x_train = ((double[][])(ds.data));
        double[] y_train = ((double[])(ds.target));
        double[][] x_test = ((double[][])(new double[][]{new double[]{1.5}, new double[]{3.5}}));
        double[] y_test = ((double[])(new double[]{2.5, 4.5}));
        double[] predictions_1 = ((double[])(xgboost(((double[][])(x_train)), ((double[])(y_train)), ((double[][])(x_test)))));
        System.out.println("Predictions:");
        System.out.println(java.util.Arrays.toString(predictions_1));
        System.out.println("Mean Absolute Error:");
        System.out.println(mean_absolute_error(((double[])(y_test)), ((double[])(predictions_1))));
        System.out.println("Mean Square Error:");
        System.out.println(mean_squared_error(((double[])(y_test)), ((double[])(predictions_1))));
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
