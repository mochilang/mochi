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
        long n_estimators_1 = 3;
        Tree[] trees_1 = ((Tree[])(new Tree[]{}));
        double[] predictions_1 = ((double[])(new double[]{}));
        long i_1 = 0;
        while (i_1 < target.length) {
            predictions_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(predictions_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_1 = i_1 + 1;
        }
        long est_1 = 0;
        while (est_1 < n_estimators_1) {
            double[] residuals_1 = ((double[])(new double[]{}));
            long j_1 = 0;
            while (j_1 < target.length) {
                residuals_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(residuals_1), java.util.stream.DoubleStream.of(target[(int)(j_1)] - predictions_1[(int)(j_1)])).toArray()));
                j_1 = j_1 + 1;
            }
            double sum_feat_1 = 0.0;
            j_1 = 0;
            while (j_1 < features.length) {
                sum_feat_1 = sum_feat_1 + features[(int)(j_1)][(int)(0)];
                j_1 = j_1 + 1;
            }
            double threshold_1 = sum_feat_1 / (((Number)(features.length)).doubleValue());
            double left_sum_1 = 0.0;
            long left_count_1 = 0;
            double right_sum_1 = 0.0;
            long right_count_1 = 0;
            j_1 = 0;
            while (j_1 < features.length) {
                if (features[(int)(j_1)][(int)(0)] <= threshold_1) {
                    left_sum_1 = left_sum_1 + residuals_1[(int)(j_1)];
                    left_count_1 = left_count_1 + 1;
                } else {
                    right_sum_1 = right_sum_1 + residuals_1[(int)(j_1)];
                    right_count_1 = right_count_1 + 1;
                }
                j_1 = j_1 + 1;
            }
            double left_value_1 = 0.0;
            if (left_count_1 > 0) {
                left_value_1 = left_sum_1 / (((Number)(left_count_1)).doubleValue());
            }
            double right_value_1 = 0.0;
            if (right_count_1 > 0) {
                right_value_1 = right_sum_1 / (((Number)(right_count_1)).doubleValue());
            }
            j_1 = 0;
            while (j_1 < features.length) {
                if (features[(int)(j_1)][(int)(0)] <= threshold_1) {
predictions_1[(int)(j_1)] = predictions_1[(int)(j_1)] + learning_rate * left_value_1;
                } else {
predictions_1[(int)(j_1)] = predictions_1[(int)(j_1)] + learning_rate * right_value_1;
                }
                j_1 = j_1 + 1;
            }
            trees_1 = ((Tree[])(java.util.stream.Stream.concat(java.util.Arrays.stream(trees_1), java.util.stream.Stream.of(new Tree(threshold_1, left_value_1, right_value_1))).toArray(Tree[]::new)));
            est_1 = est_1 + 1;
        }
        double[] preds_1 = ((double[])(new double[]{}));
        long t_1 = 0;
        while (t_1 < test_features.length) {
            double pred_1 = 0.0;
            long k_1 = 0;
            while (k_1 < trees_1.length) {
                if (test_features[(int)(t_1)][(int)(0)] <= trees_1[(int)(k_1)].threshold) {
                    pred_1 = pred_1 + learning_rate * trees_1[(int)(k_1)].left_value;
                } else {
                    pred_1 = pred_1 + learning_rate * trees_1[(int)(k_1)].right_value;
                }
                k_1 = k_1 + 1;
            }
            preds_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(preds_1), java.util.stream.DoubleStream.of(pred_1)).toArray()));
            t_1 = t_1 + 1;
        }
        return preds_1;
    }

    static double mean_absolute_error(double[] y_true, double[] y_pred) {
        double sum = 0.0;
        long i_3 = 0;
        while (i_3 < y_true.length) {
            double diff_1 = y_true[(int)(i_3)] - y_pred[(int)(i_3)];
            if (diff_1 < 0.0) {
                diff_1 = -diff_1;
            }
            sum = sum + diff_1;
            i_3 = i_3 + 1;
        }
        return sum / (((Number)(y_true.length)).doubleValue());
    }

    static double mean_squared_error(double[] y_true, double[] y_pred) {
        double sum_1 = 0.0;
        long i_5 = 0;
        while (i_5 < y_true.length) {
            double diff_3 = y_true[(int)(i_5)] - y_pred[(int)(i_5)];
            sum_1 = sum_1 + diff_3 * diff_3;
            i_5 = i_5 + 1;
        }
        return sum_1 / (((Number)(y_true.length)).doubleValue());
    }

    static void main() {
        Dataset california = new Dataset(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{3.0}, new double[]{4.0}}, new double[]{2.0, 3.0, 4.0, 5.0});
        Dataset ds_1 = data_handling(california);
        double[][] x_train_1 = ((double[][])(ds_1.data));
        double[] y_train_1 = ((double[])(ds_1.target));
        double[][] x_test_1 = ((double[][])(new double[][]{new double[]{1.5}, new double[]{3.5}}));
        double[] y_test_1 = ((double[])(new double[]{2.5, 4.5}));
        double[] predictions_3 = ((double[])(xgboost(((double[][])(x_train_1)), ((double[])(y_train_1)), ((double[][])(x_test_1)))));
        System.out.println("Predictions:");
        System.out.println(java.util.Arrays.toString(predictions_3));
        System.out.println("Mean Absolute Error:");
        System.out.println(mean_absolute_error(((double[])(y_test_1)), ((double[])(predictions_3))));
        System.out.println("Mean Square Error:");
        System.out.println(mean_squared_error(((double[])(y_test_1)), ((double[])(predictions_3))));
    }
    public static void main(String[] args) {
        main();
    }
}
