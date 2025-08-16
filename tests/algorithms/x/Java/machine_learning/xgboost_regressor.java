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
        double learning_rate = (double)(0.5);
        long n_estimators_1 = 3L;
        Tree[] trees_1 = ((Tree[])(new Tree[]{}));
        double[] predictions_1 = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(target.length)) {
            predictions_1 = ((double[])(appendDouble(predictions_1, (double)(0.0))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long est_1 = 0L;
        while ((long)(est_1) < (long)(n_estimators_1)) {
            double[] residuals_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(target.length)) {
                residuals_1 = ((double[])(appendDouble(residuals_1, (double)((double)(target[(int)((long)(j_1))]) - (double)(predictions_1[(int)((long)(j_1))])))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            double sum_feat_1 = (double)(0.0);
            j_1 = 0L;
            while ((long)(j_1) < (long)(features.length)) {
                sum_feat_1 = (double)((double)(sum_feat_1) + (double)(features[(int)((long)(j_1))][(int)(0L)]));
                j_1 = (long)((long)(j_1) + 1L);
            }
            double threshold_1 = (double)((double)(sum_feat_1) / (double)((((Number)(features.length)).doubleValue())));
            double left_sum_1 = (double)(0.0);
            long left_count_1 = 0L;
            double right_sum_1 = (double)(0.0);
            long right_count_1 = 0L;
            j_1 = 0L;
            while ((long)(j_1) < (long)(features.length)) {
                if ((double)(features[(int)((long)(j_1))][(int)(0L)]) <= (double)(threshold_1)) {
                    left_sum_1 = (double)((double)(left_sum_1) + (double)(residuals_1[(int)((long)(j_1))]));
                    left_count_1 = (long)((long)(left_count_1) + 1L);
                } else {
                    right_sum_1 = (double)((double)(right_sum_1) + (double)(residuals_1[(int)((long)(j_1))]));
                    right_count_1 = (long)((long)(right_count_1) + 1L);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            double left_value_1 = (double)(0.0);
            if ((long)(left_count_1) > 0L) {
                left_value_1 = (double)((double)(left_sum_1) / (double)((((Number)(left_count_1)).doubleValue())));
            }
            double right_value_1 = (double)(0.0);
            if ((long)(right_count_1) > 0L) {
                right_value_1 = (double)((double)(right_sum_1) / (double)((((Number)(right_count_1)).doubleValue())));
            }
            j_1 = 0L;
            while ((long)(j_1) < (long)(features.length)) {
                if ((double)(features[(int)((long)(j_1))][(int)(0L)]) <= (double)(threshold_1)) {
predictions_1[(int)((long)(j_1))] = (double)((double)(predictions_1[(int)((long)(j_1))]) + (double)((double)(learning_rate) * (double)(left_value_1)));
                } else {
predictions_1[(int)((long)(j_1))] = (double)((double)(predictions_1[(int)((long)(j_1))]) + (double)((double)(learning_rate) * (double)(right_value_1)));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            trees_1 = ((Tree[])(java.util.stream.Stream.concat(java.util.Arrays.stream(trees_1), java.util.stream.Stream.of(new Tree(threshold_1, left_value_1, right_value_1))).toArray(Tree[]::new)));
            est_1 = (long)((long)(est_1) + 1L);
        }
        double[] preds_1 = ((double[])(new double[]{}));
        long t_1 = 0L;
        while ((long)(t_1) < (long)(test_features.length)) {
            double pred_1 = (double)(0.0);
            long k_1 = 0L;
            while ((long)(k_1) < (long)(trees_1.length)) {
                if ((double)(test_features[(int)((long)(t_1))][(int)(0L)]) <= (double)(trees_1[(int)((long)(k_1))].threshold)) {
                    pred_1 = (double)((double)(pred_1) + (double)((double)(learning_rate) * (double)(trees_1[(int)((long)(k_1))].left_value)));
                } else {
                    pred_1 = (double)((double)(pred_1) + (double)((double)(learning_rate) * (double)(trees_1[(int)((long)(k_1))].right_value)));
                }
                k_1 = (long)((long)(k_1) + 1L);
            }
            preds_1 = ((double[])(appendDouble(preds_1, (double)(pred_1))));
            t_1 = (long)((long)(t_1) + 1L);
        }
        return preds_1;
    }

    static double mean_absolute_error(double[] y_true, double[] y_pred) {
        double sum = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(y_true.length)) {
            double diff_1 = (double)((double)(y_true[(int)((long)(i_3))]) - (double)(y_pred[(int)((long)(i_3))]));
            if ((double)(diff_1) < (double)(0.0)) {
                diff_1 = (double)(-diff_1);
            }
            sum = (double)((double)(sum) + (double)(diff_1));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(sum) / (double)((((Number)(y_true.length)).doubleValue()));
    }

    static double mean_squared_error(double[] y_true, double[] y_pred) {
        double sum_1 = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(y_true.length)) {
            double diff_3 = (double)((double)(y_true[(int)((long)(i_5))]) - (double)(y_pred[(int)((long)(i_5))]));
            sum_1 = (double)((double)(sum_1) + (double)((double)(diff_3) * (double)(diff_3)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return (double)(sum_1) / (double)((((Number)(y_true.length)).doubleValue()));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
