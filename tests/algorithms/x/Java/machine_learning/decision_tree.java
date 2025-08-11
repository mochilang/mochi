public class Main {
    interface Tree {}

    static class Leaf implements Tree {
        double prediction;
        Leaf(double prediction) {
            this.prediction = prediction;
        }
        Leaf() {}
        @Override public String toString() {
            return String.format("{'prediction': %s}", String.valueOf(prediction));
        }
    }

    static class Branch implements Tree {
        double decision_boundary;
        Tree left;
        Tree right;
        Branch(double decision_boundary, Tree left, Tree right) {
            this.decision_boundary = decision_boundary;
            this.left = left;
            this.right = right;
        }
        Branch() {}
        @Override public String toString() {
            return String.format("{'decision_boundary': %s, 'left': %s, 'right': %s}", String.valueOf(decision_boundary), String.valueOf(left), String.valueOf(right));
        }
    }

    static double PI;
    static double TWO_PI;
    static long seed = 0;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double sin(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2_1 = y * y;
        double y3_1 = y2_1 * y;
        double y5_1 = y3_1 * y2_1;
        double y7_1 = y5_1 * y2_1;
        return y - y3_1 / 6.0 + y5_1 / 120.0 - y7_1 / 5040.0;
    }

    static double rand() {
        seed = ((long)(Math.floorMod(((long)((1103515245 * seed + 12345))), 2147483648L)));
        return ((Number)(seed)).doubleValue() / 2147483648.0;
    }

    static double mean(double[] vals) {
        double sum = 0.0;
        long i_1 = 0;
        while (i_1 < vals.length) {
            sum = sum + vals[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return sum / vals.length;
    }

    static double mean_squared_error(double[] labels, double prediction) {
        double total = 0.0;
        long i_3 = 0;
        while (i_3 < labels.length) {
            double diff_1 = labels[(int)(i_3)] - prediction;
            total = total + diff_1 * diff_1;
            i_3 = i_3 + 1;
        }
        return total / labels.length;
    }

    static Tree train_tree(double[] x, double[] y, long depth, long min_leaf_size) {
        if (x.length < 2 * min_leaf_size) {
            return ((Tree)(new Leaf(mean(((double[])(y))))));
        }
        if (depth == 1) {
            return ((Tree)(new Leaf(mean(((double[])(y))))));
        }
        long best_split_1 = 0;
        double min_error_1 = mean_squared_error(((double[])(x)), mean(((double[])(y)))) * 2.0;
        long i_5 = 0;
        while (i_5 < x.length) {
            if (java.util.Arrays.copyOfRange(x, (int)(0), (int)(i_5)).length < min_leaf_size) {
                i_5 = i_5;
            } else             if (java.util.Arrays.copyOfRange(x, (int)(i_5), (int)(x.length)).length < min_leaf_size) {
                i_5 = i_5;
            } else {
                double err_left_1 = mean_squared_error(((double[])(java.util.Arrays.copyOfRange(x, (int)(0), (int)(i_5)))), mean(((double[])(java.util.Arrays.copyOfRange(y, (int)(0), (int)(i_5))))));
                double err_right_1 = mean_squared_error(((double[])(java.util.Arrays.copyOfRange(x, (int)(i_5), (int)(x.length)))), mean(((double[])(java.util.Arrays.copyOfRange(y, (int)(i_5), (int)(y.length))))));
                double err_1 = err_left_1 + err_right_1;
                if (err_1 < min_error_1) {
                    best_split_1 = i_5;
                    min_error_1 = err_1;
                }
            }
            i_5 = i_5 + 1;
        }
        if (best_split_1 != 0) {
            double[] left_x_1 = ((double[])(java.util.Arrays.copyOfRange(x, (int)(0), (int)(best_split_1))));
            double[] left_y_1 = ((double[])(java.util.Arrays.copyOfRange(y, (int)(0), (int)(best_split_1))));
            double[] right_x_1 = ((double[])(java.util.Arrays.copyOfRange(x, (int)(best_split_1), (int)(x.length))));
            double[] right_y_1 = ((double[])(java.util.Arrays.copyOfRange(y, (int)(best_split_1), (int)(y.length))));
            double boundary_1 = x[(int)(best_split_1)];
            Tree left_tree_1 = train_tree(((double[])(left_x_1)), ((double[])(left_y_1)), depth - 1, min_leaf_size);
            Tree right_tree_1 = train_tree(((double[])(right_x_1)), ((double[])(right_y_1)), depth - 1, min_leaf_size);
            return ((Tree)(new Branch(boundary_1, left_tree_1, right_tree_1)));
        }
        return ((Tree)(new Leaf(mean(((double[])(y))))));
    }

    static double predict(Tree tree, double value) {
        return tree instanceof Leaf ? ((Leaf)(tree)).prediction : value >= ((Number)(((Branch)(tree)).decision_boundary)).intValue() ? predict(((Branch)(tree)).right, value) : predict(((Branch)(tree)).left, value);
    }

    static void main() {
        double[] x = ((double[])(new double[]{}));
        double v_1 = -1.0;
        while (v_1 < 1.0) {
            x = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x), java.util.stream.DoubleStream.of(v_1)).toArray()));
            v_1 = v_1 + 0.005;
        }
        double[] y_2 = ((double[])(new double[]{}));
        long i_7 = 0;
        while (i_7 < x.length) {
            y_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_2), java.util.stream.DoubleStream.of(sin(x[(int)(i_7)]))).toArray()));
            i_7 = i_7 + 1;
        }
        Tree tree_1 = train_tree(((double[])(x)), ((double[])(y_2)), 10, 10);
        double[] test_cases_1 = ((double[])(new double[]{}));
        i_7 = 0;
        while (i_7 < 10) {
            test_cases_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(test_cases_1), java.util.stream.DoubleStream.of(rand() * 2.0 - 1.0)).toArray()));
            i_7 = i_7 + 1;
        }
        double[] predictions_1 = ((double[])(new double[]{}));
        i_7 = 0;
        while (i_7 < test_cases_1.length) {
            predictions_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(predictions_1), java.util.stream.DoubleStream.of(predict(tree_1, test_cases_1[(int)(i_7)]))).toArray()));
            i_7 = i_7 + 1;
        }
        double sum_err_1 = 0.0;
        i_7 = 0;
        while (i_7 < test_cases_1.length) {
            double diff_3 = predictions_1[(int)(i_7)] - test_cases_1[(int)(i_7)];
            sum_err_1 = sum_err_1 + diff_3 * diff_3;
            i_7 = i_7 + 1;
        }
        double avg_error_1 = sum_err_1 / test_cases_1.length;
        System.out.println("Test values: " + _p(test_cases_1));
        System.out.println("Predictions: " + _p(predictions_1));
        System.out.println("Average error: " + _p(avg_error_1));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        TWO_PI = 6.283185307179586;
        seed = 123456789;
        main();
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
