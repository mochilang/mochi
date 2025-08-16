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

    static double PI = (double)(3.141592653589793);
    static double TWO_PI = (double)(6.283185307179586);
    static long seed = 123456789L;

    static double _mod(double x, double m) {
        return (double)(x) - (double)((double)((((Number)(((Number)((double)(x) / (double)(m))).intValue())).doubleValue())) * (double)(m));
    }

    static double sin(double x) {
        double y = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_1 = (double)((double)(y) * (double)(y));
        double y3_1 = (double)((double)(y2_1) * (double)(y));
        double y5_1 = (double)((double)(y3_1) * (double)(y2_1));
        double y7_1 = (double)((double)(y5_1) * (double)(y2_1));
        return (double)((double)((double)(y) - (double)((double)(y3_1) / (double)(6.0))) + (double)((double)(y5_1) / (double)(120.0))) - (double)((double)(y7_1) / (double)(5040.0));
    }

    static double rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)(1103515245L * (long)(seed)) + 12345L))), 2147483648L))));
        return (double)(((Number)(seed)).doubleValue()) / (double)(2147483648.0);
    }

    static double mean(double[] vals) {
        double sum = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vals.length)) {
            sum = (double)((double)(sum) + (double)(vals[(int)((long)(i_1))]));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return (double)(sum) / (double)(vals.length);
    }

    static double mean_squared_error(double[] labels, double prediction) {
        double total = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(labels.length)) {
            double diff_1 = (double)((double)(labels[(int)((long)(i_3))]) - (double)(prediction));
            total = (double)((double)(total) + (double)((double)(diff_1) * (double)(diff_1)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(total) / (double)(labels.length);
    }

    static Tree train_tree(double[] x, double[] y, long depth, long min_leaf_size) {
        if ((long)(x.length) < (long)(2L * (long)(min_leaf_size))) {
            return ((Tree)(new Leaf(mean(((double[])(y))))));
        }
        if ((long)(depth) == 1L) {
            return ((Tree)(new Leaf(mean(((double[])(y))))));
        }
        long best_split_1 = 0L;
        double min_error_1 = (double)((double)(mean_squared_error(((double[])(x)), (double)(mean(((double[])(y)))))) * (double)(2.0));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(x.length)) {
            if ((long)(java.util.Arrays.copyOfRange(x, (int)(0L), (int)((long)(i_5))).length) < (long)(min_leaf_size)) {
                i_5 = (long)(i_5);
            } else             if ((long)(java.util.Arrays.copyOfRange(x, (int)((long)(i_5)), (int)((long)(x.length))).length) < (long)(min_leaf_size)) {
                i_5 = (long)(i_5);
            } else {
                double err_left_1 = (double)(mean_squared_error(((double[])(java.util.Arrays.copyOfRange(x, (int)(0L), (int)((long)(i_5))))), (double)(mean(((double[])(java.util.Arrays.copyOfRange(y, (int)(0L), (int)((long)(i_5)))))))));
                double err_right_1 = (double)(mean_squared_error(((double[])(java.util.Arrays.copyOfRange(x, (int)((long)(i_5)), (int)((long)(x.length))))), (double)(mean(((double[])(java.util.Arrays.copyOfRange(y, (int)((long)(i_5)), (int)((long)(y.length)))))))));
                double err_1 = (double)((double)(err_left_1) + (double)(err_right_1));
                if ((double)(err_1) < (double)(min_error_1)) {
                    best_split_1 = (long)(i_5);
                    min_error_1 = (double)(err_1);
                }
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        if ((long)(best_split_1) != 0L) {
            double[] left_x_1 = ((double[])(java.util.Arrays.copyOfRange(x, (int)(0L), (int)((long)(best_split_1)))));
            double[] left_y_1 = ((double[])(java.util.Arrays.copyOfRange(y, (int)(0L), (int)((long)(best_split_1)))));
            double[] right_x_1 = ((double[])(java.util.Arrays.copyOfRange(x, (int)((long)(best_split_1)), (int)((long)(x.length)))));
            double[] right_y_1 = ((double[])(java.util.Arrays.copyOfRange(y, (int)((long)(best_split_1)), (int)((long)(y.length)))));
            double boundary_1 = (double)(x[(int)((long)(best_split_1))]);
            Tree left_tree_1 = train_tree(((double[])(left_x_1)), ((double[])(left_y_1)), (long)((long)(depth) - 1L), (long)(min_leaf_size));
            Tree right_tree_1 = train_tree(((double[])(right_x_1)), ((double[])(right_y_1)), (long)((long)(depth) - 1L), (long)(min_leaf_size));
            return ((Tree)(new Branch(boundary_1, left_tree_1, right_tree_1)));
        }
        return ((Tree)(new Leaf(mean(((double[])(y))))));
    }

    static double predict(Tree tree, double value) {
        return tree instanceof Leaf ? ((Leaf)(tree)).prediction : (double)(value) >= ((Number)(((Branch)(tree)).decision_boundary)).doubleValue() ? predict(((Branch)(tree)).right, (double)(value)) : predict(((Branch)(tree)).left, (double)(value));
    }

    static void main() {
        double[] x = ((double[])(new double[]{}));
        double v_1 = (double)(-1.0);
        while ((double)(v_1) < (double)(1.0)) {
            x = ((double[])(appendDouble(x, (double)(v_1))));
            v_1 = (double)((double)(v_1) + (double)(0.005));
        }
        double[] y_2 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(x.length)) {
            y_2 = ((double[])(appendDouble(y_2, (double)(sin((double)(x[(int)((long)(i_7))]))))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        Tree tree_1 = train_tree(((double[])(x)), ((double[])(y_2)), 10L, 10L);
        double[] test_cases_1 = ((double[])(new double[]{}));
        i_7 = 0L;
        while ((long)(i_7) < 10L) {
            test_cases_1 = ((double[])(appendDouble(test_cases_1, (double)((double)((double)(rand()) * (double)(2.0)) - (double)(1.0)))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        double[] predictions_1 = ((double[])(new double[]{}));
        i_7 = 0L;
        while ((long)(i_7) < (long)(test_cases_1.length)) {
            predictions_1 = ((double[])(appendDouble(predictions_1, (double)(predict(tree_1, (double)(test_cases_1[(int)((long)(i_7))]))))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        double sum_err_1 = (double)(0.0);
        i_7 = 0L;
        while ((long)(i_7) < (long)(test_cases_1.length)) {
            double diff_3 = (double)((double)(predictions_1[(int)((long)(i_7))]) - (double)(test_cases_1[(int)((long)(i_7))]));
            sum_err_1 = (double)((double)(sum_err_1) + (double)((double)(diff_3) * (double)(diff_3)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        double avg_error_1 = (double)((double)(sum_err_1) / (double)(test_cases_1.length));
        System.out.println("Test values: " + _p(test_cases_1));
        System.out.println("Predictions: " + _p(predictions_1));
        System.out.println("Average error: " + _p(avg_error_1));
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
