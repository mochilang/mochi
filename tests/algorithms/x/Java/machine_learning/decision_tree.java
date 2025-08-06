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
    static int seed = 0;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double sin(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y3 = y2 * y;
        double y5 = y3 * y2;
        double y7 = y5 * y2;
        return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0;
    }

    static double rand() {
        seed = ((int)(Math.floorMod(((long)((1103515245 * seed + 12345))), 2147483648L)));
        return ((Number)(seed)).doubleValue() / 2147483648.0;
    }

    static double mean(double[] vals) {
        double sum = 0.0;
        int i = 0;
        while (i < vals.length) {
            sum = sum + vals[i];
            i = i + 1;
        }
        return sum / vals.length;
    }

    static double mean_squared_error(double[] labels, double prediction) {
        double total = 0.0;
        int i_1 = 0;
        while (i_1 < labels.length) {
            double diff = labels[i_1] - prediction;
            total = total + diff * diff;
            i_1 = i_1 + 1;
        }
        return total / labels.length;
    }

    static Tree train_tree(double[] x, double[] y, int depth, int min_leaf_size) {
        if (x.length < 2 * min_leaf_size) {
            return new Leaf(mean(((double[])(y))));
        }
        if (depth == 1) {
            return new Leaf(mean(((double[])(y))));
        }
        int best_split = 0;
        double min_error = mean_squared_error(((double[])(x)), mean(((double[])(y)))) * 2.0;
        int i_2 = 0;
        while (i_2 < x.length) {
            if (java.util.Arrays.copyOfRange(x, 0, i_2).length < min_leaf_size) {
                i_2 = i_2;
            } else             if (java.util.Arrays.copyOfRange(x, i_2, x.length).length < min_leaf_size) {
                i_2 = i_2;
            } else {
                double err_left = mean_squared_error(((double[])(java.util.Arrays.copyOfRange(x, 0, i_2))), mean(((double[])(java.util.Arrays.copyOfRange(y, 0, i_2)))));
                double err_right = mean_squared_error(((double[])(java.util.Arrays.copyOfRange(x, i_2, x.length))), mean(((double[])(java.util.Arrays.copyOfRange(y, i_2, y.length)))));
                double err = err_left + err_right;
                if (err < min_error) {
                    best_split = i_2;
                    min_error = err;
                }
            }
            i_2 = i_2 + 1;
        }
        if (best_split != 0) {
            double[] left_x = ((double[])(java.util.Arrays.copyOfRange(x, 0, best_split)));
            double[] left_y = ((double[])(java.util.Arrays.copyOfRange(y, 0, best_split)));
            double[] right_x = ((double[])(java.util.Arrays.copyOfRange(x, best_split, x.length)));
            double[] right_y = ((double[])(java.util.Arrays.copyOfRange(y, best_split, y.length)));
            double boundary = x[best_split];
            Tree left_tree = train_tree(((double[])(left_x)), ((double[])(left_y)), depth - 1, min_leaf_size);
            Tree right_tree = train_tree(((double[])(right_x)), ((double[])(right_y)), depth - 1, min_leaf_size);
            return new Branch(boundary, left_tree, right_tree);
        }
        return new Leaf(mean(((double[])(y))));
    }

    static double predict(Tree tree, double value) {
        return tree instanceof Leaf ? ((Leaf)(tree)).prediction : value >= ((Number)(((Branch)(tree)).decision_boundary)).intValue() ? predict(((Branch)(tree)).right, value) : predict(((Branch)(tree)).left, value);
    }

    static void main() {
        double[] x = ((double[])(new double[]{}));
        double v = -1.0;
        while (v < 1.0) {
            x = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x), java.util.stream.DoubleStream.of(v)).toArray()));
            v = v + 0.005;
        }
        double[] y_1 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < x.length) {
            y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_1), java.util.stream.DoubleStream.of(sin(x[i_3]))).toArray()));
            i_3 = i_3 + 1;
        }
        Tree tree = train_tree(((double[])(x)), ((double[])(y_1)), 10, 10);
        double[] test_cases = ((double[])(new double[]{}));
        i_3 = 0;
        while (i_3 < 10) {
            test_cases = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(test_cases), java.util.stream.DoubleStream.of(rand() * 2.0 - 1.0)).toArray()));
            i_3 = i_3 + 1;
        }
        double[] predictions = ((double[])(new double[]{}));
        i_3 = 0;
        while (i_3 < test_cases.length) {
            predictions = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(predictions), java.util.stream.DoubleStream.of(predict(tree, test_cases[i_3]))).toArray()));
            i_3 = i_3 + 1;
        }
        double sum_err = 0.0;
        i_3 = 0;
        while (i_3 < test_cases.length) {
            double diff_1 = predictions[i_3] - test_cases[i_3];
            sum_err = sum_err + diff_1 * diff_1;
            i_3 = i_3 + 1;
        }
        double avg_error = sum_err / test_cases.length;
        System.out.println("Test values: " + _p(test_cases));
        System.out.println("Predictions: " + _p(predictions));
        System.out.println("Average error: " + _p(avg_error));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            TWO_PI = 6.283185307179586;
            seed = 123456789;
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
