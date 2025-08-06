public class Main {
    static double[][] X;
    static double[] Y;
    static double[][] test_data;
    static double[][] w1 = new double[0][];
    static double[] b1 = new double[0];
    static double[] w2 = new double[0];
    static double b2 = 0;
    static int[] preds_1;

    static double exp_taylor(double x) {
        double term = 1.0;
        double sum = 1.0;
        double i = 1.0;
        while (i < 20.0) {
            term = term * x / i;
            sum = sum + term;
            i = i + 1.0;
        }
        return sum;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + exp_taylor(-x));
    }

    static void train(int epochs, double lr) {
        int e = 0;
        while (e < epochs) {
            int i_1 = 0;
            while (i_1 < X.length) {
                double x0 = X[i_1][0];
                double x1 = X[i_1][1];
                double target = Y[i_1];
                double z1 = w1[0][0] * x0 + w1[1][0] * x1 + b1[0];
                double z2 = w1[0][1] * x0 + w1[1][1] * x1 + b1[1];
                double h1 = sigmoid(z1);
                double h2 = sigmoid(z2);
                double z3 = w2[0] * h1 + w2[1] * h2 + b2;
                double out = sigmoid(z3);
                double error = out - target;
                double d1 = h1 * (1.0 - h1) * w2[0] * error;
                double d2 = h2 * (1.0 - h2) * w2[1] * error;
w2[0] = w2[0] - lr * error * h1;
w2[1] = w2[1] - lr * error * h2;
                b2 = b2 - lr * error;
w1[0][0] = w1[0][0] - lr * d1 * x0;
w1[1][0] = w1[1][0] - lr * d1 * x1;
b1[0] = b1[0] - lr * d1;
w1[0][1] = w1[0][1] - lr * d2 * x0;
w1[1][1] = w1[1][1] - lr * d2 * x1;
b1[1] = b1[1] - lr * d2;
                i_1 = i_1 + 1;
            }
            e = e + 1;
        }
    }

    static int[] predict(double[][] samples) {
        int[] preds = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < samples.length) {
            double x0_1 = samples[i_2][0];
            double x1_1 = samples[i_2][1];
            double z1_1 = w1[0][0] * x0_1 + w1[1][0] * x1_1 + b1[0];
            double z2_1 = w1[0][1] * x0_1 + w1[1][1] * x1_1 + b1[1];
            double h1_1 = sigmoid(z1_1);
            double h2_1 = sigmoid(z2_1);
            double z3_1 = w2[0] * h1_1 + w2[1] * h2_1 + b2;
            double out_1 = sigmoid(z3_1);
            int label = 0;
            if (out_1 >= 0.5) {
                label = 1;
            }
            preds = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(preds), java.util.stream.IntStream.of(label)).toArray()));
            i_2 = i_2 + 1;
        }
        return preds;
    }

    static int[] wrapper(int[] y) {
        return y;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            X = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{1.0, 1.0}, new double[]{1.0, 0.0}, new double[]{0.0, 1.0}}));
            Y = ((double[])(new double[]{0.0, 1.0, 0.0, 0.0}));
            test_data = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 1.0}, new double[]{1.0, 1.0}}));
            w1 = ((double[][])(new double[][]{new double[]{0.5, -0.5}, new double[]{0.5, 0.5}}));
            b1 = ((double[])(new double[]{0.0, 0.0}));
            w2 = ((double[])(new double[]{0.5, -0.5}));
            b2 = 0.0;
            train(4000, 0.5);
            preds_1 = ((int[])(wrapper(((int[])(predict(((double[][])(test_data))))))));
            System.out.println(_p(preds_1));
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
