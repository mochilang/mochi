public class Main {

    static double absf(double x) {
        if (x < 0.0) {
            return 0.0 - x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double ln_series(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term = t;
        double sum = 0.0;
        int n = 1;
        while (n <= 19) {
            sum = sum + term / (((Number)(n)).doubleValue());
            term = term * t * t;
            n = n + 2;
        }
        return 2.0 * sum;
    }

    static double ln(double x) {
        double y = x;
        int k = 0;
        while (y >= 10.0) {
            y = y / 10.0;
            k = k + 1;
        }
        while (y < 1.0) {
            y = y * 10.0;
            k = k - 1;
        }
        return ln_series(y) + (((Number)(k)).doubleValue()) * ln_series(10.0);
    }

    static double mae(double[] predict, double[] actual) {
        double sum_1 = 0.0;
        int i_1 = 0;
        while (i_1 < predict.length) {
            double diff = predict[i_1] - actual[i_1];
            sum_1 = sum_1 + absf(diff);
            i_1 = i_1 + 1;
        }
        return sum_1 / (((Number)(predict.length)).doubleValue());
    }

    static double mse(double[] predict, double[] actual) {
        double sum_2 = 0.0;
        int i_2 = 0;
        while (i_2 < predict.length) {
            double diff_1 = predict[i_2] - actual[i_2];
            sum_2 = sum_2 + diff_1 * diff_1;
            i_2 = i_2 + 1;
        }
        return sum_2 / (((Number)(predict.length)).doubleValue());
    }

    static double rmse(double[] predict, double[] actual) {
        return sqrtApprox(mse(((double[])(predict)), ((double[])(actual))));
    }

    static double rmsle(double[] predict, double[] actual) {
        double sum_3 = 0.0;
        int i_3 = 0;
        while (i_3 < predict.length) {
            double lp = ln(predict[i_3] + 1.0);
            double la = ln(actual[i_3] + 1.0);
            double diff_2 = lp - la;
            sum_3 = sum_3 + diff_2 * diff_2;
            i_3 = i_3 + 1;
        }
        return sqrtApprox(sum_3 / (((Number)(predict.length)).doubleValue()));
    }

    static double mbd(double[] predict, double[] actual) {
        double diff_sum = 0.0;
        double actual_sum = 0.0;
        int i_4 = 0;
        while (i_4 < predict.length) {
            diff_sum = diff_sum + (predict[i_4] - actual[i_4]);
            actual_sum = actual_sum + actual[i_4];
            i_4 = i_4 + 1;
        }
        double n_1 = ((Number)(predict.length)).doubleValue();
        double numerator = diff_sum / n_1;
        double denominator = actual_sum / n_1;
        return numerator / denominator * 100.0;
    }

    static double manual_accuracy(double[] predict, double[] actual) {
        int correct = 0;
        int i_5 = 0;
        while (i_5 < predict.length) {
            if (predict[i_5] == actual[i_5]) {
                correct = correct + 1;
            }
            i_5 = i_5 + 1;
        }
        return (((Number)(correct)).doubleValue()) / (((Number)(predict.length)).doubleValue());
    }

    static void main() {
        double[] actual = ((double[])(new double[]{1.0, 2.0, 3.0}));
        double[] predict = ((double[])(new double[]{1.0, 4.0, 3.0}));
        System.out.println(_p(mae(((double[])(predict)), ((double[])(actual)))));
        System.out.println(_p(mse(((double[])(predict)), ((double[])(actual)))));
        System.out.println(_p(rmse(((double[])(predict)), ((double[])(actual)))));
        System.out.println(_p(rmsle(((double[])(new double[]{10.0, 2.0, 30.0})), ((double[])(new double[]{10.0, 10.0, 30.0})))));
        System.out.println(_p(mbd(((double[])(new double[]{2.0, 3.0, 4.0})), ((double[])(new double[]{1.0, 2.0, 3.0})))));
        System.out.println(_p(mbd(((double[])(new double[]{0.0, 1.0, 1.0})), ((double[])(new double[]{1.0, 2.0, 3.0})))));
        System.out.println(_p(manual_accuracy(((double[])(predict)), ((double[])(actual)))));
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
