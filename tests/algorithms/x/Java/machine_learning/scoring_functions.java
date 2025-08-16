public class Main {

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(0.0) - (double)(x);
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double ln_series(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_1 = (double)(t);
        double sum_1 = (double)(0.0);
        long n_1 = 1L;
        while ((long)(n_1) <= 19L) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)((((Number)(n_1)).doubleValue()))));
            term_1 = (double)((double)((double)(term_1) * (double)(t)) * (double)(t));
            n_1 = (long)((long)(n_1) + 2L);
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double ln(double x) {
        double y = (double)(x);
        long k_1 = 0L;
        while ((double)(y) >= (double)(10.0)) {
            y = (double)((double)(y) / (double)(10.0));
            k_1 = (long)((long)(k_1) + 1L);
        }
        while ((double)(y) < (double)(1.0)) {
            y = (double)((double)(y) * (double)(10.0));
            k_1 = (long)((long)(k_1) - 1L);
        }
        return (double)(ln_series((double)(y))) + (double)((double)((((Number)(k_1)).doubleValue())) * (double)(ln_series((double)(10.0))));
    }

    static double mae(double[] predict, double[] actual) {
        double sum_2 = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(predict.length)) {
            double diff_1 = (double)((double)(predict[(int)((long)(i_3))]) - (double)(actual[(int)((long)(i_3))]));
            sum_2 = (double)((double)(sum_2) + (double)(absf((double)(diff_1))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(sum_2) / (double)((((Number)(predict.length)).doubleValue()));
    }

    static double mse(double[] predict, double[] actual) {
        double sum_3 = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(predict.length)) {
            double diff_3 = (double)((double)(predict[(int)((long)(i_5))]) - (double)(actual[(int)((long)(i_5))]));
            sum_3 = (double)((double)(sum_3) + (double)((double)(diff_3) * (double)(diff_3)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return (double)(sum_3) / (double)((((Number)(predict.length)).doubleValue()));
    }

    static double rmse(double[] predict, double[] actual) {
        return sqrtApprox((double)(mse(((double[])(predict)), ((double[])(actual)))));
    }

    static double rmsle(double[] predict, double[] actual) {
        double sum_4 = (double)(0.0);
        long i_7 = 0L;
        while ((long)(i_7) < (long)(predict.length)) {
            double lp_1 = (double)(ln((double)((double)(predict[(int)((long)(i_7))]) + (double)(1.0))));
            double la_1 = (double)(ln((double)((double)(actual[(int)((long)(i_7))]) + (double)(1.0))));
            double diff_5 = (double)((double)(lp_1) - (double)(la_1));
            sum_4 = (double)((double)(sum_4) + (double)((double)(diff_5) * (double)(diff_5)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return sqrtApprox((double)((double)(sum_4) / (double)((((Number)(predict.length)).doubleValue()))));
    }

    static double mbd(double[] predict, double[] actual) {
        double diff_sum = (double)(0.0);
        double actual_sum_1 = (double)(0.0);
        long i_9 = 0L;
        while ((long)(i_9) < (long)(predict.length)) {
            diff_sum = (double)((double)(diff_sum) + (double)(((double)(predict[(int)((long)(i_9))]) - (double)(actual[(int)((long)(i_9))]))));
            actual_sum_1 = (double)((double)(actual_sum_1) + (double)(actual[(int)((long)(i_9))]));
            i_9 = (long)((long)(i_9) + 1L);
        }
        double n_3 = (double)(((Number)(predict.length)).doubleValue());
        double numerator_1 = (double)((double)(diff_sum) / (double)(n_3));
        double denominator_1 = (double)((double)(actual_sum_1) / (double)(n_3));
        return (double)((double)(numerator_1) / (double)(denominator_1)) * (double)(100.0);
    }

    static double manual_accuracy(double[] predict, double[] actual) {
        long correct = 0L;
        long i_11 = 0L;
        while ((long)(i_11) < (long)(predict.length)) {
            if ((double)(predict[(int)((long)(i_11))]) == (double)(actual[(int)((long)(i_11))])) {
                correct = (long)((long)(correct) + 1L);
            }
            i_11 = (long)((long)(i_11) + 1L);
        }
        return (double)((((Number)(correct)).doubleValue())) / (double)((((Number)(predict.length)).doubleValue()));
    }

    static void main() {
        double[] actual = ((double[])(new double[]{1.0, 2.0, 3.0}));
        double[] predict_1 = ((double[])(new double[]{1.0, 4.0, 3.0}));
        System.out.println(_p(mae(((double[])(predict_1)), ((double[])(actual)))));
        System.out.println(_p(mse(((double[])(predict_1)), ((double[])(actual)))));
        System.out.println(_p(rmse(((double[])(predict_1)), ((double[])(actual)))));
        System.out.println(_p(rmsle(((double[])(new double[]{10.0, 2.0, 30.0})), ((double[])(new double[]{10.0, 10.0, 30.0})))));
        System.out.println(_p(mbd(((double[])(new double[]{2.0, 3.0, 4.0})), ((double[])(new double[]{1.0, 2.0, 3.0})))));
        System.out.println(_p(mbd(((double[])(new double[]{0.0, 1.0, 1.0})), ((double[])(new double[]{1.0, 2.0, 3.0})))));
        System.out.println(_p(manual_accuracy(((double[])(predict_1)), ((double[])(actual)))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
