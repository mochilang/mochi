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
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double ln_series(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_1 = t;
        double sum_1 = 0.0;
        long n_1 = 1;
        while (n_1 <= 19) {
            sum_1 = sum_1 + term_1 / (((Number)(n_1)).doubleValue());
            term_1 = term_1 * t * t;
            n_1 = n_1 + 2;
        }
        return 2.0 * sum_1;
    }

    static double ln(double x) {
        double y = x;
        long k_1 = 0;
        while (y >= 10.0) {
            y = y / 10.0;
            k_1 = k_1 + 1;
        }
        while (y < 1.0) {
            y = y * 10.0;
            k_1 = k_1 - 1;
        }
        return ln_series(y) + (((Number)(k_1)).doubleValue()) * ln_series(10.0);
    }

    static double mae(double[] predict, double[] actual) {
        double sum_2 = 0.0;
        long i_3 = 0;
        while (i_3 < predict.length) {
            double diff_1 = predict[(int)(i_3)] - actual[(int)(i_3)];
            sum_2 = sum_2 + absf(diff_1);
            i_3 = i_3 + 1;
        }
        return sum_2 / (((Number)(predict.length)).doubleValue());
    }

    static double mse(double[] predict, double[] actual) {
        double sum_3 = 0.0;
        long i_5 = 0;
        while (i_5 < predict.length) {
            double diff_3 = predict[(int)(i_5)] - actual[(int)(i_5)];
            sum_3 = sum_3 + diff_3 * diff_3;
            i_5 = i_5 + 1;
        }
        return sum_3 / (((Number)(predict.length)).doubleValue());
    }

    static double rmse(double[] predict, double[] actual) {
        return sqrtApprox(mse(((double[])(predict)), ((double[])(actual))));
    }

    static double rmsle(double[] predict, double[] actual) {
        double sum_4 = 0.0;
        long i_7 = 0;
        while (i_7 < predict.length) {
            double lp_1 = ln(predict[(int)(i_7)] + 1.0);
            double la_1 = ln(actual[(int)(i_7)] + 1.0);
            double diff_5 = lp_1 - la_1;
            sum_4 = sum_4 + diff_5 * diff_5;
            i_7 = i_7 + 1;
        }
        return sqrtApprox(sum_4 / (((Number)(predict.length)).doubleValue()));
    }

    static double mbd(double[] predict, double[] actual) {
        double diff_sum = 0.0;
        double actual_sum_1 = 0.0;
        long i_9 = 0;
        while (i_9 < predict.length) {
            diff_sum = diff_sum + (predict[(int)(i_9)] - actual[(int)(i_9)]);
            actual_sum_1 = actual_sum_1 + actual[(int)(i_9)];
            i_9 = i_9 + 1;
        }
        double n_3 = ((Number)(predict.length)).doubleValue();
        double numerator_1 = diff_sum / n_3;
        double denominator_1 = actual_sum_1 / n_3;
        return numerator_1 / denominator_1 * 100.0;
    }

    static double manual_accuracy(double[] predict, double[] actual) {
        long correct = 0;
        long i_11 = 0;
        while (i_11 < predict.length) {
            if (predict[(int)(i_11)] == actual[(int)(i_11)]) {
                correct = correct + 1;
            }
            i_11 = i_11 + 1;
        }
        return (((Number)(correct)).doubleValue()) / (((Number)(predict.length)).doubleValue());
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
