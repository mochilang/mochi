public class Main {

    static String key(long x, long y) {
        return _p(x) + "," + _p(y);
    }

    static java.util.Map<String,Double> joint_probability_distribution(long[] x_values, long[] y_values, double[] x_probabilities, double[] y_probabilities) {
        java.util.Map<String,Double> result = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        long i_1 = 0;
        while (i_1 < x_values.length) {
            long j_1 = 0;
            while (j_1 < y_values.length) {
                String k_1 = String.valueOf(key(x_values[(int)(i_1)], y_values[(int)(j_1)]));
result.put(k_1, x_probabilities[(int)(i_1)] * y_probabilities[(int)(j_1)]);
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double expectation(long[] values, double[] probabilities) {
        double total = 0.0;
        long i_3 = 0;
        while (i_3 < values.length) {
            total = total + (((double)(values[(int)(i_3)]))) * probabilities[(int)(i_3)];
            i_3 = i_3 + 1;
        }
        return total;
    }

    static double variance(long[] values, double[] probabilities) {
        double mean = expectation(((long[])(values)), ((double[])(probabilities)));
        double total_2 = 0.0;
        long i_5 = 0;
        while (i_5 < values.length) {
            double diff_1 = (((double)(values[(int)(i_5)]))) - mean;
            total_2 = total_2 + diff_1 * diff_1 * probabilities[(int)(i_5)];
            i_5 = i_5 + 1;
        }
        return total_2;
    }

    static double covariance(long[] x_values, long[] y_values, double[] x_probabilities, double[] y_probabilities) {
        double mean_x = expectation(((long[])(x_values)), ((double[])(x_probabilities)));
        double mean_y_1 = expectation(((long[])(y_values)), ((double[])(y_probabilities)));
        double total_4 = 0.0;
        long i_7 = 0;
        while (i_7 < x_values.length) {
            long j_3 = 0;
            while (j_3 < y_values.length) {
                double diff_x_1 = (((double)(x_values[(int)(i_7)]))) - mean_x;
                double diff_y_1 = (((double)(y_values[(int)(j_3)]))) - mean_y_1;
                total_4 = total_4 + diff_x_1 * diff_y_1 * x_probabilities[(int)(i_7)] * y_probabilities[(int)(j_3)];
                j_3 = j_3 + 1;
            }
            i_7 = i_7 + 1;
        }
        return total_4;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x / 2.0;
        long i_9 = 0;
        while (i_9 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_9 = i_9 + 1;
        }
        return guess_1;
    }

    static double standard_deviation(double v) {
        return sqrtApprox(v);
    }

    static void main() {
        long[] x_values = ((long[])(new long[]{1, 2}));
        long[] y_values_1 = ((long[])(new long[]{-2, 5, 8}));
        double[] x_probabilities_1 = ((double[])(new double[]{0.7, 0.3}));
        double[] y_probabilities_1 = ((double[])(new double[]{0.3, 0.5, 0.2}));
        java.util.Map<String,Double> jpd_1 = joint_probability_distribution(((long[])(x_values)), ((long[])(y_values_1)), ((double[])(x_probabilities_1)), ((double[])(y_probabilities_1)));
        long i_11 = 0;
        while (i_11 < x_values.length) {
            long j_5 = 0;
            while (j_5 < y_values_1.length) {
                String k_3 = String.valueOf(key(x_values[(int)(i_11)], y_values_1[(int)(j_5)]));
                double prob_1 = (double)(((double)(jpd_1).getOrDefault(k_3, 0.0)));
                System.out.println(k_3 + "=" + _p(prob_1));
                j_5 = j_5 + 1;
            }
            i_11 = i_11 + 1;
        }
        double ex_1 = expectation(((long[])(x_values)), ((double[])(x_probabilities_1)));
        double ey_1 = expectation(((long[])(y_values_1)), ((double[])(y_probabilities_1)));
        double vx_1 = variance(((long[])(x_values)), ((double[])(x_probabilities_1)));
        double vy_1 = variance(((long[])(y_values_1)), ((double[])(y_probabilities_1)));
        double cov_1 = covariance(((long[])(x_values)), ((long[])(y_values_1)), ((double[])(x_probabilities_1)), ((double[])(y_probabilities_1)));
        System.out.println("Ex=" + _p(ex_1));
        System.out.println("Ey=" + _p(ey_1));
        System.out.println("Vx=" + _p(vx_1));
        System.out.println("Vy=" + _p(vy_1));
        System.out.println("Cov=" + _p(cov_1));
        System.out.println("Sx=" + _p(standard_deviation(vx_1)));
        System.out.println("Sy=" + _p(standard_deviation(vy_1)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
