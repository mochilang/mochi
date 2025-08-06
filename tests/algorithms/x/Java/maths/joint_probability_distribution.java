public class Main {

    static String key(int x, int y) {
        return _p(x) + "," + _p(y);
    }

    static java.util.Map<String,Double> joint_probability_distribution(int[] x_values, int[] y_values, double[] x_probabilities, double[] y_probabilities) {
        java.util.Map<String,Double> result = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        int i = 0;
        while (i < x_values.length) {
            int j = 0;
            while (j < y_values.length) {
                String k = String.valueOf(key(x_values[i], y_values[j]));
result.put(k, x_probabilities[i] * y_probabilities[j]);
                j = j + 1;
            }
            i = i + 1;
        }
        return result;
    }

    static double expectation(int[] values, double[] probabilities) {
        double total = 0.0;
        int i_1 = 0;
        while (i_1 < values.length) {
            total = total + (((double)(values[i_1]))) * probabilities[i_1];
            i_1 = i_1 + 1;
        }
        return total;
    }

    static double variance(int[] values, double[] probabilities) {
        double mean = expectation(((int[])(values)), ((double[])(probabilities)));
        double total_1 = 0.0;
        int i_2 = 0;
        while (i_2 < values.length) {
            double diff = (((double)(values[i_2]))) - mean;
            total_1 = total_1 + diff * diff * probabilities[i_2];
            i_2 = i_2 + 1;
        }
        return total_1;
    }

    static double covariance(int[] x_values, int[] y_values, double[] x_probabilities, double[] y_probabilities) {
        double mean_x = expectation(((int[])(x_values)), ((double[])(x_probabilities)));
        double mean_y = expectation(((int[])(y_values)), ((double[])(y_probabilities)));
        double total_2 = 0.0;
        int i_3 = 0;
        while (i_3 < x_values.length) {
            int j_1 = 0;
            while (j_1 < y_values.length) {
                double diff_x = (((double)(x_values[i_3]))) - mean_x;
                double diff_y = (((double)(y_values[j_1]))) - mean_y;
                total_2 = total_2 + diff_x * diff_y * x_probabilities[i_3] * y_probabilities[j_1];
                j_1 = j_1 + 1;
            }
            i_3 = i_3 + 1;
        }
        return total_2;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i_4 = 0;
        while (i_4 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_4 = i_4 + 1;
        }
        return guess;
    }

    static double standard_deviation(double v) {
        return sqrtApprox(v);
    }

    static void main() {
        int[] x_values = ((int[])(new int[]{1, 2}));
        int[] y_values = ((int[])(new int[]{-2, 5, 8}));
        double[] x_probabilities = ((double[])(new double[]{0.7, 0.3}));
        double[] y_probabilities = ((double[])(new double[]{0.3, 0.5, 0.2}));
        java.util.Map<String,Double> jpd = joint_probability_distribution(((int[])(x_values)), ((int[])(y_values)), ((double[])(x_probabilities)), ((double[])(y_probabilities)));
        int i_5 = 0;
        while (i_5 < x_values.length) {
            int j_2 = 0;
            while (j_2 < y_values.length) {
                String k_1 = String.valueOf(key(x_values[i_5], y_values[j_2]));
                double prob = (double)(((double)(jpd).getOrDefault(k_1, 0.0)));
                System.out.println(k_1 + "=" + _p(prob));
                j_2 = j_2 + 1;
            }
            i_5 = i_5 + 1;
        }
        double ex = expectation(((int[])(x_values)), ((double[])(x_probabilities)));
        double ey = expectation(((int[])(y_values)), ((double[])(y_probabilities)));
        double vx = variance(((int[])(x_values)), ((double[])(x_probabilities)));
        double vy = variance(((int[])(y_values)), ((double[])(y_probabilities)));
        double cov = covariance(((int[])(x_values)), ((int[])(y_values)), ((double[])(x_probabilities)), ((double[])(y_probabilities)));
        System.out.println("Ex=" + _p(ex));
        System.out.println("Ey=" + _p(ey));
        System.out.println("Vx=" + _p(vx));
        System.out.println("Vy=" + _p(vy));
        System.out.println("Cov=" + _p(cov));
        System.out.println("Sx=" + _p(standard_deviation(vx)));
        System.out.println("Sy=" + _p(standard_deviation(vy)));
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
