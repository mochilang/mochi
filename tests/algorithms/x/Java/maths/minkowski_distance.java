public class Main {

    static double abs_val(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double pow_float(double base, long exp) {
        double result = 1.0;
        long i_1 = 0;
        while (i_1 < exp) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double nth_root(double value, long n) {
        if (value == 0.0) {
            return 0.0;
        }
        double x_1 = value / (((Number)(n)).doubleValue());
        long i_3 = 0;
        while (i_3 < 20) {
            double num_1 = (((Number)((n - 1))).doubleValue()) * x_1 + value / pow_float(x_1, n - 1);
            x_1 = num_1 / (((Number)(n)).doubleValue());
            i_3 = i_3 + 1;
        }
        return x_1;
    }

    static double minkowski_distance(double[] point_a, double[] point_b, long order) {
        if (order < 1) {
            throw new RuntimeException(String.valueOf("The order must be greater than or equal to 1."));
        }
        if (point_a.length != point_b.length) {
            throw new RuntimeException(String.valueOf("Both points must have the same dimension."));
        }
        double total_1 = 0.0;
        long idx_1 = 0;
        while (idx_1 < point_a.length) {
            double diff_1 = abs_val(point_a[(int)(idx_1)] - point_b[(int)(idx_1)]);
            total_1 = total_1 + pow_float(diff_1, order);
            idx_1 = idx_1 + 1;
        }
        return nth_root(total_1, order);
    }

    static void test_minkowski() {
        if (abs_val(minkowski_distance(((double[])(new double[]{1.0, 1.0})), ((double[])(new double[]{2.0, 2.0})), 1) - 2.0) > 0.0001) {
            throw new RuntimeException(String.valueOf("minkowski_distance test1 failed"));
        }
        if (abs_val(minkowski_distance(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})), ((double[])(new double[]{5.0, 6.0, 7.0, 8.0})), 2) - 8.0) > 0.0001) {
            throw new RuntimeException(String.valueOf("minkowski_distance test2 failed"));
        }
    }

    static void main() {
        test_minkowski();
        System.out.println(minkowski_distance(((double[])(new double[]{5.0})), ((double[])(new double[]{0.0})), 3));
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
}
