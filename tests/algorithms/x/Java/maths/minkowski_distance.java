public class Main {

    static double abs_val(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double pow_float(double base, int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double nth_root(double value, int n) {
        if (value == 0.0) {
            return 0.0;
        }
        double x = value / (((Number)(n)).doubleValue());
        int i_1 = 0;
        while (i_1 < 20) {
            double num = (((Number)((n - 1))).doubleValue()) * x + value / pow_float(x, n - 1);
            x = num / (((Number)(n)).doubleValue());
            i_1 = i_1 + 1;
        }
        return x;
    }

    static double minkowski_distance(double[] point_a, double[] point_b, int order) {
        if (order < 1) {
            throw new RuntimeException(String.valueOf("The order must be greater than or equal to 1."));
        }
        if (point_a.length != point_b.length) {
            throw new RuntimeException(String.valueOf("Both points must have the same dimension."));
        }
        double total = 0.0;
        int idx = 0;
        while (idx < point_a.length) {
            double diff = abs_val(point_a[idx] - point_b[idx]);
            total = total + pow_float(diff, order);
            idx = idx + 1;
        }
        return nth_root(total, order);
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
