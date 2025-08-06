public class Main {

    static boolean is_geometric_series(double[] series) {
        if (series.length == 0) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        if (series.length == 1) {
            return true;
        }
        if (series[0] == 0.0) {
            return false;
        }
        double ratio = series[1] / series[0];
        int i = 0;
        while (i < series.length - 1) {
            if (series[i] == 0.0) {
                return false;
            }
            if (series[i + 1] / series[i] != ratio) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static double geometric_mean(double[] series) {
        if (series.length == 0) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        double product = 1.0;
        int i_1 = 0;
        while (i_1 < series.length) {
            product = product * series[i_1];
            i_1 = i_1 + 1;
        }
        int n = series.length;
        return nth_root(product, n);
    }

    static double pow_float(double base, int exp) {
        double result = 1.0;
        int i_2 = 0;
        while (i_2 < exp) {
            result = result * base;
            i_2 = i_2 + 1;
        }
        return result;
    }

    static double nth_root(double value, int n) {
        if (value == 0.0) {
            return 0.0;
        }
        double low = 0.0;
        double high = value;
        if (value < 1.0) {
            high = 1.0;
        }
        double mid = (low + high) / 2.0;
        int i_3 = 0;
        while (i_3 < 40) {
            double mp = pow_float(mid, n);
            if (mp > value) {
                high = mid;
            } else {
                low = mid;
            }
            mid = (low + high) / 2.0;
            i_3 = i_3 + 1;
        }
        return mid;
    }

    static void test_geometric() {
        double[] a = ((double[])(new double[]{2.0, 4.0, 8.0}));
        if (!(Boolean)is_geometric_series(((double[])(a)))) {
            throw new RuntimeException(String.valueOf("expected geometric series"));
        }
        double[] b = ((double[])(new double[]{1.0, 2.0, 3.0}));
        if (((Boolean)(is_geometric_series(((double[])(b)))))) {
            throw new RuntimeException(String.valueOf("expected non geometric series"));
        }
    }

    static void main() {
        test_geometric();
        System.out.println(geometric_mean(((double[])(new double[]{2.0, 4.0, 8.0}))));
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
