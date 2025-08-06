public class Main {

    static double abs(double x) {
        if (x >= 0.0) {
            return x;
        } else {
            return -x;
        }
    }

    static double chebyshev_distance(double[] point_a, double[] point_b) {
        if (point_a.length != point_b.length) {
            throw new RuntimeException(String.valueOf("Both points must have the same dimension."));
        }
        double max_diff = 0.0;
        int i = 0;
        while (i < point_a.length) {
            double diff = ((Number)(Math.abs(point_a[i] - point_b[i]))).doubleValue();
            if (diff > max_diff) {
                max_diff = diff;
            }
            i = i + 1;
        }
        return max_diff;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(chebyshev_distance(((double[])(new double[]{1.0, 1.0})), ((double[])(new double[]{2.0, 2.0}))));
            System.out.println(chebyshev_distance(((double[])(new double[]{1.0, 1.0, 9.0})), ((double[])(new double[]{2.0, 2.0, -5.2}))));
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
