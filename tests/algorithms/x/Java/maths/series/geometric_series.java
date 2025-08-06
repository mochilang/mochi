public class Main {

    static double[] geometric_series(double nth_term, double start_term_a, double common_ratio_r) {
        int n = ((Number)(nth_term)).intValue();
        if (n <= 0 || start_term_a == 0.0 || common_ratio_r == 0.0) {
            return new double[]{};
        }
        double[] series = ((double[])(new double[]{}));
        double current = start_term_a;
        int i = 0;
        while (i < n) {
            series = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(series), java.util.stream.DoubleStream.of(current)).toArray()));
            current = current * common_ratio_r;
            i = i + 1;
        }
        return series;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(geometric_series(4.0, 2.0, 2.0));
            System.out.println(geometric_series(4.0, 2.0, -2.0));
            System.out.println(geometric_series(4.0, -2.0, 2.0));
            System.out.println(geometric_series(-4.0, 2.0, 2.0));
            System.out.println(geometric_series(0.0, 100.0, 500.0));
            System.out.println(geometric_series(1.0, 1.0, 1.0));
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
