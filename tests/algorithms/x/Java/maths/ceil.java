public class Main {
    static double[] values;

    static long ceil(double x) {
        long truncated = (long)(((Number)(x)).intValue());
        double frac_1 = (double)((double)(x) - (double)((((Number)(truncated)).doubleValue())));
        if ((double)(frac_1) <= (double)(0.0)) {
            return truncated;
        }
        return (long)(truncated) + 1L;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            values = ((double[])(new double[]{1.0, -1.0, 0.0, -0.0, 1.1, -1.1, 1.0, -1.0, 1000000000.0}));
            for (double v : values) {
                System.out.println(ceil((double)(v)));
            }
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
