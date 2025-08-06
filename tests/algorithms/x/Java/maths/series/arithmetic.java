public class Main {

    static boolean is_arithmetic_series(double[] xs) {
        if (xs.length == 0) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        if (xs.length == 1) {
            return true;
        }
        double diff = xs[1] - xs[0];
        int i = 0;
        while (i < xs.length - 1) {
            if (xs[i + 1] - xs[i] != diff) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static double arithmetic_mean(double[] xs) {
        if (xs.length == 0) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        double total = 0.0;
        int i_1 = 0;
        while (i_1 < xs.length) {
            total = total + xs[i_1];
            i_1 = i_1 + 1;
        }
        return total / (((Number)(xs.length)).doubleValue());
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(is_arithmetic_series(((double[])(new double[]{2.0, 4.0, 6.0})))));
            System.out.println(_p(is_arithmetic_series(((double[])(new double[]{3.0, 6.0, 12.0, 24.0})))));
            System.out.println(_p(arithmetic_mean(((double[])(new double[]{2.0, 4.0, 6.0})))));
            System.out.println(_p(arithmetic_mean(((double[])(new double[]{3.0, 6.0, 9.0, 12.0})))));
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
