public class Main {
    static double r1;
    static double r2;
    static double r3;

    static double fx(double x, double a) {
        return x * x - a;
    }

    static double fx_derivative(double x) {
        return 2.0 * x;
    }

    static double get_initial_point(double a) {
        double start = 2.0;
        while (start <= a) {
            start = start * start;
        }
        return start;
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double square_root_iterative(double a, int max_iter, double tolerance) {
        if (a < 0.0) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        double value = get_initial_point(a);
        int i = 0;
        while (i < max_iter) {
            double prev_value = value;
            value = value - fx(value, a) / fx_derivative(value);
            if (abs_float(prev_value - value) < tolerance) {
                return value;
            }
            i = i + 1;
        }
        return value;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = square_root_iterative(4.0, 9999, 1e-14);
            System.out.println(_p(r1));
            r2 = square_root_iterative(3.2, 9999, 1e-14);
            System.out.println(_p(r2));
            r3 = square_root_iterative(140.0, 9999, 1e-14);
            System.out.println(_p(r3));
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
