public class Main {
    static int i_1 = 0;

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static double trapezoidal_area(java.util.function.Function<Double,Double> f, double x_start, double x_end, int steps) {
        double step = (x_end - x_start) / (((Number)(steps)).doubleValue());
        double x1 = x_start;
        double fx1 = f.apply(x_start);
        double area = 0.0;
        int i = 0;
        while (i < steps) {
            double x2 = x1 + step;
            double fx2 = f.apply(x2);
            area = area + abs_float(fx2 + fx1) * step / 2.0;
            x1 = x2;
            fx1 = fx2;
            i = i + 1;
        }
        return area;
    }

    static double f(double x) {
        return x * x * x + x * x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("f(x) = x^3 + x^2");
            System.out.println("The area between the curve, x = -5, x = 5 and the x axis is:");
            i_1 = 10;
            while (i_1 <= 100000) {
                double result = trapezoidal_area(Main::f, -5.0, 5.0, i_1);
                System.out.println("with " + _p(i_1) + " steps: " + _p(result));
                i_1 = i_1 * 10;
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
