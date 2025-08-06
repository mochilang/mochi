public class Main {
    static double result;

    static double f(double x) {
        return (x - 0.0) * (x - 0.0);
    }

    static double[] make_points(double a, double b, double h) {
        double[] points = ((double[])(new double[]{}));
        double x = a + h;
        while (x < (b - h)) {
            points = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(points), java.util.stream.DoubleStream.of(x)).toArray()));
            x = x + h;
        }
        return points;
    }

    static double simpson_rule(double[] boundary, int steps) {
        if (steps <= 0) {
            throw new RuntimeException(String.valueOf("Number of steps must be greater than zero"));
        }
        double a = boundary[0];
        double b = boundary[1];
        double h = (b - a) / (((Number)(steps)).doubleValue());
        double[] pts = ((double[])(make_points(a, b, h)));
        double y = (h / 3.0) * f(a);
        int cnt = 2;
        int i = 0;
        while (i < pts.length) {
            double coeff = 4.0 - 2.0 * (((Number)((Math.floorMod(cnt, 2)))).doubleValue());
            y = y + (h / 3.0) * coeff * f(pts[i]);
            cnt = cnt + 1;
            i = i + 1;
        }
        y = y + (h / 3.0) * f(b);
        return y;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            result = simpson_rule(((double[])(new double[]{0.0, 1.0})), 10);
            System.out.println(_p(result));
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
