public class Main {

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static double bisection(java.util.function.Function<Double,Double> f, double a, double b) {
        double start = a;
        double end_1 = b;
        if (f.apply(a) == 0.0) {
            return a;
        }
        if (f.apply(b) == 0.0) {
            return b;
        }
        if (f.apply(a) * f.apply(b) > 0.0) {
            throw new RuntimeException(String.valueOf("could not find root in given interval."));
        }
        double mid_1 = start + (end_1 - start) / 2.0;
        while (abs_float(start - mid_1) > 1e-07) {
            double fmid_1 = f.apply(mid_1);
            if (fmid_1 == 0.0) {
                return mid_1;
            }
            if (fmid_1 * f.apply(start) < 0.0) {
                end_1 = mid_1;
            } else {
                start = mid_1;
            }
            mid_1 = start + (end_1 - start) / 2.0;
        }
        return mid_1;
    }

    static double f(double x) {
        return x * x * x - 2.0 * x - 5.0;
    }

    static void main() {
        System.out.println(_p(bisection(Main::f, 1.0, 1000.0)));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
