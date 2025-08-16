public class Main {
    static double PI = (double)(3.141592653589793);
    static double TWO_PI = (double)(6.283185307179586);

    static double _mod(double x, double m) {
        return (double)(x) - (double)(Math.floor((double)(x) / (double)(m)) * (double)(m));
    }

    static double cos(double x) {
        double y = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_1 = (double)((double)(y) * (double)(y));
        double y4_1 = (double)((double)(y2_1) * (double)(y2_1));
        double y6_1 = (double)((double)(y4_1) * (double)(y2_1));
        return (double)((double)((double)(1.0) - (double)((double)(y2_1) / (double)(2.0))) + (double)((double)(y4_1) / (double)(24.0))) - (double)((double)(y6_1) / (double)(720.0));
    }

    static double radians(double deg) {
        return (double)((double)(deg) * (double)(PI)) / (double)(180.0);
    }

    static double abs_val(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double malus_law(double initial_intensity, double angle) {
        if ((double)(initial_intensity) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The value of intensity cannot be negative"));
        }
        if ((double)(angle) < (double)(0.0) || (double)(angle) > (double)(360.0)) {
            throw new RuntimeException(String.valueOf("In Malus Law, the angle is in the range 0-360 degrees"));
        }
        double theta_1 = (double)(radians((double)(angle)));
        double c_1 = (double)(cos((double)(theta_1)));
        return (double)(initial_intensity) * (double)(((double)(c_1) * (double)(c_1)));
    }

    static void main() {
        System.out.println(_p(malus_law((double)(100.0), (double)(60.0))));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
