public class Main {
    static double PI;

    static double radians(double degree) {
        return degree / (180.0 / PI);
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static boolean almost_equal(double a, double b) {
        return abs_float(a - b) <= 1e-08;
    }

    static void test_radians() {
        if (!(Boolean)almost_equal(radians(180.0), PI)) {
            throw new RuntimeException(String.valueOf("radians 180 failed"));
        }
        if (!(Boolean)almost_equal(radians(92.0), 1.6057029118347832)) {
            throw new RuntimeException(String.valueOf("radians 92 failed"));
        }
        if (!(Boolean)almost_equal(radians(274.0), 4.782202150464463)) {
            throw new RuntimeException(String.valueOf("radians 274 failed"));
        }
        if (!(Boolean)almost_equal(radians(109.82), 1.9167205845401725)) {
            throw new RuntimeException(String.valueOf("radians 109.82 failed"));
        }
    }

    static void main() {
        test_radians();
        System.out.println(_p(radians(180.0)));
        System.out.println(_p(radians(92.0)));
        System.out.println(_p(radians(274.0)));
        System.out.println(_p(radians(109.82)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
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
        return String.valueOf(v);
    }
}
