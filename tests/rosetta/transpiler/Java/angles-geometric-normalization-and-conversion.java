public class Main {

    static double d2d(double d) {
        return d % 360.0;
    }

    static double g2g(double g) {
        return g % 400.0;
    }

    static double m2m(double m) {
        return m % 6400.0;
    }

    static double r2r(double r) {
        return r % (2.0 * 3.141592653589793);
    }

    static double d2g(double d) {
        return d2d(d) * 400.0 / 360.0;
    }

    static double d2m(double d) {
        return d2d(d) * 6400.0 / 360.0;
    }

    static double d2r(double d) {
        return d2d(d) * 3.141592653589793 / 180.0;
    }

    static double g2d(double g) {
        return g2g(g) * 360.0 / 400.0;
    }

    static double g2m(double g) {
        return g2g(g) * 6400.0 / 400.0;
    }

    static double g2r(double g) {
        return g2g(g) * 3.141592653589793 / 200.0;
    }

    static double m2d(double m) {
        return m2m(m) * 360.0 / 6400.0;
    }

    static double m2g(double m) {
        return m2m(m) * 400.0 / 6400.0;
    }

    static double m2r(double m) {
        return m2m(m) * 3.141592653589793 / 3200.0;
    }

    static double r2d(double r) {
        return r2r(r) * 180.0 / 3.141592653589793;
    }

    static double r2g(double r) {
        return r2r(r) * 200.0 / 3.141592653589793;
    }

    static double r2m(double r) {
        return r2r(r) * 3200.0 / 3.141592653589793;
    }

    static void main() {
        double[] angles = new double[]{-2.0, -1.0, 0.0, 1.0, 2.0, 6.2831853, 16.0, 57.2957795, 359.0, 399.0, 6399.0, 1000000.0};
        System.out.println("degrees normalized_degs gradians mils radians");
        for (var a : angles) {
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(a) + " ") + String.valueOf(d2d(a))) + " ") + String.valueOf(d2g(a))) + " ") + String.valueOf(d2m(a))) + " ") + String.valueOf(d2r(a)));
        }
        System.out.println("\ngradians normalized_grds degrees mils radians");
        for (var a : angles) {
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(a) + " ") + String.valueOf(g2g(a))) + " ") + String.valueOf(g2d(a))) + " ") + String.valueOf(g2m(a))) + " ") + String.valueOf(g2r(a)));
        }
        System.out.println("\nmils normalized_mils degrees gradians radians");
        for (var a : angles) {
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(a) + " ") + String.valueOf(m2m(a))) + " ") + String.valueOf(m2d(a))) + " ") + String.valueOf(m2g(a))) + " ") + String.valueOf(m2r(a)));
        }
        System.out.println("\nradians normalized_rads degrees gradians mils");
        for (var a : angles) {
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(a) + " ") + String.valueOf(r2r(a))) + " ") + String.valueOf(r2d(a))) + " ") + String.valueOf(r2g(a))) + " ") + String.valueOf(r2m(a)));
        }
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
