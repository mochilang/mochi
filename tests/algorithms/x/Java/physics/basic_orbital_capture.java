public class Main {
    static double G;
    static double C;
    static double PI;

    static double pow10(long n) {
        double result = 1.0;
        long i_1 = 0L;
        while ((long)(i_1) < n) {
            result = result * 10.0;
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }

    static double sqrt(double x) {
        if ((double)(x) <= 0.0) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(20)) {
            guess_1 = (guess_1 + (double)(x) / guess_1) / 2.0;
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return guess_1;
    }

    static double abs(double x) {
        if ((double)(x) < 0.0) {
            return -x;
        }
        return x;
    }

    static double capture_radii(double target_body_radius, double target_body_mass, double projectile_velocity) {
        if ((double)(target_body_mass) < 0.0) {
            throw new RuntimeException(String.valueOf("Mass cannot be less than 0"));
        }
        if ((double)(target_body_radius) < 0.0) {
            throw new RuntimeException(String.valueOf("Radius cannot be less than 0"));
        }
        if ((double)(projectile_velocity) > (double)(C)) {
            throw new RuntimeException(String.valueOf("Cannot go beyond speed of light"));
        }
        double escape_velocity_squared_1 = (2.0 * (double)(G) * (double)(target_body_mass)) / (double)(target_body_radius);
        double denom_1 = (double)(projectile_velocity) * (double)(projectile_velocity);
        double capture_radius_1 = (double)(target_body_radius) * (double)(sqrt(1.0 + escape_velocity_squared_1 / denom_1));
        return capture_radius_1;
    }

    static double capture_area(double capture_radius) {
        if ((double)(capture_radius) < 0.0) {
            throw new RuntimeException(String.valueOf("Cannot have a capture radius less than 0"));
        }
        double sigma_1 = (double)(PI) * (double)(capture_radius) * (double)(capture_radius);
        return sigma_1;
    }

    static void run_tests() {
        double r = (double)(capture_radii(6.957 * (double)(pow10(8L)), 1.99 * (double)(pow10(30L)), 25000.0));
        if (Math.abs((double)(r) - 1.720959069143714 * (double)(pow10(10L))) > 1.0) {
            throw new RuntimeException(String.valueOf("capture_radii failed"));
        }
        double a_1 = (double)(capture_area((double)(r)));
        if (Math.abs((double)(a_1) - 9.304455331801812 * (double)(pow10(20L))) > 1.0) {
            throw new RuntimeException(String.valueOf("capture_area failed"));
        }
    }

    static void main() {
        run_tests();
        double r_2 = (double)(capture_radii(6.957 * (double)(pow10(8L)), 1.99 * (double)(pow10(30L)), 25000.0));
        System.out.println(_p(r_2));
        System.out.println(_p(capture_area((double)(r_2))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            G = 6.6743e-11;
            C = 299792458.0;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
