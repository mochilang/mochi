public class Main {
    static double G;
    static double C;
    static double PI;

    static double pow10(int n) {
        double result = 1.0;
        int i = 0;
        while (i < n) {
            result = result * 10.0;
            i = i + 1;
        }
        return result;
    }

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i_1 = 0;
        while (i_1 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess;
    }

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double capture_radii(double target_body_radius, double target_body_mass, double projectile_velocity) {
        if (target_body_mass < 0.0) {
            throw new RuntimeException(String.valueOf("Mass cannot be less than 0"));
        }
        if (target_body_radius < 0.0) {
            throw new RuntimeException(String.valueOf("Radius cannot be less than 0"));
        }
        if (projectile_velocity > C) {
            throw new RuntimeException(String.valueOf("Cannot go beyond speed of light"));
        }
        double escape_velocity_squared = (2.0 * G * target_body_mass) / target_body_radius;
        double denom = projectile_velocity * projectile_velocity;
        double capture_radius = target_body_radius * sqrt(1.0 + escape_velocity_squared / denom);
        return capture_radius;
    }

    static double capture_area(double capture_radius) {
        if (capture_radius < 0.0) {
            throw new RuntimeException(String.valueOf("Cannot have a capture radius less than 0"));
        }
        double sigma = PI * capture_radius * capture_radius;
        return sigma;
    }

    static void run_tests() {
        double r = capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000.0);
        if (Math.abs(r - 1.720959069143714 * pow10(10)) > 1.0) {
            throw new RuntimeException(String.valueOf("capture_radii failed"));
        }
        double a = capture_area(r);
        if (Math.abs(a - 9.304455331801812 * pow10(20)) > 1.0) {
            throw new RuntimeException(String.valueOf("capture_area failed"));
        }
    }

    static void main() {
        run_tests();
        double r_1 = capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000.0);
        System.out.println(_p(r_1));
        System.out.println(_p(capture_area(r_1)));
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
        return String.valueOf(v);
    }
}
