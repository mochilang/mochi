public class Main {
    static double PI;
    static double REDUCED_PLANCK_CONSTANT;
    static double SPEED_OF_LIGHT;

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 100) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static java.util.Map<String,Double> casimir_force(double force, double area, double distance) {
        int zero_count = 0;
        if (force == 0.0) {
            zero_count = zero_count + 1;
        }
        if (area == 0.0) {
            zero_count = zero_count + 1;
        }
        if (distance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if (force < 0.0) {
            throw new RuntimeException(String.valueOf("Magnitude of force can not be negative"));
        }
        if (distance < 0.0) {
            throw new RuntimeException(String.valueOf("Distance can not be negative"));
        }
        if (area < 0.0) {
            throw new RuntimeException(String.valueOf("Area can not be negative"));
        }
        if (force == 0.0) {
            double num = REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT * PI * PI * area;
            double den = 240.0 * distance * distance * distance * distance;
            double f = num / den;
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("force", f)));
        }
        if (area == 0.0) {
            double num_1 = 240.0 * force * distance * distance * distance * distance;
            double den_1 = REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT * PI * PI;
            double a = num_1 / den_1;
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("area", a)));
        }
        double num_2 = REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT * PI * PI * area;
        double den_2 = 240.0 * force;
        double inner = num_2 / den_2;
        double d = sqrtApprox(sqrtApprox(inner));
        return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("distance", d)));
    }

    static void main() {
        System.out.println(_p(casimir_force(0.0, 4.0, 0.03)));
        System.out.println(_p(casimir_force(2.635e-10, 0.0023, 0.0)));
        System.out.println(_p(casimir_force(2.737e-18, 0.0, 0.0023746)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            REDUCED_PLANCK_CONSTANT = 1.054571817e-34;
            SPEED_OF_LIGHT = 300000000.0;
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
