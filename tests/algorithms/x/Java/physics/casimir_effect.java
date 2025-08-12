public class Main {
    static double PI;
    static double REDUCED_PLANCK_CONSTANT;
    static double SPEED_OF_LIGHT;

    static double sqrtApprox(double x) {
        if ((double)(x) <= 0.0) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(100)) {
            guess_1 = (guess_1 + (double)(x) / guess_1) / 2.0;
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return guess_1;
    }

    static java.util.Map<String,Double> casimir_force(double force, double area, double distance) {
        long zero_count = 0L;
        if ((double)(force) == 0.0) {
            zero_count = (long)((long)(zero_count) + (long)(1));
        }
        if ((double)(area) == 0.0) {
            zero_count = (long)((long)(zero_count) + (long)(1));
        }
        if ((double)(distance) == 0.0) {
            zero_count = (long)((long)(zero_count) + (long)(1));
        }
        if ((long)(zero_count) != (long)(1)) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(force) < 0.0) {
            throw new RuntimeException(String.valueOf("Magnitude of force can not be negative"));
        }
        if ((double)(distance) < 0.0) {
            throw new RuntimeException(String.valueOf("Distance can not be negative"));
        }
        if ((double)(area) < 0.0) {
            throw new RuntimeException(String.valueOf("Area can not be negative"));
        }
        if ((double)(force) == 0.0) {
            double num_1 = (double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT) * (double)(PI) * (double)(PI) * (double)(area);
            double den_1 = 240.0 * (double)(distance) * (double)(distance) * (double)(distance) * (double)(distance);
            double f_1 = num_1 / den_1;
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("force", f_1)))));
        }
        if ((double)(area) == 0.0) {
            double num_3 = 240.0 * (double)(force) * (double)(distance) * (double)(distance) * (double)(distance) * (double)(distance);
            double den_3 = (double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT) * (double)(PI) * (double)(PI);
            double a_1 = num_3 / den_3;
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("area", a_1)))));
        }
        double num_5 = (double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT) * (double)(PI) * (double)(PI) * (double)(area);
        double den_5 = 240.0 * (double)(force);
        double inner_1 = num_5 / den_5;
        double d_1 = (double)(sqrtApprox((double)(sqrtApprox(inner_1))));
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("distance", (double)(d_1))))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
