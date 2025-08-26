public class Main {
    static double G = (double)(6.6743e-11);
    static double C = (double)(299792458.0);
    static double PI = (double)(3.141592653589793);

    static double pow10(java.math.BigInteger n) {
        double result = (double)(1.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            result = (double)((double)(result) * (double)(10.0));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(result);
    }

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)(x);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(guess_1);
    }

    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(-x);
        }
        return (double)(x);
    }

    static double capture_radii(double target_body_radius, double target_body_mass, double projectile_velocity) {
        if ((double)(target_body_mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Mass cannot be less than 0"));
        }
        if ((double)(target_body_radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Radius cannot be less than 0"));
        }
        if ((double)(projectile_velocity) > (double)(C)) {
            throw new RuntimeException(String.valueOf("Cannot go beyond speed of light"));
        }
        double escape_velocity_squared_1 = (double)((double)(((double)((double)(2.0) * (double)(G)) * (double)(target_body_mass))) / (double)(target_body_radius));
        double denom_1 = (double)((double)(projectile_velocity) * (double)(projectile_velocity));
        double capture_radius_1 = (double)((double)(target_body_radius) * (double)(sqrt((double)((double)(1.0) + (double)((double)(escape_velocity_squared_1) / (double)(denom_1))))));
        return (double)(capture_radius_1);
    }

    static double capture_area(double capture_radius) {
        if ((double)(capture_radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Cannot have a capture radius less than 0"));
        }
        double sigma_1 = (double)((double)((double)(PI) * (double)(capture_radius)) * (double)(capture_radius));
        return (double)(sigma_1);
    }

    static void run_tests() {
        double r = (double)(capture_radii((double)((double)(6.957) * (double)(pow10(java.math.BigInteger.valueOf(8)))), (double)((double)(1.99) * (double)(pow10(java.math.BigInteger.valueOf(30)))), (double)(25000.0)));
        if (Math.abs((double)(r) - (double)((double)(1.720959069143714) * (double)(pow10(java.math.BigInteger.valueOf(10))))) > (double)(1.0)) {
            throw new RuntimeException(String.valueOf("capture_radii failed"));
        }
        double a_1 = (double)(capture_area((double)(r)));
        if (Math.abs((double)(a_1) - (double)((double)(9.304455331801812) * (double)(pow10(java.math.BigInteger.valueOf(20))))) > (double)(1.0)) {
            throw new RuntimeException(String.valueOf("capture_area failed"));
        }
    }

    static void main() {
        run_tests();
        double r_2 = (double)(capture_radii((double)((double)(6.957) * (double)(pow10(java.math.BigInteger.valueOf(8)))), (double)((double)(1.99) * (double)(pow10(java.math.BigInteger.valueOf(30)))), (double)(25000.0)));
        System.out.println(_p(r_2));
        System.out.println(_p(capture_area((double)(r_2))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
