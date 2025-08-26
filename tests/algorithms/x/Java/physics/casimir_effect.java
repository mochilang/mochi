public class Main {
    static double PI = (double)(3.141592653589793);
    static double REDUCED_PLANCK_CONSTANT = (double)(1.054571817e-34);
    static double SPEED_OF_LIGHT = (double)(300000000.0);

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)(x);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(100)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(guess_1);
    }

    static java.util.Map<String,Double> casimir_force(double force, double area, double distance) {
        java.math.BigInteger zero_count = java.math.BigInteger.valueOf(0);
        if ((double)(force) == (double)(0.0)) {
            zero_count = zero_count.add(java.math.BigInteger.valueOf(1));
        }
        if ((double)(area) == (double)(0.0)) {
            zero_count = zero_count.add(java.math.BigInteger.valueOf(1));
        }
        if ((double)(distance) == (double)(0.0)) {
            zero_count = zero_count.add(java.math.BigInteger.valueOf(1));
        }
        if (zero_count.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(force) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Magnitude of force can not be negative"));
        }
        if ((double)(distance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Distance can not be negative"));
        }
        if ((double)(area) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Area can not be negative"));
        }
        if ((double)(force) == (double)(0.0)) {
            double num_1 = (double)((double)((double)((double)((double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT)) * (double)(PI)) * (double)(PI)) * (double)(area));
            double den_1 = (double)((double)((double)((double)((double)(240.0) * (double)(distance)) * (double)(distance)) * (double)(distance)) * (double)(distance));
            double f_1 = (double)((double)(num_1) / (double)(den_1));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("force", (double)(f_1)); }}));
        }
        if ((double)(area) == (double)(0.0)) {
            double num_3 = (double)((double)((double)((double)((double)((double)(240.0) * (double)(force)) * (double)(distance)) * (double)(distance)) * (double)(distance)) * (double)(distance));
            double den_3 = (double)((double)((double)((double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT)) * (double)(PI)) * (double)(PI));
            double a_1 = (double)((double)(num_3) / (double)(den_3));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("area", (double)(a_1)); }}));
        }
        double num_5 = (double)((double)((double)((double)((double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT)) * (double)(PI)) * (double)(PI)) * (double)(area));
        double den_5 = (double)((double)(240.0) * (double)(force));
        double inner_1 = (double)((double)(num_5) / (double)(den_5));
        double d_1 = (double)(sqrtApprox((double)(sqrtApprox((double)(inner_1)))));
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("distance", (double)(d_1)); }}));
    }

    static void main() {
        System.out.println(_p(casimir_force((double)(0.0), (double)(4.0), (double)(0.03))));
        System.out.println(_p(casimir_force((double)(2.635e-10), (double)(0.0023), (double)(0.0))));
        System.out.println(_p(casimir_force((double)(2.737e-18), (double)(0.0), (double)(0.0023746))));
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
