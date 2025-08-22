public class Main {
    static double PI = (double)(3.141592653589793);
    static double REDUCED_PLANCK_CONSTANT = (double)(1.054571817e-34);
    static double SPEED_OF_LIGHT = (double)(300000000.0);

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 100L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static java.util.Map<String,Double> casimir_force(double force, double area, double distance) {
        long zero_count = 0L;
        if ((double)(force) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(area) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(distance) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((long)(zero_count) != 1L) {
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
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("force", (double)(f_1))))));
        }
        if ((double)(area) == (double)(0.0)) {
            double num_3 = (double)((double)((double)((double)((double)((double)(240.0) * (double)(force)) * (double)(distance)) * (double)(distance)) * (double)(distance)) * (double)(distance));
            double den_3 = (double)((double)((double)((double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT)) * (double)(PI)) * (double)(PI));
            double a_1 = (double)((double)(num_3) / (double)(den_3));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("area", (double)(a_1))))));
        }
        double num_5 = (double)((double)((double)((double)((double)(REDUCED_PLANCK_CONSTANT) * (double)(SPEED_OF_LIGHT)) * (double)(PI)) * (double)(PI)) * (double)(area));
        double den_5 = (double)((double)(240.0) * (double)(force));
        double inner_1 = (double)((double)(num_5) / (double)(den_5));
        double d_1 = (double)(sqrtApprox((double)(sqrtApprox((double)(inner_1)))));
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("distance", (double)(d_1))))));
    }

    static void main() {
        System.out.println(_p(casimir_force((double)(0.0), (double)(4.0), (double)(0.03))));
        System.out.println(_p(casimir_force((double)(2.635e-10), (double)(0.0023), (double)(0.0))));
        System.out.println(_p(casimir_force((double)(2.737e-18), (double)(0.0), (double)(0.0023746))));
    }
    public static void main(String[] args) {
        main();
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
