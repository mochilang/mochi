public class Main {
    static double COULOMBS_CONSTANT = (double)(8988000000.0);

    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static java.util.Map<String,Double> coulombs_law(double force, double charge1, double charge2, double distance) {
        double charge_product = ((Number)(Math.abs((double)(charge1) * (double)(charge2)))).doubleValue();
        long zero_count_1 = 0L;
        if ((double)(force) == (double)(0.0)) {
            zero_count_1 = (long)((long)(zero_count_1) + 1L);
        }
        if ((double)(charge1) == (double)(0.0)) {
            zero_count_1 = (long)((long)(zero_count_1) + 1L);
        }
        if ((double)(charge2) == (double)(0.0)) {
            zero_count_1 = (long)((long)(zero_count_1) + 1L);
        }
        if ((double)(distance) == (double)(0.0)) {
            zero_count_1 = (long)((long)(zero_count_1) + 1L);
        }
        if ((long)(zero_count_1) != 1L) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(distance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Distance cannot be negative"));
        }
        if ((double)(force) == (double)(0.0)) {
            double f_1 = (double)((double)((double)(COULOMBS_CONSTANT) * (double)(charge_product)) / (double)(((double)(distance) * (double)(distance))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("force", (double)(f_1))))));
        }
        if ((double)(charge1) == (double)(0.0)) {
            double c1_1 = (double)((double)(Math.abs(force) * (double)(((double)(distance) * (double)(distance)))) / (double)(((double)(COULOMBS_CONSTANT) * (double)(charge2))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("charge1", (double)(c1_1))))));
        }
        if ((double)(charge2) == (double)(0.0)) {
            double c2_1 = (double)((double)(Math.abs(force) * (double)(((double)(distance) * (double)(distance)))) / (double)(((double)(COULOMBS_CONSTANT) * (double)(charge1))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("charge2", (double)(c2_1))))));
        }
        double d_1 = (double)(sqrtApprox((double)((double)((double)(COULOMBS_CONSTANT) * (double)(charge_product)) / Math.abs(force))));
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("distance", (double)(d_1))))));
    }

    static void print_map(java.util.Map<String,Double> m) {
        for (String k : m.keySet()) {
            System.out.println("{\"" + k + "\": " + _p(((double)(m).getOrDefault(k, 0.0))) + "}");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            print_map(coulombs_law((double)(0.0), (double)(3.0), (double)(5.0), (double)(2000.0)));
            print_map(coulombs_law((double)(10.0), (double)(3.0), (double)(5.0), (double)(0.0)));
            print_map(coulombs_law((double)(10.0), (double)(0.0), (double)(5.0), (double)(2000.0)));
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
