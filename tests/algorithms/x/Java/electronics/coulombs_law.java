public class Main {
    static double COULOMBS_CONSTANT;

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static java.util.Map<String,Double> coulombs_law(double force, double charge1, double charge2, double distance) {
        double charge_product = ((Number)(Math.abs(charge1 * charge2))).doubleValue();
        int zero_count = 0;
        if (force == 0.0) {
            zero_count = zero_count + 1;
        }
        if (charge1 == 0.0) {
            zero_count = zero_count + 1;
        }
        if (charge2 == 0.0) {
            zero_count = zero_count + 1;
        }
        if (distance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if (distance < 0.0) {
            throw new RuntimeException(String.valueOf("Distance cannot be negative"));
        }
        if (force == 0.0) {
            double f = COULOMBS_CONSTANT * charge_product / (distance * distance);
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("force", f)));
        }
        if (charge1 == 0.0) {
            double c1 = Math.abs(force) * (distance * distance) / (COULOMBS_CONSTANT * charge2);
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("charge1", c1)));
        }
        if (charge2 == 0.0) {
            double c2 = Math.abs(force) * (distance * distance) / (COULOMBS_CONSTANT * charge1);
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("charge2", c2)));
        }
        double d = sqrtApprox(COULOMBS_CONSTANT * charge_product / Math.abs(force));
        return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("distance", d)));
    }

    static void print_map(java.util.Map<String,Double> m) {
        for (String k : m.keySet()) {
            System.out.println("{\"" + (String)(k) + "\": " + _p(((double)(m).getOrDefault(k, 0.0))) + "}");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            COULOMBS_CONSTANT = 8988000000.0;
            print_map(coulombs_law(0.0, 3.0, 5.0, 2000.0));
            print_map(coulombs_law(10.0, 3.0, 5.0, 0.0));
            print_map(coulombs_law(10.0, 0.0, 5.0, 2000.0));
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
