public class Main {
    static double GRAVITATIONAL_CONSTANT = (double)(6.6743e-11);
    static class Result {
        String kind;
        double value;
        Result(String kind, double value) {
            this.kind = kind;
            this.value = value;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'kind': '%s', 'value': %s}", String.valueOf(kind), String.valueOf(value));
        }
    }

    static Result r1;
    static Result r2;
    static Result r3;
    static Result r4;

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static Result gravitational_law(double force, double mass_1, double mass_2, double distance) {
        long zero_count = 0L;
        if ((double)(force) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(mass_1) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(mass_2) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(distance) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((long)(zero_count) != 1L) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(force) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Gravitational force can not be negative"));
        }
        if ((double)(distance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Distance can not be negative"));
        }
        if ((double)(mass_1) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Mass can not be negative"));
        }
        if ((double)(mass_2) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Mass can not be negative"));
        }
        double product_of_mass_1 = (double)((double)(mass_1) * (double)(mass_2));
        if ((double)(force) == (double)(0.0)) {
            double f_1 = (double)((double)((double)(GRAVITATIONAL_CONSTANT) * (double)(product_of_mass_1)) / (double)(((double)(distance) * (double)(distance))));
            return new Result("force", f_1);
        }
        if ((double)(mass_1) == (double)(0.0)) {
            double m1_1 = (double)((double)((double)(force) * (double)(((double)(distance) * (double)(distance)))) / (double)(((double)(GRAVITATIONAL_CONSTANT) * (double)(mass_2))));
            return new Result("mass_1", m1_1);
        }
        if ((double)(mass_2) == (double)(0.0)) {
            double m2_1 = (double)((double)((double)(force) * (double)(((double)(distance) * (double)(distance)))) / (double)(((double)(GRAVITATIONAL_CONSTANT) * (double)(mass_1))));
            return new Result("mass_2", m2_1);
        }
        double d_1 = (double)(sqrtApprox((double)((double)((double)(GRAVITATIONAL_CONSTANT) * (double)(product_of_mass_1)) / (double)(force))));
        return new Result("distance", d_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = gravitational_law((double)(0.0), (double)(5.0), (double)(10.0), (double)(20.0));
            r2 = gravitational_law((double)(7367.382), (double)(0.0), (double)(74.0), (double)(3048.0));
            r3 = gravitational_law((double)(100.0), (double)(5.0), (double)(0.0), (double)(3.0));
            r4 = gravitational_law((double)(100.0), (double)(5.0), (double)(10.0), (double)(0.0));
            System.out.println(r1.kind + " " + _p(r1.value));
            System.out.println(r2.kind + " " + _p(r2.value));
            System.out.println(r3.kind + " " + _p(r3.value));
            System.out.println(r4.kind + " " + _p(r4.value));
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
