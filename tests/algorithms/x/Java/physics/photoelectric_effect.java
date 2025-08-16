public class Main {
    static double PLANCK_CONSTANT_JS;
    static double PLANCK_CONSTANT_EVS;

    static double pow10(long exp) {
        double result = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result = (double)((double)(result) * (double)(10.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double maximum_kinetic_energy(double frequency, double work_function, boolean in_ev) {
        if ((double)(frequency) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Frequency can't be negative."));
        }
        double energy_1 = (double)(in_ev ? (double)((double)(PLANCK_CONSTANT_EVS) * (double)(frequency)) - (double)(work_function) : (double)((double)(PLANCK_CONSTANT_JS) * (double)(frequency)) - (double)(work_function));
        if ((double)(energy_1) > (double)(0.0)) {
            return energy_1;
        }
        return 0.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PLANCK_CONSTANT_JS = (double)((double)(6.6261) / (double)(pow10(34L)));
            PLANCK_CONSTANT_EVS = (double)((double)(4.1357) / (double)(pow10(15L)));
            System.out.println(_p(maximum_kinetic_energy((double)(1000000.0), (double)(2.0), false)));
            System.out.println(_p(maximum_kinetic_energy((double)(1000000.0), (double)(2.0), true)));
            System.out.println(_p(maximum_kinetic_energy((double)(10000000000000000.0), (double)(2.0), true)));
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
