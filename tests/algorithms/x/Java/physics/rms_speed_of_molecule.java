public class Main {
    static double UNIVERSAL_GAS_CONSTANT = (double)(8.3144598);
    static double vrms;

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 10L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double rms_speed_of_molecule(double temperature, double molar_mass) {
        if ((double)(temperature) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Temperature cannot be less than 0 K"));
        }
        if ((double)(molar_mass) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Molar mass cannot be less than or equal to 0 kg/mol"));
        }
        double num_1 = (double)((double)((double)(3.0) * (double)(UNIVERSAL_GAS_CONSTANT)) * (double)(temperature));
        double val_1 = (double)((double)(num_1) / (double)(molar_mass));
        double result_1 = (double)(sqrt((double)(val_1)));
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("rms_speed_of_molecule(100, 2) = " + _p(rms_speed_of_molecule((double)(100.0), (double)(2.0))));
            System.out.println("rms_speed_of_molecule(273, 12) = " + _p(rms_speed_of_molecule((double)(273.0), (double)(12.0))));
            vrms = (double)(rms_speed_of_molecule((double)(300.0), (double)(28.0)));
            System.out.println("Vrms of Nitrogen gas at 300 K is " + _p(vrms) + " m/s");
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
