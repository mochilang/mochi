public class Main {
    static class CarrierResult {
        String name;
        double value;
        CarrierResult(String name, double value) {
            this.name = name;
            this.value = value;
        }
        CarrierResult() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'value': %s}", String.valueOf(name), String.valueOf(value));
        }
    }

    static CarrierResult r1;
    static CarrierResult r2;
    static CarrierResult r3;

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static CarrierResult carrier_concentration(double electron_conc, double hole_conc, double intrinsic_conc) {
        long zero_count = 0L;
        if ((double)(electron_conc) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(hole_conc) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((double)(intrinsic_conc) == (double)(0.0)) {
            zero_count = (long)((long)(zero_count) + 1L);
        }
        if ((long)(zero_count) != 1L) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        }
        if ((double)(electron_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Electron concentration cannot be negative in a semiconductor"));
        }
        if ((double)(hole_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Hole concentration cannot be negative in a semiconductor"));
        }
        if ((double)(intrinsic_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Intrinsic concentration cannot be negative in a semiconductor"));
        }
        if ((double)(electron_conc) == (double)(0.0)) {
            return new CarrierResult("electron_conc", (double)(((double)(intrinsic_conc) * (double)(intrinsic_conc))) / (double)(hole_conc));
        }
        if ((double)(hole_conc) == (double)(0.0)) {
            return new CarrierResult("hole_conc", (double)(((double)(intrinsic_conc) * (double)(intrinsic_conc))) / (double)(electron_conc));
        }
        if ((double)(intrinsic_conc) == (double)(0.0)) {
            return new CarrierResult("intrinsic_conc", sqrtApprox((double)((double)(electron_conc) * (double)(hole_conc))));
        }
        return new CarrierResult("", -1.0);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = carrier_concentration((double)(25.0), (double)(100.0), (double)(0.0));
            System.out.println(r1.name + ", " + _p(r1.value));
            r2 = carrier_concentration((double)(0.0), (double)(1600.0), (double)(200.0));
            System.out.println(r2.name + ", " + _p(r2.value));
            r3 = carrier_concentration((double)(1000.0), (double)(0.0), (double)(1200.0));
            System.out.println(r3.name + ", " + _p(r3.value));
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
