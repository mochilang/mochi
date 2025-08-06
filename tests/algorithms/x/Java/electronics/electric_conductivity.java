public class Main {
    static double ELECTRON_CHARGE;
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

    static Result electric_conductivity(double conductivity, double electron_conc, double mobility) {
        int zero_count = 0;
        if (conductivity == 0.0) {
            zero_count = zero_count + 1;
        }
        if (electron_conc == 0.0) {
            zero_count = zero_count + 1;
        }
        if (mobility == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        }
        if (conductivity < 0.0) {
            throw new RuntimeException(String.valueOf("Conductivity cannot be negative"));
        }
        if (electron_conc < 0.0) {
            throw new RuntimeException(String.valueOf("Electron concentration cannot be negative"));
        }
        if (mobility < 0.0) {
            throw new RuntimeException(String.valueOf("mobility cannot be negative"));
        }
        if (conductivity == 0.0) {
            return new Result("conductivity", mobility * electron_conc * ELECTRON_CHARGE);
        }
        if (electron_conc == 0.0) {
            return new Result("electron_conc", conductivity / (mobility * ELECTRON_CHARGE));
        }
        return new Result("mobility", conductivity / (electron_conc * ELECTRON_CHARGE));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ELECTRON_CHARGE = 1.6021e-19;
            r1 = electric_conductivity(25.0, 100.0, 0.0);
            r2 = electric_conductivity(0.0, 1600.0, 200.0);
            r3 = electric_conductivity(1000.0, 0.0, 1200.0);
            System.out.println(r1.kind + " " + _p(r1.value));
            System.out.println(r2.kind + " " + _p(r2.value));
            System.out.println(r3.kind + " " + _p(r3.value));
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
