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
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static CarrierResult carrier_concentration(double electron_conc, double hole_conc, double intrinsic_conc) {
        int zero_count = 0;
        if (electron_conc == 0.0) {
            zero_count = zero_count + 1;
        }
        if (hole_conc == 0.0) {
            zero_count = zero_count + 1;
        }
        if (intrinsic_conc == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        }
        if (electron_conc < 0.0) {
            throw new RuntimeException(String.valueOf("Electron concentration cannot be negative in a semiconductor"));
        }
        if (hole_conc < 0.0) {
            throw new RuntimeException(String.valueOf("Hole concentration cannot be negative in a semiconductor"));
        }
        if (intrinsic_conc < 0.0) {
            throw new RuntimeException(String.valueOf("Intrinsic concentration cannot be negative in a semiconductor"));
        }
        if (electron_conc == 0.0) {
            return new CarrierResult("electron_conc", (intrinsic_conc * intrinsic_conc) / hole_conc);
        }
        if (hole_conc == 0.0) {
            return new CarrierResult("hole_conc", (intrinsic_conc * intrinsic_conc) / electron_conc);
        }
        if (intrinsic_conc == 0.0) {
            return new CarrierResult("intrinsic_conc", sqrtApprox(electron_conc * hole_conc));
        }
        return new CarrierResult("", -1.0);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = carrier_concentration(25.0, 100.0, 0.0);
            System.out.println(r1.name + ", " + _p(r1.value));
            r2 = carrier_concentration(0.0, 1600.0, 200.0);
            System.out.println(r2.name + ", " + _p(r2.value));
            r3 = carrier_concentration(1000.0, 0.0, 1200.0);
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
        return String.valueOf(v);
    }
}
