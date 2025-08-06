public class Main {
    static double BOLTZMANN;
    static double ELECTRON_VOLT;
    static double TEMPERATURE;

    static double pow10(int n) {
        double result = 1.0;
        int i = 0;
        while (i < n) {
            result = result * 10.0;
            i = i + 1;
        }
        return result;
    }

    static double ln_series(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term = t;
        double sum = 0.0;
        int n = 1;
        while (n <= 19) {
            sum = sum + term / (((Number)(n)).doubleValue());
            term = term * t * t;
            n = n + 2;
        }
        return 2.0 * sum;
    }

    static double ln(double x) {
        double y = x;
        int k = 0;
        while (y >= 10.0) {
            y = y / 10.0;
            k = k + 1;
        }
        while (y < 1.0) {
            y = y * 10.0;
            k = k - 1;
        }
        return ln_series(y) + (((Number)(k)).doubleValue()) * ln_series(10.0);
    }

    static double builtin_voltage(double donor_conc, double acceptor_conc, double intrinsic_conc) {
        if (donor_conc <= 0.0) {
            throw new RuntimeException(String.valueOf("Donor concentration should be positive"));
        }
        if (acceptor_conc <= 0.0) {
            throw new RuntimeException(String.valueOf("Acceptor concentration should be positive"));
        }
        if (intrinsic_conc <= 0.0) {
            throw new RuntimeException(String.valueOf("Intrinsic concentration should be positive"));
        }
        if (donor_conc <= intrinsic_conc) {
            throw new RuntimeException(String.valueOf("Donor concentration should be greater than intrinsic concentration"));
        }
        if (acceptor_conc <= intrinsic_conc) {
            throw new RuntimeException(String.valueOf("Acceptor concentration should be greater than intrinsic concentration"));
        }
        return BOLTZMANN * TEMPERATURE * ln((donor_conc * acceptor_conc) / (intrinsic_conc * intrinsic_conc)) / ELECTRON_VOLT;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            BOLTZMANN = 1.380649 / pow10(23);
            ELECTRON_VOLT = 1.602176634 / pow10(19);
            TEMPERATURE = 300.0;
            System.out.println(_p(builtin_voltage(pow10(17), pow10(17), pow10(10))));
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
