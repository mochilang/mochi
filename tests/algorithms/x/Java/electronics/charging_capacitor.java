public class Main {

    static double expApprox(double x) {
        double y = x;
        boolean is_neg = false;
        if (x < 0.0) {
            is_neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (((Boolean)(is_neg))) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double round3(double x) {
        double scaled = x * 1000.0;
        if (scaled >= 0.0) {
            scaled = scaled + 0.5;
        } else {
            scaled = scaled - 0.5;
        }
        int scaled_int = ((Number)(scaled)).intValue();
        return (((Number)(scaled_int)).doubleValue()) / 1000.0;
    }

    static double charging_capacitor(double source_voltage, double resistance, double capacitance, double time_sec) {
        if (source_voltage <= 0.0) {
            throw new RuntimeException(String.valueOf("Source voltage must be positive."));
        }
        if (resistance <= 0.0) {
            throw new RuntimeException(String.valueOf("Resistance must be positive."));
        }
        if (capacitance <= 0.0) {
            throw new RuntimeException(String.valueOf("Capacitance must be positive."));
        }
        double exponent = -time_sec / (resistance * capacitance);
        double voltage = source_voltage * (1.0 - expApprox(exponent));
        return round3(voltage);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(charging_capacitor(0.2, 0.9, 8.4, 0.5));
            System.out.println(charging_capacitor(2.2, 3.5, 2.4, 9.0));
            System.out.println(charging_capacitor(15.0, 200.0, 20.0, 2.0));
            System.out.println(charging_capacitor(20.0, 2000.0, 0.0003, 4.0));
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
}
