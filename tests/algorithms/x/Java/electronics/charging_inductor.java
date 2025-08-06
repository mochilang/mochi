public class Main {

    static double expApprox(double x) {
        if (x < 0.0) {
            return 1.0 / expApprox(-x);
        }
        if (x > 1.0) {
            double half = expApprox(x / 2.0);
            return half * half;
        }
        double sum = 1.0;
        double term = 1.0;
        int n = 1;
        while (n < 20) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double result = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            result = result * 10.0;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static double charging_inductor(double source_voltage, double resistance, double inductance, double time) {
        if (source_voltage <= 0.0) {
            throw new RuntimeException(String.valueOf("Source voltage must be positive."));
        }
        if (resistance <= 0.0) {
            throw new RuntimeException(String.valueOf("Resistance must be positive."));
        }
        if (inductance <= 0.0) {
            throw new RuntimeException(String.valueOf("Inductance must be positive."));
        }
        double exponent = (-time * resistance) / inductance;
        double current = source_voltage / resistance * (1.0 - expApprox(exponent));
        return round(current, 3);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(charging_inductor(5.8, 1.5, 2.3, 2.0));
            System.out.println(charging_inductor(8.0, 5.0, 3.0, 2.0));
            System.out.println(charging_inductor(8.0, 5.0 * pow10(2), 3.0, 2.0));
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
