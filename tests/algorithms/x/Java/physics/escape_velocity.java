public class Main {

    static double pow10(long n) {
        double p = 1.0;
        long k_1 = 0L;
        if (n >= (long)(0)) {
            while ((long)(k_1) < n) {
                p = p * 10.0;
                k_1 = (long)((long)(k_1) + (long)(1));
            }
        } else {
            long m_1 = -n;
            while ((long)(k_1) < (long)(m_1)) {
                p = p / 10.0;
                k_1 = (long)((long)(k_1) + (long)(1));
            }
        }
        return p;
    }

    static double sqrt_newton(double n) {
        if ((double)(n) == 0.0) {
            return 0.0;
        }
        double x_1 = (double)(n);
        long j_1 = 0L;
        while ((long)(j_1) < (long)(20)) {
            x_1 = (x_1 + (double)(n) / x_1) / 2.0;
            j_1 = (long)((long)(j_1) + (long)(1));
        }
        return x_1;
    }

    static double round3(double x) {
        double y = (double)(x) * 1000.0 + 0.5;
        long yi_1 = (long)(((Number)(y)).intValue());
        if ((((Number)(yi_1)).doubleValue()) > y) {
            yi_1 = (long)((long)(yi_1) - (long)(1));
        }
        return (((Number)(yi_1)).doubleValue()) / 1000.0;
    }

    static double escape_velocity(double mass, double radius) {
        if ((double)(radius) == 0.0) {
            throw new RuntimeException(String.valueOf("Radius cannot be zero."));
        }
        double G_1 = 6.6743 * (double)(pow10((long)(-11)));
        double velocity_1 = (double)(sqrt_newton(2.0 * G_1 * (double)(mass) / (double)(radius)));
        return round3((double)(velocity_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(escape_velocity(5.972 * (double)(pow10(24L)), 6.371 * (double)(pow10(6L))));
            System.out.println(escape_velocity(7.348 * (double)(pow10(22L)), 1.737 * (double)(pow10(6L))));
            System.out.println(escape_velocity(1.898 * (double)(pow10(27L)), 6.9911 * (double)(pow10(7L))));
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
