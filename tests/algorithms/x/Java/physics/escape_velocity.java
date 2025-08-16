public class Main {

    static double pow10(long n) {
        double p = (double)(1.0);
        long k_1 = 0L;
        if ((long)(n) >= 0L) {
            while ((long)(k_1) < (long)(n)) {
                p = (double)((double)(p) * (double)(10.0));
                k_1 = (long)((long)(k_1) + 1L);
            }
        } else {
            long m_1 = (long)(-n);
            while ((long)(k_1) < (long)(m_1)) {
                p = (double)((double)(p) / (double)(10.0));
                k_1 = (long)((long)(k_1) + 1L);
            }
        }
        return p;
    }

    static double sqrt_newton(double n) {
        if ((double)(n) == (double)(0.0)) {
            return 0.0;
        }
        double x_1 = (double)(n);
        long j_1 = 0L;
        while ((long)(j_1) < 20L) {
            x_1 = (double)((double)(((double)(x_1) + (double)((double)(n) / (double)(x_1)))) / (double)(2.0));
            j_1 = (long)((long)(j_1) + 1L);
        }
        return x_1;
    }

    static double round3(double x) {
        double y = (double)((double)((double)(x) * (double)(1000.0)) + (double)(0.5));
        long yi_1 = (long)(((Number)(y)).intValue());
        if ((double)((((Number)(yi_1)).doubleValue())) > (double)(y)) {
            yi_1 = (long)((long)(yi_1) - 1L);
        }
        return (double)((((Number)(yi_1)).doubleValue())) / (double)(1000.0);
    }

    static double escape_velocity(double mass, double radius) {
        if ((double)(radius) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Radius cannot be zero."));
        }
        double G_1 = (double)((double)(6.6743) * (double)(pow10((long)(-11))));
        double velocity_1 = (double)(sqrt_newton((double)((double)((double)((double)(2.0) * (double)(G_1)) * (double)(mass)) / (double)(radius))));
        return round3((double)(velocity_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(escape_velocity((double)((double)(5.972) * (double)(pow10(24L))), (double)((double)(6.371) * (double)(pow10(6L)))));
            System.out.println(escape_velocity((double)((double)(7.348) * (double)(pow10(22L))), (double)((double)(1.737) * (double)(pow10(6L)))));
            System.out.println(escape_velocity((double)((double)(1.898) * (double)(pow10(27L))), (double)((double)(6.9911) * (double)(pow10(7L)))));
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
