public class Main {

    static double pow10(java.math.BigInteger n) {
        double p = (double)(1.0);
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        if (n.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            while (k_1.compareTo(n) < 0) {
                p = (double)((double)(p) * (double)(10.0));
                k_1 = k_1.add(java.math.BigInteger.valueOf(1));
            }
        } else {
            java.math.BigInteger m_1 = (n).negate();
            while (k_1.compareTo(m_1) < 0) {
                p = (double)((double)(p) / (double)(10.0));
                k_1 = k_1.add(java.math.BigInteger.valueOf(1));
            }
        }
        return (double)(p);
    }

    static double sqrt_newton(double n) {
        if ((double)(n) == (double)(0.0)) {
            return (double)(0.0);
        }
        double x_1 = (double)(n);
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
        while (j_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            x_1 = (double)((double)(((double)(x_1) + (double)((double)(n) / (double)(x_1)))) / (double)(2.0));
            j_1 = j_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(x_1);
    }

    static double round3(double x) {
        double y = (double)((double)((double)(x) * (double)(1000.0)) + (double)(0.5));
        java.math.BigInteger yi_1 = new java.math.BigInteger(String.valueOf(((Number)(y)).intValue()));
        if ((double)((((Number)(yi_1)).doubleValue())) > (double)(y)) {
            yi_1 = yi_1.subtract(java.math.BigInteger.valueOf(1));
        }
        return (double)((double)((((Number)(yi_1)).doubleValue())) / (double)(1000.0));
    }

    static double escape_velocity(double mass, double radius) {
        if ((double)(radius) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Radius cannot be zero."));
        }
        double G_1 = (double)((double)(6.6743) * (double)(pow10((java.math.BigInteger.valueOf(11)).negate())));
        double velocity_1 = (double)(sqrt_newton((double)((double)((double)((double)(2.0) * (double)(G_1)) * (double)(mass)) / (double)(radius))));
        return (double)(round3((double)(velocity_1)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(escape_velocity((double)((double)(5.972) * (double)(pow10(java.math.BigInteger.valueOf(24)))), (double)((double)(6.371) * (double)(pow10(java.math.BigInteger.valueOf(6))))));
            System.out.println(escape_velocity((double)((double)(7.348) * (double)(pow10(java.math.BigInteger.valueOf(22)))), (double)((double)(1.737) * (double)(pow10(java.math.BigInteger.valueOf(6))))));
            System.out.println(escape_velocity((double)((double)(1.898) * (double)(pow10(java.math.BigInteger.valueOf(27)))), (double)((double)(6.9911) * (double)(pow10(java.math.BigInteger.valueOf(7))))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
