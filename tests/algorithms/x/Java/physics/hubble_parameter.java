public class Main {

    static double pow(double base, java.math.BigInteger exp) {
        double result = (double)(1.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(exp) < 0) {
            result = (double)((double)(result) * (double)(base));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(result);
    }

    static double sqrt_approx(double x) {
        if ((double)(x) == (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(guess_1);
    }

    static double hubble_parameter(double hubble_constant, double radiation_density, double matter_density, double dark_energy, double redshift) {
        double[] parameters = ((double[])(new double[]{(double)(redshift), (double)(radiation_density), (double)(matter_density), (double)(dark_energy)}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(parameters.length))) < 0) {
            if ((double)(parameters[_idx((parameters).length, ((java.math.BigInteger)(i_5)).longValue())]) < (double)(0.0)) {
                throw new RuntimeException(String.valueOf("All input parameters must be positive"));
            }
            i_5 = i_5.add(java.math.BigInteger.valueOf(1));
        }
        i_5 = java.math.BigInteger.valueOf(1);
        while (i_5.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
            if ((double)(parameters[_idx((parameters).length, ((java.math.BigInteger)(i_5)).longValue())]) > (double)(1.0)) {
                throw new RuntimeException(String.valueOf("Relative densities cannot be greater than one"));
            }
            i_5 = i_5.add(java.math.BigInteger.valueOf(1));
        }
        double curvature_1 = (double)((double)(1.0) - (double)(((double)((double)(matter_density) + (double)(radiation_density)) + (double)(dark_energy))));
        double zp1_1 = (double)((double)(redshift) + (double)(1.0));
        double e2_1 = (double)((double)((double)((double)((double)(radiation_density) * (double)(pow((double)(zp1_1), java.math.BigInteger.valueOf(4)))) + (double)((double)(matter_density) * (double)(pow((double)(zp1_1), java.math.BigInteger.valueOf(3))))) + (double)((double)(curvature_1) * (double)(pow((double)(zp1_1), java.math.BigInteger.valueOf(2))))) + (double)(dark_energy));
        return (double)((double)(hubble_constant) * (double)(sqrt_approx((double)(e2_1))));
    }

    static void test_hubble_parameter() {
        double h = (double)(hubble_parameter((double)(68.3), (double)(0.0001), (double)(0.3), (double)(0.7), (double)(0.0)));
        if ((double)(h) < (double)(68.2999) || (double)(h) > (double)(68.3001)) {
            throw new RuntimeException(String.valueOf("hubble_parameter test failed"));
        }
    }

    static void main() {
        test_hubble_parameter();
        System.out.println(hubble_parameter((double)(68.3), (double)(0.0001), (double)(0.3), (double)(0.7), (double)(0.0)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
