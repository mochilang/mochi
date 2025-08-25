public class Main {
    static java.math.BigInteger seed = java.math.BigInteger.valueOf(1);
    static double INITIAL_VALUE = (double)(0.02);
    static double result;

    static java.math.BigInteger rand() {
        seed = new java.math.BigInteger(String.valueOf((seed.multiply(java.math.BigInteger.valueOf(1103515245)).add(java.math.BigInteger.valueOf(12345))).remainder(java.math.BigInteger.valueOf(2147483648L))));
        return new java.math.BigInteger(String.valueOf(seed));
    }

    static java.math.BigInteger randint(java.math.BigInteger low, java.math.BigInteger high) {
        return new java.math.BigInteger(String.valueOf((rand().remainder((high.subtract(low).add(java.math.BigInteger.valueOf(1))))).add(low)));
    }

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < (double)(0.0)) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(30)) < 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(y)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (is_neg_1) {
            return (double)((double)(1.0) / (double)(sum_1));
        }
        return (double)(sum_1);
    }

    static double sigmoid(double x) {
        return (double)((double)(1.0) / (double)(((double)(1.0) + (double)(expApprox((double)(-x))))));
    }

    static double sigmoid_derivative(double sig_val) {
        return (double)((double)(sig_val) * (double)(((double)(1.0) - (double)(sig_val))));
    }

    static double forward_propagation(java.math.BigInteger expected, java.math.BigInteger number_propagations) {
        double weight = (double)((double)((double)(2.0) * (double)((((Number)(randint(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(100)))).doubleValue()))) - (double)(1.0));
        double layer_1_1 = (double)(0.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(number_propagations) < 0) {
            layer_1_1 = (double)(sigmoid((double)((double)(INITIAL_VALUE) * (double)(weight))));
            double layer_1_error_1 = (double)((double)(((double)(((Number)(expected)).doubleValue()) / (double)(100.0))) - (double)(layer_1_1));
            double layer_1_delta_1 = (double)((double)(layer_1_error_1) * (double)(sigmoid_derivative((double)(layer_1_1))));
            weight = (double)((double)(weight) + (double)((double)(INITIAL_VALUE) * (double)(layer_1_delta_1)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)((double)(layer_1_1) * (double)(100.0));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = java.math.BigInteger.valueOf(1);
            result = (double)(forward_propagation(java.math.BigInteger.valueOf(32), java.math.BigInteger.valueOf(450000)));
            System.out.println(result);
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
