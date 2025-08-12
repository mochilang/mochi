public class Main {
    static long seed = 0;
    static double INITIAL_VALUE;
    static double result;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)(seed * (long)(1103515245)) + (long)(12345)))), 2147483648L))));
        return seed;
    }

    static long randint(long low, long high) {
        return (long)((Math.floorMod(rand(), ((long)(high - low) + (long)(1))))) + low;
    }

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < 0.0) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1L;
        while (n_1 < (long)(30)) {
            term_1 = (double)(term_1) * (double)(y) / (((Number)(n_1)).doubleValue());
            sum_1 = (double)(sum_1) + (double)(term_1);
            n_1 = (long)(n_1 + (long)(1));
        }
        if (is_neg_1) {
            return 1.0 / (double)(sum_1);
        }
        return sum_1;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + (double)(expApprox((double)(-x))));
    }

    static double sigmoid_derivative(double sig_val) {
        return (double)(sig_val) * (1.0 - (double)(sig_val));
    }

    static double forward_propagation(long expected, long number_propagations) {
        double weight = 2.0 * (((Number)(randint(1L, 100L))).doubleValue()) - 1.0;
        double layer_1_1 = 0.0;
        long i_1 = 0L;
        while (i_1 < number_propagations) {
            layer_1_1 = (double)(sigmoid((double)(INITIAL_VALUE) * (double)(weight)));
            double layer_1_error_1 = (((Number)(expected)).doubleValue() / 100.0) - (double)(layer_1_1);
            double layer_1_delta_1 = (double)(layer_1_error_1) * (double)(sigmoid_derivative((double)(layer_1_1)));
            weight = (double)(weight) + (double)(INITIAL_VALUE) * (double)(layer_1_delta_1);
            i_1 = (long)(i_1 + (long)(1));
        }
        return (double)(layer_1_1) * 100.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1L;
            INITIAL_VALUE = 0.02;
            seed = 1L;
            result = (double)(forward_propagation(32L, 450000L));
            System.out.println(result);
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
