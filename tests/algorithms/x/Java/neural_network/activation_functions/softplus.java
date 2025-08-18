public class Main {

    static double ln(double x) {
        if ((double)(x) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double y2_1 = (double)((double)(y_1) * (double)(y_1));
        double term_1 = (double)(y_1);
        double sum_1 = (double)(0.0);
        long k_1 = 0L;
        while ((long)(k_1) < 10L) {
            double denom_1 = (double)(((Number)(((long)(2L * (long)(k_1)) + 1L))).doubleValue());
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)(denom_1)));
            term_1 = (double)((double)(term_1) * (double)(y2_1));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double exp(double x) {
        double term_2 = (double)(1.0);
        double sum_3 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term_2 = (double)((double)((double)(term_2) * (double)(x)) / (double)((((Number)(n_1)).doubleValue())));
            sum_3 = (double)((double)(sum_3) + (double)(term_2));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_3;
    }

    static double[] softplus(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vector.length)) {
            double x_1 = (double)(vector[(int)((long)(i_1))]);
            double value_1 = (double)(Math.log((double)(1.0) + (double)(Math.exp(x_1))));
            result = ((double[])(appendDouble(result, (double)(value_1))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static void main() {
        double[] v1 = ((double[])(new double[]{2.3, 0.6, -2.0, -3.8}));
        double[] v2_1 = ((double[])(new double[]{-9.2, -0.3, 0.45, -4.56}));
        double[] r1_1 = ((double[])(softplus(((double[])(v1)))));
        double[] r2_1 = ((double[])(softplus(((double[])(v2_1)))));
        System.out.println(java.util.Arrays.toString(r1_1));
        System.out.println(java.util.Arrays.toString(r2_1));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
