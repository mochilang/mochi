public class Main {
    static double[] sample;

    static double exp_taylor(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        double i_1 = (double)(1.0);
        while ((double)(i_1) < (double)(20.0)) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)(i_1));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = (double)((double)(i_1) + (double)(1.0));
        }
        return (double)(sum_1);
    }

    static double[] sigmoid(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(vector.length))) < 0) {
            double x_1 = (double)(vector[_idx((vector).length, ((java.math.BigInteger)(i_3)).longValue())]);
            double value_1 = (double)((double)(1.0) / (double)(((double)(1.0) + (double)(exp_taylor((double)(-x_1))))));
            result = ((double[])(appendDouble(result, (double)(value_1))));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(result));
    }

    static double[] gaussian_error_linear_unit(double[] vector) {
        double[] result_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(vector.length))) < 0) {
            double x_3 = (double)(vector[_idx((vector).length, ((java.math.BigInteger)(i_5)).longValue())]);
            double gelu_1 = (double)((double)(x_3) * (double)(((double)(1.0) / (double)(((double)(1.0) + (double)(exp_taylor((double)((double)(-1.702) * (double)(x_3)))))))));
            result_1 = ((double[])(appendDouble(result_1, (double)(gelu_1))));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(result_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sample = ((double[])(new double[]{(double)(-1.0), (double)(1.0), (double)(2.0)}));
            System.out.println(java.util.Arrays.toString(sigmoid(((double[])(sample)))));
            System.out.println(java.util.Arrays.toString(gaussian_error_linear_unit(((double[])(sample)))));
            System.out.println(java.util.Arrays.toString(gaussian_error_linear_unit(((double[])(new double[]{(double)(-3.0)})))));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
