public class Main {

    static double exp(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1L;
        while ((long)(n_1) < (long)(20)) {
            term = term * (double)(x) / (((Number)(n_1)).doubleValue());
            sum_1 = sum_1 + term;
            n_1 = (long)((long)(n_1) + (long)(1));
        }
        return sum_1;
    }

    static double[] scaled_exponential_linear_unit(double[] vector, double alpha, double lambda_) {
        double[] result = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vector.length)) {
            double x_1 = (double)(vector[(int)((long)(i_1))]);
            double y_1 = (double)(x_1) > 0.0 ? (double)(lambda_) * (double)(x_1) : (double)(lambda_) * (double)(alpha) * ((double)(exp((double)(x_1))) - 1.0);
            result = ((double[])(appendDouble(result, y_1)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(scaled_exponential_linear_unit(((double[])(new double[]{1.3, 3.7, 2.4})), 1.6732, 1.0507));
            System.out.println(scaled_exponential_linear_unit(((double[])(new double[]{1.3, 4.7, 8.2})), 1.6732, 1.0507));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
