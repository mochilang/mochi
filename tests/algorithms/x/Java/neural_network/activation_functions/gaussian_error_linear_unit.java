public class Main {
    static double[] sample;

    static double exp_taylor(double x) {
        double term = 1.0;
        double sum = 1.0;
        double i = 1.0;
        while (i < 20.0) {
            term = term * x / i;
            sum = sum + term;
            i = i + 1.0;
        }
        return sum;
    }

    static double[] sigmoid(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < vector.length) {
            double x = vector[i_1];
            double value = 1.0 / (1.0 + exp_taylor(-x));
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(value)).toArray()));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double[] gaussian_error_linear_unit(double[] vector) {
        double[] result_1 = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < vector.length) {
            double x_1 = vector[i_2];
            double gelu = x_1 * (1.0 / (1.0 + exp_taylor(-1.702 * x_1)));
            result_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_1), java.util.stream.DoubleStream.of(gelu)).toArray()));
            i_2 = i_2 + 1;
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sample = ((double[])(new double[]{-1.0, 1.0, 2.0}));
            System.out.println(sigmoid(((double[])(sample))));
            System.out.println(gaussian_error_linear_unit(((double[])(sample))));
            System.out.println(gaussian_error_linear_unit(((double[])(new double[]{-3.0}))));
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
