public class Main {

    static double exp(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 20) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double[] scaled_exponential_linear_unit(double[] vector, double alpha, double lambda_) {
        double[] result = ((double[])(new double[]{}));
        int i = 0;
        while (i < vector.length) {
            double x = vector[i];
            double y = x > 0.0 ? lambda_ * x : lambda_ * alpha * (exp(x) - 1.0);
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(y)).toArray()));
            i = i + 1;
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
}
