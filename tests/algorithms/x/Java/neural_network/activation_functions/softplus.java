public class Main {

    static double ln(double x) {
        if (x <= 0.0) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y = (x - 1.0) / (x + 1.0);
        double y2 = y * y;
        double term = y;
        double sum = 0.0;
        int k = 0;
        while (k < 10) {
            double denom = ((Number)((2 * k + 1))).doubleValue();
            sum = sum + term / denom;
            term = term * y2;
            k = k + 1;
        }
        return 2.0 * sum;
    }

    static double exp(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int n = 1;
        while (n < 20) {
            term_1 = term_1 * x / (((Number)(n)).doubleValue());
            sum_1 = sum_1 + term_1;
            n = n + 1;
        }
        return sum_1;
    }

    static double[] softplus(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        int i = 0;
        while (i < vector.length) {
            double x = vector[i];
            double value = ln(1.0 + exp(x));
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(value)).toArray()));
            i = i + 1;
        }
        return result;
    }

    static void main() {
        double[] v1 = ((double[])(new double[]{2.3, 0.6, -2.0, -3.8}));
        double[] v2 = ((double[])(new double[]{-9.2, -0.3, 0.45, -4.56}));
        double[] r1 = ((double[])(softplus(((double[])(v1)))));
        double[] r2 = ((double[])(softplus(((double[])(v2)))));
        System.out.println(java.util.Arrays.toString(r1));
        System.out.println(java.util.Arrays.toString(r2));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
