public class Main {

    static double exp_approx(double x) {
        double sum = 1.0;
        double term = 1.0;
        int i = 1;
        double absx = x < 0.0 ? -x : x;
        while (i <= 20) {
            term = term * absx / (((Number)(i)).doubleValue());
            sum = sum + term;
            i = i + 1;
        }
        if (x < 0.0) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double[] exponential_linear_unit(double[] vector, double alpha) {
        double[] result = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < vector.length) {
            double v = vector[i_1];
            if (v > 0.0) {
                result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(v)).toArray()));
            } else {
                double neg = alpha * (exp_approx(v) - 1.0);
                result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(neg)).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(exponential_linear_unit(((double[])(new double[]{2.3, 0.6, -2.0, -3.8})), 0.3)));
            System.out.println(_p(exponential_linear_unit(((double[])(new double[]{-9.2, -0.3, 0.45, -4.56})), 0.067)));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
