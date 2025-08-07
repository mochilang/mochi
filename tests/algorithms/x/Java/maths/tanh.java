public class Main {

    static double expApprox(double x) {
        boolean neg = false;
        double y = x;
        if (x < 0.0) {
            neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (neg) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double[] tangent_hyperbolic(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        int i = 0;
        while (i < vector.length) {
            double x = vector[i];
            double t = (2.0 / (1.0 + expApprox(-2.0 * x))) - 1.0;
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(t)).toArray()));
            i = i + 1;
        }
        return result;
    }

    static void main() {
        double[] v1 = ((double[])(new double[]{1.0, 5.0, 6.0, -0.67}));
        double[] v2 = ((double[])(new double[]{8.0, 10.0, 2.0, -0.98, 13.0}));
        System.out.println(_p(tangent_hyperbolic(((double[])(v1)))));
        System.out.println(_p(tangent_hyperbolic(((double[])(v2)))));
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
