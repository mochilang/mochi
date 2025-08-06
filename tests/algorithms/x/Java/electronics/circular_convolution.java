public class Main {
    static double[] example1;
    static double[] example2;
    static double[] example3;
    static double[] example4;

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            p = p * 10.0;
            i_1 = i_1 + 1;
        }
        return p;
    }

    static double roundn(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static double[] pad(double[] signal, int target) {
        double[] s = ((double[])(signal));
        while (s.length < target) {
            s = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(s), java.util.stream.DoubleStream.of(0.0)).toArray()));
        }
        return s;
    }

    static double[] circular_convolution(double[] a, double[] b) {
        int n1 = a.length;
        int n2 = b.length;
        int n = n1 > n2 ? n1 : n2;
        double[] x = ((double[])(pad(((double[])(a)), n)));
        double[] y = ((double[])(pad(((double[])(b)), n)));
        double[] res = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < n) {
            double sum = 0.0;
            int k = 0;
            while (k < n) {
                int j = Math.floorMod((i_2 - k), n);
                int idx = j < 0 ? j + n : j;
                sum = sum + x[k] * y[idx];
                k = k + 1;
            }
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(roundn(sum, 2))).toArray()));
            i_2 = i_2 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example1 = ((double[])(circular_convolution(((double[])(new double[]{2.0, 1.0, 2.0, -1.0})), ((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
            System.out.println(_p(example1));
            example2 = ((double[])(circular_convolution(((double[])(new double[]{0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6})), ((double[])(new double[]{0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5})))));
            System.out.println(_p(example2));
            example3 = ((double[])(circular_convolution(((double[])(new double[]{-1.0, 1.0, 2.0, -2.0})), ((double[])(new double[]{0.5, 1.0, -1.0, 2.0, 0.75})))));
            System.out.println(_p(example3));
            example4 = ((double[])(circular_convolution(((double[])(new double[]{1.0, -1.0, 2.0, 3.0, -1.0})), ((double[])(new double[]{1.0, 2.0, 3.0})))));
            System.out.println(_p(example4));
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
