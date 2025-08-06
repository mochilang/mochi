public class Main {
    static double[][] control;

    static double n_choose_k(int n, int k) {
        if (k < 0 || k > n) {
            return 0.0;
        }
        if (k == 0 || k == n) {
            return 1.0;
        }
        double result = 1.0;
        int i = 1;
        while (i <= k) {
            result = result * (1.0 * (n - k + i)) / (1.0 * i);
            i = i + 1;
        }
        return result;
    }

    static double pow_float(double base, int exp) {
        double result_1 = 1.0;
        int i_1 = 0;
        while (i_1 < exp) {
            result_1 = result_1 * base;
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static double[] basis_function(double[][] points, double t) {
        int degree = points.length - 1;
        double[] res = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 <= degree) {
            double coef = n_choose_k(degree, i_2);
            double term = pow_float(1.0 - t, degree - i_2) * pow_float(t, i_2);
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(coef * term)).toArray()));
            i_2 = i_2 + 1;
        }
        return res;
    }

    static double[] bezier_point(double[][] points, double t) {
        double[] basis = ((double[])(basis_function(((double[][])(points)), t)));
        double x = 0.0;
        double y = 0.0;
        int i_3 = 0;
        while (i_3 < points.length) {
            x = x + basis[i_3] * points[i_3][0];
            y = y + basis[i_3] * points[i_3][1];
            i_3 = i_3 + 1;
        }
        return new double[]{x, y};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            control = ((double[][])(new double[][]{new double[]{1.0, 1.0}, new double[]{1.0, 2.0}}));
            System.out.println(_p(basis_function(((double[][])(control)), 0.0)));
            System.out.println(_p(basis_function(((double[][])(control)), 1.0)));
            System.out.println(_p(bezier_point(((double[][])(control)), 0.0)));
            System.out.println(_p(bezier_point(((double[][])(control)), 1.0)));
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
