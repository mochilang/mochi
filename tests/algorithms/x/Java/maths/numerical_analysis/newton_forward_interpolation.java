public class Main {
    static double[] x_points;
    static double[] y_points;

    static double ucal(double u, int p) {
        double temp = u;
        int i = 1;
        while (i < p) {
            temp = temp * (u - (((Number)(i)).doubleValue()));
            i = i + 1;
        }
        return temp;
    }

    static double factorial(int n) {
        double result = 1.0;
        int i_1 = 2;
        while (i_1 <= n) {
            result = result * (((Number)(i_1)).doubleValue());
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double newton_forward_interpolation(double[] x, double[] y0, double value) {
        int n = x.length;
        double[][] y = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < n) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            y = ((double[][])(appendObj(y, row)));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < n) {
y[i_2][0] = y0[i_2];
            i_2 = i_2 + 1;
        }
        int i1 = 1;
        while (i1 < n) {
            int j1 = 0;
            while (j1 < n - i1) {
y[j1][i1] = y[j1 + 1][i1 - 1] - y[j1][i1 - 1];
                j1 = j1 + 1;
            }
            i1 = i1 + 1;
        }
        double u = (value - x[0]) / (x[1] - x[0]);
        double sum = y[0][0];
        int k = 1;
        while (k < n) {
            sum = sum + (ucal(u, k) * y[0][k]) / factorial(k);
            k = k + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            x_points = ((double[])(new double[]{0.0, 1.0, 2.0, 3.0}));
            y_points = ((double[])(new double[]{0.0, 1.0, 8.0, 27.0}));
            System.out.println(_p(newton_forward_interpolation(((double[])(x_points)), ((double[])(y_points)), 1.5)));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
