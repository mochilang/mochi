public class Main {
    static double[][] A;
    static double[] b;
    static double[] x_1;

    static double[] zeros(int n) {
        double[] res = ((double[])(new double[]{}));
        int i = 0;
        while (i < n) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i = i + 1;
        }
        return res;
    }

    static double dot(double[] a, double[] b) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < a.length) {
            sum = sum + a[i_1] * b[i_1];
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double[] mat_vec_mul(double[][] m, double[] v) {
        double[] res_1 = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < m.length) {
            double s = 0.0;
            int j = 0;
            while (j < m[i_2].length) {
                s = s + m[i_2][j] * v[j];
                j = j + 1;
            }
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(s)).toArray()));
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static double[] vec_add(double[] a, double[] b) {
        double[] res_2 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < a.length) {
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(a[i_3] + b[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_3 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < a.length) {
            res_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_3), java.util.stream.DoubleStream.of(a[i_4] - b[i_4])).toArray()));
            i_4 = i_4 + 1;
        }
        return res_3;
    }

    static double[] scalar_mul(double s, double[] v) {
        double[] res_4 = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < v.length) {
            res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(s * v[i_5])).toArray()));
            i_5 = i_5 + 1;
        }
        return res_4;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i_6 = 0;
        while (i_6 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_6 = i_6 + 1;
        }
        return guess;
    }

    static double norm(double[] v) {
        return sqrtApprox(dot(((double[])(v)), ((double[])(v))));
    }

    static double[] conjugate_gradient(double[][] A, double[] b, int max_iterations, double tol) {
        int n = b.length;
        double[] x = ((double[])(zeros(n)));
        double[] r = ((double[])(vec_sub(((double[])(b)), ((double[])(mat_vec_mul(((double[][])(A)), ((double[])(x))))))));
        double[] p = ((double[])(r));
        double rs_old = dot(((double[])(r)), ((double[])(r)));
        int i_7 = 0;
        while (i_7 < max_iterations) {
            double[] Ap = ((double[])(mat_vec_mul(((double[][])(A)), ((double[])(p)))));
            double alpha = rs_old / dot(((double[])(p)), ((double[])(Ap)));
            x = ((double[])(vec_add(((double[])(x)), ((double[])(scalar_mul(alpha, ((double[])(p))))))));
            r = ((double[])(vec_sub(((double[])(r)), ((double[])(scalar_mul(alpha, ((double[])(Ap))))))));
            double rs_new = dot(((double[])(r)), ((double[])(r)));
            if (sqrtApprox(rs_new) < tol) {
                break;
            }
            double beta = rs_new / rs_old;
            p = ((double[])(vec_add(((double[])(r)), ((double[])(scalar_mul(beta, ((double[])(p))))))));
            rs_old = rs_new;
            i_7 = i_7 + 1;
        }
        return x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            A = ((double[][])(new double[][]{new double[]{8.73256573, -5.02034289, -2.68709226}, new double[]{-5.02034289, 3.78188322, 0.91980451}, new double[]{-2.68709226, 0.91980451, 1.94746467}}));
            b = ((double[])(new double[]{-5.80872761, 3.23807431, 1.95381422}));
            x_1 = ((double[])(conjugate_gradient(((double[][])(A)), ((double[])(b)), 1000, 1e-08)));
            System.out.println(_p(_geto(x_1, 0)));
            System.out.println(_p(_geto(x_1, 1)));
            System.out.println(_p(_geto(x_1, 2)));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
