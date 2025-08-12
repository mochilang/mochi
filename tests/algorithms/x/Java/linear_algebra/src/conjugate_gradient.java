public class Main {
    static double[][] A;
    static double[] b;
    static double[] x_2;

    static double[] zeros(long n) {
        double[] res = ((double[])(new double[]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            res = ((double[])(appendDouble(res, 0.0)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static double dot(double[] a, double[] b) {
        double sum = 0.0;
        long i_3 = 0L;
        while (i_3 < a.length) {
            sum = sum + a[(int)((long)(i_3))] * b[(int)((long)(i_3))];
            i_3 = i_3 + 1;
        }
        return sum;
    }

    static double[] mat_vec_mul(double[][] m, double[] v) {
        double[] res_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while (i_5 < m.length) {
            double s_1 = 0.0;
            long j_1 = 0L;
            while (j_1 < m[(int)((long)(i_5))].length) {
                s_1 = s_1 + m[(int)((long)(i_5))][(int)((long)(j_1))] * v[(int)((long)(j_1))];
                j_1 = j_1 + 1;
            }
            res_1 = ((double[])(appendDouble(res_1, s_1)));
            i_5 = i_5 + 1;
        }
        return res_1;
    }

    static double[] vec_add(double[] a, double[] b) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while (i_7 < a.length) {
            res_2 = ((double[])(appendDouble(res_2, a[(int)((long)(i_7))] + b[(int)((long)(i_7))])));
            i_7 = i_7 + 1;
        }
        return res_2;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_3 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while (i_9 < a.length) {
            res_3 = ((double[])(appendDouble(res_3, a[(int)((long)(i_9))] - b[(int)((long)(i_9))])));
            i_9 = i_9 + 1;
        }
        return res_3;
    }

    static double[] scalar_mul(double s, double[] v) {
        double[] res_4 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while (i_11 < v.length) {
            res_4 = ((double[])(appendDouble(res_4, s * v[(int)((long)(i_11))])));
            i_11 = i_11 + 1;
        }
        return res_4;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_13 = 0L;
        while (i_13 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_13 = i_13 + 1;
        }
        return guess_1;
    }

    static double norm(double[] v) {
        return sqrtApprox(dot(((double[])(v)), ((double[])(v))));
    }

    static double[] conjugate_gradient(double[][] A, double[] b, long max_iterations, double tol) {
        long n = b.length;
        double[] x_1 = ((double[])(zeros(n)));
        double[] r_1 = ((double[])(vec_sub(((double[])(b)), ((double[])(mat_vec_mul(((double[][])(A)), ((double[])(x_1))))))));
        double[] p_1 = ((double[])(r_1));
        double rs_old_1 = dot(((double[])(r_1)), ((double[])(r_1)));
        long i_15 = 0L;
        while (i_15 < max_iterations) {
            double[] Ap_1 = ((double[])(mat_vec_mul(((double[][])(A)), ((double[])(p_1)))));
            double alpha_1 = rs_old_1 / dot(((double[])(p_1)), ((double[])(Ap_1)));
            x_1 = ((double[])(vec_add(((double[])(x_1)), ((double[])(scalar_mul(alpha_1, ((double[])(p_1))))))));
            r_1 = ((double[])(vec_sub(((double[])(r_1)), ((double[])(scalar_mul(alpha_1, ((double[])(Ap_1))))))));
            double rs_new_1 = dot(((double[])(r_1)), ((double[])(r_1)));
            if (sqrtApprox(rs_new_1) < tol) {
                break;
            }
            double beta_1 = rs_new_1 / rs_old_1;
            p_1 = ((double[])(vec_add(((double[])(r_1)), ((double[])(scalar_mul(beta_1, ((double[])(p_1))))))));
            rs_old_1 = rs_new_1;
            i_15 = i_15 + 1;
        }
        return x_1;
    }
    public static void main(String[] args) {
        A = ((double[][])(new double[][]{new double[]{8.73256573, -5.02034289, -2.68709226}, new double[]{-5.02034289, 3.78188322, 0.91980451}, new double[]{-2.68709226, 0.91980451, 1.94746467}}));
        b = ((double[])(new double[]{-5.80872761, 3.23807431, 1.95381422}));
        x_2 = ((double[])(conjugate_gradient(((double[][])(A)), ((double[])(b)), 1000L, 1e-08)));
        System.out.println(_p(_getd(x_2, ((Number)(0)).intValue())));
        System.out.println(_p(_getd(x_2, ((Number)(1)).intValue())));
        System.out.println(_p(_getd(x_2, ((Number)(2)).intValue())));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
