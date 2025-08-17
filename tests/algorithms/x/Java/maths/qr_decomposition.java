public class Main {
    static class QR {
        double[][] q;
        double[][] r;
        QR(double[][] q, double[][] r) {
            this.q = q;
            this.r = r;
        }
        QR() {}
        @Override public String toString() {
            return String.format("{'q': %s, 'r': %s}", String.valueOf(q), String.valueOf(r));
        }
    }

    static double[][] A;
    static QR result;

    static double sqrt_approx(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double sign(double x) {
        if ((double)(x) >= (double)(0.0)) {
            return 1.0;
        } else {
            return -1.0;
        }
    }

    static double vector_norm(double[] v) {
        double sum = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(v.length)) {
            sum = (double)((double)(sum) + (double)((double)(v[(int)((long)(i_3))]) * (double)(v[(int)((long)(i_3))])));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double n_1 = (double)(sqrt_approx((double)(sum)));
        return n_1;
    }

    static double[][] identity_matrix(long n) {
        double[][] mat = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(n)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(n)) {
                if ((long)(i_5) == (long)(j_1)) {
                    row_1 = ((double[])(appendDouble(row_1, (double)(1.0))));
                } else {
                    row_1 = ((double[])(appendDouble(row_1, (double)(0.0))));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            mat = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return mat;
    }

    static double[][] copy_matrix(double[][] a) {
        double[][] mat_1 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(a.length)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(a[(int)((long)(i_7))].length)) {
                row_3 = ((double[])(appendDouble(row_3, (double)(a[(int)((long)(i_7))][(int)((long)(j_3))]))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            mat_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_1), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return mat_1;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        long m = (long)(a.length);
        long n_3 = (long)(a[(int)(0L)].length);
        long p_1 = (long)(b[(int)(0L)].length);
        double[][] res_1 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(m)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(p_1)) {
                double sum_2 = (double)(0.0);
                long k_1 = 0L;
                while ((long)(k_1) < (long)(n_3)) {
                    sum_2 = (double)((double)(sum_2) + (double)((double)(a[(int)((long)(i_9))][(int)((long)(k_1))]) * (double)(b[(int)((long)(k_1))][(int)((long)(j_5))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_5 = ((double[])(appendDouble(row_5, (double)(sum_2))));
                j_5 = (long)((long)(j_5) + 1L);
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return res_1;
    }

    static QR qr_decomposition(double[][] a) {
        long m_1 = (long)(a.length);
        long n_5 = (long)(a[(int)(0L)].length);
        long t_1 = (long)((long)(m_1) < (long)(n_5) ? m_1 : n_5);
        double[][] q_1 = ((double[][])(identity_matrix((long)(m_1))));
        double[][] r_1 = ((double[][])(copy_matrix(((double[][])(a)))));
        long k_3 = 0L;
        while ((long)(k_3) < (long)((long)(t_1) - 1L)) {
            double[] x_1 = ((double[])(new double[]{}));
            long i_11 = (long)(k_3);
            while ((long)(i_11) < (long)(m_1)) {
                x_1 = ((double[])(appendDouble(x_1, (double)(r_1[(int)((long)(i_11))][(int)((long)(k_3))]))));
                i_11 = (long)((long)(i_11) + 1L);
            }
            double[] e1_1 = ((double[])(new double[]{}));
            i_11 = 0L;
            while ((long)(i_11) < (long)(x_1.length)) {
                if ((long)(i_11) == 0L) {
                    e1_1 = ((double[])(appendDouble(e1_1, (double)(1.0))));
                } else {
                    e1_1 = ((double[])(appendDouble(e1_1, (double)(0.0))));
                }
                i_11 = (long)((long)(i_11) + 1L);
            }
            double alpha_1 = (double)(vector_norm(((double[])(x_1))));
            double s_1 = (double)((double)(sign((double)(x_1[(int)(0L)]))) * (double)(alpha_1));
            double[] v_1 = ((double[])(new double[]{}));
            i_11 = 0L;
            while ((long)(i_11) < (long)(x_1.length)) {
                v_1 = ((double[])(appendDouble(v_1, (double)((double)(x_1[(int)((long)(i_11))]) + (double)((double)(s_1) * (double)(e1_1[(int)((long)(i_11))]))))));
                i_11 = (long)((long)(i_11) + 1L);
            }
            double vnorm_1 = (double)(vector_norm(((double[])(v_1))));
            i_11 = 0L;
            while ((long)(i_11) < (long)(v_1.length)) {
v_1[(int)((long)(i_11))] = (double)((double)(v_1[(int)((long)(i_11))]) / (double)(vnorm_1));
                i_11 = (long)((long)(i_11) + 1L);
            }
            long size_1 = (long)(v_1.length);
            double[][] qk_small_1 = ((double[][])(new double[][]{}));
            i_11 = 0L;
            while ((long)(i_11) < (long)(size_1)) {
                double[] row_7 = ((double[])(new double[]{}));
                long j_8 = 0L;
                while ((long)(j_8) < (long)(size_1)) {
                    double delta_1 = (double)((long)(i_11) == (long)(j_8) ? 1.0 : 0.0);
                    row_7 = ((double[])(appendDouble(row_7, (double)((double)(delta_1) - (double)((double)((double)(2.0) * (double)(v_1[(int)((long)(i_11))])) * (double)(v_1[(int)((long)(j_8))]))))));
                    j_8 = (long)((long)(j_8) + 1L);
                }
                qk_small_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(qk_small_1), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
                i_11 = (long)((long)(i_11) + 1L);
            }
            double[][] qk_1 = ((double[][])(identity_matrix((long)(m_1))));
            i_11 = 0L;
            while ((long)(i_11) < (long)(size_1)) {
                long j_9 = 0L;
                while ((long)(j_9) < (long)(size_1)) {
qk_1[(int)((long)((long)(k_3) + (long)(i_11)))][(int)((long)((long)(k_3) + (long)(j_9)))] = (double)(qk_small_1[(int)((long)(i_11))][(int)((long)(j_9))]);
                    j_9 = (long)((long)(j_9) + 1L);
                }
                i_11 = (long)((long)(i_11) + 1L);
            }
            q_1 = ((double[][])(matmul(((double[][])(q_1)), ((double[][])(qk_1)))));
            r_1 = ((double[][])(matmul(((double[][])(qk_1)), ((double[][])(r_1)))));
            k_3 = (long)((long)(k_3) + 1L);
        }
        return new QR(q_1, r_1);
    }

    static void print_matrix(double[][] mat) {
        long i_12 = 0L;
        while ((long)(i_12) < (long)(mat.length)) {
            String line_1 = "";
            long j_11 = 0L;
            while ((long)(j_11) < (long)(mat[(int)((long)(i_12))].length)) {
                line_1 = line_1 + _p(_getd(mat[(int)((long)(i_12))], ((Number)(j_11)).intValue()));
                if ((long)((long)(j_11) + 1L) < (long)(mat[(int)((long)(i_12))].length)) {
                    line_1 = line_1 + " ";
                }
                j_11 = (long)((long)(j_11) + 1L);
            }
            System.out.println(line_1);
            i_12 = (long)((long)(i_12) + 1L);
        }
    }
    public static void main(String[] args) {
        A = ((double[][])(new double[][]{new double[]{12.0, -51.0, 4.0}, new double[]{6.0, 167.0, -68.0}, new double[]{-4.0, 24.0, -41.0}}));
        result = qr_decomposition(((double[][])(A)));
        print_matrix(((double[][])(result.q)));
        print_matrix(((double[][])(result.r)));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
