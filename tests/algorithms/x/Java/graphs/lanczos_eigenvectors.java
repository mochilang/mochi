public class Main {
    static long seed = 123456789L;
    static class LanczosResult {
        double[][] t;
        double[][] q;
        LanczosResult(double[][] t, double[][] q) {
            this.t = t;
            this.q = q;
        }
        LanczosResult() {}
        @Override public String toString() {
            return String.format("{'t': %s, 'q': %s}", String.valueOf(t), String.valueOf(q));
        }
    }

    static class EigenResult {
        double[] values;
        double[][] vectors;
        EigenResult(double[] values, double[][] vectors) {
            this.values = values;
            this.vectors = vectors;
        }
        EigenResult() {}
        @Override public String toString() {
            return String.format("{'values': %s, 'vectors': %s}", String.valueOf(values), String.valueOf(vectors));
        }
    }

    static long[][] graph;
    static EigenResult result_2;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return seed;
    }

    static double random() {
        return (double)(((double)(1.0) * (double)(rand()))) / (double)(2147483648.0);
    }

    static double sqrtApprox(double x) {
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

    static double absf(double x) {
        return (double)(x) < (double)(0.0) ? -x : x;
    }

    static double dot(double[] a, double[] b) {
        double s = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(a.length)) {
            s = (double)((double)(s) + (double)((double)(a[(int)((long)(i_3))]) * (double)(b[(int)((long)(i_3))])));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return s;
    }

    static double[] vector_scale(double[] v, double s) {
        double[] res = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(v.length)) {
            res = ((double[])(appendDouble(res, (double)((double)(v[(int)((long)(i_5))]) * (double)(s)))));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return res;
    }

    static double[] vector_sub(double[] a, double[] b) {
        double[] res_1 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(a.length)) {
            res_1 = ((double[])(appendDouble(res_1, (double)((double)(a[(int)((long)(i_7))]) - (double)(b[(int)((long)(i_7))])))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return res_1;
    }

    static double[] vector_add(double[] a, double[] b) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(a.length)) {
            res_2 = ((double[])(appendDouble(res_2, (double)((double)(a[(int)((long)(i_9))]) + (double)(b[(int)((long)(i_9))])))));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return res_2;
    }

    static double[][] zeros_matrix(long r, long c) {
        double[][] m = ((double[][])(new double[][]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(r)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(c)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(0.0))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            m = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(m), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return m;
    }

    static double[] column(double[][] m, long idx) {
        double[] col = ((double[])(new double[]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(m.length)) {
            col = ((double[])(appendDouble(col, (double)(m[(int)((long)(i_13))][(int)((long)(idx))]))));
            i_13 = (long)((long)(i_13) + 1L);
        }
        return col;
    }

    static void validate_adjacency_list(long[][] graph) {
        long i_14 = 0L;
        while ((long)(i_14) < (long)(graph.length)) {
            long j_3 = 0L;
            while ((long)(j_3) < (long)(graph[(int)((long)(i_14))].length)) {
                long v_1 = (long)(graph[(int)((long)(i_14))][(int)((long)(j_3))]);
                if ((long)(v_1) < 0L || (long)(v_1) >= (long)(graph.length)) {
                    throw new RuntimeException(String.valueOf("Invalid neighbor"));
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_14 = (long)((long)(i_14) + 1L);
        }
    }

    static double[] multiply_matrix_vector(long[][] graph, double[] vector) {
        long n = (long)(graph.length);
        if ((long)(vector.length) != (long)(n)) {
            throw new RuntimeException(String.valueOf("Vector length must match number of nodes"));
        }
        double[] result_1 = ((double[])(new double[]{}));
        long i_16 = 0L;
        while ((long)(i_16) < (long)(n)) {
            double sum_1 = (double)(0.0);
            long j_5 = 0L;
            while ((long)(j_5) < (long)(graph[(int)((long)(i_16))].length)) {
                long nb_1 = (long)(graph[(int)((long)(i_16))][(int)((long)(j_5))]);
                sum_1 = (double)((double)(sum_1) + (double)(vector[(int)((long)(nb_1))]));
                j_5 = (long)((long)(j_5) + 1L);
            }
            result_1 = ((double[])(appendDouble(result_1, (double)(sum_1))));
            i_16 = (long)((long)(i_16) + 1L);
        }
        return result_1;
    }

    static LanczosResult lanczos_iteration(long[][] graph, long k) {
        long n_1 = (long)(graph.length);
        if ((long)(k) < 1L || (long)(k) > (long)(n_1)) {
            throw new RuntimeException(String.valueOf("invalid number of eigenvectors"));
        }
        double[][] q_1 = ((double[][])(zeros_matrix((long)(n_1), (long)(k))));
        double[][] t_1 = ((double[][])(zeros_matrix((long)(k), (long)(k))));
        double[] v_3 = ((double[])(new double[]{}));
        long i_18 = 0L;
        while ((long)(i_18) < (long)(n_1)) {
            v_3 = ((double[])(appendDouble(v_3, (double)(random()))));
            i_18 = (long)((long)(i_18) + 1L);
        }
        double ss_1 = (double)(0.0);
        i_18 = 0L;
        while ((long)(i_18) < (long)(n_1)) {
            ss_1 = (double)((double)(ss_1) + (double)((double)(v_3[(int)((long)(i_18))]) * (double)(v_3[(int)((long)(i_18))])));
            i_18 = (long)((long)(i_18) + 1L);
        }
        double vnorm_1 = (double)(sqrtApprox((double)(ss_1)));
        i_18 = 0L;
        while ((long)(i_18) < (long)(n_1)) {
q_1[(int)((long)(i_18))][(int)((long)(0))] = (double)((double)(v_3[(int)((long)(i_18))]) / (double)(vnorm_1));
            i_18 = (long)((long)(i_18) + 1L);
        }
        double beta_1 = (double)(0.0);
        long j_7 = 0L;
        while ((long)(j_7) < (long)(k)) {
            double[] w_1 = ((double[])(multiply_matrix_vector(((long[][])(graph)), ((double[])(column(((double[][])(q_1)), (long)(j_7)))))));
            if ((long)(j_7) > 0L) {
                w_1 = ((double[])(vector_sub(((double[])(w_1)), ((double[])(vector_scale(((double[])(column(((double[][])(q_1)), (long)((long)(j_7) - 1L)))), (double)(beta_1)))))));
            }
            double alpha_1 = (double)(dot(((double[])(column(((double[][])(q_1)), (long)(j_7)))), ((double[])(w_1))));
            w_1 = ((double[])(vector_sub(((double[])(w_1)), ((double[])(vector_scale(((double[])(column(((double[][])(q_1)), (long)(j_7)))), (double)(alpha_1)))))));
            double ss2_1 = (double)(0.0);
            long p_1 = 0L;
            while ((long)(p_1) < (long)(n_1)) {
                ss2_1 = (double)((double)(ss2_1) + (double)((double)(w_1[(int)((long)(p_1))]) * (double)(w_1[(int)((long)(p_1))])));
                p_1 = (long)((long)(p_1) + 1L);
            }
            beta_1 = (double)(sqrtApprox((double)(ss2_1)));
t_1[(int)((long)(j_7))][(int)((long)(j_7))] = (double)(alpha_1);
            if ((long)(j_7) < (long)((long)(k) - 1L)) {
t_1[(int)((long)(j_7))][(int)((long)((long)(j_7) + 1L))] = (double)(beta_1);
t_1[(int)((long)((long)(j_7) + 1L))][(int)((long)(j_7))] = (double)(beta_1);
                if ((double)(beta_1) > (double)(1e-10)) {
                    double[] wnorm_1 = ((double[])(vector_scale(((double[])(w_1)), (double)((double)(1.0) / (double)(beta_1)))));
                    long r_1 = 0L;
                    while ((long)(r_1) < (long)(n_1)) {
q_1[(int)((long)(r_1))][(int)((long)((long)(j_7) + 1L))] = (double)(wnorm_1[(int)((long)(r_1))]);
                        r_1 = (long)((long)(r_1) + 1L);
                    }
                }
            }
            j_7 = (long)((long)(j_7) + 1L);
        }
        return new LanczosResult(t_1, q_1);
    }

    static EigenResult jacobi_eigen(double[][] a_in, long max_iter) {
        long n_2 = (long)(a_in.length);
        double[][] a_1 = ((double[][])(a_in));
        double[][] v_5 = ((double[][])(zeros_matrix((long)(n_2), (long)(n_2))));
        long i_20 = 0L;
        while ((long)(i_20) < (long)(n_2)) {
v_5[(int)((long)(i_20))][(int)((long)(i_20))] = (double)(1.0);
            i_20 = (long)((long)(i_20) + 1L);
        }
        long iter_1 = 0L;
        while ((long)(iter_1) < (long)(max_iter)) {
            long p_3 = 0L;
            long q_3 = 1L;
            double max_1 = (double)(absf((double)(a_1[(int)((long)(p_3))][(int)((long)(q_3))])));
            i_20 = 0L;
            while ((long)(i_20) < (long)(n_2)) {
                long j_9 = (long)((long)(i_20) + 1L);
                while ((long)(j_9) < (long)(n_2)) {
                    double val_1 = (double)(absf((double)(a_1[(int)((long)(i_20))][(int)((long)(j_9))])));
                    if ((double)(val_1) > (double)(max_1)) {
                        max_1 = (double)(val_1);
                        p_3 = (long)(i_20);
                        q_3 = (long)(j_9);
                    }
                    j_9 = (long)((long)(j_9) + 1L);
                }
                i_20 = (long)((long)(i_20) + 1L);
            }
            if ((double)(max_1) < (double)(1e-08)) {
                break;
            }
            double app_1 = (double)(a_1[(int)((long)(p_3))][(int)((long)(p_3))]);
            double aqq_1 = (double)(a_1[(int)((long)(q_3))][(int)((long)(q_3))]);
            double apq_1 = (double)(a_1[(int)((long)(p_3))][(int)((long)(q_3))]);
            double theta_1 = (double)((double)(((double)(aqq_1) - (double)(app_1))) / (double)(((double)(2.0) * (double)(apq_1))));
            double t_3 = (double)((double)(1.0) / (double)(((double)(absf((double)(theta_1))) + (double)(sqrtApprox((double)((double)((double)(theta_1) * (double)(theta_1)) + (double)(1.0)))))));
            if ((double)(theta_1) < (double)(0.0)) {
                t_3 = (double)(-t_3);
            }
            double c_1 = (double)((double)(1.0) / (double)(sqrtApprox((double)((double)(1.0) + (double)((double)(t_3) * (double)(t_3))))));
            double s_2 = (double)((double)(t_3) * (double)(c_1));
            double tau_1 = (double)((double)(s_2) / (double)(((double)(1.0) + (double)(c_1))));
a_1[(int)((long)(p_3))][(int)((long)(p_3))] = (double)((double)(app_1) - (double)((double)(t_3) * (double)(apq_1)));
a_1[(int)((long)(q_3))][(int)((long)(q_3))] = (double)((double)(aqq_1) + (double)((double)(t_3) * (double)(apq_1)));
a_1[(int)((long)(p_3))][(int)((long)(q_3))] = (double)(0.0);
a_1[(int)((long)(q_3))][(int)((long)(p_3))] = (double)(0.0);
            long k_1 = 0L;
            while ((long)(k_1) < (long)(n_2)) {
                if ((long)(k_1) != (long)(p_3) && (long)(k_1) != (long)(q_3)) {
                    double akp_1 = (double)(a_1[(int)((long)(k_1))][(int)((long)(p_3))]);
                    double akq_1 = (double)(a_1[(int)((long)(k_1))][(int)((long)(q_3))]);
a_1[(int)((long)(k_1))][(int)((long)(p_3))] = (double)((double)(akp_1) - (double)((double)(s_2) * (double)(((double)(akq_1) + (double)((double)(tau_1) * (double)(akp_1))))));
a_1[(int)((long)(p_3))][(int)((long)(k_1))] = (double)(a_1[(int)((long)(k_1))][(int)((long)(p_3))]);
a_1[(int)((long)(k_1))][(int)((long)(q_3))] = (double)((double)(akq_1) + (double)((double)(s_2) * (double)(((double)(akp_1) - (double)((double)(tau_1) * (double)(akq_1))))));
a_1[(int)((long)(q_3))][(int)((long)(k_1))] = (double)(a_1[(int)((long)(k_1))][(int)((long)(q_3))]);
                }
                k_1 = (long)((long)(k_1) + 1L);
            }
            k_1 = 0L;
            while ((long)(k_1) < (long)(n_2)) {
                double vkp_1 = (double)(v_5[(int)((long)(k_1))][(int)((long)(p_3))]);
                double vkq_1 = (double)(v_5[(int)((long)(k_1))][(int)((long)(q_3))]);
v_5[(int)((long)(k_1))][(int)((long)(p_3))] = (double)((double)(vkp_1) - (double)((double)(s_2) * (double)(((double)(vkq_1) + (double)((double)(tau_1) * (double)(vkp_1))))));
v_5[(int)((long)(k_1))][(int)((long)(q_3))] = (double)((double)(vkq_1) + (double)((double)(s_2) * (double)(((double)(vkp_1) - (double)((double)(tau_1) * (double)(vkq_1))))));
                k_1 = (long)((long)(k_1) + 1L);
            }
            iter_1 = (long)((long)(iter_1) + 1L);
        }
        double[] eigenvalues_1 = ((double[])(new double[]{}));
        i_20 = 0L;
        while ((long)(i_20) < (long)(n_2)) {
            eigenvalues_1 = ((double[])(appendDouble(eigenvalues_1, (double)(a_1[(int)((long)(i_20))][(int)((long)(i_20))]))));
            i_20 = (long)((long)(i_20) + 1L);
        }
        return new EigenResult(eigenvalues_1, v_5);
    }

    static double[][] matmul(double[][] a, double[][] b) {
        long rows = (long)(a.length);
        long cols_1 = (long)(b[(int)((long)(0))].length);
        long inner_1 = (long)(b.length);
        double[][] m_2 = ((double[][])(zeros_matrix((long)(rows), (long)(cols_1))));
        long i_22 = 0L;
        while ((long)(i_22) < (long)(rows)) {
            long j_11 = 0L;
            while ((long)(j_11) < (long)(cols_1)) {
                double s_4 = (double)(0.0);
                long k_3 = 0L;
                while ((long)(k_3) < (long)(inner_1)) {
                    s_4 = (double)((double)(s_4) + (double)((double)(a[(int)((long)(i_22))][(int)((long)(k_3))]) * (double)(b[(int)((long)(k_3))][(int)((long)(j_11))])));
                    k_3 = (long)((long)(k_3) + 1L);
                }
m_2[(int)((long)(i_22))][(int)((long)(j_11))] = (double)(s_4);
                j_11 = (long)((long)(j_11) + 1L);
            }
            i_22 = (long)((long)(i_22) + 1L);
        }
        return m_2;
    }

    static EigenResult sort_eigenpairs(double[] vals, double[][] vecs) {
        long n_3 = (long)(vals.length);
        double[] values_1 = ((double[])(vals));
        double[][] vectors_1 = ((double[][])(vecs));
        long i_24 = 0L;
        while ((long)(i_24) < (long)(n_3)) {
            long j_13 = 0L;
            while ((long)(j_13) < (long)((long)(n_3) - 1L)) {
                if ((double)(values_1[(int)((long)(j_13))]) < (double)(values_1[(int)((long)((long)(j_13) + 1L))])) {
                    double tmp_1 = (double)(values_1[(int)((long)(j_13))]);
values_1[(int)((long)(j_13))] = (double)(values_1[(int)((long)((long)(j_13) + 1L))]);
values_1[(int)((long)((long)(j_13) + 1L))] = (double)(tmp_1);
                    long r_3 = 0L;
                    while ((long)(r_3) < (long)(vectors_1.length)) {
                        double tv_1 = (double)(vectors_1[(int)((long)(r_3))][(int)((long)(j_13))]);
vectors_1[(int)((long)(r_3))][(int)((long)(j_13))] = (double)(vectors_1[(int)((long)(r_3))][(int)((long)((long)(j_13) + 1L))]);
vectors_1[(int)((long)(r_3))][(int)((long)((long)(j_13) + 1L))] = (double)(tv_1);
                        r_3 = (long)((long)(r_3) + 1L);
                    }
                }
                j_13 = (long)((long)(j_13) + 1L);
            }
            i_24 = (long)((long)(i_24) + 1L);
        }
        return new EigenResult(values_1, vectors_1);
    }

    static EigenResult find_lanczos_eigenvectors(long[][] graph, long k) {
        validate_adjacency_list(((long[][])(graph)));
        LanczosResult res_4 = lanczos_iteration(((long[][])(graph)), (long)(k));
        EigenResult eig_1 = jacobi_eigen(((double[][])(res_4.t)), 50L);
        EigenResult sorted_1 = sort_eigenpairs(((double[])(eig_1.values)), ((double[][])(eig_1.vectors)));
        double[][] final_vectors_1 = ((double[][])(matmul(((double[][])(res_4.q)), ((double[][])(sorted_1.vectors)))));
        return new EigenResult(sorted_1.values, final_vectors_1);
    }

    static String list_to_string(double[] arr) {
        String s_5 = "[";
        long i_26 = 0L;
        while ((long)(i_26) < (long)(arr.length)) {
            s_5 = s_5 + _p(_getd(arr, ((Number)(i_26)).intValue()));
            if ((long)(i_26) < (long)((long)(arr.length) - 1L)) {
                s_5 = s_5 + ", ";
            }
            i_26 = (long)((long)(i_26) + 1L);
        }
        return s_5 + "]";
    }

    static String matrix_to_string(double[][] m) {
        String s_6 = "[";
        long i_28 = 0L;
        while ((long)(i_28) < (long)(m.length)) {
            s_6 = s_6 + String.valueOf(list_to_string(((double[])(m[(int)((long)(i_28))]))));
            if ((long)(i_28) < (long)((long)(m.length) - 1L)) {
                s_6 = s_6 + "; ";
            }
            i_28 = (long)((long)(i_28) + 1L);
        }
        return s_6 + "]";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((long[][])(new long[][]{new long[]{1, 2}, new long[]{0, 2}, new long[]{0, 1}}));
            result_2 = find_lanczos_eigenvectors(((long[][])(graph)), 2L);
            System.out.println(list_to_string(((double[])(result_2.values))));
            System.out.println(matrix_to_string(((double[][])(result_2.vectors))));
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
