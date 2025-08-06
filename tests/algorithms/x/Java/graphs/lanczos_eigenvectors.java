public class Main {
    static int seed = 0;
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

    static int[][] graph;
    static EigenResult result_1;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double absf(double x) {
        return x < 0.0 ? -x : x;
    }

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        int i_1 = 0;
        while (i_1 < a.length) {
            s = s + a[i_1] * b[i_1];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static double[] vector_scale(double[] v, double s) {
        double[] res = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < v.length) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(v[i_2] * s)).toArray()));
            i_2 = i_2 + 1;
        }
        return res;
    }

    static double[] vector_sub(double[] a, double[] b) {
        double[] res_1 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < a.length) {
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(a[i_3] - b[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static double[] vector_add(double[] a, double[] b) {
        double[] res_2 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < a.length) {
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(a[i_4] + b[i_4])).toArray()));
            i_4 = i_4 + 1;
        }
        return res_2;
    }

    static double[][] zeros_matrix(int r, int c) {
        double[][] m = ((double[][])(new double[][]{}));
        int i_5 = 0;
        while (i_5 < r) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < c) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            m = ((double[][])(appendObj(m, row)));
            i_5 = i_5 + 1;
        }
        return m;
    }

    static double[] column(double[][] m, int idx) {
        double[] col = ((double[])(new double[]{}));
        int i_6 = 0;
        while (i_6 < m.length) {
            col = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(col), java.util.stream.DoubleStream.of(m[i_6][idx])).toArray()));
            i_6 = i_6 + 1;
        }
        return col;
    }

    static void validate_adjacency_list(int[][] graph) {
        int i_7 = 0;
        while (i_7 < graph.length) {
            int j_1 = 0;
            while (j_1 < graph[i_7].length) {
                int v = graph[i_7][j_1];
                if (v < 0 || v >= graph.length) {
                    throw new RuntimeException(String.valueOf("Invalid neighbor"));
                }
                j_1 = j_1 + 1;
            }
            i_7 = i_7 + 1;
        }
    }

    static double[] multiply_matrix_vector(int[][] graph, double[] vector) {
        int n = graph.length;
        if (vector.length != n) {
            throw new RuntimeException(String.valueOf("Vector length must match number of nodes"));
        }
        double[] result = ((double[])(new double[]{}));
        int i_8 = 0;
        while (i_8 < n) {
            double sum = 0.0;
            int j_2 = 0;
            while (j_2 < graph[i_8].length) {
                int nb = graph[i_8][j_2];
                sum = sum + vector[nb];
                j_2 = j_2 + 1;
            }
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(sum)).toArray()));
            i_8 = i_8 + 1;
        }
        return result;
    }

    static LanczosResult lanczos_iteration(int[][] graph, int k) {
        int n_1 = graph.length;
        if (k < 1 || k > n_1) {
            throw new RuntimeException(String.valueOf("invalid number of eigenvectors"));
        }
        double[][] q = ((double[][])(zeros_matrix(n_1, k)));
        double[][] t = ((double[][])(zeros_matrix(k, k)));
        double[] v_1 = ((double[])(new double[]{}));
        int i_9 = 0;
        while (i_9 < n_1) {
            v_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(v_1), java.util.stream.DoubleStream.of(random())).toArray()));
            i_9 = i_9 + 1;
        }
        double ss = 0.0;
        i_9 = 0;
        while (i_9 < n_1) {
            ss = ss + v_1[i_9] * v_1[i_9];
            i_9 = i_9 + 1;
        }
        double vnorm = sqrtApprox(ss);
        i_9 = 0;
        while (i_9 < n_1) {
q[i_9][0] = v_1[i_9] / vnorm;
            i_9 = i_9 + 1;
        }
        double beta = 0.0;
        int j_3 = 0;
        while (j_3 < k) {
            double[] w = ((double[])(multiply_matrix_vector(((int[][])(graph)), ((double[])(column(((double[][])(q)), j_3))))));
            if (j_3 > 0) {
                w = ((double[])(vector_sub(((double[])(w)), ((double[])(vector_scale(((double[])(column(((double[][])(q)), j_3 - 1))), beta))))));
            }
            double alpha = dot(((double[])(column(((double[][])(q)), j_3))), ((double[])(w)));
            w = ((double[])(vector_sub(((double[])(w)), ((double[])(vector_scale(((double[])(column(((double[][])(q)), j_3))), alpha))))));
            double ss2 = 0.0;
            int p = 0;
            while (p < n_1) {
                ss2 = ss2 + w[p] * w[p];
                p = p + 1;
            }
            beta = sqrtApprox(ss2);
t[j_3][j_3] = alpha;
            if (j_3 < k - 1) {
t[j_3][j_3 + 1] = beta;
t[j_3 + 1][j_3] = beta;
                if (beta > 1e-10) {
                    double[] wnorm = ((double[])(vector_scale(((double[])(w)), 1.0 / beta)));
                    int r = 0;
                    while (r < n_1) {
q[r][j_3 + 1] = wnorm[r];
                        r = r + 1;
                    }
                }
            }
            j_3 = j_3 + 1;
        }
        return new LanczosResult(t, q);
    }

    static EigenResult jacobi_eigen(double[][] a_in, int max_iter) {
        int n_2 = a_in.length;
        double[][] a = ((double[][])(a_in));
        double[][] v_2 = ((double[][])(zeros_matrix(n_2, n_2)));
        int i_10 = 0;
        while (i_10 < n_2) {
v_2[i_10][i_10] = 1.0;
            i_10 = i_10 + 1;
        }
        int iter = 0;
        while (iter < max_iter) {
            int p_1 = 0;
            int q_1 = 1;
            double max = absf(a[p_1][q_1]);
            i_10 = 0;
            while (i_10 < n_2) {
                int j_4 = i_10 + 1;
                while (j_4 < n_2) {
                    double val = absf(a[i_10][j_4]);
                    if (val > max) {
                        max = val;
                        p_1 = i_10;
                        q_1 = j_4;
                    }
                    j_4 = j_4 + 1;
                }
                i_10 = i_10 + 1;
            }
            if (max < 1e-08) {
                break;
            }
            double app = a[p_1][p_1];
            double aqq = a[q_1][q_1];
            double apq = a[p_1][q_1];
            double theta = (aqq - app) / (2.0 * apq);
            double t_1 = 1.0 / (absf(theta) + sqrtApprox(theta * theta + 1.0));
            if (theta < 0.0) {
                t_1 = -t_1;
            }
            double c = 1.0 / sqrtApprox(1.0 + t_1 * t_1);
            double s_1 = t_1 * c;
            double tau = s_1 / (1.0 + c);
a[p_1][p_1] = app - t_1 * apq;
a[q_1][q_1] = aqq + t_1 * apq;
a[p_1][q_1] = 0.0;
a[q_1][p_1] = 0.0;
            int k = 0;
            while (k < n_2) {
                if (k != p_1 && k != q_1) {
                    double akp = a[k][p_1];
                    double akq = a[k][q_1];
a[k][p_1] = akp - s_1 * (akq + tau * akp);
a[p_1][k] = a[k][p_1];
a[k][q_1] = akq + s_1 * (akp - tau * akq);
a[q_1][k] = a[k][q_1];
                }
                k = k + 1;
            }
            k = 0;
            while (k < n_2) {
                double vkp = v_2[k][p_1];
                double vkq = v_2[k][q_1];
v_2[k][p_1] = vkp - s_1 * (vkq + tau * vkp);
v_2[k][q_1] = vkq + s_1 * (vkp - tau * vkq);
                k = k + 1;
            }
            iter = iter + 1;
        }
        double[] eigenvalues = ((double[])(new double[]{}));
        i_10 = 0;
        while (i_10 < n_2) {
            eigenvalues = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(eigenvalues), java.util.stream.DoubleStream.of(a[i_10][i_10])).toArray()));
            i_10 = i_10 + 1;
        }
        return new EigenResult(eigenvalues, v_2);
    }

    static double[][] matmul(double[][] a, double[][] b) {
        int rows = a.length;
        int cols = b[0].length;
        int inner = b.length;
        double[][] m_1 = ((double[][])(zeros_matrix(rows, cols)));
        int i_11 = 0;
        while (i_11 < rows) {
            int j_5 = 0;
            while (j_5 < cols) {
                double s_2 = 0.0;
                int k_1 = 0;
                while (k_1 < inner) {
                    s_2 = s_2 + a[i_11][k_1] * b[k_1][j_5];
                    k_1 = k_1 + 1;
                }
m_1[i_11][j_5] = s_2;
                j_5 = j_5 + 1;
            }
            i_11 = i_11 + 1;
        }
        return m_1;
    }

    static EigenResult sort_eigenpairs(double[] vals, double[][] vecs) {
        int n_3 = vals.length;
        double[] values = ((double[])(vals));
        double[][] vectors = ((double[][])(vecs));
        int i_12 = 0;
        while (i_12 < n_3) {
            int j_6 = 0;
            while (j_6 < n_3 - 1) {
                if (values[j_6] < values[j_6 + 1]) {
                    double tmp = values[j_6];
values[j_6] = values[j_6 + 1];
values[j_6 + 1] = tmp;
                    int r_1 = 0;
                    while (r_1 < vectors.length) {
                        double tv = vectors[r_1][j_6];
vectors[r_1][j_6] = vectors[r_1][j_6 + 1];
vectors[r_1][j_6 + 1] = tv;
                        r_1 = r_1 + 1;
                    }
                }
                j_6 = j_6 + 1;
            }
            i_12 = i_12 + 1;
        }
        return new EigenResult(values, vectors);
    }

    static EigenResult find_lanczos_eigenvectors(int[][] graph, int k) {
        validate_adjacency_list(((int[][])(graph)));
        LanczosResult res_3 = lanczos_iteration(((int[][])(graph)), k);
        EigenResult eig = jacobi_eigen(((double[][])(res_3.t)), 50);
        EigenResult sorted = sort_eigenpairs(((double[])(eig.values)), ((double[][])(eig.vectors)));
        double[][] final_vectors = ((double[][])(matmul(((double[][])(res_3.q)), ((double[][])(sorted.vectors)))));
        return new EigenResult(sorted.values, final_vectors);
    }

    static String list_to_string(double[] arr) {
        String s_3 = "[";
        int i_13 = 0;
        while (i_13 < arr.length) {
            s_3 = s_3 + _p(_geto(arr, i_13));
            if (i_13 < arr.length - 1) {
                s_3 = s_3 + ", ";
            }
            i_13 = i_13 + 1;
        }
        return s_3 + "]";
    }

    static String matrix_to_string(double[][] m) {
        String s_4 = "[";
        int i_14 = 0;
        while (i_14 < m.length) {
            s_4 = s_4 + String.valueOf(list_to_string(((double[])(m[i_14]))));
            if (i_14 < m.length - 1) {
                s_4 = s_4 + "; ";
            }
            i_14 = i_14 + 1;
        }
        return s_4 + "]";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 123456789;
            graph = ((int[][])(new int[][]{new int[]{1, 2}, new int[]{0, 2}, new int[]{0, 1}}));
            result_1 = find_lanczos_eigenvectors(((int[][])(graph)), 2);
            System.out.println(list_to_string(((double[])(result_1.values))));
            System.out.println(matrix_to_string(((double[][])(result_1.vectors))));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
