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

    static double sign(double x) {
        if (x >= 0.0) {
            return 1.0;
        } else {
            return -1.0;
        }
    }

    static double vector_norm(double[] v) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < v.length) {
            sum = sum + v[i_1] * v[i_1];
            i_1 = i_1 + 1;
        }
        double n = sqrt_approx(sum);
        return n;
    }

    static double[][] identity_matrix(int n) {
        double[][] mat = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < n) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                if (i_2 == j) {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j = j + 1;
            }
            mat = ((double[][])(appendObj(mat, row)));
            i_2 = i_2 + 1;
        }
        return mat;
    }

    static double[][] copy_matrix(double[][] a) {
        double[][] mat_1 = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < a.length) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < a[i_3].length) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(a[i_3][j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            mat_1 = ((double[][])(appendObj(mat_1, row_1)));
            i_3 = i_3 + 1;
        }
        return mat_1;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        int m = a.length;
        int n_1 = a[0].length;
        int p = b[0].length;
        double[][] res = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < m) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < p) {
                double sum_1 = 0.0;
                int k = 0;
                while (k < n_1) {
                    sum_1 = sum_1 + a[i_4][k] * b[k][j_2];
                    k = k + 1;
                }
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(sum_1)).toArray()));
                j_2 = j_2 + 1;
            }
            res = ((double[][])(appendObj(res, row_2)));
            i_4 = i_4 + 1;
        }
        return res;
    }

    static QR qr_decomposition(double[][] a) {
        int m_1 = a.length;
        int n_2 = a[0].length;
        int t = m_1 < n_2 ? m_1 : n_2;
        double[][] q = ((double[][])(identity_matrix(m_1)));
        double[][] r = ((double[][])(copy_matrix(((double[][])(a)))));
        int k_1 = 0;
        while (k_1 < t - 1) {
            double[] x = ((double[])(new double[]{}));
            int i_5 = k_1;
            while (i_5 < m_1) {
                x = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x), java.util.stream.DoubleStream.of(r[i_5][k_1])).toArray()));
                i_5 = i_5 + 1;
            }
            double[] e1 = ((double[])(new double[]{}));
            i_5 = 0;
            while (i_5 < x.length) {
                if (i_5 == 0) {
                    e1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(e1), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    e1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(e1), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                i_5 = i_5 + 1;
            }
            double alpha = vector_norm(((double[])(x)));
            double s = sign(x[0]) * alpha;
            double[] v = ((double[])(new double[]{}));
            i_5 = 0;
            while (i_5 < x.length) {
                v = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(v), java.util.stream.DoubleStream.of(x[i_5] + s * e1[i_5])).toArray()));
                i_5 = i_5 + 1;
            }
            double vnorm = vector_norm(((double[])(v)));
            i_5 = 0;
            while (i_5 < v.length) {
v[i_5] = v[i_5] / vnorm;
                i_5 = i_5 + 1;
            }
            int size = v.length;
            double[][] qk_small = ((double[][])(new double[][]{}));
            i_5 = 0;
            while (i_5 < size) {
                double[] row_3 = ((double[])(new double[]{}));
                int j_3 = 0;
                while (j_3 < size) {
                    double delta = i_5 == j_3 ? 1.0 : 0.0;
                    row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(delta - 2.0 * v[i_5] * v[j_3])).toArray()));
                    j_3 = j_3 + 1;
                }
                qk_small = ((double[][])(appendObj(qk_small, row_3)));
                i_5 = i_5 + 1;
            }
            double[][] qk = ((double[][])(identity_matrix(m_1)));
            i_5 = 0;
            while (i_5 < size) {
                int j_4 = 0;
                while (j_4 < size) {
qk[k_1 + i_5][k_1 + j_4] = qk_small[i_5][j_4];
                    j_4 = j_4 + 1;
                }
                i_5 = i_5 + 1;
            }
            q = ((double[][])(matmul(((double[][])(q)), ((double[][])(qk)))));
            r = ((double[][])(matmul(((double[][])(qk)), ((double[][])(r)))));
            k_1 = k_1 + 1;
        }
        return new QR(q, r);
    }

    static void print_matrix(double[][] mat) {
        int i_6 = 0;
        while (i_6 < mat.length) {
            String line = "";
            int j_5 = 0;
            while (j_5 < mat[i_6].length) {
                line = line + _p(_geto(mat[i_6], j_5));
                if (j_5 + 1 < mat[i_6].length) {
                    line = line + " ";
                }
                j_5 = j_5 + 1;
            }
            System.out.println(line);
            i_6 = i_6 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            A = ((double[][])(new double[][]{new double[]{12.0, -51.0, 4.0}, new double[]{6.0, 167.0, -68.0}, new double[]{-4.0, 24.0, -41.0}}));
            result = qr_decomposition(((double[][])(A)));
            print_matrix(((double[][])(result.q)));
            print_matrix(((double[][])(result.r)));
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
