public class Main {
    static double PI;
    static long seed = 0;
    static class Vector {
        double[] components;
        Vector(double[] components) {
            this.components = components;
        }
        Vector() {}
        @Override public String toString() {
            return String.format("{'components': %s}", String.valueOf(components));
        }
    }

    static class Matrix {
        double[][] data;
        long width;
        long height;
        Matrix(double[][] data, long width, long height) {
            this.data = data;
            this.width = width;
            this.height = height;
        }
        Matrix() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'width': %s, 'height': %s}", String.valueOf(data), String.valueOf(width), String.valueOf(height));
        }
    }


    static long rand() {
        seed = ((long)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static long random_int(long a, long b) {
        long r = Math.floorMod(rand(), (b - a + 1));
        return a + r;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0L;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double arcsin_taylor(double x) {
        double term = x;
        double sum_1 = x;
        long n_1 = 1L;
        while (n_1 < 10) {
            double num_1 = (2.0 * (((Number)(n_1)).doubleValue()) - 1.0) * (2.0 * (((Number)(n_1)).doubleValue()) - 1.0) * x * x * term;
            double den_1 = (2.0 * (((Number)(n_1)).doubleValue())) * (2.0 * (((Number)(n_1)).doubleValue()) + 1.0);
            term = num_1 / den_1;
            sum_1 = sum_1 + term;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double acos_taylor(double x) {
        return PI / 2.0 - arcsin_taylor(x);
    }

    static long vector_len(Vector v) {
        return v.components.length;
    }

    static String vector_to_string(Vector v) {
        String s = "(";
        long i_3 = 0L;
        while (i_3 < v.components.length) {
            s = s + _p(_getd(v.components, ((Number)(i_3)).intValue()));
            if (i_3 < v.components.length - 1) {
                s = s + ",";
            }
            i_3 = i_3 + 1;
        }
        s = s + ")";
        return s;
    }

    static Vector vector_add(Vector a, Vector b) {
        long size = vector_len(a);
        if (size != vector_len(b)) {
            return new Vector(new double[]{});
        }
        double[] res_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while (i_5 < size) {
            res_1 = ((double[])(appendDouble(res_1, a.components[(int)((long)(i_5))] + b.components[(int)((long)(i_5))])));
            i_5 = i_5 + 1;
        }
        return new Vector(res_1);
    }

    static Vector vector_sub(Vector a, Vector b) {
        long size_1 = vector_len(a);
        if (size_1 != vector_len(b)) {
            return new Vector(new double[]{});
        }
        double[] res_3 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while (i_7 < size_1) {
            res_3 = ((double[])(appendDouble(res_3, a.components[(int)((long)(i_7))] - b.components[(int)((long)(i_7))])));
            i_7 = i_7 + 1;
        }
        return new Vector(res_3);
    }

    static boolean vector_eq(Vector a, Vector b) {
        if (vector_len(a) != vector_len(b)) {
            return false;
        }
        long i_9 = 0L;
        while (i_9 < vector_len(a)) {
            if (a.components[(int)((long)(i_9))] != b.components[(int)((long)(i_9))]) {
                return false;
            }
            i_9 = i_9 + 1;
        }
        return true;
    }

    static Vector vector_mul_scalar(Vector v, double s) {
        double[] res_4 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while (i_11 < vector_len(v)) {
            res_4 = ((double[])(appendDouble(res_4, v.components[(int)((long)(i_11))] * s)));
            i_11 = i_11 + 1;
        }
        return new Vector(res_4);
    }

    static double vector_dot(Vector a, Vector b) {
        long size_2 = vector_len(a);
        if (size_2 != vector_len(b)) {
            return 0.0;
        }
        double sum_3 = 0.0;
        long i_13 = 0L;
        while (i_13 < size_2) {
            sum_3 = sum_3 + a.components[(int)((long)(i_13))] * b.components[(int)((long)(i_13))];
            i_13 = i_13 + 1;
        }
        return sum_3;
    }

    static Vector vector_copy(Vector v) {
        double[] res_5 = ((double[])(new double[]{}));
        long i_15 = 0L;
        while (i_15 < vector_len(v)) {
            res_5 = ((double[])(appendDouble(res_5, v.components[(int)((long)(i_15))])));
            i_15 = i_15 + 1;
        }
        return new Vector(res_5);
    }

    static double vector_component(Vector v, long idx) {
        return v.components[(int)((long)(idx))];
    }

    static Vector vector_change_component(Vector v, long pos, double value) {
        double[] comps = ((double[])(v.components));
comps[(int)((long)(pos))] = value;
        return new Vector(comps);
    }

    static double vector_euclidean_length(Vector v) {
        double sum_4 = 0.0;
        long i_17 = 0L;
        while (i_17 < v.components.length) {
            sum_4 = sum_4 + v.components[(int)((long)(i_17))] * v.components[(int)((long)(i_17))];
            i_17 = i_17 + 1;
        }
        double result_1 = sqrtApprox(sum_4);
        return result_1;
    }

    static double vector_angle(Vector a, Vector b, boolean deg) {
        double num_2 = vector_dot(a, b);
        double den_3 = vector_euclidean_length(a) * vector_euclidean_length(b);
        double ang_1 = acos_taylor(num_2 / den_3);
        if (((Boolean)(deg))) {
            ang_1 = ang_1 * 180.0 / PI;
        }
        return ang_1;
    }

    static Vector zero_vector(long d) {
        double[] res_6 = ((double[])(new double[]{}));
        long i_19 = 0L;
        while (i_19 < d) {
            res_6 = ((double[])(appendDouble(res_6, 0.0)));
            i_19 = i_19 + 1;
        }
        return new Vector(res_6);
    }

    static Vector unit_basis_vector(long d, long pos) {
        double[] res_7 = ((double[])(new double[]{}));
        long i_21 = 0L;
        while (i_21 < d) {
            if (i_21 == pos) {
                res_7 = ((double[])(appendDouble(res_7, 1.0)));
            } else {
                res_7 = ((double[])(appendDouble(res_7, 0.0)));
            }
            i_21 = i_21 + 1;
        }
        return new Vector(res_7);
    }

    static Vector axpy(double s, Vector x, Vector y) {
        return vector_add(vector_mul_scalar(x, s), y);
    }

    static Vector random_vector(long n, long a, long b) {
        double[] res_8 = ((double[])(new double[]{}));
        long i_23 = 0L;
        while (i_23 < n) {
            res_8 = ((double[])(appendDouble(res_8, ((Number)(random_int(a, b))).doubleValue())));
            i_23 = i_23 + 1;
        }
        return new Vector(res_8);
    }

    static String matrix_to_string(Matrix m) {
        String ans = "";
        long i_25 = 0L;
        while (i_25 < m.height) {
            ans = ans + "|";
            long j_1 = 0L;
            while (j_1 < m.width) {
                ans = ans + _p(_getd(m.data[(int)((long)(i_25))], ((Number)(j_1)).intValue()));
                if (j_1 < m.width - 1) {
                    ans = ans + ",";
                }
                j_1 = j_1 + 1;
            }
            ans = ans + "|\n";
            i_25 = i_25 + 1;
        }
        return ans;
    }

    static Matrix matrix_add(Matrix a, Matrix b) {
        if (a.width != b.width || a.height != b.height) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] mat_1 = ((double[][])(new double[][]{}));
        long i_27 = 0L;
        while (i_27 < a.height) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while (j_3 < a.width) {
                row_1 = ((double[])(appendDouble(row_1, a.data[(int)((long)(i_27))][(int)((long)(j_3))] + b.data[(int)((long)(i_27))][(int)((long)(j_3))])));
                j_3 = j_3 + 1;
            }
            mat_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_27 = i_27 + 1;
        }
        return new Matrix(mat_1, a.width, a.height);
    }

    static Matrix matrix_sub(Matrix a, Matrix b) {
        if (a.width != b.width || a.height != b.height) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] mat_3 = ((double[][])(new double[][]{}));
        long i_29 = 0L;
        while (i_29 < a.height) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while (j_5 < a.width) {
                row_3 = ((double[])(appendDouble(row_3, a.data[(int)((long)(i_29))][(int)((long)(j_5))] - b.data[(int)((long)(i_29))][(int)((long)(j_5))])));
                j_5 = j_5 + 1;
            }
            mat_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_3), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_29 = i_29 + 1;
        }
        return new Matrix(mat_3, a.width, a.height);
    }

    static Vector matrix_mul_vector(Matrix m, Vector v) {
        if (v.components.length != m.width) {
            return new Vector(new double[]{});
        }
        Vector res_10 = zero_vector(m.height);
        long i_31 = 0L;
        while (i_31 < m.height) {
            double sum_6 = 0.0;
            long j_7 = 0L;
            while (j_7 < m.width) {
                sum_6 = sum_6 + m.data[(int)((long)(i_31))][(int)((long)(j_7))] * v.components[(int)((long)(j_7))];
                j_7 = j_7 + 1;
            }
            res_10 = vector_change_component(res_10, i_31, sum_6);
            i_31 = i_31 + 1;
        }
        return res_10;
    }

    static Matrix matrix_mul_scalar(Matrix m, double s) {
        double[][] mat_4 = ((double[][])(new double[][]{}));
        long i_33 = 0L;
        while (i_33 < m.height) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while (j_9 < m.width) {
                row_5 = ((double[])(appendDouble(row_5, m.data[(int)((long)(i_33))][(int)((long)(j_9))] * s)));
                j_9 = j_9 + 1;
            }
            mat_4 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_4), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_33 = i_33 + 1;
        }
        return new Matrix(mat_4, m.width, m.height);
    }

    static double matrix_component(Matrix m, long x, long y) {
        return m.data[(int)((long)(x))][(int)((long)(y))];
    }

    static Matrix matrix_change_component(Matrix m, long x, long y, double value) {
        double[][] data = ((double[][])(m.data));
data[(int)((long)(x))][(int)((long)(y))] = value;
        return new Matrix(data, m.width, m.height);
    }

    static double matrix_minor(Matrix m, long x, long y) {
        if (m.height != m.width) {
            return 0.0;
        }
        double[][] minor_1 = ((double[][])(new double[][]{}));
        long i_35 = 0L;
        while (i_35 < m.height) {
            if (i_35 != x) {
                double[] row_7 = ((double[])(new double[]{}));
                long j_11 = 0L;
                while (j_11 < m.width) {
                    if (j_11 != y) {
                        row_7 = ((double[])(appendDouble(row_7, m.data[(int)((long)(i_35))][(int)((long)(j_11))])));
                    }
                    j_11 = j_11 + 1;
                }
                minor_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(minor_1), java.util.stream.Stream.of(row_7)).toArray(double[][]::new)));
            }
            i_35 = i_35 + 1;
        }
        Matrix sub_1 = new Matrix(minor_1, m.width - 1, m.height - 1);
        return matrix_determinant(sub_1);
    }

    static double matrix_cofactor(Matrix m, long x, long y) {
        double sign = Math.floorMod((x + y), 2) == 0 ? 1.0 : -1.0;
        return sign * matrix_minor(m, x, y);
    }

    static double matrix_determinant(Matrix m) {
        if (m.height != m.width) {
            return 0.0;
        }
        if (m.height == 0) {
            return 0.0;
        }
        if (m.height == 1) {
            return m.data[(int)((long)(0))][(int)((long)(0))];
        }
        if (m.height == 2) {
            return m.data[(int)((long)(0))][(int)((long)(0))] * m.data[(int)((long)(1))][(int)((long)(1))] - m.data[(int)((long)(0))][(int)((long)(1))] * m.data[(int)((long)(1))][(int)((long)(0))];
        }
        double sum_8 = 0.0;
        long y_1 = 0L;
        while (y_1 < m.width) {
            sum_8 = sum_8 + m.data[(int)((long)(0))][(int)((long)(y_1))] * matrix_cofactor(m, 0L, y_1);
            y_1 = y_1 + 1;
        }
        return sum_8;
    }

    static Matrix square_zero_matrix(long n) {
        double[][] mat_5 = ((double[][])(new double[][]{}));
        long i_37 = 0L;
        while (i_37 < n) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_13 = 0L;
            while (j_13 < n) {
                row_9 = ((double[])(appendDouble(row_9, 0.0)));
                j_13 = j_13 + 1;
            }
            mat_5 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_5), java.util.stream.Stream.of(row_9)).toArray(double[][]::new)));
            i_37 = i_37 + 1;
        }
        return new Matrix(mat_5, n, n);
    }

    static Matrix random_matrix(long w, long h, long a, long b) {
        double[][] mat_6 = ((double[][])(new double[][]{}));
        long i_39 = 0L;
        while (i_39 < h) {
            double[] row_11 = ((double[])(new double[]{}));
            long j_15 = 0L;
            while (j_15 < w) {
                row_11 = ((double[])(appendDouble(row_11, ((Number)(random_int(a, b))).doubleValue())));
                j_15 = j_15 + 1;
            }
            mat_6 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_6), java.util.stream.Stream.of(row_11)).toArray(double[][]::new)));
            i_39 = i_39 + 1;
        }
        return new Matrix(mat_6, w, h);
    }

    static void main() {
        Vector v1 = new Vector(new double[]{1.0, 2.0, 3.0});
        Vector v2_1 = new Vector(new double[]{4.0, 5.0, 6.0});
        System.out.println(vector_to_string(vector_add(v1, v2_1)));
        System.out.println(_p(vector_dot(v1, v2_1)));
        System.out.println(_p(vector_euclidean_length(v1)));
        Matrix m_1 = new Matrix(new double[][]{new double[]{1.0, 2.0}, new double[]{3.0, 4.0}}, 2, 2);
        System.out.println(_p(matrix_determinant(m_1)));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        seed = 123456789L;
        main();
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
