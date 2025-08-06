public class Main {
    static double PI;
    static int seed = 0;
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
        int width;
        int height;
        Matrix(double[][] data, int width, int height) {
            this.data = data;
            this.width = width;
            this.height = height;
        }
        Matrix() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'width': %s, 'height': %s}", String.valueOf(data), String.valueOf(width), String.valueOf(height));
        }
    }


    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static int random_int(int a, int b) {
        int r = Math.floorMod(rand(), (b - a + 1));
        return a + r;
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

    static double arcsin_taylor(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n < 10) {
            double num = (2.0 * (((Number)(n)).doubleValue()) - 1.0) * (2.0 * (((Number)(n)).doubleValue()) - 1.0) * x * x * term;
            double den = (2.0 * (((Number)(n)).doubleValue())) * (2.0 * (((Number)(n)).doubleValue()) + 1.0);
            term = num / den;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double acos_taylor(double x) {
        return PI / 2.0 - arcsin_taylor(x);
    }

    static int vector_len(Vector v) {
        return v.components.length;
    }

    static String vector_to_string(Vector v) {
        String s = "(";
        int i_1 = 0;
        while (i_1 < v.components.length) {
            s = s + _p(_getd(v.components, i_1));
            if (i_1 < v.components.length - 1) {
                s = s + ",";
            }
            i_1 = i_1 + 1;
        }
        s = s + ")";
        return s;
    }

    static Vector vector_add(Vector a, Vector b) {
        int size = vector_len(a);
        if (size != vector_len(b)) {
            return new Vector(new double[]{});
        }
        double[] res = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < size) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(a.components[i_2] + b.components[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        return new Vector(res);
    }

    static Vector vector_sub(Vector a, Vector b) {
        int size_1 = vector_len(a);
        if (size_1 != vector_len(b)) {
            return new Vector(new double[]{});
        }
        double[] res_1 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < size_1) {
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(a.components[i_3] - b.components[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return new Vector(res_1);
    }

    static boolean vector_eq(Vector a, Vector b) {
        if (vector_len(a) != vector_len(b)) {
            return false;
        }
        int i_4 = 0;
        while (i_4 < vector_len(a)) {
            if (a.components[i_4] != b.components[i_4]) {
                return false;
            }
            i_4 = i_4 + 1;
        }
        return true;
    }

    static Vector vector_mul_scalar(Vector v, double s) {
        double[] res_2 = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < vector_len(v)) {
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(v.components[i_5] * s)).toArray()));
            i_5 = i_5 + 1;
        }
        return new Vector(res_2);
    }

    static double vector_dot(Vector a, Vector b) {
        int size_2 = vector_len(a);
        if (size_2 != vector_len(b)) {
            return 0.0;
        }
        double sum_1 = 0.0;
        int i_6 = 0;
        while (i_6 < size_2) {
            sum_1 = sum_1 + a.components[i_6] * b.components[i_6];
            i_6 = i_6 + 1;
        }
        return sum_1;
    }

    static Vector vector_copy(Vector v) {
        double[] res_3 = ((double[])(new double[]{}));
        int i_7 = 0;
        while (i_7 < vector_len(v)) {
            res_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_3), java.util.stream.DoubleStream.of(v.components[i_7])).toArray()));
            i_7 = i_7 + 1;
        }
        return new Vector(res_3);
    }

    static double vector_component(Vector v, int idx) {
        return v.components[idx];
    }

    static Vector vector_change_component(Vector v, int pos, double value) {
        double[] comps = ((double[])(v.components));
comps[pos] = value;
        return new Vector(comps);
    }

    static double vector_euclidean_length(Vector v) {
        double sum_2 = 0.0;
        int i_8 = 0;
        while (i_8 < v.components.length) {
            sum_2 = sum_2 + v.components[i_8] * v.components[i_8];
            i_8 = i_8 + 1;
        }
        double result = sqrtApprox(sum_2);
        return result;
    }

    static double vector_angle(Vector a, Vector b, boolean deg) {
        double num_1 = vector_dot(a, b);
        double den_1 = vector_euclidean_length(a) * vector_euclidean_length(b);
        double ang = acos_taylor(num_1 / den_1);
        if (((Boolean)(deg))) {
            ang = ang * 180.0 / PI;
        }
        return ang;
    }

    static Vector zero_vector(int d) {
        double[] res_4 = ((double[])(new double[]{}));
        int i_9 = 0;
        while (i_9 < d) {
            res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_9 = i_9 + 1;
        }
        return new Vector(res_4);
    }

    static Vector unit_basis_vector(int d, int pos) {
        double[] res_5 = ((double[])(new double[]{}));
        int i_10 = 0;
        while (i_10 < d) {
            if (i_10 == pos) {
                res_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_5), java.util.stream.DoubleStream.of(1.0)).toArray()));
            } else {
                res_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_5), java.util.stream.DoubleStream.of(0.0)).toArray()));
            }
            i_10 = i_10 + 1;
        }
        return new Vector(res_5);
    }

    static Vector axpy(double s, Vector x, Vector y) {
        return vector_add(vector_mul_scalar(x, s), y);
    }

    static Vector random_vector(int n, int a, int b) {
        double[] res_6 = ((double[])(new double[]{}));
        int i_11 = 0;
        while (i_11 < n) {
            res_6 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_6), java.util.stream.DoubleStream.of(((Number)(random_int(a, b))).doubleValue())).toArray()));
            i_11 = i_11 + 1;
        }
        return new Vector(res_6);
    }

    static String matrix_to_string(Matrix m) {
        String ans = "";
        int i_12 = 0;
        while (i_12 < m.height) {
            ans = ans + "|";
            int j = 0;
            while (j < m.width) {
                ans = ans + _p(_getd(m.data[i_12], j));
                if (j < m.width - 1) {
                    ans = ans + ",";
                }
                j = j + 1;
            }
            ans = ans + "|\n";
            i_12 = i_12 + 1;
        }
        return ans;
    }

    static Matrix matrix_add(Matrix a, Matrix b) {
        if (a.width != b.width || a.height != b.height) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] mat = ((double[][])(new double[][]{}));
        int i_13 = 0;
        while (i_13 < a.height) {
            double[] row = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < a.width) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(a.data[i_13][j_1] + b.data[i_13][j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            mat = ((double[][])(appendObj(mat, row)));
            i_13 = i_13 + 1;
        }
        return new Matrix(mat, a.width, a.height);
    }

    static Matrix matrix_sub(Matrix a, Matrix b) {
        if (a.width != b.width || a.height != b.height) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] mat_1 = ((double[][])(new double[][]{}));
        int i_14 = 0;
        while (i_14 < a.height) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < a.width) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(a.data[i_14][j_2] - b.data[i_14][j_2])).toArray()));
                j_2 = j_2 + 1;
            }
            mat_1 = ((double[][])(appendObj(mat_1, row_1)));
            i_14 = i_14 + 1;
        }
        return new Matrix(mat_1, a.width, a.height);
    }

    static Vector matrix_mul_vector(Matrix m, Vector v) {
        if (v.components.length != m.width) {
            return new Vector(new double[]{});
        }
        Vector res_7 = zero_vector(m.height);
        int i_15 = 0;
        while (i_15 < m.height) {
            double sum_3 = 0.0;
            int j_3 = 0;
            while (j_3 < m.width) {
                sum_3 = sum_3 + m.data[i_15][j_3] * v.components[j_3];
                j_3 = j_3 + 1;
            }
            res_7 = vector_change_component(res_7, i_15, sum_3);
            i_15 = i_15 + 1;
        }
        return res_7;
    }

    static Matrix matrix_mul_scalar(Matrix m, double s) {
        double[][] mat_2 = ((double[][])(new double[][]{}));
        int i_16 = 0;
        while (i_16 < m.height) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < m.width) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(m.data[i_16][j_4] * s)).toArray()));
                j_4 = j_4 + 1;
            }
            mat_2 = ((double[][])(appendObj(mat_2, row_2)));
            i_16 = i_16 + 1;
        }
        return new Matrix(mat_2, m.width, m.height);
    }

    static double matrix_component(Matrix m, int x, int y) {
        return m.data[x][y];
    }

    static Matrix matrix_change_component(Matrix m, int x, int y, double value) {
        double[][] data = ((double[][])(m.data));
data[x][y] = value;
        return new Matrix(data, m.width, m.height);
    }

    static double matrix_minor(Matrix m, int x, int y) {
        if (m.height != m.width) {
            return 0.0;
        }
        double[][] minor = ((double[][])(new double[][]{}));
        int i_17 = 0;
        while (i_17 < m.height) {
            if (i_17 != x) {
                double[] row_3 = ((double[])(new double[]{}));
                int j_5 = 0;
                while (j_5 < m.width) {
                    if (j_5 != y) {
                        row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(m.data[i_17][j_5])).toArray()));
                    }
                    j_5 = j_5 + 1;
                }
                minor = ((double[][])(appendObj(minor, row_3)));
            }
            i_17 = i_17 + 1;
        }
        Matrix sub = new Matrix(minor, m.width - 1, m.height - 1);
        return matrix_determinant(sub);
    }

    static double matrix_cofactor(Matrix m, int x, int y) {
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
            return m.data[0][0];
        }
        if (m.height == 2) {
            return m.data[0][0] * m.data[1][1] - m.data[0][1] * m.data[1][0];
        }
        double sum_4 = 0.0;
        int y = 0;
        while (y < m.width) {
            sum_4 = sum_4 + m.data[0][y] * matrix_cofactor(m, 0, y);
            y = y + 1;
        }
        return sum_4;
    }

    static Matrix square_zero_matrix(int n) {
        double[][] mat_3 = ((double[][])(new double[][]{}));
        int i_18 = 0;
        while (i_18 < n) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_6 = 0;
            while (j_6 < n) {
                row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j_6 = j_6 + 1;
            }
            mat_3 = ((double[][])(appendObj(mat_3, row_4)));
            i_18 = i_18 + 1;
        }
        return new Matrix(mat_3, n, n);
    }

    static Matrix random_matrix(int w, int h, int a, int b) {
        double[][] mat_4 = ((double[][])(new double[][]{}));
        int i_19 = 0;
        while (i_19 < h) {
            double[] row_5 = ((double[])(new double[]{}));
            int j_7 = 0;
            while (j_7 < w) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(((Number)(random_int(a, b))).doubleValue())).toArray()));
                j_7 = j_7 + 1;
            }
            mat_4 = ((double[][])(appendObj(mat_4, row_5)));
            i_19 = i_19 + 1;
        }
        return new Matrix(mat_4, w, h);
    }

    static void main() {
        Vector v1 = new Vector(new double[]{1.0, 2.0, 3.0});
        Vector v2 = new Vector(new double[]{4.0, 5.0, 6.0});
        System.out.println(vector_to_string(vector_add(v1, v2)));
        System.out.println(_p(vector_dot(v1, v2)));
        System.out.println(_p(vector_euclidean_length(v1)));
        Matrix m = new Matrix(new double[][]{new double[]{1.0, 2.0}, new double[]{3.0, 4.0}}, 2, 2);
        System.out.println(_p(matrix_determinant(m)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            seed = 123456789;
            main();
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
