public class Main {
    static int[] vx;
    static int[] vs;
    static int[] vsize;
    static int[] va;
    static int[] vb;
    static int[] vsum;
    static int[] vsub;
    static double[] vmul;
    static int[] zvec;
    static String zstr;
    static int zcount = 0;
    static int zi = 0;
    static int[] vcopy;
    static int[] vchange = new int[0];
    static int[][] ma = new int[0][];
    static int[][] mb = new int[0][];
    static int[] mv;
    static int[][] msc;
    static int[][] mc = new int[0][];
    static int[][] madd = new int[0][];
    static int[][] msub = new int[0][];
    static int[][] mzero;

    static String int_to_string(int n) {
        if (n == 0) {
            return "0";
        }
        int num = n;
        boolean neg = false;
        if (num < 0) {
            neg = true;
            num = -num;
        }
        String res = "";
        while (num > 0) {
            int digit = Math.floorMod(num, 10);
            String ch = _substr("0123456789", digit, digit + 1);
            res = ch + res;
            num = num / 10;
        }
        if (neg) {
            res = "-" + res;
        }
        return res;
    }

    static String float_to_string(double x, int dec) {
        boolean neg_1 = false;
        double num_1 = x;
        if (num_1 < 0.0) {
            neg_1 = true;
            num_1 = -num_1;
        }
        int int_part = ((Number)(num_1)).intValue();
        String res_1 = String.valueOf(int_to_string(int_part));
        if (dec > 0) {
            res_1 = res_1 + ".";
            double frac = num_1 - (((Number)(int_part)).doubleValue());
            int i = 0;
            while (i < dec) {
                frac = frac * 10.0;
                int digit_1 = ((Number)(frac)).intValue();
                res_1 = res_1 + _substr("0123456789", digit_1, digit_1 + 1);
                frac = frac - (((Number)(digit_1)).doubleValue());
                i = i + 1;
            }
        }
        if (neg_1) {
            res_1 = "-" + res_1;
        }
        return res_1;
    }

    static int vector_component(int[] v, int i) {
        return v[i];
    }

    static String vector_str_int(int[] v) {
        String s = "(";
        int i_1 = 0;
        while (i_1 < v.length) {
            s = s + String.valueOf(int_to_string(v[i_1]));
            if (i_1 + 1 < v.length) {
                s = s + ",";
            }
            i_1 = i_1 + 1;
        }
        s = s + ")";
        return s;
    }

    static String vector_str_float(double[] v, int dec) {
        String s_1 = "(";
        int i_2 = 0;
        while (i_2 < v.length) {
            s_1 = s_1 + String.valueOf(float_to_string(v[i_2], dec));
            if (i_2 + 1 < v.length) {
                s_1 = s_1 + ",";
            }
            i_2 = i_2 + 1;
        }
        s_1 = s_1 + ")";
        return s_1;
    }

    static int[] vector_add(int[] a, int[] b) {
        int[] res_2 = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < a.length) {
            res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(a[i_3] + b[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static int[] vector_sub(int[] a, int[] b) {
        int[] res_3 = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 < a.length) {
            res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(a[i_4] - b[i_4])).toArray()));
            i_4 = i_4 + 1;
        }
        return res_3;
    }

    static double[] vector_scalar_mul(int[] v, double s) {
        double[] res_4 = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < v.length) {
            res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of((((double)(v[i_5]))) * s)).toArray()));
            i_5 = i_5 + 1;
        }
        return res_4;
    }

    static int vector_dot(int[] a, int[] b) {
        int sum = 0;
        int i_6 = 0;
        while (i_6 < a.length) {
            sum = sum + a[i_6] * b[i_6];
            i_6 = i_6 + 1;
        }
        return sum;
    }

    static double sqrt_newton(double x) {
        if (x == 0.0) {
            return 0.0;
        }
        double low = 0.0;
        double high = x;
        if (x < 1.0) {
            high = 1.0;
        }
        double mid = 0.0;
        int i_7 = 0;
        while (i_7 < 40) {
            mid = (low + high) / 2.0;
            if (mid * mid > x) {
                high = mid;
            } else {
                low = mid;
            }
            i_7 = i_7 + 1;
        }
        return mid;
    }

    static double euclidean_length(int[] v) {
        double sum_1 = 0.0;
        int i_8 = 0;
        while (i_8 < v.length) {
            double val = ((double)(v[i_8]));
            sum_1 = sum_1 + val * val;
            i_8 = i_8 + 1;
        }
        return sqrt_newton(sum_1);
    }

    static int[] zero_vector(int n) {
        int[] v = ((int[])(new int[]{}));
        int i_9 = 0;
        while (i_9 < n) {
            v = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(v), java.util.stream.IntStream.of(0)).toArray()));
            i_9 = i_9 + 1;
        }
        return v;
    }

    static int[] unit_basis_vector(int n, int idx) {
        int[] v_1 = ((int[])(zero_vector(n)));
v_1[idx] = 1;
        return v_1;
    }

    static int[] axpy(int a, int[] x, int[] y) {
        int[] res_5 = ((int[])(new int[]{}));
        int i_10 = 0;
        while (i_10 < x.length) {
            res_5 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_5), java.util.stream.IntStream.of(a * x[i_10] + y[i_10])).toArray()));
            i_10 = i_10 + 1;
        }
        return res_5;
    }

    static int[] copy_vector(int[] x) {
        int[] res_6 = ((int[])(new int[]{}));
        int i_11 = 0;
        while (i_11 < x.length) {
            res_6 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_6), java.util.stream.IntStream.of(x[i_11])).toArray()));
            i_11 = i_11 + 1;
        }
        return res_6;
    }

    static void change_component(int[] v, int idx, int val) {
v[idx] = val;
    }

    static String matrix_str(int[][] m) {
        String s_2 = "";
        int i_12 = 0;
        while (i_12 < m.length) {
            s_2 = s_2 + "|";
            int j = 0;
            while (j < m[0].length) {
                s_2 = s_2 + String.valueOf(int_to_string(m[i_12][j]));
                if (j + 1 < m[0].length) {
                    s_2 = s_2 + ",";
                }
                j = j + 1;
            }
            s_2 = s_2 + "|\n";
            i_12 = i_12 + 1;
        }
        return s_2;
    }

    static int[][] submatrix(int[][] m, int row, int col) {
        int[][] res_7 = ((int[][])(new int[][]{}));
        int i_13 = 0;
        while (i_13 < m.length) {
            if (i_13 != row) {
                int[] r = ((int[])(new int[]{}));
                int j_1 = 0;
                while (j_1 < m[0].length) {
                    if (j_1 != col) {
                        r = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(m[i_13][j_1])).toArray()));
                    }
                    j_1 = j_1 + 1;
                }
                res_7 = ((int[][])(appendObj(res_7, r)));
            }
            i_13 = i_13 + 1;
        }
        return res_7;
    }

    static int determinant(int[][] m) {
        int n = m.length;
        if (n == 1) {
            return m[0][0];
        }
        if (n == 2) {
            return m[0][0] * m[1][1] - m[0][1] * m[1][0];
        }
        int det = 0;
        int c = 0;
        while (c < n) {
            int[][] sub = ((int[][])(submatrix(((int[][])(m)), 0, c)));
            int sign = 1;
            if (Math.floorMod(c, 2) == 1) {
                sign = -1;
            }
            det = det + sign * m[0][c] * determinant(((int[][])(sub)));
            c = c + 1;
        }
        return det;
    }

    static int matrix_minor(int[][] m, int row, int col) {
        return determinant(((int[][])(submatrix(((int[][])(m)), row, col))));
    }

    static int matrix_cofactor(int[][] m, int row, int col) {
        int sign_1 = 1;
        if (Math.floorMod((row + col), 2) == 1) {
            sign_1 = -1;
        }
        return sign_1 * matrix_minor(((int[][])(m)), row, col);
    }

    static int[] matrix_mul_vector(int[][] m, int[] v) {
        int[] res_8 = ((int[])(new int[]{}));
        int i_14 = 0;
        while (i_14 < m.length) {
            int sum_2 = 0;
            int j_2 = 0;
            while (j_2 < m[0].length) {
                sum_2 = sum_2 + m[i_14][j_2] * v[j_2];
                j_2 = j_2 + 1;
            }
            res_8 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_8), java.util.stream.IntStream.of(sum_2)).toArray()));
            i_14 = i_14 + 1;
        }
        return res_8;
    }

    static int[][] matrix_mul_scalar(int[][] m, int s) {
        int[][] res_9 = ((int[][])(new int[][]{}));
        int i_15 = 0;
        while (i_15 < m.length) {
            int[] row = ((int[])(new int[]{}));
            int j_3 = 0;
            while (j_3 < m[0].length) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(m[i_15][j_3] * s)).toArray()));
                j_3 = j_3 + 1;
            }
            res_9 = ((int[][])(appendObj(res_9, row)));
            i_15 = i_15 + 1;
        }
        return res_9;
    }

    static void matrix_change_component(int[][] m, int i, int j, int val) {
m[i][j] = val;
    }

    static int matrix_component(int[][] m, int i, int j) {
        return m[i][j];
    }

    static int[][] matrix_add(int[][] a, int[][] b) {
        int[][] res_10 = ((int[][])(new int[][]{}));
        int i_16 = 0;
        while (i_16 < a.length) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_4 = 0;
            while (j_4 < a[0].length) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(a[i_16][j_4] + b[i_16][j_4])).toArray()));
                j_4 = j_4 + 1;
            }
            res_10 = ((int[][])(appendObj(res_10, row_1)));
            i_16 = i_16 + 1;
        }
        return res_10;
    }

    static int[][] matrix_sub(int[][] a, int[][] b) {
        int[][] res_11 = ((int[][])(new int[][]{}));
        int i_17 = 0;
        while (i_17 < a.length) {
            int[] row_2 = ((int[])(new int[]{}));
            int j_5 = 0;
            while (j_5 < a[0].length) {
                row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(a[i_17][j_5] - b[i_17][j_5])).toArray()));
                j_5 = j_5 + 1;
            }
            res_11 = ((int[][])(appendObj(res_11, row_2)));
            i_17 = i_17 + 1;
        }
        return res_11;
    }

    static int[][] square_zero_matrix(int n) {
        int[][] m = ((int[][])(new int[][]{}));
        int i_18 = 0;
        while (i_18 < n) {
            m = ((int[][])(appendObj(m, zero_vector(n))));
            i_18 = i_18 + 1;
        }
        return m;
    }

    static void assert_int(String name, int actual, int expected) {
        if (actual == expected) {
            System.out.println(name + " ok");
        } else {
            System.out.println(name + " fail " + String.valueOf(int_to_string(actual)) + " != " + String.valueOf(int_to_string(expected)));
        }
    }

    static void assert_str(String name, String actual, String expected) {
        if ((actual.equals(expected))) {
            System.out.println(name + " ok");
        } else {
            System.out.println(name + " fail");
            System.out.println(actual);
            System.out.println(expected);
        }
    }

    static void assert_float(String name, double actual, double expected, double eps) {
        double diff = actual - expected;
        if (diff < 0.0) {
            diff = -diff;
        }
        if (diff <= eps) {
            System.out.println(name + " ok");
        } else {
            System.out.println(name + " fail");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            vx = ((int[])(new int[]{1, 2, 3}));
            assert_int("component0", vector_component(((int[])(vx)), 0), 1);
            assert_int("component2", vector_component(((int[])(vx)), 2), 3);
            vs = ((int[])(new int[]{0, 0, 0, 0, 0, 1}));
            assert_str("str_vector", String.valueOf(vector_str_int(((int[])(vs)))), "(0,0,0,0,0,1)");
            vsize = ((int[])(new int[]{1, 2, 3, 4}));
            assert_int("size", vsize.length, 4);
            va = ((int[])(new int[]{1, 2, 3}));
            vb = ((int[])(new int[]{1, 1, 1}));
            vsum = ((int[])(vector_add(((int[])(va)), ((int[])(vb)))));
            assert_int("add0", vector_component(((int[])(vsum)), 0), 2);
            assert_int("add1", vector_component(((int[])(vsum)), 1), 3);
            assert_int("add2", vector_component(((int[])(vsum)), 2), 4);
            vsub = ((int[])(vector_sub(((int[])(va)), ((int[])(vb)))));
            assert_int("sub0", vector_component(((int[])(vsub)), 0), 0);
            assert_int("sub1", vector_component(((int[])(vsub)), 1), 1);
            assert_int("sub2", vector_component(((int[])(vsub)), 2), 2);
            vmul = ((double[])(vector_scalar_mul(((int[])(va)), 3.0)));
            assert_str("scalar_mul", String.valueOf(vector_str_float(((double[])(vmul)), 1)), "(3.0,6.0,9.0)");
            assert_int("dot_product", vector_dot(((int[])(new int[]{2, -1, 4})), ((int[])(new int[]{1, -2, -1}))), 0);
            zvec = ((int[])(zero_vector(10)));
            zstr = String.valueOf(vector_str_int(((int[])(zvec))));
            zcount = 0;
            zi = 0;
            while (zi < _runeLen(zstr)) {
                if ((_substr(zstr, zi, zi + 1).equals("0"))) {
                    zcount = zcount + 1;
                }
                zi = zi + 1;
            }
            assert_int("zero_vector", zcount, 10);
            assert_str("unit_basis", String.valueOf(vector_str_int(((int[])(unit_basis_vector(3, 1))))), "(0,1,0)");
            assert_str("axpy", String.valueOf(vector_str_int(((int[])(axpy(2, ((int[])(new int[]{1, 2, 3})), ((int[])(new int[]{1, 0, 1}))))))), "(3,4,7)");
            vcopy = ((int[])(copy_vector(((int[])(new int[]{1, 0, 0, 0, 0, 0})))));
            assert_str("copy", String.valueOf(vector_str_int(((int[])(vcopy)))), "(1,0,0,0,0,0)");
            vchange = ((int[])(new int[]{1, 0, 0}));
            change_component(((int[])(vchange)), 0, 0);
            change_component(((int[])(vchange)), 1, 1);
            assert_str("change_component", String.valueOf(vector_str_int(((int[])(vchange)))), "(0,1,0)");
            ma = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{2, 4, 5}, new int[]{6, 7, 8}}));
            assert_str("matrix_str", String.valueOf(matrix_str(((int[][])(ma)))), "|1,2,3|\n|2,4,5|\n|6,7,8|\n");
            assert_int("determinant", determinant(((int[][])(ma))), -5);
            mb = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{4, 5, 6}, new int[]{7, 8, 9}}));
            mv = ((int[])(matrix_mul_vector(((int[][])(mb)), ((int[])(new int[]{1, 2, 3})))));
            assert_str("matrix_vec_mul", String.valueOf(vector_str_int(((int[])(mv)))), "(14,32,50)");
            msc = ((int[][])(matrix_mul_scalar(((int[][])(mb)), 2)));
            assert_str("matrix_scalar_mul", String.valueOf(matrix_str(((int[][])(msc)))), "|2,4,6|\n|8,10,12|\n|14,16,18|\n");
            mc = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{2, 4, 5}, new int[]{6, 7, 8}}));
            matrix_change_component(((int[][])(mc)), 0, 2, 5);
            assert_str("change_component_matrix", String.valueOf(matrix_str(((int[][])(mc)))), "|1,2,5|\n|2,4,5|\n|6,7,8|\n");
            assert_int("matrix_component", matrix_component(((int[][])(mc)), 2, 1), 7);
            madd = ((int[][])(matrix_add(((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{2, 4, 5}, new int[]{6, 7, 8}})), ((int[][])(new int[][]{new int[]{1, 2, 7}, new int[]{2, 4, 5}, new int[]{6, 7, 10}})))));
            assert_str("matrix_add", String.valueOf(matrix_str(((int[][])(madd)))), "|2,4,10|\n|4,8,10|\n|12,14,18|\n");
            msub = ((int[][])(matrix_sub(((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{2, 4, 5}, new int[]{6, 7, 8}})), ((int[][])(new int[][]{new int[]{1, 2, 7}, new int[]{2, 4, 5}, new int[]{6, 7, 10}})))));
            assert_str("matrix_sub", String.valueOf(matrix_str(((int[][])(msub)))), "|0,0,-4|\n|0,0,0|\n|0,0,-2|\n");
            mzero = ((int[][])(square_zero_matrix(5)));
            assert_str("square_zero_matrix", String.valueOf(matrix_str(((int[][])(mzero)))), "|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n");
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
