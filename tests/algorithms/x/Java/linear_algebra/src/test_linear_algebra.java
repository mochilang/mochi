public class Main {
    static long[] vx;
    static long[] vs;
    static long[] vsize;
    static long[] va;
    static long[] vb;
    static long[] vsum;
    static long[] vsub;
    static double[] vmul;
    static long[] zvec;
    static String zstr;
    static long zcount = 0;
    static long zi = 0;
    static long[] vcopy;
    static long[] vchange = new long[0];
    static long[][] ma = new long[0][];
    static long[][] mb = new long[0][];
    static long[] mv;
    static long[][] msc;
    static long[][] mc = new long[0][];
    static long[][] madd = new long[0][];
    static long[][] msub = new long[0][];
    static long[][] mzero;

    static String int_to_string(long n) {
        if (n == 0) {
            return "0";
        }
        long num_1 = n;
        boolean neg_1 = false;
        if (num_1 < 0) {
            neg_1 = true;
            num_1 = -num_1;
        }
        String res_1 = "";
        while (num_1 > 0) {
            long digit_1 = Math.floorMod(num_1, 10);
            String ch_1 = _substr("0123456789", (int)((long)(digit_1)), (int)((long)(digit_1 + 1))));
            res_1 = ch_1 + res_1;
            num_1 = Math.floorDiv(num_1, 10);
        }
        if (neg_1) {
            res_1 = "-" + res_1;
        }
        return res_1;
    }

    static String float_to_string(double x, long dec) {
        boolean neg_2 = false;
        double num_3 = x;
        if (num_3 < 0.0) {
            neg_2 = true;
            num_3 = -num_3;
        }
        long int_part_1 = ((Number)(num_3)).intValue();
        String res_3 = String.valueOf(int_to_string(int_part_1));
        if (dec > 0) {
            res_3 = res_3 + ".";
            double frac_1 = num_3 - (((Number)(int_part_1)).doubleValue());
            long i_1 = 0L;
            while (i_1 < dec) {
                frac_1 = frac_1 * 10.0;
                long digit_3 = ((Number)(frac_1)).intValue();
                res_3 = res_3 + _substr("0123456789", (int)((long)(digit_3)), (int)((long)(digit_3 + 1))));
                frac_1 = frac_1 - (((Number)(digit_3)).doubleValue());
                i_1 = i_1 + 1;
            }
        }
        if (neg_2) {
            res_3 = "-" + res_3;
        }
        return res_3;
    }

    static long vector_component(long[] v, long i) {
        return v[(int)((long)(i))];
    }

    static String vector_str_int(long[] v) {
        String s = "(";
        long i_3 = 0L;
        while (i_3 < v.length) {
            s = s + String.valueOf(int_to_string(v[(int)((long)(i_3))]));
            if (i_3 + 1 < v.length) {
                s = s + ",";
            }
            i_3 = i_3 + 1;
        }
        s = s + ")";
        return s;
    }

    static String vector_str_float(double[] v, long dec) {
        String s_1 = "(";
        long i_5 = 0L;
        while (i_5 < v.length) {
            s_1 = s_1 + String.valueOf(float_to_string(v[(int)((long)(i_5))], dec));
            if (i_5 + 1 < v.length) {
                s_1 = s_1 + ",";
            }
            i_5 = i_5 + 1;
        }
        s_1 = s_1 + ")";
        return s_1;
    }

    static long[] vector_add(long[] a, long[] b) {
        long[] res_4 = ((long[])(new long[]{}));
        long i_7 = 0L;
        while (i_7 < a.length) {
            res_4 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_4), java.util.stream.LongStream.of(a[(int)((long)(i_7))] + b[(int)((long)(i_7))])).toArray()));
            i_7 = i_7 + 1;
        }
        return res_4;
    }

    static long[] vector_sub(long[] a, long[] b) {
        long[] res_5 = ((long[])(new long[]{}));
        long i_9 = 0L;
        while (i_9 < a.length) {
            res_5 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_5), java.util.stream.LongStream.of(a[(int)((long)(i_9))] - b[(int)((long)(i_9))])).toArray()));
            i_9 = i_9 + 1;
        }
        return res_5;
    }

    static double[] vector_scalar_mul(long[] v, double s) {
        double[] res_6 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while (i_11 < v.length) {
            res_6 = ((double[])(appendDouble(res_6, (((double)(v[(int)((long)(i_11))]))) * s)));
            i_11 = i_11 + 1;
        }
        return res_6;
    }

    static long vector_dot(long[] a, long[] b) {
        long sum = 0L;
        long i_13 = 0L;
        while (i_13 < a.length) {
            sum = sum + a[(int)((long)(i_13))] * b[(int)((long)(i_13))];
            i_13 = i_13 + 1;
        }
        return sum;
    }

    static double sqrt_newton(double x) {
        if (x == 0.0) {
            return 0.0;
        }
        double low_1 = 0.0;
        double high_1 = x;
        if (x < 1.0) {
            high_1 = 1.0;
        }
        double mid_1 = 0.0;
        long i_15 = 0L;
        while (i_15 < 40) {
            mid_1 = (low_1 + high_1) / 2.0;
            if (mid_1 * mid_1 > x) {
                high_1 = mid_1;
            } else {
                low_1 = mid_1;
            }
            i_15 = i_15 + 1;
        }
        return mid_1;
    }

    static double euclidean_length(long[] v) {
        double sum_1 = 0.0;
        long i_17 = 0L;
        while (i_17 < v.length) {
            double val_1 = ((double)(v[(int)((long)(i_17))]));
            sum_1 = sum_1 + val_1 * val_1;
            i_17 = i_17 + 1;
        }
        return sqrt_newton(sum_1);
    }

    static long[] zero_vector(long n) {
        long[] v = ((long[])(new long[]{}));
        long i_19 = 0L;
        while (i_19 < n) {
            v = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(v), java.util.stream.LongStream.of(0L)).toArray()));
            i_19 = i_19 + 1;
        }
        return v;
    }

    static long[] unit_basis_vector(long n, long idx) {
        long[] v_1 = ((long[])(zero_vector(n)));
v_1[(int)((long)(idx))] = 1;
        return v_1;
    }

    static long[] axpy(long a, long[] x, long[] y) {
        long[] res_7 = ((long[])(new long[]{}));
        long i_21 = 0L;
        while (i_21 < x.length) {
            res_7 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_7), java.util.stream.LongStream.of(a * x[(int)((long)(i_21))] + y[(int)((long)(i_21))])).toArray()));
            i_21 = i_21 + 1;
        }
        return res_7;
    }

    static long[] copy_vector(long[] x) {
        long[] res_8 = ((long[])(new long[]{}));
        long i_23 = 0L;
        while (i_23 < x.length) {
            res_8 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_8), java.util.stream.LongStream.of(x[(int)((long)(i_23))])).toArray()));
            i_23 = i_23 + 1;
        }
        return res_8;
    }

    static void change_component(long[] v, long idx, long val) {
v[(int)((long)(idx))] = val;
    }

    static String matrix_str(long[][] m) {
        String s_2 = "";
        long i_25 = 0L;
        while (i_25 < m.length) {
            s_2 = s_2 + "|";
            long j_1 = 0L;
            while (j_1 < m[(int)((long)(0))].length) {
                s_2 = s_2 + String.valueOf(int_to_string(m[(int)((long)(i_25))][(int)((long)(j_1))]));
                if (j_1 + 1 < m[(int)((long)(0))].length) {
                    s_2 = s_2 + ",";
                }
                j_1 = j_1 + 1;
            }
            s_2 = s_2 + "|\n";
            i_25 = i_25 + 1;
        }
        return s_2;
    }

    static long[][] submatrix(long[][] m, long row, long col) {
        long[][] res_9 = ((long[][])(new long[][]{}));
        long i_27 = 0L;
        while (i_27 < m.length) {
            if (i_27 != row) {
                long[] r_1 = ((long[])(new long[]{}));
                long j_3 = 0L;
                while (j_3 < m[(int)((long)(0))].length) {
                    if (j_3 != col) {
                        r_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(r_1), java.util.stream.LongStream.of(m[(int)((long)(i_27))][(int)((long)(j_3))])).toArray()));
                    }
                    j_3 = j_3 + 1;
                }
                res_9 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_9), java.util.stream.Stream.of(r_1)).toArray(long[][]::new)));
            }
            i_27 = i_27 + 1;
        }
        return res_9;
    }

    static long determinant(long[][] m) {
        long n = m.length;
        if (n == 1) {
            return m[(int)((long)(0))][(int)((long)(0))];
        }
        if (n == 2) {
            return m[(int)((long)(0))][(int)((long)(0))] * m[(int)((long)(1))][(int)((long)(1))] - m[(int)((long)(0))][(int)((long)(1))] * m[(int)((long)(1))][(int)((long)(0))];
        }
        long det_1 = 0L;
        long c_1 = 0L;
        while (c_1 < n) {
            long[][] sub_1 = ((long[][])(submatrix(((long[][])(m)), 0L, c_1)));
            long sign_1 = 1L;
            if (Math.floorMod(c_1, 2) == 1) {
                sign_1 = -1;
            }
            det_1 = det_1 + sign_1 * m[(int)((long)(0))][(int)((long)(c_1))] * determinant(((long[][])(sub_1)));
            c_1 = c_1 + 1;
        }
        return det_1;
    }

    static long matrix_minor(long[][] m, long row, long col) {
        return determinant(((long[][])(submatrix(((long[][])(m)), row, col))));
    }

    static long matrix_cofactor(long[][] m, long row, long col) {
        long sign_2 = 1L;
        if (Math.floorMod((row + col), 2) == 1) {
            sign_2 = -1;
        }
        return sign_2 * matrix_minor(((long[][])(m)), row, col);
    }

    static long[] matrix_mul_vector(long[][] m, long[] v) {
        long[] res_10 = ((long[])(new long[]{}));
        long i_29 = 0L;
        while (i_29 < m.length) {
            long sum_3 = 0L;
            long j_5 = 0L;
            while (j_5 < m[(int)((long)(0))].length) {
                sum_3 = sum_3 + m[(int)((long)(i_29))][(int)((long)(j_5))] * v[(int)((long)(j_5))];
                j_5 = j_5 + 1;
            }
            res_10 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_10), java.util.stream.LongStream.of(sum_3)).toArray()));
            i_29 = i_29 + 1;
        }
        return res_10;
    }

    static long[][] matrix_mul_scalar(long[][] m, long s) {
        long[][] res_11 = ((long[][])(new long[][]{}));
        long i_31 = 0L;
        while (i_31 < m.length) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_7 = 0L;
            while (j_7 < m[(int)((long)(0))].length) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(m[(int)((long)(i_31))][(int)((long)(j_7))] * s)).toArray()));
                j_7 = j_7 + 1;
            }
            res_11 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_11), java.util.stream.Stream.of(row_1)).toArray(long[][]::new)));
            i_31 = i_31 + 1;
        }
        return res_11;
    }

    static void matrix_change_component(long[][] m, long i, long j, long val) {
m[(int)((long)(i))][(int)((long)(j))] = val;
    }

    static long matrix_component(long[][] m, long i, long j) {
        return m[(int)((long)(i))][(int)((long)(j))];
    }

    static long[][] matrix_add(long[][] a, long[][] b) {
        long[][] res_12 = ((long[][])(new long[][]{}));
        long i_33 = 0L;
        while (i_33 < a.length) {
            long[] row_3 = ((long[])(new long[]{}));
            long j_9 = 0L;
            while (j_9 < a[(int)((long)(0))].length) {
                row_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_3), java.util.stream.LongStream.of(a[(int)((long)(i_33))][(int)((long)(j_9))] + b[(int)((long)(i_33))][(int)((long)(j_9))])).toArray()));
                j_9 = j_9 + 1;
            }
            res_12 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_12), java.util.stream.Stream.of(row_3)).toArray(long[][]::new)));
            i_33 = i_33 + 1;
        }
        return res_12;
    }

    static long[][] matrix_sub(long[][] a, long[][] b) {
        long[][] res_13 = ((long[][])(new long[][]{}));
        long i_35 = 0L;
        while (i_35 < a.length) {
            long[] row_5 = ((long[])(new long[]{}));
            long j_11 = 0L;
            while (j_11 < a[(int)((long)(0))].length) {
                row_5 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_5), java.util.stream.LongStream.of(a[(int)((long)(i_35))][(int)((long)(j_11))] - b[(int)((long)(i_35))][(int)((long)(j_11))])).toArray()));
                j_11 = j_11 + 1;
            }
            res_13 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_13), java.util.stream.Stream.of(row_5)).toArray(long[][]::new)));
            i_35 = i_35 + 1;
        }
        return res_13;
    }

    static long[][] square_zero_matrix(long n) {
        long[][] m = ((long[][])(new long[][]{}));
        long i_37 = 0L;
        while (i_37 < n) {
            m = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(m), java.util.stream.Stream.of(zero_vector(n))).toArray(long[][]::new)));
            i_37 = i_37 + 1;
        }
        return m;
    }

    static void assert_int(String name, long actual, long expected) {
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
        vx = ((long[])(new long[]{1, 2, 3}));
        assert_int("component0", vector_component(((long[])(vx)), 0L), 1L);
        assert_int("component2", vector_component(((long[])(vx)), 2L), 3L);
        vs = ((long[])(new long[]{0, 0, 0, 0, 0, 1}));
        assert_str("str_vector", String.valueOf(vector_str_int(((long[])(vs)))), "(0,0,0,0,0,1)");
        vsize = ((long[])(new long[]{1, 2, 3, 4}));
        assert_int("size", vsize.length, 4L);
        va = ((long[])(new long[]{1, 2, 3}));
        vb = ((long[])(new long[]{1, 1, 1}));
        vsum = ((long[])(vector_add(((long[])(va)), ((long[])(vb)))));
        assert_int("add0", vector_component(((long[])(vsum)), 0L), 2L);
        assert_int("add1", vector_component(((long[])(vsum)), 1L), 3L);
        assert_int("add2", vector_component(((long[])(vsum)), 2L), 4L);
        vsub = ((long[])(vector_sub(((long[])(va)), ((long[])(vb)))));
        assert_int("sub0", vector_component(((long[])(vsub)), 0L), 0L);
        assert_int("sub1", vector_component(((long[])(vsub)), 1L), 1L);
        assert_int("sub2", vector_component(((long[])(vsub)), 2L), 2L);
        vmul = ((double[])(vector_scalar_mul(((long[])(va)), 3.0)));
        assert_str("scalar_mul", String.valueOf(vector_str_float(((double[])(vmul)), 1L)), "(3.0,6.0,9.0)");
        assert_int("dot_product", vector_dot(((long[])(new long[]{2, -1, 4})), ((long[])(new long[]{1, -2, -1}))), 0L);
        zvec = ((long[])(zero_vector(10L)));
        zstr = String.valueOf(vector_str_int(((long[])(zvec))));
        zcount = 0;
        zi = 0;
        while (zi < _runeLen(zstr)) {
            if ((_substr(zstr, (int)((long)(zi)), (int)((long)(zi + 1)))).equals("0"))) {
                zcount = zcount + 1;
            }
            zi = zi + 1;
        }
        assert_int("zero_vector", zcount, 10L);
        assert_str("unit_basis", String.valueOf(vector_str_int(((long[])(unit_basis_vector(3L, 1L))))), "(0,1,0)");
        assert_str("axpy", String.valueOf(vector_str_int(((long[])(axpy(2L, ((long[])(new long[]{1, 2, 3})), ((long[])(new long[]{1, 0, 1}))))))), "(3,4,7)");
        vcopy = ((long[])(copy_vector(((long[])(new long[]{1, 0, 0, 0, 0, 0})))));
        assert_str("copy", String.valueOf(vector_str_int(((long[])(vcopy)))), "(1,0,0,0,0,0)");
        vchange = ((long[])(new long[]{1, 0, 0}));
        change_component(((long[])(vchange)), 0L, 0L);
        change_component(((long[])(vchange)), 1L, 1L);
        assert_str("change_component", String.valueOf(vector_str_int(((long[])(vchange)))), "(0,1,0)");
        ma = ((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{2, 4, 5}, new long[]{6, 7, 8}}));
        assert_str("matrix_str", String.valueOf(matrix_str(((long[][])(ma)))), "|1,2,3|\n|2,4,5|\n|6,7,8|\n");
        assert_int("determinant", determinant(((long[][])(ma))), -5);
        mb = ((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{4, 5, 6}, new long[]{7, 8, 9}}));
        mv = ((long[])(matrix_mul_vector(((long[][])(mb)), ((long[])(new long[]{1, 2, 3})))));
        assert_str("matrix_vec_mul", String.valueOf(vector_str_int(((long[])(mv)))), "(14,32,50)");
        msc = ((long[][])(matrix_mul_scalar(((long[][])(mb)), 2L)));
        assert_str("matrix_scalar_mul", String.valueOf(matrix_str(((long[][])(msc)))), "|2,4,6|\n|8,10,12|\n|14,16,18|\n");
        mc = ((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{2, 4, 5}, new long[]{6, 7, 8}}));
        matrix_change_component(((long[][])(mc)), 0L, 2L, 5L);
        assert_str("change_component_matrix", String.valueOf(matrix_str(((long[][])(mc)))), "|1,2,5|\n|2,4,5|\n|6,7,8|\n");
        assert_int("matrix_component", matrix_component(((long[][])(mc)), 2L, 1L), 7L);
        madd = ((long[][])(matrix_add(((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{2, 4, 5}, new long[]{6, 7, 8}})), ((long[][])(new long[][]{new long[]{1, 2, 7}, new long[]{2, 4, 5}, new long[]{6, 7, 10}})))));
        assert_str("matrix_add", String.valueOf(matrix_str(((long[][])(madd)))), "|2,4,10|\n|4,8,10|\n|12,14,18|\n");
        msub = ((long[][])(matrix_sub(((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{2, 4, 5}, new long[]{6, 7, 8}})), ((long[][])(new long[][]{new long[]{1, 2, 7}, new long[]{2, 4, 5}, new long[]{6, 7, 10}})))));
        assert_str("matrix_sub", String.valueOf(matrix_str(((long[][])(msub)))), "|0,0,-4|\n|0,0,0|\n|0,0,-2|\n");
        mzero = ((long[][])(square_zero_matrix(5L)));
        assert_str("square_zero_matrix", String.valueOf(matrix_str(((long[][])(mzero)))), "|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n");
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
