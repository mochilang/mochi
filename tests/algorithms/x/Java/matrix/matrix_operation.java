public class Main {

    static double[][] add(double[][][] matrices) {
        long rows = (long)(matrices[(int)(0L)].length);
        long cols_1 = (long)(matrices[(int)(0L)][(int)(0L)].length);
        long r_1 = 0L;
        double[][] result_1 = ((double[][])(new double[][]{}));
        while ((long)(r_1) < (long)(rows)) {
            double[] row_1 = ((double[])(new double[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < (long)(cols_1)) {
                double sum_1 = (double)(0.0);
                long m_1 = 0L;
                while ((long)(m_1) < (long)(matrices.length)) {
                    sum_1 = (double)((double)(sum_1) + (double)(matrices[(int)((long)(m_1))][(int)((long)(r_1))][(int)((long)(c_1))]));
                    m_1 = (long)((long)(m_1) + 1L);
                }
                row_1 = ((double[])(appendDouble(row_1, (double)(sum_1))));
                c_1 = (long)((long)(c_1) + 1L);
            }
            result_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            r_1 = (long)((long)(r_1) + 1L);
        }
        return result_1;
    }

    static double[][] subtract(double[][] a, double[][] b) {
        long rows_1 = (long)(a.length);
        long cols_3 = (long)(a[(int)(0L)].length);
        long r_3 = 0L;
        double[][] result_3 = ((double[][])(new double[][]{}));
        while ((long)(r_3) < (long)(rows_1)) {
            double[] row_3 = ((double[])(new double[]{}));
            long c_3 = 0L;
            while ((long)(c_3) < (long)(cols_3)) {
                row_3 = ((double[])(appendDouble(row_3, (double)((double)(a[(int)((long)(r_3))][(int)((long)(c_3))]) - (double)(b[(int)((long)(r_3))][(int)((long)(c_3))])))));
                c_3 = (long)((long)(c_3) + 1L);
            }
            result_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            r_3 = (long)((long)(r_3) + 1L);
        }
        return result_3;
    }

    static double[][] scalar_multiply(double[][] matrix, double n) {
        double[][] result_4 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(matrix.length)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(matrix[(int)((long)(i_1))].length)) {
                row_5 = ((double[])(appendDouble(row_5, (double)((double)(matrix[(int)((long)(i_1))][(int)((long)(j_1))]) * (double)(n)))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            result_4 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_4), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result_4;
    }

    static double[][] multiply(double[][] a, double[][] b) {
        long rowsA = (long)(a.length);
        long colsA_1 = (long)(a[(int)(0L)].length);
        long rowsB_1 = (long)(b.length);
        long colsB_1 = (long)(b[(int)(0L)].length);
        double[][] result_6 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(rowsA)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(colsB_1)) {
                double sum_3 = (double)(0.0);
                long k_1 = 0L;
                while ((long)(k_1) < (long)(colsA_1)) {
                    sum_3 = (double)((double)(sum_3) + (double)((double)(a[(int)((long)(i_3))][(int)((long)(k_1))]) * (double)(b[(int)((long)(k_1))][(int)((long)(j_3))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_7 = ((double[])(appendDouble(row_7, (double)(sum_3))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            result_6 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_6), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result_6;
    }

    static double[][] identity(long n) {
        double[][] result_7 = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(n)) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(n)) {
                if ((long)(i_5) == (long)(j_5)) {
                    row_9 = ((double[])(appendDouble(row_9, (double)(1.0))));
                } else {
                    row_9 = ((double[])(appendDouble(row_9, (double)(0.0))));
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            result_7 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_7), java.util.stream.Stream.of(new double[][]{row_9})).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return result_7;
    }

    static double[][] transpose(double[][] matrix) {
        long rows_2 = (long)(matrix.length);
        long cols_5 = (long)(matrix[(int)(0L)].length);
        double[][] result_9 = ((double[][])(new double[][]{}));
        long c_5 = 0L;
        while ((long)(c_5) < (long)(cols_5)) {
            double[] row_11 = ((double[])(new double[]{}));
            long r_5 = 0L;
            while ((long)(r_5) < (long)(rows_2)) {
                row_11 = ((double[])(appendDouble(row_11, (double)(matrix[(int)((long)(r_5))][(int)((long)(c_5))]))));
                r_5 = (long)((long)(r_5) + 1L);
            }
            result_9 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_9), java.util.stream.Stream.of(new double[][]{row_11})).toArray(double[][]::new)));
            c_5 = (long)((long)(c_5) + 1L);
        }
        return result_9;
    }

    static double[][] minor(double[][] matrix, long row, long column) {
        double[][] result_10 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(matrix.length)) {
            if ((long)(i_7) != (long)(row)) {
                double[] new_row_1 = ((double[])(new double[]{}));
                long j_7 = 0L;
                while ((long)(j_7) < (long)(matrix[(int)((long)(i_7))].length)) {
                    if ((long)(j_7) != (long)(column)) {
                        new_row_1 = ((double[])(appendDouble(new_row_1, (double)(matrix[(int)((long)(i_7))][(int)((long)(j_7))]))));
                    }
                    j_7 = (long)((long)(j_7) + 1L);
                }
                result_10 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_10), java.util.stream.Stream.of(new double[][]{new_row_1})).toArray(double[][]::new)));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return result_10;
    }

    static double determinant(double[][] matrix) {
        if ((long)(matrix.length) == 1L) {
            return matrix[(int)(0L)][(int)(0L)];
        }
        double det_1 = (double)(0.0);
        long c_7 = 0L;
        while ((long)(c_7) < (long)(matrix[(int)(0L)].length)) {
            double[][] sub_1 = ((double[][])(minor(((double[][])(matrix)), 0L, (long)(c_7))));
            double sign_1 = (double)(Math.floorMod(c_7, 2) == 0L ? 1.0 : -1.0);
            det_1 = (double)((double)(det_1) + (double)((double)((double)(matrix[(int)(0L)][(int)((long)(c_7))]) * (double)(determinant(((double[][])(sub_1))))) * (double)(sign_1)));
            c_7 = (long)((long)(c_7) + 1L);
        }
        return det_1;
    }

    static double[][] inverse(double[][] matrix) {
        double det_2 = (double)(determinant(((double[][])(matrix))));
        if ((double)(det_2) == (double)(0.0)) {
            return new double[][]{};
        }
        long size_1 = (long)(matrix.length);
        double[][] matrix_minor_1 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(size_1)) {
            double[] row_13 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < (long)(size_1)) {
                double[][] m_3 = ((double[][])(minor(((double[][])(matrix)), (long)(i_9), (long)(j_9))));
                row_13 = ((double[])(appendDouble(row_13, (double)(determinant(((double[][])(m_3)))))));
                j_9 = (long)((long)(j_9) + 1L);
            }
            matrix_minor_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix_minor_1), java.util.stream.Stream.of(new double[][]{row_13})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        double[][] cofactors_1 = ((double[][])(new double[][]{}));
        i_9 = 0L;
        while ((long)(i_9) < (long)(size_1)) {
            double[] row_15 = ((double[])(new double[]{}));
            long j_11 = 0L;
            while ((long)(j_11) < (long)(size_1)) {
                double sign_3 = (double)(Math.floorMod(((long)(i_9) + (long)(j_11)), 2) == 0L ? 1.0 : -1.0);
                row_15 = ((double[])(appendDouble(row_15, (double)((double)(matrix_minor_1[(int)((long)(i_9))][(int)((long)(j_11))]) * (double)(sign_3)))));
                j_11 = (long)((long)(j_11) + 1L);
            }
            cofactors_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(cofactors_1), java.util.stream.Stream.of(new double[][]{row_15})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        double[][] adjugate_1 = ((double[][])(transpose(((double[][])(cofactors_1)))));
        return scalar_multiply(((double[][])(adjugate_1)), (double)((double)(1.0) / (double)(det_2)));
    }

    static void main() {
        double[][] matrix_a = ((double[][])(new double[][]{new double[]{12.0, 10.0}, new double[]{3.0, 9.0}}));
        double[][] matrix_b_1 = ((double[][])(new double[][]{new double[]{3.0, 4.0}, new double[]{7.0, 4.0}}));
        double[][] matrix_c_1 = ((double[][])(new double[][]{new double[]{11.0, 12.0, 13.0, 14.0}, new double[]{21.0, 22.0, 23.0, 24.0}, new double[]{31.0, 32.0, 33.0, 34.0}, new double[]{41.0, 42.0, 43.0, 44.0}}));
        double[][] matrix_d_1 = ((double[][])(new double[][]{new double[]{3.0, 0.0, 2.0}, new double[]{2.0, 0.0, -2.0}, new double[]{0.0, 1.0, 1.0}}));
        System.out.println("Add Operation, add(matrix_a, matrix_b) = " + _p(add(((double[][][])(new double[][][]{matrix_a, matrix_b_1})))) + " \n");
        System.out.println("Multiply Operation, multiply(matrix_a, matrix_b) = " + _p(multiply(((double[][])(matrix_a)), ((double[][])(matrix_b_1)))) + " \n");
        System.out.println("Identity: " + _p(identity(5L)) + "\n");
        System.out.println("Minor of " + _p(matrix_c_1) + " = " + _p(minor(((double[][])(matrix_c_1)), 1L, 2L)) + " \n");
        System.out.println("Determinant of " + _p(matrix_b_1) + " = " + _p(determinant(((double[][])(matrix_b_1)))) + " \n");
        System.out.println("Inverse of " + _p(matrix_d_1) + " = " + _p(inverse(((double[][])(matrix_d_1)))) + "\n");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
}
