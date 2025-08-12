public class Main {
    static double[][] example_matrix = new double[0][];
    static double[] solution = new double[0];

    static void panic(String msg) {
        System.out.println(msg);
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double[][] copy_matrix(double[][] src) {
        double[][] res = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while (i_1 < src.length) {
            double[] row_src_1 = ((double[])(src[(int)((long)(i_1))]));
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while (j_1 < row_src_1.length) {
                row_1 = ((double[])(appendDouble(row_1, row_src_1[(int)((long)(j_1))])));
                j_1 = j_1 + 1;
            }
            res = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static double[] solve_linear_system(double[][] matrix) {
        double[][] ab = ((double[][])(copy_matrix(((double[][])(matrix)))));
        long num_rows_1 = ab.length;
        long num_cols_1 = ab[(int)((long)(0))].length - 1;
        if (num_rows_1 != num_cols_1) {
            panic("Matrix is not square");
        }
        long column_num_1 = 0L;
        while (column_num_1 < num_rows_1) {
            long i_3 = column_num_1;
            while (i_3 < num_cols_1) {
                if (abs_float(ab[(int)((long)(i_3))][(int)((long)(column_num_1))]) > abs_float(ab[(int)((long)(column_num_1))][(int)((long)(column_num_1))])) {
                    double[] temp_1 = ((double[])(ab[(int)((long)(column_num_1))]));
ab[(int)((long)(column_num_1))] = ((double[])(ab[(int)((long)(i_3))]));
ab[(int)((long)(i_3))] = ((double[])(temp_1));
                }
                i_3 = i_3 + 1;
            }
            if (abs_float(ab[(int)((long)(column_num_1))][(int)((long)(column_num_1))]) < 1e-08) {
                panic("Matrix is singular");
            }
            if (column_num_1 != 0) {
                i_3 = column_num_1;
                while (i_3 < num_rows_1) {
                    double factor_1 = ab[(int)((long)(i_3))][(int)((long)(column_num_1 - 1))] / ab[(int)((long)(column_num_1 - 1))][(int)((long)(column_num_1 - 1))];
                    long j_3 = 0L;
                    while (j_3 < ab[(int)((long)(i_3))].length) {
ab[(int)((long)(i_3))][(int)((long)(j_3))] = ab[(int)((long)(i_3))][(int)((long)(j_3))] - factor_1 * ab[(int)((long)(column_num_1 - 1))][(int)((long)(j_3))];
                        j_3 = j_3 + 1;
                    }
                    i_3 = i_3 + 1;
                }
            }
            column_num_1 = column_num_1 + 1;
        }
        double[] x_lst_1 = ((double[])(new double[]{}));
        long t_1 = 0L;
        while (t_1 < num_rows_1) {
            x_lst_1 = ((double[])(appendDouble(x_lst_1, 0.0)));
            t_1 = t_1 + 1;
        }
        column_num_1 = num_rows_1 - 1;
        while (column_num_1 >= 0) {
            double x_1 = ab[(int)((long)(column_num_1))][(int)((long)(num_cols_1))] / ab[(int)((long)(column_num_1))][(int)((long)(column_num_1))];
x_lst_1[(int)((long)(column_num_1))] = x_1;
            long i_5 = column_num_1 - 1;
            while (i_5 >= 0) {
ab[(int)((long)(i_5))][(int)((long)(num_cols_1))] = ab[(int)((long)(i_5))][(int)((long)(num_cols_1))] - ab[(int)((long)(i_5))][(int)((long)(column_num_1))] * x_1;
                i_5 = i_5 - 1;
            }
            column_num_1 = column_num_1 - 1;
        }
        return x_lst_1;
    }
    public static void main(String[] args) {
        example_matrix = ((double[][])(new double[][]{new double[]{5.0, -5.0, -3.0, 4.0, -11.0}, new double[]{1.0, -4.0, 6.0, -4.0, -10.0}, new double[]{-2.0, -5.0, 4.0, -5.0, -12.0}, new double[]{-3.0, -3.0, 5.0, -5.0, 8.0}}));
        System.out.println("Matrix:");
        System.out.println(_p(example_matrix));
        solution = ((double[])(solve_linear_system(((double[][])(example_matrix)))));
        System.out.println(_p(solution));
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
}
