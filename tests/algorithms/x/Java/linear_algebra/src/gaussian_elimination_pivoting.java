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
        int i = 0;
        while (i < src.length) {
            double[] row_src = ((double[])(src[i]));
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < row_src.length) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(row_src[j])).toArray()));
                j = j + 1;
            }
            res = ((double[][])(appendObj(res, row)));
            i = i + 1;
        }
        return res;
    }

    static double[] solve_linear_system(double[][] matrix) {
        double[][] ab = ((double[][])(copy_matrix(((double[][])(matrix)))));
        int num_rows = ab.length;
        int num_cols = ab[0].length - 1;
        if (num_rows != num_cols) {
            throw new RuntimeException(String.valueOf("Matrix is not square"));
            return new double[]{};
        }
        int column_num = 0;
        while (column_num < num_rows) {
            int i_1 = column_num;
            while (i_1 < num_cols) {
                if (abs_float(ab[i_1][column_num]) > abs_float(ab[column_num][column_num])) {
                    double[] temp = ((double[])(ab[column_num]));
ab[column_num] = ((double[])(ab[i_1]));
ab[i_1] = ((double[])(temp));
                }
                i_1 = i_1 + 1;
            }
            if (abs_float(ab[column_num][column_num]) < 1e-08) {
                throw new RuntimeException(String.valueOf("Matrix is singular"));
                return new double[]{};
            }
            if (column_num != 0) {
                i_1 = column_num;
                while (i_1 < num_rows) {
                    double factor = ab[i_1][column_num - 1] / ab[column_num - 1][column_num - 1];
                    int j_1 = 0;
                    while (j_1 < ab[i_1].length) {
ab[i_1][j_1] = ab[i_1][j_1] - factor * ab[column_num - 1][j_1];
                        j_1 = j_1 + 1;
                    }
                    i_1 = i_1 + 1;
                }
            }
            column_num = column_num + 1;
        }
        double[] x_lst = ((double[])(new double[]{}));
        int t = 0;
        while (t < num_rows) {
            x_lst = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x_lst), java.util.stream.DoubleStream.of(0.0)).toArray()));
            t = t + 1;
        }
        column_num = num_rows - 1;
        while (column_num >= 0) {
            double x = ab[column_num][num_cols] / ab[column_num][column_num];
x_lst[column_num] = x;
            int i_2 = column_num - 1;
            while (i_2 >= 0) {
ab[i_2][num_cols] = ab[i_2][num_cols] - ab[i_2][column_num] * x;
                i_2 = i_2 - 1;
            }
            column_num = column_num - 1;
        }
        return x_lst;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example_matrix = ((double[][])(new double[][]{new double[]{5.0, -5.0, -3.0, 4.0, -11.0}, new double[]{1.0, -4.0, 6.0, -4.0, -10.0}, new double[]{-2.0, -5.0, 4.0, -5.0, -12.0}, new double[]{-3.0, -3.0, 5.0, -5.0, 8.0}}));
            System.out.println("Matrix:");
            System.out.println(_p(example_matrix));
            solution = ((double[])(solve_linear_system(((double[][])(example_matrix)))));
            System.out.println(_p(solution));
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
}
