public class Main {
    static double[][] mat = new double[0][];

    static double[][] invert_matrix(double[][] matrix) {
        int n = matrix.length;
        double[][] aug = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < n) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(matrix[i][j])).toArray()));
                j = j + 1;
            }
            int k = 0;
            while (k < n) {
                if (i == k) {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                k = k + 1;
            }
            aug = ((double[][])(appendObj(aug, row)));
            i = i + 1;
        }
        int col = 0;
        while (col < n) {
            int pivot_row = col;
            int r = col;
            while (r < n) {
                if (aug[r][col] != 0.0) {
                    pivot_row = r;
                    break;
                }
                r = r + 1;
            }
            if (aug[pivot_row][col] == 0.0) {
                throw new RuntimeException(String.valueOf("Matrix is not invertible"));
            }
            if (pivot_row != col) {
                double[] temp = ((double[])(aug[col]));
aug[col] = ((double[])(aug[pivot_row]));
aug[pivot_row] = ((double[])(temp));
            }
            double pivot = aug[col][col];
            int c = 0;
            while (c < 2 * n) {
aug[col][c] = aug[col][c] / pivot;
                c = c + 1;
            }
            int r2 = 0;
            while (r2 < n) {
                if (r2 != col) {
                    double factor = aug[r2][col];
                    int c2 = 0;
                    while (c2 < 2 * n) {
aug[r2][c2] = aug[r2][c2] - factor * aug[col][c2];
                        c2 = c2 + 1;
                    }
                }
                r2 = r2 + 1;
            }
            col = col + 1;
        }
        double[][] inv = ((double[][])(new double[][]{}));
        int r3 = 0;
        while (r3 < n) {
            double[] row_1 = ((double[])(new double[]{}));
            int c3 = 0;
            while (c3 < n) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(aug[r3][c3 + n])).toArray()));
                c3 = c3 + 1;
            }
            inv = ((double[][])(appendObj(inv, row_1)));
            r3 = r3 + 1;
        }
        return inv;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            mat = ((double[][])(new double[][]{new double[]{4.0, 7.0}, new double[]{2.0, 6.0}}));
            System.out.println("Original Matrix:");
            System.out.println(java.util.Arrays.deepToString(mat));
            System.out.println("Inverted Matrix:");
            System.out.println(invert_matrix(((double[][])(mat))));
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
}
