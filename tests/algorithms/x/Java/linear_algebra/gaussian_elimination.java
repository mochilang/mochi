public class Main {

    static double[][] retroactive_resolution(double[][] coefficients, double[][] vector) {
        int rows = coefficients.length;
        double[][] x = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < rows) {
            double[] inner = ((double[])(new double[]{}));
            inner = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(inner), java.util.stream.DoubleStream.of(0.0)).toArray()));
            x = ((double[][])(appendObj(x, inner)));
            i = i + 1;
        }
        int r = rows - 1;
        while (r >= 0) {
            double total = 0.0;
            int c = r + 1;
            while (c < rows) {
                total = total + coefficients[r][c] * x[c][0];
                c = c + 1;
            }
x[r][0] = (vector[r][0] - total) / coefficients[r][r];
            r = r - 1;
        }
        return x;
    }

    static double[][] gaussian_elimination(double[][] coefficients, double[][] vector) {
        int rows_1 = coefficients.length;
        int columns = coefficients[0].length;
        if (rows_1 != columns) {
            return new double[][]{};
        }
        double[][] augmented = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < rows_1) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < columns) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(coefficients[i_1][j])).toArray()));
                j = j + 1;
            }
            row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(vector[i_1][0])).toArray()));
            augmented = ((double[][])(appendObj(augmented, row)));
            i_1 = i_1 + 1;
        }
        int row_idx = 0;
        while (row_idx < rows_1 - 1) {
            double pivot = augmented[row_idx][row_idx];
            int col = row_idx + 1;
            while (col < rows_1) {
                double factor = augmented[col][row_idx] / pivot;
                int k = row_idx;
                while (k < columns + 1) {
augmented[col][k] = augmented[col][k] - factor * augmented[row_idx][k];
                    k = k + 1;
                }
                col = col + 1;
            }
            row_idx = row_idx + 1;
        }
        double[][] coeffs = ((double[][])(new double[][]{}));
        double[][] vec = ((double[][])(new double[][]{}));
        int r_1 = 0;
        while (r_1 < rows_1) {
            double[] row_1 = ((double[])(new double[]{}));
            int c_1 = 0;
            while (c_1 < columns) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(augmented[r_1][c_1])).toArray()));
                c_1 = c_1 + 1;
            }
            coeffs = ((double[][])(appendObj(coeffs, row_1)));
            vec = ((double[][])(appendObj(vec, new double[]{augmented[r_1][columns]})));
            r_1 = r_1 + 1;
        }
        double[][] x_1 = ((double[][])(retroactive_resolution(((double[][])(coeffs)), ((double[][])(vec)))));
        return x_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(gaussian_elimination(((double[][])(new double[][]{new double[]{1.0, -4.0, -2.0}, new double[]{5.0, 2.0, -2.0}, new double[]{1.0, -1.0, 0.0}})), ((double[][])(new double[][]{new double[]{-2.0}, new double[]{-3.0}, new double[]{4.0}}))));
            System.out.println(gaussian_elimination(((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{5.0, 2.0}})), ((double[][])(new double[][]{new double[]{5.0}, new double[]{5.0}}))));
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
