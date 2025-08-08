public class Main {
    static int[][] image;
    static double[][] glcm;
    static double[] descriptors;
    static int idx = 0;

    static int abs_int(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double ln(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double e = 2.718281828;
        int n = 0;
        double y = x;
        while (y >= e) {
            y = y / e;
            n = n + 1;
        }
        while (y <= 1.0 / e) {
            y = y * e;
            n = n - 1;
        }
        y = y - 1.0;
        double term = y;
        double result = 0.0;
        int k = 1;
        while (k <= 20) {
            if (Math.floorMod(k, 2) == 1) {
                result = result + term / (1.0 * k);
            } else {
                result = result - term / (1.0 * k);
            }
            term = term * y;
            k = k + 1;
        }
        return result + (1.0 * n);
    }

    static double[][] matrix_concurrency(int[][] image, int[] coord) {
        int offset_x = coord[0];
        int offset_y = coord[1];
        int max_val = 0;
        for (int r = 0; r < image.length; r++) {
            for (int c = 0; c < image[r].length; c++) {
                if (image[r][c] > max_val) {
                    max_val = image[r][c];
                }
            }
        }
        int size = max_val + 1;
        double[][] matrix = ((double[][])(new double[][]{}));
        for (int i = 0; i < size; i++) {
            double[] row = ((double[])(new double[]{}));
            for (int j = 0; j < size; j++) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
            }
            matrix = ((double[][])(appendObj((double[][])matrix, row)));
        }
        for (int x = 1; x < image.length - 1; x++) {
            for (int y = 1; y < image[x].length - 1; y++) {
                int base = image[x][y];
                int offset = image[x + offset_x][y + offset_y];
matrix[base][offset] = matrix[base][offset] + 1.0;
            }
        }
        double total = 0.0;
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                total = total + matrix[i][j];
            }
        }
        if (total == 0.0) {
            return matrix;
        }
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
matrix[i][j] = matrix[i][j] / total;
            }
        }
        return matrix;
    }

    static double[] haralick_descriptors(double[][] matrix) {
        int rows = matrix.length;
        int cols = matrix[0].length;
        double maximum_prob = 0.0;
        double correlation = 0.0;
        double energy = 0.0;
        double contrast = 0.0;
        double dissimilarity = 0.0;
        double inverse_difference = 0.0;
        double homogeneity = 0.0;
        double entropy = 0.0;
        int i_1 = 0;
        while (i_1 < rows) {
            int j = 0;
            while (j < cols) {
                double val = matrix[i_1][j];
                if (val > maximum_prob) {
                    maximum_prob = val;
                }
                correlation = correlation + (1.0 * i_1 * j) * val;
                energy = energy + val * val;
                int diff = i_1 - j;
                int adiff = abs_int(diff);
                contrast = contrast + val * (1.0 * diff * diff);
                dissimilarity = dissimilarity + val * (1.0 * adiff);
                inverse_difference = inverse_difference + val / (1.0 + (1.0 * adiff));
                homogeneity = homogeneity + val / (1.0 + (1.0 * diff * diff));
                if (val > 0.0) {
                    entropy = entropy - (val * ln(val));
                }
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return new double[]{maximum_prob, correlation, energy, contrast, dissimilarity, inverse_difference, homogeneity, entropy};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            image = ((int[][])(new int[][]{new int[]{0, 1, 0}, new int[]{1, 0, 1}, new int[]{0, 1, 0}}));
            glcm = ((double[][])(matrix_concurrency(((int[][])(image)), ((int[])(new int[]{0, 1})))));
            descriptors = ((double[])(haralick_descriptors(((double[][])(glcm)))));
            idx = 0;
            while (idx < descriptors.length) {
                System.out.println(_p(_getd(descriptors, idx)));
                idx = idx + 1;
            }
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
