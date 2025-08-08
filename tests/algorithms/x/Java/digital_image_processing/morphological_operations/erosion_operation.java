public class Main {
    static int[][][] rgb_img;
    static double[][] gray_img;
    static boolean[][] img1;
    static int[][] kernel1;
    static boolean[][] img2;
    static int[][] kernel2;

    static double[][] rgb_to_gray(int[][][] rgb) {
        double[][] gray = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < rgb.length) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < rgb[i].length) {
                double r = ((double)(rgb[i][j][0]));
                double g = ((double)(rgb[i][j][1]));
                double b = ((double)(rgb[i][j][2]));
                double value = 0.2989 * r + 0.587 * g + 0.114 * b;
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(value)).toArray()));
                j = j + 1;
            }
            gray = ((double[][])(appendObj(gray, row)));
            i = i + 1;
        }
        return gray;
    }

    static boolean[][] gray_to_binary(double[][] gray) {
        boolean[][] binary = ((boolean[][])(new boolean[][]{}));
        int i_1 = 0;
        while (i_1 < gray.length) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            int j_1 = 0;
            while (j_1 < gray[i_1].length) {
                row_1 = ((boolean[])(appendBool(row_1, gray[i_1][j_1] > 127.0 && gray[i_1][j_1] <= 255.0)));
                j_1 = j_1 + 1;
            }
            binary = ((boolean[][])(appendObj(binary, row_1)));
            i_1 = i_1 + 1;
        }
        return binary;
    }

    static boolean[][] erosion(boolean[][] image, int[][] kernel) {
        int h = image.length;
        int w = image[0].length;
        int k_h = kernel.length;
        int k_w = kernel[0].length;
        int pad_y = Math.floorDiv(k_h, 2);
        int pad_x = Math.floorDiv(k_w, 2);
        boolean[][] padded = ((boolean[][])(new boolean[][]{}));
        int y = 0;
        while (y < h + 2 * pad_y) {
            boolean[] row_2 = ((boolean[])(new boolean[]{}));
            int x = 0;
            while (x < w + 2 * pad_x) {
                row_2 = ((boolean[])(appendBool(row_2, false)));
                x = x + 1;
            }
            padded = ((boolean[][])(appendObj(padded, row_2)));
            y = y + 1;
        }
        y = 0;
        while (y < h) {
            int x_1 = 0;
            while (x_1 < w) {
padded[pad_y + y][pad_x + x_1] = image[y][x_1];
                x_1 = x_1 + 1;
            }
            y = y + 1;
        }
        boolean[][] output = ((boolean[][])(new boolean[][]{}));
        y = 0;
        while (y < h) {
            boolean[] row_out = ((boolean[])(new boolean[]{}));
            int x_2 = 0;
            while (x_2 < w) {
                int sum = 0;
                int ky = 0;
                while (ky < k_h) {
                    int kx = 0;
                    while (kx < k_w) {
                        if (kernel[ky][kx] == 1 && ((Boolean)(padded[y + ky][x_2 + kx]))) {
                            sum = sum + 1;
                        }
                        kx = kx + 1;
                    }
                    ky = ky + 1;
                }
                row_out = ((boolean[])(appendBool(row_out, sum == 5)));
                x_2 = x_2 + 1;
            }
            output = ((boolean[][])(appendObj(output, row_out)));
            y = y + 1;
        }
        return output;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            rgb_img = ((int[][][])(new int[][][]{new int[][]{new int[]{127, 255, 0}}}));
            System.out.println(_p(rgb_to_gray(((int[][][])(rgb_img)))));
            gray_img = ((double[][])(new double[][]{new double[]{127.0, 255.0, 0.0}}));
            System.out.println(_p(gray_to_binary(((double[][])(gray_img)))));
            img1 = ((boolean[][])(new boolean[][]{new boolean[]{true, true, false}}));
            kernel1 = ((int[][])(new int[][]{new int[]{0, 1, 0}}));
            System.out.println(_p(erosion(((boolean[][])(img1)), ((int[][])(kernel1)))));
            img2 = ((boolean[][])(new boolean[][]{new boolean[]{true, false, false}}));
            kernel2 = ((int[][])(new int[][]{new int[]{1, 1, 0}}));
            System.out.println(_p(erosion(((boolean[][])(img2)), ((int[][])(kernel2)))));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
