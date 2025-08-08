public class Main {

    static double[][] rgb_to_gray(int[][][] rgb) {
        double[][] result = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < rgb.length) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < rgb[i].length) {
                int r = rgb[i][j][0];
                int g = rgb[i][j][1];
                int b = rgb[i][j][2];
                double gray = 0.2989 * (1.0 * r) + 0.587 * (1.0 * g) + 0.114 * (1.0 * b);
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(gray)).toArray()));
                j = j + 1;
            }
            result = ((double[][])(appendObj((double[][])result, row)));
            i = i + 1;
        }
        return result;
    }

    static int[][] gray_to_binary(double[][] gray) {
        int[][] result_1 = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < gray.length) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < gray[i_1].length) {
                double v = gray[i_1][j_1];
                if (v > 127.0 && v <= 255.0) {
                    row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(1)).toArray()));
                } else {
                    row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
                }
                j_1 = j_1 + 1;
            }
            result_1 = ((int[][])(appendObj((int[][])result_1, row_1)));
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static int[][] dilation(int[][] image, int[][] kernel) {
        int img_h = image.length;
        int img_w = image[0].length;
        int k_h = kernel.length;
        int k_w = kernel[0].length;
        int pad_h = Math.floorDiv(k_h, 2);
        int pad_w = Math.floorDiv(k_w, 2);
        int p_h = img_h + 2 * pad_h;
        int p_w = img_w + 2 * pad_w;
        int[][] padded = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < p_h) {
            int[] row_2 = ((int[])(new int[]{}));
            int j_2 = 0;
            while (j_2 < p_w) {
                row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(0)).toArray()));
                j_2 = j_2 + 1;
            }
            padded = ((int[][])(appendObj((int[][])padded, row_2)));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < img_h) {
            int j_3 = 0;
            while (j_3 < img_w) {
padded[pad_h + i_2][pad_w + j_3] = image[i_2][j_3];
                j_3 = j_3 + 1;
            }
            i_2 = i_2 + 1;
        }
        int[][] output = ((int[][])(new int[][]{}));
        i_2 = 0;
        while (i_2 < img_h) {
            int[] row_3 = ((int[])(new int[]{}));
            int j_4 = 0;
            while (j_4 < img_w) {
                int sum = 0;
                int ky = 0;
                while (ky < k_h) {
                    int kx = 0;
                    while (kx < k_w) {
                        if (kernel[ky][kx] == 1) {
                            sum = sum + padded[i_2 + ky][j_4 + kx];
                        }
                        kx = kx + 1;
                    }
                    ky = ky + 1;
                }
                if (sum > 0) {
                    row_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_3), java.util.stream.IntStream.of(1)).toArray()));
                } else {
                    row_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_3), java.util.stream.IntStream.of(0)).toArray()));
                }
                j_4 = j_4 + 1;
            }
            output = ((int[][])(appendObj((int[][])output, row_3)));
            i_2 = i_2 + 1;
        }
        return output;
    }

    static void print_float_matrix(double[][] mat) {
        int i_3 = 0;
        while (i_3 < mat.length) {
            String line = "";
            int j_5 = 0;
            while (j_5 < mat[i_3].length) {
                line = line + _p(_getd(mat[i_3], j_5));
                if (j_5 < mat[i_3].length - 1) {
                    line = line + " ";
                }
                j_5 = j_5 + 1;
            }
            System.out.println(line);
            i_3 = i_3 + 1;
        }
    }

    static void print_int_matrix(int[][] mat) {
        int i_4 = 0;
        while (i_4 < mat.length) {
            String line_1 = "";
            int j_6 = 0;
            while (j_6 < mat[i_4].length) {
                line_1 = line_1 + _p(_geti(mat[i_4], j_6));
                if (j_6 < mat[i_4].length - 1) {
                    line_1 = line_1 + " ";
                }
                j_6 = j_6 + 1;
            }
            System.out.println(line_1);
            i_4 = i_4 + 1;
        }
    }

    static void main() {
        int[][][] rgb_example = ((int[][][])(new int[][][]{new int[][]{new int[]{127, 255, 0}}}));
        print_float_matrix(((double[][])(rgb_to_gray(((int[][][])(rgb_example))))));
        double[][] gray_example = ((double[][])(new double[][]{new double[]{26.0, 255.0, 14.0}, new double[]{5.0, 147.0, 20.0}, new double[]{1.0, 200.0, 0.0}}));
        print_int_matrix(((int[][])(gray_to_binary(((double[][])(gray_example))))));
        int[][] binary_image = ((int[][])(new int[][]{new int[]{0, 1, 0}, new int[]{0, 1, 0}, new int[]{0, 1, 0}}));
        int[][] kernel = ((int[][])(new int[][]{new int[]{0, 1, 0}, new int[]{1, 1, 1}, new int[]{0, 1, 0}}));
        print_int_matrix(((int[][])(dilation(((int[][])(binary_image)), ((int[][])(kernel))))));
    }
    public static void main(String[] args) {
        main();
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
