public class Main {
    static int[][] image;
    static int[][] laplace_kernel;
    static int[][] result;

    static int[][] pad_edge(int[][] image, int pad_size) {
        int height = image.length;
        int width = image[0].length;
        int new_height = height + pad_size * 2;
        int new_width = width + pad_size * 2;
        int[][] padded = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < new_height) {
            int[] row = ((int[])(new int[]{}));
            int src_i = i;
            if (src_i < pad_size) {
                src_i = 0;
            }
            if (src_i >= height + pad_size) {
                src_i = height - 1;
            } else {
                src_i = src_i - pad_size;
            }
            int j = 0;
            while (j < new_width) {
                int src_j = j;
                if (src_j < pad_size) {
                    src_j = 0;
                }
                if (src_j >= width + pad_size) {
                    src_j = width - 1;
                } else {
                    src_j = src_j - pad_size;
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(image[src_i][src_j])).toArray()));
                j = j + 1;
            }
            padded = ((int[][])(appendObj((int[][])padded, row)));
            i = i + 1;
        }
        return padded;
    }

    static int[][] im2col(int[][] image, int block_h, int block_w) {
        int rows = image.length;
        int cols = image[0].length;
        int dst_height = rows - block_h + 1;
        int dst_width = cols - block_w + 1;
        int[][] image_array = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < dst_height) {
            int j_1 = 0;
            while (j_1 < dst_width) {
                int[] window = ((int[])(new int[]{}));
                int bi = 0;
                while (bi < block_h) {
                    int bj = 0;
                    while (bj < block_w) {
                        window = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(window), java.util.stream.IntStream.of(image[i_1 + bi][j_1 + bj])).toArray()));
                        bj = bj + 1;
                    }
                    bi = bi + 1;
                }
                image_array = ((int[][])(appendObj((int[][])image_array, window)));
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return image_array;
    }

    static int[] flatten(int[][] matrix) {
        int[] out = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < matrix.length) {
            int j_2 = 0;
            while (j_2 < matrix[i_2].length) {
                out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(matrix[i_2][j_2])).toArray()));
                j_2 = j_2 + 1;
            }
            i_2 = i_2 + 1;
        }
        return out;
    }

    static int dot(int[] a, int[] b) {
        int sum = 0;
        int i_3 = 0;
        while (i_3 < a.length) {
            sum = sum + a[i_3] * b[i_3];
            i_3 = i_3 + 1;
        }
        return sum;
    }

    static int[][] img_convolve(int[][] image, int[][] kernel) {
        int height_1 = image.length;
        int width_1 = image[0].length;
        int k_size = kernel.length;
        int pad_size = Math.floorDiv(k_size, 2);
        int[][] padded_1 = ((int[][])(pad_edge(((int[][])(image)), pad_size)));
        int[][] image_array_1 = ((int[][])(im2col(((int[][])(padded_1)), k_size, k_size)));
        int[] kernel_flat = ((int[])(flatten(((int[][])(kernel)))));
        int[][] dst = ((int[][])(new int[][]{}));
        int idx = 0;
        int i_4 = 0;
        while (i_4 < height_1) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_3 = 0;
            while (j_3 < width_1) {
                int val = dot(((int[])(image_array_1[idx])), ((int[])(kernel_flat)));
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(val)).toArray()));
                idx = idx + 1;
                j_3 = j_3 + 1;
            }
            dst = ((int[][])(appendObj((int[][])dst, row_1)));
            i_4 = i_4 + 1;
        }
        return dst;
    }

    static void print_matrix(int[][] m) {
        int i_5 = 0;
        while (i_5 < m.length) {
            String line = "";
            int j_4 = 0;
            while (j_4 < m[i_5].length) {
                if (j_4 > 0) {
                    line = line + " ";
                }
                line = line + _p(_geti(m[i_5], j_4));
                j_4 = j_4 + 1;
            }
            System.out.println(line);
            i_5 = i_5 + 1;
        }
    }
    public static void main(String[] args) {
        image = ((int[][])(new int[][]{new int[]{1, 2, 3, 0, 0}, new int[]{4, 5, 6, 0, 0}, new int[]{7, 8, 9, 0, 0}, new int[]{0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0}}));
        laplace_kernel = ((int[][])(new int[][]{new int[]{0, 1, 0}, new int[]{1, -4, 1}, new int[]{0, 1, 0}}));
        result = ((int[][])(img_convolve(((int[][])(image)), ((int[][])(laplace_kernel)))));
        print_matrix(((int[][])(result)));
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
}
