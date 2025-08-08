public class Main {

    static int[][] default_matrix_multiplication(int[][] a, int[][] b) {
        return new int[][]{new int[]{a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1]}, new int[]{a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1]}};
    }

    static int[][] matrix_addition(int[][] matrix_a, int[][] matrix_b) {
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < matrix_a.length) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < matrix_a[i].length) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(matrix_a[i][j] + matrix_b[i][j])).toArray()));
                j = j + 1;
            }
            result = ((int[][])(appendObj((int[][])result, row)));
            i = i + 1;
        }
        return result;
    }

    static int[][] matrix_subtraction(int[][] matrix_a, int[][] matrix_b) {
        int[][] result_1 = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < matrix_a.length) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < matrix_a[i_1].length) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(matrix_a[i_1][j_1] - matrix_b[i_1][j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            result_1 = ((int[][])(appendObj((int[][])result_1, row_1)));
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static int[][][] split_matrix(int[][] a) {
        int n = a.length;
        int mid = Math.floorDiv(n, 2);
        int[][] top_left = ((int[][])(new int[][]{}));
        int[][] top_right = ((int[][])(new int[][]{}));
        int[][] bot_left = ((int[][])(new int[][]{}));
        int[][] bot_right = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < mid) {
            int[] left_row = ((int[])(new int[]{}));
            int[] right_row = ((int[])(new int[]{}));
            int j_2 = 0;
            while (j_2 < mid) {
                left_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left_row), java.util.stream.IntStream.of(a[i_2][j_2])).toArray()));
                right_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right_row), java.util.stream.IntStream.of(a[i_2][j_2 + mid])).toArray()));
                j_2 = j_2 + 1;
            }
            top_left = ((int[][])(appendObj((int[][])top_left, left_row)));
            top_right = ((int[][])(appendObj((int[][])top_right, right_row)));
            i_2 = i_2 + 1;
        }
        i_2 = mid;
        while (i_2 < n) {
            int[] left_row_1 = ((int[])(new int[]{}));
            int[] right_row_1 = ((int[])(new int[]{}));
            int j_3 = 0;
            while (j_3 < mid) {
                left_row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left_row_1), java.util.stream.IntStream.of(a[i_2][j_3])).toArray()));
                right_row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right_row_1), java.util.stream.IntStream.of(a[i_2][j_3 + mid])).toArray()));
                j_3 = j_3 + 1;
            }
            bot_left = ((int[][])(appendObj((int[][])bot_left, left_row_1)));
            bot_right = ((int[][])(appendObj((int[][])bot_right, right_row_1)));
            i_2 = i_2 + 1;
        }
        return new int[][][]{top_left, top_right, bot_left, bot_right};
    }

    static int[] matrix_dimensions(int[][] matrix) {
        return new int[]{matrix.length, matrix[0].length};
    }

    static int next_power_of_two(int n) {
        int p = 1;
        while (p < n) {
            p = p * 2;
        }
        return p;
    }

    static int[][] pad_matrix(int[][] mat, int rows, int cols) {
        int[][] res = ((int[][])(new int[][]{}));
        int i_3 = 0;
        while (i_3 < rows) {
            int[] row_2 = ((int[])(new int[]{}));
            int j_4 = 0;
            while (j_4 < cols) {
                int v = 0;
                if (i_3 < mat.length && j_4 < mat[0].length) {
                    v = mat[i_3][j_4];
                }
                row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(v)).toArray()));
                j_4 = j_4 + 1;
            }
            res = ((int[][])(appendObj((int[][])res, row_2)));
            i_3 = i_3 + 1;
        }
        return res;
    }

    static int[][] actual_strassen(int[][] matrix_a, int[][] matrix_b) {
        if (matrix_dimensions(((int[][])(matrix_a)))[0] == 2) {
            return default_matrix_multiplication(((int[][])(matrix_a)), ((int[][])(matrix_b)));
        }
        int[][][] parts_a = ((int[][][])(split_matrix(((int[][])(matrix_a)))));
        int[][] a = ((int[][])(parts_a[0]));
        int[][] b = ((int[][])(parts_a[1]));
        int[][] c = ((int[][])(parts_a[2]));
        int[][] d = ((int[][])(parts_a[3]));
        int[][][] parts_b = ((int[][][])(split_matrix(((int[][])(matrix_b)))));
        int[][] e = ((int[][])(parts_b[0]));
        int[][] f = ((int[][])(parts_b[1]));
        int[][] g = ((int[][])(parts_b[2]));
        int[][] h = ((int[][])(parts_b[3]));
        int[][] t1 = ((int[][])(actual_strassen(((int[][])(a)), ((int[][])(matrix_subtraction(((int[][])(f)), ((int[][])(h))))))));
        int[][] t2 = ((int[][])(actual_strassen(((int[][])(matrix_addition(((int[][])(a)), ((int[][])(b))))), ((int[][])(h)))));
        int[][] t3 = ((int[][])(actual_strassen(((int[][])(matrix_addition(((int[][])(c)), ((int[][])(d))))), ((int[][])(e)))));
        int[][] t4 = ((int[][])(actual_strassen(((int[][])(d)), ((int[][])(matrix_subtraction(((int[][])(g)), ((int[][])(e))))))));
        int[][] t5 = ((int[][])(actual_strassen(((int[][])(matrix_addition(((int[][])(a)), ((int[][])(d))))), ((int[][])(matrix_addition(((int[][])(e)), ((int[][])(h))))))));
        int[][] t6 = ((int[][])(actual_strassen(((int[][])(matrix_subtraction(((int[][])(b)), ((int[][])(d))))), ((int[][])(matrix_addition(((int[][])(g)), ((int[][])(h))))))));
        int[][] t7 = ((int[][])(actual_strassen(((int[][])(matrix_subtraction(((int[][])(a)), ((int[][])(c))))), ((int[][])(matrix_addition(((int[][])(e)), ((int[][])(f))))))));
        int[][] top_left_1 = ((int[][])(matrix_addition(((int[][])(matrix_subtraction(((int[][])(matrix_addition(((int[][])(t5)), ((int[][])(t4))))), ((int[][])(t2))))), ((int[][])(t6)))));
        int[][] top_right_1 = ((int[][])(matrix_addition(((int[][])(t1)), ((int[][])(t2)))));
        int[][] bot_left_1 = ((int[][])(matrix_addition(((int[][])(t3)), ((int[][])(t4)))));
        int[][] bot_right_1 = ((int[][])(matrix_subtraction(((int[][])(matrix_subtraction(((int[][])(matrix_addition(((int[][])(t1)), ((int[][])(t5))))), ((int[][])(t3))))), ((int[][])(t7)))));
        int[][] new_matrix = ((int[][])(new int[][]{}));
        int i_4 = 0;
        while (i_4 < top_right_1.length) {
            new_matrix = ((int[][])(appendObj((int[][])new_matrix, java.util.stream.IntStream.concat(java.util.Arrays.stream(top_left_1[i_4]), java.util.Arrays.stream(top_right_1[i_4])).toArray())));
            i_4 = i_4 + 1;
        }
        i_4 = 0;
        while (i_4 < bot_right_1.length) {
            new_matrix = ((int[][])(appendObj((int[][])new_matrix, java.util.stream.IntStream.concat(java.util.Arrays.stream(bot_left_1[i_4]), java.util.Arrays.stream(bot_right_1[i_4])).toArray())));
            i_4 = i_4 + 1;
        }
        return new_matrix;
    }

    static int[][] strassen(int[][] matrix1, int[][] matrix2) {
        int[] dims1 = ((int[])(matrix_dimensions(((int[][])(matrix1)))));
        int[] dims2 = ((int[])(matrix_dimensions(((int[][])(matrix2)))));
        if (dims1[1] != dims2[0]) {
            return new int[][]{};
        }
        int maximum = ((Number)(_max(new int[]{dims1[0], dims1[1], dims2[0], dims2[1]}))).intValue();
        int size = next_power_of_two(maximum);
        int[][] new_matrix1 = ((int[][])(pad_matrix(((int[][])(matrix1)), size, size)));
        int[][] new_matrix2 = ((int[][])(pad_matrix(((int[][])(matrix2)), size, size)));
        int[][] result_padded = ((int[][])(actual_strassen(((int[][])(new_matrix1)), ((int[][])(new_matrix2)))));
        int[][] final_matrix = ((int[][])(new int[][]{}));
        int i_5 = 0;
        while (i_5 < dims1[0]) {
            int[] row_3 = ((int[])(new int[]{}));
            int j_5 = 0;
            while (j_5 < dims2[1]) {
                row_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_3), java.util.stream.IntStream.of(result_padded[i_5][j_5])).toArray()));
                j_5 = j_5 + 1;
            }
            final_matrix = ((int[][])(appendObj((int[][])final_matrix, row_3)));
            i_5 = i_5 + 1;
        }
        return final_matrix;
    }

    static void main() {
        int[][] matrix1 = ((int[][])(new int[][]{new int[]{2, 3, 4, 5}, new int[]{6, 4, 3, 1}, new int[]{2, 3, 6, 7}, new int[]{3, 1, 2, 4}, new int[]{2, 3, 4, 5}, new int[]{6, 4, 3, 1}, new int[]{2, 3, 6, 7}, new int[]{3, 1, 2, 4}, new int[]{2, 3, 4, 5}, new int[]{6, 2, 3, 1}}));
        int[][] matrix2 = ((int[][])(new int[][]{new int[]{0, 2, 1, 1}, new int[]{16, 2, 3, 3}, new int[]{2, 2, 7, 7}, new int[]{13, 11, 22, 4}}));
        int[][] res_1 = ((int[][])(strassen(((int[][])(matrix1)), ((int[][])(matrix2)))));
        System.out.println(java.util.Arrays.deepToString(res_1));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _max(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] > m) m = a[i];
        return m;
    }
}
