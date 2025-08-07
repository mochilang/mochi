public class Main {
    static int[][] sample;

    static int update_area_of_max_square(int row, int col, int rows, int cols, int[][] mat, int[] largest_square_area) {
        if (row >= rows || col >= cols) {
            return 0;
        }
        int right = update_area_of_max_square(row, col + 1, rows, cols, ((int[][])(mat)), ((int[])(largest_square_area)));
        int diagonal = update_area_of_max_square(row + 1, col + 1, rows, cols, ((int[][])(mat)), ((int[])(largest_square_area)));
        int down = update_area_of_max_square(row + 1, col, rows, cols, ((int[][])(mat)), ((int[])(largest_square_area)));
        if (mat[row][col] == 1) {
            int sub = 1 + _min(new int[]{right, diagonal, down});
            if (sub > largest_square_area[0]) {
largest_square_area[0] = sub;
            }
            return sub;
        } else {
            return 0;
        }
    }

    static int largest_square_area_in_matrix_top_down(int rows, int cols, int[][] mat) {
        int[] largest = ((int[])(new int[]{0}));
        update_area_of_max_square(0, 0, rows, cols, ((int[][])(mat)), ((int[])(largest)));
        return largest[0];
    }

    static int update_area_of_max_square_with_dp(int row, int col, int rows, int cols, int[][] mat, int[][] dp_array, int[] largest_square_area) {
        if (row >= rows || col >= cols) {
            return 0;
        }
        if (dp_array[row][col] != (-1)) {
            return dp_array[row][col];
        }
        int right_1 = update_area_of_max_square_with_dp(row, col + 1, rows, cols, ((int[][])(mat)), ((int[][])(dp_array)), ((int[])(largest_square_area)));
        int diagonal_1 = update_area_of_max_square_with_dp(row + 1, col + 1, rows, cols, ((int[][])(mat)), ((int[][])(dp_array)), ((int[])(largest_square_area)));
        int down_1 = update_area_of_max_square_with_dp(row + 1, col, rows, cols, ((int[][])(mat)), ((int[][])(dp_array)), ((int[])(largest_square_area)));
        if (mat[row][col] == 1) {
            int sub_1 = 1 + _min(new int[]{right_1, diagonal_1, down_1});
            if (sub_1 > largest_square_area[0]) {
largest_square_area[0] = sub_1;
            }
dp_array[row][col] = sub_1;
            return sub_1;
        } else {
dp_array[row][col] = 0;
            return 0;
        }
    }

    static int largest_square_area_in_matrix_top_down_with_dp(int rows, int cols, int[][] mat) {
        int[] largest_1 = ((int[])(new int[]{0}));
        int[][] dp_array = ((int[][])(new int[][]{}));
        int r = 0;
        while (r < rows) {
            int[] row_list = ((int[])(new int[]{}));
            int c = 0;
            while (c < cols) {
                row_list = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_list), java.util.stream.IntStream.of(-1)).toArray()));
                c = c + 1;
            }
            dp_array = ((int[][])(appendObj(dp_array, row_list)));
            r = r + 1;
        }
        update_area_of_max_square_with_dp(0, 0, rows, cols, ((int[][])(mat)), ((int[][])(dp_array)), ((int[])(largest_1)));
        return largest_1[0];
    }

    static int largest_square_area_in_matrix_bottom_up(int rows, int cols, int[][] mat) {
        int[][] dp_array_1 = ((int[][])(new int[][]{}));
        int r_1 = 0;
        while (r_1 <= rows) {
            int[] row_list_1 = ((int[])(new int[]{}));
            int c_1 = 0;
            while (c_1 <= cols) {
                row_list_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_list_1), java.util.stream.IntStream.of(0)).toArray()));
                c_1 = c_1 + 1;
            }
            dp_array_1 = ((int[][])(appendObj(dp_array_1, row_list_1)));
            r_1 = r_1 + 1;
        }
        int largest_2 = 0;
        int row = rows - 1;
        while (row >= 0) {
            int col = cols - 1;
            while (col >= 0) {
                int right_2 = dp_array_1[row][col + 1];
                int diagonal_2 = dp_array_1[row + 1][col + 1];
                int bottom = dp_array_1[row + 1][col];
                if (mat[row][col] == 1) {
                    int value = 1 + _min(new int[]{right_2, diagonal_2, bottom});
dp_array_1[row][col] = value;
                    if (value > largest_2) {
                        largest_2 = value;
                    }
                } else {
dp_array_1[row][col] = 0;
                }
                col = col - 1;
            }
            row = row - 1;
        }
        return largest_2;
    }

    static int largest_square_area_in_matrix_bottom_up_space_optimization(int rows, int cols, int[][] mat) {
        int[] current_row = ((int[])(new int[]{}));
        int i = 0;
        while (i <= cols) {
            current_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current_row), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int[] next_row = ((int[])(new int[]{}));
        int j = 0;
        while (j <= cols) {
            next_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(next_row), java.util.stream.IntStream.of(0)).toArray()));
            j = j + 1;
        }
        int largest_3 = 0;
        int row_1 = rows - 1;
        while (row_1 >= 0) {
            int col_1 = cols - 1;
            while (col_1 >= 0) {
                int right_3 = current_row[col_1 + 1];
                int diagonal_3 = next_row[col_1 + 1];
                int bottom_1 = next_row[col_1];
                if (mat[row_1][col_1] == 1) {
                    int value_1 = 1 + _min(new int[]{right_3, diagonal_3, bottom_1});
current_row[col_1] = value_1;
                    if (value_1 > largest_3) {
                        largest_3 = value_1;
                    }
                } else {
current_row[col_1] = 0;
                }
                col_1 = col_1 - 1;
            }
            next_row = ((int[])(current_row));
            current_row = ((int[])(new int[]{}));
            int t = 0;
            while (t <= cols) {
                current_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current_row), java.util.stream.IntStream.of(0)).toArray()));
                t = t + 1;
            }
            row_1 = row_1 - 1;
        }
        return largest_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sample = ((int[][])(new int[][]{new int[]{1, 1}, new int[]{1, 1}}));
            System.out.println(largest_square_area_in_matrix_top_down(2, 2, ((int[][])(sample))));
            System.out.println(largest_square_area_in_matrix_top_down_with_dp(2, 2, ((int[][])(sample))));
            System.out.println(largest_square_area_in_matrix_bottom_up(2, 2, ((int[][])(sample))));
            System.out.println(largest_square_area_in_matrix_bottom_up_space_optimization(2, 2, ((int[][])(sample))));
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

    static int _min(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] < m) m = a[i];
        return m;
    }
}
