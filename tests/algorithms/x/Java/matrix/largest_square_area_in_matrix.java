public class Main {
    static long[][] sample;

    static long update_area_of_max_square(long row, long col, long rows, long cols, long[][] mat, long[] largest_square_area) {
        if (row >= rows || col >= cols) {
            return 0;
        }
        long right_1 = update_area_of_max_square(row, col + 1, rows, cols, ((long[][])(mat)), ((long[])(largest_square_area)));
        long diagonal_1 = update_area_of_max_square(row + 1, col + 1, rows, cols, ((long[][])(mat)), ((long[])(largest_square_area)));
        long down_1 = update_area_of_max_square(row + 1, col, rows, cols, ((long[][])(mat)), ((long[])(largest_square_area)));
        if (mat[(int)((long)(row))][(int)((long)(col))] == 1) {
            long sub_1 = 1 + _minLong(new long[]{right_1, diagonal_1, down_1});
            if (sub_1 > largest_square_area[(int)((long)(0))]) {
largest_square_area[(int)((long)(0))] = sub_1;
            }
            return sub_1;
        } else {
            return 0;
        }
    }

    static long largest_square_area_in_matrix_top_down(long rows, long cols, long[][] mat) {
        long[] largest = ((long[])(new long[]{0}));
        update_area_of_max_square(0L, 0L, rows, cols, ((long[][])(mat)), ((long[])(largest)));
        return largest[(int)((long)(0))];
    }

    static long update_area_of_max_square_with_dp(long row, long col, long rows, long cols, long[][] mat, long[][] dp_array, long[] largest_square_area) {
        if (row >= rows || col >= cols) {
            return 0;
        }
        if (dp_array[(int)((long)(row))][(int)((long)(col))] != (-1)) {
            return dp_array[(int)((long)(row))][(int)((long)(col))];
        }
        long right_3 = update_area_of_max_square_with_dp(row, col + 1, rows, cols, ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area)));
        long diagonal_3 = update_area_of_max_square_with_dp(row + 1, col + 1, rows, cols, ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area)));
        long down_3 = update_area_of_max_square_with_dp(row + 1, col, rows, cols, ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area)));
        if (mat[(int)((long)(row))][(int)((long)(col))] == 1) {
            long sub_3 = 1 + _minLong(new long[]{right_3, diagonal_3, down_3});
            if (sub_3 > largest_square_area[(int)((long)(0))]) {
largest_square_area[(int)((long)(0))] = sub_3;
            }
dp_array[(int)((long)(row))][(int)((long)(col))] = sub_3;
            return sub_3;
        } else {
dp_array[(int)((long)(row))][(int)((long)(col))] = 0L;
            return 0;
        }
    }

    static long largest_square_area_in_matrix_top_down_with_dp(long rows, long cols, long[][] mat) {
        long[] largest_1 = ((long[])(new long[]{0}));
        long[][] dp_array_1 = ((long[][])(new long[][]{}));
        long r_1 = 0L;
        while (r_1 < rows) {
            long[] row_list_1 = ((long[])(new long[]{}));
            long c_1 = 0L;
            while (c_1 < cols) {
                row_list_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_list_1), java.util.stream.LongStream.of(-1)).toArray()));
                c_1 = c_1 + 1;
            }
            dp_array_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_array_1), java.util.stream.Stream.of(row_list_1)).toArray(long[][]::new)));
            r_1 = r_1 + 1;
        }
        update_area_of_max_square_with_dp(0L, 0L, rows, cols, ((long[][])(mat)), ((long[][])(dp_array_1)), ((long[])(largest_1)));
        return largest_1[(int)((long)(0))];
    }

    static long largest_square_area_in_matrix_bottom_up(long rows, long cols, long[][] mat) {
        long[][] dp_array_2 = ((long[][])(new long[][]{}));
        long r_3 = 0L;
        while (r_3 <= rows) {
            long[] row_list_3 = ((long[])(new long[]{}));
            long c_3 = 0L;
            while (c_3 <= cols) {
                row_list_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_list_3), java.util.stream.LongStream.of(0L)).toArray()));
                c_3 = c_3 + 1;
            }
            dp_array_2 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_array_2), java.util.stream.Stream.of(row_list_3)).toArray(long[][]::new)));
            r_3 = r_3 + 1;
        }
        long largest_3 = 0L;
        long row_1 = rows - 1;
        while (row_1 >= 0) {
            long col_1 = cols - 1;
            while (col_1 >= 0) {
                long right_5 = dp_array_2[(int)((long)(row_1))][(int)((long)(col_1 + 1))];
                long diagonal_5 = dp_array_2[(int)((long)(row_1 + 1))][(int)((long)(col_1 + 1))];
                long bottom_1 = dp_array_2[(int)((long)(row_1 + 1))][(int)((long)(col_1))];
                if (mat[(int)((long)(row_1))][(int)((long)(col_1))] == 1) {
                    long value_1 = 1 + _minLong(new long[]{right_5, diagonal_5, bottom_1});
dp_array_2[(int)((long)(row_1))][(int)((long)(col_1))] = value_1;
                    if (value_1 > largest_3) {
                        largest_3 = value_1;
                    }
                } else {
dp_array_2[(int)((long)(row_1))][(int)((long)(col_1))] = 0L;
                }
                col_1 = col_1 - 1;
            }
            row_1 = row_1 - 1;
        }
        return largest_3;
    }

    static long largest_square_area_in_matrix_bottom_up_space_optimization(long rows, long cols, long[][] mat) {
        long[] current_row = ((long[])(new long[]{}));
        long i_1 = 0L;
        while (i_1 <= cols) {
            current_row = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(current_row), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = i_1 + 1;
        }
        long[] next_row_1 = ((long[])(new long[]{}));
        long j_1 = 0L;
        while (j_1 <= cols) {
            next_row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(next_row_1), java.util.stream.LongStream.of(0L)).toArray()));
            j_1 = j_1 + 1;
        }
        long largest_5 = 0L;
        long row_3 = rows - 1;
        while (row_3 >= 0) {
            long col_3 = cols - 1;
            while (col_3 >= 0) {
                long right_7 = current_row[(int)((long)(col_3 + 1))];
                long diagonal_7 = next_row_1[(int)((long)(col_3 + 1))];
                long bottom_3 = next_row_1[(int)((long)(col_3))];
                if (mat[(int)((long)(row_3))][(int)((long)(col_3))] == 1) {
                    long value_3 = 1 + _minLong(new long[]{right_7, diagonal_7, bottom_3});
current_row[(int)((long)(col_3))] = value_3;
                    if (value_3 > largest_5) {
                        largest_5 = value_3;
                    }
                } else {
current_row[(int)((long)(col_3))] = 0L;
                }
                col_3 = col_3 - 1;
            }
            next_row_1 = ((long[])(current_row));
            current_row = ((long[])(new long[]{}));
            long t_1 = 0L;
            while (t_1 <= cols) {
                current_row = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(current_row), java.util.stream.LongStream.of(0L)).toArray()));
                t_1 = t_1 + 1;
            }
            row_3 = row_3 - 1;
        }
        return largest_5;
    }
    public static void main(String[] args) {
        sample = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 1}}));
        System.out.println(largest_square_area_in_matrix_top_down(2L, 2L, ((long[][])(sample))));
        System.out.println(largest_square_area_in_matrix_top_down_with_dp(2L, 2L, ((long[][])(sample))));
        System.out.println(largest_square_area_in_matrix_bottom_up(2L, 2L, ((long[][])(sample))));
        System.out.println(largest_square_area_in_matrix_bottom_up_space_optimization(2L, 2L, ((long[][])(sample))));
    }

    static long _minLong(long[] arr) {
        long m = arr[0];
        for (int i = 1; i < arr.length; i++) {
            long v = arr[i];
            if (v < m) m = v;
        }
        return m;
    }
}
