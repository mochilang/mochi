public class Main {
    static long[][] sample = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 1}}));

    static long update_area_of_max_square(long row, long col, long rows, long cols, long[][] mat, long[] largest_square_area) {
        if ((long)(row) >= (long)(rows) || (long)(col) >= (long)(cols)) {
            return 0;
        }
        long right_1 = (long)(update_area_of_max_square((long)(row), (long)((long)(col) + 1L), (long)(rows), (long)(cols), ((long[][])(mat)), ((long[])(largest_square_area))));
        long diagonal_1 = (long)(update_area_of_max_square((long)((long)(row) + 1L), (long)((long)(col) + 1L), (long)(rows), (long)(cols), ((long[][])(mat)), ((long[])(largest_square_area))));
        long down_1 = (long)(update_area_of_max_square((long)((long)(row) + 1L), (long)(col), (long)(rows), (long)(cols), ((long[][])(mat)), ((long[])(largest_square_area))));
        if ((long)(mat[(int)((long)(row))][(int)((long)(col))]) == 1L) {
            long sub_1 = (long)(1L + (long)(_minLong(new long[]{right_1, diagonal_1, down_1})));
            if ((long)(sub_1) > (long)(largest_square_area[(int)(0L)])) {
largest_square_area[(int)(0L)] = (long)(sub_1);
            }
            return sub_1;
        } else {
            return 0;
        }
    }

    static long largest_square_area_in_matrix_top_down(long rows, long cols, long[][] mat) {
        long[] largest = ((long[])(new long[]{0}));
        update_area_of_max_square(0L, 0L, (long)(rows), (long)(cols), ((long[][])(mat)), ((long[])(largest)));
        return largest[(int)(0L)];
    }

    static long update_area_of_max_square_with_dp(long row, long col, long rows, long cols, long[][] mat, long[][] dp_array, long[] largest_square_area) {
        if ((long)(row) >= (long)(rows) || (long)(col) >= (long)(cols)) {
            return 0;
        }
        if ((long)(dp_array[(int)((long)(row))][(int)((long)(col))]) != (long)((-1))) {
            return dp_array[(int)((long)(row))][(int)((long)(col))];
        }
        long right_3 = (long)(update_area_of_max_square_with_dp((long)(row), (long)((long)(col) + 1L), (long)(rows), (long)(cols), ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area))));
        long diagonal_3 = (long)(update_area_of_max_square_with_dp((long)((long)(row) + 1L), (long)((long)(col) + 1L), (long)(rows), (long)(cols), ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area))));
        long down_3 = (long)(update_area_of_max_square_with_dp((long)((long)(row) + 1L), (long)(col), (long)(rows), (long)(cols), ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area))));
        if ((long)(mat[(int)((long)(row))][(int)((long)(col))]) == 1L) {
            long sub_3 = (long)(1L + (long)(_minLong(new long[]{right_3, diagonal_3, down_3})));
            if ((long)(sub_3) > (long)(largest_square_area[(int)(0L)])) {
largest_square_area[(int)(0L)] = (long)(sub_3);
            }
dp_array[(int)((long)(row))][(int)((long)(col))] = (long)(sub_3);
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
        while ((long)(r_1) < (long)(rows)) {
            long[] row_list_1 = ((long[])(new long[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < (long)(cols)) {
                row_list_1 = ((long[])(appendLong(row_list_1, (long)(-1))));
                c_1 = (long)((long)(c_1) + 1L);
            }
            dp_array_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_array_1), java.util.stream.Stream.of(new long[][]{row_list_1})).toArray(long[][]::new)));
            r_1 = (long)((long)(r_1) + 1L);
        }
        update_area_of_max_square_with_dp(0L, 0L, (long)(rows), (long)(cols), ((long[][])(mat)), ((long[][])(dp_array_1)), ((long[])(largest_1)));
        return largest_1[(int)(0L)];
    }

    static long largest_square_area_in_matrix_bottom_up(long rows, long cols, long[][] mat) {
        long[][] dp_array_2 = ((long[][])(new long[][]{}));
        long r_3 = 0L;
        while ((long)(r_3) <= (long)(rows)) {
            long[] row_list_3 = ((long[])(new long[]{}));
            long c_3 = 0L;
            while ((long)(c_3) <= (long)(cols)) {
                row_list_3 = ((long[])(appendLong(row_list_3, 0L)));
                c_3 = (long)((long)(c_3) + 1L);
            }
            dp_array_2 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_array_2), java.util.stream.Stream.of(new long[][]{row_list_3})).toArray(long[][]::new)));
            r_3 = (long)((long)(r_3) + 1L);
        }
        long largest_3 = 0L;
        long row_1 = (long)((long)(rows) - 1L);
        while ((long)(row_1) >= 0L) {
            long col_1 = (long)((long)(cols) - 1L);
            while ((long)(col_1) >= 0L) {
                long right_5 = (long)(dp_array_2[(int)((long)(row_1))][(int)((long)((long)(col_1) + 1L))]);
                long diagonal_5 = (long)(dp_array_2[(int)((long)((long)(row_1) + 1L))][(int)((long)((long)(col_1) + 1L))]);
                long bottom_1 = (long)(dp_array_2[(int)((long)((long)(row_1) + 1L))][(int)((long)(col_1))]);
                if ((long)(mat[(int)((long)(row_1))][(int)((long)(col_1))]) == 1L) {
                    long value_1 = (long)(1L + (long)(_minLong(new long[]{right_5, diagonal_5, bottom_1})));
dp_array_2[(int)((long)(row_1))][(int)((long)(col_1))] = (long)(value_1);
                    if ((long)(value_1) > (long)(largest_3)) {
                        largest_3 = (long)(value_1);
                    }
                } else {
dp_array_2[(int)((long)(row_1))][(int)((long)(col_1))] = 0L;
                }
                col_1 = (long)((long)(col_1) - 1L);
            }
            row_1 = (long)((long)(row_1) - 1L);
        }
        return largest_3;
    }

    static long largest_square_area_in_matrix_bottom_up_space_optimization(long rows, long cols, long[][] mat) {
        long[] current_row = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(cols)) {
            current_row = ((long[])(appendLong(current_row, 0L)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] next_row_1 = ((long[])(new long[]{}));
        long j_1 = 0L;
        while ((long)(j_1) <= (long)(cols)) {
            next_row_1 = ((long[])(appendLong(next_row_1, 0L)));
            j_1 = (long)((long)(j_1) + 1L);
        }
        long largest_5 = 0L;
        long row_3 = (long)((long)(rows) - 1L);
        while ((long)(row_3) >= 0L) {
            long col_3 = (long)((long)(cols) - 1L);
            while ((long)(col_3) >= 0L) {
                long right_7 = (long)(current_row[(int)((long)((long)(col_3) + 1L))]);
                long diagonal_7 = (long)(next_row_1[(int)((long)((long)(col_3) + 1L))]);
                long bottom_3 = (long)(next_row_1[(int)((long)(col_3))]);
                if ((long)(mat[(int)((long)(row_3))][(int)((long)(col_3))]) == 1L) {
                    long value_3 = (long)(1L + (long)(_minLong(new long[]{right_7, diagonal_7, bottom_3})));
current_row[(int)((long)(col_3))] = (long)(value_3);
                    if ((long)(value_3) > (long)(largest_5)) {
                        largest_5 = (long)(value_3);
                    }
                } else {
current_row[(int)((long)(col_3))] = 0L;
                }
                col_3 = (long)((long)(col_3) - 1L);
            }
            next_row_1 = ((long[])(current_row));
            current_row = ((long[])(new long[]{}));
            long t_1 = 0L;
            while ((long)(t_1) <= (long)(cols)) {
                current_row = ((long[])(appendLong(current_row, 0L)));
                t_1 = (long)((long)(t_1) + 1L);
            }
            row_3 = (long)((long)(row_3) - 1L);
        }
        return largest_5;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(largest_square_area_in_matrix_top_down(2L, 2L, ((long[][])(sample))));
            System.out.println(largest_square_area_in_matrix_top_down_with_dp(2L, 2L, ((long[][])(sample))));
            System.out.println(largest_square_area_in_matrix_bottom_up(2L, 2L, ((long[][])(sample))));
            System.out.println(largest_square_area_in_matrix_bottom_up_space_optimization(2L, 2L, ((long[][])(sample))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
