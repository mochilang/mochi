public class Main {
    static long[][] sample;

    static long update_area_of_max_square(long row, long col, long rows, long cols, long[][] mat, long[] largest_square_area) {
        if (row >= rows || col >= cols) {
            return 0;
        }
        long right_1 = update_area_of_max_square(row, (long)(col + (long)(1)), rows, cols, ((long[][])(mat)), ((long[])(largest_square_area)));
        long diagonal_1 = update_area_of_max_square((long)(row + (long)(1)), (long)(col + (long)(1)), rows, cols, ((long[][])(mat)), ((long[])(largest_square_area)));
        long down_1 = update_area_of_max_square((long)(row + (long)(1)), col, rows, cols, ((long[][])(mat)), ((long[])(largest_square_area)));
        if (_geti(((long[])_geto(mat, (int)((long)(row)))), (int)((long)(col))) == (long)(1)) {
            long sub_1 = (long)((long)(1) + _minLong(new long[]{right_1, diagonal_1, down_1}));
            if ((long)(sub_1) > _geti(largest_square_area, (int)((long)(0)))) {
largest_square_area[(int)((long)(0))] = (long)(sub_1);
            }
            return sub_1;
        } else {
            return 0;
        }
    }

    static long largest_square_area_in_matrix_top_down(long rows, long cols, long[][] mat) {
        long[] largest = ((long[])(new long[]{0}));
        update_area_of_max_square(0L, 0L, rows, cols, ((long[][])(mat)), ((long[])(largest)));
        return _geti(largest, (int)((long)(0)));
    }

    static long update_area_of_max_square_with_dp(long row, long col, long rows, long cols, long[][] mat, long[][] dp_array, long[] largest_square_area) {
        if (row >= rows || col >= cols) {
            return 0;
        }
        if (_geti(((long[])_geto(dp_array, (int)((long)(row)))), (int)((long)(col))) != (long)((-1))) {
            return _geti(((long[])_geto(dp_array, (int)((long)(row)))), (int)((long)(col)));
        }
        long right_3 = update_area_of_max_square_with_dp(row, (long)(col + (long)(1)), rows, cols, ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area)));
        long diagonal_3 = update_area_of_max_square_with_dp((long)(row + (long)(1)), (long)(col + (long)(1)), rows, cols, ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area)));
        long down_3 = update_area_of_max_square_with_dp((long)(row + (long)(1)), col, rows, cols, ((long[][])(mat)), ((long[][])(dp_array)), ((long[])(largest_square_area)));
        if (_geti(((long[])_geto(mat, (int)((long)(row)))), (int)((long)(col))) == (long)(1)) {
            long sub_3 = (long)((long)(1) + _minLong(new long[]{right_3, diagonal_3, down_3}));
            if ((long)(sub_3) > _geti(largest_square_area, (int)((long)(0)))) {
largest_square_area[(int)((long)(0))] = (long)(sub_3);
            }
((long[])_geto(dp_array, (int)((long)(row))))[(int)((long)(col))] = (long)(sub_3);
            return sub_3;
        } else {
((long[])_geto(dp_array, (int)((long)(row))))[(int)((long)(col))] = 0L;
            return 0;
        }
    }

    static long largest_square_area_in_matrix_top_down_with_dp(long rows, long cols, long[][] mat) {
        long[] largest_1 = ((long[])(new long[]{0}));
        long[][] dp_array_1 = ((long[][])(new long[][]{}));
        long r_1 = 0L;
        while ((long)(r_1) < rows) {
            long[] row_list_1 = ((long[])(new long[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < cols) {
                row_list_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_list_1), java.util.stream.LongStream.of((long)(-1))).toArray()));
                c_1 = (long)((long)(c_1) + (long)(1));
            }
            dp_array_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_array_1), java.util.stream.Stream.of(row_list_1)).toArray(long[][]::new)));
            r_1 = (long)((long)(r_1) + (long)(1));
        }
        update_area_of_max_square_with_dp(0L, 0L, rows, cols, ((long[][])(mat)), ((long[][])(dp_array_1)), ((long[])(largest_1)));
        return _geti(largest_1, (int)((long)(0)));
    }

    static long largest_square_area_in_matrix_bottom_up(long rows, long cols, long[][] mat) {
        long[][] dp_array_2 = ((long[][])(new long[][]{}));
        long r_3 = 0L;
        while ((long)(r_3) <= rows) {
            long[] row_list_3 = ((long[])(new long[]{}));
            long c_3 = 0L;
            while ((long)(c_3) <= cols) {
                row_list_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_list_3), java.util.stream.LongStream.of(0L)).toArray()));
                c_3 = (long)((long)(c_3) + (long)(1));
            }
            dp_array_2 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_array_2), java.util.stream.Stream.of(row_list_3)).toArray(long[][]::new)));
            r_3 = (long)((long)(r_3) + (long)(1));
        }
        long largest_3 = 0L;
        long row_1 = (long)(rows - (long)(1));
        while ((long)(row_1) >= (long)(0)) {
            long col_1 = (long)(cols - (long)(1));
            while ((long)(col_1) >= (long)(0)) {
                long right_5 = _geti(((long[])_geto(dp_array_2, (int)((long)(row_1)))), (int)((long)((long)(col_1) + (long)(1))));
                long diagonal_5 = _geti(((long[])_geto(dp_array_2, (int)((long)((long)(row_1) + (long)(1))))), (int)((long)((long)(col_1) + (long)(1))));
                long bottom_1 = _geti(((long[])_geto(dp_array_2, (int)((long)((long)(row_1) + (long)(1))))), (int)((long)(col_1)));
                if (_geti(((long[])_geto(mat, (int)((long)(row_1)))), (int)((long)(col_1))) == (long)(1)) {
                    long value_1 = (long)((long)(1) + _minLong(new long[]{right_5, diagonal_5, bottom_1}));
((long[])_geto(dp_array_2, (int)((long)(row_1))))[(int)((long)(col_1))] = (long)(value_1);
                    if ((long)(value_1) > (long)(largest_3)) {
                        largest_3 = (long)(value_1);
                    }
                } else {
((long[])_geto(dp_array_2, (int)((long)(row_1))))[(int)((long)(col_1))] = 0L;
                }
                col_1 = (long)((long)(col_1) - (long)(1));
            }
            row_1 = (long)((long)(row_1) - (long)(1));
        }
        return largest_3;
    }

    static long largest_square_area_in_matrix_bottom_up_space_optimization(long rows, long cols, long[][] mat) {
        long[] current_row = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= cols) {
            current_row = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(current_row), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        long[] next_row_1 = ((long[])(new long[]{}));
        long j_1 = 0L;
        while ((long)(j_1) <= cols) {
            next_row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(next_row_1), java.util.stream.LongStream.of(0L)).toArray()));
            j_1 = (long)((long)(j_1) + (long)(1));
        }
        long largest_5 = 0L;
        long row_3 = (long)(rows - (long)(1));
        while ((long)(row_3) >= (long)(0)) {
            long col_3 = (long)(cols - (long)(1));
            while ((long)(col_3) >= (long)(0)) {
                long right_7 = _geti(current_row, (int)((long)((long)(col_3) + (long)(1))));
                long diagonal_7 = _geti(next_row_1, (int)((long)((long)(col_3) + (long)(1))));
                long bottom_3 = _geti(next_row_1, (int)((long)(col_3)));
                if (_geti(((long[])_geto(mat, (int)((long)(row_3)))), (int)((long)(col_3))) == (long)(1)) {
                    long value_3 = (long)((long)(1) + _minLong(new long[]{right_7, diagonal_7, bottom_3}));
current_row[(int)((long)(col_3))] = (long)(value_3);
                    if ((long)(value_3) > (long)(largest_5)) {
                        largest_5 = (long)(value_3);
                    }
                } else {
current_row[(int)((long)(col_3))] = 0L;
                }
                col_3 = (long)((long)(col_3) - (long)(1));
            }
            next_row_1 = ((long[])(current_row));
            current_row = ((long[])(new long[]{}));
            long t_1 = 0L;
            while ((long)(t_1) <= cols) {
                current_row = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(current_row), java.util.stream.LongStream.of(0L)).toArray()));
                t_1 = (long)((long)(t_1) + (long)(1));
            }
            row_3 = (long)((long)(row_3) - (long)(1));
        }
        return largest_5;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sample = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 1}}));
            System.out.println(largest_square_area_in_matrix_top_down(2L, 2L, ((long[][])(sample))));
            System.out.println(largest_square_area_in_matrix_top_down_with_dp(2L, 2L, ((long[][])(sample))));
            System.out.println(largest_square_area_in_matrix_bottom_up(2L, 2L, ((long[][])(sample))));
            System.out.println(largest_square_area_in_matrix_bottom_up_space_optimization(2L, 2L, ((long[][])(sample))));
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

    static long _minLong(long[] arr) {
        long m = arr[0];
        for (int i = 1; i < arr.length; i++) {
            long v = arr[i];
            if (v < m) m = v;
        }
        return m;
    }

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
