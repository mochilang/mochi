public class Main {
    static long[][] matrix_1_to_4 = ((long[][])(new long[][]{new long[]{1, 2}, new long[]{3, 4}}));
    static long[][] matrix_5_to_8 = ((long[][])(new long[][]{new long[]{5, 6}, new long[]{7, 8}}));
    static long[][] matrix_count_up = ((long[][])(new long[][]{new long[]{1, 2, 3, 4}, new long[]{5, 6, 7, 8}, new long[]{9, 10, 11, 12}, new long[]{13, 14, 15, 16}}));
    static long[][] matrix_unordered = ((long[][])(new long[][]{new long[]{5, 8, 1, 2}, new long[]{6, 7, 3, 0}, new long[]{4, 5, 9, 1}, new long[]{2, 6, 10, 14}}));

    static boolean is_square(long[][] matrix) {
        long n = (long)(matrix.length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            if ((long)(matrix[(int)((long)(i_1))].length) != (long)(n)) {
                return false;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return true;
    }

    static long[][] matrix_multiply(long[][] a, long[][] b) {
        long rows = (long)(a.length);
        long cols_1 = (long)(b[(int)(0L)].length);
        long inner_1 = (long)(b.length);
        long[][] result_1 = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(rows)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(cols_1)) {
                long sum_1 = 0L;
                long k_1 = 0L;
                while ((long)(k_1) < (long)(inner_1)) {
                    sum_1 = (long)((long)(sum_1) + (long)((long)(a[(int)((long)(i_3))][(int)((long)(k_1))]) * (long)(b[(int)((long)(k_1))][(int)((long)(j_1))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_1 = ((long[])(appendLong(row_1, (long)(sum_1))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            result_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result_1;
    }

    static void multiply(long i, long j, long k, long[][] a, long[][] b, long[][] result, long n, long m) {
        if ((long)(i) >= (long)(n)) {
            return;
        }
        if ((long)(j) >= (long)(m)) {
            multiply((long)((long)(i) + 1L), 0L, 0L, ((long[][])(a)), ((long[][])(b)), ((long[][])(result)), (long)(n), (long)(m));
            return;
        }
        if ((long)(k) >= (long)(b.length)) {
            multiply((long)(i), (long)((long)(j) + 1L), 0L, ((long[][])(a)), ((long[][])(b)), ((long[][])(result)), (long)(n), (long)(m));
            return;
        }
result[(int)((long)(i))][(int)((long)(j))] = (long)((long)(result[(int)((long)(i))][(int)((long)(j))]) + (long)((long)(a[(int)((long)(i))][(int)((long)(k))]) * (long)(b[(int)((long)(k))][(int)((long)(j))])));
        multiply((long)(i), (long)(j), (long)((long)(k) + 1L), ((long[][])(a)), ((long[][])(b)), ((long[][])(result)), (long)(n), (long)(m));
    }

    static long[][] matrix_multiply_recursive(long[][] a, long[][] b) {
        if ((long)(a.length) == 0L || (long)(b.length) == 0L) {
            return new long[][]{};
        }
        if ((long)(a.length) != (long)(b.length) || (!(Boolean)is_square(((long[][])(a)))) || (!(Boolean)is_square(((long[][])(b))))) {
            throw new RuntimeException(String.valueOf("Invalid matrix dimensions"));
        }
        long n_2 = (long)(a.length);
        long m_1 = (long)(b[(int)(0L)].length);
        long[][] result_3 = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(n_2)) {
            long[] row_3 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(m_1)) {
                row_3 = ((long[])(appendLong(row_3, 0L)));
                j_3 = (long)((long)(j_3) + 1L);
            }
            result_3 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(new long[][]{row_3})).toArray(long[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        multiply(0L, 0L, 0L, ((long[][])(a)), ((long[][])(b)), ((long[][])(result_3)), (long)(n_2), (long)(m_1));
        return result_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(matrix_multiply_recursive(((long[][])(matrix_1_to_4)), ((long[][])(matrix_5_to_8))));
            System.out.println(matrix_multiply_recursive(((long[][])(matrix_count_up)), ((long[][])(matrix_unordered))));
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
}
