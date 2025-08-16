public class Main {

    static boolean is_valid_matrix(long[][] matrix) {
        if ((long)(matrix.length) == 0L) {
            return false;
        }
        long cols_1 = (long)(matrix[(int)((long)(0))].length);
        for (long[] row : matrix) {
            if ((long)(row.length) != (long)(cols_1)) {
                return false;
            }
        }
        return true;
    }

    static long[] spiral_traversal(long[][] matrix) {
        if (!(Boolean)is_valid_matrix(((long[][])(matrix)))) {
            return new long[]{};
        }
        long rows_1 = (long)(matrix.length);
        long cols_3 = (long)(matrix[(int)((long)(0))].length);
        long top_1 = 0L;
        long bottom_1 = (long)((long)(rows_1) - 1L);
        long left_1 = 0L;
        long right_1 = (long)((long)(cols_3) - 1L);
        long[] result_1 = ((long[])(new long[]{}));
        while ((long)(left_1) <= (long)(right_1) && (long)(top_1) <= (long)(bottom_1)) {
            long i_1 = (long)(left_1);
            while ((long)(i_1) <= (long)(right_1)) {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(matrix[(int)((long)(top_1))][(int)((long)(i_1))]))).toArray()));
                i_1 = (long)((long)(i_1) + 1L);
            }
            top_1 = (long)((long)(top_1) + 1L);
            i_1 = (long)(top_1);
            while ((long)(i_1) <= (long)(bottom_1)) {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(matrix[(int)((long)(i_1))][(int)((long)(right_1))]))).toArray()));
                i_1 = (long)((long)(i_1) + 1L);
            }
            right_1 = (long)((long)(right_1) - 1L);
            if ((long)(top_1) <= (long)(bottom_1)) {
                i_1 = (long)(right_1);
                while ((long)(i_1) >= (long)(left_1)) {
                    result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(matrix[(int)((long)(bottom_1))][(int)((long)(i_1))]))).toArray()));
                    i_1 = (long)((long)(i_1) - 1L);
                }
                bottom_1 = (long)((long)(bottom_1) - 1L);
            }
            if ((long)(left_1) <= (long)(right_1)) {
                i_1 = (long)(bottom_1);
                while ((long)(i_1) >= (long)(top_1)) {
                    result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(matrix[(int)((long)(i_1))][(int)((long)(left_1))]))).toArray()));
                    i_1 = (long)((long)(i_1) - 1L);
                }
                left_1 = (long)((long)(left_1) + 1L);
            }
        }
        return result_1;
    }

    static void spiral_print_clockwise(long[][] matrix) {
        for (long value : spiral_traversal(((long[][])(matrix)))) {
            System.out.println(_p(value));
        }
    }

    static void main() {
        long[][] a = ((long[][])(new long[][]{new long[]{1, 2, 3, 4}, new long[]{5, 6, 7, 8}, new long[]{9, 10, 11, 12}}));
        spiral_print_clockwise(((long[][])(a)));
        System.out.println(_p(spiral_traversal(((long[][])(a)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
