public class Main {

    static long[] populate_current_row(long[][] triangle, long current_row_idx) {
        long[] row = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(current_row_idx)) {
            if ((long)(i_1) == 0L || (long)(i_1) == (long)(current_row_idx)) {
                row = ((long[])(appendLong(row, 1L)));
            } else {
                long left_1 = (long)(triangle[(int)((long)((long)(current_row_idx) - 1L))][(int)((long)((long)(i_1) - 1L))]);
                long right_1 = (long)(triangle[(int)((long)((long)(current_row_idx) - 1L))][(int)((long)(i_1))]);
                row = ((long[])(appendLong(row, (long)((long)(left_1) + (long)(right_1)))));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return row;
    }

    static long[][] generate_pascal_triangle(long num_rows) {
        if ((long)(num_rows) <= 0L) {
            return new long[][]{};
        }
        long[][] triangle_1 = ((long[][])(new long[][]{}));
        long row_idx_1 = 0L;
        while ((long)(row_idx_1) < (long)(num_rows)) {
            long[] row_2 = ((long[])(populate_current_row(((long[][])(triangle_1)), (long)(row_idx_1))));
            triangle_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(triangle_1), java.util.stream.Stream.of(new long[][]{row_2})).toArray(long[][]::new)));
            row_idx_1 = (long)((long)(row_idx_1) + 1L);
        }
        return triangle_1;
    }

    static String row_to_string(long[] row, long total_rows, long row_idx) {
        String line = "";
        long spaces_1 = (long)((long)((long)(total_rows) - (long)(row_idx)) - 1L);
        long s_1 = 0L;
        while ((long)(s_1) < (long)(spaces_1)) {
            line = line + " ";
            s_1 = (long)((long)(s_1) + 1L);
        }
        long c_1 = 0L;
        while ((long)(c_1) <= (long)(row_idx)) {
            line = line + _p(_geti(row, ((Number)(c_1)).intValue()));
            if ((long)(c_1) != (long)(row_idx)) {
                line = line + " ";
            }
            c_1 = (long)((long)(c_1) + 1L);
        }
        return line;
    }

    static void print_pascal_triangle(long num_rows) {
        long[][] triangle_2 = ((long[][])(generate_pascal_triangle((long)(num_rows))));
        long r_1 = 0L;
        while ((long)(r_1) < (long)(num_rows)) {
            String line_2 = String.valueOf(row_to_string(((long[])(triangle_2[(int)((long)(r_1))])), (long)(num_rows), (long)(r_1)));
            System.out.println(line_2);
            r_1 = (long)((long)(r_1) + 1L);
        }
    }

    static void main() {
        print_pascal_triangle(5L);
        System.out.println(_p(generate_pascal_triangle(5L)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
