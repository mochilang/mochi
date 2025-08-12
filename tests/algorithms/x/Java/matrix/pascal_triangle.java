public class Main {

    static long[] populate_current_row(long[][] triangle, long current_row_idx) {
        long[] row = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= current_row_idx) {
            if ((long)(i_1) == (long)(0) || (long)(i_1) == current_row_idx) {
                row = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row), java.util.stream.LongStream.of(1L)).toArray()));
            } else {
                long left_1 = _geti(((long[])_geto(triangle, (int)((long)(current_row_idx - (long)(1))))), (int)((long)((long)(i_1) - (long)(1))));
                long right_1 = _geti(((long[])_geto(triangle, (int)((long)(current_row_idx - (long)(1))))), (int)((long)(i_1)));
                row = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row), java.util.stream.LongStream.of((long)(left_1 + right_1))).toArray()));
            }
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return row;
    }

    static long[][] generate_pascal_triangle(long num_rows) {
        if (num_rows <= (long)(0)) {
            return new long[][]{};
        }
        long[][] triangle_1 = ((long[][])(new long[][]{}));
        long row_idx_1 = 0L;
        while ((long)(row_idx_1) < num_rows) {
            long[] row_2 = ((long[])(populate_current_row(((long[][])(triangle_1)), (long)(row_idx_1))));
            triangle_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(triangle_1), java.util.stream.Stream.of(row_2)).toArray(long[][]::new)));
            row_idx_1 = (long)((long)(row_idx_1) + (long)(1));
        }
        return triangle_1;
    }

    static String row_to_string(long[] row, long total_rows, long row_idx) {
        String line = "";
        long spaces_1 = (long)((long)(total_rows - row_idx) - (long)(1));
        long s_1 = 0L;
        while ((long)(s_1) < (long)(spaces_1)) {
            line = line + " ";
            s_1 = (long)((long)(s_1) + (long)(1));
        }
        long c_1 = 0L;
        while ((long)(c_1) <= row_idx) {
            line = line + _p(_geti(row, ((Number)(c_1)).intValue()));
            if ((long)(c_1) != row_idx) {
                line = line + " ";
            }
            c_1 = (long)((long)(c_1) + (long)(1));
        }
        return line;
    }

    static void print_pascal_triangle(long num_rows) {
        long[][] triangle_2 = ((long[][])(generate_pascal_triangle(num_rows)));
        long r_1 = 0L;
        while ((long)(r_1) < num_rows) {
            String line_2 = String.valueOf(row_to_string(((long[])(((long[])_geto(triangle_2, (int)((long)(r_1)))))), num_rows, (long)(r_1)));
            System.out.println(line_2);
            r_1 = (long)((long)(r_1) + (long)(1));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
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
