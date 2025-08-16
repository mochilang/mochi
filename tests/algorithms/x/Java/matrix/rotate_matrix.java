public class Main {
    static long[][] mat_2 = new long[0][];
    static long[][] r90;
    static long[][] r180;
    static long[][] r270;

    static long abs_int(long n) {
        if ((long)(n) < 0L) {
            return -n;
        }
        return n;
    }

    static long[][] make_matrix(long row_size) {
        long size = (long)(abs_int((long)(row_size)));
        if ((long)(size) == 0L) {
            size = 4L;
        }
        long[][] mat_1 = ((long[][])(new long[][]{}));
        long y_1 = 0L;
        while ((long)(y_1) < (long)(size)) {
            long[] row_1 = ((long[])(new long[]{}));
            long x_1 = 0L;
            while ((long)(x_1) < (long)(size)) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of((long)((long)(1L + (long)(x_1)) + (long)((long)(y_1) * (long)(size))))).toArray()));
                x_1 = (long)((long)(x_1) + 1L);
            }
            mat_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_1), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            y_1 = (long)((long)(y_1) + 1L);
        }
        return mat_1;
    }

    static long[][] transpose(long[][] mat) {
        long n = (long)(mat.length);
        long[][] result_1 = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            long[] row_3 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(n)) {
                row_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_3), java.util.stream.LongStream.of((long)(mat[(int)((long)(j_1))][(int)((long)(i_1))]))).toArray()));
                j_1 = (long)((long)(j_1) + 1L);
            }
            result_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new long[][]{row_3})).toArray(long[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result_1;
    }

    static long[][] reverse_row(long[][] mat) {
        long[][] result_2 = ((long[][])(new long[][]{}));
        long i_3 = (long)((long)(mat.length) - 1L);
        while ((long)(i_3) >= 0L) {
            result_2 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_2), java.util.stream.Stream.of(new long[][]{mat[(int)((long)(i_3))]})).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) - 1L);
        }
        return result_2;
    }

    static long[][] reverse_column(long[][] mat) {
        long[][] result_3 = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(mat.length)) {
            long[] row_5 = ((long[])(new long[]{}));
            long j_3 = (long)((long)(mat[(int)((long)(i_5))].length) - 1L);
            while ((long)(j_3) >= 0L) {
                row_5 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_5), java.util.stream.LongStream.of((long)(mat[(int)((long)(i_5))][(int)((long)(j_3))]))).toArray()));
                j_3 = (long)((long)(j_3) - 1L);
            }
            result_3 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(new long[][]{row_5})).toArray(long[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return result_3;
    }

    static long[][] rotate_90(long[][] mat) {
        long[][] t = ((long[][])(transpose(((long[][])(mat)))));
        long[][] rr_1 = ((long[][])(reverse_row(((long[][])(t)))));
        return rr_1;
    }

    static long[][] rotate_180(long[][] mat) {
        long[][] rc = ((long[][])(reverse_column(((long[][])(mat)))));
        long[][] rr_3 = ((long[][])(reverse_row(((long[][])(rc)))));
        return rr_3;
    }

    static long[][] rotate_270(long[][] mat) {
        long[][] t_1 = ((long[][])(transpose(((long[][])(mat)))));
        long[][] rc_2 = ((long[][])(reverse_column(((long[][])(t_1)))));
        return rc_2;
    }

    static String row_to_string(long[] row) {
        String line = "";
        long i_7 = 0L;
        while ((long)(i_7) < (long)(row.length)) {
            if ((long)(i_7) == 0L) {
                line = _p(_geti(row, ((Number)(i_7)).intValue()));
            } else {
                line = line + " " + _p(_geti(row, ((Number)(i_7)).intValue()));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return line;
    }

    static void print_matrix(long[][] mat) {
        long i_8 = 0L;
        while ((long)(i_8) < (long)(mat.length)) {
            System.out.println(row_to_string(((long[])(mat[(int)((long)(i_8))]))));
            i_8 = (long)((long)(i_8) + 1L);
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            mat_2 = ((long[][])(make_matrix(4L)));
            System.out.println("\norigin:\n");
            print_matrix(((long[][])(mat_2)));
            System.out.println("\nrotate 90 counterclockwise:\n");
            r90 = ((long[][])(rotate_90(((long[][])(mat_2)))));
            print_matrix(((long[][])(r90)));
            mat_2 = ((long[][])(make_matrix(4L)));
            System.out.println("\norigin:\n");
            print_matrix(((long[][])(mat_2)));
            System.out.println("\nrotate 180:\n");
            r180 = ((long[][])(rotate_180(((long[][])(mat_2)))));
            print_matrix(((long[][])(r180)));
            mat_2 = ((long[][])(make_matrix(4L)));
            System.out.println("\norigin:\n");
            print_matrix(((long[][])(mat_2)));
            System.out.println("\nrotate 270 counterclockwise:\n");
            r270 = ((long[][])(rotate_270(((long[][])(mat_2)))));
            print_matrix(((long[][])(r270)));
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

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
