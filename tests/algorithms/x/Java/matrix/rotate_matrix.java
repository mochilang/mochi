public class Main {
    static int[][] mat_1 = new int[0][];
    static int[][] r90;
    static int[][] r180;
    static int[][] r270;

    static int abs_int(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static int[][] make_matrix(int row_size) {
        int size = abs_int(row_size);
        if (size == 0) {
            size = 4;
        }
        int[][] mat = ((int[][])(new int[][]{}));
        int y = 0;
        while (y < size) {
            int[] row = ((int[])(new int[]{}));
            int x = 0;
            while (x < size) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(1 + x + y * size)).toArray()));
                x = x + 1;
            }
            mat = ((int[][])(appendObj(mat, row)));
            y = y + 1;
        }
        return mat;
    }

    static int[][] transpose(int[][] mat) {
        int n = mat.length;
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] row_1 = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(mat[j][i])).toArray()));
                j = j + 1;
            }
            result = ((int[][])(appendObj(result, row_1)));
            i = i + 1;
        }
        return result;
    }

    static int[][] reverse_row(int[][] mat) {
        int[][] result_1 = ((int[][])(new int[][]{}));
        int i_1 = mat.length - 1;
        while (i_1 >= 0) {
            result_1 = ((int[][])(appendObj(result_1, mat[i_1])));
            i_1 = i_1 - 1;
        }
        return result_1;
    }

    static int[][] reverse_column(int[][] mat) {
        int[][] result_2 = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < mat.length) {
            int[] row_2 = ((int[])(new int[]{}));
            int j_1 = mat[i_2].length - 1;
            while (j_1 >= 0) {
                row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(mat[i_2][j_1])).toArray()));
                j_1 = j_1 - 1;
            }
            result_2 = ((int[][])(appendObj(result_2, row_2)));
            i_2 = i_2 + 1;
        }
        return result_2;
    }

    static int[][] rotate_90(int[][] mat) {
        int[][] t = ((int[][])(transpose(((int[][])(mat)))));
        int[][] rr = ((int[][])(reverse_row(((int[][])(t)))));
        return rr;
    }

    static int[][] rotate_180(int[][] mat) {
        int[][] rc = ((int[][])(reverse_column(((int[][])(mat)))));
        int[][] rr_1 = ((int[][])(reverse_row(((int[][])(rc)))));
        return rr_1;
    }

    static int[][] rotate_270(int[][] mat) {
        int[][] t_1 = ((int[][])(transpose(((int[][])(mat)))));
        int[][] rc_1 = ((int[][])(reverse_column(((int[][])(t_1)))));
        return rc_1;
    }

    static String row_to_string(int[] row) {
        String line = "";
        int i_3 = 0;
        while (i_3 < row.length) {
            if (i_3 == 0) {
                line = _p(_geti(row, i_3));
            } else {
                line = line + " " + _p(_geti(row, i_3));
            }
            i_3 = i_3 + 1;
        }
        return line;
    }

    static void print_matrix(int[][] mat) {
        int i_4 = 0;
        while (i_4 < mat.length) {
            System.out.println(row_to_string(((int[])(mat[i_4]))));
            i_4 = i_4 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            mat_1 = ((int[][])(make_matrix(4)));
            System.out.println("\norigin:\n");
            print_matrix(((int[][])(mat_1)));
            System.out.println("\nrotate 90 counterclockwise:\n");
            r90 = ((int[][])(rotate_90(((int[][])(mat_1)))));
            print_matrix(((int[][])(r90)));
            mat_1 = ((int[][])(make_matrix(4)));
            System.out.println("\norigin:\n");
            print_matrix(((int[][])(mat_1)));
            System.out.println("\nrotate 180:\n");
            r180 = ((int[][])(rotate_180(((int[][])(mat_1)))));
            print_matrix(((int[][])(r180)));
            mat_1 = ((int[][])(make_matrix(4)));
            System.out.println("\norigin:\n");
            print_matrix(((int[][])(mat_1)));
            System.out.println("\nrotate 270 counterclockwise:\n");
            r270 = ((int[][])(rotate_270(((int[][])(mat_1)))));
            print_matrix(((int[][])(r270)));
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
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
