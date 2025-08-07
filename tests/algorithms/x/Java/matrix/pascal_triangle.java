public class Main {

    static int[] populate_current_row(int[][] triangle, int current_row_idx) {
        int[] row = ((int[])(new int[]{}));
        int i = 0;
        while (i <= current_row_idx) {
            if (i == 0 || i == current_row_idx) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(1)).toArray()));
            } else {
                int left = triangle[current_row_idx - 1][i - 1];
                int right = triangle[current_row_idx - 1][i];
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(left + right)).toArray()));
            }
            i = i + 1;
        }
        return row;
    }

    static int[][] generate_pascal_triangle(int num_rows) {
        if (num_rows <= 0) {
            return new int[][]{};
        }
        int[][] triangle = ((int[][])(new int[][]{}));
        int row_idx = 0;
        while (row_idx < num_rows) {
            int[] row_1 = ((int[])(populate_current_row(((int[][])(triangle)), row_idx)));
            triangle = ((int[][])(appendObj(triangle, row_1)));
            row_idx = row_idx + 1;
        }
        return triangle;
    }

    static String row_to_string(int[] row, int total_rows, int row_idx) {
        String line = "";
        int spaces = total_rows - row_idx - 1;
        int s = 0;
        while (s < spaces) {
            line = line + " ";
            s = s + 1;
        }
        int c = 0;
        while (c <= row_idx) {
            line = line + _p(_geti(row, c));
            if (c != row_idx) {
                line = line + " ";
            }
            c = c + 1;
        }
        return line;
    }

    static void print_pascal_triangle(int num_rows) {
        int[][] triangle_1 = ((int[][])(generate_pascal_triangle(num_rows)));
        int r = 0;
        while (r < num_rows) {
            String line_1 = String.valueOf(row_to_string(((int[])(triangle_1[r])), num_rows, r));
            System.out.println(line_1);
            r = r + 1;
        }
    }

    static void main() {
        print_pascal_triangle(5);
        System.out.println(_p(generate_pascal_triangle(5)));
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
