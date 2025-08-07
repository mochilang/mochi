public class Main {

    static boolean is_valid_matrix(int[][] matrix) {
        if (matrix.length == 0) {
            return false;
        }
        int cols = matrix[0].length;
        for (int[] row : matrix) {
            if (row.length != cols) {
                return false;
            }
        }
        return true;
    }

    static int[] spiral_traversal(int[][] matrix) {
        if (!(Boolean)is_valid_matrix(((int[][])(matrix)))) {
            return new int[]{};
        }
        int rows = matrix.length;
        int cols_1 = matrix[0].length;
        int top = 0;
        int bottom = rows - 1;
        int left = 0;
        int right = cols_1 - 1;
        int[] result = ((int[])(new int[]{}));
        while (left <= right && top <= bottom) {
            int i = left;
            while (i <= right) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(matrix[top][i])).toArray()));
                i = i + 1;
            }
            top = top + 1;
            i = top;
            while (i <= bottom) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(matrix[i][right])).toArray()));
                i = i + 1;
            }
            right = right - 1;
            if (top <= bottom) {
                i = right;
                while (i >= left) {
                    result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(matrix[bottom][i])).toArray()));
                    i = i - 1;
                }
                bottom = bottom - 1;
            }
            if (left <= right) {
                i = bottom;
                while (i >= top) {
                    result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(matrix[i][left])).toArray()));
                    i = i - 1;
                }
                left = left + 1;
            }
        }
        return result;
    }

    static void spiral_print_clockwise(int[][] matrix) {
        for (int value : spiral_traversal(((int[][])(matrix)))) {
            System.out.println(_p(value));
        }
    }

    static void main() {
        int[][] a = ((int[][])(new int[][]{new int[]{1, 2, 3, 4}, new int[]{5, 6, 7, 8}, new int[]{9, 10, 11, 12}}));
        spiral_print_clockwise(((int[][])(a)));
        System.out.println(_p(spiral_traversal(((int[][])(a)))));
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
        return String.valueOf(v);
    }
}
