public class Main {
    static int[][] m1 = new int[0][];
    static int[][] m2 = new int[0][];

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static int minimum_cost_path(int[][] matrix) {
        int rows = matrix.length;
        int cols = matrix[0].length;
        int j = 1;
        while (j < cols) {
            int[] row0 = ((int[])(matrix[0]));
row0[j] = row0[j] + row0[j - 1];
matrix[0] = ((int[])(row0));
            j = j + 1;
        }
        int i = 1;
        while (i < rows) {
            int[] row = ((int[])(matrix[i]));
row[0] = row[0] + matrix[i - 1][0];
matrix[i] = ((int[])(row));
            i = i + 1;
        }
        i = 1;
        while (i < rows) {
            int[] row_1 = ((int[])(matrix[i]));
            j = 1;
            while (j < cols) {
                int up = matrix[i - 1][j];
                int left = row_1[j - 1];
                int best = min_int(up, left);
row_1[j] = row_1[j] + best;
                j = j + 1;
            }
matrix[i] = ((int[])(row_1));
            i = i + 1;
        }
        return matrix[rows - 1][cols - 1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            m1 = ((int[][])(new int[][]{new int[]{2, 1}, new int[]{3, 1}, new int[]{4, 2}}));
            m2 = ((int[][])(new int[][]{new int[]{2, 1, 4}, new int[]{2, 1, 3}, new int[]{3, 2, 1}}));
            System.out.println(_p(minimum_cost_path(((int[][])(m1)))));
            System.out.println(_p(minimum_cost_path(((int[][])(m2)))));
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
