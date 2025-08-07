public class Main {
    static int[][] matrix;

    static String encode(int row, int col) {
        return _p(row) + "," + _p(col);
    }

    static boolean is_safe(int row, int col, int rows, int cols) {
        return row >= 0 && row < rows && col >= 0 && col < cols;
    }

    static boolean has(java.util.Map<String,Boolean> seen, String key) {
        return seen.containsKey(key);
    }

    static int depth_first_search(int row, int col, java.util.Map<String,Boolean> seen, int[][] mat) {
        int rows = mat.length;
        int cols = mat[0].length;
        String key = String.valueOf(encode(row, col));
        if (((Boolean)(is_safe(row, col, rows, cols))) && (!(Boolean)has(seen, key)) && mat[row][col] == 1) {
seen.put(key, true);
            return 1 + depth_first_search(row + 1, col, seen, ((int[][])(mat))) + depth_first_search(row - 1, col, seen, ((int[][])(mat))) + depth_first_search(row, col + 1, seen, ((int[][])(mat))) + depth_first_search(row, col - 1, seen, ((int[][])(mat)));
        } else {
            return 0;
        }
    }

    static int find_max_area(int[][] mat) {
        java.util.Map<String,Boolean> seen = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        int rows_1 = mat.length;
        int max_area = 0;
        int r = 0;
        while (r < rows_1) {
            int[] line = ((int[])(mat[r]));
            int cols_1 = line.length;
            int c = 0;
            while (c < cols_1) {
                if (line[c] == 1) {
                    String key_1 = String.valueOf(encode(r, c));
                    if (!(Boolean)(seen.containsKey(key_1))) {
                        int area = depth_first_search(r, c, seen, ((int[][])(mat)));
                        if (area > max_area) {
                            max_area = area;
                        }
                    }
                }
                c = c + 1;
            }
            r = r + 1;
        }
        return max_area;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            matrix = ((int[][])(new int[][]{new int[]{0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0}, new int[]{0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0}, new int[]{0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}}));
            System.out.println(find_max_area(((int[][])(matrix))));
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
