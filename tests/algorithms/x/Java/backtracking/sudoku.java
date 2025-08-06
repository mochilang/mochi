public class Main {
    static int[][] initial_grid;
    static int[][] no_solution;
    static int[][][] examples;
    static int idx = 0;

    static boolean is_safe(int[][] grid, int row, int column, int n) {
        for (int i = 0; i < 9; i++) {
            if (grid[row][i] == n || grid[i][column] == n) {
                return false;
            }
        }
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (grid[(row - Math.floorMod(row, 3)) + i][(column - Math.floorMod(column, 3)) + j] == n) {
                    return false;
                }
            }
        }
        return true;
    }

    static int[] find_empty_location(int[][] grid) {
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                if (grid[i][j] == 0) {
                    return new int[]{i, j};
                }
            }
        }
        return new int[]{};
    }

    static boolean sudoku(int[][] grid) {
        int[] loc = ((int[])(find_empty_location(((int[][])(grid)))));
        if (loc.length == 0) {
            return true;
        }
        int row = loc[0];
        int column = loc[1];
        for (int digit = 1; digit < 10; digit++) {
            if (((Boolean)(is_safe(((int[][])(grid)), row, column, digit)))) {
grid[row][column] = digit;
                if (((Boolean)(sudoku(((int[][])(grid)))))) {
                    return true;
                }
grid[row][column] = 0;
            }
        }
        return false;
    }

    static void print_solution(int[][] grid) {
        for (int r = 0; r < grid.length; r++) {
            String line = "";
            for (int c = 0; c < grid[r].length; c++) {
                line = line + _p(_geti(grid[r], c));
                if (c < grid[r].length - 1) {
                    line = line + " ";
                }
            }
            System.out.println(line);
        }
    }
    public static void main(String[] args) {
        initial_grid = ((int[][])(new int[][]{new int[]{3, 0, 6, 5, 0, 8, 4, 0, 0}, new int[]{5, 2, 0, 0, 0, 0, 0, 0, 0}, new int[]{0, 8, 7, 0, 0, 0, 0, 3, 1}, new int[]{0, 0, 3, 0, 1, 0, 0, 8, 0}, new int[]{9, 0, 0, 8, 6, 3, 0, 0, 5}, new int[]{0, 5, 0, 0, 9, 0, 6, 0, 0}, new int[]{1, 3, 0, 0, 0, 0, 2, 5, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 7, 4}, new int[]{0, 0, 5, 2, 0, 6, 3, 0, 0}}));
        no_solution = ((int[][])(new int[][]{new int[]{5, 0, 6, 5, 0, 8, 4, 0, 3}, new int[]{5, 2, 0, 0, 0, 0, 0, 0, 2}, new int[]{1, 8, 7, 0, 0, 0, 0, 3, 1}, new int[]{0, 0, 3, 0, 1, 0, 0, 8, 0}, new int[]{9, 0, 0, 8, 6, 3, 0, 0, 5}, new int[]{0, 5, 0, 0, 9, 0, 6, 0, 0}, new int[]{1, 3, 0, 0, 0, 0, 2, 5, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 7, 4}, new int[]{0, 0, 5, 2, 0, 6, 3, 0, 0}}));
        examples = ((int[][][])(new int[][][]{initial_grid, no_solution}));
        idx = 0;
        while (idx < examples.length) {
            System.out.println("\nExample grid:\n====================");
            print_solution(((int[][])(examples[idx])));
            System.out.println("\nExample grid solution:");
            if (((Boolean)(sudoku(((int[][])(examples[idx])))))) {
                print_solution(((int[][])(examples[idx])));
            } else {
                System.out.println("Cannot find a solution.");
            }
            idx = idx + 1;
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
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
