public class Main {
    static String puzzle;
    static int[][] grid_1;

    static int[][] string_to_grid(String s) {
        int[][] grid = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < 9) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < 9) {
                String ch = _substr(s, i * 9 + j, i * 9 + j + 1);
                int val = 0;
                if (!(ch.equals("0")) && !(ch.equals("."))) {
                    val = Integer.parseInt(ch);
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(val)).toArray()));
                j = j + 1;
            }
            grid = ((int[][])(appendObj(grid, row)));
            i = i + 1;
        }
        return grid;
    }

    static void print_grid(int[][] grid) {
        for (int r = 0; r < 9; r++) {
            String line = "";
            for (int c = 0; c < 9; c++) {
                line = line + _p(_geti(grid[r], c));
                if (c < 8) {
                    line = line + " ";
                }
            }
            System.out.println(line);
        }
    }

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

    static int[] find_empty(int[][] grid) {
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                if (grid[i][j] == 0) {
                    return new int[]{i, j};
                }
            }
        }
        return new int[]{};
    }

    static boolean solve(int[][] grid) {
        int[] loc = ((int[])(find_empty(((int[][])(grid)))));
        if (loc.length == 0) {
            return true;
        }
        int row_1 = loc[0];
        int column = loc[1];
        for (int digit = 1; digit < 10; digit++) {
            if (((Boolean)(is_safe(((int[][])(grid)), row_1, column, digit)))) {
grid[row_1][column] = digit;
                if (((Boolean)(solve(((int[][])(grid)))))) {
                    return true;
                }
grid[row_1][column] = 0;
            }
        }
        return false;
    }
    public static void main(String[] args) {
        puzzle = "003020600900305001001806400008102900700000008006708200002609500800203009005010300";
        grid_1 = ((int[][])(string_to_grid(puzzle)));
        System.out.println("Original grid:");
        print_grid(((int[][])(grid_1)));
        if (((Boolean)(solve(((int[][])(grid_1)))))) {
            System.out.println("\nSolved grid:");
            print_grid(((int[][])(grid_1)));
        } else {
            System.out.println("\nNo solution found");
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
