public class Main {
    static int[][] maze;
    static int n;

    static boolean run_maze(int[][] maze, int i, int j, int dr, int dc, int[][] sol) {
        int size = maze.length;
        if (i == dr && j == dc && maze[i][j] == 0) {
sol[i][j] = 0;
            return true;
        }
        boolean lower_flag = (i >= 0) && (j >= 0);
        boolean upper_flag = (i < size) && (j < size);
        if (lower_flag && upper_flag) {
            boolean block_flag = (sol[i][j] == 1) && (maze[i][j] == 0);
            if (block_flag) {
sol[i][j] = 0;
                if (((Boolean)(run_maze(((int[][])(maze)), i + 1, j, dr, dc, ((int[][])(sol))))) || ((Boolean)(run_maze(((int[][])(maze)), i, j + 1, dr, dc, ((int[][])(sol))))) || ((Boolean)(run_maze(((int[][])(maze)), i - 1, j, dr, dc, ((int[][])(sol))))) || ((Boolean)(run_maze(((int[][])(maze)), i, j - 1, dr, dc, ((int[][])(sol)))))) {
                    return true;
                }
sol[i][j] = 1;
                return false;
            }
        }
        return false;
    }

    static int[][] solve_maze(int[][] maze, int sr, int sc, int dr, int dc) {
        int size_1 = maze.length;
        if (!(0 <= sr && sr < size_1 && 0 <= sc && sc < size_1 && 0 <= dr && dr < size_1 && 0 <= dc && dc < size_1)) {
            throw new RuntimeException(String.valueOf("Invalid source or destination coordinates"));
        }
        int[][] sol = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < size_1) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < size_1) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(1)).toArray()));
                j = j + 1;
            }
            sol = ((int[][])(appendObj(sol, row)));
            i = i + 1;
        }
        boolean solved = run_maze(((int[][])(maze)), sr, sc, dr, dc, ((int[][])(sol)));
        if (((Boolean)(solved))) {
            return sol;
        } else {
            throw new RuntimeException(String.valueOf("No solution exists!"));
        }
    }
    public static void main(String[] args) {
        maze = ((int[][])(new int[][]{new int[]{0, 1, 0, 1, 1}, new int[]{0, 0, 0, 0, 0}, new int[]{1, 0, 1, 0, 1}, new int[]{0, 0, 1, 0, 0}, new int[]{1, 0, 0, 1, 0}}));
        n = maze.length - 1;
        System.out.println(_p(solve_maze(((int[][])(maze)), 0, 0, n, n)));
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
}
