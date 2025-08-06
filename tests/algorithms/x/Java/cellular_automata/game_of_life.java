public class Main {
    static boolean[][] glider;
    static boolean[][] board = new boolean[0][];
    static int i = 0;

    static int count_alive_neighbours(boolean[][] board, int row, int col) {
        int size = board.length;
        int alive = 0;
        int dr = -1;
        while (dr < 2) {
            int dc = -1;
            while (dc < 2) {
                int nr = row + dr;
                int nc = col + dc;
                if (!(dr == 0 && dc == 0) && nr >= 0 && nr < size && nc >= 0 && nc < size) {
                    if (((Boolean)(board[nr][nc]))) {
                        alive = alive + 1;
                    }
                }
                dc = dc + 1;
            }
            dr = dr + 1;
        }
        return alive;
    }

    static boolean next_state(boolean current, int alive) {
        boolean state = ((Boolean)(current));
        if (((Boolean)(current))) {
            if (alive < 2) {
                state = false;
            } else             if (alive == 2 || alive == 3) {
                state = true;
            } else {
                state = false;
            }
        } else         if (alive == 3) {
            state = true;
        }
        return state;
    }

    static boolean[][] step(boolean[][] board) {
        int size_1 = board.length;
        boolean[][] new_board = ((boolean[][])(new boolean[][]{}));
        int r = 0;
        while (r < size_1) {
            boolean[] new_row = ((boolean[])(new boolean[]{}));
            int c = 0;
            while (c < size_1) {
                int alive_1 = count_alive_neighbours(((boolean[][])(board)), r, c);
                boolean cell = board[r][c];
                boolean updated = next_state(cell, alive_1);
                new_row = ((boolean[])(appendBool(new_row, ((Boolean)(updated)))));
                c = c + 1;
            }
            new_board = ((boolean[][])(appendObj(new_board, new_row)));
            r = r + 1;
        }
        return new_board;
    }

    static void show(boolean[][] board) {
        int r_1 = 0;
        while (r_1 < board.length) {
            String line = "";
            int c_1 = 0;
            while (c_1 < board[r_1].length) {
                if (((Boolean)(board[r_1][c_1]))) {
                    line = line + "#";
                } else {
                    line = line + ".";
                }
                c_1 = c_1 + 1;
            }
            System.out.println(line);
            r_1 = r_1 + 1;
        }
    }
    public static void main(String[] args) {
        glider = ((boolean[][])(new boolean[][]{new boolean[]{false, true, false, false, false}, new boolean[]{false, false, true, false, false}, new boolean[]{true, true, true, false, false}, new boolean[]{false, false, false, false, false}, new boolean[]{false, false, false, false, false}}));
        board = ((boolean[][])(glider));
        System.out.println("Initial");
        show(((boolean[][])(board)));
        i = 0;
        while (i < 4) {
            board = ((boolean[][])(step(((boolean[][])(board)))));
            System.out.println("\nStep " + _p(i + 1));
            show(((boolean[][])(board)));
            i = i + 1;
        }
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
}
