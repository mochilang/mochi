public class Main {
    static int[][] board_1;

    static int[][] get_valid_pos(int[] position, int n) {
        int y = position[0];
        int x = position[1];
        int[][] positions = ((int[][])(new int[][]{new int[]{y + 1, x + 2}, new int[]{y - 1, x + 2}, new int[]{y + 1, x - 2}, new int[]{y - 1, x - 2}, new int[]{y + 2, x + 1}, new int[]{y + 2, x - 1}, new int[]{y - 2, x + 1}, new int[]{y - 2, x - 1}}));
        int[][] permissible = ((int[][])(new int[][]{}));
        for (int idx = 0; idx < positions.length; idx++) {
            int[] inner = ((int[])(positions[idx]));
            int y_test = inner[0];
            int x_test = inner[1];
            if (y_test >= 0 && y_test < n && x_test >= 0 && x_test < n) {
                permissible = ((int[][])(appendObj(permissible, inner)));
            }
        }
        return permissible;
    }

    static boolean is_complete(int[][] board) {
        for (int i = 0; i < board.length; i++) {
            int[] row = ((int[])(board[i]));
            for (int j = 0; j < row.length; j++) {
                if (row[j] == 0) {
                    return false;
                }
            }
        }
        return true;
    }

    static boolean open_knight_tour_helper(int[][] board, int[] pos, int curr) {
        if (((Boolean)(is_complete(((int[][])(board)))))) {
            return true;
        }
        int[][] moves = ((int[][])(get_valid_pos(((int[])(pos)), board.length)));
        for (int i = 0; i < moves.length; i++) {
            int[] position = ((int[])(moves[i]));
            int y_1 = position[0];
            int x_1 = position[1];
            if (board[y_1][x_1] == 0) {
board[y_1][x_1] = curr + 1;
                if (((Boolean)(open_knight_tour_helper(((int[][])(board)), ((int[])(position)), curr + 1)))) {
                    return true;
                }
board[y_1][x_1] = 0;
            }
        }
        return false;
    }

    static int[][] open_knight_tour(int n) {
        int[][] board = ((int[][])(new int[][]{}));
        for (int i = 0; i < n; i++) {
            int[] row_1 = ((int[])(new int[]{}));
            for (int j = 0; j < n; j++) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
            }
            board = ((int[][])(appendObj(board, row_1)));
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
board[i][j] = 1;
                if (((Boolean)(open_knight_tour_helper(((int[][])(board)), ((int[])(new int[]{i, j})), 1)))) {
                    return board;
                }
board[i][j] = 0;
            }
        }
        System.out.println("Open Knight Tour cannot be performed on a board of size " + _p(n));
        return board;
    }
    public static void main(String[] args) {
        board_1 = ((int[][])(open_knight_tour(1)));
        System.out.println(board_1[0][0]);
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
