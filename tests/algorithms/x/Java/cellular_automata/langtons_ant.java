public class Main {
    static boolean[][] create_board(int width, int height) {
        boolean[][] board = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i < height) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j < width) {
                row = ((boolean[])(appendBool(row, true)));
                j = j + 1;
            }
            board = ((boolean[][])(appendObj(board, row)));
            i = i + 1;
        }
        return board;
    }

    static int[] move_ant(boolean[][] board, int x, int y, int direction) {
        if (((Boolean)(board[x][y]))) {
            direction = Math.floorMod((direction + 1), 4);
        } else {
            direction = Math.floorMod((direction + 3), 4);
        }
        int old_x = x;
        int old_y = y;
        if (direction == 0) {
            x = x - 1;
        } else         if (direction == 1) {
            y = y + 1;
        } else         if (direction == 2) {
            x = x + 1;
        } else {
            y = y - 1;
        }
board[old_x][old_y] = !(Boolean)board[old_x][old_y];
        return new int[]{x, y, direction};
    }

    static boolean[][] langtons_ant(int width, int height, int steps) {
        boolean[][] board_1 = ((boolean[][])(create_board(width, height)));
        int x = width / 2;
        int y = height / 2;
        int dir = 3;
        int s = 0;
        while (s < steps) {
            int[] state = ((int[])(move_ant(((boolean[][])(board_1)), x, y, dir)));
            x = state[0];
            y = state[1];
            dir = state[2];
            s = s + 1;
        }
        return board_1;
    }
    public static void main(String[] args) {
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
}
