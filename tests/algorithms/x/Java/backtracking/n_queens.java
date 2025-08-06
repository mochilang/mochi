public class Main {

    static int[][] create_board(int n) {
        int[][] board = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            board = ((int[][])(appendObj(board, row)));
            i = i + 1;
        }
        return board;
    }

    static boolean is_safe(int[][] board, int row, int column) {
        int n = board.length;
        int i_1 = 0;
        while (i_1 < row) {
            if (board[i_1][column] == 1) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        i_1 = row - 1;
        int j_1 = column - 1;
        while (i_1 >= 0 && j_1 >= 0) {
            if (board[i_1][j_1] == 1) {
                return false;
            }
            i_1 = i_1 - 1;
            j_1 = j_1 - 1;
        }
        i_1 = row - 1;
        j_1 = column + 1;
        while (i_1 >= 0 && j_1 < n) {
            if (board[i_1][j_1] == 1) {
                return false;
            }
            i_1 = i_1 - 1;
            j_1 = j_1 + 1;
        }
        return true;
    }

    static String row_string(int[] row) {
        String s = "";
        int j_2 = 0;
        while (j_2 < row.length) {
            if (row[j_2] == 1) {
                s = s + "Q ";
            } else {
                s = s + ". ";
            }
            j_2 = j_2 + 1;
        }
        return s;
    }

    static void printboard(int[][] board) {
        int i_2 = 0;
        while (i_2 < board.length) {
            System.out.println(row_string(((int[])(board[i_2]))));
            i_2 = i_2 + 1;
        }
    }

    static int solve(int[][] board, int row) {
        if (row >= board.length) {
            printboard(((int[][])(board)));
            System.out.println("");
            return 1;
        }
        int count = 0;
        int i_3 = 0;
        while (i_3 < board.length) {
            if (((Boolean)(is_safe(((int[][])(board)), row, i_3)))) {
board[row][i_3] = 1;
                count = count + solve(((int[][])(board)), row + 1);
board[row][i_3] = 0;
            }
            i_3 = i_3 + 1;
        }
        return count;
    }

    static int n_queens(int n) {
        int[][] board_1 = ((int[][])(create_board(n)));
        int total = solve(((int[][])(board_1)), 0);
        System.out.println("The total number of solutions are: " + _p(total));
        return total;
    }
    public static void main(String[] args) {
        n_queens(4);
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
