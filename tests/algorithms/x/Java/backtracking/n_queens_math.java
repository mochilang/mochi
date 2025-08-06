public class Main {

    static boolean contains(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String repeat(String s, int times) {
        String result = "";
        int i_1 = 0;
        while (i_1 < times) {
            result = result + s;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static String[] build_board(int[] pos, int n) {
        String[] board = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < pos.length) {
            int col = pos[i_2];
            String line = (String)(_repeat(". ", col)) + "Q " + (String)(_repeat(". ", n - 1 - col));
            board = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(board), java.util.stream.Stream.of(line)).toArray(String[]::new)));
            i_2 = i_2 + 1;
        }
        return board;
    }

    static String[][] depth_first_search(int[] pos, int[] dr, int[] dl, int n) {
        int row = pos.length;
        if (row == n) {
            String[][] single = ((String[][])(new String[][]{}));
            single = ((String[][])(appendObj(single, build_board(((int[])(pos)), n))));
            return single;
        }
        String[][] boards = ((String[][])(new String[][]{}));
        int col_1 = 0;
        while (col_1 < n) {
            if (((Boolean)(contains(((int[])(pos)), col_1))) || ((Boolean)(contains(((int[])(dr)), row - col_1))) || ((Boolean)(contains(((int[])(dl)), row + col_1)))) {
                col_1 = col_1 + 1;
                continue;
            }
            String[][] result_1 = ((String[][])(depth_first_search(((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pos), java.util.stream.IntStream.of(col_1)).toArray())), ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dr), java.util.stream.IntStream.of(row - col_1)).toArray())), ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dl), java.util.stream.IntStream.of(row + col_1)).toArray())), n)));
            boards = ((String[][])(concat(boards, result_1)));
            col_1 = col_1 + 1;
        }
        return boards;
    }

    static int n_queens_solution(int n) {
        String[][] boards_1 = ((String[][])(depth_first_search(((int[])(new int[]{})), ((int[])(new int[]{})), ((int[])(new int[]{})), n)));
        int i_3 = 0;
        while (i_3 < boards_1.length) {
            int j = 0;
            while (j < boards_1[i_3].length) {
                System.out.println(boards_1[i_3][j]);
                j = j + 1;
            }
            System.out.println("");
            i_3 = i_3 + 1;
        }
        System.out.println(String.valueOf(boards_1.length) + " " + "solutions were found.");
        return boards_1.length;
    }
    public static void main(String[] args) {
        n_queens_solution(4);
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
}
