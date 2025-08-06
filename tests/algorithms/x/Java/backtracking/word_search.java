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

    static int get_point_key(int len_board, int len_board_column, int row, int column) {
        return len_board * len_board_column * row + column;
    }

    static boolean search_from(String[][] board, String word, int row, int column, int word_index, int[] visited) {
        if (!(board[row][column].equals(_substr(word, word_index, word_index + 1)))) {
            return false;
        }
        if (word_index == _runeLen(word) - 1) {
            return true;
        }
        int len_board = board.length;
        int len_board_column = board[0].length;
        int[] dir_i = ((int[])(new int[]{0, 0, -1, 1}));
        int[] dir_j = ((int[])(new int[]{1, -1, 0, 0}));
        int k = 0;
        while (k < 4) {
            int next_i = row + dir_i[k];
            int next_j = column + dir_j[k];
            if (!(0 <= next_i && next_i < len_board && 0 <= next_j && next_j < len_board_column)) {
                k = k + 1;
                continue;
            }
            int key = get_point_key(len_board, len_board_column, next_i, next_j);
            if (((Boolean)(contains(((int[])(visited)), key)))) {
                k = k + 1;
                continue;
            }
            int[] new_visited = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(visited), java.util.stream.IntStream.of(key)).toArray()));
            if (((Boolean)(search_from(((String[][])(board)), word, next_i, next_j, word_index + 1, ((int[])(new_visited)))))) {
                return true;
            }
            k = k + 1;
        }
        return false;
    }

    static boolean word_exists(String[][] board, String word) {
        int len_board_1 = board.length;
        int len_board_column_1 = board[0].length;
        int i_1 = 0;
        while (i_1 < len_board_1) {
            int j = 0;
            while (j < len_board_column_1) {
                int key_1 = get_point_key(len_board_1, len_board_column_1, i_1, j);
                int[] visited = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new int[]{}), java.util.stream.IntStream.of(key_1)).toArray()));
                if (((Boolean)(search_from(((String[][])(board)), word, i_1, j, 0, ((int[])(visited)))))) {
                    return true;
                }
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static void main() {
        String[][] board = ((String[][])(new String[][]{new String[]{"A", "B", "C", "E"}, new String[]{"S", "F", "C", "S"}, new String[]{"A", "D", "E", "E"}}));
        System.out.println(word_exists(((String[][])(board)), "ABCCED"));
        System.out.println(word_exists(((String[][])(board)), "SEE"));
        System.out.println(word_exists(((String[][])(board)), "ABCB"));
    }
    public static void main(String[] args) {
        main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
    }
}
