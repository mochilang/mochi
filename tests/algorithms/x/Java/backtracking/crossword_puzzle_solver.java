public class Main {
    static String[][] puzzle = new String[0][];
    static String[] words = new String[0];
    static boolean[] used = new boolean[0];

    static boolean is_valid(String[][] puzzle, String word, int row, int col, boolean vertical) {
        for (int i = 0; i < _runeLen(word); i++) {
            if (((Boolean)(vertical))) {
                if (row + i >= puzzle.length || !(puzzle[row + i][col].equals(""))) {
                    return false;
                }
            } else             if (col + i >= puzzle[0].length || !(puzzle[row][col + i].equals(""))) {
                return false;
            }
        }
        return true;
    }

    static void place_word(String[][] puzzle, String word, int row, int col, boolean vertical) {
        for (int i = 0; i < _runeLen(word); i++) {
            String ch = word.substring(i, i+1);
            if (((Boolean)(vertical))) {
puzzle[row + i][col] = ch;
            } else {
puzzle[row][col + i] = ch;
            }
        }
    }

    static void remove_word(String[][] puzzle, String word, int row, int col, boolean vertical) {
        for (int i = 0; i < _runeLen(word); i++) {
            if (((Boolean)(vertical))) {
puzzle[row + i][col] = "";
            } else {
puzzle[row][col + i] = "";
            }
        }
    }

    static boolean solve_crossword(String[][] puzzle, String[] words, boolean[] used) {
        for (int row = 0; row < puzzle.length; row++) {
            for (int col = 0; col < puzzle[0].length; col++) {
                if ((puzzle[row][col].equals(""))) {
                    for (int i = 0; i < words.length; i++) {
                        if (!(Boolean)used[i]) {
                            String word = words[i];
                            for (boolean vertical : new boolean[]{true, false}) {
                                if (((Boolean)(is_valid(((String[][])(puzzle)), word, row, col, vertical)))) {
                                    place_word(((String[][])(puzzle)), word, row, col, vertical);
used[i] = true;
                                    if (((Boolean)(solve_crossword(((String[][])(puzzle)), ((String[])(words)), ((boolean[])(used)))))) {
                                        return true;
                                    }
used[i] = false;
                                    remove_word(((String[][])(puzzle)), word, row, col, vertical);
                                }
                            }
                        }
                    }
                    return false;
                }
            }
        }
        return true;
    }
    public static void main(String[] args) {
        puzzle = ((String[][])(new String[][]{new String[]{"", "", ""}, new String[]{"", "", ""}, new String[]{"", "", ""}}));
        words = ((String[])(new String[]{"cat", "dog", "car"}));
        used = ((boolean[])(new boolean[]{false, false, false}));
        if (((Boolean)(solve_crossword(((String[][])(puzzle)), ((String[])(words)), ((boolean[])(used)))))) {
            System.out.println("Solution found:");
            for (String[] row : puzzle) {
                System.out.println(java.util.Arrays.toString(row));
            }
        } else {
            System.out.println("No solution found:");
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
