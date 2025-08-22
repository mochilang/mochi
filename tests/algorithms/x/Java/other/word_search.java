public class Main {
    static class WordSearch {
        String[] words;
        long width;
        long height;
        String[][] board;
        WordSearch(String[] words, long width, long height, String[][] board) {
            this.words = words;
            this.width = width;
            this.height = height;
            this.board = board;
        }
        WordSearch() {}
        @Override public String toString() {
            return String.format("{'words': %s, 'width': %s, 'height': %s, 'board': %s}", String.valueOf(words), String.valueOf(width), String.valueOf(height), String.valueOf(board));
        }
    }

    static long seed = 123456789L;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return seed;
    }

    static long rand_range(long max) {
        return Math.floorMod(rand(), max);
    }

    static long[] shuffle(long[] list_int) {
        long i = (long)((long)(list_int.length) - 1L);
        while ((long)(i) > 0L) {
            long j_1 = (long)(rand_range((long)((long)(i) + 1L)));
            long tmp_1 = (long)(list_int[(int)((long)(i))]);
list_int[(int)((long)(i))] = (long)(list_int[(int)((long)(j_1))]);
list_int[(int)((long)(j_1))] = (long)(tmp_1);
            i = (long)((long)(i) - 1L);
        }
        return list_int;
    }

    static String rand_letter() {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        long i_2 = (long)(rand_range(26L));
        return _substr(letters, (int)((long)(i_2)), (int)((long)((long)(i_2) + 1L)));
    }

    static WordSearch make_word_search(String[] words, long width, long height) {
        String[][] board = ((String[][])(new String[][]{}));
        long r_1 = 0L;
        while ((long)(r_1) < (long)(height)) {
            String[] row_1 = ((String[])(new String[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < (long)(width)) {
                row_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of("")).toArray(String[]::new)));
                c_1 = (long)((long)(c_1) + 1L);
            }
            board = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(board), java.util.stream.Stream.of(new String[][]{row_1})).toArray(String[][]::new)));
            r_1 = (long)((long)(r_1) + 1L);
        }
        return new WordSearch(words, width, height, board);
    }

    static boolean insert_dir(WordSearch ws, String word, long dr, long dc, long[] rows, long[] cols) {
        long word_len = (long)(_runeLen(word));
        long ri_1 = 0L;
        while ((long)(ri_1) < (long)(rows.length)) {
            long row_3 = (long)(rows[(int)((long)(ri_1))]);
            long ci_1 = 0L;
            while ((long)(ci_1) < (long)(cols.length)) {
                long col_1 = (long)(cols[(int)((long)(ci_1))]);
                long end_r_1 = (long)((long)(row_3) + (long)((long)(dr) * (long)(((long)(word_len) - 1L))));
                long end_c_1 = (long)((long)(col_1) + (long)((long)(dc) * (long)(((long)(word_len) - 1L))));
                if ((long)(end_r_1) < 0L || (long)(end_r_1) >= (long)(ws.height) || (long)(end_c_1) < 0L || (long)(end_c_1) >= (long)(ws.width)) {
                    ci_1 = (long)((long)(ci_1) + 1L);
                    continue;
                }
                long k_1 = 0L;
                boolean ok_1 = true;
                while ((long)(k_1) < (long)(word_len)) {
                    long rr_1 = (long)((long)(row_3) + (long)((long)(dr) * (long)(k_1)));
                    long cc_1 = (long)((long)(col_1) + (long)((long)(dc) * (long)(k_1)));
                    if (!(ws.board[(int)((long)(rr_1))][(int)((long)(cc_1))].equals(""))) {
                        ok_1 = false;
                        break;
                    }
                    k_1 = (long)((long)(k_1) + 1L);
                }
                if (ok_1) {
                    k_1 = 0L;
                    while ((long)(k_1) < (long)(word_len)) {
                        long rr2_1 = (long)((long)(row_3) + (long)((long)(dr) * (long)(k_1)));
                        long cc2_1 = (long)((long)(col_1) + (long)((long)(dc) * (long)(k_1)));
                        String[] row_list_1 = ((String[])(ws.board[(int)((long)(rr2_1))]));
row_list_1[(int)((long)(cc2_1))] = _substr(word, (int)((long)(k_1)), (int)((long)((long)(k_1) + 1L)));
                        k_1 = (long)((long)(k_1) + 1L);
                    }
                    return true;
                }
                ci_1 = (long)((long)(ci_1) + 1L);
            }
            ri_1 = (long)((long)(ri_1) + 1L);
        }
        return false;
    }

    static void generate_board(WordSearch ws) {
        long[] dirs_r = ((long[])(new long[]{-1, -1, 0, 1, 1, 1, 0, -1}));
        long[] dirs_c_1 = ((long[])(new long[]{0, 1, 1, 1, 0, -1, -1, -1}));
        long i_4 = 0L;
        while ((long)(i_4) < (long)(ws.words.length)) {
            String word_1 = ws.words[(int)((long)(i_4))];
            long[] rows_1 = ((long[])(new long[]{}));
            long r_3 = 0L;
            while ((long)(r_3) < (long)(ws.height)) {
                rows_1 = ((long[])(appendLong(rows_1, (long)(r_3))));
                r_3 = (long)((long)(r_3) + 1L);
            }
            long[] cols_1 = ((long[])(new long[]{}));
            long c_3 = 0L;
            while ((long)(c_3) < (long)(ws.width)) {
                cols_1 = ((long[])(appendLong(cols_1, (long)(c_3))));
                c_3 = (long)((long)(c_3) + 1L);
            }
            rows_1 = ((long[])(shuffle(((long[])(rows_1)))));
            cols_1 = ((long[])(shuffle(((long[])(cols_1)))));
            long d_1 = (long)(rand_range(8L));
            insert_dir(ws, word_1, (long)(dirs_r[(int)((long)(d_1))]), (long)(dirs_c_1[(int)((long)(d_1))]), ((long[])(rows_1)), ((long[])(cols_1)));
            i_4 = (long)((long)(i_4) + 1L);
        }
    }

    static String visualise(WordSearch ws, boolean add_fake_chars) {
        String result = "";
        long r_5 = 0L;
        while ((long)(r_5) < (long)(ws.height)) {
            long c_5 = 0L;
            while ((long)(c_5) < (long)(ws.width)) {
                String ch_1 = ws.board[(int)((long)(r_5))][(int)((long)(c_5))];
                if ((ch_1.equals(""))) {
                    if (add_fake_chars) {
                        ch_1 = String.valueOf(rand_letter());
                    } else {
                        ch_1 = "#";
                    }
                }
                result = result + ch_1 + " ";
                c_5 = (long)((long)(c_5) + 1L);
            }
            result = result + "\n";
            r_5 = (long)((long)(r_5) + 1L);
        }
        return result;
    }

    static void main() {
        String[] words = ((String[])(new String[]{"cat", "dog", "snake", "fish"}));
        WordSearch ws_1 = make_word_search(((String[])(words)), 10L, 10L);
        generate_board(ws_1);
        System.out.println(visualise(ws_1, true));
    }
    public static void main(String[] args) {
        main();
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
